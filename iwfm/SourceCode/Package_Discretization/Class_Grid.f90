!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2021
!  State of California, Department of Water Resources
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  (http://www.gnu.org/copyleft/gpl.html)
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!  For tecnical support, e-mail: IWFMtechsupport@water.ca.gov
!***********************************************************************
MODULE Class_Grid
  !$ USE OMP_LIB
  USE MessageLogger      , ONLY: SetLastMessage           , &
                                 MessageArray             , &
                                 f_iFatal
  USE GeneralUtilities   , ONLY: GetUniqueArrayComponents , &
                                 IntTotext                , &
                                 LocateInList             , &
                                 AllocArray               , &
                                 ShellSort
  USE GenericLinkedList  , ONLY: GenericLinkedListType
  IMPLICIT NONE


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: GridType                    , &
            IntegerLinkedListType       , &
            NodeArea                    , &
            ElementArea                 , &
            ElementVertexArea           , &
            ElementVertexAreaFraction   , &
            CheckElementConvexity       , &
            CountTriElements            , &
            CountQuadElements           , &
            ListConnectedNodes          , &
            ListSurroundingElems        , &
            Compute_DELShpI_DELShpJ     , &
            Compute_Rot_DELShpI_DELShpJ


  ! -------------------------------------------------------------
  ! --- GRID DATA TYPE
  ! -------------------------------------------------------------
  TYPE GridType
      REAL(8),ALLOCATABLE           :: X(:)        !Nodal X coordinates for each (node)
      REAL(8),ALLOCATABLE           :: Y(:)        !Nodal Y coordinates for each (node)
      INTEGER,ALLOCATABLE           :: NVertex(:)  !Number of vertices for each (element)
      INTEGER,ALLOCATABLE           :: Vertex(:,:) !Vertex numbers in counter-clockwise direction given as (4,element) combination
  CONTAINS
      PROCEDURE,PASS :: Init      => New
      PROCEDURE,PASS :: KillGrid
      PROCEDURE,PASS :: Centroid
      PROCEDURE,PASS :: FEInterpolate
      PROCEDURE,PASS :: FEInterpolate_AtCell
      PROCEDURE,PASS :: GetElementListViolatingMaxAngleConstraint
      PROCEDURE,PASS :: IsPointInElement
  END TYPE GridType


  ! -------------------------------------------------------------
  ! --- INTEGER LINKED LIST TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLinkedListType) :: IntegerLinkedListType
  END TYPE IntegerLinkedListType


  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  INTERFACE ElementArea
     MODULE PROCEDURE ElementArea_ForArrayList
     MODULE PROCEDURE ElementArea_WithBeginEndIndex
  END INTERFACE ElementArea

  INTERFACE NodeArea
     MODULE PROCEDURE NodeArea_ForArrayList
     MODULE PROCEDURE NodeArea_WithBeginEndIndex
  END INTERFACE NodeArea



  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: ModNameLen=12
  CHARACTER(LEN=12),PARAMETER :: ModName='Class_Grid::'
  INTEGER,PARAMETER           :: NPGAUSS=2




CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** METHODS RELATED TO GRID
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CONSTRUCTOR
  ! -------------------------------------------------------------
  SUBROUTINE New(Grid,X,Y,NVertex,Vertex,iStat)
    CLASS(GridType),INTENT(OUT) :: Grid
    REAL(8),INTENT(IN)          :: X(:),Y(:)
    INTEGER,INTENT(IN)          :: NVertex(:),Vertex(:,:)
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure=ModName // 'New'
    INTEGER                     :: ErrorCode

    !Initialize
    iStat = 0

    !Allocate memory for nodes
    ALLOCATE(Grid%X(SIZE(X)) , Grid%Y(SIZE(Y)) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for nodal coordinates of a grid!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Allocate memory for elements
    ALLOCATE(Grid%NVertex(SIZE(NVertex)) , Grid%Vertex(4,SIZE(NVertex)) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for elements of a grid!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Populate nodes and elements of the grid
    Grid%X       = X
    Grid%Y       = Y
    Grid%NVertex = NVertex
    Grid%Vertex  = Vertex

  END SUBROUTINE New


  ! -------------------------------------------------------------
  ! --- DESTRUCTOR
  ! -------------------------------------------------------------
  SUBROUTINE KillGrid(Grid)
    CLASS(GridType)::Grid

    !Local varibales
    INTEGER :: ErrorCode

    !Deallocate memory for nodal coordinates
    DEALLOCATE(Grid%X , Grid%Y , Grid%NVertex , Grid%Vertex , STAT=ErrorCode)

  END SUBROUTINE KillGrid



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** METHODS RELATED TO NODES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMPUTE NODAL AREA FOR A LIST OF NODES GIVEN AS ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE NodeArea_ForArrayList(Grid,NodeList,iNodeIDs,Area,iStat)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)        :: NodeList(:),iNodeIDs(:)
    REAL(8),INTENT(OUT)       :: Area(:)
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure=ModName // 'NodeArea_ForArrayList'
    INTEGER                      :: indx,indxElem,indxVertex,indxList,NodeNumber,ID
    REAL(8)                      :: DummyReal(1)

    !Initailize
    iStat = 0
    Area  = 0.0

    !Compute
    DO indxElem=1,SIZE(Grid%NVertex)
      DO indxVertex=1,Grid%NVertex(indxElem)
        NodeNumber = Grid%Vertex(indxVertex,indxElem)
        indxList   = LocateInList(NodeNumber,NodeList)
        IF (indxList .LT. 1) CYCLE
        CALL ElementVertexArea(Grid,indxElem,[indxVertex],DummyReal,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Area(indxList) = Area(indxList) + DummyReal(1)
      END DO
    END DO

    !Check all node areas are greater than zero
    DO indx=1,SIZE(NodeList)
        NodeNumber = NodeList(indx)
        IF (Area(indx) .LE. 0.0) THEN
            ID = iNodeIDs(NodeNumber)
            CALL SetLastMessage('Nodal area at node '//TRIM(IntToText(ID))//' is zero!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO

  END SUBROUTINE NodeArea_ForArrayList


  ! -------------------------------------------------------------
  ! --- COMPUTE NODAL AREA FOR A LIST OF NODES GIVEN AS BEGINNING AND END INDICIES
  ! -------------------------------------------------------------
  SUBROUTINE NodeArea_WithBeginEndIndex(Grid,iNodeIDs,iBeginIndex,iEndIndex,Area,iStat)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)        :: iNodeIDs(:),iBeginIndex,iEndIndex
    REAL(8),INTENT(OUT)       :: Area(:)
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure=ModName // 'NodeArea_WithBeginEndIndex'
    INTEGER                      :: indx,indxElem,indxVertex,indxList,NodeNumber,ID
    REAL(8)                      :: DummyReal(1)

    !Initialize
    iStat         = 0
    Area          = 0.0

    !Compute
    DO indxElem=1,SIZE(Grid%NVertex)
      DO indxVertex=1,Grid%NVertex(indxElem)
        NodeNumber = Grid%Vertex(indxVertex,indxElem)
        IF (NodeNumber .LT. iBeginIndex) CYCLE
        IF (NodeNumber .GT. iEndIndex) CYCLE
        indxList       = NodeNumber - iBeginIndex + 1
        CALL ElementVertexArea(Grid,indxElem,[indxVertex],DummyReal,iStat)
        IF (iStat .EQ. -1) RETURN
        Area(indxList) = Area(indxList) + DummyReal(1)
      END DO
    END DO

    !Check all node areas are greater than zero
    DO indx=1,iEndIndex-iBeginIndex+1
        NodeNumber = iBeginIndex + indx - 1
        IF (Area(indx) .LE. 0.0) THEN
            ID = iNodeIDs(NodeNumber)
            CALL SetLastMessage('Nodal area at node '//TRIM(IntToText(ID))//' is zero!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO

  END SUBROUTINE NodeArea_WithBeginEndIndex


  ! -------------------------------------------------------------
  ! --- CONSTRUCT THE LIST OF CONNECTED NODES FOR A NODE
  ! -------------------------------------------------------------
  SUBROUTINE ListConnectedNodes(Grid,iNodeIndex,TheList,iStat)
    TYPE(GridType),INTENT(IN)       :: Grid
    INTEGER,INTENT(IN)              :: iNodeIndex
    INTEGER,ALLOCATABLE,INTENT(OUT) :: TheList(:)
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    INTEGER                     :: indx,indxElem,NodeI,ErrorCode
    INTEGER,ALLOCATABLE         :: iWorkArray(:)
    TYPE(IntegerLinkedListType) :: NodeList

    !Initialize
    iStat = 0

    !Find the nodes connected to node
    DO indxElem=1,SIZE(Grid%NVertex)
      IF (LocateInList(iNodeIndex,Grid%Vertex(:,indxElem)) .LT. 1) CYCLE

      DO indx=1,Grid%NVertex(indxElem)
        NodeI = Grid%Vertex(indx,indxElem)
        IF (NodeI .EQ. iNodeIndex) CYCLE
        CALL NodeList%AddNode(NodeI,iStat)
        IF (iStat .EQ. -1) RETURN
      END DO
    END DO

    !Transfer unique connected node data to permanent data type
    CALL NodeList%GetArray(iWorkArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL GetUniqueArrayComponents(iWorkArray,TheList)
    CALL ShellSort(TheList)

    !Free memory from the linked-list
    CALL NodeList%Delete()
    DEALLOCATE (iWorkArray , STAT=ErrorCode)

  END SUBROUTINE ListConnectedNodes


  ! -------------------------------------------------------------
  ! --- GATHER THE LIST OF SURROUNDING ELEMENTS FOR EACH NODE
  ! -------------------------------------------------------------
  SUBROUTINE ListSurroundingElems(Grid,iNodeIndex,TheList,iStat)
    TYPE(GridType),TARGET,INTENT(IN) :: Grid
    INTEGER,INTENT(IN)               :: iNodeIndex
    INTEGER,ALLOCATABLE,INTENT(OUT)  :: TheList(:)
    INTEGER,INTENT(OUT)              :: iStat

    !Local variables
    INTEGER                     :: indxElem
    TYPE(IntegerLinkedListType) :: SurElemList

    !Initialize
    iStat = 0

    !Find the elements surrounding a node
    DO indxElem=1,SIZE(Grid%NVertex)
      IF (LocateInList(iNodeIndex,Grid%Vertex(:,indxElem)) .LT. 1) CYCLE
      CALL SurElemList%AddNode(indxElem,iStat)
      IF (iStat .EQ. -1) RETURN
    END DO

    !Transfer data to permanent data type
    CALL SurElemList%GetArray(TheList,iStat)
    IF (iStat .EQ. -1) RETURN

    !Free memory from the linked-list
    CALL SurElemList%Delete()

  END SUBROUTINE ListSurroundingElems


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** METHODS RELATED TO ELEMENTS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT AREA FOR A LIST OF ELEMENTS GIVEN WITH BEGINING AND ENDING INDEX
  ! -------------------------------------------------------------
  SUBROUTINE ElementArea_WithBeginEndIndex(Grid,iElemIDs,iBeginIndex,iEndIndex,Area,iStat)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)        :: iElemIDs(:),iBeginIndex,iEndIndex
    REAL(8),INTENT(OUT)       :: Area(:)
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+29) :: ThisProcedure = ModName // 'ElementArea_WithBeginEndIndex'
    INTEGER                      :: ElemNo,NVertex,indxNode,Vertex(4),ID
    REAL(8)                      :: XP(4),YP(4)

    !Initialize
    iStat = 0

    DO ElemNo=iBeginIndex,iEndIndex

      NVertex = Grid%NVertex(ElemNo)
      Vertex  = Grid%Vertex(:,ElemNo)

      !Vertex coordinates
      XP(1:NVertex) = Grid%X(Vertex(1:NVertex))
      YP(1:NVertex) = Grid%Y(Vertex(1:NVertex))

      !Triangular element
      IF (NVertex .EQ. 3) THEN
        Area(ElemNo) = TRI_AREA(XP(1:3),YP(1:3))

      !Quadrilateral element
      ELSE
        Area(ElemNo) = 0.0
        DO indxNode=1,4
          Area(ElemNo) = Area(ElemNo)+QUAD_INTGRL(indxNode,0,XP,YP,NPGAUSS,QUAD_FUNC_AREA)
        END DO
      END IF

      !Check if area is greater than zero
      IF (Area(ElemNo) .LE. 0.0) THEN  !Problem with node coordinates
          ID = iElemIDs(ElemNo)
          MessageArray(1) = 'The area for element '//TRIM(IntToText(ID))//' is less than or equal to zero!'
          MessageArray(2) = 'Check the nodal coordinates for this element.'
          CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

    END DO

  END SUBROUTINE ElementArea_WithBeginEndIndex


  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT AREA FOR A LIST OF ELEMENTS GIVEN IN ARRAY FORM
  ! -------------------------------------------------------------
  SUBROUTINE ElementArea_ForArrayList(Grid,ElemList,iElemIDs,Area,iStat)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)        :: ElemList(:),iElemIDs(:)
    REAL(8),INTENT(OUT)       :: Area(:)
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ElementArea_ForArrayList'
    INTEGER                      :: indx,ElemNo,NVertex,Vertex(4),indxNode,ID
    REAL(8)                      :: XP(4),YP(4)

    !Initialize
    iStat = 0

    DO indx=1,SIZE(ElemList)

      ElemNo  = ElemList(indx)
      NVertex = Grid%NVertex(ElemNo)
      Vertex  = Grid%Vertex(:,ElemNo)

      !Vertex coordinates
      XP(1:NVertex) = Grid%X(Vertex(1:NVertex))
      YP(1:NVertex) = Grid%Y(Vertex(1:NVertex))

      !Triangular element
      IF (NVertex .EQ. 3) THEN
        Area(indx) = TRI_AREA(XP(1:3),YP(1:3))

      !Quadrilateral element
      ELSE
        Area(indx) = 0.0
        DO indxNode=1,4
          Area(indx) = Area(indx) + QUAD_INTGRL(indxNode,0,XP,YP,NPGAUSS,QUAD_FUNC_AREA)
        END DO
      END IF

      !Check if area is greater than zero
      IF (Area(indx) .LE. 0.0) THEN  !Problem with node coordinates
          ID = iElemIDs(ElemNo)
          MessageArray(1)='The area for element '//TRIM(IntToText(ID))//' is less than or equal to zero!'
          MessageArray(2)='Check the nodal coordinates for this element.'
          CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

    END DO

  END SUBROUTINE ElementArea_ForArrayList


  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT VERTEX AREA
  ! -------------------------------------------------------------
  SUBROUTINE ElementVertexArea(Grid,indxElem,VertexList,Area,iStat)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)        :: indxElem,VertexList(:)
    REAL(8),INTENT(OUT)       :: Area(SIZE(VertexList))
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+17) :: ThisProcedure = ModName // 'ElementVertexArea'
    INTEGER                      :: NVertex,Vertex(4),indxVertex
    REAL(8)                      :: XP(4),YP(4)

    !Initialize
    iStat   = 0
    NVertex = Grid%NVertex(indxElem)
    Vertex  = Grid%Vertex(:,indxElem)

    !Vertex coordinates
    XP(1:NVertex) = Grid%X(Vertex(1:NVertex))
    YP(1:NVertex) = Grid%Y(Vertex(1:NVertex))

    !Triangular element
    IF (NVertex .EQ. 3) THEN
      Area = TRI_AREA(XP(1:3),YP(1:3))/3.0
      IF (Area(1) .LE. 0.0) THEN  !Problem with node coordinates
        MessageArray(1)='The area for element '//TRIM(IntToText(indxElem))//' at node '//TRIM(IntToText(Vertex(1)))//' is less than or equal to zero!'
        MessageArray(2)='Check the nodal coordinates for this element.'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF

    !Quadrilateral element
    ELSE
      DO indxVertex=1,SIZE(VertexList)
        Area(indxVertex) = QUAD_INTGRL(VertexList(indxVertex),0,XP,YP,NPGAUSS,QUAD_FUNC_AREA)
        IF (Area(indxVertex) .LE. 0.0) THEN  !Problem with node coordinates
          MessageArray(1)='The area for element '//TRIM(IntToText(indxElem))//' at node '//TRIM(IntToText(Vertex(indxVertex)))//' is less than or equal to zero!'
          MessageArray(2)='Check the nodal coordinates for this element.'
          CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
        END IF
      END DO
    END IF

  END SUBROUTINE ElementVertexArea


  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT VERTEX AREA FRACTION
  ! -------------------------------------------------------------
  SUBROUTINE ElementVertexAreaFraction(Grid,indxElem,iElemIDs,VertexList,Fract,iStat)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)        :: indxElem,iElemIDs(:),VertexList(:)
    REAL(8),INTENT(OUT)       :: Fract(SIZE(VertexList))
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'ElementVertexAreaFraction'
    REAL(8)                                :: Area(1)

    !Triangular element
    IF (Grid%NVertex(indxElem) .EQ. 3) THEN
      Fract = 1d0/3d0
      iStat = 0

    !Quadrilateral element
    ELSE
      CALL ElementArea(Grid,[indxElem],iElemIDs,Area,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL ElementVertexArea(Grid,indxElem,VertexList,Fract,iStat)  ;  IF (iStat .EQ. -1) RETURN
      Fract = Fract / Area(1)
    END IF

  END SUBROUTINE ElementVertexAreaFraction


  ! -------------------------------------------------------------
  ! --- CHECK IF AN ELEMENT IS CONVEX
  ! -------------------------------------------------------------
  PURE SUBROUTINE CheckElementConvexity(NVertex,Vertex,X,Y,ConvexNode)
    INTEGER,INTENT(IN)           :: NVertex,Vertex(:)
    REAL(8),INTENT(IN)           :: X(:),Y(:)
    INTEGER,INTENT(OUT)          :: ConvexNode

    !Local variables
    INTEGER :: indx,IFF,IBB
    REAL(8) :: CrossProduct

    !Initialize
    ConvexNode =  0

    !If element is triangular it cannot be convex
    IF (NVertex .EQ. 3) RETURN

    !Check if element is convex
    !Walking along the cell faces in the counter-clockwise direction and generating vectors between vertices,
    !if cross product of two vectors joining at a vertex is negative we are turning right (internal angle greater than 180),
    !otherwise we are turning left (what we want)
    DO indx=1,4
      IFF=indx+1  ;  IF (IFF.GT.4) IFF=1
      IBB=indx-1  ;  IF (IBB.LT.1) IBB=4
      CrossProduct =  (X(indx)-X(IBB))*(Y(IFF)-Y(indx)) &
                    + (Y(IBB)-Y(indx))*(X(IFF)-X(indx))
      IF (CrossProduct .LE. 0.0) THEN
        ConvexNode = Vertex(indx)
        EXIT
      END IF
   END DO

  END SUBROUTINE CheckElementConvexity


  ! -------------------------------------------------------------
  ! --- GET THE LIST OF ELEMENTS WITH ONE OR MORE ANGLES GREATER THAN USER DEFINED ANGLE GIVEN IN DEGREES
  ! -------------------------------------------------------------
  SUBROUTINE GetElementListViolatingMaxAngleConstraint(Grid,MaxAngle,ElemList,NodeList,iStat)
    CLASS(GridType),INTENT(IN)      :: Grid
    REAL(8),INTENT(IN)              :: MaxAngle
    INTEGER,ALLOCATABLE,INTENT(OUT) :: ElemList(:),NodeList(:)
    INTEGER,INTENT(OUT)             :: iStat

    !Local data type - elements violating maximum angle constraint in a linked list
    TYPE,EXTENDS(GenericLinkedListType) :: ElemNodeListType
    END TYPE ElemNodeListType

    !Local data type - element-vertex combination violating maximum angle constraint
    TYPE ElemNodeType
        INTEGER :: Elem = 0
        INTEGER :: Node = 0
    END TYPE ElemNodeType

    !Local variables
    INTEGER                :: IFF,IBB,NVertex,indxElem,indxVertex,NData,indx,Vertex(4)
    REAL(8)                :: XP(4),YP(4),CrossProduct,DotProduct,MaxAngleRadian,LenA,LenB,Angle
    TYPE(ElemNodeListType) :: ElemNodeLinkedList
    TYPE(ElemNodeType)     :: ElemNodeData
    CLASS(*),POINTER       :: pCurrent
    REAL(8),PARAMETER      :: PI = ACOS(-1d0)

    !Initialize
    iStat = 0

    !Convert degree angle to radian angle
    MaxAngleRadian = MaxAngle * PI / 180d0

    !Loop over elements
    DO indxElem=1,SIZE(Grid%NVertex)
        !Element vertex info
        NVertex       = Grid%NVertex(indxElem)
        Vertex        = Grid%Vertex(:,indxElem)
        XP(1:NVertex) = Grid%X(Vertex(1:NVertex))
        YP(1:NVertex) = Grid%Y(Vertex(1:NVertex))

        !Loop over vertices of the element
        DO indxVertex=1,NVertex
            IFF          = indxVertex + 1  ;  IF (IFF .GT. NVertex) IFF = 1
            IBB          = indxVertex - 1  ;  IF (IBB .LT. 1)       IBB = NVertex
            CrossProduct =  (XP(indxVertex)-XP(IBB)) * (YP(IFF)-YP(indxVertex))  &
                          + (YP(IBB)-YP(indxVertex)) * (XP(IFF)-XP(indxVertex))
            DotProduct   = DOT_PRODUCT([XP(indxVertex)-XP(IBB),YP(indxVertex)-YP(IBB)] , [XP(IFF)-XP(indxVertex),YP(IFF)-YP(indxVertex)])
            LenA         = SQRT((YP(IBB) - YP(indxVertex)) * (YP(IBB) - YP(indxVertex)) + (XP(IBB) - XP(indxVertex)) * (XP(IBB) - XP(indxVertex)))
            LenB         = SQRT((YP(IFF) - YP(indxVertex)) * (YP(IFF) - YP(indxVertex)) + (XP(IFF) - XP(indxVertex)) * (XP(IFF) - XP(indxVertex)))
            IF (CrossProduct .EQ. 0.0) THEN
                Angle = PI
            ELSE IF (CrossProduct .LT. 0.0) THEN
                Angle = PI + ACOS(DotProduct / (LenA * LenB))
            ELSE
                Angle = PI - ACOS(DotProduct / (LenA * LenB))
            END IF
            !Add element and node to list if in viloation of the max angle
            IF (Angle .GE. MaxAngleRadian) THEN
                ElemNodeData%Elem = indxElem
                ElemNodeData%Node = Vertex(indxVertex)
                CALL ElemNodeLinkedList%AddNode(ElemNodeData,iStat)
                IF (iStat .EQ. -1) RETURN
            END IF
        END DO
    END DO

    !Number of element-node combinations in violation of max angle constraint
    NData = ElemNodeLinkedList%GetNNodes()

    !Allocate memory for return arguments
    ALLOCATE (ElemList(NData) , NodeList(NData))

    !Store data in return arguments
    CALL ElemNodeLinkedList%Reset()
    DO indx=1,NData
        pCurrent => ElemNodeLinkedList%GetCurrentValue()
        SELECT TYPE (pCurrent)
           TYPE IS (ElemNodeType)
              ElemList(indx) = pCurrent%Elem
              NodeList(indx) = pCurrent%Node
        END SELECT
        CALL ElemNodeLinkedList%Next()
    END DO

    !Clear memory from linked list
    CALL ElemNodeLinkedList%Delete()

  END SUBROUTINE GetElementListViolatingMaxAngleConstraint


  ! -------------------------------------------------------------
  ! --- COUNT THE NUMBER OF TRIANGULAR ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION CountTriElements(Grid) RESULT(NTriangular)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER                   :: NTriangular

    NTriangular = COUNT(Grid%NVertex .EQ. 3)

  END FUNCTION CountTriElements


  ! -------------------------------------------------------------
  ! --- COUNT THE NUMBER OF QUADRILATERAL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION CountQuadElements(Grid) RESULT(NQuad)
    TYPE(GridType),INTENT(IN) :: Grid
    INTEGER                   :: NQuad

    NQuad = COUNT(Grid%NVertex .EQ. 4)

  END FUNCTION CountQuadElements


  ! -------------------------------------------------------------
  ! --- COMPUTE del_wi * delwj INTEGRAL
  ! -------------------------------------------------------------
  SUBROUTINE Compute_DELShpI_DELShpJ(Grid,ElemNo,iElemIDs,Integral,ElemArea,iStat)
    TYPE(GridType),INTENT(IN)       :: Grid
    INTEGER,INTENT(IN)              :: ElemNo,iElemIDs(:)
    REAL(8),ALLOCATABLE,INTENT(OUT) :: Integral(:)
    REAL(8),OPTIONAL,INTENT(IN)     :: ElemArea
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+23) :: ThisProcedure=ModName//'Compute_DELShpI_DELShpJ'
    INTEGER                      :: NVertex,NIntegral,indx,I,J,Vertex(4)
    REAL(8)                      :: LocalElemArea,XP(4),YP(4),Area(1)

    !Initiliaze
    iStat     = 0
    NVertex   = Grid%NVertex(ElemNo)
    Vertex    = Grid%Vertex(:,ElemNo)
    NIntegral = SUM((/(indx,indx=1,NVertex-1)/))      !Number of integrals to be computed for the element
    CALL AllocArray(Integral,NIntegral,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (PRESENT(ElemArea)) THEN
      LocalElemArea = ElemArea
    ELSE
      CALL ElementArea(Grid,[ElemNo],iElemIDs,Area,iStat)
      IF (iStat .EQ. -1) RETURN
      LocalElemArea = Area(1)
    END IF
    indx = 0

    !Nodal coordinates
    XP(1:NVertex) = Grid%X(Vertex(1:NVertex))
    YP(1:NVertex) = Grid%Y(Vertex(1:NVertex))

    !Triangular element
    IF (NVertex .EQ. 3) THEN
      DO I=1,3
        DO J=I+1,3
          indx = indx+1
          CALL TRI_INTGRL(I,J,XP(1:NVertex),YP(1:NVertex),LocalElemArea,Integral(indx),iStat)
          IF (iStat .EQ. -1) RETURN
        END DO
      END DO

    !Quadrilateral element
    ELSE
      DO I=1,4
        DO J=I+1,4
          indx = indx+1
          Integral(indx) = QUAD_INTGRL(I,J,XP,YP,NPGAUSS,QUAD_FUNC_DIFF)
        END DO
      END DO

    END IF

  END SUBROUTINE Compute_DELShpI_DELShpJ


  ! -------------------------------------------------------------
  ! --- COMPUTE rot_del_wi * delwj INTEGRAL
  ! -------------------------------------------------------------
  SUBROUTINE Compute_Rot_DELShpI_DELShpJ(Grid,iElemIndex,iElemIDs,Integral,ElemArea,iStat)
    TYPE(GridType),INTENT(IN)       :: Grid
    INTEGER,INTENT(IN)              :: iElemIndex,iElemIDs(:)
    REAL(8),ALLOCATABLE,INTENT(OUT) :: Integral(:)
    REAL(8),OPTIONAL,INTENT(IN)     :: ElemArea
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+27) :: ThisProcedure = ModName // 'Compute_Rot_DELShpI_DELShpJ'
    INTEGER                      :: NVertex,NIntegral,indx,I,J,Vertex(4)
    REAL(8)                      :: LocalElemArea,XP(4),YP(4),Area(1)

    !Initiliaze
    iStat     = 0
    NVertex   = Grid%NVertex(iElemIndex)
    Vertex    = Grid%Vertex(:,iElemIndex)
    NIntegral = SUM((/(indx,indx=1,NVertex-1)/))      !Number of integrals to be computed for the element
    CALL AllocArray(Integral,NIntegral,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (PRESENT(ElemArea)) THEN
      LocalElemArea = ElemArea
    ELSE
      CALL ElementArea(Grid,[iElemIndex],iElemIDs,Area,iStat)
      IF (iStat .EQ. -1) RETURN
      LocalElemArea = Area(1)
    END IF
    indx = 0

    !Nodal coordinates
    XP(1:NVertex) = Grid%X(Vertex(1:NVertex))
    YP(1:NVertex) = Grid%Y(Vertex(1:NVertex))

    !Triangular element
    IF (NVertex .EQ. 3) THEN
      DO I=1,3
        DO J=I+1,3
          indx = indx+1
          CALL TRI_ROT(I,J,XP(1:NVertex),YP(1:NVertex),LocalElemArea,Integral(indx),iStat)
          IF (iStat .EQ. -1) RETURN
        END DO
      END DO

    !Quadrilateral element
    ELSE
      DO I=1,4
        DO J=I+1,4
          indx = indx+1
          Integral(indx) = QUAD_INTGRL(I,J,XP(1:NVertex),YP(1:NVertex),NPGAUSS,QUAD_FUNC_ROT)
        END DO
      END DO

    END IF

  END SUBROUTINE Compute_Rot_DELShpI_DELShpJ


  ! -------------------------------------------------------------
  ! --- FIND THE CENTROID OF AN ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE Centroid(Grid,iElemIndex,XC,YC)
    CLASS(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)         :: iElemIndex
    REAL(8),INTENT(OUT)        :: XC,YC

    !Local variables
    INTEGER :: NVertex
    REAL(8) :: X(Grid%NVertex(iElemIndex))  , &
               Y(Grid%NVertex(iElemIndex))

    !Initialize
    NVertex = Grid%NVertex(iElemIndex)
    X       = Grid%X(Grid%Vertex(1:NVertex,iElemIndex))
    Y       = Grid%Y(Grid%Vertex(1:NVertex,iElemIndex))

    !Compute
    XC = SUM(X)/REAL(NVertex,8)
    YC = SUM(Y)/REAL(NVertex,8)

  END SUBROUTINE Centroid




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SHAPE FUNCTION INTEGRATION METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************


  ! -------------------------------------------------------------
  ! -------------------------------------------------------------
  ! --- METHODS FOR TRIANGULAR ELEMENTS
  ! -------------------------------------------------------------
  ! -------------------------------------------------------------

  ! -------------------------------------------------------------
  ! --- AREA OF TRIANGULAR ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION TRI_AREA(XP,YP) RESULT(AREA)
    REAL(8),INTENT(IN)::XP(3),YP(3)
    REAL(8)::AREA

    AREA= (XP(1)-XP(3))*(YP(2)-YP(3)) + (XP(2)-XP(3))*(YP(3)-YP(1))
    AREA= 0.5*AREA

  END FUNCTION TRI_AREA

  ! -------------------------------------------------------------
  ! --- INTEGRATION OF DIFFUSION TERM OVER TRIANGULAR ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE TRI_INTGRL(I,J,XP,YP,AREA,Integral,iStat)
    INTEGER,INTENT(IN)  :: I,J
    REAL(8),INTENT(IN)  :: XP(3),YP(3),AREA
    REAL(8),INTENT(OUT) :: Integral
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+10)::ThisProcedure=ModName//'TRI_INTGRL'
    INTEGER::K

    !Initialize
    iStat    = 0
    Integral = 0.0

    !Check I and J are not equal
    IF (I .EQ. J) THEN
        CALL SetLastMessage('Node I and Node J are equal in integration of diffusion term!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Find node that is not node I or Node J
    DO K=1,3
      IF (.NOT.(K.EQ.I.OR.K.EQ.J)) EXIT
    END DO

    !Compute integral
    Integral= (YP(I)-YP(K))*(YP(K)-YP(J)) + (XP(K)-XP(I))*(XP(J)-XP(K))
    Integral= Integral/(4.0*AREA)

  END SUBROUTINE TRI_INTGRL


  ! -------------------------------------------------------------
  ! --- INTEGRATION OF ROTATION TERM OVER TRIANGULAR ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE TRI_ROT(I,J,XP,YP,AREA,Rot,iStat)
    INTEGER,INTENT(IN)  :: I,J
    REAL(8),INTENT(IN)  :: XP(3),YP(3),AREA
    REAL(8),INTENT(OUT) :: Rot
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+7) :: ThisProcedure=ModName//'TRI_ROT'
    INTEGER                     :: K

    !Initialize
    iStat = 0

    !Check I and J are not equal
    IF (I .EQ. J) THEN
        CALL SetLastMessage('Node I and Node J are equal in integration of rotation term!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Find node that is not node I or Node J
    DO K=1,3
      IF (.NOT.(K.EQ.I.OR.K.EQ.J)) EXIT
    END DO

    !Compute integral
    Rot = (XP(K)-XP(J))*(YP(K)-YP(I)) - (YP(J)-YP(K))*(XP(I)-XP(K))
    Rot = Rot/(4D0*AREA)

  END SUBROUTINE TRI_ROT


 ! -------------------------------------------------------------
  ! -------------------------------------------------------------
  ! --- SHAPE FUNCTIONS FOR QUADRILATERAL ELEMENTS AND THEIR DERIVATIVES
  ! -------------------------------------------------------------
  ! -------------------------------------------------------------

  ! -------------------------------------------------------------
  ! --- SHAPE FUNCTIONS
  ! -------------------------------------------------------------
  REAL(8) FUNCTION QUAD_SHAPE(XI,ETA,NSHP)
    REAL(8),INTENT(IN)::XI,ETA
    INTEGER,INTENT(IN)::NSHP

    IF (NSHP.EQ.1) THEN
      QUAD_SHAPE=0.25*(XI-1.0)*(ETA-1.0)
    ELSEIF (NSHP.EQ.2) THEN
      QUAD_SHAPE=0.25*(XI+1.0)*(1.0-ETA)
    ELSEIF (NSHP.EQ.3) THEN
      QUAD_SHAPE=0.25*(XI+1.0)*(ETA+1.0)
    ELSEIF (NSHP.EQ.4) THEN
      QUAD_SHAPE=0.25*(1.0-XI)*(ETA+1.0)
    END IF

  END FUNCTION QUAD_SHAPE

  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF SHAPE FUNCTIONS W.R.T. XI
  ! -------------------------------------------------------------
  REAL(8) FUNCTION DXI_QUAD_SHAPE(ETA,NSHP)
    REAL(8),INTENT(IN)::ETA
    INTEGER,INTENT(IN)::NSHP

    IF (NSHP.EQ.1) THEN
      DXI_QUAD_SHAPE=0.25*(ETA-1.0)
    ELSEIF (NSHP.EQ.2) THEN
      DXI_QUAD_SHAPE=0.25*(1.0-ETA)
    ELSEIF (NSHP.EQ.3) THEN
      DXI_QUAD_SHAPE=0.25*(ETA+1.0)
    ELSEIF (NSHP.EQ.4) THEN
      DXI_QUAD_SHAPE=-0.25*(ETA+1.0)
    END IF

  END FUNCTION DXI_QUAD_SHAPE

  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF SHAPE FUNCTIONS W.R.T. ETA
  ! -------------------------------------------------------------
  REAL(8) FUNCTION DETA_QUAD_SHAPE(XI,NSHP)
    REAL(8),INTENT(IN)::XI
    INTEGER,INTENT(IN)::NSHP

    IF (NSHP.EQ.1) THEN
      DETA_QUAD_SHAPE=0.25*(XI-1.0)
    ELSEIF (NSHP.EQ.2) THEN
      DETA_QUAD_SHAPE=-0.25*(XI+1.0)
    ELSEIF (NSHP.EQ.3) THEN
      DETA_QUAD_SHAPE=0.25*(XI+1.0)
    ELSEIF (NSHP.EQ.4) THEN
      DETA_QUAD_SHAPE=0.25*(1.0-XI)
    END IF

  END FUNCTION DETA_QUAD_SHAPE

  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF SHAPE FUNCTIONS W.R.T. X
  ! -------------------------------------------------------------
  REAL(8) FUNCTION DX_QUAD_SHAPE(XI,ETA,XP,YP,NSHP)
    REAL(8),INTENT(IN)::XI,ETA,XP(4),YP(4)
    INTEGER,INTENT(IN)::NSHP

    DX_QUAD_SHAPE = 1.0/DET_JACOB(XI,ETA,XP,YP) *             &
                  ( D_DETA(XI,YP)*DXI_QUAD_SHAPE(ETA,NSHP)    &
                   -D_DXI(ETA,YP)*DETA_QUAD_SHAPE(XI,NSHP))

  END FUNCTION DX_QUAD_SHAPE

  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF SHAPE FUNCTIONS W.R.T. Y
  ! -------------------------------------------------------------
  REAL(8) FUNCTION DY_QUAD_SHAPE(XI,ETA,XP,YP,NSHP)
    REAL(8),INTENT(IN)::XI,ETA,XP(4),YP(4)
    INTEGER,INTENT(IN)::NSHP

    DY_QUAD_SHAPE = 1.0/DET_JACOB(XI,ETA,XP,YP) *              &
                  (-D_DETA(XI,XP)*DXI_QUAD_SHAPE(ETA,NSHP)     &
                   +D_DXI(ETA,XP)*DETA_QUAD_SHAPE(XI,NSHP))

  END FUNCTION DY_QUAD_SHAPE


  ! -------------------------------------------------------------
  ! -------------------------------------------------------------
  ! --- COORDINATE TRANSFORMATION FUNCTIONS
  ! -------------------------------------------------------------
  ! -------------------------------------------------------------

  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF CARTESIAN COORDINATE (X OR Y) WRT XI
  ! -------------------------------------------------------------
  REAL(8) FUNCTION D_DXI(ETA,P)
    REAL(8),INTENT(IN)::ETA,P(4)

    INTEGER::INDX

    D_DXI=0.0
    DO INDX=1,4
      D_DXI=D_DXI+P(INDX)*DXI_QUAD_SHAPE(ETA,INDX)
    END DO

  END FUNCTION D_DXI

  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF CARTESIAN COORDINATE (X OR Y) WRT ETA
  ! -------------------------------------------------------------
  REAL(8) FUNCTION D_DETA(XI,P)
    REAL(8),INTENT(IN)::XI,P(4)

    INTEGER::INDX

    D_DETA=0.0
    DO INDX=1,4
      D_DETA=D_DETA+P(INDX)*DETA_QUAD_SHAPE(XI,INDX)
    END DO

  END FUNCTION D_DETA

  ! -------------------------------------------------------------
  ! --- DETERMINANT OF THE JACOBIAN OF THE TRANSFORMATION
  ! -------------------------------------------------------------
  REAL(8) FUNCTION DET_JACOB(XI,ETA,XP,YP)
    REAL(8),INTENT(IN)::XI,ETA,XP(4),YP(4)

    DET_JACOB = D_DXI(ETA,XP)*D_DETA(XI,YP) - D_DXI(ETA,YP)*D_DETA(XI,XP)

  END FUNCTION DET_JACOB


  ! -------------------------------------------------------------
  ! -------------------------------------------------------------
  ! --- INTEGRANDS FOR QUADRILATERAL ELEMENTS
  ! -------------------------------------------------------------
  ! -------------------------------------------------------------

  ! -------------------------------------------------------------
  ! --- INTEGRAND FOR ROTATION TERM OVER QUADRILATERAL ELEMENTS
  ! -------------------------------------------------------------
  REAL(8) FUNCTION QUAD_FUNC_ROT(I,J,XI,ETA,XP,YP)
    REAL(8),INTENT(IN)::XI,ETA,XP(4),YP(4)
    INTEGER,INTENT(IN)::I,J

    QUAD_FUNC_ROT =  DY_QUAD_SHAPE(XI,ETA,XP,YP,I) * DX_QUAD_SHAPE(XI,ETA,XP,YP,J)    &
                    -DX_QUAD_SHAPE(XI,ETA,XP,YP,I) * DY_QUAD_SHAPE(XI,ETA,XP,YP,J)
    QUAD_FUNC_ROT = QUAD_FUNC_ROT * DET_JACOB(XI,ETA,XP,YP)

  END FUNCTION QUAD_FUNC_ROT

  ! -------------------------------------------------------------
  ! --- INTEGRAND FOR DIFFUSION TERM OVER QUADRILATERAL ELEMENTS
  ! -------------------------------------------------------------
  REAL(8) FUNCTION QUAD_FUNC_DIFF(I,J,XI,ETA,XP,YP)
    REAL(8),INTENT(IN)::XI,ETA,XP(4),YP(4)
    INTEGER,INTENT(IN)::I,J

    QUAD_FUNC_DIFF =  DX_QUAD_SHAPE(XI,ETA,XP,YP,I) * DX_QUAD_SHAPE(XI,ETA,XP,YP,J)  &
                     +DY_QUAD_SHAPE(XI,ETA,XP,YP,I) * DY_QUAD_SHAPE(XI,ETA,XP,YP,J)
    QUAD_FUNC_DIFF = QUAD_FUNC_DIFF * DET_JACOB(XI,ETA,XP,YP)

  END FUNCTION QUAD_FUNC_DIFF

  ! -------------------------------------------------------------
  ! --- INTEGRAND FOR AREA TERM OVER QUADRILATERAL ELEMENTS
  ! -------------------------------------------------------------
  REAL(8) FUNCTION QUAD_FUNC_AREA(I,J,XI,ETA,XP,YP)
    REAL(8),INTENT(IN)::XI,ETA, XP(4),YP(4)
    INTEGER,INTENT(IN)::I,J

    QUAD_FUNC_AREA = QUAD_SHAPE(XI,ETA,I) * DET_JACOB(XI,ETA,XP,YP)

  END FUNCTION QUAD_FUNC_AREA


  ! -------------------------------------------------------------
  ! -------------------------------------------------------------
  ! --- INTEGRATION OVER QUADRILATERAL ELEMENTS USING GAUSSIAN QUADRATURE
  ! -------------------------------------------------------------
  ! -------------------------------------------------------------
  REAL(8) FUNCTION QUAD_INTGRL(I,J,XP,YP,NPOINT,FUNC)
    REAL(8),INTENT(IN)::XP(4),YP(4)
    REAL(8),EXTERNAL::FUNC
    INTEGER,INTENT(IN)::I,J,NPOINT

    !Local variables
    REAL(8)::VPOINT,VPOINT1,VPOINT2,W1,W2

    !2-POINT GAUSSIAN QUADRATURE
    IF (NPOINT.EQ.2) THEN
      VPOINT=1d0/sqrt(3d0)
      QUAD_INTGRL =  FUNC(I,J,VPOINT,VPOINT,XP,YP)   &
                   + FUNC(I,J,VPOINT,-VPOINT,XP,YP)  &
                   + FUNC(I,J,-VPOINT,VPOINT,XP,YP)  &
                   + FUNC(I,J,-VPOINT,-VPOINT,XP,YP)

    !3-POINT GAUSSIAN QUADRATURE
    ELSEIF (NPOINT.EQ.3) THEN
      VPOINT1=0D0
      VPOINT2=0.774596669241483d0
      W1=0.8888888888888889d0
      W2=0.5555555555555556d0
      QUAD_INTGRL =  W1*W1*FUNC(I,J,VPOINT1,VPOINT1,XP,YP)   &
                   + W1*W2*FUNC(I,J,VPOINT1,VPOINT2,XP,YP)   &
                   + W1*W2*FUNC(I,J,VPOINT1,-VPOINT2,XP,YP)  &
                   + W2*W1*FUNC(I,J,VPOINT2,VPOINT1,XP,YP)   &
                   + W2*W2*FUNC(I,J,VPOINT2,VPOINT2,XP,YP)   &
                   + W2*W2*FUNC(I,J,VPOINT2,-VPOINT2,XP,YP)  &
                   + W2*W1*FUNC(I,J,-VPOINT2,VPOINT1,XP,YP)  &
                   + W2*W2*FUNC(I,J,-VPOINT2,VPOINT2,XP,YP)  &
                   + W2*W2*FUNC(I,J,-VPOINT2,-VPOINT2,XP,YP)

    !4-POINT GAUSSIAN QUADRATURE
    ELSEIF (NPOINT.EQ.4) THEN
      VPOINT1=0.339981043584856d0
      VPOINT2=0.861136311594053d0
      W1=0.652145154862546d0
      W2=0.347854845137454d0
      QUAD_INTGRL =  W1*W1*FUNC(I,J,VPOINT1,VPOINT1,XP,YP)   &
                   + W1*W1*FUNC(I,J,VPOINT1,-VPOINT1,XP,YP)  &
                   + W1*W2*FUNC(I,J,VPOINT1,VPOINT2,XP,YP)   &
                   + W1*W2*FUNC(I,J,VPOINT1,-VPOINT2,XP,YP)  &
                   + W1*W1*FUNC(I,J,-VPOINT1,VPOINT1,XP,YP)  &
                   + W1*W1*FUNC(I,J,-VPOINT1,-VPOINT1,XP,YP) &
                   + W1*W2*FUNC(I,J,-VPOINT1,VPOINT2,XP,YP)  &
                   + W1*W2*FUNC(I,J,-VPOINT1,-VPOINT2,XP,YP) &
                   + W2*W1*FUNC(I,J,VPOINT2,VPOINT1,XP,YP)   &
                   + W2*W1*FUNC(I,J,VPOINT2,-VPOINT1,XP,YP)  &
                   + W2*W2*FUNC(I,J,VPOINT2,VPOINT2,XP,YP)   &
                   + W2*W2*FUNC(I,J,VPOINT2,-VPOINT2,XP,YP)  &
                   + W2*W1*FUNC(I,J,-VPOINT2,VPOINT1,XP,YP)  &
                   + W2*W1*FUNC(I,J,-VPOINT2,-VPOINT1,XP,YP) &
                   + W2*W2*FUNC(I,J,-VPOINT2,VPOINT2,XP,YP)  &
                   + W2*W2*FUNC(I,J,-VPOINT2,-VPOINT2,XP,YP)

    END IF

  END FUNCTION QUAD_INTGRL




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISCELLENOUS METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INTERPOLATION COEEFICIENTS AT A GIVEN LOCATION IN A GIVEN CELL
  ! -------------------------------------------------------------
  SUBROUTINE FEInterpolate_AtCell(Grid,iElem,XP,YP,Coeff)
    CLASS(GridType),INTENT(IN) :: Grid
    INTEGER,INTENT(IN)         :: iElem
    REAL(8),INTENT(IN)         :: XP,YP
    REAL(8),INTENT(OUT)        :: Coeff(:)

    !Local variables
    INTEGER :: NVertex
    REAL(8) :: X(4),Y(4)

    !Get vertex coordinates
    NVertex      = Grid%NVertex(iElem)
    X(1:NVertex) = Grid%X(Grid%Vertex(1:NVertex,iElem))
    Y(1:NVertex) = Grid%Y(Grid%Vertex(1:NVertex,iElem))

    Coeff = InterpolationCoeffs(SIZE(Coeff),XP,YP,X,Y)

  END SUBROUTINE FEInterpolate_AtCell


  ! -------------------------------------------------------------
  ! --- INTERPOLATION COEEFICIENTS AT A GIVEN LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE FEInterpolate(Grid,XP,YP,iElemIndex,Nodes,Coeff)
    CLASS(GridType),INTENT(IN)      :: Grid
    REAL(8),INTENT(IN)              :: XP,YP
    INTEGER,INTENT(OUT)             :: iElemIndex
    INTEGER,ALLOCATABLE,INTENT(OUT) :: Nodes(:)
    REAL(8),ALLOCATABLE,INTENT(OUT) :: Coeff(:)

    !Local variables
    INTEGER :: NVertex
    REAL(8) :: X(4),Y(4)

    !Find the element the point belongs to
    iElemIndex = ContainedInElement(Grid,XP,YP)

    !Return if point was not located within the grid
    IF (iElemIndex .EQ. 0) RETURN

    !Identify nodes and interpolation coefficients
    NVertex      = Grid%NVertex(iElemIndex)
    ALLOCATE (Nodes(NVertex) , Coeff(NVertex))
    Nodes        = Grid%Vertex(1:NVertex,iElemIndex)
    X(1:NVertex) = Grid%X(Nodes)
    Y(1:NVertex) = Grid%Y(Nodes)
    Coeff        = InterpolationCoeffs(NVertex,XP,YP,X,Y)

  END SUBROUTINE FEInterpolate


  ! -------------------------------------------------------------
  ! --- COMPUTE FINITE ELEMENT INTERPOLATION COEFFICIENTS
  ! -------------------------------------------------------------
  FUNCTION InterpolationCoeffs(NVertex,XP,YP,X,Y) RESULT(Coeff)
    INTEGER,INTENT(IN) :: NVertex
    REAL(8),INTENT(IN) :: XP,YP,X(4),Y(4)
        REAL(8)            :: Coeff(NVertex)

    !Local variables
        REAL(8) :: XIJ,XJK,XKI,YIJ,YJK,YKI,XT,YT,A,B,BO,BX,BY,CO,CX,CY,C

    !Initialize
        Coeff = -1.0

    !Triangular element
    IF (NVertex .EQ. 3) THEN
      XIJ=X(1)-X(2)
      XJK=X(2)-X(3)
      XKI=X(3)-X(1)
      YIJ=Y(1)-Y(2)
      YJK=Y(2)-Y(3)
      YKI=Y(3)-Y(1)
      XT=(-X(1)*Y(3)+X(3)*Y(1)+YKI*XP-XKI*YP)/(-XKI*YJK+XJK*YKI)
      XT=MAX(0.0,XT)
      YT=( X(1)*Y(2)-X(2)*Y(1)+YIJ*XP-XIJ*YP)/(-XKI*YJK+XJK*YKI)
      YT=MAX(0.0,YT)
      Coeff(1) = 1.0-MIN(1.0,XT+YT)
      Coeff(2) = XT
      Coeff(3) = YT

    !Quadrilateral element
    ELSE
      A  = (Y(1)-Y(2))*(X(3)-X(4))-(Y(3)-Y(4))*(X(1)-X(2))
      BO = Y(1)*X(4)-Y(2)*X(3)+Y(3)*X(2)-Y(4)*X(1)
      BX = -Y(1)+Y(2)-Y(3)+Y(4)
      BY = X(1)-X(2)+X(3)-X(4)
      B  = BO+BX*XP+BY*YP
      CO = -(Y(1)+Y(2))*(X(3)+X(4))+(Y(3)+Y(4))*(X(1)+X(2))
      CX = Y(1)+Y(2)-Y(3)-Y(4)
      CY = -X(1)-X(2)+X(3)+X(4)
      C  = CO+2.0*(CX*XP+CY*YP)
      IF (A .EQ. 0.0) THEN
        IF (B .EQ. 0.0) RETURN
        XT = -C/(2.0*B)
      ELSE
        XT = B*B-A*C
        IF (XT .LT. 0.0) RETURN
        XT = (-B+SQRT(XT))/A
      END IF
      XT = MAX(-1.0,MIN(XT,1.0))
      A  = (Y(2)-Y(3))*(X(1)-X(4))-(Y(1)-Y(4))*(X(2)-X(3))
      BO = Y(1)*X(2)-Y(2)*X(1)+Y(3)*X(4)-Y(4)*X(3)
      BX = -Y(1)+Y(2)-Y(3)+Y(4)
      BY = X(1)-X(2)+X(3)-X(4)
      B  = BO+BX*XP+BY*YP
      CO = -(Y(1)+Y(4))*(X(2)+X(3))+(Y(2)+Y(3))*(X(1)+X(4))
      CX = Y(1)-Y(2)-Y(3)+Y(4)
      CY = -X(1)+X(2)+X(3)-X(4)
      C  = CO+2.0*(CX*XP+CY*YP)
      IF (A .EQ. 0.0) THEN
        IF (B .EQ. 0.0) RETURN
        YT = -C/(2.0*B)
      ELSE
        YT = B*B-A*C
        IF (YT .LT. 0.0) RETURN
        YT = (-B-SQRT(YT))/A
      END IF
      YT       = MAX(-1.0,MIN(YT,1.0))
      Coeff(1) = 0.25*(1.0-XT)*(1.0-YT)
      Coeff(2) = 0.25*(1.0+XT)*(1.0-YT)
      Coeff(3) = 0.25*(1.0+XT)*(1.0+YT)
      Coeff(4) = 0.25*(1.0-XT)*(1.0+YT)
    END IF

  END FUNCTION InterpolationCoeffs


  ! -------------------------------------------------------------
  ! --- CHECK IF A POINT (XP,YP) LIES IN A GIVEN ELEMENT
  ! -------------------------------------------------------------
  PURE FUNCTION IsPointInElement(Grid,XP,YP,iElem) RESULT(lIsIn)
    CLASS(GridType),INTENT(IN) :: Grid
    REAL(8),INTENT(IN)         :: XP,YP
    INTEGER,INTENT(IN)         :: iElem
    LOGICAL                    :: lIsIn
    
    !Local variables
    INTEGER :: NVertex,indxVertex,Vertex(4)
    REAL(8) :: X(4),Y(4),X1,Y1,X2,Y2,XX,YX,DotProduct
    
    !Initialize
    lIsIn = .TRUE.
    
    !Element vertices
    NVertex      = Grid%NVertex(iElem)
    Vertex       = Grid%Vertex(:,iElem)
    X(1:NVertex) = Grid%X(Vertex(1:NVertex))
    Y(1:NVertex) = Grid%Y(Vertex(1:NVertex))
    
    !Check
    DO indxVertex=1,NVertex
        IF (XP .EQ. X(indxVertex)) THEN
            IF (YP .EQ. Y(indxVertex)) EXIT
        END IF
        X1 = X(indxVertex)
        Y1 = Y(indxVertex)
        IF (indxVertex .LT. NVertex) THEN
            X2 = X(indxVertex+1)
            Y2 = Y(indxVertex+1)
            IF (XP .EQ. X2) THEN
                IF (YP .EQ. Y2) EXIT
            END IF        
        ELSE
            X2 = X(1)
            Y2 = Y(1)
        END IF
        CALL XPoint(X1,Y1,X2,Y2,XP,YP,XX,YX)
        DotProduct = DOT_PRODUCT([(XP-XX),(YP-YX)] , [(Y1-Y2),(X2-X1)])
        IF (DotProduct .LT. 0.0) THEN
            lIsIn = .FALSE.
            EXIT
        END IF
    END DO
    
  END FUNCTION IsPointInElement
  
  
  ! -------------------------------------------------------------
  ! --- FIND THE FIRST ELEMENT THAT A POINT (XP,YP) LIES IN - GATEWAY
  ! -------------------------------------------------------------
  FUNCTION ContainedInElement(Grid,XP,YP) RESULT(iElem)
    TYPE(GridType),INTENT(IN) :: Grid
    REAL(8),INTENT(IN)        :: XP,YP
    INTEGER                   :: iElem

!DIR$ IF (_OPENMP .NE. 0)
    !$ iElem = ContainedInElement_OMP(Grid,XP,YP)
!DIR$ ELSE
    iElem = ContainedInElement_Sequential(Grid,XP,YP)
!DIR$ END IF

  END FUNCTION ContainedInElement


  ! -------------------------------------------------------------
  ! --- FIND THE FIRST ELEMENT THAT A POINT (XP,YP) LIES IN - OPENMP VERSION
  ! -------------------------------------------------------------
  FUNCTION ContainedInElement_OMP(Grid,XP,YP) RESULT(iElem)
    TYPE(GridType),INTENT(IN) :: Grid
    REAL(8),INTENT(IN)        :: XP,YP
    INTEGER                   :: iElem

    !Local variables
    INTEGER           :: NVertex,indxElem,indxVertex,NElements,iEndElem,iBeginElem,Vertex(4)
    REAL(8)           :: DotProduct,X1,Y1,X2,Y2,XX,YX,X(4),Y(4)
    LOGICAL           :: lInThisElem
    INTEGER,PARAMETER :: iChunkSize = 5000

    !Initialize
    iElem      = 0
    NElements  = SIZE(Grid%NVertex)
    iBeginElem = 0

    DO
        iEndElem = iBeginElem + iChunkSize
        IF (iEndElem .GT. NElements) iEndElem = NElements
        iBeginElem = iBeginElem + 1

        !Iterate over elements
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem,NVertex,Vertex,X,Y,lInThisElem,indxVertex,X1,Y1,X2,Y2,XX,YX,DotProduct) IF(NElements > iChunkSize)
        DO indxElem=iBeginElem,iEndElem
            IF (iElem .GT. 0) CYCLE
            NVertex      = Grid%NVertex(indxElem)
            Vertex       = Grid%Vertex(:,indxElem)
            X(1:NVertex) = Grid%X(Vertex(1:NVertex))
            Y(1:NVertex) = Grid%Y(Vertex(1:NVertex))
            lInThisElem  = .TRUE.
            DO indxVertex=1,NVertex
                IF (XP .EQ. X(indxVertex)) THEN
                    IF (YP .EQ. Y(indxVertex)) EXIT
                END IF
                X1 = X(indxVertex)
                Y1 = Y(indxVertex)
                IF (indxVertex .LT. NVertex) THEN
                    X2 = X(indxVertex+1)
                    Y2 = Y(indxVertex+1)
                ELSE
                    X2 = X(1)
                    Y2 = Y(1)
                END IF
                CALL XPoint(X1,Y1,X2,Y2,XP,YP,XX,YX)
                DotProduct = DOT_PRODUCT([(XP-XX),(YP-YX)] , [(Y1-Y2),(X2-X1)])
                IF (DotProduct .LT. 0.0) THEN
                    lInThisElem = .FALSE.
                    EXIT
                END IF
            END DO
            IF (lInThisElem) THEN
                iElem = indxElem
                !$OMP FLUSH(iElem)
            END IF
        END DO
        !$OMP END PARALLEL DO

        IF (iElem .GT. 0) EXIT
        iBeginElem = iEndElem

    END DO

  END FUNCTION ContainedInElement_OMP


  ! -------------------------------------------------------------
  ! --- FIND THE FIRST ELEMENT THAT A POINT (XP,YP) LIES IN - SEQUENTIAL VERSION
  ! -------------------------------------------------------------
  FUNCTION ContainedInElement_Sequential(Grid,XP,YP) RESULT(iElem)
    TYPE(GridType),INTENT(IN) :: Grid
    REAL(8),INTENT(IN)        :: XP,YP
    INTEGER                   :: iElem

    !Local variables
    INTEGER :: NVertex,indxElem,indxVertex,Vertex(4)
    REAL(8) :: DotProduct,X1,Y1,X2,Y2,XX,YX,X(4),Y(4)
    LOGICAL :: lInThisElem

    !Initialize
    iElem = 0

    !Iterate over elements
    DO indxElem=1,SIZE(Grid%NVertex)
        NVertex      = Grid%NVertex(indxElem)
        Vertex       = Grid%Vertex(:,indxElem)
        X(1:NVertex) = Grid%X(Vertex(1:NVertex))
        Y(1:NVertex) = Grid%Y(Vertex(1:NVertex))
        lInThisElem  = .TRUE.
        DO indxVertex=1,NVertex
            IF (XP .EQ. X(indxVertex)) THEN
                IF (YP .EQ. Y(indxVertex)) EXIT
            END IF
            X1 = X(indxVertex)
            Y1 = Y(indxVertex)
            IF (indxVertex .LT. NVertex) THEN
                X2 = X(indxVertex+1)
                Y2 = Y(indxVertex+1)
                IF (XP .EQ. X2) THEN
                    IF (YP .EQ. Y2) EXIT
                END IF        
            ELSE
                X2 = X(1)
                Y2 = Y(1)
            END IF
            CALL XPoint(X1,Y1,X2,Y2,XP,YP,XX,YX)
            DotProduct = DOT_PRODUCT([(XP-XX),(YP-YX)] , [(Y1-Y2),(X2-X1)])
            IF (DotProduct .LT. 0.0) THEN
                lInThisElem = .FALSE.
                EXIT
            END IF
        END DO
        IF (lInThisElem) THEN
            iElem = indxElem
            RETURN
        END IF
    END DO

  END FUNCTION ContainedInElement_Sequential
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT FINDS INTERSECTION BETWEEN A LINE GIVEN BY [(X1,Y1),(X2,Y2)]
  ! ---  AND A LINE PERPENDICULAR TO IT WHICH ALSO CROSSES (XP,YP)
  ! -------------------------------------------------------------
  PURE SUBROUTINE XPoint(X1,Y1,X2,Y2,XP,YP,XX,YX)
    REAL(8),INTENT(IN)  :: X1,Y1,X2,Y2,XP,YP
    REAL(8),INTENT(OUT) :: XX,YX

    !Local variables
    REAL(8)::SLOPEL,SLOPEG

    IF (X1.NE.X2 .AND. Y1.NE.Y2) THEN
        SLOPEL = (Y2-Y1)/(X2-X1)
        SLOPEG = -1D0/SLOPEL
        XX     = (SLOPEL*X1-SLOPEG*XP+YP-Y1)/(SLOPEL-SLOPEG)
        YX     = SLOPEG*XX - SLOPEG*XP + YP
    ELSEIF (X1 .EQ. X2) THEN
        XX = X1
        YX = YP
    ELSEIF (Y1 .EQ. Y2) THEN
        XX = XP
        YX = Y1
    END IF

  END SUBROUTINE XPoint


END MODULE
