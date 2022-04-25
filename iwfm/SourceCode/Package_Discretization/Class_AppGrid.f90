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
MODULE Class_AppGrid
  !$ USE OMP_LIB
  USE IOInterface       , ONLY: GenericFileType           
  USE GeneralUtilities
  USE MessageLogger     , ONLY: LogMessage                , &
                                SetLastMessage            , &
                                MessageArray              , &
                                f_iWarn                   , &
                                f_iFatal                  , &
                                EchoProgress
  USE GenericLinkedList , ONLY: GenericLinkedListType
  USE Class_Grid        
  USE Class_AppFace     , ONLY: AppFaceType
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
  PUBLIC :: AppGridType                                , &
            AppNodeType                                , &
            AppElementType                             , &
            AppFaceType                               
          

  ! -------------------------------------------------------------
  ! --- AQUIFER NODE DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppNodeType
      INTEGER             :: ID                     =  0       !Node ID defined by user
      REAL(8)             :: Area                   =  0.0     !Area of influence
      LOGICAL             :: BoundaryNode           = .FALSE.  !If located on domain boundary
      INTEGER             :: NConnectedNode         =  0       !Number of connected nodes to a node
      INTEGER             :: NFaceID                =  0       !Number of faces connected at a node
      INTEGER,ALLOCATABLE :: SurroundingElement(:)             !Indices of surrounding elements
      INTEGER,ALLOCATABLE :: ConnectedNode(:)                  !Indices of nodes that the node is connected to
      INTEGER,ALLOCATABLE :: FaceID(:)                         !Indices for element faces that meet at the node
      INTEGER,ALLOCATABLE :: ElemID_OnCCWSide(:)               !Index of element that is located on the counter-clockwise side of the face
      REAL(8),ALLOCATABLE :: IrrotationalCoeff(:)              !Coefficents to represent irrotationality at a node
  END TYPE AppNodeType


  ! -------------------------------------------------------------
  ! --- AQUIFER ELEMENT DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppElementType
      INTEGER             :: ID                              =  0       !Element ID defined by user
      INTEGER             :: Subregion                       =  0       !Subregion number that the element belongs to
      REAL(8)             :: Area                            =  0.0     !Element area
      INTEGER,ALLOCATABLE :: FaceID(:)                                  !Face IDs sorrunding the element
      REAL(8),ALLOCATABLE :: VertexArea(:)                              !Integral of shape function that gives the corresponding area for a vertex
      REAL(8),ALLOCATABLE :: VertexAreaFraction(:)                      !Fraction of vertex area to element area
      REAL(8),ALLOCATABLE :: Integral_DELShpI_DELShpJ(:)                !Integral of DELwi * DELwj
      REAL(8),ALLOCATABLE :: Integral_Rot_DELShpI_DELShpJ(:)            !Integral of DEL'wi * DELwj; used in computing locally-conservative face flows in Z-Budget
  END TYPE AppElementType


  ! -------------------------------------------------------------
  ! --- SUBREGION DATA TYPES
  ! -------------------------------------------------------------
  !Neighbor subregion type
  TYPE NeighborRegionType
    INTEGER             :: RegionNo         =  0       !Adjacent subregion index
    INTEGER             :: NRegBndFace      =  0       !Number of element faces at the subregion interface
    INTEGER,ALLOCATABLE :: RegBndFace(:)               !List of element faces at the subregion interface        
  END TYPE NeighborRegionType
  
  !Subregion data type
  INTEGER,PARAMETER :: iRegionNameLen = 50
  TYPE AppRegionType
      INTEGER                              :: ID                  = 0       !Subregion ID defined by user
      CHARACTER(LEN=iRegionNameLen)        :: Name                = ''      !Name of the region
      INTEGER                              :: NRegionElements     = 0       !Number elements in the region
      INTEGER                              :: NNeighborRegions    = 0       !Number of adjacent regions
      REAL(8)                              :: Area                = 0.0     !Subregion area
      INTEGER,ALLOCATABLE                  :: RegionElements(:)             !List of elements in the subregion
      TYPE(NeighborRegionType),ALLOCATABLE :: NeighborRegions(:)            !List of neighbor regions and relevant information
  END TYPE AppRegionType
    
    
  ! -------------------------------------------------------------
  ! --- AQUIFER GRID DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GridType) :: AppGridType
    INTEGER                          :: NNodes              =  0
    INTEGER                          :: NElements           =  0
    INTEGER                          :: NFaces              =  0
    INTEGER                          :: NSubregions         =  0
    INTEGER                          :: NSumConnectedNode   =  0
    INTEGER                          :: NBoundaryFaces      =  0
    INTEGER,ALLOCATABLE              :: BoundaryFaceList(:)
    TYPE(AppNodeType),ALLOCATABLE    :: AppNode(:)          
    TYPE(AppElementType),ALLOCATABLE :: AppElement(:)    
    TYPE(AppFaceType)                :: AppFace          
    TYPE(AppRegionType),ALLOCATABLE  :: AppSubregion(:)
  CONTAINS
    PROCEDURE,PASS :: ReadAppGridData
    PROCEDURE,PASS :: ReadProcessedAppGridData
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: GetNElements                       
    PROCEDURE,PASS :: GetNNodes                          
    PROCEDURE,PASS :: GetNTriElements                    
    PROCEDURE,PASS :: GetNQuadElements                   
    PROCEDURE,PASS :: GetNSubregions 
    PROCEDURE,PASS :: GetNodeIDs
    PROCEDURE,PASS :: GetElementIDs
    PROCEDURE,PASS :: GetSubregionIDs
    PROCEDURE,PASS :: GetSubregionAreaForOne                   
    PROCEDURE,PASS :: GetSubregionAreaForAll                   
    PROCEDURE,PASS :: GetSubregionNames                  
    PROCEDURE,PASS :: GetSubregionInterfaces             
    PROCEDURE,PASS :: GetElementVertex 
    PROCEDURE,PASS :: GetElementVertexIndex
    PROCEDURE,PASS :: GetElementGivenVertex
    PROCEDURE,PASS :: GetElementGivenVertices
    PROCEDURE,PASS :: GetFaceGivenNodes
    PROCEDURE,PASS :: GetNodeXY
    PROCEDURE,PASS :: GetBoundaryLengthAtNode
    PROCEDURE,PASS :: WritePreProcessedData 
    PROCEDURE,PASS :: IsBoundaryNode
    PROCEDURE,PASS :: ElemData_To_NodeData               
    PROCEDURE,PASS :: NodeData_To_ElemData               
    PROCEDURE,PASS :: AreaAverage_ElemData_From_NodeData 
    PROCEDURE,PASS :: AccumElemValuesToSubregions        
    PROCEDURE,PASS :: AccumSomeElemValuesToSubregions    
    PROCEDURE,PASS :: AccumNodeValuesToSubregions        
    PROCEDURE,PASS :: AccumSomeNodeValuesToSubregions    
    GENERIC        :: New              => ReadAppGridData          , &
                                          ReadProcessedAppGridData
    GENERIC        :: GetSubregionArea => GetSubregionAreaForOne   , &
                                          GetSubregionAreaForAll
  END TYPE AppGridType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppGrid::'



CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW APPLICATION NODE
  ! -------------------------------------------------------------
  FUNCTION AppNode_New(ID,Area,BoundaryNode,NConnectedNode,NFaceID,SurroundingElement,ConnectedNode,FaceID,ElemID_OnCCWSide) RESULT(AppNode)
    REAL(8),INTENT(IN) :: Area
    LOGICAL,INTENT(IN) :: BoundaryNode
    INTEGER,INTENT(IN) :: ID,NConnectedNode,NFaceID,SurroundingElement(:),ConnectedNode(:),FaceID(:),ElemID_OnCCWSide(:)
    TYPE(AppNodeType)  :: AppNode

    AppNode%ID              = ID
    AppNode%Area            = Area
    AppNode%BoundaryNode    = BoundaryNode
    AppNode%NConnectedNode  = NConnectedNode
    AppNode%NFaceID         = NFaceID                               
    ALLOCATE (AppNode%SurroundingElement(SIZE(SurroundingElement))) ; AppNode%SurroundingElement = SurroundingElement
    ALLOCATE (AppNode%ConnectedNode(NConnectedNode))                ; AppNode%ConnectedNode      = ConnectedNode
                                                                      CALL ShellSort(AppNode%ConnectedNode)
    ALLOCATE (AppNode%FaceID(NFaceID))                              ; AppNode%FaceID             = FaceID
    ALLOCATE (AppNode%ElemID_OnCCWSide(SIZE(ElemID_OnCCWSide)))     ; AppNode%ElemID_OnCCWSide   = ElemID_OnCCWSide

  END FUNCTION AppNode_New


  ! -------------------------------------------------------------
  ! --- NEW APPLICATION ELEMENT
  ! -------------------------------------------------------------
  FUNCTION AppElement_New(ID,Subregion,Area,FaceID,VertexArea,VertexAreaFraction,Integral_DELShpI_DELShpJ,Integral_Rot_DELShpI_DELShpJ) RESULT(AppElement)
    INTEGER,INTENT(IN)   :: ID,Subregion
    REAL(8),INTENT(IN)   :: Area
    INTEGER,INTENT(IN)   :: FaceID(:)
    REAL(8),INTENT(IN)   :: VertexArea(:)
    REAL(8),INTENT(IN)   :: VertexAreaFraction(:)
    REAL(8),INTENT(IN)   :: Integral_DELShpI_DELShpJ(:)
    REAL(8),INTENT(IN)   :: Integral_Rot_DELShpI_DELShpJ(:)
    TYPE(AppElementType) :: AppElement

    AppElement%ID        = ID
    AppElement%Subregion = Subregion
    AppElement%Area      = Area
    ALLOCATE (AppElement%Integral_Rot_DELShpI_DelShpJ(SIZE(Integral_Rot_DELShpI_DELShpJ))) ; AppElement%Integral_Rot_DELShpI_DELShpJ = Integral_Rot_DELShpI_DELShpJ
    ALLOCATE (AppElement%Integral_DELShpI_DELShpJ(SIZE(Integral_DELShpI_DELShpJ)))         ; AppElement%Integral_DELShpI_DELShpJ     = Integral_DELShpI_DELShpJ
    ALLOCATE (AppElement%VertexAreaFraction(SIZE(VertexAreaFraction)))                     ; AppElement%VertexAreaFraction           = VertexAreaFraction
    ALLOCATE (AppElement%VertexArea(SIZE(VertexArea)))                                     ; AppElement%VertexArea                   = VertexArea
    ALLOCATE (AppElement%FaceID(SIZE(FaceID)))                                             ; AppElement%FaceID                       = FaceID

  END FUNCTION AppElement_New


  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL APPLICATION GRID
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppGrid)
    CLASS(AppGridType) :: AppGrid
    
    !Local variables
    INTEGER           :: ErrorCode
    TYPE(AppGridType) :: Dummy
    
    CALL AppGrid%GridType%KillGrid()
    CALL AppGrid%AppFace%Kill()
    DEALLOCATE (AppGrid%AppNode       , &
                AppGrid%AppElement    , &
                AppGrid%AppSubregion  , &
                STAT=ErrorCode        )
    
    SELECT TYPE (p => AppGrid)
        TYPE IS (AppGridType)
            p = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET NODE IDs
  ! -------------------------------------------------------------
  SUBROUTINE GetNodeIDs(AppGrid,iIDs)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(OUT)           :: iIDs(AppGrid%NNodes)
    
    iIDs = AppGrid%AppNode%ID
    
  END SUBROUTINE GetNodeIDs
    
    
  ! -------------------------------------------------------------
  ! --- GET ELEMENT IDs
  ! -------------------------------------------------------------
  SUBROUTINE GetElementIDs(AppGrid,iIDs)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(OUT)           :: iIDs(AppGrid%NElements)
    
    iIDs = AppGrid%AppElement%ID
    
  END SUBROUTINE GetElementIDs
    
    
  ! -------------------------------------------------------------
  ! --- GET SUBREGION IDs
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionIDs(AppGrid,iIDs)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(OUT)           :: iIDs(AppGrid%NSubregions)
    
    iIDs = AppGrid%AppSubregion%ID
    
  END SUBROUTINE GetSubregionIDs
    
    
  ! -------------------------------------------------------------
  ! --- GET THE LENGTH OF BOUNDARY ASSOCIATED WITH A BOUNDARY NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetBoundaryLengthAtNode(AppGrid,iNode) RESULT(rLength)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: iNode
    REAL(8)                       :: rLength
    
    !Local variables
    INTEGER :: indxFace,iFaceID
    
    !Initialize
    rLength = 0.0
    
    !If node is not a boundary node, return 0
    IF (.NOT. AppGrid%IsBoundaryNode(iNode)) RETURN
    
    !Calculate length as the sum of half faces
    DO indxFace=1,AppGrid%AppNode(iNode)%NFaceID
        iFaceID = AppGrid%AppNode(iNode)%FaceID(indxFace)
        IF (AppGrid%AppFace%BoundaryFace(iFaceID)) rLength = rLength + 0.5 * AppGrid%AppFace%Length(iFaceID)
    END DO
    
  END FUNCTION GetBoundaryLengthAtNode
  
  
  ! -------------------------------------------------------------
  ! --- GET THE FACE NUMBER GIVEN THE NODES
  ! -------------------------------------------------------------
  FUNCTION GetFaceGivenNodes(AppGrid,Nodes) RESULT(FaceID)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: Nodes(2)
    INTEGER                       :: FaceID

    !Initialize
    FaceID = AppGrid%AppFace%GetFaceGivenNodes(Nodes)

  END FUNCTION GetFaceGivenNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT GIVEN VERTICES
  ! -------------------------------------------------------------
  FUNCTION GetElementGivenVertices(AppGrid,Vertices) RESULT(iElem)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: Vertices(:)
    INTEGER                       :: iElem
    
    !Local variables
    INTEGER :: indxElem,Vertex(4),iSize,indxNode
    LOGICAL :: lFound(SIZE(Vertices))
    
    !Initialize
    iElem = 0
    iSize = SIZE(Vertices)
    
    !Make sure no more than 4 vertices are specified
    IF (iSize .GT. 4) RETURN
    
    !Find element
    DO indxElem=1,AppGrid%NElements
      IF (AppGrid%NVertex(indxElem) .LT. iSize) CYCLE
      Vertex  = AppGrid%Vertex(:,indxElem)
      lFound  = .FALSE.
      DO indxNode=1,iSize
        IF (ANY(Vertices(indxNode) .EQ. Vertex)) THEN
            lFound(indxNode) =.TRUE.
        ELSE
            EXIT
        END IF
      END DO
      IF (ALL(lFound)) THEN
        iElem = indxElem
        RETURN
      END IF
    END DO
    
  END FUNCTION GetElementGivenVertices
  

  ! -------------------------------------------------------------
  ! --- GET ELEMENT GIVEN ONE VERTEX
  ! --- (Returns the first element that includes the vertex)
  ! -------------------------------------------------------------
  FUNCTION GetElementGivenVertex(AppGrid,iVertex) RESULT(iElem)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: iVertex
    INTEGER                       :: iElem
    
    !Local variables
    INTEGER :: indxElem,iVertices(4)
    
    !Initialize
    iElem = 0
    
    !Find element
    DO indxElem=1,AppGrid%NElements
      iVertices  = AppGrid%Vertex(:,indxElem)
      IF (LocateInList(iVertex,iVertices) .GT. 0) THEN
          iElem = indxElem
          RETURN
      END IF
    END DO
    
  END FUNCTION GetElementGivenVertex
  

  ! -------------------------------------------------------------
  ! --- GET SUBREGION NAMES
  ! -------------------------------------------------------------
  FUNCTION GetSubregionNames(AppGrid) RESULT(Names)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    CHARACTER(LEN=iRegionNameLen) :: Names(AppGrid%NSubregions)
    
    Names = AppGrid%AppSubregion%Name
    
  END FUNCTION GetSubregionNames
  

  ! -------------------------------------------------------------
  ! --- GET AREA OF ONE SUBREGION
  ! -------------------------------------------------------------
  FUNCTION GetSubregionAreaForOne(AppGrid,indxRegion) RESULT(Area)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: indxRegion
    REAL(8)                       :: Area
    
    Area = AppGrid%AppSubregion(indxRegion)%Area
  
  END FUNCTION GetSubregionAreaForOne
  
  
  ! -------------------------------------------------------------
  ! --- GET AREA OF ALL SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION GetSubregionAreaForAll(AppGrid) RESULT(pArea)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    REAL(8)                       :: pArea(AppGrid%NSubregions)
    
    pArea = AppGrid%AppSubregion%Area
  
  END FUNCTION GetSubregionAreaForAll
  
  
  ! -------------------------------------------------------------
  ! --- GET FACES BETWEEN TWO SUBREGIONS
  ! --- *** Note: Area outside the model domain has a subregion 
  ! ---           number of NSubregions+1
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionInterfaces(AppGrid,iRegion1,iRegion2,Faces) 
    CLASS(AppGridType),TARGET,INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)                   :: iRegion1,iRegion2
    INTEGER,ALLOCATABLE,INTENT(OUT)      :: Faces(:)
    
    !Local variables
    INTEGER :: ErrorCode,indxRegion
    TYPE(AppregionType),POINTER :: pRegion
    TYPE(NeighborRegionType),POINTER :: pNeighborRegion
    
    !Deallocate Faces
    DEALLOCATE (Faces , STAT=ErrorCode)
    
    !Initialize
    pRegion => AppGrid%AppSubregion(iRegion1)
    
    !Locate the iRegion2 information for iRegion1
    DO indxRegion=1,pRegion%NNeighborRegions
      pNeighborRegion => pRegion%NeighborRegions(indxRegion)
      IF (pNeighborRegion%RegionNo .EQ. iRegion2) THEN
        ALLOCATE (Faces(pNeighborRegion%NRegBndFace))
        Faces = pNeighborRegion%RegBndFace
      END IF
    END DO
    
    NULLIFY (pRegion , pNeighborRegion)
    
  END SUBROUTINE GetSubregionInterfaces
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION GetNSubregions(AppGrid) RESULT(NReg)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER                       :: NReg
    
    NReg = AppGrid%NSubregions
    
  END FUNCTION GetNSubregions
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES
  ! -------------------------------------------------------------
  FUNCTION GetNNodes(AppGrid) RESULT(NodeCount)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER                       :: NodeCount

    NodeCount = AppGrid%NNodes

  END FUNCTION GetNNodes


  ! -------------------------------------------------------------
  ! --- GET ALL NODE COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetNodeXY(AppGrid,X,Y)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)           :: X(AppGrid%NNodes) , Y(AppGrid%NNodes)

    X = AppGrid%X
    Y = AppGrid%Y

  END SUBROUTINE GetNodeXY


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION GetNElements(AppGrid) RESULT(ElementCount)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER                       :: ElementCount

    ElementCount = AppGrid%NElements

  END FUNCTION GetNElements


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TRIANGULAR ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION GetNTriElements(AppGrid) RESULT(TriElementCount)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER                       :: TriElementCount

    TriElementCount = CountTriElements(AppGrid%GridType)

  END FUNCTION GetNTriElements


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF QUADRILATERAL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION GetNQuadElements(AppGrid) RESULT(QuadElementCount)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER                       :: QuadElementCount

    QuadElementCount = CountQuadElements(AppGrid%GridType)

  END FUNCTION GetNQuadElements


  ! -------------------------------------------------------------
  ! --- GET INDEX OF A VERTEX WITHIN THE LIST OF VERTICES FOR AN ELEMENT
  ! -------------------------------------------------------------
  FUNCTION GetElementVertexIndex(AppGrid,iVertex,iElem) RESULT(iIndex) 
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: iVertex,iElem
    INTEGER                       :: iIndex
    
    !Local variables
    INTEGER :: Vertex(4)
    
    Vertex = AppGrid%Vertex(:,iElem)
    iIndex = LocateInList(iVertex,Vertex)
        
  END FUNCTION GetElementVertexIndex
  
  
  ! -------------------------------------------------------------
  ! --- GET SPECIFIC ELEMENT CONFIGURATION DATA
  ! -------------------------------------------------------------
  FUNCTION GetElementVertex(AppGrid,indxElem) RESULT(Vertex) 
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: indxElem
    INTEGER                       :: Vertex(4)

    Vertex = AppGrid%Vertex(:,indxElem)

  END FUNCTION GetElementVertex




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA FILE READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ GRID DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadAppGridData(AppGrid,NodeFileName,ElementConfigFileName,iStat) 
    CLASS(AppGridType),INTENT(OUT) :: AppGrid 
    CHARACTER(LEN=*),INTENT(IN)    :: NodeFileName , ElementConfigFileName
    INTEGER,INTENT(OUT)            :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+15)              :: ThisProcedure = ModName // 'ReadAppGridData'
    INTEGER                                   :: ErrorCode
    INTEGER,ALLOCATABLE                       :: iElemSubregionIDs(:),iSubregionIDs(:),NVertex(:),Vertex_IDs(:,:),NodeID(:),ElemID(:)
    REAL(8),ALLOCATABLE                       :: X(:),Y(:)
    CHARACTER(LEN=iRegionNameLen),ALLOCATABLE :: cSubregionNames(:)
    
    !Initialize
    iStat = 0
    
    !Check if grid is initialized
    IF (AppGrid%NNodes .NE. 0) THEN
        CALL SetLastMessage('An application grid that is already defined is being re-defined!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Print progress
    CALL EchoProgress('Instantiating application grid')

    !Read grid data     
    CALL ReadNodeData(NodeFileName,NodeID,X,Y,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadElementConfigData(ElementConfigFileName,ElemID,NVertex,Vertex_IDs,iElemSubregionIDs,iSubregionIDs,cSubregionNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Instantiate application grid
    CALL ConstructAppGrid(X,Y,NVertex,Vertex_IDs,NodeID,ElemID,iElemSubregionIDs,iSubregionIDs,cSubregionNames,AppGrid,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Free memory
    DEALLOCATE (X , Y , NVertex , Vertex_IDs ,  iElemSubregionIDs , cSubregionNames , iSubregionIDs , STAT=ErrorCode)

  END SUBROUTINE ReadAppGridData
    

  ! -------------------------------------------------------------
  ! --- READ NODE DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadNodeData(FileName,ID,X,Y,iStat)
    CHARACTER(LEN=*),INTENT(IN)     :: FileName
    INTEGER,ALLOCATABLE,INTENT(OUT) :: ID(:)
    REAL(8),ALLOCATABLE,INTENT(OUT) :: X(:),Y(:)
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+12) :: ThisProcedure = ModName // 'ReadNodeData'
    TYPE(GenericFileType)        :: NodeDataFile
    REAL(8),ALLOCATABLE          :: Dummy2DRealArray(:,:)
    REAL(8)                      :: Factor
    INTEGER                      :: indx,NNodes,ErrorCode,indx1,ID1,ID2
    
    !Initialize
    iStat = 0

    !Open the node data file for reading
    CALL NodeDataFile%New(FileName, InputFile=.TRUE., IsTSFile=.FALSE., Descriptor='Node coordinates data file', FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Read number of nodes and conversion factor
    CALL NodeDataFile%ReadData(NNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN  !Number of nodes
    CALL NodeDataFile%ReadData(Factor,iStat)  ;  IF (iStat .EQ. -1) RETURN  !Factor to convert node coordinates
    
    !Allocate memory for node data set
    ALLOCATE (X(NNodes) , Y(NNodes) , ID(NNodes) , STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory for grid nodes!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Read node coordinates
    CALL AllocArray(Dummy2DRealArray,NNodes,3,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL NodeDataFile%ReadData(Dummy2DRealArray,iStat)              ;  IF (iStat .EQ. -1) RETURN

    !Check for errors; if no errors save node data
    DO indx=1,NNodes
        !Node ID
        ID(indx) = INT(Dummy2DRealArray(indx,1))
        ID1      = ID(indx)
      
        !Coordinates
        X(indx) = Dummy2DRealArray(indx,2) * Factor 
        Y(indx) = Dummy2DRealArray(indx,3) * Factor
        
        DO indx1=1,indx-1
            ID2 = ID(indx1)
            
            !Make sure same node ID is not entered more than once
            IF (ID1 .EQ. ID2) THEN
                 CALL SetLastMessage('Node ID ' // TRIM(IntToText(ID1)) // ' is defined more than once!',f_iFatal,ThisProcedure)
                 iStat = -1
                 RETURN
            END IF
       
            !Check if two nodes have the same coordinates
            IF (X(indx1) .EQ. X(indx)) THEN
                IF (Y(indx1) .EQ. Y(indx)) THEN
                    CALL SetLastMessage('Nodes '//TRIM(IntToText(ID1))//' and '//TRIM(IntToText(ID2))//' have the same coordinates!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END IF
        END DO
      
    END DO

    !Release memory from dummy array
    DEALLOCATE (Dummy2DRealArray , STAT=ErrorCode)

    !Close file
    CALL NodeDataFile%Kill()

  END SUBROUTINE ReadNodeData


  ! -------------------------------------------------------------
  ! --- READ ELEMENT CONFIGURATION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadElementConfigData(FileName,ID,NVertex,Vertex_ID,iElemSubregionIDs,iSubregionIDs,cSubregionNames,iStat) 
    CHARACTER(LEN=*),INTENT(IN)                           :: FileName
    INTEGER,ALLOCATABLE,INTENT(OUT)                       :: ID(:),NVertex(:),Vertex_ID(:,:),iElemSubregionIDs(:),iSubregionIDs(:)
    CHARACTER(LEN=iRegionNameLen),ALLOCATABLE,INTENT(OUT) :: cSubregionNames(:)
    INTEGER,INTENT(OUT)                                   :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName//'ReadElementConfigData'
    TYPE(GenericFileType)        :: ElementDataFile
    INTEGER,ALLOCATABLE          :: Dummy2DIntArray(:,:),TDummy2DIntArray(:,:),UniqueSubregionID(:)
    INTEGER                      :: indx,indx1,NElements,ErrorCode,NRegions,ID1,ID2,iCheck,iLen
    CHARACTER(LEN=500)           :: ALine
    
    !Initailize
    iStat = 0

    !Open the element configuration file for reading
    CALL ElementDataFile%New(FileName, InputFile=.TRUE., IsTSFile=.FALSE., Descriptor='Element configuration data file', FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Read number of elements
    CALL ElementDataFile%ReadData(NElements,iStat)       
    IF (iStat .EQ. -1) RETURN
    
    !Read number of subregions
    CALL ElementDataFile%ReadData(NRegions,iStat)       
    IF (iStat .EQ. -1) RETURN

    !Allocate memory for the element data set
    ALLOCATE (ID(NElements) , NVertex(NElements) , Vertex_ID(4,NElements) , iElemSubregionIDs(NElements) , cSubregionNames(NRegions) , STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory for grid elements!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read subregion names
    !BACKWARD COMPATIBILITY: Check if ID numbers are provided along with subregion names 
    CALL ElementDataFile%ReadData(iCheck,iStat)
    CALL ElementDataFile%BackspaceFile()
    IF (iStat .EQ. 0) THEN
        ALLOCATE(iSubregionIDs(NRegions))
        DO indx=1,NRegions
            CALL ElementDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL CleanSpecialCharacters(ALine)
            ALine = ADJUSTL(StripTextUntilCharacter(ALine,"/"))
            READ (ALine,*) iSubregionIDs(indx)
            iLen = LEN_TRIM(IntToText(iSubregionIDs(indx)))
            cSubregionNames(indx) = ADJUSTL(ALine(iLen+1:))
        END DO
    ELSE 
        ALLOCATE(iSubregionIDs(0))
        DO indx=1,NRegions
            CALL ElementDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL CleanSpecialCharacters(ALine)
            ALine = StripTextUntilCharacter(ALine,"/")
            cSubregionNames(indx) = ADJUSTL(ALine)
        END DO
    END IF
    
    !Read element vertices and subregion number
    CALL AllocArray(Dummy2DIntArray,NElements,6,ThisProcedure,iStat)   ;  IF (iStat .EQ. -1) RETURN
    CALL AllocArray(TDummy2DIntArray,6,NElements,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ElementDataFile%ReadData(Dummy2DIntArray,iStat)               ;  IF (iStat .EQ. -1) RETURN
    TDummy2DIntArray = TRANSPOSE(Dummy2DIntArray)

    !Check for consistency, transfer info to data type
    DO indx=1,NElements
        !Element ID
        ID(indx) = INT(TDummy2DIntArray(1,indx))
        ID1      = ID(indx)

        !Nodes should not repeat
        DO indx1=2,5
            IF (ANY(TDummy2DIntArray(indx1,indx) .EQ. TDummy2DIntArray(indx1+1:5,indx))) THEN
                CALL SetLastMessage('Repeating node numbers at element '//TRIM(IntToText(ID1))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Same element ID should not be defined
        DO indx1=1,indx-1
            ID2 = ID(indx1)
            IF (ID1 .EQ. ID2) THEN
                 CALL SetLastMessage('Element ID ' // TRIM(IntToText(ID1)) // ' is defined more than once!',f_iFatal,ThisProcedure)
                 iStat = -1
                 RETURN                
            END IF
        END DO
        
        !Subregion number should be non-zero
        IF (TDummy2DIntArray(6,indx) .LE. 0) THEN
             MessageArray(1) = 'Subregion numbers should be greater than zero!'
             MessageArray(2) = 'Element number  = '//TRIM(IntToText(ID1))
             MessageArray(3) = 'Subregion number= '//TRIM(IntToText(TDummy2DIntArray(6,indx)))
             CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
             iStat = -1
             RETURN
        END IF   
        
        !If made to this point, transfer data to permenant array
        IF (TDummy2DIntArray(5,indx) .EQ. 0) THEN
            NVertex(indx) = 3  
        ELSE
            NVertex(indx) = 4  
        END IF
        Vertex_ID(:,indx)       = TDummy2DIntArray(2:5,indx)
        iElemSubregionIDs(indx) = TDummy2DIntArray(6,indx)
    END DO
    
    !Release memory from dummy array
    DEALLOCATE (Dummy2DIntArray , TDummy2DIntArray , UniqueSubregionID , STAT=ErrorCode)

    !Close file 
    CALL ElementDataFile%Kill()

  END SUBROUTINE ReadElementConfigData


  ! -------------------------------------------------------------
  ! --- READ GRID DATA FROM BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadProcessedAppGridData(AppGrid,InFile,iStat) 
    CLASS(AppGridType),INTENT(OUT) :: AppGrid
    TYPE(GenericFileType)          :: InFile
    INTEGER,INTENT(OUT)            :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+24)  :: ThisProcedure = ModName//'ReadProcessedAppGridData'
    INTEGER                       :: indx,indx1,NBndFace,ErrorCode
    INTEGER,ALLOCATABLE           :: NVertex(:),Vertex(:,:)
    REAL(8),ALLOCATABLE           :: X(:),Y(:)
    
    !Initialize
    iStat = 0

    !Check if grid is initialized
    IF (AppGrid%NNodes .GT. 0) THEN
        CALL SetLastMessage('An application grid that is already defined is being re-defined!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read grid dimensions
    CALL InFile%ReadData(AppGrid%NNodes,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGrid%NElements,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGrid%NFaces,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGrid%NSubregions,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppGrid%NBoundaryFaces,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Read base grid node data
    ALLOCATE (X(AppGrid%NNodes) , Y(AppGrid%NNodes))
    CALL InFile%ReadData(X,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(Y,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read base grid element data
    ALLOCATE (NVertex(AppGrid%NElements) , Vertex(4,AppGrid%NElements))
    CALL InFile%ReadData(NVertex,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(Vertex,iStat)   ;  IF (iStat .EQ. -1) RETURN
    
    !Base grid
    CALL AppGrid%GridType%Init(X,Y,NVertex,Vertex,iStat)
    IF (iStat .EQ. -1) RETURN

    !Read app. grid nodal data
    ALLOCATE (AppGrid%AppNode(AppGrid%NNodes))
    CALL AppNode_ReadPreprocessedData(InFile,AppGrid%NNodes,AppGrid%AppNode,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Number of connected nodes
    AppGrid%NSumConnectedNode  = SUM(AppGrid%AppNode%NConnectedNode)    

    !Read app. grid element data
    ALLOCATE (AppGrid%AppElement(AppGrid%NElements))
    CALL AppElement_ReadPreprocessedData(InFile,AppGrid%NElements,AppGrid%AppElement,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Read app. grid face data
    CALL AppGrid%AppFace%New(InFile,AppGrid%NFaces,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read boundary face list
    ALLOCATE (AppGrid%BoundaryFaceList(AppGrid%NBoundaryFaces))
    CALL InFile%ReadData(AppGrid%BoundaryFaceList,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read app. subregion data
    ALLOCATE (AppGrid%AppSubregion(AppGrid%NSubregions))
    DO indx=1,AppGrid%NSubregions
      ASSOCIATE (pAppSubregion => AppGrid%AppSubregion(indx))
          CALL InFile%ReadData(pAppSubregion%ID,iStat)                ;  IF (iStat .EQ. -1) RETURN
          CALL InFile%ReadData(pAppSubregion%Name,iStat)              ;  IF (iStat .EQ. -1) RETURN
          CALL InFile%ReadData(pAppSubregion%NRegionElements,iStat)   ;  IF (iStat .EQ. -1) RETURN
          CALL InFile%ReadData(pAppSubregion%NNeighborRegions,iStat)  ;  IF (iStat .EQ. -1) RETURN
          CALL InFile%ReadData(pAppSubregion%Area,iStat)              ;  IF (iStat .EQ. -1) RETURN
          ALLOCATE (pAppSubregion%RegionElements(pAppSubregion%NRegionElements))
             CALL InFile%ReadData(pAppSubregion%RegionElements,iStat)  ;  IF (iStat .EQ. -1) RETURN
          ALLOCATE (pAppSubregion%NeighborRegions(pAppSubregion%NNeighborRegions))
             CALL InFile%ReadData(pAppSubregion%NeighborRegions%RegionNo,iStat)     ;  IF (iStat .EQ. -1) RETURN
             CALL InFile%ReadData(pAppSubregion%NeighborRegions%NregBndFace,iStat)  ;  IF (iStat .EQ. -1) RETURN
             DO indx1=1,pAppSubregion%NNeighborRegions
                nBndFace = pAppSubregion%NeighborRegions(indx1)%NRegBndFace
                ALLOCATE (pAppSubregion%NeighborRegions(indx1)%RegBndFace(NBndFace))
                CALL InFile%ReadData(pAppSubregion%NeighborRegions(indx1)%RegBndFace,iStat)  
                IF (iStat .EQ. -1) RETURN
             END DO 
      END ASSOCIATE
    END DO
        
    !Free memory
    DEALLOCATE (X , Y , NVertex , Vertex , STAT=ErrorCode)

  END SUBROUTINE ReadProcessedAppGridData
  
  
  ! -------------------------------------------------------------
  ! --- READ PRE-PROCESSED AppNode DATA FROM BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppNode_ReadPreprocessedData(InFile,NNodes,AppNode,iStat)
    TYPE(GenericFileType) :: Infile
    INTEGER,INTENT(IN)    :: NNodes
    TYPE(AppNodeType)     :: AppNode(NNodes)
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+28) :: ThisProcedure = ModName // 'AppNode_ReadPreprocessedData'
    INTEGER                      :: indx,NFaceID,iSizeSurroundingElem,iSizeConnectedNode,ErrorCode
    
    !Initialize
    iStat = 0
    
    DO indx=1,NNodes
      CALL InFile%ReadData(AppNode(indx)%ID,iStat)              ;  IF (iStat .EQ. -1) RETURN        
      CALL InFile%ReadData(AppNode(indx)%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN        
      CALL InFile%ReadData(AppNode(indx)%BoundaryNode,iStat)    ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppNode(indx)%NConnectedNode,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(NFaceID,iStat)  ;  IF (iStat .EQ. -1) RETURN   ;   AppNode(indx)%NFaceID = NFaceID
      CALL InFile%ReadData(iSizeSurroundingElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(iSizeConnectedNode,iStat)    ;  IF (iStat .EQ. -1) RETURN
      ALLOCATE (AppNode(indx)%FaceID(NFaceID)                          , &
                AppNode(indx)%ElemID_OnCCWSide(NFaceID)                , &
                AppNode(indx)%IrrotationalCoeff(NFaceID)               , &
                AppNode(indx)%SurroundingElement(iSizeSurroundingElem) , &
                AppNode(indx)%ConnectedNode(iSizeConnectedNode)        , &
                STAT=ErrorCode                                         )
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error allocating memory for application nodal data!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      CALL InFile%ReadData(AppNode(indx)%SurroundingElement,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppNode(indx)%ConnectedNode,iStat)       ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppNode(indx)%FaceID,iStat)              ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppNode(indx)%ElemID_OnCCWSide,iStat)    ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppNode(indx)%IrrotationalCoeff,iStat)   ;  IF (iStat .EQ. -1) RETURN
    END DO

  END SUBROUTINE AppNode_ReadPreprocessedData


  ! -------------------------------------------------------------
  ! --- READ PRE-PROCESSED AppElement DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppElement_ReadPreprocessedData(InFile,NElements,AppElement,iStat)
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(IN)    :: NElements
    TYPE(AppElementType)  :: AppElement(NElements)
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31) :: ThisProcedure = ModName // 'AppElement_ReadPreprocessedData'
    INTEGER                      :: indx,nFaceID,nVertexArea,nIntegral_DELShpI_DELShpJ,nIntegral_Rot_DELShpI_DELShpJ,ErrorCode
    
    !Initialize
    iStat = 0
    
    DO indx=1,NElements
      CALL InFile%ReadData(AppElement(indx)%ID,iStat)            ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppElement(indx)%Subregion,iStat)     ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppElement(indx)%Area,iStat)          ;  IF (iStat .EQ. -1) RETURN 
      CALL InFile%ReadData(nFaceID,iStat)                        ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(nVertexArea,iStat)                    ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(nIntegral_DELShpI_DELShpJ,iStat)      ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(nIntegral_Rot_DELShpI_DELShpJ,iStat)  ;  IF (iStat .EQ. -1) RETURN
      ALLOCATE (AppElement(indx)%FaceID(nFaceID)                                              , &
                AppElement(indx)%VertexArea(nVertexArea)                                      , &
                AppElement(indx)%VertexAreaFraction(nVertexArea)                              , &
                AppElement(indx)%Integral_DELShpI_DELShpJ(nIntegral_DELShpI_DELShpJ)          , &
                AppElement(indx)%Integral_Rot_DELShpI_DELShpJ(nIntegral_Rot_DELShpI_DELShpJ)  , &
                STAT=ErrorCode                                                                )
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error allocating memory for application elements!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      CALL InFile%ReadData(AppElement(indx)%FaceID,iStat)                        ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppElement(indx)%VertexArea,iStat)                    ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppElement(indx)%VertexAreaFraction,iStat)            ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppElement(indx)%Integral_DELShpI_DELShpJ,iStat)      ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppElement(indx)%Integral_Rot_DELShpI_DELShpJ,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO

  END SUBROUTINE AppElement_ReadPreprocessedData



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- WRITE GRID DATA TO PRE-PROCESSOR BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE WritePreProcessedData(AppGrid,OutFile)
    CLASS(AppGridType),TARGET,INTENT(IN) :: AppGrid
    TYPE(GenericFileType)                :: OutFile

    !Local variables
    INTEGER                      :: indx,indx1,NNodes,NElements,NFaces,NSubregions,NBoundaryFaces
    TYPE(AppRegionType),POINTER  :: AppSubregion

    !Initialize
    NNodes         = AppGrid%NNodes
    NElements      = AppGrid%NElements
    NFaces         = AppGrid%NFaces
    NSubregions    = AppGrid%NSubregions
    NBoundaryFaces = AppGrid%NBoundaryFaces

    !Grid dimensions
    CALL OutFile%WriteData(NNodes)
    CALL OutFile%WriteData(NElements)
    CALL OutFile%WriteData(NFaces)
    CALL OutFile%WriteData(NSubregions)
    CALL OutFile%WriteData(NBoundaryFaces)

    !Base grid node data
    CALL OutFile%WriteData(AppGrid%X)        
    CALL OutFile%WriteData(AppGrid%Y)        

    !Base grid element data
    CALL OutFile%WriteData(AppGrid%NVertex)
    CALL OutFile%WriteData(AppGrid%Vertex)

    !App. grid nodal data
    CALL AppNode_WritePreprocessedData(OutFile,AppGrid%AppNode)        

    !App. grid element data 
    CALL AppElement_WritePreprocessedData(OutFile,AppGrid%AppElement)        

    !App. grid face data
    CALL AppGrid%AppFace%WriteData(OutFile)
    
    !Boundary face list
    CALL OutFile%WriteData(AppGrid%BoundaryFaceList)
    
    !App. subregion data
    DO indx=1,NSubregions
      AppSubregion => AppGrid%AppSubregion(indx)
      CALL OutFile%WriteData(AppSubregion%ID)
      CALL OutFile%WriteData(AppSubregion%Name)
      CALL OutFile%WriteData(AppSubregion%NRegionElements)
      CALL OutFile%WriteData(AppSubregion%NNeighborRegions)
      CALL OutFile%WriteData(AppSubregion%Area)
      CALL OutFile%WriteData(AppSubregion%RegionElements)
      CALL OutFile%WriteData(AppSubregion%NeighborRegions%RegionNo)
      CALL OutFile%WriteData(AppSubregion%NeighborRegions%NRegBndFace)
      CALL OutFile%WriteData([(AppSubregion%NeighborRegions(indx1)%RegBndFace,indx1=1,AppSubregion%NNeighborRegions)])
    END DO
    
  END SUBROUTINE WritePreProcessedData
  
  
  ! -------------------------------------------------------------
  ! --- WRITE PRE-PROCESSED AppNode DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppNode_WritePreprocessedData(OutFile,AppNode)
    TYPE(GenericFileType)        :: OutFile
    TYPE(AppNodeType),INTENT(IN) :: AppNode(:)
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(AppNode)
        CALL OutFile%WriteData(AppNode(indx)%ID)        
        CALL OutFile%WriteData(AppNode(indx)%Area)        
        CALL OutFile%WriteData(AppNode(indx)%BoundaryNode)
        CALL OutFile%WriteData(AppNode(indx)%NConnectedNode)
        CALL OutFile%WriteData(AppNode(indx)%NFaceID)
        CALL OutFile%WriteData(SIZE(AppNode(indx)%SurroundingElement))
        CALL OutFile%WriteData(SIZE(AppNode(indx)%ConnectedNode))
        CALL OutFile%WriteData(AppNode(indx)%SurroundingElement)
        CALL OutFile%WriteData(AppNode(indx)%ConnectedNode)
        CALL OutFile%WriteData(AppNode(indx)%FaceID)
        CALL OutFile%WriteData(AppNode(indx)%ElemID_OnCCWSide)
        CALL OutFile%WriteData(AppNode(indx)%IrrotationalCoeff)
    END DO

  END SUBROUTINE AppNode_WritePreprocessedData


  ! -------------------------------------------------------------
  ! --- WRITE PRE-PROCESSED AppElement DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppElement_WritePreprocessedData(OutFile,AppElement)
    TYPE(GenericFileType)           :: OutFile
    TYPE(AppElementType),INTENT(IN) :: AppElement(:)
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(AppElement)
      CALL OutFile%WriteData(AppElement(indx)%ID)
      CALL OutFile%WriteData(AppElement(indx)%Subregion)
      CALL OutFile%WriteData(AppElement(indx)%Area)
      CALL OutFile%WriteData(SIZE(AppElement(indx)%FaceID))
      CALL OutFile%WriteData(SIZE(AppElement(indx)%VertexArea))                    
      CALL OutFile%WriteData(SIZE(AppElement(indx)%Integral_DELShpI_DELShpJ))      
      CALL OutFile%WriteData(SIZE(AppElement(indx)%Integral_Rot_DELShpI_DelShpJ))  
      CALL OutFile%WriteData(AppElement(indx)%FaceID)                              
      CALL OutFile%WriteData(AppElement(indx)%VertexArea)                          
      CALL OutFile%WriteData(AppElement(indx)%VertexAreaFraction)                  
      CALL OutFile%WriteData(AppElement(indx)%Integral_DELShpI_DELShpJ)            
      CALL OutFile%WriteData(AppElement(indx)%Integral_Rot_DELShpI_DelShpJ)        
    END DO

  END SUBROUTINE AppElement_WritePreprocessedData




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GENERATE GRID DATA BASED ON NODE AND ELEMENT INFO
  ! -------------------------------------------------------------
  SUBROUTINE ConstructAppGrid(X,Y,NVertex,Vertex_IDs,NodeID,ElemID,iElemSubregionIDs,iSubregionIDs,cSubregionNames,AppGrid,iStat)
    REAL(8),INTENT(IN)            :: X(:),Y(:)
    INTEGER,INTENT(IN)            :: NVertex(:),Vertex_IDs(:,:),NodeID(:),ElemID(:),iElemSubregionIDs(:),iSubregionIDs(:)
    CHARACTER(LEN=*),INTENT(IN)   :: cSubregionNames(:)
    TYPE(AppGridType),INTENT(OUT) :: AppGrid
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13)  :: ThisProcedure = ModName//'ConstructGrid'
    INTEGER                       :: indx,ConvexNode,indxVertex,ErrorCode,NElements,NNodes,iCount,indxFace,iStatParallel,Vertexc(4),    &
                                     Vertex(SIZE(Vertex_IDs,DIM=1),SIZE(Vertex_IDs,DIM=2)),Vertex_1D(SIZE(Vertex_IDs)),ID,              &
                                     iSubregion_Indices(SIZE(iElemSubregionIDs))
    REAL(8)                       :: Xc(4),Yc(4)
    INTEGER,ALLOCATABLE           :: DummyIntArray(:),iSubregionIDs_Unique(:)
    
    !Initialize
    iStat         = 0
    iStatParallel = 0
    
    !Convert vertex IDs to indices
    CALL ConvertID_To_Index(PACK(Vertex_IDs,MASK=.TRUE.),NodeID,Vertex_1D)
    Vertex = RESHAPE(Vertex_1D,[SIZE(Vertex_IDs,DIM=1),SIZE(Vertex_IDS,DIM=2)])
    
    !Make sure that number of subregions are consistent
    CALL GetUniqueArrayComponents(iElemSubregionIDs,iSubregionIDs_Unique)
    IF (SIZE(iSubregionIDs_Unique) .NE. SIZE(cSubregionNames)) THEN
        MessageArray(1) = 'Number of subregion names listed in Element Configuration File is'
        MessageArray(2) = 'different than the unique subregion IDs specified for elements!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
      
    !Convert subregion IDs to indices
    CALL ShellSort(iSubregionIDs_Unique)
    CALL ConvertID_To_Index(iElemSubregionIDs,iSubregionIDs_Unique,iSubregion_Indices)
    
    !Set the base grid data
    CALL AppGrid%GridType%Init(X,Y,NVertex,Vertex,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Establish the face list
    CALL AppGrid%AppFace%New(NVertex,Vertex,X,Y,iStat)  
    IF (iStat .EQ. -1) RETURN
    AppGrid%NFaces = AppGrid%AppFace%GetNFaces()
    
    !Establish the boundary face list
    AppGrid%NBoundaryFaces = COUNT(AppGrid%AppFace%BoundaryFace)
    ALLOCATE (AppGrid%BoundaryFaceList(AppGrid%NBoundaryFaces))
    iCount = 1
    DO indxFace=1,AppGrid%NFaces
        IF (AppGrid%AppFace%BoundaryFace(indxFace)) THEN
            AppGrid%BoundaryFaceList(iCount) = indxFace
            iCount                           = iCount + 1
        END IF
    END DO
    
    !Number of nodes
    NNodes         = SIZE(X)
    AppGrid%NNodes = NNodes
    ALLOCATE (AppGrid%AppNode(NNodes) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for the nodes of the application grid!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    AppGrid%AppNode%ID = NodeID
    
    !Number of elements
    NElements         = SIZE(NVertex)
    AppGrid%NElements = NElements
    ALLOCATE (AppGrid%AppElement(NElements) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for the elements of the application grid!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    AppGrid%AppElement%ID = ElemID
    
    !Check if elements are convex
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NElements,NVertex,Vertex,X,Y,iStat) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500) 
    DO indx=1,NElements
        IF (NVertex(indx) .EQ. 3) CYCLE
        Vertexc = Vertex(:,indx)
        Xc      = X(Vertexc)
        Yc      = Y(Vertexc)
        CALL CheckElementConvexity(NVertex(indx),Vertexc,Xc,Yc,ConvexNode)
        IF (ConvexNode .NE. 0) THEN
            MessageArray(1)='Element '//TRIM(IntToText(AppGrid%AppElement(indx)%ID))//' is not convex'
            MessageArray(2)='(has an angle larger than or equal to 180 degrees) at node '//TRIM(IntToText(ConvexNode)) 
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
        END IF
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    IF (iStat .EQ. -1) RETURN

    !Assign subregions to elements
    AppGrid%AppElement%Subregion = iSubregion_Indices
    
    !Compute element areas
    CALL ElementArea(AppGrid%GridType,AppGrid%AppElement%ID,1,NElements,AppGrid%AppElement%Area,iStat)
    IF (iStat .EQ. -1) RETURN

    !Identify faces for each element
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NElements,AppGrid,iStatParallel,ThisProcedure) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500) 
    DO indx=1,NElements
        CALL ListFacesForElement(AppGrid,indx,AppGrid%AppElement(indx)%FaceID,iStat)
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
        !Check if all faces are boundary faces
        IF (ALL(AppGrid%AppFace%BoundaryFace(AppGrid%AppElement(indx)%FaceID))) THEN
            IF (NElements .GT. 1) THEN
                !$OMP CRITICAL
                ID              = AppGrid%AppElement(indx)%ID
                MessageArray(1) = 'All faces of element '// TRIM(IntToText(ID)) // ' are boundary faces!'
                MessageArray(2) = 'Such a grid setup is not allowed.'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStatParallel = -1
                !$OMP END CRITICAL
            END IF
        END IF
    END DO
    !$OMP END DO 
    !$OMP END PARALLEL
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF
    
    !Compute element vertex areas and vertex area fractions
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NElements,NVertex,Vertex,AppGrid,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500) 
    DO indx=1,NElements
        ALLOCATE (AppGrid%AppElement(indx)%VertexArea(NVertex(indx)) , AppGrid%AppElement(indx)%VertexAreaFraction(NVertex(indx)) , STAT=ErrorCode)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for vertex areas for element '//TRIM(IntToText(AppGrid%AppElement(indx)%ID))//'!',f_iFatal,ThisProcedure)          
            iStatParallel = -1
        END IF
        CALL ElementVertexArea(AppGrid%GridType,indx,[(indxVertex,indxVertex=1,NVertex(indx))],AppGrid%AppElement(indx)%VertexArea,iStat)  
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
        CALL ElementVertexAreaFraction(AppGrid%GridType,indx,AppGrid%AppElement%ID,[(indxVertex,indxVertex=1,NVertex(indx))],AppGrid%AppElement(indx)%VertexAreaFraction,iStat)  
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1             
        END IF
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF
    
    !Compute integral of delwi * delwj
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NElements,AppGrid,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indx=1,NElements
        CALL Compute_DELShpI_DELShpJ(AppGrid%GridType,indx,AppGrid%AppElement%ID,AppGrid%AppElement(indx)%Integral_DELShpI_DELShpJ,AppGrid%AppElement(indx)%Area,iStat)
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
    END DO
    !$OMP END DO 
    !$OMP END PARALLEL
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF

    !Compute integral of rot_delwi * delwj
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NElements,AppGrid,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indx=1,NElements
        CALL Compute_Rot_DELShpI_DELShpJ(AppGrid%GridType,indx,AppGrid%AppElement%ID,AppGrid%AppElement(indx)%Integral_Rot_DELShpI_DELShpJ,AppGrid%AppElement(indx)%Area,iStat)
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF

    !Compute nodal areas (area of influence)
    CALL NodeArea(AppGrid%GridType,AppGrid%AppNode%ID,1,NNodes,AppGrid%AppNode%Area,iStat)
    IF (iStat .EQ. -1) RETURN

    !Identify the boundary nodes
    CALL ListBoundaryNodes(AppGrid,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AppGrid%AppNode(DummyIntArray)%BoundaryNode = .TRUE.

    !Gather surrounding elements for each node
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NNodes,AppGrid,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indx=1,NNodes
        CALL ListSurroundingElems(AppGrid%GridType,indx,AppGrid%AppNode(indx)%SurroundingElement,iStat)
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
        IF (SIZE(AppGrid%AppNode(indx)%SurroundingElement) .EQ. 0) CALL LogMessage('Node number '//TRIM(IntToText(AppGrid%AppNode(indx)%ID))//' does not have any surrounding elements!',f_iWarn,ThisProcedure)
    END DO
    !$OMP END DO 
    !$OMP END PARALLEL  
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF

    !Construct the list of connected nodes for each node
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NNodes,AppGrid,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indx=1,NNodes
        CALL ListConnectedNodes(AppGrid%GridType,indx,AppGrid%AppNode(indx)%ConnectedNode,iStat)
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
        AppGrid%AppNode(indx)%NConnectedNode = SIZE(AppGrid%AppNode(indx)%ConnectedNode)
        IF (AppGrid%AppNode(indx)%NConnectedNode .EQ. 0) CALL LogMessage('Node number '//TRIM(IntToText(AppGrid%AppNode(indx)%ID))//' does not have any connected nodes!',f_iWarn,ThisProcedure)
    END DO
    !$OMP END DO 
    !$OMP END PARALLEL  
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF
    AppGrid%NSumConnectedNode = SUM(AppGrid%AppNode%NConnectedNode)
         
    !Identify faces that meet at each node
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NNodes,AppGrid,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indx=1,NNodes
        CALL ListFacesAtNode(AppGrid,indx,AppGrid%AppNode(indx)%FaceID,iStat)  
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
        AppGrid%AppNode(indx)%NFaceID = SIZE(AppGrid%AppNode(indx)%FaceID)
        IF (AppGrid%AppNode(indx)%NFaceID .EQ. 0) CALL LogMessage('Node number '//TRIM(IntToText(AppGrid%AppNode(indx)%ID))//' does not have any faces connecting at it!',f_iWarn,ThisProcedure)
    END DO
    !$OMP END DO 
    !$OMP END PARALLEL  
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF
    
    !Check for overlapping elements
    CALL CheckForOverlaps(AppGrid,NodeID,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Check for any gaps in the grid and warn
    CALL CheckForGaps(AppGrid,NodeID,lFirstCall=.TRUE.)

    !Identify elements on the counter-clockwise side of each face that meet at each node
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NNodes,AppGrid,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indx=1,NNodes
        CALL ListElemID_CCW(AppGrid,indx,AppGrid%AppNode(indx)%ElemID_OnCCWSide,iStat)  
        IF (iStat .EQ. -1) THEN
            iStatParallel = -1
        END IF
        IF (SIZE(AppGrid%AppNode(indx)%ElemID_OnCCWSide) .EQ. 0) CALL LogMessage('Node number '//TRIM(IntToText(AppGrid%AppNode(indx)%ID))//' does not have any surrounding elements!',f_iWarn,ThisProcedure)
    END DO
    !$OMP END DO 
    !$OMP END PARALLEL  
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF
    
    !Order the faces at each nodes (and corresponding elements on the counter-clockwise side) in counter-clockwise direction
    CALL ReorderFacesInCCWDirection(AppGrid)
    
    !Compute irrotationality coefficents at each node for face flow computations
    CALL ComputeIrrotationalCoeff(NNodes,AppGrid%X,AppGrid%Y,AppGrid%AppFace,AppGrid%AppNode,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !If subregion IDs are defined along with subregion names, make sure they are consistent with unique subregion IDs compiled from element data
    IF (SIZE(iSubregionIDs) .GT. 0) THEN
        CALL ShellSort(iSubregionIDs,cSubregionNames)
        DO indx=1,SIZE(iSubregionIDs)
            IF (iSubregionIDS(indx) .NE. iSubregionIDs_Unique(indx)) THEN
                CALL SetLastMessage('No name is provided for subregion '//TRIM(IntToText(iSubregionIDs_Unique(indx))) // 'listed for some elements!',f_iFatal,ThisProcedure) 
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    
    !Compile the subregional information
    CALL CompileSubregionalData(AppGrid,iSubregionIDs_Unique,cSubregionNames,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Check for positive stiffness matrix entries
    CALL CheckForPositiveStiffness(AppGrid%NElements,AppGrid%NNodes,AppGrid%NVertex,AppGrid%Vertex,AppGrid%AppNode,AppGrid%AppElement)
    
    !Free memory
    DEALLOCATE (DummyIntArray , STAT=ErrorCode)

  END SUBROUTINE ConstructAppGrid


  ! -------------------------------------------------------------
  ! --- CHECK THAT THE GRID DOES NOT HAVE OVERLAPPING ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE CheckForOverlaps(AppGrid,NodeID,iStat)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: NodeID(:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'CheckForOverlaps'
    INTEGER                                :: iNNodesInError,iNodesInError(AppGrid%NNodes),indxNode,indxElem,ID,iCount
    REAL(8)                                :: XP,YP
    
    !Initailize
    iStat          = 0
    iNNodesInError = 0
    
    !Loop over nodes
    DO indxNode=1,AppGrid%NNodes
        !Skip if this is not a boundary node
        IF (.NOT. AppGrid%AppNode(indxNode)%BoundaryNode) CYCLE
        
        !Coordinates of the node
        XP = AppGrid%X(indxNode)
        YP = AppGrid%Y(indxNode)
        
        !Check if the node is in any element that it is not a vertex of
        !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(indxNode,XP,YP,iNNodesInError,iNodesInError,AppGrid) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
        !$OMP DO SCHEDULE(STATIC,500)
        DO indxElem=1,AppGrid%NElements
            !If element has the node as vertex, skip check
            IF (LocateInList(indxElem,AppGrid%AppNode(indxNode)%SurroundingElement) .GT. 0) CYCLE
            !Check
            IF (AppGrid%IsPointInElement(XP,YP,indxElem)) THEN
                !$OMP CRITICAL
                iNNodesInError                = iNNodesInError + 1
                iNodesInError(iNNodesInError) = indxNode
                !$OMP END CRITICAL
            END IF
        END DO
        !$OMP END DO 
        !$OMP END PARALLEL  
    END DO
    IF (iNNodesInError .GT. 0) THEN
        MessageArray(1) = 'Elements with the following node(s) are overlapping with other elements:'
        MessageArray(2) = ''
        iCount          = 0
        DO indxNode=1,iNNodesInError
            ID = NodeID(iNodesInError(indxNode))
            IF (iCount .EQ. 0) THEN
              IF (indxNode .EQ. 1) THEN
                  MessageArray(2) = TRIM(MessageArray(2)) // TRIM(IntToText(ID))
              ELSE
                  MessageArray(2) = TRIM(MessageArray(2)) // '   '  // TRIM(IntToText(ID))
              END IF  
            END IF    
            iCount = iCount + 1
            IF (MOD(iCount,15) .EQ. 0) THEN
                IF (indxNode .LT. iNNodesInError) THEN
                    iCount          = 0
                    MessageArray(2) = TRIM(MessageArray(2)) // ',' // f_cLineFeed // '*   '
                END IF
            END IF
        END DO
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
  END SUBROUTINE CheckForOverlaps
  
  
  ! -------------------------------------------------------------
  ! --- CHECK FOR GAPS IN THE GRID AND WARN THE USER FOR THEIR EXISTANCE
  ! --- Note: They can be legitimate gaps, so don't stop the execution
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE CheckForGaps(AppGrid,NodeID,lFirstCall,lFaceProcessed)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: NodeID(:)
    LOGICAL,INTENT(IN)           :: lFirstCall
    LOGICAL,OPTIONAL,INTENT(IN)  :: lFaceProcessed(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'CheckForGaps'
    INTEGER                                :: indx,iFace1,iFace2,iStartFace,iNode1,iNode2,iCCWiseNodes(2),iElem,    &
                                              iBndNodes(AppGrid%NBoundaryFaces+1),iNBndFaces,iCount,indxFace,       &
                                              iConvexNode
    REAL(8)                                :: rCrossProduct
    LOGICAL                                :: lFaceProcessed_Local(AppGrid%NBoundaryFaces)
    
    !Initialize
    iNBndFaces = AppGrid%NBoundaryFaces
    
    !Allocate memory for flag to check if a boundary face was processed
    IF (lFirstCall) THEN
        lFaceProcessed_Local = .FALSE.
    ELSE
        lFaceProcessed_Local = lFaceProcessed
    END IF
    
    !Find a boundary face that wasn't processed to start compiling a boundary polygon that was not processed
    DO indxFace=1,iNBndFaces
        !Cycle if the face has been processed
        IF (lFaceProcessed_Local(indxFace)) CYCLE
        
        !Otherwise, tag face and exit loop
        iFace1 = AppGrid%BoundaryFaceList(indxFace)
        EXIT
    END DO
    
    !If no unprocessed face was found, return
    IF (indxFace .GT. iNBndFaces) RETURN
    
    !Compile boundary face polygon
    iCount     = 0
    iStartFace = iFace1
    DO    
        lFaceProcessed_Local(LocateInList(iFace1,AppGrid%BoundaryFaceList)) = .TRUE.
        
        !Nodes of the face 
        iNode1 = AppGrid%AppFace%Node(1,iFace1)
        iNode2 = AppGrid%AppFace%Node(2,iFace1)
        
        !Nodes of the face listed in counter-clockwise
        iCount = iCount + 1
        iElem  = AppGrid%AppFace%Element(2,iFace1)
        CALL OrderNodesInCCWise(iElem,iNode1,iNode2,iCCWiseNodes)
        
        !Store boundary nodes
        iBndNodes(iCount:iCount+1) = iCCWiseNodes

        !Find the next boundary face that has the last identifed boundary node as one of the nodes
        iNode1 = iCCWiseNodes(2)
        DO indx=1,AppGrid%AppNode(iNode1)%NFaceID
            iFace2 = AppGrid%AppNode(iNode1)%FaceID(indx)
            IF (iFace1 .EQ. iFace2) CYCLE
            IF (.NOT. AppGrid%AppFace%BoundaryFace(iFace2)) CYCLE
            EXIT
        END DO
        
        !If the next boundary face is the same as the strating face, we have made a complete loop; exit boundary polygon compilation
        IF (iFace2 .EQ. iStartFace) EXIT
        
        !Otherwise, continue processing faces
        iFace1 = iFace2
    END DO
    
    !Find the curve orientation using the most lower left boundary node
    !Walking along faces and generating vectors between vertices, if the cross product of 
    !two vectors joining at a vertex is negative we are turning right (internal boundary),
    !otherwise we are turning left (external boundary)
    iConvexNode   = LeftMostNode(AppGrid%X,AppGrid%Y,iCount,iBndNodes)
    rCrossProduct = CrossProduct(AppGrid%X,AppGrid%Y,iConvexNode,iCount,iBndNodes)
    IF (rCrossProduct .LT. -1d-1) THEN
        !Boundary polygon oriented clockwise; it is an inner boundary
        CALL PrintInternalBndNodes(iCount,iBndNodes,NodeID)
    ELSEIF (rCrossProduct .GT. 1d-1) THEN
        !Boundary polygon oriented counter clockwise; it is an outer boundary
        !DO NOTHING
    END IF
    
    !If the path is not turning right or left, then try with the most upper right node
    IF (ABS(rCrossProduct) .LE. 1d-1) THEN
        !Identify the most upper right boundary node
        iConvexNode = RightMostNode(AppGrid%X,AppGrid%Y,iCount,iBndNodes)
        
        !Check curve orientation
        rCrossProduct = CrossProduct(AppGrid%X,AppGrid%Y,iConvexNode,iCount,iBndNodes)
        IF (rCrossProduct .LT. -1d-1) THEN
            !Boundary polygon oriented clockwise; it is an inner boundary
            CALL PrintInternalBndNodes(iCount,iBndNodes,NodeID)
        ELSEIF (rCrossProduct .GT. 1d-1) THEN
            !Boundary polygon oriented counter clockwise; it is an outer boundary
            !DO NOTHING
        END IF
    END IF
            
    !Process next boundary polygon
    CALL CheckForGaps(AppGrid,NodeID,lFirstCall=.FALSE.,lFaceProcessed=lFaceProcessed_Local)
    
    
  CONTAINS
  
  
    ! #############################################################
    ! ### PRINT NODES OF INTERNAL BOUNDRY
    ! #############################################################
    SUBROUTINE PrintInternalBndNodes(iDim,iBndNodes,iNodeIDs)
      INTEGER,INTENT(IN) :: iDim,iBndNodes(iDim),iNodeIDs(:)
      
      !Local variables
      INTEGER :: indxNode,ID,indxMsgArray,iMaxDim,iLoc
      
      iMaxDim = SIZE(MessageArray)
      
      MessageArray(1)  = 'The following nodes define a gap in the grid!'
      MessageArray(2)  = 'Please make sure that this is intential and not an error in the grid.'
      MessageArray(3:) = ''
      indxMsgArray     = 3
      DO indxNode=1,iDim
          ID = iNodeIDs(iBndNodes(indxNode))
          IF (indxMsgArray .GT. iMaxDim) CYCLE
          IF (LEN_TRIM(MessageArray(indxMsgArray)) .EQ. 0) THEN
              MessageArray(indxMsgArray) = TRIM(IntToText(ID)) // ','
          ELSE
              MessageArray(indxMsgArray) = TRIM(MessageArray(indxMsgArray)) // ' ' // TRIM(IntToText(ID)) // ','
          END IF
          IF (indxNode .EQ. iDim) THEN
              iLoc = LEN_TRIM(MessageArray(indxMsgArray))
              MessageArray(indxMsgArray)(iLoc:iLoc) = ''
          END IF  
          IF (MOD(indxNode,15) .EQ. 0) indxMsgArray = indxMsgArray + 1
      END DO
      CALL LogMessage(MessageArray(1:MIN(indxMsgArray,iMaxDim)),f_iWarn,ThisProcedure)
      
    END SUBROUTINE PrintInternalBndNodes
        

    ! #############################################################
    ! ### IDENTIFY RIGHT-MOST BOUNDARY NODE
    ! #############################################################
    FUNCTION RightMostNode(rX,rY,iDim,iBndNodes) RESULT(iNode)
      REAL(8),INTENT(IN) :: rX(:),rY(:)
      INTEGER,INTENT(IN) :: iDim,iBndNodes(iDim)
      INTEGER            :: iNode
      
      !Local variables
      INTEGER :: indxNode,iThisNode
      REAL(8) :: rXMax
      
      rXMax = rX(iBndNodes(1))
      iNode = 1
      DO indxNode=2,iCount
          iThisNode = iBndNodes(indxNode)
          IF (rX(iThisNode) .GT. rXMax) THEN
              iNode = indxNode
              rXMax = rX(iThisNode)
          ELSEIF (rX(iThisNode) .EQ. rXMax) THEN
              IF (rY(iThisNode) .GT. rY(iNode)) iNode = indxNode
          END IF
      END DO

    END FUNCTION RightMostNode
    
    
    ! #############################################################
    ! ### IDENTIFY LEFT-MOST BOUNDARY NODE
    ! #############################################################
    FUNCTION LeftMostNode(rX,rY,iDim,iBndNodes) RESULT(iNode)
      REAL(8),INTENT(IN) :: rX(:),rY(:)
      INTEGER,INTENT(IN) :: iDim,iBndNodes(iDim)
      INTEGER            :: iNode
      
      !Local variables
      INTEGER :: indxNode,iThisNode
      REAL(8) :: rXMin
      
      rXMin = rX(iBndNodes(1))
      iNode = 1
      DO indxNode=2,iDim
          iThisNode = iBndNodes(indxNode)
          IF (rX(iThisNode) .LT. rXMin) THEN
              iNode = indxNode
              rXMin = rX(iThisNode)
          ELSEIF (rX(iThisNode) .EQ. rXMin) THEN
              IF (rY(iThisNode) .LT. rY(iNode)) iNode = indxNode
          END IF
      END DO

    END FUNCTION LeftMostNode
    
    
    ! #############################################################
    ! ### CALCULATE CROSS PRODUCT OF TWO VECTORS JOING AT A COMMON VERTEX
    ! #############################################################
    FUNCTION CrossProduct(rX,rY,iMyNode,iDim,iBndNodes) RESULT(rXProduct)
      REAL(8),INTENT(IN) :: rX(:),rY(:) 
      INTEGER,INTENT(IN) :: iMyNode,iDim,iBndNodes(iDim)
      REAL(8)            :: rXProduct
      
      !Local variables
      INTEGER :: iNode
      REAL(8) :: rxA,rxB,rxC,ryA,ryB,ryC
      
      !Coordinates of the identified node and the nodes before and after that
      iNode = iMyNode - 1  ;  IF (iNode .EQ. 0) iNode = iDim
      rxA   = rX(iBndNodes(iNode))
      ryA   = rY(iBndNodes(iNode))
      iNode = iMyNode
      rxB   = rX(iBndNodes(iNode))
      ryB   = rY(iBndNodes(iNode))
      iNode = iMyNode + 1  ;  IF (iNode .EQ. iCount+1) iNode = 1
      rxC   = rX(iBndNodes(iNode))
      ryC   = rY(iBndNodes(iNode))

      !Cross product
      rXProduct =  (rxB-rxA)*(ryC-ryB) + (ryA-ryB)*(rxC-rxB)
      
    END FUNCTION CrossProduct
    
    
    ! #############################################################
    ! ### SUBROUTINE TO ORDER TWO NODES OF AN ELEMENT IN COUNTER-CLOCKWISE FASHION
    ! #############################################################
    SUBROUTINE OrderNodesInCCWise(iElem,iNode1,iNode2,iOrderedNodes)
      INTEGER,INTENT(IN)  :: iElem,iNode1,iNode2
      INTEGER,INTENT(OUT) :: iOrderedNodes(2)
      
      !Local variables
      INTEGER :: NVertex,Vertex(4),indxVertex,indx
      
      NVertex           = AppGrid%NVertex(iElem)
      Vertex(1:NVertex) = AppGrid%Vertex(1:NVertex,iElem)
      DO indxVertex=1,NVertex
          IF (iNode1 .EQ. Vertex(indxVertex)) THEN
              indx = indxVertex + 1  ;  IF (indx .GT. NVertex) indx = 1
              IF (iNode2 .EQ. Vertex(indx)) THEN
                  iOrderedNodes = [iNode1 , iNode2]
                  RETURN
              END IF
          ELSE IF (iNode2 .EQ. Vertex(indxVertex)) THEN
              indx = indxVertex + 1  ;  IF (indx .GT. NVertex) indx = 1
              IF (iNode1 .EQ. Vertex(indx)) THEN
                  iOrderedNodes = [iNode2 , iNode1]
                  RETURN
              END IF
          END IF    
      END DO

    END SUBROUTINE OrderNodesInCCWise
    
  END SUBROUTINE CheckForGaps
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE COEFFICIENTS FOR THE EXPRESSION OF IRROTATIONALITY AT EACH NODE
  ! --- (Assumes array Coeff is already allocated)
  ! -------------------------------------------------------------
  SUBROUTINE ComputeIrrotationalCoeff(NNodes,X,Y,AppFace,AppNode,iStat)
    INTEGER,INTENT(IN)           :: NNodes
    REAL(8),INTENT(IN)           :: X(NNodes),Y(NNodes)
    TYPE(AppFaceType),INTENT(IN) :: AppFace
    TYPE(AppNodeType)            :: AppNode(NNodes)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ComputeIrrotationalCoeff'
    INTEGER                      :: indxNode,ErrorCode,NFace,indxFace,InFaceID,OutFaceID,iLoc,indx, &
                                    iStatParallel
    REAL(8)                      :: L_InFace,L_OutFace,xM_InFace,xM_OutFace,yM_InFace,yM_OutFace, &
                                    slope_p_InFace,slope_p_OutFace,xI,xJ,xK,yI,yJ,yK,c_InFace,    &
                                    c_OutFace,xX,yX,D_InFace,D_OutFace
    
    !Initialize
    iStat         = 0
    iStatParallel = 0
    
    !Compute irrotationality coefficients at each node
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NNodes,AppNode,AppFace,X,Y,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indxNode=1,NNodes
               
      !Number of faces at node
      NFace = AppNode(indxNode)%NFaceID
      
      !Allocate memory for irrotationality coefficients, zero them out
      ALLOCATE (AppNode(indxNode)%IrrotationalCoeff(NFace) , STAT=ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating mmeory for irrotationality coefficients for node ' // TRIM(IntToText(indxNode)) // '!',f_iFatal,ThisProcedure)
          iStatParallel = -1
      END IF
      AppNode(indxNode)%IrrotationalCoeff = 0.0

      !Initialize inflow and outflow faces (the face ids are already ordered in Class_AppGrid for this purpose)
      InFaceID  = AppNode(indxNode)%FaceID(1)
      OutFaceID = AppNode(indxNode)%FaceID(2)
      
      !Loop over faces
      DO indxFace=1,NFace
        !Coordinates of nodes that make up IN (node J-K face) and OUT (node I-J face) faces
        iLoc = LocateInList(indxNode,AppFace%Node(:,OutFaceID))  ;  iLoc = iLoc+1  ;  IF (iLoc .EQ. 3) iLoc = 1
        xI   = X(AppFace%Node(iLoc,OutFaceID))
        yI   = Y(AppFace%Node(iLoc,OutFaceID))
        xJ   = X(indxNode)
        yJ   = Y(indxNode)
        iLoc = LocateInList(indxNode,AppFace%Node(:,InFaceID))  ;  iLoc = iLoc+1  ;  IF (iLoc .EQ. 3) iLoc = 1
        xK   = X(AppFace%Node(iLoc,InFaceID))
        yK   = Y(AppFace%Node(iLoc,InFaceID))
        
        !Length of IN and OUT faces
        L_InFace  = SQRT( (xK-xJ)*(xK-xJ) + (yK-yJ)*(yK-yJ) )
        L_OutFace = SQRT( (xI-xJ)*(xI-xJ) + (yI-yJ)*(yI-yJ) )

        !x-y coordinates of middle points of IN and OUT faces
        xM_InFace  = ABS( xK - xJ )/2d0 + MIN(xK , xJ)
        yM_InFace  = ABS( yK - yJ )/2d0 + MIN(yK , yJ)
        xM_OutFace = ABS( xI - xJ )/2d0 + MIN(xI , xJ)
        yM_OutFace = ABS( yI - yJ )/2d0 + MIN(yI , yJ)
        
        !Slope of lines perpendicular to IN and OUT faces
        slope_p_InFace = yK - yJ
        IF (slope_p_InFace .NE. 0.0) THEN
          slope_p_InFace = (xJ - xK) / slope_p_InFace
          c_InFace       = yM_InFace - slope_p_InFace * xM_InFace
        ELSE
          slope_p_InFace = HUGE(1d0)
        END IF
        
        slope_p_OutFace = yI - yJ
        IF (slope_p_OutFace .NE. 0.0) THEN
          slope_p_OutFace = (xJ - xI) / slope_p_OutFace
          c_OutFace       = yM_OutFace - slope_p_OutFace * xM_OutFace
        ELSE
          slope_p_OutFace = HUGE(1d0)
        END IF
       
        !x and y coordinates of intersection of perpendiculars
        IF (slope_p_InFace .EQ. 0.0) THEN
          yX = yM_InFace
          IF (slope_p_OutFace .EQ. HUGE(1d0)) THEN
            xX = xM_OutFace
          ELSE
            xX = (yX - c_OutFace) / slope_p_OutFace
          END IF
        ELSEIF (slope_p_InFace .EQ. HUGE(1d0)) THEN
          xX = xM_InFace
          IF (slope_p_OutFace .EQ. 0.0) THEN
            yX = yM_OutFace
          ELSE
            yX = slope_p_OutFace * xX + c_OutFace
          END IF
        ELSE
          IF (slope_p_OutFace .EQ. 0.0) THEN
            yX = yM_OutFace
            xX = (yX - c_InFace) / slope_p_InFace
          ELSEIF (slope_p_OutFace .EQ. HUGE(1d0)) THEN
            xX = xM_OutFace
            yX = slope_p_InFace * xX + c_InFace
          ELSE
            xX = (c_InFace - c_OutFace) / (slope_p_OutFace - slope_p_InFace)
            yX = slope_p_InFace * xX + c_InFace
          END IF
        END IF
                    
        !Distance from intersection point to IN face
        D_InFace  = SQRT( (xX - xM_InFace)*(xX - xM_InFace) + (yX - yM_InFace)*(yX - yM_InFace) )    

        !Distance from intersection point to OUT face
        D_OutFace = SQRT( (xX - xM_OutFace)*(xX - xM_OutFace) + (yX - yM_OutFace)*(yX - yM_OutFace) ) 
        
        !Finally, compute the irrotationality coefficients
        IF (AppNode(indxNode)%BoundaryNode) THEN
          !For boundary nodes, it is assumed that flux is uniform and each face gets a flow in proportion to its length
          IF (indxFace .EQ. 1) AppNode(indxNode)%IrrotationalCoeff(NFace) = L_InFace
          IF (indxFace .EQ. NFace-1) THEN
            AppNode(indxNode)%IrrotationalCoeff(1) = L_OutFace
            EXIT
          END IF
        ELSE
          AppNode(indxNode)%IrrotationalCoeff(indxFace) = AppNode(indxNode)%IrrotationalCoeff(indxFace) + D_InFace / (L_InFace/2d0)
          indx = indxFace+1  ;  IF (indx .GT. NFace) indx = 1
          AppNode(indxNode)%IrrotationalCoeff(indx) = AppNode(indxNode)%IrrotationalCoeff(indx) + D_OutFace / (L_OutFace/2d0)
        END IF
                   
        !Advance InFaceID and OutFaceID
        InFaceID  = OutFaceID
        indx      = indxFace+2  ;  IF (indx .GT. NFace) indx = 1
        OutFaceID = AppNode(indxNode)%FaceID(indx)

      END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    IF (iStatParallel .EQ. -1) iStat = -1
      
  END SUBROUTINE ComputeIrrotationalCoeff


  ! -------------------------------------------------------------
  ! --- ACCUMULATE NODAL VALUES TO SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION AccumNodeValuesToSubregions(AppGrid,NodeValues) RESULT(RegionValues)
    CLASS(AppGridType),TARGET,INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)                   :: NodeValues(AppGrid%NNodes)
    REAL(8)                              :: RegionValues(AppGrid%NSubregions)
    
    !Local variables
    INTEGER                      :: iRegion,indxElem,NVertex
    INTEGER,POINTER              :: pVertex(:)
    TYPE(AppElementType),POINTER :: pAppElement
    
    !Initialize
    RegionValues = 0.0
    
    !Aggregate node values for each subregion
    DO indxElem=1,AppGrid%NElements
      pAppElement           => AppGrid%AppElement(indxElem)
      iRegion               =  pAppElement%Subregion
      NVertex               =  AppGrid%NVertex(indxElem)
      pVertex               => AppGrid%Vertex(1:NVertex,indxElem)
      RegionValues(iRegion) =  RegionValues(iRegion) + SUM(NodeValues(pVertex) * pAppElement%VertexArea(1:NVertex) / AppGrid%AppNode(pVertex)%Area)
    END DO
    
    NULLIFY (pAppElement , pVertex)
    
  END FUNCTION AccumNodeValuesToSubregions
  
  
  ! -------------------------------------------------------------
  ! --- ACCUMULATE VALUES AT A SET OF NODES TO SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION AccumSomeNodeValuesToSubregions(AppGrid,Nodes,NodeValues) RESULT(RegionValues)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: Nodes(:)
    REAL(8),INTENT(IN)            :: NodeValues(:)
    REAL(8)                       :: RegionValues(AppGrid%NSubregions)
    
    !Local variables
    INTEGER :: iRegion,indxElem,indxNode,NodeGW,Elem,iLoc,NSubregions
    REAL(8) :: NodalArea
    
    !Initialize
    NSubregions    =  AppGrid%NSubregions
    RegionValues   =  0.0
    
    ASSOCIATE (pAppNode     => AppGrid%AppNode       , &
               pAppElement  => AppGrid%AppElement    )
    
      !Aggregate values at some nodes for each subregion
      DO indxNode=1,SIZE(Nodes)
        NodeGW    =  Nodes(indxNode)
        NodalArea =  pAppNode(NodeGW)%Area
        ASSOCIATE (pSElems   => pAppNode(NodeGW)%SurroundingElement)
          DO indxElem=1,SIZE(pSElems)
            Elem                  =  pSElems(indxElem)
            iRegion               =  pAppElement(Elem)%Subregion
            iLoc                  =  LocateInList(NodeGW,AppGrid%Vertex(:,Elem))
            RegionValues(iRegion) =  RegionValues(iRegion) + NodeValues(indxNode)*pAppElement(Elem)%VertexArea(iLoc)/NodalArea
          END DO
        END ASSOCIATE
      END DO
      
    END ASSOCIATE
            
  END FUNCTION AccumSomeNodeValuesToSubregions


  ! -------------------------------------------------------------
  ! --- ACCUMULATE ELEMENT VALUES TO SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION AccumElemValuesToSubregions(AppGrid,ElemValues) RESULT(RegionValues)
    CLASS(AppGridType),TARGET,INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)                   :: ElemValues(AppGrid%NElements)
    REAL(8)                              :: RegionValues(AppGrid%NSubregions)
    
    !Local variables
    INTEGER         :: indxRegion,indxElem
    INTEGER,POINTER :: pElemSubregion(:)
    
    !Initialize
    pElemSubregion => AppGrid%AppElement%Subregion
    RegionValues   =  0.0
    
    !Aggregate elememt values for each subregion
    DO indxElem=1,AppGrid%NElements
      indxRegion               = pElemSubregion(indxElem)
      RegionValues(indxRegion) = RegionValues(indxRegion) + ElemValues(indxElem)
    END DO
    
  END FUNCTION AccumElemValuesToSubregions


  ! -------------------------------------------------------------
  ! --- ACCUMULATE SOME ELEMENT VALUES TO SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION AccumSomeElemValuesToSubregions(AppGrid,Elems,ElemValues) RESULT(RegionValues)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: Elems(:)
    REAL(8),INTENT(IN)            :: ElemValues(:)
    REAL(8)                       :: RegionValues(AppGrid%NSubregions)
    
    !Local variables
    INTEGER :: indxElem,iRegion,iElem
    
    !Initialize
    RegionValues = 0.0
    
    !Aggregate some element values to subregions
    DO indxElem=1,SIZE(Elems)
      iElem                 = Elems(indxElem)
      iRegion               = AppGrid%AppElement(iElem)%Subregion
      RegionValues(iRegion) = RegionValues(iRegion) + ElemValues(indxElem)
    END DO
    
  END FUNCTION AccumSomeElemValuesToSubregions


  ! -------------------------------------------------------------
  ! --- COMPUTE AREA-AVERAGE ELEMENT DATA FROM NODE DATA
  ! -------------------------------------------------------------
  SUBROUTINE AreaAverage_ElemData_From_NodeData(AppGrid,NodeData,ElemData)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)            :: NodeData(:)
    REAL(8),INTENT(OUT)           :: ElemData(:)

    !Local variables
    INTEGER              :: indxElem,indxVertex,Vertex(4),iNode
    TYPE(AppElementType) :: AppElement

    !Initialize
    ElemData = 0.0

    !Distribute node data to elements
    DO indxElem=1,AppGrid%NElements
      AppElement = AppGrid%AppElement(indxElem)
      Vertex     = AppGrid%Vertex(:,indxElem)
      DO indxVertex=1,AppGrid%NVertex(indxElem)
        iNode              = Vertex(indxVertex)
        ElemData(indxElem) = ElemData(indxElem) + NodeData(iNode) * AppElement%VertexArea(indxVertex)
      END DO
      ElemData(IndxElem) = ElemData(indxElem) / AppElement%Area
    END DO

  
  END SUBROUTINE AreaAverage_ElemData_From_NodeData
  
  
  ! -------------------------------------------------------------
  ! --- GIVEN INFO FOR ELEMENTS, DISTRIBUTE THEM TO NODES
  ! -------------------------------------------------------------
  SUBROUTINE ElemData_To_NodeData(AppGrid,ElemData,NodeData)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)            :: ElemData(AppGrid%NElements)
    REAL(8),INTENT(OUT)           :: NodeData(AppGrid%NNodes)

    !Local variables
    INTEGER              :: indxElem,indxVertex,Vertex(4),Node
    TYPE(AppElementType) :: AppElement

    !Initialize
    NodeData = 0.0

    !Distribute element data to nodes
    DO indxElem=1,AppGrid%NElements
      AppElement = AppGrid%AppElement(indxElem)
      Vertex     = AppGrid%Vertex(:,indxElem)
      DO indxVertex=1,AppGrid%NVertex(indxElem)
        Node           = Vertex(indxVertex)
        NodeData(Node) = NodeData(Node) + ElemData(indxElem)*AppElement%VertexAreaFraction(indxVertex)
      END DO
    END DO

  END SUBROUTINE ElemData_To_NodeData


  ! -------------------------------------------------------------
  ! --- GIVEN INFO FOR NODES, DISTRIBUTE THEM TO ELEMENTS
  ! -------------------------------------------------------------
  PURE SUBROUTINE NodeData_To_ElemData(AppGrid,NodeData,ElemData)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)            :: NodeData(:)
    REAL(8),INTENT(OUT)           :: ElemData(:)

    !Local variables
    INTEGER              :: indxElem,indxVertex,Vertex(4),iNode
    TYPE(AppElementType) :: AppElement

    !Initialize
    ElemData = 0.0

    !Distribute node data to elements
    DO indxElem=1,AppGrid%NElements
        AppElement = AppGrid%AppElement(indxElem)
        Vertex     = AppGrid%Vertex(:,indxElem)
        DO indxVertex=1,AppGrid%NVertex(indxElem)
            iNode              = Vertex(indxVertex)
            ElemData(indxElem) = ElemData(indxElem) + NodeData(iNode) * AppElement%VertexArea(indxVertex) / AppGrid%AppNode(iNode)%Area
        END DO
    END DO

  END SUBROUTINE NodeData_To_ElemData
  
  
  ! -------------------------------------------------------------
  ! --- IDENTIFY BOUNDARY NODES
  ! -------------------------------------------------------------
  SUBROUTINE ListBoundaryNodes(AppGrid,NodeList,iStat)
    TYPE(AppGridType),TARGET,INTENT(IN) :: AppGrid
    INTEGER,ALLOCATABLE,INTENT(OUT)     :: NodeList(:)
    INTEGER,INTENT(OUT)                 :: iStat

    !Local variables
    TYPE(IntegerLinkedListType) :: BndNodeList
    INTEGER                     :: indxFace,ErrorCode,iStatParallel
    INTEGER,ALLOCATABLE         :: iWorkArray(:)
    
    !Initialize
    iStat         = 0
    iStatParallel = 0

    !Find the boundary nodes
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,BndNodeList,iStatParallel) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indxFace=1,AppGrid%NFaces
        IF (.NOT. AppGrid%AppFace%BoundaryFace(indxFace)) CYCLE
        !Add both nodes of element face to list
        !$OMP CRITICAL
        CALL BndNodeList%AddNode(AppGrid%AppFace%Node(1,indxFace),iStat)  ;  IF (iStat .EQ. -1) iStatParallel = -1
        CALL BndNodeList%AddNode(AppGrid%AppFace%Node(2,indxFace),iStat)  ;  IF (iStat .EQ. -1) iStatParallel = -1
        !$OMP END CRITICAL
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    IF (iStatParallel .EQ. -1) THEN
        iStat = -1
        RETURN
    END IF

    !Transfer unique,ordered data to return argument
    CALL BndNodeList%GetArray(iWorkArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL GetUniqueArrayComponents(iWorkArray,NodeList)
    CALL ShellSort(NodeList)
    
    !Free memory
    CALL BndNodeList%Delete()
    DEALLOCATE (iWorkArray , STAT=ErrorCode)
    
  END SUBROUTINE ListBoundaryNodes


  ! -------------------------------------------------------------
  ! --- GATHER THE LIST OF FACES CONNECTING AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE ListFacesAtNode(AppGrid,NodeNo,TheList,iStat)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    INTEGER,INTENT(IN)              :: NodeNo
    INTEGER,ALLOCATABLE,INTENT(OUT) :: TheList(:)
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    INTEGER                     :: indx
    TYPE(IntegerLinkedListType) :: FaceList
    
    !Initialize
    iStat = 0

    !Create the face list connecting at the node
    DO indx=1,AppGrid%NFaces
      IF (LocateInList(NodeNo,AppGrid%AppFace%Node(:,indx)) .GT. 0) THEN
          CALL FaceList%AddNode(indx,iStat)
          IF (iStat .EQ. -1) RETURN
      END IF
    END DO

    !Save the ordered face list in the return array
    CALL FaceList%GetArray(TheList,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ShellSort(TheList)

    !Free memory
    CALL FaceList%Delete()

  END SUBROUTINE ListFacesAtNode


  ! -------------------------------------------------------------
  ! --- FIND ELEMENT ON THE COUNTER-CLOCKWISE SIDE OF FACE
  ! -------------------------------------------------------------
  SUBROUTINE ListElemID_CCW(AppGrid,NodeNo,TheList,iStat)
    TYPE(AppGridType),TARGET,INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)                  :: NodeNo
    INTEGER,ALLOCATABLE,INTENT(OUT)     :: TheList(:)
    INTEGER,INTENT(OUT)                 :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'ListElemID_CCW'
    INTEGER                      :: indxFace,indxElem,indxVertex,NextIndx,Elem,NVertex,Vertex(4),OtherNode,NFace,iFace
    INTEGER,POINTER              :: pFaceIDs(:)

    !Initialize
    iStat     =  0
    pFaceIDs  => AppGrid%AppNode(NodeNo)%FaceID
    
    !Allocate memory for the list
    NFace = AppGrid%AppNode(NodeNo)%NFaceID
    CALL AllocArray(TheList,NFace,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Identify the elements on the counter-clockwise direction of each face
    DO indxFace=1,NFace
        TheList(indxFace) = 0
        iFace             = pFaceIDs(indxFace)
        OtherNode         = AppGrid%AppFace%Node(1,iFace)  ;  IF (OtherNode .EQ. NodeNo) OtherNode = AppGrid%AppFace%Node(2,iFace)
        DO indxElem=1,2
            Elem = AppGrid%AppFace%Element(indxElem,iFace)
            IF (Elem .EQ. 0) CYCLE
            NVertex = AppGrid%NVertex(Elem)
            Vertex  = AppGrid%Vertex(:,Elem)
            DO indxVertex=1,NVertex
                IF (Vertex(indxVertex) .NE. NodeNo) CYCLE
                NextIndx = indxVertex+1 ; IF (NextIndx .GT. NVertex) NextIndx = 1
                IF (Vertex(NextIndx) .EQ. OtherNode) THEN
                    TheList(indxFace) = Elem
                    EXIT
                END IF
            END DO       
        END DO
    END DO

  END SUBROUTINE ListElemID_CCW


  ! -------------------------------------------------------------
  ! --- RE-ORDER FACES AT NODES IN COUNTER-CLOCKWISE DIRECTION
  ! -------------------------------------------------------------
  SUBROUTINE ReorderFacesInCCWDirection(AppGrid)
    TYPE(AppGridType),TARGET :: AppGrid
    
    !Local variables
    INTEGER :: indxNode,NFace,indxFace,FaceID,FaceIndexStart,OrderedIndex(50),FaceID_Ordered(50),ElemID_onCCWSide_Ordered(50)
    
    !Order information for each node
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indxNode=1,AppGrid%NNodes
        !Number of faces connecting at node         
        NFace = AppGrid%AppNode(indxNode)%NFaceID
    
        !Compile data at the node using face information
        SELECT CASE (AppGrid%AppNode(indxNode)%BoundaryNode)
            !For boundary nodes, start analysis from the face which has model outside to its clockwise side
            CASE (.TRUE.)
                DO indxFace=1,NFace
                    FaceID = AppGrid%AppNode(indxNode)%FaceID(indxFace)
                    IF (.NOT. AppGrid%AppFace%BoundaryFace(FaceID)) CYCLE
                    IF (AppGrid%AppNode(indxNode)%ElemID_onCCWSide(indxFace) .GT. 0) THEN
                        FaceIndexStart = indxFace
                        EXIT
                    END IF
                END DO
                        
            !For internal nodes, start analysis from the first face in pFaceID list
            CASE (.FALSE.)
                FaceIndexStart = 1
                        
        END SELECT 
        OrderedIndex(1:NFace)             = CompileNodalData(indxNode,NFace,AppGrid%AppNode(indxNode)%FaceID,AppGrid%AppNode(indxNode)%ElemID_onCCWSide,FaceIndexStart) 
        FaceID_Ordered(1:NFace)           = AppGrid%AppNode(indxNode)%FaceID(OrderedIndex(1:NFace))
        ElemID_onCCWSide_Ordered(1:NFace) = AppGrid%AppNode(indxNode)%ElemID_onCCWSide(OrderedIndex(1:NFace))
        
        !Store ordered information
        AppGrid%AppNode(indxNode)%FaceID           = FaceID_Ordered(1:NFace)
        AppGrid%AppNode(indxNode)%ElemID_onCCWSide = ElemID_onCCWSide_Ordered(1:NFace)
    END DO  
    !$OMP END DO
    !$OMP END PARALLEL
               
               
  CONTAINS
  
  
    ! ############################################
    ! --- ORDER FACES AT A NODE
    ! ############################################
    FUNCTION CompileNodalData(indxNode,NFace,FaceID,ElemID_onCCWSide,FaceIndexStart) RESULT(FaceFlowIndex)
      INTEGER,INTENT(IN) :: indxNode,NFace,FaceID(NFace),ElemID_onCCWSide(NFace),FaceIndexStart
      INTEGER            :: FaceFlowIndex(NFace)
      
      !Local variables
      INTEGER         :: indxFace,indx,ElemID,indxElemFace,ElemFaceID
      INTEGER,POINTER :: pElemFaces(:)
      
      !Initialize
      FaceFlowIndex    = 0
      FaceFlowIndex(1) = FaceIndexStart
      ElemID           = ElemID_onCCWSide(FaceIndexStart)
      
      DO indx=2,NFace
        indxFace = FaceFlowIndex(indx-1)
        !Find the face index which has ElemID on its clockwise side
        pElemFaces => AppGrid%AppElement(ElemID)%FaceID
        DO indxElemFace=1,SIZE(pElemFaces)
          ElemFaceID = pElemFaces(indxElemFace)
          IF (ElemFaceID .EQ. FaceID(indxFace)) CYCLE
          IF (ANY(AppGrid%AppFace%Node(:,ElemFaceID) .EQ. indxNode)) THEN
            FaceFlowIndex(indx) = LocateInList(ElemFaceID,FaceID)
            ElemID              = ElemID_onCCWSide(FaceFlowIndex(indx))
            EXIT
          END IF
        END DO
      END DO
    
    END FUNCTION CompileNodalData
  
  END SUBROUTINE ReorderFacesInCCWDirection
  

  ! -------------------------------------------------------------
  ! --- LIST THE FACES FOR AN ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE ListFacesForElement(AppGrid,ElemNo,TheList,iStat)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    INTEGER,INTENT(IN)              :: ElemNo
    INTEGER,ALLOCATABLE,INTENT(OUT) :: TheList(:)
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+19)  :: ThisProcedure = ModName // 'ListFacesForElement'
    INTEGER                       :: NVertex,indxFace,icount

    !Initialize
    iStat   = 0
    NVertex = AppGrid%NVertex(ElemNo)

    !Allocate the list
    CALL AllocArray(TheList,NVertex,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Find the face ids
    icount = 0
    DO indxFace=1,AppGrid%NFaces
      IF (LocateInList(ElemNo,AppGrid%AppFace%Element(:,indxFace)) .GT. 0) THEN
        icount          = icount+1
        TheList(icount) = indxFace
        IF (icount .EQ. NVertex) EXIT
      END IF
    END DO
      
  END SUBROUTINE ListFacesForElement
  

  ! -------------------------------------------------------------
  ! --- COMPILE SUBREGIONAL DATA
  ! -------------------------------------------------------------
  SUBROUTINE CompileSubregionalData(AppGrid,Subregion_IDs,cSubregionNames,iStat)
    TYPE(AppGridType),TARGET    :: AppGrid
    INTEGER,INTENT(IN)          :: Subregion_IDs(:)
    CHARACTER(LEN=*),INTENT(IN) :: cSubregionNames(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22)            :: ThisProcedure = ModName // 'CompileSubregionalData'
    INTEGER                                 :: indxRegion,NSubregions,ErrorCode,Subregion(AppGrid%NElements), &
                                               NRegionElements,ElemID(AppGrid%NElements),indxFace,ElemID1,    &
                                               ElemID2,indx,RegID1,RegID2,indxRegion1,NFace,NNeighborRegions, &
                                               RegionNo
    LOGICAL                                 :: lRegionAdded
    TYPE(AppRegionType),POINTER             :: pAppSubregion
    TYPE(NeighborRegionType),POINTER        :: pNeighborRegion
    TYPE(IntegerLinkedListType),ALLOCATABLE :: NeighborRegionList(:),FaceBetweenRegionsList(:,:)
    INTEGER,ALLOCATABLE                     :: iWorkArray(:)
    
    !Initialize
    iStat     = 0
    Subregion = AppGrid%AppElement%Subregion
    ElemID    = [(indx,indx=1,AppGrid%NElements)]
    
    !Number of subregions
    NSubregions         = SIZE(cSubregionNames)
    AppGrid%NSubregions = NSubregions
    ALLOCATE (AppGrid%AppSubregion(NSubregions) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for subregional data of the application grid!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Subregion IDs and names
    DO indxRegion=1,NSubregions
        AppGrid%AppSubregion(indxRegion)%ID = Subregion_IDs(indxRegion)
        AppGrid%AppSubregion(indxRegion)%Name = TRIM(cSubregionNames(indxRegion)) // ' (SR' // TRIM(IntToText(Subregion_IDs(indxRegion))) // ')'
    END DO
    
    !Elements in each subregion and subregional area
    DO indxRegion=1,NSubregions
      pAppSubregion                 => AppGrid%AppSubregion(indxRegion) 
      NRegionElements               =  COUNT(Subregion.EQ.indxRegion)
      pAppSubregion%NRegionElements =  NRegionElements
      ALLOCATE (pAppSubregion%RegionElements(NRegionElements) ,STAT=ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for the list of elements in subregion '//TRIM(IntToText(indxRegion))//'!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF          
      pAppSubregion%RegionElements = PACK(ElemID , Subregion .EQ. indxRegion)
      pAppSubregion%Area           = SUM(AppGrid%AppElement(pAppSubregion%RegionElements)%Area) 
    END DO
    
    !Check to make sure that all subregions have at least one element
    DO indxRegion=1,NSubregions
      IF (AppGrid%AppSubregion(indxRegion)%NRegionElements .EQ. 0) THEN
        MessageArray(1) = 'All subregions must have at least one element.'
        MessageArray(2) = 'Subregion ' // TRIM(IntToText(indxRegion)) // ' has no elements!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF 
    END DO
    
    !Compile data for neighboring regions
    ALLOCATE (NeighborRegionList(NSubregions) , FaceBetweenRegionsList(NSubregions,NSubregions+1))
    DO indxFace=1,AppGrid%NFaces
      ElemID1 = AppGrid%AppFace%Element(1,indxFace)
      ElemID2 = AppGrid%AppFace%Element(2,indxFace)
      RegID1  = NSubregions+1  ;  IF (ElemID1 .NE. 0) RegID1 = AppGrid%AppElement(ElemID1)%Subregion
      RegID2  = NSubregions+1  ;  IF (ElemID2 .NE. 0) RegID2 = AppGrid%AppElement(ElemID2)%Subregion
      IF (RegID1 .EQ. RegID2) CYCLE
      
      !Add the first subregion as the neighbor to the second
      IF (.NOT. RegID2 .EQ. NSubregions+1) THEN
          CALL NeighborRegionAdded(RegID1,RegID2,lRegionAdded,iStat)
          IF (iStat .EQ. -1) RETURN
          IF (.NOT. lRegionAdded) THEN
              CALL NeighborRegionList(RegID2)%AddNode(RegID1,iStat)
              IF (iStat .EQ. -1) RETURN
          END IF
          CALL FaceBetweenRegionsList(RegID2,RegID1)%AddNode(indxFace,iStat)
          IF (iStat .EQ. -1) RETURN
      END IF
      
      !Add the second subregion as the neighbor to the first
      IF (.NOT. RegID1 .EQ. NSubregions+1) THEN
          CALL NeighborRegionAdded(RegID2,RegID1,lRegionAdded,iStat)
          IF (iStat .EQ. -1) RETURN
          IF (.NOT. lRegionAdded) THEN
              CALL NeighborRegionList(RegID1)%AddNode(RegID2,iStat)
              IF (iStat .EQ. -1) RETURN
          END IF
          CALL FaceBetweenRegionsList(RegID1,RegID2)%AddNode(indxFace,iStat)
          IF (iStat .EQ. -1) RETURN
      END IF
    END DO
    
    !Store compiled data permenanetly
    DO indxRegion=1,NSubregions
      DEALLOCATE (iWorkArray , STAT=ErrorCode)
      pAppSubregion                  => AppGrid%AppSubregion(indxRegion)
      NNeighborRegions               =  NeighborRegionList(indxRegion)%GetNNodes()
      ALLOCATE (pAppSubregion%NeighborRegions(NNeighborRegions) , iWorkArray(NNeighborRegions))
      CALL NeighborRegionList(indxRegion)%GetArray(iWorkArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL ShellSort(iWorkArray)
      pAppSubregion%NNeighborRegions         = NNeighborRegions
      pAppSubregion%NeighborRegions%RegionNo = iWorkArray
      DO indx=1,NNeighborRegions
        pNeighborRegion             => pAppSubregion%NeighborRegions(indx)
        RegionNo                    =  pNeighborRegion%RegionNo
        NFace                       =  FaceBetweenRegionsList(indxRegion,RegionNo)%GetNNodes()
        pNeighborRegion%NRegBndFace =  NFace
        CALL FaceBetweenRegionsList(indxRegion,RegionNo)%GetArray(pNeighborRegion%RegBndFace,iStat)  
        IF (iStat .EQ. -1) RETURN
      END DO
    END DO
    
    !Clear memory
    DO indxRegion=1,NSubregions
      CALL NeighborRegionList(indxRegion)%Delete()
      DO indxRegion1=1,NSubregions+1
        CALL FaceBetweenRegionsList(indxRegion,indxRegion1)%Delete()
      END DO
    END DO
    DEALLOCATE (NeighborRegionList , FaceBetweenRegionsList , iWorkArray , STAT=ErrorCode)
    
  CONTAINS
    
    ! ############################################
    ! --- CHECK IF THE NEIGHBOR REGION IS ALREADY IN THE LIST
    ! ############################################
    SUBROUTINE NeighborRegionAdded(AddRegID,ToRegID,RegionAdded,iStat)
      INTEGER,INTENT(IN)  :: AddRegID,ToRegID
      LOGICAL,INTENT(OUT) :: RegionAdded
      INTEGER,INTENT(OUT) :: iStat
      
      !Local variables
      INTEGER             :: iErr
      INTEGER,ALLOCATABLE :: iArray(:)
      
      !Initialize
      iStat = 0
      DEALLOCATE (iArray , STAT=iErr)
      CALL NeighborRegionList(ToRegID)%GetArray(iArray,iStat)
      IF (iStat .EQ. -1) RETURN
           
      !Check
      IF (LocateInList(AddRegID,iArray) .GT. 0) THEN
          RegionAdded = .TRUE.
      ELSE
          RegionAdded = .FALSE.
      END IF
        
    END SUBROUTINE NeighborRegionAdded


  END SUBROUTINE CompileSubregionalData
  
  
  ! -------------------------------------------------------------
  ! --- CHECK FOR POSITIVE STIFFNESS MATRIX ENTRIES
  ! -------------------------------------------------------------
  SUBROUTINE CheckForPositiveStiffness(NElements,NNodes,NVertex,Vertex,AppNode,AppElement)
    INTEGER,INTENT(IN)              :: NElements,NNodes,NVertex(:),Vertex(:,:)
    TYPE(AppNodeType),INTENT(IN)    :: AppNode(NNodes)
    TYPE(AppElementType),INTENT(IN) :: AppElement(NElements)
    
    !Local variables
    INTEGER :: indxElem,indxNode,indxNode1,iCount,iNode,iElem,iRow,iCol, &
               iRowTemp,indx
    REAL(8) :: rValue
        
    !Check
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NNodes,AppNode,NVertex,Vertex,AppElement) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indxNode=1,NNodes
      DO indxNode1=1,AppNode(indxNode)%NConnectedNode
        iNode = AppNode(indxNode)%ConnectedNode(indxNode1)
        
        !If these nodes are processed, cycle
        IF (iNode .LT. indxNode) CYCLE
        
        !Initialize
        iCount = 0
        rValue = 0.0
        
        !Find elements that have these nodes as vertices, and process information
        DO indxElem=1,SIZE(AppNode(indxNode)%SurroundingElement)
          iElem = AppNode(indxNode)%SurroundingElement(indxElem)
          IF (ANY(Vertex(:,iElem) .EQ. indxNode)) THEN
            IF (ANY(Vertex(:,iElem) .EQ. iNode)) THEN
              iCount   = iCount + 1
              iRowTemp = LocateInList(indxNode,Vertex(:,iElem))
              iCol     = LocateInList(iNode,Vertex(:,iElem))
              iRow     = MIN(iRowTemp , iCol)
              iCol     = MAX(iRowTemp , iCol)
              
              !Index number in the integrals
              indx    = (iRow-1)*NVertex(iElem)-iRow*(iRow-1)/2+iCol-iRow
              
              !Accumulate integral value
              rValue = rValue + AppElement(iElem)%Integral_DELShpI_DELShpJ(indx)
              
              !If 2 elements have already been found, exit the element loop
              IF (iCount .EQ. 2) EXIT
              
            END IF
          END IF
        END DO
        
        !If the integral coefficent is gereater than 0, issue a warning
        !IF (rValue .GT. 0.0) THEN
        !  WRITE (MessageArray(1),'(A,F5.3,A)') 'The stiffness coefficient for groundwater nodes '//TRIM(IntToText(indxNode))//' and '//TRIM(IntToText(iNode))//' is positive (',rValue,')!' 
        !  CALL LogMessage(MessageArray(1),f_iWarn,ThisProcedure)
        !END IF
      END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
  
  END SUBROUTINE CheckForPositiveStiffness

  
  ! -------------------------------------------------------------
  ! --- CEHCK IF A NODE IS BOUNDARY NODE
  ! -------------------------------------------------------------
  PURE FUNCTION IsBoundaryNode(AppGrid,iNode) RESULT(lBoundaryNode)
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)            :: iNode  !This is the index of the node, not the ID
    LOGICAL                       :: lBoundaryNode
    
    lBoundaryNode = AppGrid%AppNode(iNode)%BoundaryNode

  END FUNCTION IsBoundaryNode
  
END MODULE