!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2018  
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
MODULE Class_AppFace
  !$ USE OMP_LIB
  USE GenericLinkedList  , ONLY: GenericLinkedListType
  USE MessageLogger      , ONLY: SetLastMessage            , &
                                 iFatal
  USE GeneralUtilities   , ONLY: LocateInList
  USE IOInterface        , ONLY: GenericFileType       
  USE Class_Grid         , ONLY: ElementType
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
  PUBLIC :: AppFaceType                , &
            AppFace_New                , &
            AppFace_Construct          , &
            AppFace_GetFaceGivenNodes  , &   
            AppFace_WriteData
    
    
  ! -------------------------------------------------------------
  ! --- AQUIFER FACE DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppFaceType
    INTEGER :: Node(2)       = 0        !Nodes that make the face
    INTEGER :: Element(2)    = 0        !Elements on both sides of the face
    REAL(8) :: Length        = 0.0      !Length of face
    LOGICAL :: BoundaryFace  = .FALSE.  !If boundary face
  CONTAINS
    PROCEDURE,PASS :: NewAppFace
    PROCEDURE,PASS :: AppFace_ReadData_ForOne
    GENERIC        :: New                => NewAppFace                , &
                                            AppFace_ReadData_ForOne   
  END TYPE AppFaceType
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  INTERFACE AppFace_New
    MODULE PROCEDURE AppFace_ReadData_ForMany
  END INTERFACE AppFace_New
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppFace::'


  
  
  
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
  ! --- NEW APPLICATION FACE 
  ! -------------------------------------------------------------
  SUBROUTINE NewAppFace(AppFace,Node,Element,Length,BoundaryFace) 
    CLASS(AppFaceType),INTENT(OUT) :: AppFace
    INTEGER,INTENT(IN)             :: Node(2) , Element(2)
    REAL(8),INTENT(IN)             :: Length
    LOGICAL,INTENT(IN)             :: BoundaryFace

    AppFace%Node         = Node
    AppFace%Element      = Element
    AppFace%Length       = Length
    AppFace%BoundaryFace = BoundaryFace

  END SUBROUTINE NewAppFace
  
  
  ! -------------------------------------------------------------
  ! --- NEW APPLICATION FACE BY READING FROM A FILE
  ! -------------------------------------------------------------
 SUBROUTINE AppFace_ReadData_ForOne(AppFace,InFile,iStat) 
    CLASS(AppFaceType),INTENT(OUT) :: AppFace
    TYPE(GenericFileType)          :: InFile
    INTEGER,INTENT(OUT)            :: iStat
    
    CALL InFile%ReadData(AppFace%Node,iStat)          ;  IF (iStat .EQ. -1) RETURN        
    CALL InFile%ReadData(AppFace%Element,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppFace%Length,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppFace%BoundaryFace,iStat)      
    
  END SUBROUTINE AppFace_ReadData_ForOne


  ! -------------------------------------------------------------
  ! --- NEW APPLICATION FACES BY READING FROM A FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppFace_ReadData_ForMany(AppFace,InFile,NFace,iStat)
    TYPE(AppFaceType),INTENT(OUT) :: AppFace(:)
    INTEGER,INTENT(IN)            :: NFace
    TYPE(GenericFileType)         :: InFile
    INTEGER,INTENT(OUT)           :: iStat
   
    !Local variables
    INTEGER :: indx
        
    DO indx=1,NFace
      CALL InFile%ReadData(AppFace(indx)%Node,iStat)          ;  IF (iStat .EQ. -1) RETURN        
      CALL InFile%ReadData(AppFace(indx)%Element,iStat)       ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppFace(indx)%Length,iStat)        ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(AppFace(indx)%BoundaryFace,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE AppFace_ReadData_ForMany
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE FACE LIST
  ! -------------------------------------------------------------
  SUBROUTINE AppFace_Construct(Element,X,Y,FaceList,iStat)
    TYPE(ElementType),INTENT(IN)              :: Element(:)
    REAL(8),INTENT(IN)                        :: X(:),Y(:)
    TYPE(AppFaceType),ALLOCATABLE,INTENT(OUT) :: FaceList(:)
    INTEGER,INTENT(OUT)                       :: iStat

    !Local data type - element face info in a linked list
    TYPE,EXTENDS(GenericLinkedListType) :: AppFaceListType
    END TYPE AppFaceListType
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure=ModName//'AppFace_Construct'
    CHARACTER                    :: cErrorMsg*200
    INTEGER                      :: indxElem1,indxElem2,indx,NFace,indxF,NodeI,NodeF,NElements,NVertex,ErrorCode,Elem2,iStatTemp
    REAL(8)                      :: x0,x1,y0,y1
    TYPE(AppFaceType)            :: anAppFace
    TYPE(AppFaceListType)        :: AppFaceList
    LOGICAL                      :: AddFace
    CLASS(*),POINTER             :: pCurrent

    !Initialize
    iStat     = 0
    NElements = SIZE(Element)
    
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(Element,NElements,X,Y,AppFaceList) NUM_THREADS(OMP_GET_NUM_PROCS()-1) 
    !$OMP DO SCHEDULE(STATIC,500)
    !Construct the face list
    DO indxElem1=1,NElements
      NVertex =  Element(indxElem1)%NVertex
      DO indx=1,NVertex
        NodeI   = Element(indxElem1)%Vertex(indx)
        indxF   = indx+1  ;  IF (indxF .GT. NVertex) indxF = 1
        NodeF   = Element(indxElem1)%Vertex(indxF)
        Elem2   = 0
        AddFace = .TRUE.
        
        !Find other element that shares the face
        DO indxElem2=1,NElements
          IF (indxElem1 .EQ. indxElem2) CYCLE
          IF (LocateInList(NodeI,Element(indxElem2)%Vertex) .GT. 0) THEN
            IF (LocateInList(NodeF,Element(indxElem2)%Vertex) .GT. 0) THEN
              IF (indxElem2 .LT. indxElem1) THEN
                AddFace = .FALSE.
                EXIT
              ELSE
                AddFace = .TRUE.
                Elem2   = indxElem2
                EXIT 
              END IF 
            END IF
          END IF
        END DO
        
        !Add face to list
        IF (.NOT. AddFace) CYCLE
        anAppFace%Node(1)    = MIN(NodeI,NodeF)
        anAppFace%Node(2)    = MAX(NodeI,NodeF)
        anAppFace%Element(1) = MIN(indxElem1,Elem2)
        anAppFace%Element(2) = MAX(indxElem1,Elem2)
        x0                   = X(anAppFace%Node(1))
        x1                   = X(anAppFace%Node(2))
        y0                   = Y(anAppFace%Node(1))
        y1                   = Y(anAppFace%Node(2))
        anAppFace%Length     = SQRT((x0-x1)*(x0-x1) + (y0-y1)*(y0-y1))
        IF (Elem2 .EQ. 0) THEN
          anAppFace%BoundaryFace = .TRUE. 
        ELSE
          anAppFace%BoundaryFace = .FALSE.
        END IF
        !$OMP CRITICAL
        CALL AppFaceList%AddNode(anAppFace,iStatTemp)
        IF (iStatTemp .EQ. -1) iStat = iStatTemp
        !$OMP END CRITICAL
        
      END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    !If an error ocuured, return
    IF (iStat .EQ. -1) RETURN

    !Transfer face data to function return argument
    NFace = AppFaceList%GetNNodes()
    ALLOCATE(FaceList(NFace) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for element faces!'//NEW_LINE('x')//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL AppFaceList%Reset()
    DO indx=1,NFace
        pCurrent => AppFaceList%GetCurrentValue()
        SELECT TYPE (pCurrent)
           TYPE IS (AppFaceType)
              FaceList(indx) = pCurrent
        END SELECT
        CALL AppFaceList%Next()
    END DO

    !Clear memory from temporary face list
    CALL AppFaceList%Delete()

  END SUBROUTINE AppFace_Construct
  
  


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
  ! --- FIND THE FACE NUMBER GIVEN THE NODES
  ! -------------------------------------------------------------
  FUNCTION AppFace_GetFaceGivenNodes(FaceList,Nodes) RESULT(FaceID)
    TYPE(AppFaceType),INTENT(IN) :: FaceList(:)
    INTEGER,INTENT(IN)           :: Nodes(2)
    INTEGER                      :: FaceID

    !Local variables
    INTEGER :: indx

    !Initialize
    FaceID = 0

    !Find the face number that is made up of Nodes(2)
    DO indx=1,SIZE(FaceList)
      ASSOCIATE (pFaceNodes => FaceList(indx)%Node)
        IF (MINVAL(pFaceNodes).EQ.MINVAL(Nodes) .AND. MAXVAL(pFaceNodes).EQ.MAXVAL(Nodes)) THEN
          FaceID = indx
          EXIT
        END IF
      END ASSOCIATE 
    END DO

  END FUNCTION AppFace_GetFaceGivenNodes
  
  
  
  
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
  ! --- WRITE APPLICATION FACE DATA TO A FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppFace_WriteData(NFace,AppFace,OutFile)
    INTEGER,INTENT(IN)           :: NFace
    TYPE(AppFaceType),INTENT(IN) :: AppFace(NFace)
    TYPE(GenericFileType)        :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,NFace
      CALL OutFile%WriteData(AppFace(indx)%Node)       
      CALL OutFile%WriteData(AppFace(indx)%Element)
      CALL OutFile%WriteData(AppFace(indx)%Length)
      CALL OutFile%WriteData(AppFace(indx)%BoundaryFace)
    END DO
      
  END SUBROUTINE AppFace_WriteData


END MODULE