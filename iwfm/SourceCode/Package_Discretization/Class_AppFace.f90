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
MODULE Class_AppFace
  !$ USE OMP_LIB
  USE GenericLinkedList  , ONLY: GenericLinkedListType
  USE MessageLogger      , ONLY: SetLastMessage            , &
                                 f_iFatal
  USE GeneralUtilities   , ONLY: LocateInList
  USE IOInterface        , ONLY: GenericFileType       
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
  PUBLIC :: AppFaceType              
    
    
  ! -------------------------------------------------------------
  ! --- AQUIFER FACE DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppFaceType
      INTEGER,ALLOCATABLE :: Node(:,:)               !Nodes that make the face; defined as (2,face)
      INTEGER,ALLOCATABLE :: Element(:,:)            !Elements on both sides of the face; defined as (2,face) array
      REAL(8),ALLOCATABLE :: Length(:)               !Length of face; defined for each (face)
      LOGICAL,ALLOCATABLE :: BoundaryFace(:)         !If boundary face; defined for each (face)
  CONTAINS
      PROCEDURE,PASS :: Construct          
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: GetFaceGivenNodes
      PROCEDURE,PASS :: GetNFaces
      PROCEDURE,PASS :: ReadData
      PROCEDURE,PASS :: WriteData
      GENERIC        :: New                => ReadData   , & 
                                              Construct
  END TYPE AppFaceType
  
  
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
  ! --- NEW APPLICATION FACES BY READING FROM A FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadData(AppFace,InFile,NFace,iStat)
    CLASS(AppFaceType),INTENT(OUT) :: AppFace
    INTEGER,INTENT(IN)             :: NFace
    TYPE(GenericFileType)          :: InFile
    INTEGER,INTENT(OUT)            :: iStat
   
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'ReadData'
    INTEGER                               :: ErrorCode,indx
    CHARACTER                             :: cErrorMsg*500
    
    ALLOCATE (AppFace%Node(2,NFace)            , &
              AppFace%Element(2,NFace)         , &
              AppFace%Length(NFace)            , &
              AppFace%BoundaryFace(NFace)      , &
              STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for element faces!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
              
    DO indx=1,NFace
        CALL InFile%ReadData(AppFace%Node(:,indx),iStat)          ;  IF (iStat .EQ. -1) RETURN        
        CALL InFile%ReadData(AppFace%Element(:,indx),iStat)       ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(AppFace%Length(indx),iStat)          ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(AppFace%BoundaryFace(indx),iStat)    ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE ReadData
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE FACE LIST
  ! -------------------------------------------------------------
  SUBROUTINE Construct(AppFace,NVertex,Vertex,X,Y,iStat)
    CLASS(AppFaceType),INTENT(OUT) :: AppFace
    INTEGER,INTENT(IN)             :: NVertex(:),Vertex(:,:)
    REAL(8),INTENT(IN)             :: X(:),Y(:)
    INTEGER,INTENT(OUT)            :: iStat

    !Local data type - data for one face
    TYPE FaceDataType
        INTEGER :: Node(2)      = 0
        INTEGER :: Element(2)   = 0
        REAL(8) :: Length       = 0.0
        LOGICAL :: BoundaryFace = .FALSE.
    END TYPE
    
    !Local data type - element face info in a linked list
    TYPE,EXTENDS(GenericLinkedListType) :: AppFaceListType
    END TYPE AppFaceListType
    
    !Local variables
    CHARACTER(LEN=ModNameLen+9) :: ThisProcedure=ModName//'Construct'
    CHARACTER                   :: cErrorMsg*200
    INTEGER                     :: indxElem1,indxElem2,indx,NFace,indxF,NodeI,NodeF,NElements,NVertexi,ErrorCode,Elem2,iStatTemp
    REAL(8)                     :: x0,x1,y0,y1
    TYPE(FaceDataType)          :: anAppFace
    TYPE(AppFaceListType)       :: AppFaceList
    LOGICAL                     :: AddFace
    CLASS(*),POINTER            :: pCurrent

    !Initialize
    iStat     = 0
    NElements = SIZE(NVertex)
    
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NVertex,Vertex,NElements,X,Y,AppFaceList) NUM_THREADS(OMP_GET_NUM_PROCS()-1) 
    !$OMP DO SCHEDULE(STATIC,500)
    !Construct the face list
    DO indxElem1=1,NElements
      NVertexi  =  NVertex(indxElem1)
      DO indx=1,NVertexi
        NodeI   = Vertex(indx,indxElem1)
        indxF   = indx+1  ;  IF (indxF .GT. NVertexi) indxF = 1
        NodeF   = Vertex(indxF,indxElem1)
        Elem2   = 0
        AddFace = .TRUE.
        
        !Find other element that shares the face
        DO indxElem2=1,NElements
          IF (indxElem1 .EQ. indxElem2) CYCLE
          IF (LocateInList(NodeI,Vertex(:,indxElem2)) .GT. 0) THEN
            IF (LocateInList(NodeF,Vertex(:,indxElem2)) .GT. 0) THEN
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
    ALLOCATE(AppFace%Node(2,NFace)            , &
             AppFace%Element(2,NFace)         , &
             AppFace%Length(NFace)            , &
             AppFace%BoundaryFace(NFace)      , &
             STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for element faces!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL AppFaceList%Reset()
    DO indx=1,NFace
        pCurrent => AppFaceList%GetCurrentValue()
        SELECT TYPE (pCurrent)
           TYPE IS (FaceDataType)
              AppFace%Node(:,indx)       = pCurrent%Node
              AppFace%Element(:,indx)    = pCurrent%Element
              AppFace%Length(indx)       = pCurrent%Length
              AppFace%BoundaryFace(indx) = pCurrent%BoundaryFace
        END SELECT
        CALL AppFaceList%Next()
    END DO

    !Clear memory from temporary face list
    CALL AppFaceList%Delete()

  END SUBROUTINE Construct
  
  


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
  ! --- KILL APPLICATION FACES 
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppFace)
    CLASS(AppFaceType) :: AppFace
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (AppFace%Node , AppFace%Element , AppFace%Length , AppFace%BoundaryFace , STAT=ErrorCode)
    
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
  ! --- GET NUMBER OF FACES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNFaces(AppFace) RESULT(NFaces)
    CLASS(AppFaceType),INTENT(IN) :: AppFace
    INTEGER                       :: NFaces
    
    NFaces = SIZE(AppFace%Length)
    
  END FUNCTION GetNFaces
  
  
  ! -------------------------------------------------------------
  ! --- FIND THE FACE NUMBER GIVEN THE NODES
  ! -------------------------------------------------------------
  FUNCTION GetFaceGivenNodes(AppFace,Nodes) RESULT(FaceID)
    CLASS(AppFaceType),INTENT(IN) :: AppFace
    INTEGER,INTENT(IN)            :: Nodes(2)
    INTEGER                       :: FaceID

    !Local variables
    INTEGER :: indx

    !Initialize
    FaceID = 0

    !Find the face number that is made up of Nodes(2)
    DO indx=1,SIZE(AppFace%Length)
        IF (MINVAL(AppFace%Node(:,indx)).EQ.MINVAL(Nodes) .AND. MAXVAL(AppFace%Node(:,indx)).EQ.MAXVAL(Nodes)) THEN
           FaceID = indx
           EXIT
        END IF
    END DO

  END FUNCTION GetFaceGivenNodes
  
  
  
  
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
  SUBROUTINE WriteData(AppFace,OutFile)
    CLASS(AppFaceType),INTENT(IN) :: AppFace
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(AppFace%BoundaryFace)
        CALL OutFile%WriteData(AppFace%Node(:,indx))       
        CALL OutFile%WriteData(AppFace%Element(:,indx))
        CALL OutFile%WriteData(AppFace%Length(indx))
        CALL OutFile%WriteData(AppFace%BoundaryFace(indx))
    END DO
      
  END SUBROUTINE WriteData


END MODULE