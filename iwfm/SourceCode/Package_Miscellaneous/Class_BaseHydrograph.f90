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
MODULE Class_BaseHydrograph
  USE MessageLogger          , ONLY: SetLastMessage                 , &
                                     LogMessage                     , &
                                     MessageArray                   , &
                                     f_iFatal                       , &
                                     f_iInfo                          
  USE GeneralUtilities       , ONLY: ConvertID_To_Index             , &
                                     StripTextUntilCharacter        , &
                                     FirstLocation                  , &
                                     IntToText                      , &
                                     UpperCase                      , &
                                     ArrangeText                    , &
                                     EstablishAbsolutePathFileName  , &
                                     CleanSpecialCharacters         , &
                                     PrepareTitle
  USE TimeSeriesUtilities    , ONLY: TimeStepType                   , &
                                     IncrementTimeStamp
  USE IOInterface            , ONLY: GenericFileType                , &
                                     iGetFileType_FromName          , &
                                     f_iUNKNOWN                     , &
                                     f_iDSS
  USE GenericLinkedList      , ONLY: GenericLinkedListType 
  USE Package_Discretization , ONLY: AppGridType                    , &
                                     StratigraphyType               
  USE TSDFileHandler         , ONLY: RealTSDataInFileType           , &
                                     PrepareTSDOutputFile           , &
                                     ReadTSData
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
  PUBLIC :: BaseHydrographType     , &
            HydOutputType          , &
            f_iHyd_AtXY            , &
            f_iHyd_AtNode          , &
            f_iHyd_GWHead          , &
            f_iHyd_Subsidence
  
  
  ! -------------------------------------------------------------
  ! --- BASE HYDROGRAPH DATA TYPE
  ! -------------------------------------------------------------
  TYPE BaseHydrographType
      CHARACTER(LEN=30) :: cName    = ''     !Name of the hydrograph location
      INTEGER           :: ID       = 0      !Hydrograph ID
      INTEGER           :: iLayer   = 0      !Aquifer layer for the hydrograph 
      INTEGER           :: iElement = 0      !Element where hydrograph location is located at (for nodal hydrographs this is the first element found wjere nodes is located)
  END TYPE BaseHydrographType


  ! -------------------------------------------------------------
  ! --- HYDROGRAPH-AT-NODE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseHydrographType) :: HydAtNodeType
      PRIVATE
      INTEGER :: iNode = 0   !Node number where hydrograph print-out is needed
  END TYPE HydAtNodeType
  
  
  ! -------------------------------------------------------------
  ! --- HYDROGRAPH-AT-XY-COORDINATE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseHydrographType) :: HydAtXYType
      PRIVATE
      REAL(8)             :: X           = 0.0  !x-coordinate
      REAL(8)             :: Y           = 0.0  !y-coordinate
      INTEGER,ALLOCATABLE :: iNodes(:)          !Node numbers surrpunding the element iElement
      REAL(8),ALLOCATABLE :: rFactors(:)        !Scaling factor for each node of iNodes to compute the head at x-y location
  END TYPE HydAtXYType

      
  ! -------------------------------------------------------------
  ! --- HYDROGRAPH DATA TYPE TO KEEP THE ORDER AS SPECIFIED BY THE USER
  ! -------------------------------------------------------------
  TYPE OrderedHydType
      PRIVATE
      INTEGER :: iHydType  = -1
      INTEGER :: indx      = 0
  END TYPE OrderedHydType
  

  ! -------------------------------------------------------------
  ! --- HYDROGRAPH OUTPUT DATA TYPE
  ! -------------------------------------------------------------
  TYPE HydOutputType
      PRIVATE
      TYPE(GenericFileType),ALLOCATABLE      :: OutFile                        !Output file for the gw hydrographs
      TYPE(RealTSDataInFileType),ALLOCATABLE :: InFile_ForInquiry              !If the model is instantiated for post-processing this file is used to read hydrographs in; instead of the OutFile above
      INTEGER                                :: NHyd_AtNode         = 0        !Number of hydrographs defined at a node
      INTEGER                                :: NHyd_AtXY           = 0        !Number of hydrographs defined at x-y cooordinates
      TYPE(HydAtNodeType),ALLOCATABLE        :: Hyd_AtNode(:)                  !List of hydrograph-at-node locations
      TYPE(HydAtXYType),ALLOCATABLE          :: Hyd_AtXY(:)                    !List of hydrograph-at-xy-coordinate locations
      TYPE(OrderedHydType),ALLOCATABLE       :: OrderedHydList(:)              !List of hydrographs in order as given by the user
  CONTAINS
      PROCEDURE,PASS :: New          
      PROCEDURE,PASS :: Kill         
      PROCEDURE,PASS :: PrintResults 
      PROCEDURE,PASS :: ReadHydrograph_AtLocation
      PROCEDURE,PASS :: GetFileName
      PROCEDURE,PASS :: GetNHydrographs
      PROCEDURE,PASS :: GetHydrographIDs
      PROCEDURE,PASS :: GetHydrographCoordinates
      PROCEDURE,PASS :: GetHydrographNames
      PROCEDURE,PASS :: IsDefined
      PROCEDURE,PASS :: Transfer_to_HDF
  END TYPE HydOutputType  

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: f_iHyd_AtXY       = 0  , &
                                         f_iHyd_AtNode     = 1  , &
                                         f_iHyd_GWHead     = 1  , &
                                         f_iHyd_Subsidence = 2
  INTEGER,PARAMETER                   :: ModNameLen  = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'Class_BaseHydrograph::'
  


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
  ! --- NEW OUTPUT FOR USER-SPECIFIED HYDROGRAPH PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE New(HydOutput,IsForInquiry,InFile,cWorkingDirectory,AppGrid,Stratigraphy,iGWNodeIDs,iHydFor,UNITLTOU,CPart,TimeStep,iStat)
    CLASS(HydOutputType)              :: HydOutput
    LOGICAL,INTENT(IN)                :: IsForInquiry
    TYPE(GenericFileType)             :: InFile
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: iGWNodeIDs(:),iHydFor
    CHARACTER(LEN=*),INTENT(IN)       :: cWorkingDirectory,UNITLTOU,CPart
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER                  :: NHyd,indx
    REAL(8)                  :: FactXY
    CHARACTER                :: cFileName*1200,ALine*1000
    CHARACTER(:),ALLOCATABLE :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Read the general data
    CALL InFile%ReadData(NHyd,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactXY,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(cFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  cFileName = StripTextUntilCharacter(cFileName,'/')  ;  CALL CleanSpecialCharacters(cFileName)
    
    !If the filename is empty or the number of hydrographs is set to zero, return
    IF (cFileName .EQ. '') THEN
        DO indx=1,NHyd
            CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        END DO
        RETURN
    ELSE
        IF (NHyd .EQ. 0) RETURN
    END IF
    
    !Read the hydrograph data
    CALL HydrographList_New(AppGrid,Stratigraphy,iGWNodeIDs,iHydFor,NHyd,FactXY,InFile,HydOutput%Hyd_AtNode,HydOutput%Hyd_AtXY,HydOutput%OrderedHydList,iStat)
    IF (iStat .EQ. -1) RETURN
    HydOutput%NHyd_AtNode = SIZE(HydOutput%Hyd_AtNode)
    HydOutput%NHyd_AtXY   = SIZE(HydOutput%Hyd_AtXY)

    !Instantiate the hydrograph output file
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cFileName)),cWorkingDirectory,cAbsPathFileName)
    IF (IsForInquiry) THEN
        ALLOCATE (HydOutput%InFile_ForInquiry)
        CALL PrepHydInFile_ForInquiry(cAbsPathFileName,iGWNodeIDs,AppGrid%AppElement%ID,iHydFor,CPart,HydOutput%Hyd_AtNode,HydOutput%Hyd_AtXY,HydOutput%OrderedHydList,TimeStep,HydOutput%InFile_ForInquiry,iStat)
    ELSE
        ALLOCATE (HydOutput%OutFile)
        CALL PrepHydOutFile(cAbsPathFileName,iGWNodeIDs,AppGrid%AppElement%ID,iHydFor,UNITLTOU,CPart,HydOutput%Hyd_AtNode,HydOutput%Hyd_AtXY,HydOutput%OrderedHydList,TimeStep,HydOutput%OutFile,iStat)
    END IF
    
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- NEW LIST OF HYDROGRAPH DATA
  ! -------------------------------------------------------------
  SUBROUTINE HydrographList_New(AppGrid,Stratigraphy,iGWNodeIDs,iHydFor,NHyd,FactXY,InFile,Hyd_AtNode,Hyd_AtXY,OrderedHydList,iStat)
    TYPE(AppGridType),INTENT(IN)                 :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)            :: Stratigraphy
    INTEGER,INTENT(IN)                           :: iGWNodeIDs(:),iHydFor,NHyd
    REAL(8),INTENT(IN)                           :: FactXY
    TYPE(GenericFileType)                        :: InFile
    TYPE(HydAtNodeType),ALLOCATABLE,INTENT(OUT)  :: Hyd_AtNode(:)
    TYPE(HydAtXYType),ALLOCATABLE,INTENT(OUT)    :: Hyd_AtXY(:)
    TYPE(OrderedHydType),ALLOCATABLE,INTENT(OUT) :: OrderedHydList(:)
    INTEGER,INTENT(OUT)                          :: iStat
    
    !Local data types
    TYPE,EXTENDS(GenericLinkedListType) :: HydListType
    END TYPE HydListType
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'HydrographList_New'
    INTEGER                      :: indx,ErrorCode,ID,iHydType,iHydLayer,iHydNode,indx1,iLoc,        &
                                    iElem,iSize,NHyd_AtXY,NHyd_AtNode,iCount_AtNode,iCount_AtXY,     &
                                    iNodeIndex
    REAL(8)                      :: X,Y,DummyArray(3)
    CHARACTER                    :: ALine*1000,ALinePerm*1000,cErrorMsg*300,cHydDescriptor*11,cHydDescriptorCapital*11
    INTEGER,ALLOCATABLE          :: Nodes(:)
    REAL(8),ALLOCATABLE          :: Coeff(:)
    TYPE(HydAtNodeType)          :: aNodeHyd
    TYPE(HydAtXYType)            :: aXYHyd
    TYPE(HydListType)            :: HydList
    CLASS(*),POINTER             :: pCurrent
    
    !Initialize
    iStat = 0
    SELECT CASE (iHydFor)
        CASE (f_iHyd_GWHead)
            cHydDescriptor        = 'groundwater'
            cHydDescriptorCapital = 'Groundwater'
        CASE (f_iHyd_Subsidence)
            cHydDescriptor        = 'subsidence'
            cHydDescriptorCapital = 'Subsidence'
    END SELECT
        
    !Allocate hydrograph pointers
    ALLOCATE (OrderedHydList(NHyd) ,STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for ordered '//TRIM(cHydDescriptor)//' hydrograph list!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read the specific hydrograph information and process
    NHyd_AtXY   = 0
    NHyd_AtNode = 0
    DO indx=1,NHyd
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)  
        ALine     = ADJUSTL(ALine)
        ALinePerm = ALine
        !Read the first 3 entries
        DO indx1=1,3
            READ (ALine,*,IOSTAT=ErrorCode) DummyArray(indx1)
            IF (ErrorCode .NE. 0) THEN
                MessageArray(1) = 'Error in data entry for '//TRIM(cHydDescriptor)//' hydrograph specification '//TRIM(IntToText(indx))//'!'
                MessageArray(2) = 'Model is trying to read the following data line:'
                MessageArray(3) = TRIM(ADJUSTL(ALinePerm))
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (indx1 .EQ. 1) ID = DummyArray(indx1)
            iLoc = FirstLocation(' ',ALine)
            IF (iLoc .EQ. 0) THEN
                CALL SetLastMessage('Error in data entry for '//TRIM(cHydDescriptor)//' hydrograph specification '//TRIM(IntToText(ID))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            ALine = ADJUSTL(ALine(iLoc:LEN_TRIM(ALine)))
        END DO
        iHydType  = DummyArray(2)
        iHydLayer = DummyArray(3)
        
        !Make sure hydrograph type is recognized
        IF (.NOT.(iHydType .EQ. f_iHyd_AtXY   .OR.   iHydType .EQ. f_iHyd_AtNode)) THEN
            CALL SetLastMessage(TRIM(cHydDescriptorCapital)//' hydrograph type listed for hydrograph ID '//TRIM(IntToText(ID))//' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF

        !Make sure layer is modeled
        IF (iHydLayer .LT. 0   .OR.   iHydLayer .GT. Stratigraphy%NLayers) THEN
            CALL SetLastMessage(TRIM(cHydDescriptorCapital)//' hydrograph layer listed for hydrograph ID '//TRIM(IntToText(ID))//' is outside model bounds!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Based on hydrograph type, read the rest of the entries
        IF (iHydType .EQ. f_iHyd_AtXY) THEN
            DO indx1=1,2
                READ (ALine,*,IOSTAT=ErrorCode) DummyArray(indx1)
                IF (ErrorCode .NE. 0) THEN
                    MessageArray(1) = 'Error in data entry for '//TRIM(cHydDescriptor)//' hydrograph specification ID '//TRIM(IntToText(ID))//'!'
                    MessageArray(2) = 'Model is trying to read the following data line:'
                    MessageArray(3) = TRIM(ADJUSTL(ALinePerm))
                    CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                iLoc = FirstLocation(' ',ALine)
                IF (iLoc .EQ. 0) THEN
                    CALL SetLastMessage('Error in data entry for '//TRIM(cHydDescriptor)//' hydrograph specification ID '//TRIM(IntToText(ID))//'!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                ALine = ADJUSTL(ALine(iLoc:LEN_TRIM(ALine)))
            END DO
            X = DummyArray(1) * FactXY
            Y = DummyArray(2) * FactXY
        ELSE
            READ (ALine,*,IOSTAT=ErrorCode) iHydNode
            IF (ErrorCode .NE. 0) THEN
                MessageArray(1) = 'Error in data entry for '//TRIM(cHydDescriptor)//' hydrograph specification ID '//TRIM(IntToText(ID))//'!'
                MessageArray(2) = 'Model is trying to read the following data line:'
                MessageArray(3) = TRIM(ADJUSTL(ALinePerm))
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            CALL ConvertID_To_Index(iHydNode,iGWNodeIDs,iNodeIndex)
            IF (iNodeIndex .EQ. 0) THEN
                CALL SetLastMessage('Node number listed for '//TRIM(cHydDescriptor)//' hydrograph specification ID '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            iLoc = FirstLocation(' ',ALine)
            IF (iLoc .EQ. 0) THEN
                CALL SetLastMessage('Error in data entry for '//TRIM(cHydDescriptor)//' hydrograph specification ID '//TRIM(IntToText(ID))//'!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            ALine = ADJUSTL(ALine(iLoc:LEN_TRIM(ALine)))
        END IF    
        
        !Process hydrograph based on type
        SELECT CASE (iHydType)
            CASE (f_iHyd_AtXY)
                CALL AppGrid%FEInterpolate(X,Y,iElem,Nodes,Coeff)   
                IF (iElem .EQ. 0) THEN
                    CALL SetLastMessage(TRIM(cHydDescriptorCapital)//' hydrograph ID '//TRIM(IntToText(ID))//' is not in the model domain!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                IF (iHydLayer .NE. 0) THEN
                    IF (ALL(Stratigraphy%ActiveNode(Nodes,iHydLayer) .EQ. .FALSE.))  &
                        CALL LogMessage(TRIM(cHydDescriptorCapital)//' hydrograph ID '//TRIM(IntToText(ID))//' is located in an inactive layer!',f_iInfo,ThisProcedure)
                ELSE
                    IF (ALL(Stratigraphy%ActiveNode(Nodes,:) .EQ. .FALSE.))  &
                        CALL LogMessage(TRIM(cHydDescriptorCapital)//' hydrograph ID '//TRIM(IntToText(ID))//' is located in an inactive layer!',f_iInfo,ThisProcedure)
                END IF
                iSize           = SIZE(Nodes)
                aXYHyd%cName    = ALine
                aXyHyd%ID       = ID
                aXYHyd%X        = X
                aXYHyd%Y        = Y
                aXYHyd%iLayer   = iHydLayer
                aXYHyd%iElement = iElem
                DEALLOCATE (aXYHyd%iNodes , aXYHyd%rFactors , STAT=ErrorCode) 
                ALLOCATE (aXYHyd%iNodes(iSize) , aXYHyd%rFactors(iSize))
                aXYHyd%iNodes   = Nodes  
                aXYHyd%rFactors = Coeff  
                NHyd_AtXY       = NHyd_AtXY + 1
                CALL HydList%AddNode(aXYHyd,iStat)
                IF (iStat .EQ. -1) RETURN
            CASE (f_iHyd_AtNode)
                IF (iHydLayer .NE. 0) THEN
                    IF (.NOT. Stratigraphy%ActiveNode(iNodeIndex,iHydLayer))  &
                        CALL LogMessage('Groundwater node '//TRIM(IntToText(iHydNode))//' at layer '//TRIM(IntToText(iHydLayer))//' for '//TRIM(cHydDescriptor)//' hydrograph ID '//TRIM(IntToText(ID))//' is inactive!',f_iInfo,ThisProcedure)
                ELSE
                    IF (ALL(Stratigraphy%ActiveNode(iNodeIndex,:) .EQ. .FALSE.))  &
                        CALL LogMessage('There are no active layers at node '//TRIM(IntToText(iHydNode))//' for '//TRIM(cHydDescriptor)//' hydrograph ID '//TRIM(IntToText(ID))//'!',f_iInfo,ThisProcedure)
                END IF
                aNodeHyd%cName    = ALine
                ANodeHyd%ID       = ID
                aNodeHyd%iLayer   = iHydLayer
                aNodeHyd%iElement = AppGrid%GetElementGivenVertex(iNodeIndex)
                aNodeHyd%iNode    = iNodeIndex
                NHyd_AtNode       = NHyd_AtNode + 1
                CALL HydList%AddNode(aNodeHyd,iStat)
                IF (iStat .EQ. -1) RETURN
        END SELECT
    END DO

    !Store hydrograph data in persistent variables
    ALLOCATE (Hyd_AtNode(NHyd_AtNode) , Hyd_AtXY(NHyd_AtXY) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for '//TRIM(cHydDescriptor)//' hydrograph data!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL HydList%Reset()
    iCount_AtNode = 0
    iCount_AtXY   = 0
    DO indx=1,NHyd
        pCurrent => HydList%GetCurrentValue()
        SELECT TYPE (pCurrent)
            TYPE IS (HydAtNodeType)
                iCount_AtNode                 = iCount_AtNode + 1
                Hyd_AtNode(iCount_AtNode)     = pCurrent
                OrderedHydList(indx)%iHydType = f_iHyd_AtNode
                OrderedHydList(indx)%indx     = iCount_AtNode
            TYPE IS (HydAtXYType)
                iCount_AtXY                   = iCount_AtXY + 1
                Hyd_AtXY(iCount_AtXY)         = pCurrent
                OrderedHydList(indx)%iHydType = f_iHyd_AtXY
                OrderedHydList(indx)%indx     = iCount_AtXY
        END SELECT
        CALL HydList%Next()    
    END DO
    
    !Clear memory
    CALL HydList%Delete()
  
  END SUBROUTINE HydrographList_New
  
  
  
  
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
  ! --- KILL HYDROGRAPH DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(HydOutput)
    CLASS(HydOutputType) :: HydOutput
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Clear allocated arrays
    DEALLOCATE(HydOutput%OrderedHydList , HydOutput%Hyd_AtNode , HydOutput%Hyd_AtXY , STAT=ErrorCode)
    HydOutput%NHyd_AtNode = 0
    HydOutput%NHyd_AtXY   = 0
    
    !Close output/input file
    IF (ALLOCATED(HydOutput%OutFile)) THEN
        CALL HydOutput%OutFile%Kill()
        DEALLOCATE (HydOutput%OutFile , STAT=ErrorCode)
    END IF
    IF (ALLOCATED(HydOutput%InFile_ForInquiry)) THEN
        CALL HydOutput%InFile_ForInquiry%Close()
        DEALLOCATE (HydOutput%InFile_ForInquiry , STAT=ErrorCode)
    END IF
    
  END SUBROUTINE Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF HYDROGRAPH OUTPUT IS SPECIFIED
  ! -------------------------------------------------------------
  FUNCTION IsDefined(HydOutput) RESULT(lDefined)
    CLASS(HydOutputType),INTENT(IN) :: HydOutput
    LOGICAL                         :: lDefined
    
    lDefined = .FALSE.
    IF (ALLOCATED(HydOutput%OutFile)) THEN
        IF (HydOutput%OutFile%iGetFIleType() .NE. f_iUNKNOWN) lDefined = .TRUE.
    ELSEIF (ALLOCATED(HydOutput%InFile_ForInquiry)) THEN
        IF (HydOutput%InFile_ForInquiry%File%iGetFileType() .NE. f_iUNKNOWN) lDefined = .TRUE.
    END IF
    
  END FUNCTION IsDefined
  
  
  
  
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
  ! --- GET FILENAME
  ! -------------------------------------------------------------
  SUBROUTINE GetFileName(HydOutput,cName)
    CLASS(HydOutputType),INTENT(IN)      :: HydOutput
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cName
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cName , STAT=ErrorCode)
    
    IF (ALLOCATED(HydOutput%OutFile)) THEN
        CALL HydOutput%OutFile%GetName(cName)
    ELSEIF (ALLOCATED(HydOutput%InFile_ForInquiry)) THEN
        CALL HydOutput%InFile_ForInquiry%GetFileName(cName)
    END IF
    
  END SUBROUTINE GetFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(HydOutput) RESULT(NHydrographs)
    CLASS(HydOutputType),INTENT(IN) :: HydOutput
    INTEGER                         :: NHydrographs
    
    NHydrographs = HydOutput%NHyd_AtNode + HydOutput%NHyd_AtXY
    
  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(HydOutput,IDs)
    CLASS(HydOutputType),INTENT(IN) :: HydOutput
    INTEGER,INTENT(OUT)             :: IDs(:)
    
    !Local variables
    INTEGER :: indxHyd,indx
    
    DO indxHyd=1,SIZE(HydOutput%OrderedHydList)
        SELECT CASE (HydOutput%OrderedHydList(indxHyd)%iHydType)
            CASE (f_iHyd_AtXY)
                indx         = HydOutput%OrderedHydList(indxHyd)%indx
                IDs(indxHyd) = HydOutput%Hyd_AtXY(indx)%ID
                
            CASE (f_iHyd_AtNode)
                indx         = HydOutput%OrderedHydList(indxHyd)%indx
                IDs(indxHyd) = HydOutput%Hyd_AtNode(indx)%ID
                
        END SELECT
    END DO
    
  END SUBROUTINE GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(HydOutput,GridX,GridY,XHyd,YHyd)
    CLASS(HydOutputType),INTENT(IN) :: HydOutput
    REAL(8),INTENT(IN)              :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)             :: XHyd(:),YHyd(:)
    
    !Local variables
    INTEGER :: indxHyd,indx,iNode
    
    DO indxHyd=1,SIZE(HydOutput%OrderedHydList)
        SELECT CASE (HydOutput%OrderedHydList(indxHyd)%iHydType)
            CASE (f_iHyd_AtXY)
                indx          = HydOutput%OrderedHydList(indxHyd)%indx
                XHyd(indxHyd) = HydOutput%Hyd_AtXY(indx)%X
                YHyd(indxHyd) = HydOutput%Hyd_AtXY(indx)%Y
                
            CASE (f_iHyd_AtNode)
                indx       = HydOutput%OrderedHydList(indxHyd)%indx
                iNode      = HydOutput%Hyd_AtNode(indx)%iNode
                XHyd(indxHyd) = GridX(iNode)
                YHyd(indxHyd) = GridY(iNode)
                
        END SELECT
    END DO
    
  END SUBROUTINE GetHydrographCoordinates

  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographNames(HydOutput,cNamesList)
    CLASS(HydOutputType),INTENT(IN) :: HydOutput
    CHARACTER(LEN=*),INTENT(OUT)    :: cNamesList(:)  !Assumes array is previously dimensioned based on the number of hydrographs
    
    !Local variables
    INTEGER :: indxHyd,indx
    
    DO indxHyd=1,SIZE(HydOutput%OrderedHydList)
        SELECT CASE (HydOutput%OrderedHydList(indxHyd)%iHydType)
            CASE (f_iHyd_AtXY)
                indx                = HydOutput%OrderedHydList(indxHyd)%indx
                cNamesList(indxHyd) = HydOutput%Hyd_AtXY(indx)%cName
                
            CASE (f_iHyd_AtNode)
                indx                = HydOutput%OrderedHydList(indxHyd)%indx
                cNamesList(indxHyd) = HydOutput%Hyd_AtNode(indx)%cName
                
        END SELECT
    END DO
    
  END SUBROUTINE GetHydrographNames
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ SIMULATED GW HEADS OR SUBSIDENCE AT A HYDROGRAPH LOCATION
  ! --- Note: Assumes InFile_ForInquiry file exists
  ! -------------------------------------------------------------
  SUBROUTINE ReadHydrograph_AtLocation(HydOutput,iHydID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputFactor,rConversionFactor,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(HydOutputType)        :: HydOutput
    INTEGER,INTENT(IN)          :: iHydID
    CHARACTER(LEN=*),INTENT(IN) :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)          :: rOutputFactor,rConversionFactor     !rOutputFactor is the conversion factor when the heads were being printed out during simulation; rConversionFactor is the factor to convert the heads in the simulation units to the output units for post-processing
    INTEGER,INTENT(OUT)         :: nActualOutput,iStat
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
       
    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'ReadHydrograph_AtLocation'
    INTEGER                                :: FileReadCode,nHydTotal,iHydIndex,indx,iHydType,iPointer
    REAL(8)                                :: rEffectiveFactor
    
    !Initialize
    iStat = 0
    
    !Total number of hydrographs
    nHydTotal = HydOutput%NHyd_AtNode + HydOutput%NHyd_AtXY
    
    !Convert hydrograph ID to index
    iHydIndex = 0
    DO indx=1,nHydTotal
        iHydType = HydOutput%OrderedHydList(indx)%iHydType
        iPointer = HydOutput%OrderedHydList(indx)%indx
        SELECT CASE (iHydType)
            CASE (f_iHyd_AtXY)
                IF (iHydID .EQ. HydOutput%Hyd_AtXY(iPointer)%ID) THEN
                    iHydIndex = indx
                    EXIT
                END IF
                
            CASE (f_iHyd_AtNode)
                IF (iHydID .EQ. HydOutput%Hyd_AtNode(iPointer)%ID) THEN
                    iHydIndex = indx
                    EXIT
                END IF
        END SELECT
    END DO
    IF (iHydIndex .EQ. 0) THEN
        CALL SetLastMessage('Groundwater hydrograph ID '//TRIM(IntToText(iHydID))//' is not in the model!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read data
    CALL HydOutput%InFile_ForInquiry%ReadData(iHydIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,FileReadCode,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Convert unit
    rEffectiveFactor = rConversionFactor / rOutputFactor
    IF (rEffectiveFactor .NE. 1d0) rOutputValues = rOutputValues * rEffectiveFactor
    
    !Rewind file
    CALL HydOutput%InFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
  END SUBROUTINE ReadHydrograph_AtLocation

  
  
  
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
  ! --- PRINT-OUT HYDROGRAPHS AT USER-SPECIFIED LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(HydOutput,Stratigraphy,iHydFor,rValues,rFactor,TimeStep,lEndOfSimulation)
    CLASS(HydOutputType)              :: HydOutput
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: iHydFor
    REAL(8),INTENT(IN)                :: rValues(:,:),rFactor
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    LOGICAL,INTENT(IN)                :: lEndOfSimulation
    
    !Local variables
    INTEGER   :: indxHyd,indx,iHydLayer,iHydNode,NLayers,indxLayer,iCount,iNodes(4),nVertex,indxVertex,iNode,NHyd
    REAL(8)   :: DummyArray(HydOutput%NHyd_AtNode+HydOutput%NHyd_AtXY),rFactors(4),HXY(Stratigraphy%NLayers)
    CHARACTER :: SimulationTime*21
    
    !Initialize
    NLayers    = Stratigraphy%NLayers
    NHyd       = HydOutput%NHyd_AtNode + HydOutput%NHyd_AtXY
    DummyArray = 0.0
    
    !Compile hydrographs
    DO indxHyd=1,NHyd
        indx = HydOutput%OrderedHydList(indxHyd)%indx
        SELECT CASE (HydOutput%OrderedHydList(indxHyd)%iHydType)
            CASE (f_iHyd_AtNode)
                iHydLayer = HydOutput%Hyd_AtNode(indx)%iLayer
                iHydNode  = HydOutput%Hyd_AtNode(indx)%iNode
                IF (iHydLayer .EQ. 0) THEN
                    SELECT CASE (iHydFor)
                        !Hydrograph is for average groundwater head over layers
                        CASE (f_iHyd_GWHead)
                            DummyArray(indxHyd) = SUM(rValues(iHydNode,:) , MASK=Stratigraphy%ActiveNode(iHydNode,:)) * rFactor / REAL(COUNT(Stratigraphy%ActiveNode(iHydNode,:)),8)
                        
                        !Hydrograph is for total subsidence over layers    
                        CASE (f_iHyd_Subsidence)
                            DummyArray(indxHyd) = SUM(rValues(iHydNode,:)) * rFactor
                            
                    END SELECT
                ELSE
                    !Hydrograph is for a specific layer
                    DummyArray(indxHyd) = rValues(iHydNode,iHydLayer) * rFactor
                END IF
                
            CASE (f_iHyd_AtXY)
                iHydLayer           = HydOutput%Hyd_AtXY(indx)%iLayer
                nVertex             = SIZE(HydOutput%Hyd_AtXY(indx)%iNodes)
                iNodes(1:nVertex)   = HydOutput%Hyd_AtXY(indx)%iNodes
                rFactors(1:nVertex) = HydOutput%Hyd_AtXY(indx)%rFactors
                IF (iHydLayer .EQ. 0) THEN
                    HXY    = 0.0
                    iCount = 0
                    DO indxLayer=1,NLayers
                        IF (ANY(Stratigraphy%ActiveNode(iNodes,indxLayer))) iCount = iCount + 1
                        DO indxVertex=1,nVertex
                            iNode          = iNodes(indxVertex)
                            HXY(indxLayer) = HXY(indxLayer) + rValues(iNode,indxLayer) * rFactors(indxVertex)
                        END DO
                    END DO
                    SELECT CASE (iHydFor)
                        !Hydrograph is for average groundwater heads over layers
                        CASE (f_iHyd_GWHead)
                            DummyArray(indxHyd) = SUM(HXY) * rFactor / REAL(iCount,8)
                            
                        !Hydrograph is for total subsidence over layers    
                        CASE (f_iHyd_Subsidence)
                            DummyArray(indxHyd) = SUM(HXY) * rFactor
                    END SELECT
                ELSE
                    !Hydrograph is for a specific layer
                    DO indxVertex=1,nVertex
                        iNode               = iNodes(indxVertex)
                        DummyArray(indxHyd) = DummyArray(indxHyd) + rValues(iNode,iHydLayer) * rFactors(indxVertex)
                    END DO
                    DummyArray(indxHYd) = DummyArray(indxHyd) * rFactor
                END IF

        END SELECT
    END DO
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print out the results
    CALL HydOutput%OutFile%WriteData(SimulationTime,DummyArray,FinalPrint=lEndOfSimulation)
    
  END SUBROUTINE PrintResults

  
  
  
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
  ! --- PREPARE HYDROGRAPH OUTPUT FILE FOR PRINT-OUT
  ! -------------------------------------------------------------
  SUBROUTINE PrepHydOutFile(cFileName,NodeIDs,ElementIDs,iHydFor,UNITLTOU,CPartIn,Hyd_AtNode,Hyd_AtXY,OrderedHydList,TimeStep,OutFile,iStat)
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName,UNITLTOU,cPartIn
    INTEGER,INTENT(IN)              :: NodeIDs(:),ElementIDs(:),iHydFor
    TYPE(HydAtNodeType),INTENT(IN)  :: Hyd_AtNode(:)
    TYPE(HydAtXYType),INTENT(IN)    :: Hyd_AtXY(:)
    TYPE(OrderedHydType),INTENT(IN) :: OrderedHydList(:)
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    TYPE(GenericFileType)           :: OutFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'PrepHydOutFile'
    INTEGER                      :: NHyd,indx,indx1,Layers(SIZE(OrderedHydList)),IDs(SIZE(OrderedHydList)), &
                                    GWNodes(SIZE(OrderedHydList)),Elements(SIZE(OrderedHydList))
    CHARACTER                    :: Text*20,cFormatSpec*500,TitleLines(1)*3000,WorkArray(2)*3000,           &
                                    Header(5,1+SIZE(OrderedHydList))*50,HeaderFormat(5)*500,                &
                                    DataUnit(1)*10,DataType(1)*10,CPart(1)*32,FPart(1)*32,                  &
                                    cHydDescriptor*11
    
    !Initialize
    iStat      = 0
    NHyd       = SIZE(OrderedHydList)
    Text       = IntToText(NHyd)
    Header     = ''
    TitleLines = ''
    SELECT CASE (iHydFor)
        CASE (f_iHyd_GWHead)
            cHydDescriptor = 'groundwater'
        CASE (f_iHyd_Subsidence)
            cHydDescriptor = 'subsidence'
    END SELECT
    
    !Open file
    CALL OutFile%New(FileName=cFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor=TRIM(cHydDescriptor)//' hydrograph output',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that DSS file is used only if it is a time tracking simulation
    IF (OutFile%iGetFileType() .EQ. f_iDSS) THEN
        IF (.NOT. TimeStep%TrackTime) THEN
            CALL SetLastMessage('DSS files for '//TRIM(cHydDescriptor)//' hydrograph printing can only be used for time-tracking simulations.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Compile node, layer and element info
    DO indx=1,NHyd
      indx1 = OrderedHydList(indx)%indx
      SELECT CASE (OrderedHydList(indx)%iHydType)
          CASE (f_iHyd_AtNode)
              IDs(indx)      = Hyd_AtNode(indx1)%ID
              Layers(indx)   = Hyd_AtNode(indx1)%iLayer 
              Elements(indx) = ElementIDs(Hyd_AtNode(indx1)%iElement)
              GWNodes(indx)  = NodeIDs(Hyd_AtNode(indx1)%iNode)
          CASE (f_iHyd_AtXY)
              IDs(indx)      = Hyd_AtXY(indx1)%ID
              Layers(indx)   = Hyd_AtXY(indx1)%iLayer 
              Elements(indx) = ElementIDs(Hyd_AtXY(indx1)%iElement)
              GWNodes(indx)  = 0
      END SELECT
    END DO

    !Prepare header lines for ASCII file
    cFormatSpec  = '(A21,'//TRIM(Text)//'(2X,F10.4))'
    WorkArray(1) = ArrangeText(UpperCase(TRIM(cHydDescriptor))//' HYDROGRAPH',37)
    WorkArray(2) = ArrangeText('(UNIT=',UNITLTOU,')',37)
    CALL PrepareTitle(TitleLines(1),WorkArray(1:2),39,42)
    WRITE (Header(1,1),'(A1,10X,A13)') '*','HYDROGRAPH ID'
    WRITE (Header(2,1),'(A1,18X,A5)') '*','LAYER'
    WRITE (Header(3,1),'(A1,19X,A4)') '*','NODE'
    WRITE (Header(4,1),'(A1,16X,A7)') '*','ELEMENT'
    DO indx=1,NHyd
      WRITE (Header(1,indx+1),'(I7)') IDs(indx)
      WRITE (Header(2,indx+1),'(I7)') Layers(indx)
      WRITE (Header(3,indx+1),'(I7)') GWNodes(indx)
      WRITE (Header(4,indx+1),'(I7)') Elements(indx)
    END DO
    WRITE (Header(5,1),'(A1,8X,A4)') '*','TIME'
    HeaderFormat(1) = '(A24,2X,'//TRIM(Text)//'(A7,5X))'
    HeaderFormat(2) = '(A24,2X,'//TRIM(Text)//'(A7,5X))'
    HeaderFormat(3) = '(A24,2X,'//TRIM(Text)//'(A7,5X))'
    HeaderFormat(4) = '(A24,2X,'//TRIM(Text)//'(A7,5X))'
    HeaderFormat(5) = '(A13,'//TRIM(Text)//'(A))'
    
    !DSS file output data
    DataUnit(1) = UNITLTOU
    DataType(1) = 'INST-VAL'
    CPart(1)    = ADJUSTL(UpperCase(CPartIn))
    FPart(1)    = UpperCase(TRIM(cHydDescriptor)) // '_HYDROGRAPHS'

    !Prepare the file for print-out    
    CALL PrepareTSDOutputFile(OutFile                                 , &
                              NColumnsOfData          = NHyd          , &
                              NRowsOfData             = 1             , &
                              OverwriteNColumnsOfData = .TRUE.        , &
                              FormatSpec              = cFormatSpec   , &
                              Title                   = TitleLines    , &
                              Header                  = Header        , &
                              HeaderFormat            = HeaderFormat  , &
                              PrintColumnNo           = .FALSE.       , &
                              DataUnit                = DataUnit      , &
                              DataType                = DataType      , &
                              CPart                   = CPart         , &
                              FPart                   = FPart         , &
                              UnitT                   = TimeStep%Unit , &
                              IDs                     = IDs           , &
                              Layers                  = Layers        , &
                              GWNodes                 = GWNodes       , &
                              Elements                = Elements      , &
                              iStat                   = iStat         )
    
  END SUBROUTINE PrepHydOutFile
  
  
  
  
  ! -------------------------------------------------------------
  ! --- NEW FILE FOR HEADS AT USER-DEFINED LOCATIONS OPENED FOR INPUT FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE PrepHydInFile_ForInquiry(cFileName,NodeIDs,ElementIDs,iHydFor,CPart,Hyd_AtNode,Hyd_AtXY,OrderedHydList,TimeStep,InFile,iStat)
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName
    INTEGER,INTENT(IN)              :: NodeIDs(:),ElementIDs(:),iHydFor
    CHARACTER(LEN=*),INTENT(IN)     :: CPart
    TYPE(HydAtNodeType),INTENT(IN)  :: Hyd_AtNode(:)
    TYPE(HydAtXYType),INTENT(IN)    :: Hyd_AtXY(:)
    TYPE(OrderedHydType),INTENT(IN) :: OrderedHydList(:)
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    TYPE(RealTSDataInFileType)      :: InFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24),PARAMETER :: ThisProcedure = ModName // 'PrepHydInFile_ForInquiry'
    INTEGER                                :: indx,indx1,NHydrographs,Layers(SIZE(OrderedHydList)),         &
                                              GWNodes(SIZE(OrderedHydList)),Elements(SIZE(OrderedHydList)), &
                                              IDs(SIZE(OrderedHydList))
    CHARACTER                              :: cPathNames(SIZE(OrderedHydList))*80,cHydDescriptor*11
    CHARACTER(LEN=32)                      :: BPart,EPart,FPart
    
    !Initialize
    iStat        = 0
    NHydrographs = SIZE(OrderedHydList)
    
    !Type of hydrograph
    SELECT CASE (iHydFor)
        CASE (f_iHyd_GWHead)
            cHydDescriptor = 'groundwater'
        CASE (f_iHyd_Subsidence)
            cHydDescriptor = 'subsidence'
    END SELECT
        
    !Compile node, layer and element info
    DO indx=1,NHydrographs
      indx1 = OrderedHydList(indx)%indx
      SELECT CASE (OrderedHydList(indx)%iHydType)
      CASE (f_iHyd_AtNode)
              IDs(indx)      = Hyd_AtNode(indx1)%ID
              Layers(indx)   = Hyd_AtNode(indx1)%iLayer 
              Elements(indx) = ElementIDs(Hyd_AtNode(indx1)%iElement)
              GWNodes(indx)  = NodeIDs(Hyd_AtNode(indx1)%iNode)
          CASE (f_iHyd_AtXY)
              IDs(indx)      = Hyd_AtXY(indx1)%ID
              Layers(indx)   = Hyd_AtXY(indx1)%iLayer 
              Elements(indx) = ElementIDs(Hyd_AtXY(indx1)%iElement)
              GWNodes(indx)  = 0
      END SELECT
    END DO

    !Pathname parts
    EPart = UpperCase(TRIM(TimeStep%Unit))
    FPart = TRIM(UpperCase(cHydDescriptor)) // '_HYDROGRAPHS'
        
    !Instantiate file according to its type
    IF (iGetFileType_FromName(cFileName) .EQ. f_iDSS) THEN
        !Form pathnames
        DO indx=1,NHydrographs
            BPart            = 'ID' // TRIM(IntToText(IDs(indx))) // ':L' // TRIM(IntToText(Layers(indx))) // ':E' // TRIM(IntToText(Elements(indx))) // ':GW' // TRIM(IntToText(GWNodes(indx)))
            cPathnames(indx) = '/IWFM/' // TRIM(BPart) // '/' // TRIM(CPart) // '//' // TRIM(EPart) // '/' //TRIM(FPart) // '/'
        END DO
        CALL InFile%Init(cFileName,TRIM(cHydDescriptor)//' hydrograph at selected locations',TimeStep%TrackTime,nCol=NHydrographs,cPathNames=cPathNames,iStat=iStat)
    ELSE
        CALL InFile%Init(cFileName,TRIM(cHydDescriptor)//' hydrograph at selected locations',BlocksToSkip=0,nCol=NHydrographs,iStat=iStat)
    END IF
    
  END SUBROUTINE PrepHydInFile_ForInquiry
  
  
  ! -------------------------------------------------------------
  ! --- TRANSFER HYDROGRAPHS FROM TEXT/DSS FILE TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE Transfer_To_HDF(HydOut,cDescription,cDataSet,NTIME,TimeStep,rFact_LT,iStat)
    CLASS(HydOutputType)          :: HydOut
    CHARACTER(LEN=*),INTENT(IN)   :: cDescription,cDataSet
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    REAL(8),INTENT(IN)            :: rFact_LT
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER                  :: NColumns(1),FileReadCode,indxTime
    REAL(8)                  :: rGWHeads(HydOut%NHyd_AtNode+HydOut%NHyd_AtXY,1),rConvFactor
    CHARACTER                :: cDataSetName(1)*50,cHDFFileName*500
    TYPE(GenericFileType)    :: OutFile
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(TimeStepType)       :: TimeStep_Local
    
    !Get the name of the text/DSS file 
    CALL HydOut%InFile_ForInquiry%GetFileName(cFileName)
    
    !Name for the HDF file
    cHDFFileName = TRIM(ADJUSTL(StripTextUntilCharacter(cFileName,'.',Back=.TRUE.))) // '.hdf'
    
    !Open output file HDF file
    CALL OutFile%New(FileName=TRIM(cHDFFileName),InputFile=.FALSE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Create dataset; use NTIME+1 because this output includes initial conditions as well
    NColumns(1)     = HydOut%NHyd_AtNode + HydOut%NHyd_AtXY
    cDataSetName(1) = cDataSet
    CALL OutFile%CreateHDFDataSet(cPathNames=cDataSetName,NColumns=NColumns,NTime=NTIME+1,TimeStep=TimeStep,DataType=0d0,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Conversion factor used when printing out results
    rConvFactor = 1.0 / rFact_LT

    !Transfer heads to HDF file; use NTIME+1 because this output includes initial conditions as well
    TimeStep_Local = TimeStep
    DO indxTime=1,NTIME+1
        !Read data
        CALL ReadTSData(TimeStep_Local,cDescription,HydOut%InFile_ForInquiry,FileReadCode,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Transfer  values to matrix to be written to HDF file
        rGWHeads(:,1) = HydOut%InFile_ForInquiry%rValues
        
        !Convert unit back to simulation units
        IF (rConvFactor .NE. 1.0) rGWHeads = rGWHeads * rConvFactor
        
        !Store heads into HDF file
        CALL OutFile%WriteData(rGWHeads)
        
        !Advance time
        TimeStep_Local%CurrentTimeStep    = TimeStep_Local%CurrentTimeStep + 1
        TimeStep_Local%CurrentDateAndTime = IncrementTimeStamp(TimeStep_Local%CurrentDateAndTime,TimeStep_Local%DELTAT_InMinutes)
    END DO
    
    !Rewind input file
    CALL HydOut%InFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
    !Close HDF file
    CALL OutFile%Kill()
    
  END SUBROUTINE Transfer_To_HDF



END MODULE