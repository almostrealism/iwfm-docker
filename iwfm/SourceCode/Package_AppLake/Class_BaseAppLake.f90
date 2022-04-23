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
MODULE Class_BaseAppLake
   USE Class_Version               , ONLY: VersionType             , &
                                           ReadVersion
   USE MessageLogger               , ONLY: EchoProgress            , &
                                           SetLastMessage          , &
                                           MessageArray            , &
                                           iFatal
   USE GeneralUtilities            , ONLY: ArrangeText             , &
                                           UpperCase               , &
                                           IntToText               , &
                                           ShellSort               , &
                                           LocateInList            , &
                                           AllocArray
   USE GenericLinkedList           , ONLY: GenericLinkedListType
   USE TimeSeriesUtilities         , ONLY: TimeStepType            , &
                                           IncrementTimeStamp
   USE IOInterface                 , ONLY: GenericFileType
   USE Package_Misc                , ONLY: PairedDataType          , &
                                           iLakeComp               , &
                                           iLocationType_Lake      , &
                                           iAllLocationIDsListed
   USE Package_Budget              , ONLY: BudgetType              , &
                                           BudgetHeaderType        , &
                                           VolumeUnitMarker        , &
                                           AreaUnitMarker          , &
                                           LengthUnitMarker        , &
                                           LocationNameMarker      , &
                                           AreaMarker              , &
                                           VR                      , &
                                           VLE                     , &
                                           VLB                     , &
                                           LT                      , &
                                           PER_CUM                 , &
                                           PER_AVER
   USE Package_Discretization      , ONLY: AppGridType             , &
                                           StratigraphyType
   USE Package_ComponentConnectors , ONLY: StrmLakeConnectorType   , &
                                           LakeGWConnectorType     , &
                                           iStrmToLakeType         , &
                                           iBypassToLakeType
   USE Package_PrecipitationET     , ONLY: ETType                  , &
                                           PrecipitationType
   USE Package_Matrix              , ONLY: MatrixType
   USE Class_Lake                  , ONLY: LakeType
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
  PUBLIC :: BaseAppLakeType         , &
            PrepareLakeBudgetHeader , &
            GenerateRatingTable


  ! -------------------------------------------------------------
  ! --- BASE APPLICATION LAKES DATA TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: BaseAppLakeType
      TYPE(VersionType)                 :: Version
      INTEGER                           :: NLakes                 = 0
      TYPE(LakeType),ALLOCATABLE        :: Lakes(:)
      LOGICAL                           :: LakeBudRawFile_Defined = .FALSE.
      TYPE(BudgetType),ALLOCATABLE      :: LakeBudRawFile
      TYPE(GenericFileType),ALLOCATABLE :: FinalElevFile
      LOGICAL                           :: lFinalElevFile_Defined = .FALSE. 
  CONTAINS
      PROCEDURE(Abstract_SetStaticComponent),PASS,DEFERRED             :: SetStaticComponent
      PROCEDURE(Abstract_SetStaticComponentFromBinFile),PASS,DEFERRED  :: SetStaticComponentFromBinFile
      PROCEDURE(Abstract_SetDynamicComponent),PASS,DEFERRED            :: SetDynamicComponent
      PROCEDURE(Abstract_SetAllComponents),PASS,DEFERRED               :: SetAllComponents
      PROCEDURE(Abstract_SetAllComponentsWithoutBinFile),PASS,DEFERRED :: SetAllComponentsWithoutBinFile 
      PROCEDURE(Abstract_KillImplementation),PASS,DEFERRED             :: KillImplementation
      PROCEDURE(Abstract_GetVersion),PASS,DEFERRED                     :: GetVersion
      PROCEDURE(Abstract_Simulate),PASS,DEFERRED                       :: Simulate
      PROCEDURE(Abstract_ReadTSData),PASS,DEFERRED                     :: ReadTSData
      PROCEDURE(Abstract_CheckExternalTSDataPointers),PASS,DEFERRED    :: CheckExternalTSDataPointers
      PROCEDURE(Abstract_ConvertTimeUnit),PASS,DEFERRED                :: ConvertTimeUnit
      PROCEDURE,PASS                                                   :: Kill
      PROCEDURE,PASS                                                   :: GetNDataList_AtLocationType
      PROCEDURE,PASS                                                   :: GetDataList_AtLocationType
      PROCEDURE,PASS                                                   :: GetLocationsWithData
      PROCEDURE,PASS                                                   :: GetSubDataList_AtLocation
      PROCEDURE,PASS                                                   :: GetModelData_AtLocation
      PROCEDURE,PASS                                                   :: GetNLakes 
      PROCEDURE,PASS                                                   :: GetNames
      PROCEDURE,PASS                                                   :: GetNTotalLakeNodes          
      PROCEDURE,PASS                                                   :: GetNElementsInLake          
      PROCEDURE,PASS                                                   :: GetNodes                    
      PROCEDURE,PASS                                                   :: GetLakeElements             
      PROCEDURE,PASS                                                   :: GetAllLakeElements          
      PROCEDURE,NOPASS                                                 :: GetLakeElements_FromFile
      PROCEDURE,PASS                                                   :: GetElevs                    
      PROCEDURE,PASS                                                   :: GetMaxElevs                    
      PROCEDURE,PASS                                                   :: ReadRestartData
      PROCEDURE,PASS                                                   :: ReadPreprocessedData
      PROCEDURE,PASS                                                   :: PrintResults 
      PROCEDURE,PASS                                                   :: PrintRestartData
      PROCEDURE,PASS                                                   :: WritePreprocessedData       
      PROCEDURE,PASS                                                   :: RegisterWithMatrix
      PROCEDURE,PASS                                                   :: ResetElevations             
      PROCEDURE,PASS                                                   :: AdvanceState  
      PROCEDURE,PASS                                                   :: UpdateHeads 
      PROCEDURE,PASS                                                   :: ComputeLakeETa
      GENERIC                                                          :: New                            => SetStaticComponent             , &
                                                                                                            SetStaticComponentFromBinFile  , &
                                                                                                            SetDynamicComponent            , &
                                                                                                            SetAllComponents               , &
                                                                                                            SetAllComponentsWithoutBinFile
  END TYPE BaseAppLakeType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  CHARACTER(LEN=11),PARAMETER :: cDataList_AtLake = 'Lake budget'
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: NLakeBudColumns = 13
  CHARACTER(LEN=27),PARAMETER :: cBudgetColumnTitles(NLakeBudColumns) = ['Beginning Storage (+)'       , &
                                                                         'Ending Storage (-)'          , &
                                                                         'Flow from Upstream Lake (+)' , &
                                                                         'Flow from Streams (+)'       , &
                                                                         'Flow from Bypasses (+)'      , &
                                                                         'Runoff (+)'                  , &
                                                                         'Return Flow (+)'             , &
                                                                         'Precipitation (+)'           , &
                                                                         'Gain from Groundwater (+)'   , &
                                                                         'Lake Evaporation (-)'        , &
                                                                         'Lake Outflow (-)'            , &
                                                                         'Discrepancy (=)'             , &
                                                                         'Lake Surface Elevation'      ]
  

  ! -------------------------------------------------------------
  ! --- MISC. DATA 
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 19
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_BaseAppLake::'

   
  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE

    SUBROUTINE Abstract_SetStaticComponent(AppLake,cFileName,Stratigraphy,AppGrid,NStrmNodes,StrmLakeConnector,LakeGWConnector,iStat)
      IMPORT                                :: BaseAppLakeType,StratigraphyType,AppGridType,StrmLakeConnectorType,LakeGWConnectorType
      CLASS(BaseAppLakeType),INTENT(OUT)    :: AppLake
      CHARACTER(LEN=*),INTENT(IN)           :: cFileName
      TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
      TYPE(AppGridType),INTENT(IN)          :: AppGrid
      INTEGER,INTENT(IN)                    :: NStrmNodes
      TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
      TYPE(LakeGWConnectorType),INTENT(OUT) :: LakeGWConnector
      INTEGER,INTENT(OUT)                   :: iStat
    END SUBROUTINE Abstract_SetStaticComponent
    
    
    SUBROUTINE Abstract_SetStaticComponentFromBinFile(AppLake,BinFile,iStat)
      IMPORT                             :: BaseAppLakeType,GenericFileType
      CLASS(BaseAppLakeType),INTENT(OUT) :: AppLake
      TYPE(GenericFileType)              :: BinFile
      INTEGER,INTENT(OUT)                :: iStat
    END SUBROUTINE Abstract_SetStaticComponentFromBinFile
    
    
    SUBROUTINE Abstract_SetDynamicComponent(AppLake,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,LakeGWConnector,iStat)
      IMPORT                        :: BaseAppLakeType,TimeStepType,AppGridType,LakeGWConnectorType
      CLASS(BaseAppLakeType)        :: AppLake
      LOGICAL,INTENT(IN)            :: IsForInquiry
      CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
      TYPE(TimeStepType),INTENT(IN) :: TimeStep
      INTEGER,INTENT(IN)            :: NTIME
      TYPE(AppGridType),INTENT(IN)  :: AppGrid
      TYPE(LakeGWConnectorType)     :: LakeGWConnector
      INTEGER,INTENT(OUT)           :: iStat
    END SUBROUTINE Abstract_SetDynamicComponent
    
    
    SUBROUTINE Abstract_SetAllComponents(AppLake,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,BinFile,LakeGWConnector,iStat)
      IMPORT                             :: BaseAppLakeType,TimeStepType,AppGridType,GenericFileType,LakeGWConnectorType
      CLASS(BaseAppLakeType),INTENT(OUT) :: AppLake
      LOGICAL,INTENT(IN)                 :: IsForInquiry
      CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cSimWorkingDirectory
      TYPE(TimeStepType),INTENT(IN)      :: TimeStep
      INTEGER,INTENT(IN)                 :: NTIME
      TYPE(AppGridType),INTENT(IN)       :: AppGrid
      TYPE(GenericFileType)              :: BinFile
      TYPE(LakeGWConnectorType)          :: LakeGWConnector
      INTEGER,INTENT(OUT)                :: iStat
    END SUBROUTINE Abstract_SetAllComponents

    
    SUBROUTINE Abstract_SetAllComponentsWithoutBinFile(AppLake,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,NStrmNodes,StrmLakeConnector,LakeGWConnector,iStat)
      IMPORT                                :: BaseAppLakeType,AppGridType,StratigraphyType,TimeStepType,StrmLakeConnectorType,LakeGWConnectorType
      CLASS(BaseAppLakeType),INTENT(OUT)    :: AppLake
      LOGICAL,INTENT(IN)                    :: IsForInquiry
      CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSimFileName,cSimWorkingDirectory
      TYPE(AppGridType),INTENT(IN)          :: AppGrid
      TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
      TYPE(TimeStepType),INTENT(IN)         :: TimeStep
      INTEGER,INTENT(IN)                    :: NTIME,NStrmNodes
      TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
      TYPE(LakeGWConnectorType),INTENT(OUT) :: LakeGWConnector
      INTEGER,INTENT(OUT)                   :: iStat
    END SUBROUTINE Abstract_SetAllComponentsWithoutBinFile
    
    
    SUBROUTINE Abstract_KillImplementation(AppLake)
      IMPORT                 :: BaseAppLakeType
      CLASS(BaseAppLakeType) :: AppLake
    END SUBROUTINE Abstract_KillImplementation

    
     FUNCTION Abstract_GetVersion(AppLake) RESULT(cVrs)
        IMPORT                   :: BaseAppLakeType
        CLASS(BaseAppLakeType)   :: AppLake
        CHARACTER(:),ALLOCATABLE :: cVrs
     END FUNCTION Abstract_GetVersion
     
     
     FUNCTION Abstract_GetMaxElevs(AppLake) RESULT(MaxElevs)
       IMPORT                            :: BaseAppLakeType
       CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
       REAL(8)                           :: MaxElevs(AppLake%NLakes)
     END FUNCTION Abstract_GetMaxElevs
     
     
     SUBROUTINE Abstract_Simulate(AppLake,GSElevs,GWHeads,Runoff,ReturnFlow,LakeGWConnector,StrmLakeConnector,Matrix)
      IMPORT                               :: BaseAppLakeType,LakeGWConnectorType,StrmLakeConnectorType,MatrixType
      CLASS(BaseAppLakeType)               :: AppLake
      REAL(8),INTENT(IN)                   :: GSElevs(:),GWHeads(:,:),Runoff(:),ReturnFlow(:)
      TYPE(LakeGWConnectorType),INTENT(IN) :: LakeGWConnector
      TYPE(StrmLakeConnectorType)          :: StrmLakeConnector
      TYPE(MatrixType)                     :: Matrix
    END SUBROUTINE Abstract_Simulate
    
    
    SUBROUTINE Abstract_ReadTSData(AppLake,TimeStep,ET,Precip,iStat)
      IMPORT                             :: BaseAppLakeType,TimeStepType,ETType,PrecipitationType
      CLASS(BaseAppLakeType)             :: AppLake
      TYPE(TimeStepType),INTENT(IN)      :: TimeStep
      TYPE(ETType),INTENT(IN)            :: ET
      TYPE(PrecipitationType),INTENT(IN) :: Precip
      INTEGER,INTENT(OUT)                :: iStat
    END SUBROUTINE Abstract_ReadTSData
    
    
    SUBROUTINE Abstract_CheckExternalTSDataPointers(AppLake,Precip,ET,iStat)
      IMPORT                             :: BaseAppLakeType,PrecipitationType,ETType
      CLASS(BaseAppLakeType),INTENT(IN)  :: AppLake
      TYPE(PrecipitationType),INTENT(IN) :: Precip
      TYPE(ETType),INTENT(IN)            :: ET
      INTEGER,INTENT(OUT)                :: iStat
    END SUBROUTINE Abstract_CheckExternalTSDataPointers


    SUBROUTINE Abstract_ConvertTimeUnit(AppLake,NewUnit)
      IMPORT                      :: BaseAppLakeType
      CLASS(BaseAppLakeType)      :: AppLake
      CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    END SUBROUTINE Abstract_ConvertTimeUnit
    
  END INTERFACE
  



CONTAINS
    
    
    
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL BASE LAKE DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppLake)
    CLASS(BaseAppLakeType) :: AppLake
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Kill the child
    CALL AppLake%KillImplementation()
    
    !Deallocate array attributes
    DEALLOCATE (AppLake%Lakes , STAT=ErrorCode)
    
    !Close lake budget output file
    IF (AppLake%LakeBudRawFile_Defined) THEN
        CALL AppLake%LakeBudRawFile%Kill()
        DEALLOCATE (AppLake%LakeBudRawFile , STAT=ErrorCode)
    END IF
    
    !Close final elevations output file
    IF (AppLake%lFinalElevFile_Defined) THEN
        CALL AppLake%FinalElevFile%Kill()
        DEALLOCATE (AppLake%FinalElevFile , STAT=ErrorCode)
    END IF
    
    !Set attributes to their default values
    AppLake%NLakes                 = 0
    AppLake%LakeBudRawFile_Defined = .FALSE.
    AppLake%lFinalElevFile_Defined = .FALSE.
    
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
  ! --- GET MAX LAKE ELEVS
  ! -------------------------------------------------------------
  FUNCTION GetMaxElevs(AppLake) RESULT(MaxElevs)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    REAL(8)                           :: MaxElevs(AppLake%NLakes)
    
    MaxElevs = AppLake%Lakes%MaxElev
    
  END FUNCTION GetMaxElevs
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE ELEVS
  ! -------------------------------------------------------------
  SUBROUTINE GetElevs(AppLake,Elevs)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    REAL(8)                           :: Elevs(:)
    
    Elevs = AppLake%Lakes%Elev
    
  END SUBROUTINE GetElevs
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL LAKE ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE GetAllLakeElements(AppLake,iLakeElems)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,ALLOCATABLE               :: iLakeElems(:)
    
    !Local variables
    INTEGER :: NElems,ErrorCode,iCount,indxLake
    
    !Return if there are no lakes
    IF (AppLake%NLakes .EQ. 0) RETURN
    
    !Initialize
    iCount = 0
    DEALLOCATE (iLakeElems , STAT=ErrorCode)
    
    !Total number of lake elements
    NElems = SUM(AppLake%Lakes%NElements)
    
    !Allocate memory for array
    ALLOCATE (iLakeElems(NElems))
    
    !Compile element numbers
    DO indxLake=1,AppLake%NLakes
      iLakeElems(iCount+1:iCount+AppLake%Lakes(indxLake)%NElements) = AppLake%Lakes(indxLake)%Elements
      iCount                                                        = iCount + AppLake%Lakes(indxLake)%NElements
    END DO
    
    !Order them
    CALL ShellSort(iLakeElems)
    
  END SUBROUTINE GetAllLakeElements
  

  ! -------------------------------------------------------------
  ! --- GET ELEMENTS FOR A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetLakeElements(AppLake,iLake,Elements)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLake
    INTEGER,ALLOCATABLE,INTENT(OUT)   :: Elements(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (Elements , STAT=ErrorCode)
    
    ALLOCATE (Elements(AppLake%Lakes(iLake)%NElements))
    Elements = AppLake%Lakes(iLake)%Elements
    
  END SUBROUTINE GetLakeElements
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE ELEMENTS FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetLakeElements_FromFile(cFileName,iListElems,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    INTEGER,ALLOCATABLE         :: iListElems(:)
    INTEGER,INTENT(OUT)         :: iStat

    !Local data type
    TYPE,EXTENDS(GenericLinkedListType)  :: ElemListType
    END TYPE ElemListType
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'GetLakeElements_FromFile'
    INTEGER                      :: NLakes,indxLake,DummyArray(5),NElements,indxElem,iElem,indxElem1,ID
    CHARACTER(:),ALLOCATABLE     :: cVersion
    TYPE(GenericFileType)        :: InFile
    TYPE(ElemListType)           :: ElemList
    
    !Initialize
    iStat = 0
    
    !Return if filename is empty
    IF (cFileName .EQ. '') THEN
        ALLOCATE (iListElems(0))
        RETURN
    END IF
    
    !Open file
    CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='pre-processor lake data file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read the first line that holds version number
    CALL ReadVersion(InFile,'LAKE',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Number of lakes
    CALL InFile%ReadData(NLakes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NLakes .EQ. 0) RETURN
    
    !Read lake elements 
    DO indxLake=1,NLakes
        CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Lake ID
        ID = DummyArray(1)
        IF (ID .NE. indxLake) THEN
            MessageArray(1) = 'Lake data should be entered sequentially!'
            MessageArray(2) = 'Lake number expected = '//TRIM(IntToText(indxLake))
            MessageArray(3) = 'Lake number entered  = '//TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Lake elements
        NElements = DummyArray(4)
        CALL ElemList%AddNode(DummyArray(5),iStat)  ;  IF (iStat .EQ. -1) RETURN
        DO indxElem=2,NElements
            CALL InFile%ReadData(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL ElemList%AddNode(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
        END DO
                
    END DO
    
    !Retrieve the lake elements as a whole
    CALL ElemList%GetArray(iListElems,iStat)   ;  IF (iStat .EQ. -1) RETURN
    
    !Make sure lake elements are not listed more than once
    DO indxElem=1,SIZE(iListElems)-1
        iElem =  iListElems(indxElem)
        DO indxElem1=indxElem+1,SIZE(iListElems)
            IF (iElem .EQ. iListElems(indxElem1)) THEN
                CALL SetLastMessage('Element '//TRIM(IntToText(iElem))//' is listed more than once as a lake element!' ,iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END DO
    
    !Sort lake elements
    CALL ShellSort(iListElems)
   
    !Close lake data file
    CALL InFile%Kill()
    
    !Clear memory
    CALL ElemList%Delete()

  END SUBROUTINE GetLakeElements_FromFile
  

  ! -------------------------------------------------------------
  ! --- GET THE NODES OF A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetNodes(AppLake,iLakeNo,Nodes)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLakeNo                      
    INTEGER,ALLOCATABLE,INTENT(OUT)   :: Nodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (Nodes , STAT=ErrorCode)
    ALLOCATE (Nodes(AppLake%Lakes(iLakeNo)%NNodes))
    Nodes = AppLake%Lakes(iLakeNo)%Nodes
    
  END SUBROUTINE GetNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS IN A LAKE
  ! -------------------------------------------------------------
  FUNCTION GetNElementsInLake(AppLake,iLake) RESULT(NElems)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLake
    INTEGER                           :: NElems
    
    NElems = AppLake%Lakes(iLake)%NElements
    
  END FUNCTION GetNElementsInLake
  
  
  ! -------------------------------------------------------------
  ! --- GET THE TOTAL NUMBER OF UNIQUE LAKE NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNTotalLakeNodes(AppLake) RESULT(NNodes)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER                           :: NNodes
    
    NNodes = SUM(AppLake%Lakes%NNodes)
    
  END FUNCTION GetNTotalLakeNodes


  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF LAKES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNLakes(AppLake) RESULT(NLakes)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER                           :: NLakes
    
    NLakes = AppLake%NLakes
    
  END FUNCTION GetNLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE NAMES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNames(AppLake,cNamesList)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    CHARACTER(LEN=*),INTENT(OUT)      :: cNamesList(:)  !Assumes array is previously dimensioned with respect to number of lakes
    
    cNamesList = AppLake%Lakes%cName
    
  END SUBROUTINE GetNames
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF DATA TYPES FOR POST-PROCESSING AT A LAKE
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(AppLake)  RESULT(NData)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER                           :: NData
    
    !Is lake budget defined?
    IF (AppLake%LakeBudRawFile_Defined) THEN
        NData = 1
    ELSE
        NData = 0
    END IF
    
  END FUNCTION GetNDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF DATA TYPES FOR POST-PROCESSING AT A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType(AppLake,iLocationType,cDataList,cFileList,lBudgetType) 
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE      :: cDataList(:),cFileList(:)
    LOGICAL,ALLOCATABLE               :: lBudgetType(:)
    
    !Local variables
    INTEGER                  :: ErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (cDataList , cFileList , lBudgetType , STAT=ErrorCode)
    
    !If loaction type is not lake return
    IF (iLocationType .NE. iLocationType_Lake) RETURN
    
    !Is lake budget defined?
    IF (AppLake%LakeBudRawFile_Defined) THEN
        ALLOCATE (cDataList(1) , cFileList(1) , lBudgetType(1))
        cDataList   = cDataList_AtLake
        lBudgetType = .TRUE.
        CALL AppLake%LakeBudRawFile%GetFileName(cFileName)
        cFileList = ''
        cFileList = cFileName
    END IF
    
  END SUBROUTINE GetDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATIONS THAT HAS A DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetLocationsWithData(AppLake,iLocationType,cDataType,iLocations) 
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)       :: cDataType   !Not used since there is only one data type for lakes for post-processing
    INTEGER,ALLOCATABLE,INTENT(OUT)   :: iLocations(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Return if location type is not a lake
    IF (iLocationType .NE. iLocationType_Lake) THEN
        DEALLOCATE (iLocations , STAT=ErrorCode)
        RETURN
    END IF
    
    !Is lake budget defined?
    IF (AppLake%LakeBudRawFile_Defined) THEN
        ALLOCATE (iLocations(1))
        iLocations = iAllLocationIDsListed
    END IF
    
  END SUBROUTINE GetLocationsWithData
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF FILES WHERE DATA FOR POST-PROCESSING RESIDE
  ! -------------------------------------------------------------
  SUBROUTINE GetFileList_AtLocationType(AppLake,cFileList) 
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    CHARACTER(LEN=*),ALLOCATABLE      :: cFileList(:)
    
    !Local variables
    INTEGER                  :: ErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (cFileList , STAT=ErrorCode)
    
    !Is lake budget defined?
    IF (AppLake%LakeBudRawFile_Defined) THEN
        CALL AppLake%LakeBudRawFile%GetFileName(cFileName)
        ALLOCATE (cFileList(1))
        cFileList(1) = ''
        cFileList(1) = cFileName
    END IF
    
  END SUBROUTINE GetFileList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET SUB-COMPONENTS OF A DATA TYPE FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_AtLocation(AppLake,iLocationType,cDataType,cSubDataList) 
    CLASS(BaseAppLakeType),INTENT(IN)        :: AppLake
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    !Only lake budget has sub-data
    IF (iLocationType .EQ. iLocationType_Lake) THEN
        IF (TRIM(cDataType) .EQ. cDataList_AtLake) THEN
            IF (AppLake%LakeBudRawFile_Defined) THEN
                ALLOCATE (cSubDataList(NLakeBudColumns))
                cSubDataList = cBudgetColumnTitles
            END IF
        END IF
    END IF
    
  END SUBROUTINE GetSubDataList_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LAKE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation(AppLake,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(BaseAppLakeType)      :: AppLake
    INTEGER,INTENT(IN)          :: iLocationType,iLocationID,iCol
    CHARACTER(LEN=*),INTENT(IN) :: cDataType,cOutputBeginDateAndTime,cOutputEndDAteAndTime,cOutputInterval
    REAL(8),INTENT(IN)          :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)         :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    !Only lake budget data can be returned
    IF (.NOT. (iLocationType.EQ.iLocationType_Lake  .AND.  TRIM(cDataType).EQ.cDataList_AtLake)) RETURN
    
    !If lake budget output is not defined return
    IF (.NOT. AppLake%LakeBudRawFile_Defined) RETURN
    
    !Get the data for the specified budget column for the specified period with specified interval
    CALL AppLake%LakeBudRawFile%ReadData(iLocationID,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    
  END SUBROUTINE GetModelData_AtLocation
  
  

  
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
  ! --- READ RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(AppLake,InFile,iStat)
    CLASS(BaseAppLakeType) :: AppLake
    TYPE(GenericFileType)  :: InFile
    INTEGER,INTENT(OUT)    :: iStat
        
    CALL InFile%ReadData(AppLake%Lakes%Storage,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppLake%Lakes%Storage_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppLake%Lakes%Elev,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppLake%Lakes%Elev_P,iStat)     ;  IF (iStat .EQ. -1) RETURN

  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadPreprocessedData(AppLake,InFile,iStat)
    CLASS(BaseAppLakeType),INTENT(OUT) :: AppLake
    TYPE(GenericFileType)              :: InFile
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadPreprocessedData'
    INTEGER                      :: NLakes,ErrorCode,indxLake
    
    !Initailize
    iStat = 0
    
    !Read number of lakes modeled
    CALL InFile%ReadData(NLakes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AppLake%NLakes = NLakes
    
    !Allocate memory
    ALLOCATE (AppLake%Lakes(NLakes) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for application lakes!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Lake data
    DO indxLake=1,NLakes
      ASSOCIATE (pLake => AppLake%Lakes(indxLake))
        CALL InFile%ReadData(pLake%NElements,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(pLake%NNodes,iStat)     ;  IF (iStat .EQ. -1) RETURN
        ALLOCATE (pLake%Elements(pLake%NElements) , &
                  pLake%Nodes(pLake%NNodes)       , &
                  pLake%NodeAreas(pLake%NNodes)   , &
                  STAT=ErrorCode                  )
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for lake '//TRIM(IntToText(indxLake))//'!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL InFile%ReadData(pLake%Area,iStat)             ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(pLake%OutflowDestType,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(pLake%OutflowDest,iStat)      ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(pLake%Elements,iStat)         ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(pLake%Nodes,iStat)            ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(pLake%NodeAreas,iStat)        ;  IF (iStat .EQ. -1) RETURN
        CALL pLake%RatingTable%New(InFile,iStat)           ;  IF (iStat .EQ. -1) RETURN
      END ASSOCIATE
    END DO
    
  END SUBROUTINE ReadPreprocessedData
  
  
  
  
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
  ! --- WRITE PREPROCESSED APPLICATION LAKES DATA
  ! -------------------------------------------------------------
  SUBROUTINE WritePreprocessedData(AppLake,OutFile)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    TYPE(GenericFileType)             :: OutFile
    
    !Local variables
    INTEGER :: indxLake
    
    !Number of lakes
    CALL OutFile%WriteData(AppLake%NLakes)
    
    !Lake data
    DO indxLake=1,AppLake%NLakes
      ASSOCIATE (pLake => AppLake%Lakes(indxLake))
        CALL OutFile%WriteData(pLake%NElements)
        CALL OutFile%WriteData(pLake%NNodes)
        CALL OutFile%WriteData(pLake%Area)
        CALL OutFile%WriteData(pLake%OutflowDestType)
        CALL OutFile%WriteData(pLake%OutflowDest)
        CALL OutFile%WriteData(pLake%Elements)
        CALL OutFile%WriteData(pLake%Nodes)
        CALL OutFile%WriteData(pLake%NodeAreas)
        CALL pLake%RatingTable%WriteToFile(Outfile)
      END ASSOCIATE
    END DO
    
  END SUBROUTINE WritePreprocessedData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppLake,OutFile)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    TYPE(GenericFileType)             :: OutFile
    
    CALL OutFile%WriteData(AppLake%Lakes%Storage)
    CALL OutFile%WriteData(AppLake%Lakes%Storage_P)
    CALL OutFile%WriteData(AppLake%Lakes%Elev)
    CALL OutFile%WriteData(AppLake%Lakes%Elev_P)

  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT APPLICATION LAKES SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppLake,TimeStep,lEndOfSimulation,Runoff,ReturnFlow,LakeGWConnector,StrmLakeConnector)
    CLASS(BaseAppLakeType)                 :: AppLake
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    LOGICAL,INTENT(IN)                     :: lEndOfSimulation
    REAL(8),INTENT(IN)                     :: Runoff(:),ReturnFlow(:)
    TYPE(LakeGWConnectorType),INTENT(IN)   :: LakeGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN) :: StrmLakeConnector
    
    !Local variables
    INTEGER                           :: indxLake
    REAL(8)                           :: DummyArray(NLakeBudColumns,AppLake%NLakes)
    REAL(8),DIMENSION(AppLake%NLakes) :: Error,StrmInflows,LakeGWFlows,LakePrecip,BypassInflows
    
    !Return if raw budget output file is not defined
    IF (.NOT. AppLake%LakeBudRawFile_Defined) RETURN
    
    !Echo progress
    CALL EchoProgress('Printing results of lake simulation')

    !Initialize
    LakeGWFlows = LakeGWConnector%GetFlowAtLakes()
    LakePrecip  = AppLake%Lakes%PrecipRate * AppLake%Lakes%Area
    DO indxLake=1,AppLake%NLakes
      StrmInflows(indxLake)   = StrmLakeConnector%GetFlow(iStrmToLakeType,indxLake)   
      BypassInflows(indxLake) = StrmLakeConnector%GetFlow(iBypassToLakeType,indxLake)
    END DO
     
    ASSOCIATE (pLakes => AppLake%Lakes)
      Error =  pLakes%Storage_P                 &
             - pLakes%Storage                   &
             + pLakes%InFlowUplake              &
             + StrmInflows                      &
             + BypassInflows                    &
             + Runoff                           &
             + ReturnFlow                       &
             + LakePrecip                       &
             - LakeGWFlows                      &
             - pLakes%ETa                       &
             - pLakes%Outflow
      DummyArray(1,:)  = pLakes%Storage_P
      DummyArray(2,:)  = pLakes%Storage
      DummyArray(3,:)  = pLakes%InflowUpLake
      DummyArray(4,:)  = StrmInflows
      DummyArray(5,:)  = BypassInflows
      DummyArray(6,:)  = Runoff
      DummyArray(7,:)  = ReturnFlow      
      DummyArray(8,:)  = LakePrecip
      DummyArray(9,:)  = -LakeGWFlows
      DummyArray(10,:) = pLakes%ETa
      DummyArray(11,:) = pLakes%Outflow
      DummyArray(12,:) = Error
      DummyArray(13,:) = pLakes%Elev
    END ASSOCIATE
                 
    !Print out values to binary file
    CALL AppLake%LakeBudRawFile%WriteData(DummyArray)
    
    !If end-of-simulation, print final lake elevations
    IF (lEndOfSimulation) THEN
        IF (AppLake%lFinalElevFile_Defined) CALL PrintFinalElevs(AppLake%Lakes%Elev,TimeStep,AppLake%FinalElevFile)
    END IF

  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT END-OF-SIMULATION LAKE ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalElevs(Elevs,TimeStep,OutFile) 
    REAL(8),INTENT(IN)            :: Elevs(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER   :: indxLake
    CHARACTER :: SimulationTime*21,Text*500
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF
    
    !Print header
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C ***** LAKE ELEVATIONS AT '//TRIM(SimulationTime))
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C')    
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    CALL OutFile%WriteData('     1.0                           / FACT')
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    CALL OutFile%WriteData('C    ILAKE         HLAKE')
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    
    !Print final elevations
    DO indxLake=1,SIZE(Elevs)
        WRITE (Text,'(I8,100F16.6)') indxLake,Elevs(indxLake)
        CALL OutFile%WriteData(TRIM(Text))
    END DO
    
  END SUBROUTINE PrintFinalElevs
  

  
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
  ! --- ADD LAKE COMPONENT TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(AppLake,Matrix,iStat)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    TYPE(MatrixType)                  :: Matrix
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER :: indxLake,LakeNode(1)
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('Registering lake component with matrix...')
    
    !Add component to matrix
    CALL Matrix%AddComponent(iLakeComp,AppLake%NLakes,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Add connectivity
    DO indxLake=1,AppLake%NLakes
        LakeNode(1) = indxLake
        CALL Matrix%AddConnectivity(iLakeComp,indxLake,iLakeComp,LakeNode,iStat)
        IF (iStat .EQ. -1) RETURN
    END DO
       
  END SUBROUTINE RegisterWithMatrix

  
  ! -------------------------------------------------------------
  ! --- PREPARE HEADER FOR LAKE BUDGET RAW FILE
  ! -------------------------------------------------------------
  FUNCTION PrepareLakeBudgetHeader(Lakes,NTIME,TimeStep,cVersion) RESULT(Header)
    TYPE(LakeType),INTENT(IN)     :: Lakes(:)
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetHeaderType)        :: Header
   
    !Local variables
    INTEGER,PARAMETER :: TitleLen           = 186  , &
                         NTitles            = 4    , &
                         NColumnHeaderLines = 4    
    TYPE(TimeStepType):: TimeStepLocal
    INTEGER           :: iCount,indxLocation,indxCol,indx,NLakes
    CHARACTER         :: UnitT*10,TextTime*17
    CHARACTER         :: Text1*12
    CHARACTER(LEN=6)  :: CParts(NLakeBudColumns) = (/'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'VOLUME' , &
                                                     'ELEV'   /)
    CHARACTER(LEN=17),PARAMETER :: FParts(NLakeBudColumns)=(/'BEGIN_STORAGE'      ,&
                                                             'END_STORAGE'        ,&
                                                             'FLOW_FROM_UP_LAKE'  ,&    
                                                             'FLOW_FROM_STRM'     ,&
                                                             'FLOW_FROM_BYPASS'   ,&
                                                             'RUNOFF'             ,&
                                                             'RETURN_FLOW'        ,&
                                                             'PRECIP'             ,&
                                                             'GAIN_FROM_GW'       ,&
                                                             'EVAPOTR'            ,&
                                                             'OUTFLOW'            ,&
                                                             'DISCREPANCY'        ,&
                                                             'SURFACE_ELEV'       /)
    
    !Initialize
    NLakes = SIZE(Lakes)
    Text1  = '('//TRIM(LengthUnitMarker)//')'

    !Increment the initial simulation time to represent the data begin date for budget binary output files  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    TextTime = ArrangeText(TRIM(UnitT),17)

    !Budget descriptor
    Header%cBudgetDescriptor = 'lake budget'

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    ALLOCATE (Header%Areas(NLakes))
    Header%NAreas = NLakes
    Header%Areas  = Lakes%Area

    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM LAKE PACKAGE (v'//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('LAKE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('LAKE AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,50(F12.2,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
   
    !Location names
    Header%NLocations = NLakes
    ALLOCATE (Header%cLocationNames(NLakes))
    DO indx=1,NLakes
      Header%cLocationNames(indx) = TRIM(Lakes(indx)%cName) 
    END DO
   
    !Locations
    ALLOCATE (Header%Locations(1)                                                          , &
              Header%Locations(1)%cFullColumnHeaders(NLakeBudColumns+1)                    , &
              Header%Locations(1)%iDataColumnTypes(NLakeBudColumns)                        , &
              Header%Locations(1)%iColWidth(NLakeBudColumns+1)                             , &
              Header%Locations(1)%cColumnHeaders(NLakeBudColumns+1,NColumnHeaderLines)     , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns                          = NLakeBudColumns
      pLocation%cFullColumnHeaders(1)                 = 'Time'                                           
      pLocation%cFullColumnHeaders(2:)                = cBudgetColumnTitles
      pLocation%cFullColumnHeaders(NLakeBudColumns+1) = TRIM(pLocation%cFullColumnHeaders(NLakeBudColumns+1)) // ' ('//LengthUnitMarker//')'
      pLocation%iDataColumnTypes                      = [VLB,&  !Beginning storage
                                                         VLE,&  !Ending storage
                                                         VR ,&  !Flow from upstream lake
                                                         VR ,&  !Flow from streams
                                                         VR ,&  !Flow from bypasses
                                                         VR ,&  !Runoff into lake
                                                         VR ,&  !return flow into lake
                                                         VR ,&  !Precip
                                                         VR ,&  !Gain from groundwater
                                                         VR ,&  !Lake evaporation
                                                         VR ,&  !Lake outflow
                                                         VR ,&  !Discrepency
                                                         LT ]  !Lake surface elevation
      pLocation%iColWidth                             = [17,(12,indx=1,NLakeBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        pColumnHeaders(:,1) = (/'                 ','    Beginning','    Ending   ','  Flow from  ','  Flow from  ','  Flow from  ','             ','    Return   ','             ','   Gain from ','     Lake    ','      Lake   ','             ','Lake Surface '/)
        pColumnHeaders(:,2) = (/'      Time       ','     Storage ','    Storage  ','Upstream Lake','   Streams   ','   Bypasses  ','     Runoff  ','     Flow    ','Precipitation','  Groundwater','  Evaporation','     Outflow ',' Discrepancy ','  Elevation  '/)
        pColumnHeaders(:,3) = (/      TextTime     ,'       (+)   ','      (-)    ','     (+)     ','     (+)     ','     (+)     ','      (+)    ','      (+)    ','      (+)    ','      (+)    ','     (-)     ','       (-)   ','     (=)     ',          Text1/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,13A13)'
        pFormatSpecs(2)     = '(A17,13A13)'
        pFormatSpecs(3)     = '(A17,13A13)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NLakeBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NLakeBudColumns*NLakes) , pDSSOutput%iDataTypes(NLakeBudColumns))
      iCount = 1
      DO indxLocation=1,NLakes
        DO indxCol=1,NLakeBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_LAKE_BUD/'                                      //  &  !A part
                                          UpperCase(TRIM(Lakes(indxLocation)%cName))//'/'        //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                             //  &  !C part
                                          '/'                                                    //  &  !D part
                                          TRIM(TimeStep%Unit)//'/'                               //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                    !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = (/(PER_CUM,indxCol=1,NLakeBudColumns-1),PER_AVER/)
    END ASSOCIATE

  END FUNCTION PrepareLakeBudgetHeader
  
    
  ! -------------------------------------------------------------
  ! --- RESET LAKE ELEVATIONS AND STORAGES TO PREVIOUS LEVELS
  ! -------------------------------------------------------------
  SUBROUTINE ResetElevations(AppLake)
    CLASS(BaseAppLakeType) :: AppLake
    
    AppLake%Lakes%Storage = AppLake%Lakes%Storage_P
    AppLake%Lakes%Elev    = AppLake%Lakes%Elev_P

  END SUBROUTINE ResetElevations
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF THE LAKES
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppLake)
    CLASS(BaseAppLakeType) :: AppLake
    
    AppLake%Lakes%Elev_P    = AppLake%Lakes%Elev
    AppLake%Lakes%Storage_P = AppLake%Lakes%Storage
    
  END SUBROUTINE AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- MODIFY LAKE ELEVS USING DELTA_ELEV
  ! -------------------------------------------------------------
  SUBROUTINE UpdateHeads(AppLake,HDelta)
    CLASS(BaseAppLakeType) :: AppLake
    REAL(8),INTENT(IN)     :: HDelta(:)
    
    !Local variables
    INTEGER :: indxLake
    
    ASSOCIATE (pElevs => AppLake%Lakes%Elev)
        DO indxLake=1,AppLake%NLakes
            pElevs(indxLake)                = MAX(pElevs(indxLake)-HDelta(indxLake) , AppLake%Lakes(indxLake)%RatingTable%XPoint(1))
            AppLake%Lakes(indxLake)%Storage = MAX(AppLake%Lakes(indxLake)%RatingTable%Evaluate(pElevs(indxLake)) , 0.0)
        END DO
    END ASSOCIATE
    
  END SUBROUTINE UpdateHeads
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE LAKE EVAPORATION
  ! -------------------------------------------------------------
  SUBROUTINE ComputeLakeETa(AppLake,GSElev,StrmInflows)
    CLASS(BaseAppLakeType) :: AppLake
    REAL(8),INTENT(IN)     :: GSElev(:),StrmInflows(AppLake%NLakes)

    !Local variables
    INTEGER :: indxNode,indxLake,iNode
    REAL(8) :: Stor,Evap,Area,Elev_P,rInflow,PrecipRate,ETp_Rate,ETa

    !Iterate over lakes
    DO indxLake=1,AppLake%NLakes
      !Initialize
      ETa        = 0.0
      Area       = AppLake%Lakes(indxLake)%Area
      Elev_P     = AppLake%Lakes(indxLake)%Elev_P
      rInflow    = (AppLake%Lakes(indxLake)%InflowUpLake + StrmInflows(indxLake)) / Area
      PrecipRate = AppLake%Lakes(indxLake)%PrecipRate
      ETp_Rate   = AppLake%Lakes(indxLake)%ETp_Rate
      
      !Iterate over nodes
      DO indxNode=1,AppLake%Lakes(indxLake)%NNodes
        iNode = AppLake%Lakes(indxLake)%Nodes(indxNode) 
        Stor  = MAX(Elev_P - GSElev(iNode),0.0)
        Evap  = MIN(Stor + rInflow + PrecipRate , ETp_Rate)
        ETa   = ETa + Evap*AppLake%Lakes(indxLake)%NodeAreas(indxNode)
      END DO
      
      !Store ETa in permanent arrays
      AppLake%Lakes(indxLake)%ETa =ETa
      
    END DO

  END SUBROUTINE ComputeLakeETa

  
  ! -------------------------------------------------------------
  ! --- GENERATE RATING TABLE FOR LAKES
  ! -------------------------------------------------------------
  SUBROUTINE GenerateRatingTable(AppGrid,Elements,Nodes,GSElevation,RatingTable,iStat)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: Elements(:),Nodes(:)
    REAL(8),INTENT(IN)           :: GSElevation(:)
    TYPE(PairedDataType)         :: RatingTable
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'GenerateRatingTable'
    INTEGER                      :: indx,Counter,ElemNo,indxNode,indxTable,Node,iVertex(4),NVertex
    REAL(8)                      :: ElevMin,LocalElev(SIZE(Nodes)),VertexArea(4)
    REAL(8),ALLOCATABLE          :: HLake(:),VLake(:)
             
    !Initialize
    iStat   = 0
    ElevMin = -HUGE(1d0)
    Counter = 0

    !Weed out same elevations
    DO indx=1,SIZE(Nodes)
        IF (LocateInList(GSElevation(Nodes(indx)),LocalElev(1:Counter)) .EQ. 0) THEN
            Counter            = Counter+1
            LocalElev(Counter) = GSElevation(Nodes(indx))
        END IF
    END DO
    CALL AllocArray(HLake,Counter+1,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AllocArray(VLake,Counter+1,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Order ground elevations from small to large
    DO indx=1,Counter
        IF (ElevMin .NE. MAXVAL(LocalElev(1:Counter))) THEN
            ElevMin     = MINVAL(LocalElev(1:Counter),LocalElev(1:Counter) .GT. ElevMin)
            HLake(indx) = ElevMin
        END IF
    END DO
    HLake(Counter+1) = HLake(Counter) + 10.0  !Last point in the rating table as lake elevation 10 feet above max ground surface elevation

    !Compute points of lake elevation vs storage rating table
    DO indx=1,SIZE(Elements)
        ElemNo                = Elements(indx)
        NVertex               = AppGrid%Element(ElemNo)%NVertex
        iVertex               = AppGrid%Element(ElemNo)%Vertex
        VertexArea(1:NVertex) = AppGrid%AppElement(ElemNo)%VertexArea
        DO indxNode=1,NVertex
            Node = iVertex(indxNode)
            !Computation of rating table points
            DO indxTable=1,Counter+1
                IF (HLake(indxTable) .GE. GSElevation(Node))  &
                    VLake(indxTable) = VLake(indxTable)+(HLake(indxTable)-GSElevation(Node))*VertexArea(indxNode)
            END DO
        END DO
    END DO  
        
    !Instantiate rating table
    CALL RatingTable%New(Counter+1,HLake,VLake,iStat)        

  END SUBROUTINE GenerateRatingTable  
  
END MODULE