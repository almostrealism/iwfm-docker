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
MODULE Class_Model_ForInquiry
  USE GeneralUtilities          , ONLY: EstablishAbsolutePathFileName    , &
                                        LocateInList
  USE TimeSeriesUtilities       , ONLY: TimeStepType
  USE IOInterface               , ONLY: GenericFileType                  , &
                                        DoesFileExist                    , &
                                        iGetFileType_FromName            , &
                                        TXT                              , &
                                        HDF
  USE Package_Discretization    , ONLY: AppGridType                      , &
                                        StratigraphyType
  USE Package_Misc              , ONLY: RealTSDataInFileType             , &
                                        iAllLocationIDsListed            , &
                                        iLocationType_Node               , &
                                        iLocationType_Zone               , &
                                        iLocationType_Subregion          , & 
                                        iLocationType_Lake               , & 
                                        iLocationType_StrmNode           , & 
                                        iLocationType_StrmReach          , & 
                                        iLocationType_SmallWatershed     , & 
                                        iLocationType_GWHeadObs          , &
                                        iLocationType_StrmHydObs         , &
                                        iLocationType_SubsidenceObs      , &
                                        iLocationType_TileDrain          , &
                                        iStrmComp                        , &
                                        iLakeComp                        , &
                                        iGWComp                          , &
                                        iRootZoneComp                    , &
                                        iUnsatZoneComp                   , &
                                        iSWShedComp                      
  USE Package_AppGW             , ONLY: AppGWType    
  USE Package_GWZBudget         , ONLY: GWZBudgetType
  USE Package_AppStream         , ONLY: AppStreamType
  USE Package_AppLake           , ONLY: AppLakeType
  USE Package_AppUnsatZone      , ONLY: AppUnsatZoneType
  USE Package_AppSmallWatershed , ONLY: AppSmallWatershedType
  USE Package_RootZone          , ONLY: RootZoneType
  USE Package_PrecipitationET   , ONLY: PrecipitationType
  USE Package_Budget            , ONLY: BudgetType
  USE Package_ZBudget           , ONLY: ZBudgetType                      , &
                                        ZoneListType                     , &
                                        IsZBudgetFile
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
  PUBLIC :: Model_ForInquiry_Type    , &
            LocationsWithDataType    

  
  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR LIST OF LOCATIONS WHERE DATA/SUB-DATA EXISTS
  ! -------------------------------------------------------------
  TYPE LocationsWithDataType
      INTEGER             :: NLocations = 0
      INTEGER,ALLOCATABLE :: iLocations(:)
  END TYPE LocationsWithDataType
  
  
  ! -------------------------------------------------------------
  ! --- FEATURE SUB-DATA AND FILE INFORMATION TYPE
  ! -------------------------------------------------------------
  TYPE SubDataType
      INTEGER                        :: NSubData
      CHARACTER(LEN=100),ALLOCATABLE :: cSubDataNames(:)
  END TYPE SubDataType
  
  
  ! -------------------------------------------------------------
  ! --- FEATURE, RELATED DATA, SUB-DATA AND FILE INFORMATION TYPE
  ! -------------------------------------------------------------
  TYPE DataForFeatureType
      INTEGER                                 :: NData                 = 0
      CHARACTER(LEN=100),ALLOCATABLE          :: cDataNames(:)
      INTEGER,ALLOCATABLE                     :: iDataComponentIDs(:)
      LOGICAL,ALLOCATABLE                     :: lDataIsBudgetType(:)
      CHARACTER(LEN=500),ALLOCATABLE          :: cFilesForData(:)
      TYPE(SubDataType),ALLOCATABLE           :: SubData(:)
      TYPE(LocationsWithDataType),ALLOCATABLE :: LocationsWithData(:)     
  CONTAINS
      PROCEDURE,PASS :: Kill         => DataForFeature_Kill 
      PROCEDURE,PASS :: ReadFromFile => DataForFeature_ReadFromFile
      PROCEDURE,PASS :: PrintToFile  => DataForFeature_PrintToFile
  END TYPE DataForFeatureType
  
  
  ! -------------------------------------------------------------
  ! --- SIMPLIFIED MODEL DATA TYPE FOR INQUIRY
  ! -------------------------------------------------------------
  TYPE Model_ForInquiry_Type
      INTEGER                  :: NUnsatLayers           = 0
      INTEGER                  :: NSmallWatersheds       = 0
      INTEGER                  :: NTileDrains            = 0
      INTEGER                  :: NGWHeadObs             = 0
      INTEGER                  :: NStrmHydObs            = 0
      INTEGER                  :: NSubsidenceObs         = 0
      INTEGER                  :: NTileDrainObs          = 0
      TYPE(DataForFeatureType) :: DataForNodes
      TYPE(DataForFeatureType) :: DataForZones
      TYPE(DataForFeatureType) :: DataForSubregions
      TYPE(DataForFeatureType) :: DataForLakes
      TYPE(DataForFeatureType) :: DataForStrmNodes
      TYPE(DataForFeatureType) :: DataForStrmReaches
      TYPE(DataForFeatureType) :: DataForSmallWatersheds
      TYPE(DataForFeatureType) :: DataForGWHeadObs
      TYPE(DataForFeatureType) :: DataForStrmHydObs
      TYPE(DataForFeatureType) :: DataForSubsidenceObs
      TYPE(DataForFeatureType) :: DataForTileDrainObs
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,NOPASS :: GetNDataList_AtLocationType_FromFullModel    
      PROCEDURE,PASS   :: GetNDataList_AtLocationType_FromInquiryModel 
      PROCEDURE,NOPASS :: GetDataList_AtLocationType_FromFullModel     
      PROCEDURE,PASS   :: GetDataList_AtLocationType_FromInquiryModel  
      PROCEDURE,NOPASS :: GetSubDataList_AtLocation_FromFullModel      
      PROCEDURE,PASS   :: GetSubDataList_AtLocation_FromInquiryModel  
      PROCEDURE,NOPASS :: GetModelData_AtLocation_FromFullModel        
      PROCEDURE,PASS   :: GetModelData_AtLocation_FromInquiryModel 
      PROCEDURE,NOPASS :: DeleteDataFile                  
      PROCEDURE,NOPASS :: PrintModelData
      PROCEDURE,NOPASS :: IsInstantiableFromFile
  END TYPE Model_ForInquiry_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  CHARACTER(LEN=27),PARAMETER         :: cModelDataFileName = 'IW_ModelData_ForInquiry.bin'
  INTEGER,PARAMETER                   :: ModNameLen         = 24
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName            = 'Class_Model_ForInquiry::'

  
  
  
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
  ! --- NEW MODEL FROM DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(Model,cSIMWorkingDirectory,TimeStep,NTIME,iStat)
    CLASS(Model_ForInquiry_Type)   :: Model
    CHARACTER(LEN=*),INTENT(IN)    :: cSIMWorkingDirectory
    TYPE(TimeStepType),INTENT(OUT) :: TimeStep
    INTEGER,INTENT(OUT)            :: NTIME,iStat
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(GenericFileType)    :: ModelDataFile
    
    !Initialize
    iStat = 0
    
    !Open file to read model data for inquiry
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    CALL ModelDataFile%New(cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='data file for simplified model for inquiry',iStat=iStat)
    IF (iStat .EQ. -1) GOTO 10
    
    !Read time-related data
    CALL ModelDataFile%ReadData(TimeStep%TrackTime,iStat)            ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%CurrentDateAndTime,iStat)   ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%EndDateAndTime,iStat)       ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%DeltaT,iStat)               ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%DeltaT_InMinutes,iStat)     ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%Unit,iStat)                 ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(NTIME,iStat)                         ; IF (iStat .EQ. -1) GOTO 10
    
    !Read structural data
    CALL ModelDataFile%ReadData(Model%NUnsatLayers,iStat)            ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NTileDrains,iStat)             ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NSmallWatersheds,iStat)        ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NGWHeadObs,iStat)              ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NStrmHydObs,iStat)             ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NSubsidenceObs,iStat)          ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%NTileDrainObs,iStat)           ; IF (iStat .EQ. -1) GOTO 10
    
    !Data for nodes
    CALL Model%DataForNodes%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for zones
    CALL Model%DataForZones%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !Data for subregions
    CALL Model%DataForSubregions%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !Data for lakes
    CALL Model%DataForLakes%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for stream nodes
    CALL Model%DataForStrmNodes%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for stream reaches
    CALL Model%DataForStrmReaches%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for small watersheds
    CALL Model%DataForSmallWatersheds%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for gw hydrographs
    CALL Model%DataForGWHeadObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for stream hydrographs
    CALL Model%DataForStrmHydObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for subsidence hydrographs
    CALL Model%DataForSubsidenceObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10

    !Data for tile drain hydrographs
    CALL Model%DataForTileDrainObs%ReadFromFile(ModelDataFile,iStat)  ;  IF (iStat .EQ. -1) GOTO 10
    
    !Close file
10  CALL ModelDataFile%Kill()
        
  END SUBROUTINE New
  

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
  ! --- KILL SUB-DATA
  ! -------------------------------------------------------------
  SUBROUTINE SubData_Kill(SubData)
    TYPE(SubDataType) :: SubData(:)
    
    !Local variables
    INTEGER :: indx,ErrorCode
    
    DO indx=1,SIZE(SubData)
        DEALLOCATE (SubData(indx)%cSubDataNames ,STAT=ErrorCode)
        SubData(indx)%NSubData = 0
    END DO
    
  END SUBROUTINE SubData_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL LOCATIONS WITH DATA
  ! -------------------------------------------------------------
  SUBROUTINE LocationsWithData_Kill(LocationsWithData)
    TYPE(LocationsWithDataType) :: LocationsWithData(:)
    
    !Local variables
    INTEGER :: indx,ErrorCode
    
    DO indx=1,SIZE(LocationsWithData)
        DEALLOCATE (LocationsWithData(indx)%iLocations ,STAT=ErrorCode)
        LocationsWithData(indx)%NLocations = 0
    END DO
    
  END SUBROUTINE LocationsWithData_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL DATA FOR FEATURE
  ! -------------------------------------------------------------
  SUBROUTINE DataForFeature_Kill(FeatureData)
    CLASS(DataForFeatureType) :: FeatureData
    
    !Local variables
    INTEGER                  :: ErrorCode
    TYPE(DataForFeatureType) :: Dummy
    
    IF (ALLOCATED(FeatureData%SubData)) CALL SubData_Kill(FeatureData%SubData)
    
    IF (ALLOCATED(FeatureData%LocationsWithData)) CALL LocationsWithData_Kill(FeatureData%LocationsWithData)
    
    DEALLOCATE (FeatureData%cDataNames , FeatureData%iDataComponentIDs , FeatureData%lDataIsBudgetType , FeatureData%cFilesForData , FeatureData%SubData , FeatureData%LocationsWithData , STAT=ErrorCode)
    SELECT TYPE (FeatureData)
        TYPE IS (DataForFeatureType)
            FeatureData = Dummy
    END SELECT
    
  END SUBROUTINE DataForFeature_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Model)
    CLASS(Model_ForInquiry_Type) :: Model
    
    !Local variables
    TYPE(Model_ForInquiry_Type) :: Dummy
    
    CALL Model%DataForNodes%Kill()
    CALL Model%DataForZones%Kill()
    CALL Model%DataForSubregions%Kill()
    CALL Model%DataForLakes%Kill()
    CALL Model%DataForStrmNodes%Kill()
    CALL Model%DataForStrmReaches%Kill()
    CALL Model%DataForSmallWatersheds%Kill()
    CALL Model%DataForGWHeadObs%Kill()
    CALL Model%DataForStrmHydObs%Kill()
    CALL Model%DataForTileDrainObs%Kill()
    
    SELECT TYPE (Model)
        TYPE IS (Model_ForInquiry_Type)
            Model = Dummy
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
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING FROM FULL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,iLocationID,iCompID,cDataType,iSubDataIndex,iZExtent,iElems,iLayers,iZones,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)         :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)          :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)      :: AppUnsatZone
    TYPE(AppLakeType),INTENT(IN)           :: AppLake
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                     :: iLocationType,iLocationID,iCompID,iSubDataIndex,iZExtent,iElems(:),iLayers(:),iZones(:)
    CHARACTER(LEN=*),INTENT(IN)            :: cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)                     :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                    :: iDataUnitType                           !What is the data unit type (length, area, or volume)?
    INTEGER,INTENT(OUT)                    :: nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)                    :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    INTEGER            :: iZonesWithNames(0)
    CHARACTER(LEN=0)   :: cZoneNames(0)
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    SELECT CASE (iCompID)
        CASE (iStrmComp)
            IF (AppStream%IsDefined()) CALL AppStream%GetModelData_AtLocation(iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            
        CASE (iLakeComp) 
            IF (AppLake%IsDefined()) CALL AppLake%GetModelData_AtLocation(iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                    
        CASE (iGWComp)
            IF (iLocationType .EQ. iLocationType_Zone) THEN
                CALL GWZBudget%GetModelData_AtLocation(iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            ELSE
                CALL AppGW%GetModelData_AtLocation(iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)  
            END IF
            
        CASE (iRootZoneComp)
            IF (RootZone%IsDefined()) THEN
                CALL RootZone%GetModelData_AtLocation(iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            END IF
            
        CASE (iUnsatZoneComp)
            IF (AppUnsatZone%IsDefined()) CALL AppUnsatZone%GetModelData_AtLocation(iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
        
        CASE (iSWShedComp)
            IF (AppSWShed%IsDefined()) CALL AppSWShed%GetModelData_AtLocation(iLocationType,iLocationID,cDataType,iSubDataIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
            
    END SELECT
        
  END SUBROUTINE GetModelData_AtLocation_FromFullModel

  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromInquiryModel(Model,TimeStep,NLayers,iLocationType,iLocationID,iCompID,cDataName,iSubDataIndex,iZExtent,iElems,iLayers,iZones,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    TYPE(TimeStepType),INTENT(IN)                  :: TimeStep
    INTEGER,INTENT(IN)                             :: NLayers,iLocationType,iLocationID,iCompID,iSubDataIndex,iZExtent,iElems(:),iLayers(:),iZones(:)
    CHARACTER(LEN=*),INTENT(IN)                    :: cDataName,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)                             :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                            :: iDataUnitType                           !What is the data unit type (length, area, or volume)?
    INTEGER,INTENT(OUT)                            :: nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)                            :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)                            :: iStat
    
    !Local variables
    INTEGER                          :: indx,iZonesWithNames(0),iDataUnitTypeArray(1)
    INTEGER                          :: iReadCols(1)  
    REAL(8)                          :: rValues(2,SIZE(rOutputDates))
    CHARACTER(LEN=0)                 :: cZoneNames(0)
    TYPE(DataForFeatureType),POINTER :: pFeatureData
    TYPE(BudgetType)                 :: InFile_Budget
    TYPE(ZBudgetType)                :: InFile_ZBudget
    TYPE(ZoneListType)               :: ZoneList
    TYPE(AppGWType)                  :: AppGW_ForInquiry
    TYPE(AppStreamType)              :: AppStream_ForInquiry
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_Node)
            pFeatureData => Model%DataForNodes
            
        CASE (iLocationType_Zone)
            pFeatureData => Model%DataForZones
            
        CASE (iLocationType_Subregion)
            pFeatureData => Model%DataForSubregions
        
        CASE (iLocationType_Lake)
            pFeatureData => Model%DataForLakes
            
        CASE (iLocationType_StrmNode)
            pFeatureData => Model%DataForStrmNodes

        CASE (iLocationType_StrmReach)
            pFeatureData => Model%DataForStrmReaches

        CASE (iLocationType_SmallWatershed)
            pFeatureData => Model%DataForSmallWatersheds
            
        CASE (iLocationType_GWHeadObs)
            pFeatureData => Model%DataForGWHeadObs
            
        CASE (iLocationType_StrmHydObs)
            pFeatureData => Model%DataForStrmHydObs
            
        CASE (iLocationType_SubsidenceObs)
            pFeatureData => Model%DataForSubsidenceObs
            
        CASE (iLocationType_TileDrain)
            pFeatureData => Model%DataForTileDrainObs
            
    END SELECT
        
    !Find the file that the data is in and read data
    DO indx=1,pFeatureData%NData
        IF (iCompID .EQ. pFeatureData%iDataComponentIDs(indx)) THEN
            IF (TRIM(cDataName) .EQ. TRIM(pFeatureData%cDataNames(indx))) THEN
                !Make sure location has data
                IF (.NOT. DoesLocationHaveData(iLocationID,pFeatureData%LocationsWithData(indx))) EXIT
                
                !Data file is budget or z-budget file
                IF (pFeatureData%lDataIsBudgetType(indx)) THEN
                    !It is a Z-Budget file?
                    IF (IsZBudgetFile(pFeatureData%cFilesForData(indx))) THEN
                        !Open file
                        CALL InFile_ZBudget%New(pFeatureData%cFilesForData(indx),iStat)
                        IF (iStat .EQ. -1) RETURN
                        
                        !Generate zone list
                        CALL ZoneList%New(InFile_ZBudget%Header%iNData,InFile_ZBudget%Header%lFaceFlows_Defined,InFile_ZBudget%SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
                        
                        !Read data
                        iReadCols = iSubDataIndex
                        CALL InFile_ZBudget%ReadData(ZoneList,iLocationID,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_AR,rFact_VL,iDataUnitTypeArray,nActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
                        rOutputDates(1:nActualOutput)  = rValues(1,1:nActualOutput)
                        rOutputValues(1:nActualOutput) = rValues(2,1:nActualOutput)
                        iDataUnitType                  = iDataUnitTypeArray(1)
                       
                        !Delete zone list
                        CALL ZoneList%Kill()
                        
                        !Close file
                        CALL InFile_ZBudget%Kill()
                        
                    !It is a Budget file
                    ELSE
                        !Open file    
                        CALL InFile_Budget%New(pFeatureData%cFilesForData(indx),iStat)
                        IF (iStat .EQ. -1) RETURN
                        
                        !Read data
                        CALL InFile_Budget%ReadData(iLocationID,iSubDataIndex,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                        
                        !Close file
                        CALL InFile_Budget%Kill()
                    END IF
                    
                !Otherwise
                ELSE
                    !Get data based on component ID
                    SELECT CASE (iCompID)
                        CASE (iGWComp)
                            CALL AppGW_ForInquiry%GetModelData_AtLocation(pFeatureData%cFilesForData(indx),NLayers,TimeStep,iLocationType,iLocationID,cDataName,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)

                        CASE (iStrmComp)
                            CALL AppStream_ForInquiry%GetModelData_AtLocation(pFeatureData%cFilesForData(indx),TimeStep,iLocationType,iLocationID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                   
                    END SELECT
                END IF

                !Exit loop
                EXIT
            END IF
        END IF
    END DO
    
    !Clear pointer
    NULLIFY (pFeatureData)
    
  END SUBROUTINE GetModelData_AtLocation_FromInquiryModel

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE FULL MODEL COMPONENTS
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType) RESULT(NData)
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)         :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)          :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)      :: AppUnsatZone
    TYPE(AppLAkeType),INTENT(IN)           :: AppLake
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                     :: iLocationType
    INTEGER                                :: NData
    
    !Initialize
    NData = 0
    
    !Groundwater
    NData = NData + AppGW%GetNDataList_AtLocationType(iLocationType)
    
    !Groundwater zone budget
    NData = NData + GWZBudget%GetNDataList_AtLocationType(iLocationType)
    
    !Streams
    NData = NData + AppStream%GetNDataList_AtLocationType(iLocationType)
    
    !Root zone
    NData = NData + RootZone%GetNDataList_AtLocationType(iLocationType)
    
    !Lakes
    NData = NData + AppLake%GetNDataList_AtLocationType()
    
    !Unsaturated zone
    NData = NData + AppUnsatZone%GetNDataList_AtLocationType(iLocationType)
    
    !Small watersheds
    NData = NData + AppSWShed%GetNDataList_AtLocationType()
            
  END FUNCTION GetNDataList_AtLocationType_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType_FromInquiryModel(Model,iLocationType) RESULT (NData)
    CLASS(Model_ForInquiry_Type),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                       :: iLocationType
    INTEGER                                  :: NData
    
    NData = 0
        
    SELECT CASE (iLocationType)
        CASE (iLocationType_Node)
           NData = Model%DataForNodes%NData
            
        CASE (iLocationType_Zone)
            NData = Model%DataForZones%NData
            
        CASE (iLocationType_Subregion)
            NData = Model%DataForSubregions%NData
        
        CASE (iLocationType_Lake)
            NData = Model%DataForLakes%NData
            
        CASE (iLocationType_StrmNode)
            NData = Model%DataForStrmNodes%NData

        CASE (iLocationType_StrmReach)
            NData = Model%DataForStrmReaches%NData

        CASE (iLocationType_SmallWatershed)
            NData = Model%DataForSmallWatersheds%NData
            
        CASE (iLocationType_GWHeadObs)
            NData = Model%DataForGWHeadObs%NData
            
        CASE (iLocationType_StrmHydObs)
            NData = Model%DataForStrmHydObs%NData
            
        CASE (iLocationType_SubsidenceObs)
            NData = Model%DataForSubsidenceObs%NData
            
        CASE (iLocationType_TileDrain)
            NData = Model%DataForTileDrainObs%NData
            
    END SELECT
        
  END FUNCTION GetNDataList_AtLocationType_FromInquiryModel
    

  ! -------------------------------------------------------------
  ! --- GET AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE FULL MODEL COMPONENTS
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,cDataList,cFileList,iDataCompID,lBudgetType,LocationsWithData)
    TYPE(AppGWType),INTENT(IN)                          :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)                      :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)                       :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)                   :: AppUnsatZone
    TYPE(AppLAkeType),INTENT(IN)                        :: AppLake
    TYPE(AppStreamType),INTENT(IN)                      :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN)              :: AppSWShed
    INTEGER,INTENT(IN)                                  :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)            :: cDataList(:),cFileList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)                     :: iDataCompID(:)
    LOGICAL,ALLOCATABLE,INTENT(OUT)                     :: lBudgetType(:)
    TYPE(LocationsWithDataType),ALLOCATABLE,INTENT(OUT) :: LocationsWithData(:)
    
    !Local variables
    INTEGER,PARAMETER           :: iMaxDim = 10
    INTEGER                     :: nData,ErrorCode,iDim,indx
    CHARACTER(LEN=100)          :: cDataList_Local(iMaxDim)
    CHARACTER(LEN=500)          :: cFileList_Local(iMaxDim)
    LOGICAL                     :: lBudgetType_Local(iMaxDim)
    INTEGER                     :: iDataCompID_Local(iMaxDim)
    TYPE(LocationsWithDataType) :: LocationsWithData_Local(iMaxDim)
    
    !Initialize
    nData = 0
    
    !Groundwater
    CALL AppGW%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppGW%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = iGWComp
    nData                                   = nData + iDim
    
    !Groundwater zone
    CALL GWZBudget%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL GWZBudget%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = iGWComp
    nData                                   = nData + iDim
    
    !Streams
    CALL AppStream%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppStream%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = iStrmComp
    nData                                   = nData + iDim
    
    !Root zone
    CALL RootZone%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL RootZone%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = iRootZoneComp
    nData                                   = nData + iDim
    
    !Lakes
    CALL AppLake%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppLake%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = iLakeComp
    nData                                   = nData + iDim
    
    !Unsaturated zone
    CALL AppUnsatZone%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppUnsatZone%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = iUnsatZoneComp
    nData                                   = nData + iDim
    
    !Small watersheds
    CALL AppSWShed%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    IF (ALLOCATED(cDataList)) THEN
        iDim = SIZE(cDataList)
    ELSE
        iDim = 0
    END IF
    DO indx=1,iDim
        CALL AppSWShed%GetLocationsWithData(iLocationType,cDataList(indx),LocationsWithData_Local(nData+indx)%iLocations)
        LocationsWithData_Local(nData+indx)%NLocations = SIZE(LocationsWithData_Local(nData+indx)%iLocations)
    END DO
    cDataList_Local(nData+1:nData+iDim)     = cDataList
    cFileList_Local(nData+1:nData+iDim)     = cFileList
    lBudgetType_Local(nData+1:nData+iDim)   = lBudgetType
    iDataCompID_Local(nData+1:nData+iDim)   = iSWShedComp
    nData                                   = nData + iDim
    
    !Store data in the return variables
    DEALLOCATE (iDataCompID       , STAT=ErrorCode)
    DEALLOCATE (cDataList         , STAT=ErrorCode)
    DEALLOCATE (cFileList         , STAT=ErrorCode)
    DEALLOCATE (lBudgetType       , STAT=ErrorCode)
    DEALLOCATE (LocationsWithData , STAT=ErrorCode)
    ALLOCATE (iDataCompID(nData) , cDataList(nData) , cFileList(nData) , lBudgetType(nData) ,  LocationsWithData(nData))
    iDataCompID       = iDataCompID_Local(1:nData)
    cDataList         = cDataList_Local(1:nData)
    cFileList         = cFileList_Local(1:nData)
    lBudgetType       = lBudgetType_Local(1:nData)
    LocationsWithData = LocationsWithData_Local(1:nData)

  END SUBROUTINE GetDataList_AtLocationType_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET AVAILABLE POST-PROCESSING DATA FOR A LOCATION TYPE USING THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType_FromInquiryModel(Model,iLocationType,cDataList,iDataCompID,lBudgetType)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                              :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)        :: cDataList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)                 :: iDataCompID(:)
    LOGICAL,ALLOCATABLE,INTENT(OUT)                 :: lBudgetType(:)
    
    !Local variables
    INTEGER                          :: nData,ErrorCode
    TYPE(DataForFeatureType),POINTER :: pDataForFeature
    
    !Initialize
    nData = 0
    DEALLOCATE (iDataCompID , cDataList , lBudgetType , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_Node)
            pDataForFeature => Model%DataForNodes
            
        CASE (iLocationType_Zone)
            pDataForFeature => Model%DataForZones
            
        CASE (iLocationType_Subregion)
            pDataForFeature => Model%DataForSubregions
        
        CASE (iLocationType_Lake)
            pDataForFeature => Model%DataForLakes
            
        CASE (iLocationType_StrmNode)
            pDataForFeature => Model%DataForStrmNodes

        CASE (iLocationType_StrmReach)
            pDataForFeature => Model%DataForStrmReaches

        CASE (iLocationType_SmallWatershed)
            pDataForFeature => Model%DataForSmallWatersheds
            
        CASE (iLocationType_GWHeadObs)
            pDataForFeature => Model%DataForGWHeadObs
            
        CASE (iLocationType_StrmHydObs)
            pDataForFeature => Model%DataForStrmHydObs
            
        CASE (iLocationType_SubsidenceObs)
            pDataForFeature => Model%DataForSubsidenceObs
            
        CASE (iLocationType_TileDrain)
            pDataForFeature => Model%DataForTileDrainObs
            
    END SELECT
        
    !Save info in return variables
    nData = pDataForFeature%NData
    ALLOCATE (cDataList(nData) , iDataCompID(nData) , lBudgetType(nData))
    cDataList   = pDataForFeature%cDataNames
    iDataCompID = pDataForFeature%iDataComponentIDs
    lBudgetType = pDataForFeature%lDataIsBudgetType
        
    !Clear memory
    NULLIFY (pDataForFeature)

  END SUBROUTINE GetDataList_AtLocationType_FromInquiryModel
    

  ! -------------------------------------------------------------
  ! --- GET AVAILABLE SUB-DATA TYPES FOR A LOCATION TYPE FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_AtLocation_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,iLocationID,iCompID,cDataType,cSubDataList)
    TYPE(AppGWType),INTENT(IN)               :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)           :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)            :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)        :: AppUnsatZone
    TYPE(AppLakeType),INTENT(IN)             :: AppLake
    TYPE(AppStreamType),INTENT(IN)           :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN)   :: AppSWShed
    INTEGER,INTENT(IN)                       :: iLocationType,iLocationID,iCompID
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    SELECT CASE (iCompID)
        CASE (iStrmComp)
            IF (AppStream%IsDefined()) CALL AppStream%GetSubDataList_AtLocation(iLocationType,iLocationID,cDataType,cSubDataList)
            
        CASE (iLakeComp) 
            IF (AppLake%IsDefined()) CALL AppLake%GetSubDataList_AtLocation(iLocationType,cDataType,cSubDataList)
                    
        CASE (iGWComp)
            IF (iLocationType .EQ. iLocationType_Zone) THEN
                CALL GWZBudget%GetSubDataList_AtLocation(iLocationType,iLocationID,cDataType,cSubDataList)  
            ELSE
                CALL AppGW%GetSubDataList_AtLocation(iLocationType,cDataType,cSubDataList)  
            END IF
        
        CASE (iRootZoneComp)
            IF (RootZone%IsDefined()) CALL RootZone%GetSubDataList_AtLocation(iLocationType,cDataType,cSubDataList)
        
        CASE (iUnsatZoneComp)
            IF (AppUnsatZone%IsDefined()) CALL AppUnsatZone%GetSubDataList_AtLocation(iLocationType,cDataType,cSubDataList)
        
        CASE (iSWShedComp)
            IF (AppSWShed%IsDefined()) CALL AppSWShed%GetSubDataList_AtLocation(iLocationType,cDataType,cSubDataList)
            
        CASE DEFAULT
            !The rest of the features don't have sub-data
            
    END SELECT
            
  END SUBROUTINE GetSubDataList_AtLocation_FromFullModel
  
  
  ! -------------------------------------------------------------
  ! --- GET AVAILABLE SUB-DATA TYPES FOR A LOCATION TYPE FOR POST_PROCESSING FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_AtLocation_FromInquiryModel(Model,iLocationType,iLocationID,iCompID,cDataType,cSubDataList)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    INTEGER,INTENT(IN)                             :: iLocationType,iLocationID,iCompID
    CHARACTER(LEN=*),INTENT(IN)                    :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)       :: cSubDataList(:)
    
    !Local variables
    INTEGER                          :: indx,ErrorCode
    TYPE(DataForFeatureType),POINTER :: pFeatureData
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_Node)
            pFeatureData => Model%DataForNodes

        CASE (iLocationType_Zone)
            pFeatureData => Model%DataForZones
            
        CASE (iLocationType_Subregion)
            pFeatureData => Model%DataForSubregions
            
        CASE (iLocationType_Lake)
            pFeatureData => Model%DataForLakes
            
        CASE (iLocationType_StrmNode)
            pFeatureData => Model%DataForStrmNodes
            
        CASE (iLocationType_StrmReach)
            pFeatureData => Model%DataForStrmReaches
            
        CASE (iLocationType_SmallWatershed)
            pFeatureData => Model%DataForSmallWatersheds
            
        CASE (iLocationType_GWHeadObs)
            pFeatureData => Model%DataForGWHeadObs
            
        CASE (iLocationType_StrmHydObs)
            pFeatureData => Model%DataForStrmHydObs
            
        CASE (iLocationType_SubsidenceObs)
            pFeatureData => Model%DataForSubsidenceObs
            
        CASE (iLocationType_TileDrain)
            pFeatureData => Model%DataForTileDrainObs
            
    END SELECT
    
    !Store sub-data in the return variable
    DO indx=1,pFeatureData%NData
        IF (pFeatureData%iDataComponentIDs(indx) .EQ. iCompID) THEN
            IF (TRIM(cDataType) .EQ. TRIM(pFeatureData%cDataNames(indx))) THEN
                IF (DoesLocationHaveData(iLocationID,pFeatureData%LocationsWithData(indx))) THEN
                    ALLOCATE (cSubDataList(pFeatureData%SubData(indx)%NSubData))
                    cSubDataList = ''
                    cSubDataList = pFeatureData%SubData(indx)%cSubDataNames
                    EXIT
                END IF
            END IF
        END IF
    END DO
               
    !Clear pointer
    NULLIFY (pFeatureData)
            
  END SUBROUTINE GetSubDataList_AtLocation_FromInquiryModel

  
  
  
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
  ! --- READ SUB-DATA FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE SubData_ReadFromFile(SubData,InFile,iStat)
    TYPE(SubDataType),INTENT(OUT) :: SubData(:)
    TYPE(GenericFileType)         :: InFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: indx,NSubData
    
    DO indx=1,SIZE(SubData)
        CALL InFile%ReadData(NSubData,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (NSubData .EQ. 0) CYCLE
        SubData(indx)%NSubData = NSubData
        ALLOCATE (SubData(indx)%cSubDataNames(NSubData))
        CALL InFile%ReadData(SubData(indx)%cSubDataNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE SubData_ReadFromFile
  
  
  ! -------------------------------------------------------------
  ! --- READ LOCATIONS WITH DATA FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE LocationsWithData_ReadFromFile(LocationsWithData,InFile,iStat)
    TYPE(LocationsWithDataType),INTENT(OUT) :: LocationsWithData(:)
    TYPE(GenericFileType)                   :: InFile
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables
    INTEGER :: indx,NLocations
    
    DO indx=1,SIZE(LocationsWithData)
        CALL InFile%ReadData(NLocations,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (NLocations .EQ. 0) CYCLE
        LocationsWithData(indx)%NLocations = NLocations
        ALLOCATE (LocationsWithData(indx)%iLocations(NLocations))
        CALL InFile%ReadData(LocationsWithData(indx)%iLocations,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE LocationsWithData_ReadFromFile
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FROM FILE FOR A FEATURE
  ! -------------------------------------------------------------
  SUBROUTINE DataForFeature_ReadFromFile(FeatureData,InFile,iStat)
    CLASS(DataForFeatureType) :: FeatureData
    TYPE(GenericFileType)     :: InFile
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    INTEGER :: NData
    
    CALL InFile%ReadData(NData,iStat)        
    IF (iStat .EQ. -1) RETURN
    FeatureData%NData = NData
    
    ALLOCATE (FeatureData%cDataNames(NData)        , &
              FeatureData%iDataComponentIDs(NData) , &
              FeatureData%lDataIsBudgetType(NData) , &
              FeatureData%cFilesForData(NData)     , &
              FeatureData%SubData(NData)           , &
              FeatureData%LocationsWithData(NData) )
    
    !Return if no data is available
    IF (NData .EQ. 0) RETURN
    
    CALL InFile%ReadData(FeatureData%cDataNames,iStat)         ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FeatureData%iDataComponentIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FeatureData%lDataIsBudgetType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FeatureData%cFilesForData,iStat)      ;  IF (iStat .EQ. -1) RETURN    
    
    CALL SubData_ReadFromFile(FeatureData%SubData , InFile ,iStat)
    IF (iStat .EQ. -1) RETURN

    CALL LocationsWithData_ReadFromFile(FeatureData%LocationsWithData , InFile ,iStat)
  
  END SUBROUTINE DataForFeature_ReadFromFile
  
  
  
  
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
  ! --- PRINT SUB-DATA
  ! -------------------------------------------------------------
  SUBROUTINE SubData_PrintToFile(SubData,OutFile)
    TYPE(SubDataType),INTENT(IN) :: SubData(:)
    TYPE(GenericFileType)        :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(SubData)
        CALL OutFile%WriteData(SubData(indx)%NSubData)
        CALL OutFile%WriteData(SubData(indx)%cSubDataNames)
    END DO
    
  END SUBROUTINE SubData_PrintToFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT LOCATIONS WITH DATA
  ! -------------------------------------------------------------
  SUBROUTINE LocationsWithData_PrintToFile(LocationsWithData,OutFile)
    TYPE(LocationsWithDataType),INTENT(IN) :: LocationsWithData(:)
    TYPE(GenericFileType)                  :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(LocationsWithData)
        CALL OutFile%WriteData(LocationsWithData(indx)%NLocations)
        CALL OutFile%WriteData(LocationsWithData(indx)%iLocations)
    END DO
    
  END SUBROUTINE LocationsWithData_PrintToFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT DATA FOR A FEATURE TO FILE
  ! -------------------------------------------------------------
  SUBROUTINE DataForFeature_PrintToFile(FeatureData,OutFile)
    CLASS(DataForFeatureType),INTENT(IN) :: FeatureData
    TYPE(GenericFileType)                :: OutFile
    
    CALL OutFile%WriteData(FeatureData%NData)
    CALL OutFile%WriteData(FeatureData%cDataNames)
    CALL OutFile%WriteData(FeatureData%iDataComponentIDs)
    CALL OutFile%WriteData(FeatureData%lDataIsBudgetType)
    CALL OutFile%WriteData(FeatureData%cFilesForData)
    
    CALL SubData_PrintToFile(FeatureData%SubData , OutFile)
    
    CALL LocationsWIthData_PrintToFile(FeatureData%LocationsWithData , OutFile)
    
  END SUBROUTINE DataForFeature_PrintToFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT MODEL DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintModelData(cSIMWorkingDirectory,AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,TimeStep,NTIME,iStat)
    CHARACTER(LEN=*),INTENT(IN)            :: cSIMWorkingDirectory
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(GWZBudgetType),INTENT(IN)         :: GWZBudget
    TYPE(RootZoneType),INTENT(IN)          :: RootZone
    TYPE(AppUnsatZoneType),INTENT(IN)      :: AppUnsatZone
    TYPE(AppLakeType),INTENT(IN)           :: AppLake
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(IN)                     :: NTIME
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(GenericFileType)    :: ModelDataFile
    
    !Initialize
    iStat = 0
    
    !Convert any text/DSS file output to HDF
    CALL AppGW%TransferOutputToHDF(TimeStep,NTIME,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL AppStream%TransferOutputToHDF(TimeStep,NTIME,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Open data file
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    CALL ModelDataFile%New(cFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='data file for simplified model for inquiry',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Write time-related data
    CALL ModelDataFile%WriteData(TimeStep%TrackTime)
    CALL ModelDataFile%WriteData(TimeStep%CurrentDateAndTime)
    CALL ModelDataFile%WriteData(TimeStep%EndDateAndTime)
    CALL ModelDataFile%WriteData(TimeStep%DeltaT)
    CALL ModelDataFile%WriteData(TimeStep%DeltaT_InMinutes)
    CALL ModelDataFile%WriteData(TimeStep%Unit)
    CALL ModelDataFile%WriteData(NTIME)
    
    !Write structural data
    CALL ModelDataFile%WriteData(AppUnsatZone%GetNLayers())  
    CALL ModelDataFile%WriteData(AppGW%GetNDrain())  
    CALL ModelDataFile%WriteData(AppSWShed%GetNSmallWatersheds())
    CALL ModelDataFile%WriteData(AppGW%GetNHydrographs(iLocationType_GWHeadObs))  
    CALL ModelDataFile%WriteData(AppStream%GetNHydrographs())  
    CALL ModelDataFile%WriteData(AppGW%GetNHydrographs(iLocationType_SubsidenceObs))  
    CALL ModelDataFile%WriteData(AppGW%GetNHydrographs(iLocationType_TileDrain))      
    
    !Data for nodes
    CALL CompileAndPrintFeatureData(iLocationType_Node)

    !Data for zones
    CALL CompileAndPrintFeatureData(iLocationType_Zone)
    
    !Data for subregions
    CALL CompileAndPrintFeatureData(iLocationType_Subregion)
    
    !Data for lakes
    CALL CompileAndPrintFeatureData(iLocationType_Lake)

    !Data for stream nodes
    CALL CompileAndPrintFeatureData(iLocationType_StrmNode)

    !Data for stream reaches
    CALL CompileAndPrintFeatureData(iLocationType_StrmReach)

    !Data for small watersheds
    CALL CompileAndPrintFeatureData(iLocationType_SmallWatershed)

    !Data for gw hydrographs
    CALL CompileAndPrintFeatureData(iLocationType_GWHeadObs)

    !Data for stream hydrographs
    CALL CompileAndPrintFeatureData(iLocationType_StrmHydObs)

    !Data for subsidence hydrographs
    CALL CompileAndPrintFeatureData(iLocationType_SubsidenceObs)

    !Data for tile drain hydrographs
    CALL CompileAndPrintFeatureData(iLocationType_TileDrain)
    
    !Close file
    CALL ModelDataFile%Kill()
  
    
  CONTAINS
  
  
    !########################################################
    !### COMPILE FEATURE DATA AND PRINT IT TO MODEL DATA FILE
    !########################################################
    SUBROUTINE CompileAndPrintFeatureData(iLocationType)
      INTEGER,INTENT(IN) :: iLocationType

      !Local variables
      INTEGER                  :: indx,iLocationID
      TYPE(DataForFeatureType) :: FeatureData
      
      CALL FeatureData%Kill()
      CALL GetDataList_AtLocationType_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,FeatureData%cDataNames,FeatureData%cFilesForData,FeatureData%iDataComponentIDs,FeatureData%lDataIsBudgetType,FeatureData%LocationsWithData)
      IF (ALLOCATED(FeatureData%cDataNames)) FeatureData%NData = SIZE(FeatureData%cDataNames)
      ALLOCATE (FeatureData%SubData(FeatureData%NData))
      DO indx=1,FeatureData%NData
          iLocationID = FeatureData%LocationsWithData(indx)%iLocations(1)
          IF (iLocationID .EQ. iAllLocationIDsListed) iLocationID = 1
          CALL GetSubDataList_AtLocation_FromFullModel(AppGW,GWZBudget,RootZone,AppUnsatZone,AppLake,AppStream,AppSWShed,iLocationType,iLocationID,FeatureData%iDataComponentIDs(indx),FeatureData%cDataNames(indx),FeatureData%SubData(indx)%cSubDataNames)
          IF (ALLOCATED(FeatureData%SubData(indx)%cSubDataNames)) THEN
              FeatureData%SubData(indx)%NSubData = SIZE(FeatureData%SubData(indx)%cSubDataNames)
          END IF
      END DO
      CALL FeatureData%PrintToFile(ModelDataFile)
      
    END SUBROUTINE CompileAndPrintFeatureData
    
  END SUBROUTINE PrintModelData
  
  
  
  
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
  ! --- DOES A LOCATION HAVE SPECIFIED DATA?
  ! -------------------------------------------------------------
  FUNCTION DoesLocationHaveData(iLocationID,LocationsWithData) RESULT(DoesHave)
    INTEGER,INTENT(IN)                     :: iLocationID
    TYPE(LocationsWithDataType),INTENT(IN) :: LocationsWithData
    LOGICAL                                :: DoesHave
    
    !Initialize
    DoesHave = .FALSE.
    
    IF (LocationsWithData%NLocations .EQ. 1) THEN
        IF (LocationsWithData%iLocations(1) .EQ. iAllLocationIDsListed) THEN
            DoesHave = .TRUE.
        ELSEIF (LocationsWithData%iLocations(1) .EQ. iLocationID) THEN
            DoesHave = .TRUE.
        END IF
    ELSE
        IF (LocateInList(iLocationID,LocationsWithData%iLocations)) THEN
            DoesHave = .TRUE.
        END IF
    END IF
    
  END FUNCTION DoesLocationHaveData
  
  
  ! -------------------------------------------------------------
  ! --- IS DATA FILE AVAILABLE
  ! -------------------------------------------------------------
  FUNCTION IsInstantiableFromFile(cSIMWorkingDirectory) RESULT(IsInstantiable)
    CHARACTER(LEN=*),INTENT(IN) :: cSIMWorkingDirectory
    LOGICAL                     :: IsInstantiable
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Absoulte pathname for file
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    
    !Is the file available?
    IsInstantiable = DoesFileExist(cFileName)
    
  END FUNCTION IsInstantiableFromFile
  
  
  ! -------------------------------------------------------------
  ! --- DELETE DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE DeleteDataFile(cSIMWorkingDirectory)
    CHARACTER(LEN=*),INTENT(IN) :: cSIMWorkingDirectory
    
    !Local variables
    INTEGER                  :: iStat
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(GenericFileType)    :: ModelDataFile
    
    !Absolute pathname for file
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    
    !Delete file
    CALL ModelDataFile%New(cFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='data file for simplified model for inquiry',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    CALL ModelDataFile%Kill(Status='DELETE')
    
  END SUBROUTINE DeleteDataFile   
          
END MODULE