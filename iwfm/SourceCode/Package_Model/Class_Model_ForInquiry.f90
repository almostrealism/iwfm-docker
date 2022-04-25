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
MODULE Class_Model_ForInquiry
  USE MessageLogger             , ONLY: SetLastMessage                   , &
                                        f_iFatal  
  USE GeneralUtilities          , ONLY: EstablishAbsolutePathFileName    , &
                                        LocateInList                     , &
                                        IntToText                        , &
                                        UpperCase                        , &
                                        StripTextUntilCharacter          
  USE TimeSeriesUtilities       , ONLY: TimeStepType                     , &
                                        ExtractMonth                     , &
                                        ExtractYear                      , &
                                        DayMonthYearToJulianDate         , &
                                        JulianDateAndMinutesToTimeStamp  , &
                                        CTimeStep_To_RTimeStep           , &
                                        NPeriods                         , &
                                        IncrementTimeStamp               , &
                                        OPERATOR(.TSLT.)                 , &
                                        OPERATOR(.TSGT.)                 , &
                                        f_iTimeStampLength              
  USE IOInterface               , ONLY: GenericFileType                  , &
                                        DoesFileExist                    , &
                                        iGetFileType_FromName            , &
                                        f_iTXT                           , &
                                        f_iHDF
  USE Package_Discretization    , ONLY: AppGridType                      , &
                                        StratigraphyType
  USE Package_Misc              , ONLY: RealTSDataInFileType             , &
                                        f_iAllLocationIDsListed          , &
                                        f_iLocationType_Node             , &
                                        f_iLocationType_Element          , &
                                        f_iLocationType_Zone             , &
                                        f_iLocationType_Subregion        , & 
                                        f_iLocationType_Lake             , & 
                                        f_iLocationType_StrmNode         , & 
                                        f_iLocationType_StrmReach        , & 
                                        f_iLocationType_SmallWatershed   , & 
                                        f_iLocationType_GWHeadObs        , &
                                        f_iLocationType_StrmHydObs       , &
                                        f_iLocationType_SubsidenceObs    , &
                                        f_iLocationType_TileDrainObs     , &
                                        f_iLocationType_Diversion        , &
                                        f_iLocationType_StrmNodeBud      , &
                                        f_iStrmComp                      , &
                                        f_iLakeComp                      , &
                                        f_iGWComp                        , &
                                        f_iRootZoneComp                  , &
                                        f_iUnsatZoneComp                 , &
                                        f_iSWShedComp                      
  USE Package_AppGW             , ONLY: AppGWType                        , &
                                        f_iBudgetType_GW                 , &
                                        f_cDescription_GWHyd_AtNodeLayer
  USE Package_GWZBudget         , ONLY: GWZBudgetType                    , & 
                                        f_iZBudgetType_GW                
  USE Package_AppStream         , ONLY: AppStreamType                    , &
                                        StrmNodeBudgetType               , &  
                                        f_iBudgetType_StrmNode           , &
                                        f_iBudgetType_StrmReach          , &
                                        f_iBudgetType_DiverDetail        
  USE Package_AppLake           , ONLY: AppLakeType                      , &
                                        f_iBudgetType_Lake    
  USE Package_AppUnsatZone      , ONLY: AppUnsatZoneType                 , &
                                        f_iBudgetType_UnsatZone          , &
                                        f_iZBudgetType_UnsatZone                 
  USE Package_AppSmallWatershed , ONLY: AppSmallWatershedType            , &
                                        f_iBudgetType_SWShed             
  USE Package_RootZone          , ONLY: RootZoneType                     , &
                                        f_iBudgetType_RootZone           , &
                                        f_iBudgetType_LWU                , &
                                        f_iBudgetType_NonPondedCrop_RZ   , &
                                        f_iBudgetType_NonPondedCrop_LWU  , &
                                        f_iBudgetType_PondedCrop_RZ      , &
                                        f_iBudgetType_PondedCrop_LWU     , &
                                        f_iZBudgetType_LWU               , &
                                        f_iZBudgetType_RootZone          
  USE Package_PrecipitationET   , ONLY: PrecipitationType
  USE Package_Budget            , ONLY: BudgetType                       , &
                                        f_iColumnHeaderLen
  USE Package_ZBudget           , ONLY: ZBudgetType                      , &
                                        ZoneListType                     , &
                                        IsZBudgetFile                    , &
                                        f_iColumnHeaderLen
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
            f_iFilePathLen           , &
            f_iDataDescriptionLen

  
  ! -------------------------------------------------------------
  ! --- PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iFilePathLen        = 500  , &
                       f_iDataDescriptionLen = 100
  
  
  ! -------------------------------------------------------------
  ! --- SIMPLIFIED MODEL DATA TYPE FOR INQUIRY
  ! -------------------------------------------------------------
  TYPE Model_ForInquiry_Type
      !AppGrid, Stratigraphy and TimeStep properties are stored in the Model object
      !AppStream and AppLake properties are stored in Model object
      !AppStream Reach IDs redefined here because they may be reordered differently than listed in Model object (Model object reads it from PP bin file, but that list can be reordered in Simulation)
      INTEGER                                          :: iNUnsatLayers          = 0
      INTEGER                                          :: iNSmallWatersheds      = 0
      INTEGER                                          :: iNTileDrains           = 0
      INTEGER                                          :: iNDiversions           = 0 
      INTEGER                                          :: iNGWHeadObs            = 0
      INTEGER                                          :: iNStrmHydObs           = 0
      INTEGER                                          :: iNSubsidenceObs        = 0
      INTEGER                                          :: iNTileDrainObs         = 0
      INTEGER                                          :: iNStrmNodes_WithBudget = 0 
      INTEGER,ALLOCATABLE                              :: iStrmReachIDs(:)
      INTEGER,ALLOCATABLE                              :: iDiversionIDs(:) 
      INTEGER,ALLOCATABLE                              :: iSmallWatershedIDs(:) 
      INTEGER,ALLOCATABLE                              :: iGWHeadObsIDs(:) 
      INTEGER,ALLOCATABLE                              :: iStrmHydObsIDs(:) 
      INTEGER,ALLOCATABLE                              :: iSubsidenceObsIDs(:) 
      INTEGER,ALLOCATABLE                              :: iTileDrainObsIDs(:) 
      INTEGER,ALLOCATABLE                              :: iStrmNodeIDs_WithBudget(:) 
      INTEGER                                          :: iNZBudgetFiles         = 0
      INTEGER,ALLOCATABLE                              :: iZBudgetTypeList(:)
      CHARACTER(LEN=f_iDataDescriptionLen),ALLOCATABLE :: cZBudgetDescriptions(:)
      CHARACTER(LEN=f_iFilePathLen),ALLOCATABLE        :: cZBudgetFiles(:)
      INTEGER                                          :: iNBudgetFiles          = 0
      INTEGER,ALLOCATABLE                              :: iBudgetTypeList(:)
      INTEGER,ALLOCATABLE                              :: iBudgetCompList(:)
      INTEGER,ALLOCATABLE                              :: iBudgetLocationTypeList(:)
      CHARACTER(LEN=f_iDataDescriptionLen),ALLOCATABLE :: cBudgetDescriptions(:)
      CHARACTER(LEN=f_iFilePathLen),ALLOCATABLE        :: cBudgetFiles(:)
      INTEGER                                          :: iNHydrographTypes      = 0
      INTEGER,ALLOCATABLE                              :: iHydrographLocationTypeList(:)
      INTEGER,ALLOCATABLE                              :: iHydrographCompList(:)
      CHARACTER(LEN=f_iDataDescriptionLen),ALLOCATABLE :: cHydrographDescriptions(:)
      CHARACTER(LEN=f_iFilePathLen),ALLOCATABLE        :: cHydrographFiles(:)
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetNHydrographTypes
      PROCEDURE,PASS   :: GetHydrographTypeList
      PROCEDURE,PASS   :: GetHydrograph
      PROCEDURE,PASS   :: GetBudget_N
      PROCEDURE,PASS   :: GetBudget_List
      PROCEDURE,PASS   :: GetBudget_NColumns
      PROCEDURE,PASS   :: GetBudget_ColumnTitles
      PROCEDURE,PASS   :: GetBudget_MonthlyAverageFlows
      PROCEDURE,PASS   :: GetBudget_AnnualFlows
      PROCEDURE,PASS   :: GetBudget_TSData
      PROCEDURE,PASS   :: GetBudget_CumGWStorChange
      PROCEDURE,PASS   :: GetBudget_AnnualCumGWStorChange
      PROCEDURE,PASS   :: GetZBudget_N
      PROCEDURE,PASS   :: GetZBudget_List
      PROCEDURE,PASS   :: GetZBudget_NColumns
      PROCEDURE,PASS   :: GetZBudget_ColumnTitles
      PROCEDURE,PASS   :: GetZBudget_MonthlyAverageFlows
      PROCEDURE,PASS   :: GetZBudget_AnnualFlows
      PROCEDURE,PASS   :: GetZBudget_TSData
      PROCEDURE,PASS   :: GetZBudget_CumGWStorChange
      PROCEDURE,PASS   :: GetZBudget_AnnualCumGWStorChange
      PROCEDURE,PASS   :: GetGWHeads_ForALayer
      PROCEDURE,PASS   :: GetNLocations
      PROCEDURE,PASS   :: GetLocationIDs
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
    INTEGER                  :: iNStrmNodes,iNZBudgets,iNBudgets,iNHydTypes 
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(GenericFileType)    :: ModelDataFile
    
    !Initialize
    iStat = 0
    
    !Open file to read model data for inquiry
    CALL EstablishAbsolutePathFileName(cModelDataFileName,cSIMWorkingDirectory,cFileName)
    CALL ModelDataFile%New(cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='data file for simplified model for inquiry',iStat=iStat)
    IF (iStat .EQ. -1) GOTO 10
    
    !Read time-related data
    CALL ModelDataFile%ReadData(TimeStep%TrackTime,iStat)                  ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%CurrentDateAndTime,iStat)         ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%EndDateAndTime,iStat)             ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%DeltaT,iStat)                     ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%DeltaT_InMinutes,iStat)           ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(TimeStep%Unit,iStat)                       ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(NTIME,iStat)                               ; IF (iStat .EQ. -1) GOTO 10
                                                                           
    !Read number of unsaturated zone layers and tile drains                
    CALL ModelDataFile%ReadData(Model%iNUnsatLayers,iStat)                 ; IF (iStat .EQ. -1) GOTO 10
    CALL ModelDataFile%ReadData(Model%iNTileDrains,iStat)                  ; IF (iStat .EQ. -1) GOTO 10

    !Read Stream node IDs with budget output
    CALL ModelDataFile%ReadData(Model%iNStrmNodes_WithBudget,iStat)        ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iStrmNodeIDs_WithBudget(Model%iNStrmNodes_WithBudget))                      
    CALL ModelDataFile%ReadData(Model%iStrmNodeIDs_WithBudget,iStat)       ; IF (iStat .EQ. -1) GOTO 10
    
    !Read stream reach IDs
    CALL ModelDataFile%ReadData(iNStrmNodes,iStat)                         ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iStrmReachIDs(iNStrmNodes))                           
    CALL ModelDataFile%ReadData(Model%iStrmReachIDs,iStat)                 ; IF (iStat .EQ. -1) GOTO 10
                                                                          
    !Read diversion IDs                                                   
    CALL ModelDataFile%ReadData(Model%iNDiversions,iStat)                  ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iDiversionIDs(Model%iNDiversions))                           
    CALL ModelDataFile%ReadData(Model%iDiversionIDs,iStat)                 ; IF (iStat .EQ. -1) GOTO 10
                                                                          
    !Read stream hydrograph IDs                                           
    CALL ModelDataFile%ReadData(Model%iNStrmHydObs,iStat)                  ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iStrmHydObsIDs(Model%iNStrmHydObs))                           
    CALL ModelDataFile%ReadData(Model%iStrmHydObsIDs,iStat)                ; IF (iStat .EQ. -1) GOTO 10
                                                                          
    !Read small watershed IDs                                             
    CALL ModelDataFile%ReadData(Model%iNSmallWatersheds,iStat)             ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iSmallWatershedIDs(Model%iNSmallWatersheds))                            
    CALL ModelDataFile%ReadData(Model%iSmallWatershedIDs,iStat)            ; IF (iStat .EQ. -1) GOTO 10

    !Read gw hydrograph IDs
    CALL ModelDataFile%ReadData(Model%iNGWHeadObs,iStat)                   ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iGWHeadObsIDs(Model%iNGWHeadObs))                            
    CALL ModelDataFile%ReadData(Model%iGWHeadObsIDs,iStat)                 ; IF (iStat .EQ. -1) GOTO 10
                                                                          
    !Read subsidence hydrograph IDs                                       
    CALL ModelDataFile%ReadData(Model%iNSubsidenceObs,iStat)               ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iSubsidenceObsIDs(Model%iNSubsidenceObs))                            
    CALL ModelDataFile%ReadData(Model%iSubsidenceObsIDs,iStat)             ; IF (iStat .EQ. -1) GOTO 10
                                                                          
    !Read tiledrain hydrograph IDs                                        
    CALL ModelDataFile%ReadData(Model%iNTileDrainObs,iStat)                ; IF (iStat .EQ. -1) GOTO 10
    ALLOCATE (Model%iTileDrainObsIDs(Model%iNTileDrainObs))                            
    CALL ModelDataFile%ReadData(Model%iTileDrainObsIDs,iStat)              ; IF (iStat .EQ. -1) GOTO 10
                                
    !Read Z-Budget related data
    CALL ModelDataFile%ReadData(iNZBudgets,iStat)                     ; IF (iStat .EQ. -1) GOTO 10
    Model%iNZBudgetFiles = iNZBudgets
    IF (iNZBudgets .GT. 0) THEN
        ALLOCATE (Model%iZBudgetTypeList(iNZBudgets) , Model%cZBudgetDescriptions(iNZBudgets) , Model%cZBudgetFiles(iNZBudgets))
        Model%cZBudgetDescriptions = ''
        Model%cZBudgetFiles        = ''
        CALL ModelDataFile%ReadData(Model%iZBudgetTypeList,iStat)     ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%cZBudgetDescriptions,iStat) ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%cZBudgetFiles,iStat)        ; IF (iStat .EQ. -1) GOTO 10
    END IF
        
    !Read Budget related data
    CALL ModelDataFile%ReadData(iNBudgets,iStat)                     ; IF (iStat .EQ. -1) GOTO 10
    Model%iNBudgetFiles = iNBudgets
    IF (iNBudgets .GT. 0) THEN
        ALLOCATE (Model%iBudgetTypeList(iNBudgets) , Model%iBudgetCompList(iNBudgets) , Model%iBudgetLocationTypeList(iNBudgets) , Model%cBudgetDescriptions(iNBudgets) , Model%cBudgetFiles(iNBudgets))
        Model%cBudgetDescriptions = ''
        Model%cBudgetFiles        = ''
        CALL ModelDataFile%ReadData(Model%iBudgetTypeList,iStat)         ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%iBudgetCompList,iStat)         ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%iBudgetLocationTypeList,iStat) ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%cBudgetDescriptions,iStat)     ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%cBudgetFiles,iStat)            ; IF (iStat .EQ. -1) GOTO 10
    END IF
    
    !Read Hydrograph related data                                         
    CALL ModelDataFile%ReadData(iNHydTypes,iStat)                          ; IF (iStat .EQ. -1) GOTO 10
    Model%iNHydrographTypes = iNHydTypes
    IF (iNHydTypes .GT. 0) THEN
        ALLOCATE (Model%iHydrographLocationTypeList(iNHydTypes) , Model%iHydrographCompList(iNHydTypes) , Model%cHydrographDescriptions(iNHydTypes) , Model%cHydrographFiles(iNHydTypes))
        Model%cHydrographDescriptions = ''
        Model%cHydrographFiles        = ''
        CALL ModelDataFile%ReadData(Model%iHydrographLocationTypeList,iStat)  ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%iHydrographCompList,iStat)          ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%cHydrographDescriptions,iStat)      ; IF (iStat .EQ. -1) GOTO 10
        CALL ModelDataFile%ReadData(Model%cHydrographFiles,iStat)             ; IF (iStat .EQ. -1) GOTO 10 
    END IF
    
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
  ! --- KILL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Model)
    CLASS(Model_ForInquiry_Type) :: Model
    
    !Local variables
    INTEGER                     :: iErrorCode 
    TYPE(Model_ForInquiry_Type) :: Dummy
    
    DEALLOCATE (Model%iStrmReachIDs      , &
                Model%iDiversionIDs      , &
                Model%iSmallWatershedIDs , &
                Model%iGWHeadObsIDs      , &
                Model%iStrmHydObsIDs     , &
                Model%iSubsidenceObsIds  , &
                Model%iTileDrainObsIDs   , &
                STAT=iErrorCode          )
    
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
  ! --- GET NUMBER OF AVAILABLE HYDROGRAPH OUTPUT TYPES FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographTypes(Model) RESULT(iNHydTypes)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER                                 :: iNHydTypes
    
    iNHydTypes = Model%iNHydrographTypes

  END FUNCTION GetNHydrographTypes
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF HYDROGRAPH OUTPUT TYPES FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographTypeList(Model,cHydTypeList,iHydLocationTypeList)
    CLASS(Model_ForInquiry_Type),INTENT(IN)  :: Model
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cHydTypeList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iHydLocationTypeList(:)
    
    !Local variables
    INTEGER :: iNHydTypes
    
    !If not allocated, allocate list array
    IF (.NOT. ALLOCATED(cHydTypeList)) THEN
        iNHydTypes = Model%iNHydrographTypes
        ALLOCATE (cHydTypeList(iNHydTypes) , iHydLocationTypeList(iNHydTypes))
    END IF
    
    cHydTypeList         = Model%cHydrographDescriptions
    iHydLocationTypeList = Model%iHydrographLocationTypeList

  END SUBROUTINE GetHydrographTypeList
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH FOR A GIVEN HYDROGRAPH INDEX FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrograph(Model,TimeStep,iNNodes,iLocationType,iLocationIndex,iLayer,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    TYPE(TimeStepType),INTENT(IN)           :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)             :: cBeginDate,cEndDate,cInterval
    INTEGER,INTENT(IN)                      :: iLocationType,iLocationIndex,iLayer,iNNodes
    REAL(8),INTENT(IN)                      :: rFact_LT,rFact_VL
    REAL(8),ALLOCATABLE,INTENT(OUT)         :: rDates(:),rValues(:)
    INTEGER,INTENT(OUT)                     :: iDataUnitType,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'GetHydrograph'
    INTEGER                      :: iLoc
    TYPE(AppGWType)              :: AppGW
    TYPE(AppStreamType)          :: AppStream
    
    !Find the hydrograph information
    iLoc = LocateInList(iLocationType,Model%iHydrographLocationTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Requested hydrograph data is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT CASE (Model%iHydrographCompList(iLoc))
        CASE (f_iGWComp) 
            CALL AppGW%GetHydrograph(TRIM(Model%cHydrographFiles(iLoc)),TimeStep,iNNodes,iLocationType,iLocationIndex,iLayer,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat)
                    
        CASE (f_iStrmComp)
            CALL AppStream%GetHydrograph(TRIM(Model%cHydrographFiles(iLoc)),TimeStep,iLocationIndex,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat)
            
    END SELECT
            
  END SUBROUTINE GetHydrograph
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE BUDGETS FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  FUNCTION GetBudget_N(Model) RESULT(iNBudgets)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER                                 :: iNBudgets
    
    iNBudgets = Model%iNBudgetFiles
    
  END FUNCTION GetBudget_N
  
  
  ! -------------------------------------------------------------
  ! --- GET LIST OF BUDGET OUTPUTS FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(Model,cBudgetList,iBudgetTypeList,iBudgetCompList,iBudgetLocationTypeList)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)       :: cBudgetList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)                :: iBudgetTypeList(:),iBudgetCompList(:),iBudgetLocationTypeList(:)
    
    !Local variables
    INTEGER :: iNBudgets

    !Number of budgets
    iNBudgets = Model%iNBudgetFiles
    
    !Allocate arrays
    ALLOCATE (cBudgetList(iNBudgets) , iBudgetTypeList(iNBudgets) , iBudgetCompList(iNBudgets) , iBudgetLocationTypeList(iNBudgets))
    
    !Retrieve data
    cBudgetList             = Model%cBudgetDescriptions
    iBudgetTypeList         = Model%iBudgetTypeList
    iBudgetCompList         = Model%iBudgetCompList
    iBudgetLocationTypeList = Model%iBudgetLocationTypeList
    
  END SUBROUTINE GetBudget_List
    

  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS (EXCLUDING TIME COLUMN) FOR A BUDGET FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(Model,iBudgetType,iLocationIndex,iNCols,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iBudgetType,iLocationIndex
    INTEGER,INTENT(OUT)                     :: iNCols,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'GetBudget_NColumns'
    INTEGER                      :: iLoc
    TYPE(BudgetType)             :: Budget
    
    !Locate budget in list
    iLoc = LocateInList(iBudgetType,Model%iBudgetTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Specified budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL Budget%New(Model%cBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Number of columns (includes Time column)
    CALL Budget%GetNDataColumns(iLocationIndex,iNCols,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Substract 1 to exclude Time column
    iNCols = iNCols - 1

    !Close file
    CALL Budget%Kill()
    
  END SUBROUTINE GetBudget_NColumns
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN TITLES FOR A BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(Model,iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                       :: iBudgetType,iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL 
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22)                  :: ThisProcedure = ModName // 'GetBudget_ColumnTitles'
    INTEGER                                       :: iLoc,iNCols,iErrorCode
    TYPE(BudgetType)                              :: Budget
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    !Locate budget in list
    iLoc = LocateInList(iBudgetType,Model%iBudgetTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Specified budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL Budget%New(Model%cBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Number of columns (includes Time column)
    CALL Budget%GetNDataColumns(iLocationIndex,iNCols,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the columns headers (includes Time column)
    ALLOCATE (cColTitles_Local(iNCols))
    cColTitles_Local = Budget%GetFullColumnHeaders(iLocationIndex,iNCols)
    
    !Insert units into titles
    CALL Budget%ModifyFullColumnHeaders(cUnitLT,cUnitAR,cUnitVL,cColTitles_Local)
    
    !Remove time column
    iNCols = iNCols - 1
    ALLOCATE (cColTitles(iNCols))
    cColTitles = ADJUSTL(cColTitles_Local(2:))
    
    !Close Budget file
    CALL Budget%Kill()
    
    !Clear memory
    DEALLOCATE (cColTitles_Local , STAT=iErrorCode)
                
  END SUBROUTINE GetBudget_ColumnTitles
  
  
  ! -------------------------------------------------------------
  ! --- GET AVERAGE MONTHLY BUDGET OUTPUTS, AND ERROR BOUNDS OF THE FLOWS FROM THE INQUIRY MODEL
  ! --- Note: Budget class requires feature ID, instead of index. So, index is converted to ID
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyAverageFlows(Model,iSubregionIDs,iLakeIDs,iBudgetType,iLocationIndex,iLUType,iSWShedBudType,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows,rSDFlows,cFlowNames,iStat)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    INTEGER,INTENT(IN)                             :: iSubregionIDs(:),iLakeIDs(:),iBudgetType,iLocationIndex,iLUType,iSWShedBudType
    CHARACTER(LEN=*),INTENT(IN)                    :: cAdjustedBeginDate,cAdjustedEndDate
    REAL(8),INTENT(IN)                             :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)                :: rFlows(:,:),rSDFlows(:,:)   !Flows are return in (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)       :: cFlowNames(:)
    INTEGER,INTENT(OUT)                            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+29) :: ThisProcedure = ModName // 'GetBudget_MonthlyAverageFlows'
    INTEGER                      :: iMonth,iNCols,indxCol,indxTime,iNYears,iLoc,ID
    REAL(8)                      :: rDiff   
    REAL(8),ALLOCATABLE          :: rFlowsWork(:,:)
    TYPE(RootZoneType)           :: RootZone
    TYPE(AppStreamType)          :: AppStream
    TYPE(AppGWType)              :: AppGW
    TYPE(AppUnsatZoneType)       :: AppUnsatZone
    TYPE(AppLakeType)            :: AppLake
    TYPE(AppSmallWatershedType)  :: AppSWShed
    TYPE(BudgetType)             :: Budget
    
    !Locate budget in list
    iLoc = LocateInList(iBudgetType,Model%iBudgetTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Specified budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL Budget%New(Model%cBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
            
    !Get monthly budget flows
    SELECT CASE(Model%iBudgetCompList(iLoc))
        CASE (f_iGWComp)
            ID = iSubregionIDs(iLocationIndex)
            CALL AppGW%GetBudget_MonthlyFlows(Budget,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iStrmComp)
            !Convert index to ID
            SELECT CASE (iBudgetType)
                CASE (f_iBudgetType_StrmReach) 
                    ID = Model%iStrmReachIDs(iLocationIndex)
                CASE (f_iBudgetType_StrmNode)
                    ID = Model%iStrmNodeIDs_WithBudget(iLocationIndex)
                CASE (f_iBudgetType_DiverDetail)
                    ID = Model%iDiversionIDs(iLocationIndex)
            END SELECT
            CALL AppStream%GetBudget_MonthlyFlows(Budget,iBudgetType,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iRootZoneComp)
            ID = iSubregionIDs(iLocationIndex)
            CALL RootZone%GetBudget_MonthlyFlows(Budget,iBudgetType,iLUType,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iUnsatZoneComp)
            ID = iSubregionIDs(iLocationIndex)
            CALL AppUnsatZone%GetBudget_MonthlyFlows(Budget,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iLakeComp)
            ID = iLakeIDs(iLocationIndex)
            CALL AppLake%GetBudget_MonthlyFlows(Budget,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iSWShedComp)
            ID = Model%iSmallWatershedIDs(iLocationIndex)
            CALL AppSWShed%GetBudget_MonthlyFlows(Budget,iSWShedBudType,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
    END SELECT
    IF (iStat .NE. 0) RETURN
        
    !Compute average flows
    iNCols = SIZE(cFlowNames)
    ALLOCATE (rFlows(iNCols,12))
    rFlows  = 0.0
    iMonth  = 1
    iNYears = 0
    DO indxTime=1,SIZE(rFlowsWork,DIM=2)
        DO indxCol=1,iNCols
            rFlows(indxCol,iMonth) = rFlows(indxCol,iMonth) + rFlowsWork(indxCol,indxTime)
        END DO
        iMonth = iMonth + 1
        IF (iMonth .EQ. 13) THEN
            iMonth = 1
            iNYears = iNYears + 1
        END IF
    END DO
    rFlows = rFlows / REAL(iNYears , 8)
    
    !Compute error bounds
    ALLOCATE (rSDFlows(iNCols,12))
    rSDFlows = 0.0
    iMonth   = 1
    DO indxTime=1,SIZE(rFlowsWork,DIM=2)
        DO indxCol=1,iNCols
            rDiff                    = rFlowsWork(indxCol,indxTime) - rFlows(indxCol,iMonth) 
            rSDFlows(indxCol,iMonth) = rSDFlows(indxCol,iMonth) + rDiff * rDiff
        END DO
        iMonth = iMonth + 1
        IF (iMonth .EQ. 13) iMonth = 1
    END DO
    rSDFlows = SQRT(rSDFlows / REAL(iNYears , 8))
           
    !Close Budget file
    CALL Budget%Kill()
    
  END SUBROUTINE GetBudget_MonthlyAverageFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET ANNUAL BUDGET OUTPUTS FROM THE INQUIRY MODEL
  ! --- Note: Budget class requires feature ID, instead of index. So, index is converted to ID
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_AnnualFlows(Model,iSubregionIDs,iLakeIDs,iBudgetType,iLocationIndex,iLUType,iSWShedBudType,iNYears,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    INTEGER,INTENT(IN)                             :: iSubregionIDs(:),iLakeIDs(:),iBudgetType,iLocationIndex,iLUType,iSWShedBudType,iNYears
    CHARACTER(LEN=*),INTENT(IN)                    :: cAdjustedBeginDate,cAdjustedEndDate
    REAL(8),INTENT(IN)                             :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)                :: rFlows(:,:)    !rFlows is in (column,WY) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)       :: cFlowNames(:)
    INTEGER,INTENT(OUT)                            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'GetBudget_AnnualFlows'
    INTEGER                      :: iMonth,iYear,iNCols,indxTime,iLoc,ID
    REAL(8),ALLOCATABLE          :: rFlowsWork(:,:)
    TYPE(RootZoneType)           :: RootZone
    TYPE(AppStreamType)          :: AppStream
    TYPE(AppGWType)              :: AppGW
    TYPE(AppUnsatZoneType)       :: AppUnsatZone
    TYPE(AppSmallWatershedType)  :: AppSWShed
    TYPE(AppLakeType)            :: AppLake
    TYPE(BudgetType)             :: Budget
    
    !Locate budget in list
    iLoc = LocateInList(iBudgetType,Model%iBudgetTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Specified budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL Budget%New(Model%cBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get monthly budget flows
    SELECT CASE(Model%iBudgetCompList(iLoc))
        CASE (f_iGWComp)
            ID = iSubregionIDs(iLocationIndex)
            CALL AppGW%GetBudget_MonthlyFlows(Budget,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iStrmComp)
            SELECT CASE (iBudgetType)
                CASE (f_iBudgetType_StrmReach) 
                    ID = Model%iStrmReachIDs(iLocationIndex)
                CASE (f_iBudgetType_StrmNode)
                    ID = Model%iStrmNodeIDs_WithBudget(iLocationIndex)
                CASE (f_iBudgetType_DiverDetail)
                    ID = Model%iDiversionIDs(iLocationIndex)
            END SELECT
            CALL AppStream%GetBudget_MonthlyFlows(Budget,iBudgetType,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iLakeComp)
            ID = iLakeIDs(iLocationIndex)
            CALL AppLake%GetBudget_MonthlyFlows(Budget,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
                        
        CASE (f_iRootZoneComp)
            ID =iSubregionIDs(iLocationIndex)
            CALL RootZone%GetBudget_MonthlyFlows(Budget,iBudgetType,iLUType,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iUnsatZoneComp)
            ID =iSubregionIDs(iLocationIndex)
            CALL AppUnsatZone%GetBudget_MonthlyFlows(Budget,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
        CASE (f_iSWShedComp)
            ID = Model%iSmallWatershedIDs(iLocationIndex)
            CALL AppSWShed%GetBudget_MonthlyFlows(Budget,iSWShedBudType,ID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlowsWork,cFlowNames,iStat)
            
    END SELECT
    IF (iStat .NE. 0) RETURN
        
    !Compute annual flows
    iNCols = SIZE(cFlowNames)
    ALLOCATE (rFlows(iNCols,iNYears))
    rFlows = 0.0
    iMonth = 1
    iYear  = 1
    DO indxTime=1,SIZE(rFlowsWork,DIM=2)
        rFlows(:,iYear) = rFlows(:,iYear) + rFlowsWork(:,indxTime)
        iMonth          = iMonth + 1
        IF (iMonth .GT. 12) THEN
            iMonth = 1
            iYear  = iYear + 1
        END IF
    END DO
           
    !Close Budget file
    CALL Budget%Kill()
    
  END SUBROUTINE GetBudget_AnnualFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FROM A BUDGET FILE FOR A SET OF COLUMNS FROM THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(Model,iSubregionIDs,iLakeIDs,iBudgetType,iLocationIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iSubregionIDs(:),iLakeIDs(:),iBudgetType,iLocationIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)             :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                      :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                     :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                     :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'GetBudget_TSData'
    INTEGER                      :: indx,iLoc,ID
    TYPE(BudgetType)             :: Budget
    
    !Locate budget in list
    iLoc = LocateInList(iBudgetType,Model%iBudgetTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Specified budget is not part of the model output to retrieve data from!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL Budget%New(Model%cBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Convert location index to ID since Budget expects ID
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_GW , f_iBudgetType_UnsatZone , f_iBudgetType_RootZone , f_iBudgetType_LWU)
            ID = iSubregionIDs(iLocationIndex)
        CASE (f_iBudgetType_StrmReach) 
            ID = Model%iStrmReachIDs(iLocationIndex)
        CASE (f_iBudgetType_StrmNode)
            ID = Model%iStrmNodeIDs_WithBudget(iLocationIndex)
        CASE (f_iBudgetType_DiverDetail)
            ID = Model%iDiversionIDs(iLocationIndex)
        CASE (f_iBudgetType_Lake)
            ID = iLakeIDs(iLocationIndex)
        CASE (f_iBudgetType_SWShed)
            ID = Model%iSmallWatershedIDs(iLocationIndex)
        CASE DEFAULT 
            ID = iLocationIndex 
    END SELECT
    
    !Read data
    DO indx=1,SIZE(iCols)
        CALL Budget%ReadData(ID,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
    END DO
    
    !Close Budget file
    CALL Budget%Kill()
    
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE GW STORAGE CHANGE FROM BUDGET FILE FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_CumGWStorChange(Model,iSubregionID,cBeginDate,cEndDate,cInterval,rFactVL,rOutputDates,rCumGWStorChange,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iSubregionID
    CHARACTER(LEN=*),INTENT(IN)             :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                      :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)         :: rOutputDates(:),rCumGWStorChange(:)    
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+25) :: ThisProcedure = ModName // 'GetBudget_CumGWStorChange'
    INTEGER                      :: iLoc
    TYPE(BudgetType)             :: Budget
    TYPE(AppGWType)              :: AppGW
    
    !Locate budget in list
    iLoc = LocateInList(f_iBudgetType_GW,Model%iBudgetTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Groundwater budget is not part of the model output to retrieve cumulative chnage in storage!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL Budget%New(Model%cBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
        
    !Get the data
    CALL AppGW%GetBudget_CumGWSTorChange(Budget,iSubregionID,cBeginDate,cEndDate,cInterval,rFactVL,rOutputDates,rCumGWStorChange,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Close Budget file
    CALL Budget%Kill()
    
  END SUBROUTINE GetBudget_CumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE GW STORAGE CHANGE FROM BUDGET FILE FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_AnnualCumGWStorChange(Model,iSubregionIndex,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rCumGWStorChange,iStat)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    INTEGER,INTENT(IN)                             :: iSubregionIndex
    CHARACTER(LEN=*),INTENT(IN)                    :: cAdjustedBeginDate,cAdjustedEndDate
    REAL(8),INTENT(IN)                             :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)                :: rCumGWStorChange(:)  
    INTEGER,INTENT(OUT)                            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31)      :: ThisProcedure = ModName // 'GetBudget_AnnualCumGWStorChange'
    INTEGER                           :: iLoc,iErrorCode   
    TYPE(BudgetType)                  :: Budget
    TYPE(AppGWType)                   :: AppGW
    REAL(8),ALLOCATABLE               :: rDates(:)
    
    !Locate budget in list
    iLoc = LocateInList(f_iBudgetType_GW,Model%iBudgetTypeList)
    IF (iLoc .LT. 1) THEN
        CALL SetLastMessage('Groundwater budget is not part of the model output to retrieve cumulative chnage in storage!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL Budget%New(Model%cBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the data
    CALL AppGW%GetBudget_CumGWStorChange(Budget,iSubregionIndex,cAdjustedBeginDate,cAdjustedEndDate,'1YEAR',rFactVL,rDates,rCumGWStorChange,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Close Budget file
    CALL Budget%Kill()
    
    !Clear memory
    DEALLOCATE (rDates , STAT=iErrorCode)
    
  END SUBROUTINE GetBudget_AnnualCumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE ZBUDGETS FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  FUNCTION GetZBudget_N(Model) RESULT(iNZBudgets)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER                                 :: iNZBudgets
    
    iNZBudgets = Model%iNZBudgetFiles
    
  END FUNCTION GetZBudget_N
  
  
  ! -------------------------------------------------------------
  ! --- GET NAMES OF ZBUDGET OUTPUTS FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_List(Model,cZBudgetList,iZBudgetTypeList)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)       :: cZBudgetList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)                :: iZBudgetTypeList(:)
    
    ALLOCATE (cZBudgetList(Model%iNZBudgetFiles) , iZBudgetTypeList(Model%iNZBudgetFiles))
    cZBudgetList     = Model%cZBudgetDescriptions
    iZBudgetTypeList = Model%iZBudgetTypeList

  END SUBROUTINE GetZBudget_List
    

  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS (EXCLUDING TIME COLUMN) FOR A ZONE BUDGET GIVEN ZONE ID FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_NColumns(Model,iZBudgetType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,iNCols,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iZBudgetType,iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    INTEGER,INTENT(OUT)                     :: iNCols,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'GetZBudget_NColumns' 
    TYPE(ZBudgetType)            :: ZBudget
    TYPE(ZoneListType)           :: ZoneList
    INTEGER                      :: iZonesWithNames(0),indx,iLoc
    INTEGER,ALLOCATABLE          :: iColumnList(:) 
    CHARACTER(LEN=1)             :: cZoneNames(0)
    CHARACTER(LEN=1),ALLOCATABLE :: cColumnHeaders(:)
    
    !Find the index of the ZBudget in the list
    iLoc = LocateInList(iZBudgetType,Model%iZBudgetTypeList)    
    IF (iLoc .EQ. 0) THEN
        CALL SetLastMessage('Specified zone budget type is not found as part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%New(Model%cZBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Generate zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the number of non-diversified columns; first column will be Time so that will be eliminated later
    CALL ZBudget%GetFullColumnHeaders('area units','volume units',cColumnHeaders,iStat)  ;  IF (iStat .NE. 0) RETURN
    iNCols = SIZE(cColumnHeaders)
    ALLOCATE (iColumnList(iNCols))
    iColumnList = [(indx,indx=1,iNCols)]
    
    !Now get the number of diversified columns
    CALL ZBudget%GetNDiversifiedColumns(ZoneList,iZoneID,iColumnList,iNCols,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Remove time column from the count
    iNCols = iNCols - 1
     
    !Delete zone list
    CALL ZoneList%Kill()
    
    !Close file
    CALL ZBudget%Kill()

  END SUBROUTINE GetZBudget_NColumns
  
  
  ! -------------------------------------------------------------
  ! --- GET ZBUDGET COLUMN TITLES FOR A ZBUDGET AND A ZONE FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_ColumnTitles(Model,iZBudgetType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                       :: iZBudgetType,iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitAR,cUnitVL 
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23)                  :: ThisProcedure = ModName // 'GetZBudget_ColumnTitles' 
    INTEGER                                       :: iNCols,ErrorCode,indx,iZonesWithNames(0),iLoc
    INTEGER,ALLOCATABLE                           :: iColumnList(:),iDummyArray(:)
    CHARACTER(LEN=1)                              :: cZoneNames(0)
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    TYPE(ZoneListType)                            :: ZoneList
    TYPE(ZBudgetType)                             :: ZBudget
    
    !Get the index for the Z-Budget
    iLoc = LocateInList(iZBudgetType,Model%iZBudgetTypeList)
    IF (iLoc .EQ. 0) THEN
        CALL SetLastMessage('Specified zone budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%New(Model%cZBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Generate zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the non-diversified column titles; first column will be Time so that will be eliminated later
    CALL ZBudget%GetFullColumnHeaders(cUnitAR,cUnitVL,cColTitles_Local,iStat)  ;  IF (iStat .NE. 0) RETURN
    iNCols = SIZE(cColTitles_Local)
    ALLOCATE (iColumnList(iNCols))
    iColumnList = [(indx,indx=1,iNCols)]
    
    !Now get the diversified column titles
    DEALLOCATE (cColTitles_Local , STAT = ErrorCode)
    CALL ZBudget%GetFullColumnHeaders(cUnitAR,cUnitVL,cColTitles_Local,iStat,ZoneList,iZoneID,iColumnList,iDummyArray) 
    iNCols = SIZE(cColTitles_Local) - 1
    ALLOCATE (cColTitles(iNCols))
    cColTitles = ''
    cColTitles = cColTitles_Local(2:)

    !Delete zone list
    CALL ZoneList%Kill()
    
    !Close file
    CALL ZBudget%Kill()

  END SUBROUTINE GetZBudget_ColumnTitles
  
  
  ! -------------------------------------------------------------
  ! --- GET FULL MONTHLY AVERAGE ZONE BUDGET FROM A ZBUDGET FILE FOR A SELECTED ZONE FROM THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_MonthlyAverageFlows(Model,iZBudgetType,iZoneID,iLUType,iZExtent,iElems,iLayers,iZoneIDs,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows,rSDFlows,cFlowNames,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                       :: iZBudgetType,iZoneID,iLUType,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)              :: cAdjustedBeginDate,cAdjustedEndDate
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:),rSDFlows(:,:)    !Flows are return in (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat  
  
    !Local variables
    CHARACTER(LEN=ModNameLen+29) :: ThisProcedure = ModName // 'GetBudget_MonthlyAverageFlows'
    INTEGER                      :: iZonesWithNames(0),iLoc,iMonth,iNCols,iNTime,indxTime,indxCol
    REAL(8)                      :: rNYears,rDiff
    CHARACTER                    :: cZoneNames(0)*1
    REAL(8),ALLOCATABLE          :: rFlows_Work(:,:)
    TYPE(ZoneListType)           :: ZoneList
    TYPE(ZBudgetType)            :: ZBudget
    TYPE(AppUnsatZoneType)       :: AppUnsatZone
    TYPE(GWZBudgetType)          :: GWZBudget
    TYPE(RootZoneType)           :: RootZone
    
    !Get the index for the Z-Budget
    iLoc = LocateInList(iZBudgetType,Model%iZBudgetTypeList)
    IF (iLoc .EQ. 0) THEN
        CALL SetLastMessage('Specified zone budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%New(Model%cZBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Generate zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the inflow/outflow columns depending on the ZBudget 
    SELECT CASE (iZBudgetType)
        !Process groundwater ZBudget
        CASE (f_iZBudgetType_GW)
            CALL GWZBudget%GetMonthlyFlows(ZBudget,ZoneList,iZoneID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows_Work,cFlowNames,iStat)
            
        !Process root zone ZBudgets
        CASE (f_iZBudgetType_LWU , f_iZBudgetType_RootZone)  
            CALL RootZone%GetZBudget_MonthlyFlows(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows_Work,cFlowNames,iStat)
            
        !Process unsat zone ZBudget
        CASE (f_iZBudgetType_UnsatZone)
            CALL AppUnsatZone%GetZBudget_MonthlyFlows(ZBudget,ZoneList,iZoneID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows_Work,cFlowNames,iStat)
    END SELECT
        
    !Accumulate monthly values
    iNCols = SIZE(rFlows_Work,DIM=1)
    iNTime = SIZE(rFlows_Work,DIM=2)
    ALLOCATE (rFlows(iNCols,12))
    rFlows = 0.0
    iMonth = 1
    DO indxTime=1,iNTime
        DO indxCol=1,iNCols
            rFlows(indxCol,iMonth) = rFlows(indxCol,iMonth) + rFlows_Work(indxCol,indxTime)
        END DO
        iMonth = iMonth + 1
        IF (iMonth .EQ. 13) iMonth = 1
    END DO 
    
    !Calculate monthly average
    rNYears = REAL(iNTime/12 , 8) 
    rFlows  = rFlows / rNYears

    !Calculate plus/minus standard deviation flows
    ALLOCATE (rSDFlows(iNCols,12))
    rSDFlows = 0.0
    iMonth   = 1
    DO indxTime=1,iNTime
        DO indxCol=1,iNCols
            rDiff                    = rFlows_Work(indxCol,indxTime) - rFlows(indxCol,iMonth)
            rSDFlows(indxCol,iMonth) = rSDFlows(indxCol,iMonth) + rDiff * rDiff
        END DO
        iMonth = iMonth + 1
        IF (iMonth .EQ. 13) iMonth = 1
    END DO 
    rSDFlows = SQRT(rSDFlows / rNYears)
    
    !Delete zone list
    CALL ZoneList%Kill()
    
    !Close file
    CALL ZBudget%Kill()

  END SUBROUTINE GetZBudget_MonthlyAverageFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET ZONE BUDGET ANUAL FLOWS FROM A ZBUDGET FILE FOR A SELECTED ZONE FROM THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_AnnualFlows(Model,iZBudgetType,iZoneID,iLUType,iNYears,iZExtent,iElems,iLayers,iZoneIDs,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN)  :: Model
    INTEGER,INTENT(IN)                       :: iZBudgetType,iZoneID,iLUType,iNYears,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)              :: cAdjustedBeginDate,cAdjustedEndDate
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)    !rFlows is in (column,year) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat  
  
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'GetBudget_AnnualFlows'
    INTEGER                      :: iZonesWithNames(0),iMonth,iYear,iLoc,iNCols,indxTime,iErrorCode
    CHARACTER                    :: cZoneNames(0)*1
    REAL(8),ALLOCATABLE          :: rFlows_Work(:,:)
    TYPE(ZoneListType)           :: ZoneList
    TYPE(ZBudgetType)            :: ZBudget
    TYPE(AppUnsatZoneType)       :: AppUnsatZone
    TYPE(GWZBudgetType)          :: GWZBudget
    TYPE(RootZoneType)           :: RootZone
    
    !Get the index for the Z-Budget
    iLoc = LocateInList(iZBudgetType,Model%iZBudgetTypeList)
    IF (iLoc .EQ. 0) THEN
        CALL SetLastMessage('Specified zone budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%New(Model%cZBudgetFiles(iLoc),iStat)
    IF (iStat .NE. 0) RETURN
    
    !Generate zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the inflow/outflow columns depending on the ZBudget 
    SELECT CASE (iZBudgetType)
        !Process groundwater ZBudget
        CASE (f_iZBudgetType_GW)
            CALL GWZBudget%GetMonthlyFlows(ZBudget,ZoneList,iZoneID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows_Work,cFlowNames,iStat)
            
        !Process root zone ZBudgets
        CASE (f_iZBudgetType_LWU , f_iZBudgetType_RootZone)  
            CALL RootZone%GetZBudget_MonthlyFlows(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows_Work,cFlowNames,iStat)
            
        !Process unsat zone ZBudget
        CASE (f_iZBudgetType_UnsatZone)
            CALL AppUnsatZone%GetZBudget_MonthlyFlows(ZBudget,ZoneList,iZoneID,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rFlows_Work,cFlowNames,iStat)
    END SELECT
    IF (iStat .NE. 0) RETURN
        
    !Compute annual flows
    iNCols = SIZE(cFlowNames)
    ALLOCATE (rFlows(iNCols,iNYears))
    rFlows = 0.0
    iMonth = 1
    iYear  = 1
    DO indxTime=1,SIZE(rFlows_Work,DIM=2)
        rFlows(:,iYear) = rFlows(:,iYear) + rFlows_Work(:,indxTime)
        iMonth          = iMonth + 1
        IF (iMonth .GT. 12) THEN
            iMonth = 1
            iYear  = iYear + 1
        END IF
    END DO
    
    !Clear memory
    DEALLOCATE (rFlows_Work , STAT=iErrorCode)

    !Delete zone list
    CALL ZoneList%Kill()
    
    !Close file
    CALL ZBudget%Kill()

  END SUBROUTINE GetZBudget_AnnualFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET ZONE BUDGET TIME SERIES DATA FROM A ZBUDGET FILE FOR A SELECTED ZONE AND SELECTED COLUMNS FROM THE INQUIRY MODEL 
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_TSData(Model,iZBudgetType,iZoneID,iCols,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,cInterval,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iZBudgetType,iZoneID,iCols(:),iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)             :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                      :: rFactAR,rFactVL
    REAL(8),INTENT(OUT)                     :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                     :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17) :: ThisProcedure = ModName // 'GetZBudget_TSData'
    INTEGER                      :: indx,iLoc,iZonesWithNames(0)
    REAL(8)                      :: rValues(SIZE(iCols)+1,SIZE(rOutputDates))
    CHARACTER(LEN=0)             :: cZoneNames(0)
    TYPE(ZBudgetType)            :: ZBudget
    TYPE(ZoneListType)           :: ZoneList
    
    !Get the index for the Z-Budget
    iLoc = LocateInList(iZBudgetType,Model%iZBudgetTypeList)
    IF (iLoc .EQ. 0) THEN
        CALL SetLastMessage('Specified zone budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%New(Model%cZBudgetFiles(iLoc),iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Generate zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read data
    CALL ZBudget%ReadData(ZoneList,iZoneID,iCols,cInterval,cBeginDate,cEndDate,rFactAR,rFactVL,iDataTypes,inActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indx=1,inActualOutput
        rOutputDates(indx)    = rValues(1,indx)
        rOutputValues(indx,:) = rValues(2:,indx)
    END DO
    
    !Delete zone list
    CALL ZoneList%Kill()
    
    !Close file
    CALL ZBudget%Kill()

  END SUBROUTINE GetZBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE GW STORAGE CHANGE FROM Z-BUDGET FILE FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_CumGWStorChange(Model,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutputDates,rCumStorChange,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)             :: cBeginDate,cEndDate,cOutputInterval
    REAL(8),INTENT(IN)                      :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)         :: rOutputDates(:),rCumStorChange(:)    
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure = ModName // 'GetZBudget_CumGWStorChange'
    INTEGER                      :: iZonesWithNames(0),iLoc
    CHARACTER                    :: cZoneNames(0)*1
    TYPE(ZBudgetType)            :: ZBudget
    TYPE(ZoneListType)           :: ZoneList
    TYPE(GWZBudgetType)          :: GWZBudget
    
    !Get the index for the GW Z-Budget
    iLoc = LocateInList(f_iZBudgetType_GW,Model%iZBudgetTypeList)
    IF (iLoc .EQ. 0) THEN
        CALL SetLastMessage('Groundwater zone budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%New(Model%cZBudgetFiles(iLoc),iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Generate zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Retrieve data
    CALL GWZBudget%GetCumGWStorChange(ZBudget,ZoneList,iZoneID,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutputDates,rCumStorChange,iStat)
     
    !Delete zone list
    CALL ZoneList%Kill()
    
    !Close file
    CALL ZBudget%Kill()

  END SUBROUTINE GetZBudget_CumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET Annual CUMULATIVE GW STORAGE CHANGE FROM Z-BUDGET FILE FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_AnnualCumGWStorChange(Model,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cAdjustedBeginDate,cAdjustedEndDate,rFactVL,rCumStorChange,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)             :: cAdjustedBeginDate,cAdjustedEndDate
    REAL(8),INTENT(IN)                      :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)         :: rCumStorChange(:) 
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32)      :: ThisProcedure = ModName // 'GetZBudget_AnnualCumGWStorChange'
    INTEGER                           :: iZonesWithNames(0),iLoc
    CHARACTER                         :: cZoneNames(0)*1
    REAL(8),ALLOCATABLE               :: rDates(:)
    TYPE(ZBudgetType)                 :: ZBudget
    TYPE(ZoneListType)                :: ZoneList
    TYPE(GWZBudgetType)               :: GWZBudget
    
    !Get the index for the GW Z-Budget
    iLoc = LocateInList(f_iZBudgetType_GW,Model%iZBudgetTypeList)
    IF (iLoc .EQ. 0) THEN
        CALL SetLastMessage('Groundwater zone budget is not part of the model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%New(Model%cZBudgetFiles(iLoc),iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Generate zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Retrieve data
    CALL GWZBudget%GetCumGWStorChange(ZBudget,ZoneList,iZoneID,cAdjustedBeginDate,cAdjustedEndDate,'1YEAR',rFactVL,rDates,rCumStorChange,iStat)
     
    !Delete zone list
    CALL ZoneList%Kill()
    
    !Close file
    CALL ZBudget%Kill()
    
  END SUBROUTINE GetZBudget_AnnualCumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR A PERIOD FOR POST-PROCESSING FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHeads_ForALayer(Model,NNodes,NLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: NNodes,NLayers,iLayer
    TYPE(TimeStepType),INTENT(IN)           :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)             :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)                      :: rFact_LT
    REAL(8),INTENT(OUT)                     :: rOutputDates(:),rGWHeads(:,:)
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables 
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'GetGWHeads_ForALayer'
    INTEGER                      :: indx
    TYPE(AppGWType)              :: AppGW_Dummy
    
    !Initialize
    iStat = 0
    
    !Does GWHeadsAll output even exist
    DO indx=1,Model%iNHydrographTypes
        IF (TRIM(f_cDescription_GWHyd_AtNodeLayer) .EQ. TRIM(Model%cHydrographDescriptions(indx))) THEN
            CALL AppGW_Dummy%GetGWHeads_ForALayer(Model%cHydrographFiles(indx),NNodes,NLayers,iLayer,TimeStep,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rOutputDates,rGWHeads,iStat)
            RETURN
        END IF
    END DO
    
    !If here, the data was not found
    CALL SetLastMessage('An output file for GWHeadsAll cannot be found to retrieve gw heads at all nodes and layers!',f_iFatal,ThisProcedure)
    iStat = -1
    
  END SUBROUTINE GetGWHeads_ForALayer
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF LOCATIONS, GIVEN LOCATION TYPE, FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  FUNCTION GetNLocations(Model,iLocationType) RESULT(iNLocations)
    CLASS(Model_ForInquiry_Type),TARGET,INTENT(IN) :: Model
    INTEGER,INTENT(IN)                             :: iLocationType
    INTEGER                                        :: iNLocations
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_Diversion)
            iNLocations = Model%iNDiversions
            
        CASE (f_iLocationType_SmallWatershed)
            iNLocations = Model%iNSmallWatersheds
            
        CASE (f_iLocationType_GWHeadObs)
            iNLocations = Model%iNGWHeadObs
            
        CASE (f_iLocationType_StrmHydObs)
            iNLocations = Model%iNStrmHydObs
            
        CASE (f_iLocationType_SubsidenceObs)
            iNLocations = Model%iNSubsidenceObs
            
        CASE (f_iLocationType_TileDrainObs)
            iNLocations = Model%iNTileDrainObs
            
        CASE (f_iLocationType_StrmNodeBud)
            iNLocations = Model%iNStrmNodes_WithBudget
            
    END SELECT
        
  END FUNCTION GetNLocations

  
  ! -------------------------------------------------------------
  ! --- GET LOCATION IDs, GIVEN LOCATION TYPE, FROM INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetLocationIDs(Model,iNReaches,iLocationType,iLocationIDs)
    CLASS(Model_ForInquiry_Type),INTENT(IN) :: Model
    INTEGER,INTENT(IN)                      :: iNReaches,iLocationType
    INTEGER,ALLOCATABLE                     :: iLocationIDs(:)
    
    SELECT CASE (iLocationType)
        CASE (f_iLocationType_StrmReach)
            ALLOCATE(iLocationIDs(iNReaches))
            iLocationIDs = Model%iStrmReachIDs
                
        CASE (f_iLocationType_Diversion)
            ALLOCATE(iLocationIDs(Model%iNDiversions))
            iLocationIDs = Model%iDiversionIDs
            
        CASE (f_iLocationType_SmallWatershed)
            ALLOCATE(iLocationIDs(Model%iNSmallWatersheds))
            iLocationIDs = Model%iSmallWatershedIDs
            
        CASE (f_iLocationType_GWHeadObs)
            ALLOCATE(iLocationIDs(Model%iNGWHeadObs))
            iLocationIDs = Model%iGWHeadObsIDs
            
        CASE (f_iLocationType_StrmHydObs)
            ALLOCATE(iLocationIDs(Model%iNStrmHydObs))
            iLocationIDs = Model%iStrmHydObsIDs
            
        CASE (f_iLocationType_SubsidenceObs)
            ALLOCATE(iLocationIDs(Model%iNSubsidenceObs))
            iLocationIDs = Model%iSubsidenceObsIDs
            
        CASE (f_iLocationType_TileDrainObs)
            ALLOCATE(iLocationIDs(Model%iNTileDrainObs))
            iLocationIDs = Model%iTileDrainObsIDs
            
        CASE (f_iLocationType_StrmNodeBud)
            ALLOCATE(iLocationIDs(Model%iNStrmNodes_WithBudget))
            iLocationIDs = Model%iStrmNodeIDs_WithBudget
            
    END SELECT
    
  END SUBROUTINE GetLocationIDs
  
  
  
  
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
  ! --- PRINT MODEL DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintModelData(cSIMWorkingDirectory,AppGW,AppUnsatZone,AppStream,AppSWShed,TimeStep,NTIME,cHydTypeList,cHydFileList,iHydLocationTypeList,iHydCompList,cBudgetList,cBudgetFiles,iBudgetTypeList,iBudgetCompList,iBudgetLocationTypeList,cZBudgetList,cZBudgetFiles,iZBudgetTypeList,iStat)
    CHARACTER(LEN=*),INTENT(IN)            :: cSIMWorkingDirectory
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(AppUnsatZoneType),INTENT(IN)      :: AppUnsatZone
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(IN)                     :: NTIME,iHydLocationTypeList(:),iHydCompList(:),iBudgetTypeList(:),iBudgetCompList(:),iBudgetLocationTypeList(:),iZBudgetTypeList(:)
    CHARACTER(LEN=*),INTENT(IN)            :: cHydTypeList(:),cHydFileList(:),cBudgetList(:),cBudgetFiles(:),cZBudgetList(:),cZBudgetFiles(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    INTEGER                  :: iErrorCode,iNData
    INTEGER,ALLOCATABLE      :: iIDs(:)
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
    
    !Number of unsaturated zone layers and tile drains
    CALL ModelDataFile%WriteData(AppUnsatZone%GetNLayers())  
    CALL ModelDataFile%WriteData(AppGW%GetNDrain()) 
    
    !Write stream node IDs with budget
    DEALLOCATE (iIDs , STAT=iErrorCode)
    CALL AppStream%GetStrmNodeIDs_WithBudget(iIDs)
    CALL ModelDataFile%WriteData(SIZE(iIDs))
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write stream reach IDs
    iNData = AppStream%GetNReaches()
    DEALLOCATE (iIDs , STAT=iErrorCode)
    ALLOCATE (iIDs(iNData))
    CALL AppStream%GetReachIDs(iIDs)
    CALL ModelDataFile%WriteData(iNData)
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write diversion IDs
    DEALLOCATE (iIDs , STAT=iErrorCode)
    iNData = AppStream%GetNDiver()
    ALLOCATE(iIDs(iNData))
    CALL AppStream%GetDiversionIDs(iIDs)
    CALL ModelDataFile%WriteData(iNData)
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write stream hydrograph IDs
    DEALLOCATE (iIDs , STAT=iErrorCode)
    iNData = AppStream%GetNHydrographs()
    ALLOCATE(iIDs(iNData))
    CALL AppStream%GetHydrographIDs(iIDs)
    CALL ModelDataFile%WriteData(iNData)
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write small watershed IDs
    DEALLOCATE (iIDs , STAT=iErrorCode)
    iNData = AppSWShed%GetNSmallWatersheds()
    ALLOCATE(iIDs(iNData))
    CALL AppSWShed%GetSmallWatershedIDs(iIDs)
    CALL ModelDataFile%WriteData(iNData)
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write gw hydrograph IDs
    DEALLOCATE (iIDs , STAT=iErrorCode)
    iNData = AppGW%GetNHydrographs(f_iLocationType_GWHeadObs)
    ALLOCATE(iIDs(iNData))
    CALL AppGW%GetHydrographIDs(f_iLocationType_GWHeadObs,iIDs)
    CALL ModelDataFile%WriteData(iNData)
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write subsidence hydrograph IDs
    DEALLOCATE (iIDs , STAT=iErrorCode)
    iNData = AppGW%GetNHydrographs(f_iLocationType_SubsidenceObs)
    ALLOCATE(iIDs(iNData))
    CALL AppGW%GetHydrographIDs(f_iLocationType_SubsidenceObs,iIDs)
    CALL ModelDataFile%WriteData(iNData)
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write tile drain hydrograph IDs
    DEALLOCATE (iIDs , STAT=iErrorCode)
    iNData = AppGW%GetNHydrographs(f_iLocationType_TileDrainObs)
    ALLOCATE(iIDs(iNData))
    CALL AppGW%GetHydrographIDs(f_iLocationType_TileDrainObs,iIDs)
    CALL ModelDataFile%WriteData(iNData)
    CALL ModelDataFile%WriteData(iIDs)
    
    !Write ZBudget data
    CALL ModelDataFile%WriteData(SIZE(iZBudgetTypeList))
    IF (SIZE(iZBudgetTypeList) .GT. 0) THEN
        CALL ModelDataFile%WriteData(iZBudgetTypeList)
        CALL ModelDataFile%WriteData(cZBudgetList)
        CALL ModelDataFile%WriteData(cZBudgetFiles)
    END IF
        
    !Write Budget data
    CALL ModelDataFile%WriteData(SIZE(iBudgetTypeList))
    IF (SIZE(iBudgetTypeList) .GT. 0) THEN
        CALL ModelDataFile%WriteData(iBudgetTypeList)
        CALL ModelDataFile%WriteData(iBudgetCompList)
        CALL ModelDataFile%WriteData(iBudgetLocationTypeList)
        CALL ModelDataFile%WriteData(cBudgetList)
        CALL ModelDataFile%WriteData(cBudgetFiles)
    END IF
    
    !Write Hydrograph data
    CALL ModelDataFile%WriteData(SIZE(iHydLocationTypeList))
    IF (SIZE(iHydLocationTypeList) .GT. 0) THEN
        CALL ModelDataFile%WriteData(iHydLocationTypeList)
        CALL ModelDataFile%WriteData(iHydCompList)
        CALL ModelDataFile%WriteData(cHydTypeList)
        CALL ModelDataFile%WriteData(cHydFileList)
    END IF
    
    !Close file
    CALL ModelDataFile%Kill()
      
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