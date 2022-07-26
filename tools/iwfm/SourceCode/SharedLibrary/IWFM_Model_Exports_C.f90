!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2022  
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
MODULE IWFM_Model_Exports
  !$ USE OMP_LIB
  USE,INTRINSIC :: ISO_C_BINDING  , ONLY: C_INT                                    , &
                                          C_DOUBLE                                 , &
                                          C_CHAR
  USE GeneralUtilities            , ONLY: FirstLocation                            , &
                                          GetFileDirectory                         , &
                                          String_Copy_C_F                          , &
                                          String_Copy_F_C                          , &
                                          CString_Len                              , &
                                          ConvertID_To_Index                       , &
                                          LocateInList
  USE TimeSeriesUtilities         , ONLY: TimeStepType                             , &
                                          IncrementTimeStamp                       , &
                                          IsTimeIntervalValid                      , &
                                          f_cRecognizedIntervals                   , &
                                          f_iTimeStampLength                          
  USE Package_Misc                , ONLY: f_iLandUse_Ag                            , &
                                          f_iLandUse_Urb                           , &
                                          f_iLocationType_StrmNode                 , &
                                          f_iLocationType_Element                  , &
                                          f_iLocationType_Lake                     , &
                                          f_iLocationType_Subregion                , &
                                          f_iLocationType_Zone                     , &
                                          f_iLocationType_Node                     , &
                                          f_iLocationType_GWHeadObs                , &
                                          f_iLocationType_SubsidenceObs            , &
                                          f_iLocationType_StrmReach                , &
                                          f_iLocationType_StrmHydObs               , &
                                          f_iLocationType_TileDrainObs             , &
                                          f_iLocationType_SmallWatershed  
  USE Package_AppSmallWatershed   , ONLY: f_iSWShedBudComp_RZ                      , &
                                          f_iSWShedBudComp_GW 
  USE Package_Model               , ONLY: ModelType 
  IMPLICIT NONE
  

  ! -------------------------------------------------------------
  ! --- PUBLIC VARIABLES
  ! -------------------------------------------------------------
  PUBLIC
  
  
  ! -------------------------------------------------------------
  ! --- VARIABLES
  ! -------------------------------------------------------------
  TYPE(ModelType),PRIVATE,SAVE     :: Model
  INTEGER,PRIVATE,SAVE             :: iNStrmNodes_DLL = 0 
  INTEGER,ALLOCATABLE,PRIVATE,SAVE :: iStrmNodeIDs_DLL(:)
  CHARACTER(:),ALLOCATABLE,PRIVATE :: cPreProcessorPath,cSimulationPath
  LOGICAL,PRIVATE,SAVE             :: lModel_Instantiated  = .FALSE.
  
  
  ! -------------------------------------------------------------
  ! --- MISC. DATA
  ! -------------------------------------------------------------
  INTEGER,PRIVATE,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PRIVATE,PARAMETER :: ModName    = 'IWFM_Model_Exports::'
  
  
  
CONTAINS


    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
    
  ! -------------------------------------------------------------
  ! --- INSTANTIATE MODEL DATA
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_New(LenPPFileName,cPPFileName,LenSimFileName,cSimFileName,IsRoutedStreams,IsForInquiry,iStat) BIND(C,NAME='IW_Model_New')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_New
    INTEGER(C_INT),INTENT(IN)         :: LenPPFileName,LenSimFileName,IsRoutedStreams,IsForInquiry
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cPPFileName(LenPPFileName),cSimFileName(LenSimFileName)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER :: cPPFileName_F*LenPPFileName,cSimFileName_F*LenSimFileName
    LOGICAL   :: lRoutedStreams,lForInquiry
    
    !Set environmentt for parallel processing
    !$ CALL KMP_SET_BLOCKTIME(0)
    !$ CALL OMP_SET_NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    
    !Initialize
    iStat = 0
    
    !Logical variables
    IF (IsRoutedStreams .EQ. 0) THEN
        lRoutedStreams = .FALSE.
    ELSE
        lRoutedStreams = .TRUE.
    END IF
    IF (IsForInquiry .EQ. 0) THEN
        lForInquiry = .FALSE.
    ELSE
        lForInquiry = .TRUE.
    END IF
    
    !If Model is already instantiated, return
    IF (lModel_Instantiated) THEN
        RETURN
    END IF
        
    !C strings to Fortran strings
    CALL String_Copy_C_F(cPPFileName,cPPFileName_F)
    CALL String_Copy_C_F(cSimFileName,cSimFileName_F)
        
    !Read main control data for pre-processor and simulation (if filename is specified) and 
    !  instantiate model components
    IF (LEN(cSimFileName_F) .EQ. 0) THEN
        CALL Model%New(cPPFileName_F,lRoutedStreams=lRoutedStreams,lPrintBinFile=.FALSE.,iStat=iStat)
    ELSE
        CALL Model%New(cPPFileName_F,cSimFileName_F,lRoutedStreams,lForInquiry,iStat=iStat)
    END IF
    IF (iStat .EQ. -1) THEN
        CALL Model%Kill()
        RETURN
    END IF
    
    !Get the number of stream nodes
    iNStrmNodes_DLL = Model%GetNStrmNodes()
    
    !Get stream node IDs
    ALLOCATE (iStrmNodeIDs_DLL(iNStrmNodes_DLL))
    CALL Model%GetStrmNodeIDs(iStrmNodeIDs_DLL)
   
    !Set the flag to check if Model is already instantiated
    lModel_Instantiated = .TRUE.
    
  END SUBROUTINE IW_Model_New
  
  
  
  
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
  SUBROUTINE IW_Model_Kill(iStat) BIND(C,NAME='IW_Model_Kill')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_Kill
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Local variables
    INTEGER :: ErrorCode

    iStat = 0
    
    CALL Model%Kill()
    
    DEALLOCATE (iStrmNodeIDs_DLL , STAT=ErrorCode)
    DEALLOCATE (cPreProcessorPath , cSimulationPath , STAT=ErrorCode)
    iNStrmNodes_DLL     = 0
    lModel_Instantiated = .FALSE.
    
  END SUBROUTINE IW_Model_Kill
  


  
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
  ! --- GET NUMBER OF WELLS 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNWells(iNWells,iStat) BIND(C,NAME='IW_Model_GetNWells')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNWells
    INTEGER(C_INT),INTENT(OUT) :: iNWells,iStat
    
    iNWells = Model%GetNWells()
    iStat   = 0
    
  END SUBROUTINE IW_Model_GetNWells
  
   
  ! -------------------------------------------------------------
  ! --- GET WELL IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetWellIDs(iNWells,IDs,iStat) BIND(C,NAME='IW_Model_GetWellIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetWellIDs
    INTEGER(C_INT),INTENT(IN)  :: iNWells
    INTEGER(C_INT),INTENT(OUT) :: IDs(iNWells),iStat
    
    CALL Model%GetWellIDs(IDs)
    iStat = 0
    
  END SUBROUTINE IW_Model_GetWellIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENT PUMPING 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNElemPumps(iNElemPumps,iStat) BIND(C,NAME='IW_Model_GetNElemPumps')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNElemPumps
    INTEGER(C_INT),INTENT(OUT) :: iNElemPumps,iStat
    
    iNElemPumps = Model%GetNElemPumps()
    iStat       = 0
    
  END SUBROUTINE IW_Model_GetNElemPumps
  
   
  ! -------------------------------------------------------------
  ! --- GET ELEMENT PUMPING IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetElemPumpIDs(iNElemPumps,IDs,iStat) BIND(C,NAME='IW_Model_GetElemPumpIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetElemPumpIDs
    INTEGER(C_INT),INTENT(IN)  :: iNElemPumps
    INTEGER(C_INT),INTENT(OUT) :: IDs(iNElemPumps),iStat
    
    CALL Model%GetElemPumpIDs(IDs)
    iStat = 0
    
  END SUBROUTINE IW_Model_GetElemPumpIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET FUTURE WATER DEMANDS FOR A DIVERSION AT A SPECIFIED DATE
  ! --- NOTE: This must be called after IW_ReadTSData or 
  ! ---       IW_ReadTSData_Overwrite procedures
  ! ---       for consistent operation because the TS files  
  ! ---       are rewound back to where they were after  
  ! ---       ReadTSData method was called. 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetFutureWaterDemand_ForDiversion(iDiversion,iLenDate,cDemandDate,rFactor,rDemand,iStat) BIND(C,NAME='IW_Model_GetFutureWaterDemand_ForDiversion')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetFutureWaterDemand_ForDiversion
    INTEGER(C_INT),INTENT(IN)         :: iDiversion,iLenDate
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cDemandDate(iLenDate)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactor
    REAL(C_DOUBLE),INTENT(OUT)        :: rDemand
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER :: cDemandDate_F*iLenDate
    
    !Convert C strings to F strings
    CALL String_Copy_C_F(cDemandDate,cDemandDate_F)
    
    !Retrieve
    CALL Model%GetFutureWaterDemand_ForDiversion(iDiversion,cDemandDate_F,rDemand,iStat)
    rDemand = rDemand * rFactor
    
  END SUBROUTINE IW_Model_GetFutureWaterDemand_ForDiversion

  
  ! -------------------------------------------------------------
  ! --- GET CURRENT SIMULATION DATE AND TIME
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetCurrentDateAndTime(iLenDateTime,cCurrentDateAndTime,iStat) BIND(C,NAME='IW_Model_GetCurrentDateAndTime')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetCurrentDateAndTime
    INTEGER(C_INT),INTENT(IN)          :: iLenDateTime
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cCurrentDateAndTime(iLenDateTime)
    INTEGER(C_INT),INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER :: cCurrentDateAndTime_F*f_iTimeStampLength
    
    !Initialize
    iStat = 0
    
    !Get the current date and time
    CALL Model%GetCurrentDateAndTime(cCurrentDateAndTime_F)
    
    !Fortran strings to C strings
    CALL String_Copy_F_C(cCurrentDateAndTime_F , cCurrentDateAndTime)
    
  END SUBROUTINE IW_Model_GetCurrentDateAndTime
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TIMESTEPS IN THE SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNTimeSteps(NTimeSteps,iStat) BIND(C,NAME='IW_Model_GetNTimeSteps')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNTimeSteps
    INTEGER(C_INT),INTENT(OUT) :: NTimeSteps,iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    
    !Initialize
    iStat = 0
    
    !Get the number of time steps
    CALL Model%GetTimeSpecs(TimeStep,NTimeSteps)
        
  END SUBROUTINE IW_Model_GetNTimeSteps
  
  
  ! -------------------------------------------------------------
  ! --- GET SIMULATION TIME RELATED DATA FROM Model OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetTimeSpecs(cDataDatesAndTimes,iLenDates,cInterval,iLenInterval,NData,iLocArray,iStat) BIND(C,NAME='IW_Model_GetTimeSpecs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetTimeSpecs
    INTEGER(C_INT),INTENT(IN)          :: iLenDates,iLenInterval,NData
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cDataDatesAndTimes(iLenDates),cInterval(iLenInterval)
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(NData),iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    CHARACTER          :: cDateAndTime*f_iTimeStampLength,cDataDatesAndTimes_F*iLenDates,cInterval_F*iLenInterval
    INTEGER            :: indx,nTime
    
    !Initialize
    iStat                = 0
    cDataDatesAndTimes_F = ''
    
    !Get the time step data
    CALL Model%GetTimeSpecs(TimeStep,nTime)
    
    !Data interval
    cInterval_F = TimeStep%Unit
    
    !First entry
    cDataDatesAndTimes_F = IncrementTimeStamp(TimeStep%CurrentDateAndTime,TimeStep%DeltaT_InMinutes,1)
    iLocArray(1)         = 1
    
    !Compile data
    cDateAndTime = TRIM(cDataDatesAndTimes_F)
    DO indx=2,NData
        iLocArray(indx)      = LEN_TRIM(cDataDatesAndTimes_F) + 1
        cDateAndTime         = IncrementTimeStamp(cDateAndTime,TimeStep%DeltaT_InMinutes,1)
        cDataDatesAndTimes_F = TRIM(cDataDatesAndTimes_F) // TRIM(cDateAndTime)
    END DO
    
    !Fortran strings to C strings
    CALL String_Copy_F_C(cDataDatesAndTimes_F,cDataDatesAndTimes)
    CALL String_Copy_F_C(cInterval_F,cInterval)
    
  END SUBROUTINE IW_Model_GetTimeSpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET THE POSSIBLE RESULTS OUTPUT INTERVALS BASED ON SIMULATION TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetOutputIntervals(cOutputIntervals,iLenOutputIntervals,iLocArray,iDim_LocArray_In,iDim_LocArray_Out,iStat) BIND(C,NAME='IW_Model_GetOutputIntervals')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetOutputIntervals
    INTEGER(C_INT),INTENT(IN)          :: iLenOutputIntervals,iDim_LocArray_In
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cOutputIntervals(iLenOutputIntervals)
    INTEGER(C_INT),INTENT(OUT)         :: iDim_LocArray_Out
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(iDim_LocArray_In),iStat
    
    !Local variables
    INTEGER                      :: indx,iDim,iCount,nTime
    CHARACTER(LEN=6),ALLOCATABLE :: cOutputIntervals_Local(:)
    TYPE(TimeStepType)           :: TimeStep
    CHARACTER                    :: cOutputIntervals_F*iLenOutputIntervals
    
    !Initialize
    iStat  = 0
    iDim   = SIZE(f_cRecognizedIntervals)
    iCount = 1
    
    !Get model time specs
    CALL Model%GetTimeSpecs(TimeStep,nTime)
    
    !Find the index of simulation time unit in RecognizedIntervals array
    indx              = IsTimeIntervalValid(TimeStep%Unit)  
    iDim_LocArray_Out = iDim - indx + 1

    !Allocate character array to work with
    ALLOCATE (cOutputIntervals_Local(iDim_LocArray_Out))
    cOutputIntervals_Local = ''
    
    !Populate output intervals array
    DO indx=iDim-iDim_LocArray_Out+1,iDim
        cOutputIntervals_Local(iCount) = f_cRecognizedIntervals(indx)
        iCount                         = iCount + 1
    END DO
    
    !Convert array of strings to scalar string
    CALL StringArray_To_StringScalar(cOutputIntervals_Local,cOutputIntervals_F,iLocArray(1:iDim_LocArray_Out))
    
    !Fortran string to C string
    CALL String_Copy_F_C(cOutputIntervals_F,cOutputIntervals)    
    
  END SUBROUTINE IW_Model_GetOutputIntervals
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE BUDGET OUTPUTS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_N(iNBudgets,iStat) BIND(C,NAME='IW_Model_GetBudget_N')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_N
    INTEGER(C_INT),INTENT(OUT) :: iNBudgets,iStat
    
    iStat     = 0
    iNBudgets = Model%GetBudget_N()

  END SUBROUTINE IW_Model_GetBudget_N
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF AVAILABLE BUDGET OUTPUTS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_List(iNBudgets,iLocArray,iLenBudgetList,cBudgetList,iBudgetTypeList,iBudgetLocationTypeList,iStat) BIND(C,NAME='IW_Model_GetBudget_List')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_List
    INTEGER(C_INT),INTENT(IN)          :: iNBudgets,iLenBudgetList
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(iNBudgets),iBudgetTypeList(iNBudgets),iBudgetLocationTypeList(iNBudgets),iStat
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cBudgetList(iLenBudgetList)
    
    !Local variables
    CHARACTER                      :: cBudgetList_F*iLenBudgetList
    INTEGER,ALLOCATABLE            :: iBudgetLocationTypeList_Local(:),iBudgetTypeList_Local(:),iBudgetCompList_Local(:)
    CHARACTER(LEN=100),ALLOCATABLE :: cBudgetList_Local(:)
    CHARACTER(LEN=500),ALLOCATABLE :: cFilesDummy(:)
    
    !Initialize
    iStat = 0
    
    !Return if number of Budgets is zero
    IF (iNBudgets .EQ. 0) THEN
        cBudgetList = ''
        RETURN
    END IF
    
    !Get the list
    CALL Model%GetBudget_List(cBudgetList_Local,iBudgetTypeList_Local,iBudgetCompList_Local,iBudgetLocationTypeList_Local,cFilesDummy)
    
    !Budget location and hydrologic component list
    iBudgetLocationTypeList = iBudgetLocationTypeList_Local
    iBudgetTypeList         = iBudgetTypeList_Local
        
    !Compile the Budget names list in a single string variable
    CALL StringArray_To_StringScalar(cBudgetList_Local,cBudgetList_F,iLocArray)
    CALL String_Copy_F_C(cBudgetList_F,cBudgetList)
        
  END SUBROUTINE IW_Model_GetBudget_List
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS FOR A SELECTED BUDGET (EXCLUDES TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_NColumns(iBudgetType,iLocIndex,iNCols,iStat) BIND(C,NAME='IW_Model_GetBudget_NColumns')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_NColumns
    INTEGER(C_INT),INTENT(IN)  :: iBudgetType,iLocIndex
    INTEGER(C_INT),INTENT(OUT) :: iNCols,iStat
    
    CALL Model%GetBudget_NColumns(iBudgetType,iLocIndex,iNCols,iStat)

  END SUBROUTINE IW_Model_GetBudget_NColumns  
    
  
  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES FOR A SELECTED BUDGET (EXCLUDES TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_ColumnTitles(iBudgetType,iLocIndex,iLenUnit,cUnitLT,cUnitAR,cUnitVL,iNCols,iLocArray,iLenTitles,cColTitles,iStat) BIND(C,NAME='IW_Model_GetBudget_ColumnTitles')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_ColumnTitles
    INTEGER(C_INT),INTENT(IN)          :: iBudgetType,iLocIndex,iLenUnit,iNCols,iLenTitles
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: cUnitLT(iLenUnit),cUnitAR(iLenUnit),cUnitVL(iLenUnit)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cColTitles(iLenTitles)
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(iNCols),iStat
    
    !Local variables
    CHARACTER(LEN=200),ALLOCATABLE :: cColTitles_Local(:)
    CHARACTER(LEN=iLenTitles)      :: cColTitles_F*iLenTitles,cUnitLT_F*iLenUnit,cUnitAR_F*iLenUnit,cUnitVL_F*iLenUnit
    
    !Convert C-type units to F-type 
    CALL String_Copy_C_F(cUnitLT , cUnitLT_F)
    CALL String_Copy_C_F(cUnitAR , cUnitAR_F)
    CALL String_Copy_C_F(cUnitVL , cUnitVL_F)
    
    !Retrieve column titles
    CALL Model%GetBudget_ColumnTitles(iBudgetType,iLocIndex,cUnitLT_F,cUnitAR_F,cUnitVL_F,cColTitles_Local,iStat)
    IF (iStat .NE. 0) RETURN

    !Concatenate title array into a string scalar
    CALL StringArray_To_StringScalar(cColTitles_Local,cColTitles_F,iLocArray)
    
    !Convert F-type titles to C-type
    CALL String_Copy_F_C(cColTitles_F , cColTitles)
    
  END SUBROUTINE IW_Model_GetBudget_ColumnTitles 
    
  
  ! -------------------------------------------------------------
  ! --- GET FULL MONTHLY AVERAGE BUDGET FOR A SELECTED BUDGET TYPE AND LOCATION 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_MonthlyAverageFlows(iBudgetType,iLocationIndex,iLUType,iSWShedBudCompRZ,iLenDate,cBeginDate,cEndDate,rFactVL,iNFlows_In,rFlows,rSDFlows,iNFlows_Out,iLenFlowNames,cFlowNames,iLocArray,iStat) BIND(C,NAME='IW_Model_GetBudget_MonthlyAverageFlows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_MonthlyAverageFlows
    INTEGER(C_INT),INTENT(IN)          :: iBudgetType,iLocationIndex,iLUType,iSWShedBudCompRZ,iLenDate,iNFlows_In,iLenFlowNames
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: cBeginDate(iLenDate),cEndDate(iLenDate)
    REAL(C_DOUBLE),INTENT(IN)          :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)         :: rFlows(iNFlows_In,12),rSDFlows(iNFlows_In,12)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cFlowNames(iLenFlowNames)
    INTEGER(C_INT),INTENT(OUT)         :: iNFlows_Out,iLocArray(iNFlows_In),iStat
    
    !Local variables
    INTEGER                       :: indxMon,iSWShedBudType
    CHARACTER                     :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cFlowNames_F*iLenFlowNames
    REAL(8),ALLOCATABLE           :: rFlows_Local(:,:),rSDFlows_Local(:,:)
    CHARACTER(LEN=50),ALLOCATABLE :: cFlowNames_Local(:)
    
    !Convert C-type characters to F-type
    CALL String_Copy_C_F(cBeginDate , cBeginDate_F)
    CALL String_Copy_C_F(cEndDate , cEndDate_F)
    
    !Small watershed budget type
    IF (iSWShedBudCompRZ .EQ. 1) THEN
        iSWShedBudType = f_iSWShedBudComp_RZ
    ELSE
        iSWShedBudType = f_iSWShedBudComp_GW
    END IF    
    
    !Get full budget
    CALL Model%GetBudget_MonthlyAverageFlows(iBudgetType,iLocationIndex,iLUType,iSWShedBudType,cBeginDate_F,cEndDate_F,rFactVL,rFlows_Local,rSDFlows_Local,cFlowNames_Local,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Transfer flows to return arguments
    iNFlows_Out = SIZE(rFlows_Local,DIM=1)
    DO indxMon=1,12
        rFlows(1:iNFlows_Out,indxMon)   = rFlows_Local(:,indxMon)
        rSDFlows(1:iNFlows_Out,indxMon) = rSDFlows_Local(:,indxMon)
    END DO
    
    !Concatonate flow names into string scalar and convert to C-Type
    CALL StringArray_To_StringScalar(cFlowNames_Local,cFlowNames_F,iLocArray)
    CALL String_Copy_F_C(cFlowNames_F , cFlowNames)
    
  END SUBROUTINE IW_Model_GetBudget_MonthlyAverageFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET FULL ANNUAL BUDGET FOR A SELECTED BUDGET TYPE AND LOCATION 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_AnnualFlows(iBudgetType,iLocationIndex,iLUType,iSWShedBudCompRZ,iLenDate,cBeginDate,cEndDate,rFactVL,iNFlows_In,iNTimes_In,rFlows,iNFlows_Out,iNTimes_Out,iLenFlowNames,cFlowNames,iLocArray,iWaterYears,iStat) BIND(C,NAME='IW_Model_GetBudget_AnnualFlows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_AnnualFlows
    INTEGER(C_INT),INTENT(IN)          :: iBudgetType,iLocationIndex,iLUType,iSWShedBudCompRZ,iLenDate,iNFlows_In,iNTimes_In,iLenFlowNames
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: cBeginDate(iLenDate),cEndDate(iLenDate)
    REAL(C_DOUBLE),INTENT(IN)          :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)         :: rFlows(iNFlows_In,iNTimes_In)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cFlowNames(iLenFlowNames)
    INTEGER(C_INT),INTENT(OUT)         :: iNFlows_Out,iNTimes_Out,iLocArray(iNFlows_In),iWaterYears(iNTimes_In),iStat
    
    !Local variables
    INTEGER                       :: iSWShedBudType,indxYear
    CHARACTER                     :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cFlowNames_F*iLenFlowNames
    INTEGER,ALLOCATABLE           :: iWaterYears_Local(:)
    REAL(8),ALLOCATABLE           :: rFlows_Local(:,:)
    CHARACTER(LEN=50),ALLOCATABLE :: cFlowNames_Local(:)
    
    !Convert C-type characters to F-type
    CALL String_Copy_C_F(cBeginDate , cBeginDate_F)
    CALL String_Copy_C_F(cEndDate , cEndDate_F)
    
    !Small watershed budget type
    IF (iSWShedBudCompRZ .EQ. 1) THEN
        iSWShedBudType = f_iSWShedBudComp_RZ
    ELSE
        iSWShedBudType = f_iSWShedBudComp_GW
    END IF    
    
    !Get full budget
    CALL Model%GetBudget_AnnualFlows(iBudgetType,iLocationIndex,iLUType,iSWShedBudType,cBeginDate_F,cEndDate_F,rFactVL,rFlows_Local,cFlowNames_Local,iWaterYears_Local,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Transfer flows and water years to return arguments
    iNFlows_Out = SIZE(rFlows_Local,DIM=1)
    iNTimes_Out = SIZE(rFlows_Local,DIM=2)
    DO indxYear=1,iNTimes_Out
        rFlows(1:iNFlows_Out,indxYear) = rFlows_Local(:,indxYear)
        iWaterYears(indxYear)          = iWaterYears_Local(indxYear)
    END DO
    
    !Concatonate flow names into string scalar and convert to C-Type
    CALL StringArray_To_StringScalar(cFlowNames_Local,cFlowNames_F,iLocArray)
    CALL String_Copy_F_C(cFlowNames_F , cFlowNames)
    
  END SUBROUTINE IW_Model_GetBudget_AnnualFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FROM A BUDGET FILE FOR A SELECTED LOCATION AND SELECTED COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_TSData(iBudgetType,iLocationIndex,iNCols,iCols,iLenDate,cBeginDate,cEndDate,iLenInterval,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,iNTimes_In,rOutputValues,iDataTypes,iNTimes_Out,iStat) BIND(C,NAME="IW_Model_GetBudget_TSData")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_TSData
    INTEGER(C_INT),INTENT(IN)         :: iBudgetType,iLocationIndex,iNCols,iCols(iNCols),iNTimes_In,iLenDate,iLenInterval
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDate(iLenDate),cEndDate(iLenDate),cInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactLT,rFactAR,rFactVL
    REAL(C_DOUBLE),INTENT(OUT)        :: rOutputDates(iNTimes_In),rOutputValues(iNTimes_In,iNCols)    
    INTEGER,INTENT(OUT)               :: iDataTypes(iNCols),iNTimes_Out,iStat
    
    !Local variables
    CHARACTER :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cInterval_F*iLenInterval
    
    !Convert C-type strings to F-type strings
    CALL String_Copy_C_F(cBeginDate,cBeginDate_F)
    CALL String_Copy_C_F(cEndDate,cEndDate_F)
    CALL String_Copy_C_F(cInterval,cInterval_F)
    
    !Get the data
    CALL Model%GetBudget_TSData(iBudgetType,iLocationIndex,iCols,cBeginDate_F,cEndDate_F,cInterval_F,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,iNTimes_Out,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Convert Julian time to Excel-style Julian time
    rOutputDates(1:iNTimes_Out) = rOutputDates(1:iNTimes_Out) - REAL(2415020d0,C_DOUBLE)
   
  END SUBROUTINE IW_Model_GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE CHANGE IN GW STORAGE FROM BUDGET OUTPUT FOR A SELECTED SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_CumGWStorChange(iSubregionIndex,iLenDate,cBeginDate,cEndDate,iLenInterval,cInterval,rFactVL,rOutputDates,iNTimes_In,rCumGWStorChange,iNTimes_Out,iStat) BIND(C,NAME='IW_Model_GetBudget_CumGWStorChange')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_CumGWStorChange
    INTEGER(C_INT),INTENT(IN)         :: iSubregionIndex,iNTimes_In,iLenDate,iLenInterval
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDate(iLenDate),cEndDate(iLenDate),cInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)        :: rOutputDates(iNTimes_In),rCumGWStorChange(iNTimes_In)    
    INTEGER,INTENT(OUT)               :: iNTimes_Out,iStat
    
    !Local variables
    CHARACTER           :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cInterval_F*iLenInterval
    REAL(8),ALLOCATABLE :: rDates(:),rStorChange(:)
    
    !Convert C-type strings to F-type strings
    CALL String_Copy_C_F(cBeginDate,cBeginDate_F)
    CALL String_Copy_C_F(cEndDate,cEndDate_F)
    CALL String_Copy_C_F(cInterval,cInterval_F)
    
    !Get the data
    CALL Model%GetBudget_CumGWStorChange(iSubregionIndex,cBeginDate_F,cEndDate_F,cInterval_F,rFactVL,rDates,rStorChange,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Copy data to return arguments
    iNTimes_Out                     = SIZE(rDates)
    rOutputDates(1:iNTimes_Out)     = rDates
    rCumGWStorChange(1:iNTimes_Out) = rStorChange
    
    !Convert Julian time to Excel-style Julian time
    rOutputDates(1:iNTimes_Out) = rOutputDates(1:iNTimes_Out) - REAL(2415020d0,C_DOUBLE)
   
  END SUBROUTINE IW_Model_GetBudget_CumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET ANNUAL CUMULATIVE CHANGE IN GW STORAGE FROM BUDGET OUTPUT FOR A SELECTED SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBudget_AnnualCumGWStorChange(iSubregionIndex,iLenDate,cBeginDate,cEndDate,rFactVL,iNTimes_In,rCumGWStorChange,iWaterYears,iNTimes_Out,iStat) BIND(C,NAME='IW_Model_GetBudget_AnnualCumGWStorChange')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBudget_AnnualCumGWStorChange
    INTEGER(C_INT),INTENT(IN)         :: iSubregionIndex,iNTimes_In,iLenDate
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDate(iLenDate),cEndDate(iLenDate)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)        :: rCumGWStorChange(iNTimes_In)    
    INTEGER,INTENT(OUT)               :: iWaterYears(iNTimes_In),iNTimes_Out,iStat
    
    !Local variables
    CHARACTER           :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate
    INTEGER,ALLOCATABLE :: iWaterYears_Local(:)
    REAL(8),ALLOCATABLE :: rStorChange(:)
    
    !Convert C-type strings to F-type strings
    CALL String_Copy_C_F(cBeginDate,cBeginDate_F)
    CALL String_Copy_C_F(cEndDate,cEndDate_F)
    
    !Get the data
    CALL Model%GetBudget_AnnualCumGWStorChange(iSubregionIndex,cBeginDate_F,cEndDate_F,rFactVL,rStorChange,iWaterYears_Local,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Copy data to return arguments
    iNTimes_Out                     = SIZE(iWaterYears_Local)
    iWaterYears(1:iNTimes_Out)      = iWaterYears_Local
    rCumGWStorChange(1:iNTimes_Out) = rStorChange
    
  END SUBROUTINE IW_Model_GetBudget_AnnualCumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE ZBUDGET OUTPUTS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_N(iNZBudgets,iStat) BIND(C,NAME='IW_Model_GetZBudget_N')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_N
    INTEGER(C_INT),INTENT(OUT) :: iNZBudgets,iStat
    
    iStat      = 0
    iNZBudgets = Model%GetZBudget_N()

  END SUBROUTINE IW_Model_GetZBudget_N
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF AVAILABLE ZBUDGET OUTPUTS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_List(iNZBudgets,iLocArray,iLenZBudgetList,cZBudgetList,iZBudgetTypeList,iStat) BIND(C,NAME='IW_Model_GetZBudget_List')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_List
    INTEGER(C_INT),INTENT(IN)          :: iNZBudgets,iLenZBudgetList
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(iNZBudgets),iZBudgetTypeList(iNZBudgets),iStat
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cZBudgetList(iLenZBudgetList)
    
    !Local variables
    CHARACTER                      :: cZBudgetList_F*iLenZBudgetList
    INTEGER,ALLOCATABLE            :: iZBudgetTypeList_Local(:)
    CHARACTER(LEN=200),ALLOCATABLE :: cZBudgetList_Local(:)
    CHARACTER(LEN=500),ALLOCATABLE :: cFilesDummy(:)
    
    !Initialize
    iStat = 0
    
    !Return if number of ZBudgets is zero
    IF (iNZBudgets .EQ. 0) THEN
        cZBudgetList = ''
        RETURN
    END IF
    
    !Get the list
    CALL Model%GetZBudget_List(cZBudgetList_Local,iZBudgetTypeList_Local,cFilesDummy)
    
    !ZBudget type list
    iZBudgetTypeList = iZBudgetTypeList_Local
        
    !Compile the list in a single string variable
    CALL StringArray_To_StringScalar(cZBudgetList_Local,cZBudgetList_F,iLocArray)
    CALL String_Copy_F_C(cZBudgetList_F,cZBudgetList)
        
  END SUBROUTINE IW_Model_GetZBudget_List
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS FOR A SELECTED Z-BUDGET FOR A ZONE (EXCLUDES TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_NColumns(iZBudgetType,iZoneID,iZExtent,iDimZones,iElems,iLayers,iZoneIDs,iNCols,iStat) BIND(C,NAME='IW_Model_GetZBudget_NColumns')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_NColumns
    INTEGER(C_INT),INTENT(IN)  :: iZBudgetType,iZoneID,iZExtent,iDimZones,iElems(iDimZones),iLayers(iDimZones),iZoneIDs(iDimZones)
    INTEGER(C_INT),INTENT(OUT) :: iNCols,iStat
    
    CALL Model%GetZBudget_NColumns(iZBudgetType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,iNCols,iStat)

  END SUBROUTINE IW_Model_GetZBudget_NColumns  
    
  
  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES FOR A SELECTED Z-BUDGET FOR A ZONE (EXCLUDES TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_ColumnTitles(iZBudgetType,iZoneID,iZExtent,iDimZones,iElems,iLayers,iZoneIDs,iLenUnit,cUnitAR,cUnitVL,iNCols,iLocArray,iLenTitles,cColTitles,iStat) BIND(C,NAME='IW_Model_GetZBudget_ColumnTitles')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_ColumnTitles
    INTEGER(C_INT),INTENT(IN)          :: iZBudgetType,iZoneID,iZExtent,iDimZones,iElems(iDimZones),iLayers(iDimZones),iZoneIDs(iDimZones),iLenUnit,iNCols,iLenTitles
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: cUnitAR(iLenUnit),cUnitVL(iLenUnit)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cColTitles(iLenTitles)
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(iNCols),iStat
    
    !Local variables
    CHARACTER(LEN=200),ALLOCATABLE :: cColTitles_Local(:)
    CHARACTER(LEN=iLenTitles)      :: cColTitles_F*iLenTitles,cUnitAR_F*iLenUnit,cUnitVL_F*iLenUnit
    
    !Convert C-type units to F-type 
    CALL String_Copy_C_F(cUnitAR , cUnitAR_F)
    CALL String_Copy_C_F(cUnitVL , cUnitVL_F)
    
    !Retrieve column titles
    CALL Model%GetZBudget_ColumnTitles(iZBudgetType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cUnitAR_F,cUnitVL_F,cColTitles_Local,iStat)

    !Concatenate title array into a string scalar
    CALL StringArray_To_StringScalar(cColTitles_Local,cColTitles_F,iLocArray)
    
    !Convert F-type titles to C-type
    CALL String_Copy_F_C(cColTitles_F , cColTitles)
    
  END SUBROUTINE IW_Model_GetZBudget_ColumnTitles 
    
  
  ! -------------------------------------------------------------
  ! --- GET FULL MONTHLY AVERAGE ZONE BUDGET FOR A SELECTED ZONE 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_MonthlyAverageFlows(iZBudgetType,iZoneID,iLUType,iZExtent,iNZones,iElems,iLayers,iZoneIDs,iLenDate,cBeginDate,cEndDate,rFactVL,iNFlows_In,rFlows,rSDFlows,iNFlows_Out,iLenFlowNames,cFlowNames,iLocArray,iStat) BIND(C,NAME='IW_Model_GetZBudget_MonthlyAverageFlows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_MonthlyAverageFlows
    INTEGER(C_INT),INTENT(IN)          :: iZBudgetType,iZoneID,iLUType,iZExtent,iNZones,iElems(iNZones),iLayers(iNZones),iZoneIDs(iNZones),iLenDate,iNFlows_In,iLenFlowNames
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: cBeginDate(iLenDate),cEndDate(iLenDate)
    REAL(C_DOUBLE),INTENT(IN)          :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)         :: rFlows(iNFlows_In,12),rSDFlows(iNFlows_In,12)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cFlowNames(iLenFlowNames)
    INTEGER(C_INT),INTENT(OUT)         :: iNFlows_Out,iLocArray(iNFlows_In),iStat
    
    !Local variables
    INTEGER                       :: indxMon
    CHARACTER                     :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cFlowNames_F*iLenFlowNames
    REAL(8),ALLOCATABLE           :: rFlows_Local(:,:),rSDFlows_Local(:,:)
    CHARACTER(LEN=50),ALLOCATABLE :: cFlowNames_Local(:)
    
    !Convert C-type characters to F-type
    CALL String_Copy_C_F(cBeginDate , cBeginDate_F)
    CALL String_Copy_C_F(cEndDate , cEndDate_F)
    
    !Get full budget
    CALL Model%GetZBudget_MonthlyAverageFlows(iZBudgetType,iZoneID,iLUType,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate_F,cEndDate_F,rFactVL,rFlows_Local,rSDFlows_Local,cFlowNames_Local,iStat)
    
    !Transfer flows to return arguments
    iNFlows_Out = SIZE(rFlows_Local,DIM=1)
    DO indxMon=1,12
        rFlows(1:iNFlows_Out,indxMon)   = rFlows_Local(:,indxMon)
        rSDFlows(1:iNFlows_Out,indxMon) = rSDFlows_Local(:,indxMon)
    END DO
    
    !Concatonate flow names into string scalar and convert to C-Type
    CALL StringArray_To_StringScalar(cFlowNames_Local,cFlowNames_F,iLocArray)
    CALL String_Copy_F_C(cFlowNames_F , cFlowNames)
    
  END SUBROUTINE IW_Model_GetZBudget_MonthlyAverageFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET ANNUAL ZONE BUDGET FLOWS FOR A SELECTED ZONE 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_AnnualFlows(iZBudgetType,iZoneID,iLUType,iZExtent,iNZones,iElems,iLayers,iZoneIDs,iLenDate,cBeginDate,cEndDate,rFactVL,iNFlows_In,iNTimes_In,rFlows,iNFlows_Out,iNTimes_Out,iLenFlowNames,cFlowNames,iLocArray,iWaterYears,iStat) BIND(C,NAME='IW_Model_GetZBudget_AnnualFlows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_AnnualFlows
    INTEGER(C_INT),INTENT(IN)          :: iZBudgetType,iZoneID,iLUType,iZExtent,iNZones,iElems(iNZones),iLayers(iNZones),iZoneIDs(iNZones),iLenDate,iNFlows_In,iNTimes_In,iLenFlowNames
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: cBeginDate(iLenDate),cEndDate(iLenDate)
    REAL(C_DOUBLE),INTENT(IN)          :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)         :: rFlows(iNFlows_In,iNTimes_In)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cFlowNames(iLenFlowNames)
    INTEGER(C_INT),INTENT(OUT)         :: iNFlows_Out,iNTimes_Out,iLocArray(iNFlows_In),iWaterYears(iNTimes_In),iStat
    
    !Local variables
    INTEGER                       :: indxYear
    CHARACTER                     :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cFlowNames_F*iLenFlowNames
    INTEGER,ALLOCATABLE           :: iWaterYears_Local(:)
    REAL(8),ALLOCATABLE           :: rFlows_Local(:,:)
    CHARACTER(LEN=50),ALLOCATABLE :: cFlowNames_Local(:)
    
    !Convert C-type characters to F-type
    CALL String_Copy_C_F(cBeginDate , cBeginDate_F)
    CALL String_Copy_C_F(cEndDate , cEndDate_F)
    
    !Get full budget
    CALL Model%GetZBudget_AnnualFlows(iZBudgetType,iZoneID,iLUType,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate_F,cEndDate_F,rFactVL,rFlows_Local,cFlowNames_Local,iWaterYears_Local,iStat)
    
    !Transfer flows and water years to return arguments
    iNFlows_Out = SIZE(rFlows_Local,DIM=1)
    iNTimes_Out = SIZE(rFlows_Local,DIM=2)
    DO indxYear=1,iNTimes_Out
        rFlows(1:iNFlows_Out,indxYear) = rFlows_Local(:,indxYear)
        iWaterYears(indxYear)          = iWaterYears_Local(indxYear)
    END DO
    
    !Concatonate flow names into string scalar and convert to C-Type
    CALL StringArray_To_StringScalar(cFlowNames_Local,cFlowNames_F,iLocArray)
    CALL String_Copy_F_C(cFlowNames_F , cFlowNames)
    
  END SUBROUTINE IW_Model_GetZBudget_AnnualFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET ZONE BUDGET TIME SERIES DATA FROM A ZBUDGET FILE FOR A SELECTED ZONE AND SELECTED COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_TSData(iZBudgetType,iZoneID,iNCols,iCols,iZExtent,iNZones,iElems,iLayers,iZoneIDs,iLenDate,cBeginDate,cEndDate,iLenInterval,cInterval,rFactAR,rFactVL,rOutputDates,iNTimes_In,rOutputValues,iDataTypes,iNTimes_Out,iStat) BIND(C,NAME="IW_Model_GetZBudget_TSData")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_TSData
    INTEGER(C_INT),INTENT(IN)         :: iZBudgetType,iZoneID,iNCols,iCols(iNCols),iZExtent,iNZones,iElems(iNZones),iLayers(iNZones),iZoneIDs(iNZones),iNTimes_In,iLenDate,iLenInterval
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDate(iLenDate),cEndDate(iLenDate),cInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactAR,rFactVL
    REAL(C_DOUBLE),INTENT(OUT)        :: rOutputDates(iNTimes_In),rOutputValues(iNTimes_In,iNCols)    
    INTEGER,INTENT(OUT)               :: iDataTypes(iNCols),iNTimes_Out,iStat
    
    !Local variables
    CHARACTER :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cInterval_F*iLenInterval
    
    !Convert C-type strings to F-type strings
    CALL String_Copy_C_F(cBeginDate,cBeginDate_F)
    CALL String_Copy_C_F(cEndDate,cEndDate_F)
    CALL String_Copy_C_F(cInterval,cInterval_F)
    
    !Get the data
    CALL Model%GetZBudget_TSData(iZBudgetType,iZoneID,iCols,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate_F,cEndDate_F,cInterval_F,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,iNTimes_Out,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Convert Julian time to Excel-style Julian time
    rOutputDates(1:iNTimes_Out) = rOutputDates(1:iNTimes_Out) - REAL(2415020d0,C_DOUBLE)
   
  END SUBROUTINE IW_Model_GetZBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE CHANGE IN GW STORAGE FROM Z-BUDGET OUTPUT FOR A SELECTED ZONE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_CumGWStorChange(iZoneID,iZExtent,iNZones,iElems,iLayers,iZoneIDs,iLenDate,cBeginDate,cEndDate,iLenInterval,cInterval,rFactVL,rOutputDates,iNTimes_In,rCumGWStorChange,iNTimes_Out,iStat) BIND(C,NAME='IW_Model_GetZBudget_CumGWStorChange')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_CumGWStorChange
    INTEGER(C_INT),INTENT(IN)         :: iZoneID,iZExtent,iNZones,iElems(iNZones),iLayers(iNZones),iZoneIDs(iNZones),iNTimes_In,iLenDate,iLenInterval
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDate(iLenDate),cEndDate(iLenDate),cInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)        :: rOutputDates(iNTimes_In),rCumGWStorChange(iNTimes_In)    
    INTEGER,INTENT(OUT)               :: iNTimes_Out,iStat
    
    !Local variables
    CHARACTER           :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cInterval_F*iLenInterval
    REAL(8),ALLOCATABLE :: rDates(:),rStorChange(:)
    
    !Convert C-type strings to F-type strings
    CALL String_Copy_C_F(cBeginDate,cBeginDate_F)
    CALL String_Copy_C_F(cEndDate,cEndDate_F)
    CALL String_Copy_C_F(cInterval,cInterval_F)
    
    !Get the data
    CALL Model%GetZBudget_CumGWStorChange(iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate_F,cEndDate_F,cInterval_F,rFactVL,rDates,rStorChange,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Copy data to return arguments
    iNTimes_Out                     = SIZE(rDates)
    rOutputDates(1:iNTimes_Out)     = rDates
    rCumGWStorChange(1:iNTimes_Out) = rStorChange
    
    !Convert Julian time to Excel-style Julian time
    rOutputDates(1:iNTimes_Out) = rOutputDates(1:iNTimes_Out) - REAL(2415020d0,C_DOUBLE)
   
  END SUBROUTINE IW_Model_GetZBudget_CumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET ANNUAL CUMULATIVE CHANGE IN GW STORAGE FROM BUDGET OUTPUT FOR A SELECTED SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZBudget_AnnualCumGWStorChange(iZoneID,iZExtent,iNZones,iElems,iLayers,iZoneIDs,iLenDate,cBeginDate,cEndDate,rFactVL,iNTimes_In,rCumGWStorChange,iWaterYears,iNTimes_Out,iStat) BIND(C,NAME='IW_Model_GetZBudget_AnnualCumGWStorChange')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZBudget_AnnualCumGWStorChange
    INTEGER(C_INT),INTENT(IN)         :: iZoneID,iZExtent,iNZones,iElems(iNZones),iLayers(iNZones),iZoneIDs(iNZones),iNTimes_In,iLenDate
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDate(iLenDate),cEndDate(iLenDate)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactVL
    REAL(C_DOUBLE),INTENT(OUT)        :: rCumGWStorChange(iNTimes_In)    
    INTEGER,INTENT(OUT)               :: iWaterYears(iNTimes_In),iNTimes_Out,iStat
    
    !Local variables
    CHARACTER           :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate
    INTEGER,ALLOCATABLE :: iWaterYears_Local(:)
    REAL(8),ALLOCATABLE :: rStorChange(:)
    
    !Convert C-type strings to F-type strings
    CALL String_Copy_C_F(cBeginDate,cBeginDate_F)
    CALL String_Copy_C_F(cEndDate,cEndDate_F)
    
    !Get the data
    CALL Model%GetZBudget_AnnualCumGWStorChange(iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate_F,cEndDate_F,rFactVL,rStorChange,iWaterYears_Local,iStat) 
    IF (iStat .NE. 0) RETURN
    
    !Copy data to return arguments
    iNTimes_Out                     = SIZE(iWaterYears_Local)
    iWaterYears(1:iNTimes_Out)      = iWaterYears_Local
    rCumGWStorChange(1:iNTimes_Out) = rStorChange
    
  END SUBROUTINE IW_Model_GetZBudget_AnnualCumGWStorChange
  
  
  ! -------------------------------------------------------------
  ! --- GET NAME LIST FOR A SELECTED LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNames(iLocationType,iDimLocArray,iLocArray,iLenNamesList,cNamesList,iStat) BIND(C,NAME='IW_Model_GetNames')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNames
    INTEGER(C_INT),INTENT(IN)          :: iLocationType,iDimLocArray,iLenNamesList
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(iDimLocArray),iStat
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cNamesList(iLenNamesList)
    
    !Local variables
    CHARACTER(LEN=250) :: cLocalNamesList(iDimLocArray)
    CHARACTER          :: cNamesList_F*iLenNamesList
    
    !Get names
    CALL Model%GetNames(iLocationType,cLocalNamesList,iStat)
    
    !Compile the list in a single string variable
    CALL StringArray_To_StringScalar(cLocalNamesList,cNamesList_F,iLocArray)
    CALL String_Copy_F_C(cNamesList_F,cNamesList)
    
  END SUBROUTINE IW_Model_GetNames
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT A LAYER FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetGWHeads_ForALayer(iLayer,cOutputBeginDateAndTime,cOutputEndDateAndTime,iLenDateAndTime,rFact_LT,iNNodes,iNTime,rOutputDates,rGWHeads,iStat) BIND(C,NAME='IW_Model_GetGWHeads_ForALayer')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetGWHeads_ForALayer
    INTEGER(C_INT),INTENT(IN)         :: iLayer,iLenDateAndTime,iNNodes,iNTime
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cOutputBeginDateAndTime(iLenDateAndTime),cOutputEndDateAndTime(iLenDateAndTime)
    REAL(C_DOUBLE),INTENT(IN)         :: rFact_LT
    REAL(C_DOUBLE),INTENT(OUT)        :: rOutputDates(iNTime),rGWHeads(iNNodes,iNTime)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER :: cOutputBeginDateAndTime_F*iLenDateAndTime,cOutputEndDateAndTime_F*iLenDateAndTime
    
    !Initialize
    iStat = 0
    
    !C strings to F strings
    CALL String_Copy_C_F(cOutputBeginDateAndTime,cOutputBeginDateAndTime_F)
    CALL String_Copy_C_F(cOutputEndDateAndTime,cOutputEndDateAndTime_F)
    
    !Get data
    CALL Model%GetGWHeads_ForALayer(iLayer,cOutputBeginDateAndTime_F,cOutputEndDateAndTime_F,rFact_LT,rOutputDates,rGWHeads,iStat)
    IF (iStat .EQ. -1) THEN
        CALL Model%Kill()
        RETURN
    END IF
    
    !Convert Julian time to Excel-style Julian time
    rOutputDates = rOutputDates - REAL(2415020d0,C_DOUBLE)

  END SUBROUTINE IW_Model_GetGWHeads_ForALayer
  
    
  ! -------------------------------------------------------------
  ! --- GET ALL GW HEADS AT EACH (node,layer) COMBINATION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetGWHeads_All(iNNodes,iNLayers,iPrevious,rFact,Heads,iStat) BIND (C,NAME='IW_Model_GetGWHeads_All')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetGWHeads_All
    INTEGER(C_INT),INTENT(IN)  :: iNNodes,iNLayers,iPrevious
    REAL(C_DOUBLE),INTENT(IN)  :: rFact
    REAL(C_DOUBLE),INTENT(OUT) :: Heads(iNNodes,iNLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Initialize
    iStat = 0
    
    IF (iPrevious .EQ. 0) THEN
        CALL Model%GetGWHeads_All(.FALSE. , Heads)
    ELSE
        CALL Model%GetGWHeads_All(.TRUE. , Heads)
    END IF
    IF (rFact .NE. 1.0) Heads = Heads * rFact
    
  END SUBROUTINE IW_Model_GetGWHeads_All
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE AT EACH (node,layer) COMBINATION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSubsidence_All(iNNodes,iNLayers,rFact,Subs,iStat) BIND (C,NAME='IW_Model_GetSubsidence_All')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSubsidence_All
    INTEGER(C_INT),INTENT(IN)  :: iNNodes,iNLayers
    REAL(C_DOUBLE),INTENT(IN)  :: rFact
    REAL(C_DOUBLE),INTENT(OUT) :: Subs(iNNodes,iNLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Initialize
    iStat = 0
    
    CALL Model%GetSubsidence_All(Subs)
    IF (rFact .NE. 1.0) Subs = Subs * rFact
    
  END SUBROUTINE IW_Model_GetSubsidence_All
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM FLOW AT A STREAM NODE INDEX
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmFlow(iStrmNode,rFact,rFlow,iStat) BIND (C,NAME='IW_Model_GetStrmFlow')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmFlow
    INTEGER(C_INT),INTENT(IN)  :: iStrmNode
    REAL(C_DOUBLE),INTENT(IN)  :: rFact
    REAL(C_DOUBLE),INTENT(OUT) :: rFlow
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    rFlow = Model%GetStrmFlow(iStrmNode) * rFact
    
  END SUBROUTINE IW_Model_GetStrmFlow
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmFlows(iNStrmNodes,rFact,Flows,iStat) BIND (C,NAME='IW_Model_GetStrmFlows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmFlows
    INTEGER(C_INT),INTENT(IN)  :: iNStrmNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rFact
    REAL(C_DOUBLE),INTENT(OUT) :: Flows(iNStrmNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Initialize
    iStat = 0
    
    CALL Model%GetStrmFlows(Flows)
    IF (rFact .NE. 1.0) Flows = Flows * rFact
    
  END SUBROUTINE IW_Model_GetStrmFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM INFLOWS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmNInflows(iNInflows,iStat) BIND(C,NAME='IW_Model_GetStrmNInflows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmNInflows
    INTEGER(C_INT),INTENT(OUT) :: iNInflows,iStat
    
    iStat     = 0 
    iNInflows = Model%GetStrmNInflows()
    
  END SUBROUTINE IW_Model_GetStrmNInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmInflowNodes(iNNodes,iNodes,iStat) BIND(C,NAME='IW_Model_GetStrmInflowNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmInflowNodes
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    INTEGER(C_INT),INTENT(OUT) :: iNodes(iNNodes),iStat
    
    !Local variables
    INTEGER,ALLOCATABLE :: iNodes_Local(:)
    
    iStat = 0
    CALL Model%GetStrmInflowNodes(iNodes_Local)
    iNodes = iNodes_Local
    
  END SUBROUTINE IW_Model_GetStrmInflowNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmInflowIDs(iNNodes,IDs,iStat) BIND(C,NAME='IW_Model_GetStrmInflowIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmInflowIDs
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    INTEGER(C_INT),INTENT(OUT) :: IDs(iNNodes),iStat
    
    !Local variables
    INTEGER,ALLOCATABLE :: IDs_Local(:)
    
    iStat = 0
    CALL Model%GetStrmInflowIDs(IDs_Local)
    IDs = IDs_Local
    
  END SUBROUTINE IW_Model_GetStrmInflowIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOWS AT A SET OF INFLOWS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmInflows_AtSomeInflows(iNInflows,iInflows,rConvFactor,rInflows,iStat) BIND(C,NAME='IW_Model_GetStrmInflows_AtSomeInflows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmInflows_AtSomeInflows
    INTEGER(C_INT),INTENT(IN)  :: iNInflows,iInflows(iNInflows) 
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rInflows(iNInflows)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    CALL Model%GetStrmInflows_AtSomeInflows(iInflows,rInflows) 
    rInflows = rInflows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmInflows_AtSomeInflows
  

  ! -------------------------------------------------------------
  ! --- GET ALL STREAM STAGES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmStages(iNStrmNodes,rFact,Stages,iStat) BIND (C,NAME='IW_Model_GetStrmStages')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmStages
    INTEGER(C_INT),INTENT(IN)  :: iNStrmNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rFact
    REAL(C_DOUBLE),INTENT(OUT) :: Stages(iNStrmNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmStages(Stages,iStat)
    IF (rFact .NE. 1.0) Stages = Stages * rFact
    
  END SUBROUTINE IW_Model_GetStrmStages
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AVAILABLE HYDROGRAPH TYPES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNHydrographTypes(iNHydTypes,iStat) BIND(C,NAME='IW_Model_GetNHydrographTypes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNHydrographTypes
    INTEGER(C_INT),INTENT(OUT) :: iNHydTypes,iStat
    
    iStat      = 0
    iNHydTypes = Model%GetNHydrographTypes()
    
  END SUBROUTINE IW_Model_GetNHydrographTypes
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF AVAILABLE HYDROGRAPH TYPES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetHydrographTypeList(iNHydTypes,iLocArray,iLenHydTypeList,cHydTypeList,iHydLocationTypeList,iStat) BIND(C,NAME='IW_Model_GetHydrographTypeList')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetHydrographTypeList
    INTEGER(C_INT),INTENT(IN)          :: iNHydTypes,iLenHydTypeList
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cHydTypeList(iLenHydTypeList)
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(iNHydTypes),iHydLocationTypeList(iNHydTypes),iStat
    
    !Local variables
    INTEGER,ALLOCATABLE            :: iHydLocTypeList_Local(:),iHydCompList_Local(:) 
    CHARACTER                      :: cHydTypeList_F*iLenHydTypeList
    CHARACTER(LEN=100),ALLOCATABLE :: cHydTypeList_Local(:)
    CHARACTER(LEN=500),ALLOCATABLE :: cHydFileList_Local(:)
    
    !Initialize
    iStat = 0
    
    !Retrieve data
    CALL Model%GetHydrographTypeList(cHydTypeList_Local , iHydLocTypeList_Local , iHydCompList_Local , cHydFileList_Local)
    
    !Convert string array to string scalar
    CALL StringArray_To_StringScalar(cHydTypeList_Local,cHydTypeList_F,iLocArray)
    
    !Copy local data to return arguments
    CALL String_Copy_F_C(cHydTypeList_F,cHydTypeList)
    iHydLocationTypeList = iHydLocTypeList_Local
    
  END SUBROUTINE IW_Model_GetHydrographTypeList
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF PRINTED HYDROGRAPHS FOR A SPECIFIC HYDROGRAPH TYPE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNHydrographs(iLocationType,NHydrographs,iStat) BIND(C,NAME='IW_Model_GetNHydrographs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNHydrographs
    INTEGER(C_INT),INTENT(IN)  :: iLocationType
    INTEGER(C_INT),INTENT(OUT) :: NHydrographs,iStat
    
    iStat        = 0
    NHydrographs = Model%GetNHydrographs(iLocationType)
    
  END SUBROUTINE IW_Model_GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH IDS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetHydrographIDs(iLocationType,NHydrographs,IDs,iStat) BIND(C,NAME='IW_Model_GetHydrographIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetHydrographIDs
    INTEGER(C_INT),INTENT(IN)  :: iLocationType,NHydrographs
    INTEGER(C_INT),INTENT(OUT) :: IDs(NHydrographs),iStat
    
    iStat = 0
    CALL Model%GetHydrographIDs(iLocationType,IDs)
    
  END SUBROUTINE IW_Model_GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET COORDINATES OF PRINTED HYDROGRAPHS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetHydrographCoordinates(iLocationType,NHydrographs,X,Y,iStat) BIND(C,NAME='IW_Model_GetHydrographCoordinates')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetHydrographCoordinates
    INTEGER(C_INT),INTENT(IN)  :: iLocationType,NHydrographs
    REAL(C_DOUBLE),INTENT(OUT) :: X(NHydrographs),Y(NHydrographs)
    INTEGER(C_INT),INTENT(OUT) :: iStat
      
    CALL Model%GetHydrographCoordinates(iLocationType,X,Y,iStat)
    
  END SUBROUTINE IW_Model_GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH FOR A GIVEN HYDROGRAPH INDEX 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetHydrograph(iHydType,iHydIndex,iLayer,iLenDate,cBeginDate,cEndDate,iLenInterval,cInterval,rFactLT,rFactVL,iNTimes_In,rOutputDates,rOutputValues,iDataUnitType,iNTimes_Out,iStat) BIND(C,NAME='IW_Model_GetHydrograph')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetHydrograph
    INTEGER(C_INT),INTENT(IN)         :: iHydType,iHydIndex,iLayer,iLenDate,iLenInterval,iNTimes_In
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDate(iLenDate),cEndDate(iLenDate),cInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)         :: rFactLT,rFactVL
    REAL(C_DOUBLE),INTENT(OUT)        :: rOutputDates(iNTimes_In),rOutputValues(iNTimes_In)
    INTEGER(C_INT),INTENT(OUT)        :: iDataUnitType,iNTimes_Out,iStat
    
    !Local variables
    CHARACTER           :: cBeginDate_F*iLenDate,cEndDate_F*iLenDate,cInterval_F*iLenInterval
    REAL(8),ALLOCATABLE :: rDates_Local(:),rValues_Local(:) 
    
    !Convert C strings to F strings
    CALL String_Copy_C_F(cBeginDate , cBeginDate_F)
    CALL String_Copy_C_F(cEndDate , cEndDate_F)
    CALL String_Copy_C_F(cInterval , cInterval_F)
    
    !Get data
    CALL Model%GetHydrograph(iHydType,iHydIndex,iLayer,rFactLT,rFactVL,cBeginDate_F,cEndDate_F,cInterval_F,iDataUnitType,rDates_Local,rValues_Local,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Copy local data to return arguments; also convert Julian date to Excel-style Julian date
    iNTimes_Out                  = SIZE(rDates_Local)
    rOutputDates(1:iNTimes_Out)  = rDates_Local - REAL(2415020d0,C_DOUBLE)
    rOutputValues(1:iNTimes_Out) = rValues_Local
    
  END SUBROUTINE IW_Model_GetHydrograph  
    
  
  ! -------------------------------------------------------------
  ! --- GET NODE IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNodeIDs(NNodes,IDs,iStat) BIND(C,NAME='IW_Model_GetNodeIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNodeIDs
    INTEGER(C_INT),INTENT(IN)  :: NNodes
    INTEGER(C_INT),INTENT(OUT) :: IDs(NNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    CALL Model%GetNodeIDs(IDs)
    
  END SUBROUTINE IW_Model_GetNodeIDs
  
    
  ! -------------------------------------------------------------
  ! --- GET NODE COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNodeXY(NNodes,X,Y,iStat) BIND(C,NAME='IW_Model_GetNodeXY')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNodeXY
    INTEGER(C_INT),INTENT(IN)  :: NNodes
    REAL(C_DOUBLE),INTENT(OUT) :: X(NNodes),Y(NNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    CALL Model%GetNodeXY(X,Y)
    
  END SUBROUTINE IW_Model_GetNodeXY
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNNodes(NNodes,iStat) BIND(C,NAME='IW_Model_GetNNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNNodes
    INTEGER(C_INT),INTENT(OUT) :: NNodes,iStat
    
    iStat  = 0
    NNodes = Model%GetNNodes()
    
  END SUBROUTINE IW_Model_GetNNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetElementIDs(NElem,IDs,iStat) BIND(C,NAME='IW_Model_GetElementIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetElementIDs
    INTEGER(C_INT),INTENT(IN)  :: NElem
    INTEGER(C_INT),INTENT(OUT) :: IDs(NElem),iStat
    
    iStat = 0
    CALL Model%GetElementIDs(IDs)
    
  END SUBROUTINE IW_Model_GetElementIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNElements(NElem,iStat) BIND(C,NAME='IW_Model_GetNElements')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNElements
    INTEGER(C_INT),INTENT(OUT) :: NElem,iStat
    
    iStat = 0
    NElem = Model%GetNElements()
    
  END SUBROUTINE IW_Model_GetNElements
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNLayers(NLayers,iStat) BIND(C,NAME='IW_Model_GetNLayers')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNLayers
    INTEGER(C_INT),INTENT(OUT) :: NLayers,iStat
    
    iStat   = 0
    NLayers = Model%GetNLayers()
    
  END SUBROUTINE IW_Model_GetNLayers


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SUBREGIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNSubregions(NSubregions,iStat) BIND(C,NAME='IW_Model_GetNSubregions')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNSubregions
    INTEGER(C_INT),INTENT(OUT) :: NSubregions,iStat
    
    iStat       = 0
    NSubregions = Model%GetNSubregions()
    
  END SUBROUTINE IW_Model_GetNSubregions
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGION IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSubregionIDs(NSubregion,IDs,iStat) BIND(C,NAME='IW_Model_GetSubregionIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSubregionIDs
    INTEGER(C_INT),INTENT(IN)  :: NSubregion
    INTEGER(C_INT),INTENT(OUT) :: IDs(NSubregion),iStat
    
    iStat = 0
    CALL Model%GetSubregionIDs(IDs)
    
  END SUBROUTINE IW_Model_GetSubregionIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NAME OF A SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSubregionName(iRegion,iLen,cName,iStat) BIND(C,NAME='IW_Model_GetSubregionName')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSubregionName
    INTEGER(C_INT),INTENT(IN)          :: iRegion,iLen
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cName(iLen)
    INTEGER(C_INT),INTENT(OUT)         :: iStat    
    
    !Local variables
    CHARACTER :: cName_F*iLen
    
    iStat = 0
    
    CALL Model%GetSubregionName(iRegion,cName_F)
    
    CALL String_Copy_F_C(cName_F,cName)
    
  END SUBROUTINE IW_Model_GetSubregionName
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT SUBREGIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetElemSubregions(NElem,ElemSubregions,iStat) BIND(C,NAME='IW_Model_GetElemSubregions')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetElemSubregions
    INTEGER(C_INT),INTENT(IN)  :: NElem
    INTEGER(C_INT),INTENT(OUT) :: ElemSubregions(NElem),iStat
    
    iStat = 0
    
    CALL Model%GetElemSubregions(ElemSubregions)
    
  END SUBROUTINE IW_Model_GetElemSubregions
  
  
  ! -------------------------------------------------------------
  ! --- GET STRATIGRAPHY AT X-Y COORDINATE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStratigraphy_AtXYCoordinate(iNLayers,rX,rY,rGSElev,rTopElevs,rBottomElevs,iStat) BIND(C,NAME='IW_Model_GetStratigraphy_AtXYCoordinate')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStratigraphy_AtXYCoordinate
    INTEGER(C_INT),INTENT(IN)  :: iNLayers
    REAL(C_DOUBLE),INTENT(IN)  :: rX,rY
    REAL(C_DOUBLE),INTENT(OUT) :: rGSElev,rTopElevs(iNLayers),rBottomElevs(iNLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStratigraphy_AtXYCoordinate(rX,rY,rGSElev,rTopElevs,rBottomElevs,iStat)
    
  END SUBROUTINE IW_Model_GetStratigraphy_AtXYCoordinate
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUND SURFACE ELEVATION 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetGSElev(NNodes,GSElev,iStat) BIND(C,NAME='IW_Model_GetGSElev')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetGSElev
    INTEGER(C_INT),INTENT(IN)  :: NNodes
    REAL(C_DOUBLE),INTENT(OUT) :: GSElev(NNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    
    CALL Model%GetGSElev(GSElev)
    
  END SUBROUTINE IW_Model_GetGSElev
  

  ! -------------------------------------------------------------
  ! --- GET ELEVATIONS OF AQUIFER TOPS 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquiferTopElev(NNodes,NLayers,TopElev,iStat) BIND(C,NAME='IW_Model_GetAquiferTopElev')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquiferTopElev
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: TopElev(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    
    CALL Model%GetAquiferTopElev(TopElev)
    
  END SUBROUTINE IW_Model_GetAquiferTopElev


  ! -------------------------------------------------------------
  ! --- GET ELEVATIONS OF AQUIFER BOTTOMS 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquiferBottomElev(NNodes,NLayers,BottomElev,iStat) BIND(C,NAME='IW_Model_GetAquiferBottomElev')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquiferBottomElev
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: BottomElev(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    
    CALL Model%GetAquiferBottomElev(BottomElev)
    
  END SUBROUTINE IW_Model_GetAquiferBottomElev

  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER HORIZONTAL K 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquiferHorizontalK(NNodes,NLayers,Kh,iStat) BIND(C,NAME='IW_Model_GetAquiferHorizontalK')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquiferHorizontalK
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: Kh(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetAquiferHorizontalK(Kh,iStat)
    
  END SUBROUTINE IW_Model_GetAquiferHorizontalK

  
  ! -------------------------------------------------------------
  ! --- GET AQUITARD VERTICAL K 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquitardVerticalK(NNodes,NLayers,Kv,iStat) BIND(C,NAME='IW_Model_GetAquitardVerticalK')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquitardVerticalK
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: Kv(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetAquitardVerticalK(Kv,iStat)
    
  END SUBROUTINE IW_Model_GetAquitardVerticalK

  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER VERTICAL K 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquiferVerticalK(NNodes,NLayers,Kv,iStat) BIND(C,NAME='IW_Model_GetAquiferVerticalK')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquiferVerticalK
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: Kv(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetAquiferVerticalK(Kv,iStat)
    
  END SUBROUTINE IW_Model_GetAquiferVerticalK

  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER SPECIFIC YIELD 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquiferSy(NNodes,NLayers,Sy,iStat) BIND(C,NAME='IW_Model_GetAquiferSy')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquiferSy
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: Sy(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetAquiferSy(Sy,iStat)
    
  END SUBROUTINE IW_Model_GetAquiferSy

  
  ! -------------------------------------------------------------
  ! --- GET AQUIFER STORAGE COEFFICIENT (AFTER SPECIFIC STOARGE IS MULTIPLIED BY AQUIFER THICKNESS)
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquiferSs(NNodes,NLayers,Ss,iStat) BIND(C,NAME='IW_Model_GetAquiferSs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquiferSs
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: Ss(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetAquiferSs(Ss,iStat)
    
  END SUBROUTINE IW_Model_GetAquiferSs

  
  ! -------------------------------------------------------------
  ! --- GET ALL AQUIFER PARAMETERS IN ONE SHOT
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetAquiferParameters(NNodes,NLayers,Kh,AquiferKv,AquitardKv,Sy,Ss,iStat) BIND(C,NAME='IW_Model_GetAquiferParameters')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetAquiferParameters
    INTEGER(C_INT),INTENT(IN)  :: NNodes,NLayers
    REAL(C_DOUBLE),INTENT(OUT) :: Kh(NNodes,NLayers),AquiferKv(NNodes,NLayers),AquitardKv(NNodes,NLayers),Sy(NNodes,NLayers),Ss(NNodes,NLayers)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetAquiferParameters(Kh,AquiferKv,AquitardKv,Sy,Ss,iStat)
    
  END SUBROUTINE IW_Model_GetAquiferParameters

  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetElementConfigData(iElem,iDim,Nodes,iStat) BIND(C,NAME='IW_Model_GetElementConfigData')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetElementConfigData
    INTEGER(C_INT),INTENT(IN)  :: iElem,iDim
    INTEGER(C_INT),INTENT(OUT) :: Nodes(iDim),iStat
    
    iStat = 0
    
    CALL Model%GetElementConfigData(iElem,Nodes)
    
  END SUBROUTINE IW_Model_GetElementConfigData
  
  
  ! -------------------------------------------------------------
  ! --- GET REACH INDICES FOR SOME STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReaches_ForStrmNodes(iNNodes,iStrmNodes,iReachs,iStat) BIND(C,NAME='IW_Model_GetReaches_ForStrmNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReaches_ForStrmNodes
    INTEGER(C_INT),INTENT(IN)   :: iNNodes,iStrmNodes(iNNodes)
    INTEGER(C_INT),INTENT(OUT)  :: iReachs(iNNodes),iStat
    
    CALL Model%GetReaches_ForStrmNodes(iStrmNodes,iReachs,iStat)
    
  END SUBROUTINE IW_Model_GetReaches_ForStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmNodeIDs(NStrmNodes,IDs,iStat) BIND(C,NAME='IW_Model_GetStrmNodeIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmNodeIDs
    INTEGER(C_INT),INTENT(IN)  :: NStrmNodes
    INTEGER(C_INT),INTENT(OUT) :: IDs(NStrmNodes),iStat
    
    iStat = 0
    CALL Model%GetStrmNodeIDs(IDs)
    
  END SUBROUTINE IW_Model_GetStrmNodeIDs


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNStrmNodes(NStrmNodes,iStat) BIND(C,NAME='IW_Model_GetNStrmNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNStrmNodes
    INTEGER(C_INT),INTENT(OUT) :: NStrmNodes,iStat
    
    iStat      = 0
    NStrmNodes = Model%GetNStrmNodes()
    
  END SUBROUTINE IW_Model_GetNStrmNodes


  ! -------------------------------------------------------------
  ! --- GET STREAM REACH IDS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachIDs(iNReaches,IDs,iStat) BIND(C,NAME='IW_Model_GetReachIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachIDs
    INTEGER(C_INT),INTENT(IN)  :: iNReaches
    INTEGER(C_INT),INTENT(OUT) :: IDs(iNReaches),iStat
    
    iStat = 0
    CALL Model%GetStrmReachIDs(IDs)
    
  END SUBROUTINE IW_Model_GetReachIDs


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmNUpstrmNodes(iStrmNode,iNNodes,iStat) BIND(C,NAME='IW_Model_GetStrmNUpstrmNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmNUpstrmNodes
     INTEGER(C_INT),INTENT(IN) :: iStrmNode
     INTEGER(C_INT),INTENT(OUT) :: iNNodes, iStat
     
     iStat   = 0
     iNNodes = Model%GetStrmNUpstrmNodes(iStrmNode)
     
  END SUBROUTINE IW_Model_GetStrmNUpstrmNodes
     
     
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES FLOWING INTO ANOTHER NODE 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmUpstrmNodes(iStrmNode,iNNodes,iUpstrmNodes,iStat) BIND(C,NAME='IW_Model_GetStrmUpstrmNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmUpstrmNodes
    INTEGER(C_INT),INTENT(IN)  :: iStrmNode,iNNodes
    INTEGER(C_INT),INTENT(OUT) :: iUpstrmNodes(iNNodes),iStat
    
    !Local variables
    INTEGER,ALLOCATABLE :: iNodes_Local(:)
    
    iStat = 0
    CALL Model%GetStrmUpstrmNodes(iStrmNode,iNodes_Local)
    iUpstrmNodes = iNodes_Local
    
  END SUBROUTINE IW_Model_GetStrmUpstrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF STREAM NODE ID 1 IS UPSTREAM OF STREAM NODE ID 2, CONSIDERING THE ENTIRE NETWORK
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_IsStrmUpstreamNode(iStrmNode1,iStrmNode2,iUpstrm,iStat) BIND (C,NAME='IW_Model_IsStrmUpstreamNode')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_IsStrmUpstreamNode
    INTEGER(C_INT),INTENT(IN)   :: iStrmNode1,iStrmNode2
    INTEGER(C_INT),INTENT(OUT)  :: iUpstrm,iStat
    
    !Local variables
    LOGICAL :: lUpstrm
    
    CALL Model%IsStrmUpstreamNode(iStrmNode1,iStrmNode2,lUpstrm,iStat)
    IF (lUpstrm) THEN
        iUpstrm = 1
    ELSE
        iUpstrm = 0
    END IF
    
  END SUBROUTINE IW_Model_IsStrmUpstreamNode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM REACHES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNReaches(NReach,iStat) BIND(C,NAME='IW_Model_GetNReaches')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNReaches
    INTEGER(C_INT),INTENT(OUT) :: NReach,iStat
    
    iStat  = 0
    NReach = Model%GetNReaches()
    
  END SUBROUTINE IW_Model_GetNReaches


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF REACHES THAT ARE IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachNUpstrmReaches(iReach,iNReaches,iStat) BIND(C,NAME='IW_Model_GetReachNUpstrmReaches')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachNUpstrmReaches
    INTEGER(C_INT),INTENT(IN)  :: iReach
    INTEGER(C_INT),INTENT(OUT) :: iNReaches,iStat
    
    iStat     = 0
    iNReaches = Model%GetReachNUpstrmReaches(iReach)
    
  END SUBROUTINE IW_Model_GetReachNUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET REACHES IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachUpstrmReaches(iReach,iNReaches,iUpstrmReaches,iStat) BIND(C,NAME='IW_Model_GetReachUpstrmReaches')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachUpstrmReaches
    INTEGER(C_INT),INTENT(IN)  :: iReach,iNReaches
    INTEGER(C_INT),INTENT(OUT) :: iUpstrmReaches(iNReaches),iStat
    
    !Local variables
    INTEGER,ALLOCATABLE :: iUpstrmReaches_Local(:)
    
    iStat = 0
    CALL Model%GetReachUpstrmReaches(iReach,iUpstrmReaches_Local)
    iUpstrmReaches = iUpstrmReaches_Local

  END SUBROUTINE IW_Model_GetReachUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DATA POINTS IN STREAM RATING TABLE FOR A STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNStrmRatingTablePoints(iStrmNode,N,iStat) BIND(C,NAME='IW_Model_GetNStrmRatingTablePoints')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNStrmRatingTablePoints
    INTEGER(C_INT),INTENT(IN)  :: iStrmNode
    INTEGER(C_INT),INTENT(OUT) :: N,iStat
    
    iStat = 0
    N     = Model%GetNRatingTablePoints(iStrmNode)
    
  END SUBROUTINE IW_Model_GetNStrmRatingTablePoints


  ! -------------------------------------------------------------
  ! --- GET ALL REACH UPSTREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachUpstrmNodes(NReaches,iNodes,iStat) BIND(C,NAME='IW_Model_GetReachUpstrmNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachUpstrmNodes
    INTEGER(C_INT),INTENT(IN)  :: NReaches
    INTEGER(C_INT),INTENT(OUT) :: iNodes(NReaches),iStat
    
    iStat = 0
    CALL Model%GetReachUpstrmNodes(iNodes)
        
  END SUBROUTINE IW_Model_GetReachUpstrmNodes


  ! -------------------------------------------------------------
  ! --- GET ALL REACH DOWNSTREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachDownstrmNodes(NReaches,iNodes,iStat) BIND(C,NAME='IW_Model_GetReachDownstrmNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachDownstrmNodes
    INTEGER(C_INT),INTENT(IN)  :: NReaches
    INTEGER(C_INT),INTENT(OUT) :: iNodes(NReaches),iStat
    
    iStat = 0
    CALL Model%GetReachDownstrmNodes(iNodes)
    
  END SUBROUTINE IW_Model_GetReachDownstrmNodes


  ! -------------------------------------------------------------
  ! --- GET ALL REACH OUTFLOW DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachOutflowDest(NReaches,iDest,iStat) BIND(C,NAME='IW_Model_GetReachOutflowDest')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachOutflowDest
    INTEGER(C_INT),INTENT(IN)  :: NReaches
    INTEGER(C_INT),INTENT(OUT) :: iDest(NReaches),iStat
    
    
    iStat = 0
    CALL Model%GetReachOutflowDest(iDest)
        
  END SUBROUTINE IW_Model_GetReachOutflowDest


  ! -------------------------------------------------------------
  ! --- GET ALL REACH OUTFLOW DESTINATION TYPES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachOutflowDestTypes(NReaches,iDestType,iStat) BIND(C,NAME='IW_Model_GetReachOutflowDestTypes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachOutflowDestTypes
    INTEGER(C_INT),INTENT(IN)  :: NReaches
    INTEGER(C_INT),INTENT(OUT) :: iDestType(NReaches),iStat
    
    iStat = 0
    CALL Model%GetReachOutflowDestTypes(iDestType)
        
  END SUBROUTINE IW_Model_GetReachOutflowDestTypes


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES FOR A REACH
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachNNodes(iReach,iReachNNodes,iStat) BIND(C,NAME='IW_Model_GetReachNNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachNNodes
    INTEGER(C_INT),INTENT(IN)  :: iReach
    INTEGER(C_INT),INTENT(OUT) :: iReachNNodes,iStat
    
    iStat        = 0
    iReachNNodes = Model%GetReachNNodes(iReach)
    
  END SUBROUTINE IW_Model_GetReachNNodes


  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER NODES FOR EACH STREAM NODE AT A REACH
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachGWNodes(iReach,NNodes,iGWNodes,iStat) BIND(C,NAME='IW_Model_GetReachGWNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachGWNodes
    INTEGER(C_INT),INTENT(IN)  :: iReach,NNodes
    INTEGER(C_INT),INTENT(OUT) :: iGWNodes(NNodes),iStat
    
    iStat = 0
    
    CALL Model%GetReachGWNodes(iReach,NNodes,iGWNodes)
    
  END SUBROUTINE IW_Model_GetReachGWNodes


  ! -------------------------------------------------------------
  ! --- GET STREAM NODES FOR A GIVEN REACH 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetReachStrmNodes(iReach,iNNodes,iStrmNodes,iStat) BIND(C,NAME='IW_Model_GetReachStrmNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetReachStrmNodes
    INTEGER(C_INT),INTENT(IN)  :: iReach,iNNodes
    INTEGER(C_INT),INTENT(OUT) :: iStrmNodes(iNNodes),iStat
    
    !Local variables
    INTEGER,ALLOCATABLE :: iStrmNodes_Local(:)
    
    CALL Model%GetReachStrmNodes(iReach,iStrmNodes_Local,iStat) 
    IF (iStat .EQ. 0) iStrmNodes = iStrmNodes_Local
    
  END SUBROUTINE IW_Model_GetReachStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmBottomElevs(NNodes,rElevs,iStat) BIND(C,NAME='IW_Model_GetStrmBottomElevs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmBottomElevs
    INTEGER(C_INT),INTENT(IN)  :: NNodes
    REAL(C_DOUBLE),INTENT(OUT) :: rElevs(NNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmBottomElevs(rElevs,iStat)
    
  END SUBROUTINE IW_Model_GetStrmBottomElevs
  

  ! -------------------------------------------------------------
  ! --- GET STREAM RATING TABLE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmRatingTable(iStrmNode,NPoints,Stage,Flow,iStat) BIND(C,NAME='IW_Model_GetStrmRatingTable')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmRatingTable
    INTEGER(C_INT),INTENT(IN)  :: iStrmNode,NPoints
    REAL(C_DOUBLE),INTENT(OUT) :: Stage(NPoints),Flow(NPoints)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    
    CALL Model%GetStrmRatingTable(iStrmNode,NPoints,Stage,Flow)
    
  END SUBROUTINE IW_Model_GetStrmRatingTable

  
  ! -------------------------------------------------------------
  ! --- GET NET INFLOW (EXCLUDING DIVERSIONS AND BOUNDARY INFLOWS) AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmNetInflows_ExcDivsInflows(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmNetInflows_ExcDivsInflows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmNetInflows_ExcDivsInflows
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Local variables
    REAL(8) :: rFlowsTemp(iNNodes)
        
    !Tributary inflows
    CALL Model%GetStrmTributaryInflows(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Rainfall runoff
    CALL Model%GetStrmRainfallRunoff(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Return flow
    CALL Model%GetStrmReturnFlows(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Tile drains
    CALL Model%GetStrmTileDrains(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Riparian ET
    CALL Model%GetStrmRiparianETs(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows - rFlowsTemp
    
    !Gain from GW
    CALL Model%GetStrmGainFromGW(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Gain from lakes
    CALL Model%GetStrmGainFromLakes(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Net bypass Inflows
    CALL Model%GetStrmNetBypassInflows(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
        
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmNetInflows_ExcDivsInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET NET INFLOW (EXCLUDING DIVERSIONS, BOUNDARY INFLOWS AND GW INTERACTION) AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmNetInflows_ExcDivsInflowsGW(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmNetInflows_ExcDivsInflowsGW')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmNetInflows_ExcDivsInflowsGW
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Local variables
    REAL(8) :: rFlowsTemp(iNNodes)
        
    !Tributary inflows
    CALL Model%GetStrmTributaryInflows(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Rainfall runoff
    CALL Model%GetStrmRainfallRunoff(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Return flow
    CALL Model%GetStrmReturnFlows(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Tile drains
    CALL Model%GetStrmTileDrains(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Riparian ET
    CALL Model%GetStrmRiparianETs(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows - rFlowsTemp
    
    !Gain from lakes
    CALL Model%GetStrmGainFromLakes(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
    
    !Net bypass Inflows
    CALL Model%GetStrmNetBypassInflows(rFlowsTemp,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    rFlows = rFlows + rFlowsTemp
        
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmNetInflows_ExcDivsInflowsGW
  
  
  ! -------------------------------------------------------------
  ! --- GET TRIBUTARY INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmTributaryInflows(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmTributaryInflows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmTributaryInflows
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmTributaryInflows(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmTributaryInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET RAINFALL RUNOFF AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmRainfallRunoff(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmRainfallRunoff')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmRainfallRunoff
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmRainfallRunoff(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmRainfallRunoff
  
  
  ! -------------------------------------------------------------
  ! --- GET RETURN FLOW AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmReturnFlows(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmReturnFlows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmReturnFlows
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmReturnFlows(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmReturnFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET POND DRAINS INTO ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmPondDrains(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmPondDrains')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmPondDrains
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmPondDrains(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmPondDrains
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAINS INTO ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmTileDrains(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmTileDrains')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmTileDrains
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmTileDrains(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmTileDrains
  
  
  ! -------------------------------------------------------------
  ! --- GET RIPARIAN ET FROM ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmRiparianETs(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmRiparianETs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmRiparianETs
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmRiparianETs(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmRiparianETs
  
  
  ! -------------------------------------------------------------
  ! --- GET GAIN FROM GROUNDWATER AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmGainFromGW(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmGainFromGW')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmGainFromGW
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmGainFromGW(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor
    
  END SUBROUTINE IW_Model_GetStrmGainFromGW
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOWS FROM LAKES AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmGainFromLakes(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmGainFromLakes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmGainFromLakes
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmGainFromLakes(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor

  END SUBROUTINE IW_Model_GetStrmGainFromLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET NET BYPASS INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmNetBypassInflows(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmNetBypassInflows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmNetBypassInflows
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmNetBypassInflows(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor

  END SUBROUTINE IW_Model_GetStrmNetBypassInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS INFLOWS (DIFFERENT THAN NET INFLOWS) AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmBypassInflows(iNNodes,rConvFactor,rFlows,iStat) BIND(C,NAME='IW_Model_GetStrmBypassInflows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmBypassInflows
    INTEGER(C_INT),INTENT(IN)  :: iNNodes
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rFlows(iNNodes)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmBypassInflows(rFlows,iStat)
    IF (iStat .EQ. -1) THEN
        rFlows = 0.0
        RETURN
    END IF
    
    !Unit conversion
    rFlows = rFlows * rConvFactor

  END SUBROUTINE IW_Model_GetStrmBypassInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL DIVERSIONS AT SOME DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmActualDiversions_AtSomeDiversions(iNDivs,iDivs,rConvFactor,rDivs,iStat) BIND(C,NAME='IW_Model_GetStrmActualDiversions_AtSomeDiversions')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmActualDiversions_AtSomeDiversions
    INTEGER(C_INT),INTENT(IN)  :: iNDivs,iDivs(iNDivs)
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rDivs(iNDivs)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetStrmActualDiversions_AtSomeDiversions(iDivs,rDivs,iStat)
    IF (iStat .EQ. -1) THEN
        rDivs = 0.0
        RETURN
    END IF
    
    !Convert diversions
    rDivs = rDivs * rConvFactor

  END SUBROUTINE IW_Model_GetStrmActualDiversions_AtSomeDiversions
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES FOR A GIVEN SET OF DIVERSION INDICES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmDiversionsExportNodes(iNDivs,iDivList,iStrmNodeList,iStat) BIND(C,NAME='IW_Model_GetStrmDiversionsExportNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmDiversionsExportNodes
    INTEGER(C_INT),INTENT(IN)  :: iNDivs,iDivList(iNDivs)
    INTEGER(C_INT),INTENT(OUT) :: iStrmNodeList(iNDivs),iStat
    
    iStat = 0
    CALL Model%GetStrmDiversionsExportNodes(iDivList,iStrmNodeList)
    
  END SUBROUTINE IW_Model_GetStrmDiversionsExportNodes


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS SERVED BY A DIVERSION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmDiversionNElems(iDiv,iNElems,iStat) BIND(C,NAME='IW_Model_GetStrmDiversionNElems')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmDiversionNElems
    INTEGER(C_INT),INTENT(IN)  :: iDiv
    INTEGER(C_INT),INTENT(OUT) :: iNElems,iStat
    
    iStat   = 0
    iNElems = Model%GetStrmDiversionNElems(iDiv)
    
  END SUBROUTINE IW_Model_GetStrmDiversionNElems


  ! -------------------------------------------------------------
  ! --- GET INDICES OF ELEMENTS SERVED BY A DIVERSION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetStrmDiversionElems(iDiv,iNElems,iElems,iStat) BIND(C,NAME='IW_Model_GetStrmDiversionElems')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetStrmDiversionElems
    INTEGER(C_INT),INTENT(IN)  :: iDiv,iNElems
    INTEGER(C_INT),INTENT(OUT) :: iElems(iNElems),iStat
    
    !Local variables
    INTEGER,ALLOCATABLE :: iELems_Local(:)
    
    iStat = 0
    CALL Model%GetStrmDiversionElems(iDiv,iElems_Local)
    iElems = iElems_Local
    
  END SUBROUTINE IW_Model_GetStrmDiversionElems


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNDiversions(iNDiversions,iStat) BIND(C,NAME='IW_Model_GetNDiversions')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNDiversions
    INTEGER(C_INT),INTENT(OUT) :: iNDiversions,iStat
    
    iNDiversions = Model%GetNDiversions()
    iStat        = 0
        
  END SUBROUTINE IW_Model_GetNDiversions

  
  ! -------------------------------------------------------------
  ! --- GET DIVERSION IDS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetDiversionIDs(iNDiversions,iDiversionIDs,iStat) BIND(C,NAME='IW_Model_GetDiversionIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetDiversionIDs
    INTEGER(C_INT),INTENT(IN)  :: iNDiversions
    INTEGER(C_INT),INTENT(OUT) :: iDiversionIDs(iNDiversions),iStat
    
    iStat = 0
    CALL Model%GetDiversionIDs(iDiversionIDs)
        
  END SUBROUTINE IW_Model_GetDiversionIDs

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BYPASSES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNBypasses(iNBypasses,iStat) BIND(C,NAME='IW_Model_GetNBypasses')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNBypasses
    INTEGER(C_INT),INTENT(OUT) :: iNBypasses,iStat
    
    CALL Model%GetNBypasses(iNBypasses,iStat)
        
  END SUBROUTINE IW_Model_GetNBypasses

  
  ! -------------------------------------------------------------
  ! --- GET BYPASS IDS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBypassIDs(iNBypasses,iBypassIDs,iStat) BIND(C,NAME='IW_Model_GetBypassIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBypassIDs
    INTEGER(C_INT),INTENT(IN)  :: iNBypasses
    INTEGER(C_INT),INTENT(OUT) :: iBypassIDs(iNBypasses),iStat
    
    iStat = 0
    CALL Model%GetBypassIDs(iBypassIDs)
        
  END SUBROUTINE IW_Model_GetBypassIDs

  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES FOR A GIVEN SET OF BYPASS INDICES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBypassExportNodes(iNBypass,iBypassList,iStrmNodeList,iStat) BIND(C,NAME='IW_Model_GetBypassExportNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBypassExportNodes
    INTEGER(C_INT),INTENT(IN)  :: iNBypass,iBypassList(iNBypass)
    INTEGER(C_INT),INTENT(OUT) :: iStrmNodeList(iNBypass),iStat
    
    !Local variables
    INTEGER :: indx,iDummy
    
    iStat = 0
    DO indx=1,iNBypass
        CALL Model%GetBypassDiversionOriginDestData(.TRUE.,iBypassList(indx),iStrmNodeList(indx),iDummy,iDummy)
    END DO
    
  END SUBROUTINE IW_Model_GetBypassExportNodes


  ! -------------------------------------------------------------
  ! --- GET EXPORT STREAM NODE INDICES, DESTINATION TYPES AND INDICIES FOR A GIVEN SET OF BYPASS INDICES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBypassExportDestinationData(iNBypass,iBypassList,iExpStrmNodeList,iDestTypeList,iDestList,iStat) BIND(C,NAME='IW_Model_GetBypassExportDestinationData')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBypassExportDestinationData
    INTEGER(C_INT),INTENT(IN)  :: iNBypass,iBypassList(iNBypass)
    INTEGER(C_INT),INTENT(OUT) :: iExpStrmNodeList(iNBypass),iDestTypeList(iNBypass),iDestList(iNBypass),iStat
    
    !Local variables
    INTEGER :: indx,iDummy
    
    iStat = 0
    DO indx=1,iNBypass
        CALL Model%GetBypassDiversionOriginDestData(.TRUE.,iBypassList(indx),iExpStrmNodeList(indx),iDestTypeList(indx),iDestList(indx))
    END DO
    
  END SUBROUTINE IW_Model_GetBypassExportDestinationData


  ! -------------------------------------------------------------
  ! --- GET BYPASS OUTFLOWS AT ALL BYPASSES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBypassOutflows(iNBypass,rConvFactor,rOutflows,iStat) BIND(C,NAME='IW_Model_GetBypassOutflows')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBypassOutflows
    INTEGER(C_INT),INTENT(IN)  :: iNBypass
    REAL(C_DOUBLE),INTENT(IN)  :: rConvFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rOutflows(iNBypass)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    CALL Model%GetBypassOutflows(rOutflows)
    rOutflows = rConvFactor * rOutflows
    
  END SUBROUTINE IW_Model_GetBypassOutflows

  
  ! -------------------------------------------------------------
  ! --- GET RECOVERABLE LOSS FACTOR FOR A GIVEN BYPASS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBypassRecoverableLossFactor(iBypass,rFactor,iStat) BIND(C,NAME='IW_Model_GetBypassRecoverableLossFactor')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBypassRecoverableLossFactor
    INTEGER(C_INT),INTENT(IN)  :: iBypass
    REAL(C_DOUBLE),INTENT(OUT) :: rFactor
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat   = 0
    rFactor = Model%GetBypassRecoverableLossFactor(iBypass)
    
  END SUBROUTINE IW_Model_GetBypassRecoverableLossFactor

  
  ! -------------------------------------------------------------
  ! --- GET NON-RECOVERABLE LOSS FACTOR FOR A GIVEN BYPASS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetBypassNonRecoverableLossFactor(iBypass,rFactor,iStat) BIND(C,NAME='IW_Model_GetBypassNonRecoverableLossFactor')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetBypassNonRecoverableLossFactor
    INTEGER(C_INT),INTENT(IN)  :: iBypass
    REAL(C_DOUBLE),INTENT(OUT) :: rFactor
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat   = 0
    rFactor = Model%GetBypassNonRecoverableLossFactor(iBypass)
    
  END SUBROUTINE IW_Model_GetBypassNonRecoverableLossFactor

  
  ! -------------------------------------------------------------
  ! --- GET FLAG TO CHECK IF A SUPPLY IS SERVING AG, URBAN OR BOTH
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSupplyPurpose(iSupplyType,iNSupplies,iSupplies,iAgOrUrban,iStat) BIND(C,NAME='IW_Model_GetSupplyPurpose')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSupplyPurpose
    INTEGER(C_INT),INTENT(IN)  :: iSupplyType,iNSupplies,iSupplies(iNSupplies)
    INTEGER(C_INT),INTENT(OUT) :: iAgOrUrban(iNSupplies),iStat
    
    CALL Model%GetSupplyPurpose(iSupplyType,iSupplies,iAgOrUrban,iStat)
    
  END SUBROUTINE IW_Model_GetSupplyPurpose
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF LAKES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNLakes(NLakes,iStat) BIND(C,NAME='IW_Model_GetNLakes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNLakes
    INTEGER(C_INT),INTENT(OUT) :: NLakes,iStat
    
    iStat  = 0
    NLakes = Model%GetNLakes()
    
  END SUBROUTINE IW_Model_GetNLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE IDS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetLakeIDs(NLakes,IDs,iStat) BIND(C,NAME='IW_Model_GetLakeIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetLakeIDs
    INTEGER(C_INT),INTENT(IN)  :: NLakes
    INTEGER(C_INT),INTENT(OUT) :: IDs(NLakes),iStat
    
    iStat  = 0
    CALL Model%GetLakeIDs(IDs)
    
  END SUBROUTINE IW_Model_GetLakeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS IN A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNElementsInLake(iLake,NElements,iStat) BIND(C,NAME='IW_Model_GetNElementsInLake')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNElementsInLake
    INTEGER(C_INT),INTENT(IN)  :: iLake
    INTEGER(C_INT),INTENT(OUT) :: NElements,iStat
    
    iStat     = 0
    NElements = Model%GetNElementsInLake(iLake)
    
  END SUBROUTINE IW_Model_GetNElementsInLake
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTS IN A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetElementsInLake(iLake,NElems,Elems,iStat) BIND(C,NAME='IW_Model_GetElementsInLake')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetElementsInLake
    INTEGER(C_INT),INTENT(IN)  :: iLake,NElems
    INTEGER(C_INT),INTENT(OUT) :: Elems(NElems),iStat
    
    iStat = 0
    
    CALL Model%GetElementsInLake(iLake,NElems,Elems)
    
  END SUBROUTINE IW_Model_GetElementsInLake

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TILE DRAIN NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNTileDrainNodes(NTDNodes,iStat) BIND(C,NAME='IW_Model_GetNTileDrainNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNTileDrainNodes
    INTEGER(C_INT),INTENT(OUT) :: NTDNodes,iStat
    
    iStat    = 0
    NTDNodes = Model%GetNTileDrainNodes()
    
  END SUBROUTINE IW_Model_GetNTileDrainNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN IDS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetTileDrainIDs(NTDNodes,IDs,iStat) BIND(C,NAME='IW_Model_GetTileDrainIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetTileDrainIDs
    INTEGER(C_INT),INTENT(IN)  :: NTDNodes
    INTEGER(C_INT),INTENT(OUT) :: IDs(NTDNodes),iStat
    
    iStat = 0
    CALL Model%GetTileDrainIDs(IDs)
    
  END SUBROUTINE IW_Model_GetTileDrainIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET GROUNDWATER NODES CORRESPONDING TO TILE DRAIN NODES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetTileDrainNodes(iDim,TDNodes,iStat) BIND(C,NAME='IW_Model_GetTileDrainNodes')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetTileDrainNodes
    INTEGER(C_INT),INTENT(IN)  :: iDim
    INTEGER(C_INT),INTENT(OUT) :: TDNodes(iDim),iStat
    
    !Local variables
    INTEGER             :: ErrorCode
    INTEGER,ALLOCATABLE :: iLocalTDNodes(:)
    
    CALL Model%GetTileDrainNodes(iLocalTDNodes,iStat)
    TDNodes = iLocalTDNodes
    
    DEALLOCATE (iLocalTDNodes , STAT=ErrorCode)
    
  END SUBROUTINE IW_Model_GetTileDrainNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET AG PUMPING-WEIGHTED-AVERAGE DEPTH-TO-GW AT EACH SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSubregionAgPumpingAverageDepthToGW(NSubregions,AveDepthToGW,iStat) BIND(C,NAME='IW_Model_GetSubregionAgPumpingAverageDepthToGW')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSubregionAgPumpingAverageDepthToGW
    INTEGER(C_INT),INTENT(IN)  :: NSubregions
    REAL(C_DOUBLE),INTENT(OUT) :: AveDepthToGW(NSubregions)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetSubregionAgPumpingAverageDepthToGW(AveDepthToGW,iStat)
    
  END SUBROUTINE IW_Model_GetSubregionAgPumpingAverageDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET AG PUMPING-WEIGHTED-AVERAGE DEPTH-TO-GW AT ZONES DEFINED BY ELEMENT GROUPS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetZoneAgPumpingAverageDepthToGW(iNElems,iElems,iElemZones,iNZones,rAveDepthToGW,iStat) BIND(C,NAME='IW_Model_GetZoneAgPumpingAverageDepthToGW')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetZoneAgPumpingAverageDepthToGW
    INTEGER(C_INT),INTENT(IN)  :: iNElems,iNZones,iElems(iNElems),iElemZones(iNElems)
    REAL(C_DOUBLE),INTENT(OUT) :: rAveDepthToGW(iNZones)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetZoneAgPumpingAverageDepthToGW(iElems,iElemZones,rAveDepthToGW,iStat)
    
  END SUBROUTINE IW_Model_GetZoneAgPumpingAverageDepthToGW
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SIMULATED AG CROPS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNAgCrops(NAgCrops,iStat) BIND(C,NAME='IW_Model_GetNAgCrops')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNAgCrops
    INTEGER(C_INT),INTENT(OUT) :: NAgCrops,iStat
    
    CALL Model%GetNAgCrops(NAgCrops,iStat)
    
  END SUBROUTINE IW_Model_GetNAgCrops
  
  
  ! -------------------------------------------------------------
  ! --- GET AG SUPPLY REQUIREMENT AT SELECTED LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSupplyRequirement_Ag(iLocationTypeID,iNLocations,iLocationList,rFactor,rSupplyReq,iStat) BIND(C,NAME='IW_Model_GetSupplyRequirement_Ag')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSupplyRequirement_Ag
    INTEGER(C_INT),INTENT(IN)  :: iLocationTypeID,iNLocations,iLocationList(iNLocations)
    REAL(C_DOUBLE),INTENT(IN)  :: rFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rSupplyReq(iNLocations)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetSupplyRequirement(iLocationTypeID,iLocationList,f_iLandUse_Ag,rFactor,rSupplyReq,iStat)
    
  END SUBROUTINE IW_Model_GetSupplyRequirement_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN SUPPLY REQUIREMENT AT SELECTED LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSupplyRequirement_Urb(iLocationTypeID,iNLocations,iLocationList,rFactor,rSupplyReq,iStat) BIND(C,NAME='IW_Model_GetSupplyRequirement_Urb')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSupplyRequirement_Urb
    INTEGER(C_INT),INTENT(IN)  :: iLocationTypeID,iNLocations,iLocationList(iNLocations)
    REAL(C_DOUBLE),INTENT(IN)  :: rFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rSupplyReq(iNLocations)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetSupplyRequirement(iLocationTypeID,iLocationList,f_iLandUse_Urb,rFactor,rSupplyReq,iStat)
    
  END SUBROUTINE IW_Model_GetSupplyRequirement_Urb
 
  
  ! -------------------------------------------------------------
  ! --- GET AG SUPPLY SHORTAGE AT THE ORIGIN OF SELECTED SUPPLIES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSupplyShortAtOrigin_Ag(iSupplyTypeID,iNSupplies,iSupplyList,rFactor,rSupplyShort,iStat) BIND(C,NAME='IW_Model_GetSupplyShortAtOrigin_Ag')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSupplyShortAtOrigin_Ag
    INTEGER(C_INT),INTENT(IN)  :: iSupplyTypeID,iNSupplies,iSupplyList(iNSupplies)
    REAL(C_DOUBLE),INTENT(IN)  :: rFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rSupplyShort(iNSupplies)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetSupplyShortAtOrigin_ForSomeSupplies(iSupplyTypeID,iSupplyList,f_iLandUse_Ag,rFactor,rSupplyShort,iStat)
    
  END SUBROUTINE IW_Model_GetSupplyShortAtOrigin_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN SUPPLY SHORATGE AT THEORIGIN OF SELECTED SUPPLIES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetSupplyShortAtOrigin_Urb(iSupplyTypeID,iNSupplies,iSupplyList,rFactor,rSupplyShort,iStat) BIND(C,NAME='IW_Model_GetSupplyShortAtOrigin_Urb')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetSupplyShortAtOrigin_Urb
    INTEGER(C_INT),INTENT(IN)  :: iSupplyTypeID,iNSupplies,iSupplyList(iNSupplies)
    REAL(C_DOUBLE),INTENT(IN)  :: rFactor
    REAL(C_DOUBLE),INTENT(OUT) :: rSupplyShort(iNSupplies)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%GetSupplyShortAtOrigin_ForSomeSupplies(iSupplyTypeID,iSupplyList,f_iLandUse_Urb,rFactor,rSupplyShort,iStat)
    
  END SUBROUTINE IW_Model_GetSupplyShortAtOrigin_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF LOCATIONS GIVEN LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetNLocations(iLocationType,iNLocations,iStat) BIND(C,NAME='IW_Model_GetNLocations')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetNLocations
    INTEGER(C_INT),INTENT(IN)  :: iLocationType
    INTEGER(C_INT),INTENT(OUT) :: iNLocations,iStat
    
    iStat        = 0
    iNLocations = Model%GetNLocations(iLocationType)
    
  END SUBROUTINE IW_Model_GetNLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION IDS GIVEN LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_GetLocationIDs(iLocationType,iNLocations,iLocationIDs,iStat) BIND(C,NAME='IW_Model_GetLocationIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_GetLocationIDs
    INTEGER(C_INT),INTENT(IN)  :: iLocationType,iNLocations
    INTEGER(C_INT),INTENT(OUT) :: iLOcationIDs(iNLocations),iStat
    
    !Local variables
    INTEGER             :: ErrorCode
    INTEGER,ALLOCATABLE :: iIDs(:)
    
    iStat = 0
    CALL Model%GetLocationIDs(iLocationType,iIDs)
    iLocationIDs = iIDs
    
    DEALLOCATE (iIDs , STAT=ErrorCode)
    
  END SUBROUTINE IW_Model_GetLocationIDs
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- SET PATHNAME TO PRE-PROCESSOR DIRECTORY
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_SetPreProcessorPath(iLen,cPath,iStat) BIND(C,NAME='IW_Model_SetPreProcessorPath')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_SetPreProcessorPath
    INTEGER(C_INT),INTENT(IN)         :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cPath(iLen)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER :: cPath_F*iLen
    
    iStat = 0
    
    CALL String_Copy_C_F(cPath,cPath_F)
    
    !If the pathname ends with "\" (or "/" in non-windows OS), proceed normally
    IF (cPath_F(iLen:iLen).EQ.'\'  .OR. cPath_F(iLen:iLen).EQ.'/') THEN
      ALLOCATE (CHARACTER(iLen) :: cPreProcessorPath)
      cPreProcessorPath = cPath_F
      
    !Otherwise, add that 
    ELSE
      ALLOCATE (CHARACTER(iLen+1) :: cPreProcessorPath)
      cPreProcessorPath = cPath_F
      IF (FirstLocation('\',cPath_F) .GT. 0) THEN
        cPreProcessorPath = TRIM(cPreProcessorPath) // '\'
      ELSE
        cPreProcessorPath = TRIM(cPreProcessorPath) // '/'
      END IF
      
    END IF  
      
  END SUBROUTINE IW_Model_SetPreProcessorPath
  
  
  ! -------------------------------------------------------------
  ! --- SET PATHNAME TO SIMULATION DIRECTORY
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_SetSimulationPath(iLen,cPath,iStat) BIND(C,NAME='IW_Model_SetSimulationPath')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_SetSimulationPath
    INTEGER(C_INT),INTENT(IN)         :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cPath(iLen)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER :: cPath_F*iLen
    
    iStat = 0
    
    CALL String_Copy_C_F(cPath,cPath_F)
    
    !If the pathname ends with "\" (or "/" in non-windows OS), proceed normally
    IF (cPath_F(iLen:iLen).EQ.'\'  .OR. cPath_F(iLen:iLen).EQ.'/') THEN
      ALLOCATE (CHARACTER(iLen) :: cSimulationPath)
      cSimulationPath = cPath_F
      
    !Otherwise, add that 
    ELSE
      ALLOCATE (CHARACTER(iLen+1) :: cSimulationPath)
      cSimulationPath = cPath_F
      IF (FirstLocation('\',cPath_F) .GT. 0) THEN
        cSimulationPath = TRIM(cSimulationPath) // '\'
      ELSE
        cSimulationPath = TRIM(cSimulationPath) // '/'
      END IF
      
    END IF  
      
  END SUBROUTINE IW_Model_SetSimulationPath
  
  
  ! -------------------------------------------------------------
  ! --- SET MAXIMUM NUMBER OF SUPPLY ADJUSTMENT ITERATIONS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_SetSupplyAdjustmentMaxIters(iMaxIters,iStat) BIND(C,NAME='IW_Model_SetSupplyAdjustmentMaxIters')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_SetSupplyAdjustmentMaxIters
    INTEGER(C_INT),INTENT(IN)  :: iMaxIters
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%SetSupplyAdjustmentMaxIters(iMaxIters,iStat)
  
  END SUBROUTINE IW_Model_SetSupplyAdjustmentMaxIters
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY ADJUSTMENT TOLERANCE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_SetSupplyAdjustmentTolerance(rToler,iStat) BIND(C,NAME='IW_Model_SetSupplyAdjustmentTolerance')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_SetSupplyAdjustmentTolerance
    REAL(C_DOUBLE),INTENT(IN)  :: rToler
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%SetSupplyAdjustmentTolerance(rToler,iStat)
  
  END SUBROUTINE IW_Model_SetSupplyAdjustmentTolerance
  
  
  
  
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
  ! --- DELETE MODEL INQUIRY DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_DeleteInquiryDataFile(iLenSimFileName,cSimFileName,iStat) BIND(C,NAME='IW_Model_DeleteInquiryDataFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_DeleteInquiryDataFile
    INTEGER(C_INT),INTENT(IN)         :: iLenSimFileName
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cSimFileName(iLenSimFileName)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=iLenSimFileName) :: cSimFileName_F
    CHARACTER(:),ALLOCATABLE       :: cSimWorkingDirectory
    
    !Initialize
    iStat = 0
    
    !C strings to Fortran strings
    CALL String_Copy_C_F(cSimFileName,cSimFileName_F)
    
    !Working directory
    CALL GetFileDirectory(cSimFileName_F,cSIMWorkingDirectory)

    !Delete file location in the working directory
    CALL Model%DeleteModelInquiryDataFile(cSIMWorkingDirectory)
    
  END SUBROUTINE IW_Model_DeleteInquiryDataFile    

  
  ! -------------------------------------------------------------
  ! --- SIMULATE MODEL FOR THE ENTIRE PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_SimulateAll(iStat) BIND(C,NAME='IW_Model_SimulateAll')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_SimulateAll
    INTEGER(C_INT),INTENT(OUT) :: iStat
  
    CALL Model%Simulate(0,iStat)
    IF (iStat .EQ. -1) CALL Model%Kill()
                                             
  END SUBROUTINE IW_Model_SimulateAll
  
    
  ! -------------------------------------------------------------
  ! --- SIMULATE MODEL FOR A SINGLE TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_SimulateForOneTimeStep(iStat) BIND(C,NAME='IW_Model_SimulateForOneTimeStep')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_SimulateForOneTimeStep
    INTEGER(C_INT),INTENT(OUT) :: iStat

    CALL Model%Simulate(iStat)
    IF (iStat .EQ. -1) CALL Model%Kill()
                                             
  END SUBROUTINE IW_Model_SimulateForOneTimeStep
  
    
  ! -------------------------------------------------------------
  ! --- SIMULATE MODEL FOR AN INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_SimulateForAnInterval(iLen,cInterval,iStat) BIND(C,NAME='IW_Model_SimulateForAnInterval')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_SimulateForAnInterval
    INTEGER(C_INT),INTENT(IN)         :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cInterval(iLen)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
  
    !Local variables
    CHARACTER :: cInterval_F*iLen
    
    CALL String_Copy_C_F(cInterval,cInterval_F)

    CALL Model%Simulate(cInterval_F,iStat)
    IF (iStat .EQ. -1) CALL Model%Kill()
                                             
  END SUBROUTINE IW_Model_SimulateForAnInterval
  
    
  ! -------------------------------------------------------------
  ! --- ADVANCE SIMULATION TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_AdvanceTime(iStat) BIND(C,NAME='IW_Model_AdvanceTime')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_AdvanceTime
    INTEGER(C_INT),INTENT(OUT) :: iStat
  
    iStat = 0
    
    CALL Model%AdvanceTime()
                                             
  END SUBROUTINE IW_Model_AdvanceTime
  

  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_ReadTSData(iStat) BIND(C,NAME='IW_Model_ReadTSData')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_ReadTSData
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%ReadTSData(iStat)
    IF (iStat .EQ. -1) CALL Model%Kill()
                                             
  END SUBROUTINE IW_Model_ReadTSData


  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA BUT OVERWRITE SOME BY USER DEFINED VALUES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_ReadTSData_Overwrite(iNLandUse,iNSubregions,rRegionLUAreas,iNDiversions,iDiversions,rDiversions,iNStrmInflows,iStrmInflows,rStrmInflows,iNBypasses,iBypasses,rBypasses,iStat) BIND(C,NAME='IW_Model_ReadTSData_Overwrite')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_ReadTSData_Overwrite
    INTEGER(C_INT),INTENT(IN)  :: iNLandUse,iNSubregions,iNDiversions,iNStrmInflows,iNBypasses
    INTEGER(C_INT),INTENT(IN)  :: iDiversions(iNDiversions),iStrmInflows(iNStrmInflows),iBypasses(iNBypasses)
    REAL(C_DOUBLE),INTENT(IN)  :: rRegionLUAreas(iNSubregions,iNLandUse),rDiversions(iNDiversions),rStrmInflows(iNStrmInflows),rBypasses(iNBypasses)
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    CALL Model%ReadTSData(iStat,rRegionLUAreas,iDiversions,rDiversions,iStrmInflows,rStrmInflows,iBypasses,rBypasses)
                                             
  END SUBROUTINE IW_Model_ReadTSData_Overwrite


  ! -------------------------------------------------------------
  ! --- PRINT SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_PrintResults(iStat) BIND(C,NAME='IW_Model_PrintResults')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_PrintResults
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
  
    CALL Model%PrintResults()
                                             
  END SUBROUTINE IW_Model_PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF THE MODEL IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_AdvanceState(iStat) BIND(C,NAME='IW_Model_AdvanceState') 
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_AdvanceState
    INTEGER(C_INT),INTENT(OUT) :: iStat

    iStat = 0
    
    CALL Model%AdvanceState()
                                             
  END SUBROUTINE IW_Model_AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- IS IT END OF SIMULATION?
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_IsEndOfSimulation(iEndOfSimulation,iStat) BIND(C,NAME='IW_Model_IsEndOfSimulation')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_IsEndOfSimulation
    INTEGER(C_INT),INTENT(OUT) :: iEndOfSimulation,iStat
    
    iStat = 0
    
    IF (Model%IsEndOfSimulation()) THEN
        iEndOfSImulation = 1
    ELSE
        iEndOfSimulation = 0
    END IF
    
  END SUBROUTINE IW_Model_IsEndOfSimulation
  
  
  ! -------------------------------------------------------------
  ! --- IS MODEL INSTANTIATED?
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_IsModelInstantiated(iInstantiated,iStat) BIND(C,NAME='IW_Model_IsModelInstantiated')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_IsModelInstantiated
    INTEGER(C_INT),INTENT(OUT) :: iInstantiated,iStat
    
    iStat = 0
    
    IF (lModel_Instantiated) THEN
        iInstantiated = 1
    ELSE
        iInstantiated = 0
    END IF
    
  END SUBROUTINE IW_Model_IsModelInstantiated
  
  
  ! -------------------------------------------------------------
  ! --- TURN SUPPLY ADJUSTMENTS ON/OFF
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_TurnSupplyAdjustOnOff(iDivAdjustOn,iPumpAdjustOn,iStat) BIND(C,NAME='IW_Model_TurnSupplyAdjustOnOff')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_TurnSupplyAdjustOnOff
    INTEGER(C_INT),INTENT(IN)  :: iDivAdjustOn,iPumpAdjustOn
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Local variables
    LOGICAL :: lDivAdjustOn,lPumpAdjustOn
    
    iStat = 0
    
    IF (iDivAdjustOn .EQ. 0) THEN
        lDivAdjustOn = .FALSE.
    ELSE
        lDivAdjustOn = .TRUE.
    END IF
    
    IF (iPumpAdjustOn .EQ. 0) THEN
        lPumpAdjustOn = .FALSE.
    ELSE
        lPumpAdjustOn = .TRUE.
    END IF
    
    CALL Model%TurnSupplyAdjustOnOff(lDivAdjustOn,lPumpAdjustOn,iStat)
    
  END SUBROUTINE IW_Model_TurnSupplyAdjustOnOff
  
  
  ! -------------------------------------------------------------
  ! --- RESTORE PUMPING TO READ VALUES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_RestorePumpingToReadValues(iStat) BIND(C,NAME='IW_Model_RestorePumpingToReadValues')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_RestorePumpingToReadValues
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    CALL Model%RestorePumpingToReadValues(iStat)
    
  END SUBROUTINE IW_Model_RestorePumpingToReadValues
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE FUTURE WATER DEMANDS UNTIL A SPECIFIED TIME
  ! --- NOTE: This must be called after IW_ReadTSData or 
  ! ---       IW_ReadTSData_Overwrite procedures
  ! ---       for consistent operation because the TS files  
  ! ---       are rewound back to where they were after  
  ! ---       ReadTSData method was called. 
  ! -------------------------------------------------------------
  SUBROUTINE IW_Model_ComputeFutureWaterDemands(iLenDate,cEndComputeDate,iStat) BIND(C,NAME='IW_Model_ComputeFutureWaterDemands')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Model_ComputeFutureWaterDemands
    INTEGER(C_INT),INTENT(IN)         :: iLenDate
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cEndComputeDate(iLenDate)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=iLenDate) :: cEndComputeDate_F
    
    !Convert C string to F string
    CALL String_Copy_C_F(cEndComputeDate,cEndComputeDate_F)
    
    !Compute
    CALL Model%ComputeFutureWaterDemands(cEndComputeDate_F,iStat)
    
  END SUBROUTINE IW_Model_ComputeFutureWaterDemands
  
  
  ! -------------------------------------------------------------
  ! --- PACK A CHARACTER ARRAY INTO A SINGLE CHARACTER VARIABLES
  ! -------------------------------------------------------------
  SUBROUTINE StringArray_To_StringScalar(cArray,cScalar,iLocArray)
    CHARACTER(LEN=*),INTENT(IN) :: cArray(:)
    CHARACTER(LEN=*)            :: cScalar
    INTEGER,INTENT(OUT)         :: iLocArray(:)
    
    !Local variables
    INTEGER :: indx
    
    !Initialize
    cScalar = ''
    
    !Compile the array into a single string variable
    DO indx=1,SIZE(cArray)  
        IF (indx .EQ. 1) THEN
            iLocArray(indx) = 1
        ELSE
            iLocArray(indx) = LEN_TRIM(cScalar) + 1
        END IF
        cScalar = TRIM(cScalar) // TRIM(cArray(indx))
    END DO

  END SUBROUTINE StringArray_To_StringScalar
  
  
END MODULE