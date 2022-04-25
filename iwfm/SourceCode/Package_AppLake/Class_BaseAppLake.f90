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
MODULE Class_BaseAppLake
   USE Class_Version               , ONLY: VersionType             , &
                                           ReadVersion
   USE MessageLogger               , ONLY: EchoProgress            , &
                                           SetLastMessage          , &
                                           MessageArray            , &
                                           f_iFatal
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
                                           f_iLakeComp             , &
                                           f_iLocationType_Lake    , &
                                           f_iAllLocationIDsListed , &
                                           f_iFlowDest_StrmNode
   USE Package_Budget              , ONLY: BudgetType              , &
                                           BudgetHeaderType        , &
                                           f_cVolumeUnitMarker     , &
                                           f_cAreaUnitMarker       , &
                                           f_cLengthUnitMarker     , &
                                           f_cLocationNameMarker   , &
                                           f_cAreaMarker           , &
                                           f_iColumnHeaderLen      , &
                                           f_iVR                   , &
                                           f_iVLE                  , &
                                           f_iVLB                  , &
                                           f_iLT                   , &
                                           f_iPER_CUM              , &
                                           f_iPER_AVER
   USE Package_Discretization      , ONLY: AppGridType             , &
                                           StratigraphyType
   USE Package_ComponentConnectors , ONLY: StrmLakeConnectorType   , &
                                           LakeGWConnectorType     , &
                                           f_iStrmToLakeFlow       , &
                                           f_iBypassToLakeFlow
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
            GenerateRatingTable     , &
            f_iBudgetType_Lake 


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
      PROCEDURE,PASS                                                   :: GetBudget_List
      PROCEDURE,PASS                                                   :: GetBudget_NColumns
      PROCEDURE,PASS                                                   :: GetBudget_ColumnTitles
      PROCEDURE,PASS                                                   :: GetBudget_MonthlyFlows_GivenAppLake
      PROCEDURE,NOPASS                                                 :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS                                                   :: GetBudget_TSData
      PROCEDURE,PASS                                                   :: GetNLakes 
      PROCEDURE,PASS                                                   :: GetNames
      PROCEDURE,PASS                                                   :: GetLakeIDs
      PROCEDURE,PASS                                                   :: GetLakeID
      PROCEDURE,PASS                                                   :: GetLakeIndex
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
      PROCEDURE,PASS                                                   :: DestinationIDs_To_Indices
      GENERIC                                                          :: New                            => SetStaticComponent             , &
                                                                                                            SetStaticComponentFromBinFile  , &
                                                                                                            SetDynamicComponent            , &
                                                                                                            SetAllComponents               , &
                                                                                                            SetAllComponentsWithoutBinFile
  END TYPE BaseAppLakeType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iBudgetType_Lake        = f_iLakeComp*1000 + 1
  CHARACTER(LEN=11),PARAMETER :: f_cDescription_LakeBudget = 'Lake budget'
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iNLakeBudColumns = 13
  CHARACTER(LEN=27),PARAMETER :: f_cBudgetColumnTitles(f_iNLakeBudColumns) = ['Beginning Storage (+)'       , &
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

    SUBROUTINE Abstract_SetStaticComponent(AppLake,cFileName,Stratigraphy,AppGrid,StrmLakeConnector,LakeGWConnector,iStat)
      IMPORT                                :: BaseAppLakeType,StratigraphyType,AppGridType,StrmLakeConnectorType,LakeGWConnectorType
      CLASS(BaseAppLakeType),INTENT(OUT)    :: AppLake
      CHARACTER(LEN=*),INTENT(IN)           :: cFileName
      TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
      TYPE(AppGridType),INTENT(IN)          :: AppGrid
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

    
    SUBROUTINE Abstract_SetAllComponentsWithoutBinFile(AppLake,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,LakeGWConnector,iStat)
      IMPORT                                :: BaseAppLakeType,AppGridType,StratigraphyType,TimeStepType,StrmLakeConnectorType,LakeGWConnectorType
      CLASS(BaseAppLakeType),INTENT(OUT)    :: AppLake
      LOGICAL,INTENT(IN)                    :: IsForInquiry
      CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSimFileName,cSimWorkingDirectory
      TYPE(AppGridType),INTENT(IN)          :: AppGrid
      TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
      TYPE(TimeStepType),INTENT(IN)         :: TimeStep
      INTEGER,INTENT(IN)                    :: NTIME
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
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(AppLake,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(BaseAppLakeType),INTENT(IN)        :: AppLake
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    !Local variables
    INTEGER                  :: iErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (iBudgetTypeList , iBudgetLocationTypeList , cBudgetDescriptions , cBudgetFiles , STAT=iErrorCode)
         
    !Get the list if there is a Budget generated
    IF (AppLake%LakeBudRawFile_Defined) THEN
        ALLOCATE (iBudgetTypeList(1) , iBudgetLocationTypeList(1) , cBudgetDescriptions(1) , cBudgetFiles(1))
        CALL AppLake%LakeBudRawFile%GetFileName(cFileName)
        cBudgetFiles(1)            = cFileName
        iBudgetTypeList(1)         = f_iBudgetType_Lake
        iBudgetLocationTypeList(1) = f_iLocationType_Lake
        cBudgetDescriptions(1)     = f_cDescription_LakeBudget
    ELSE
        ALLOCATE (iBudgetTypeList(0) , iBudgetLocationTypeList(0) , cBudgetDescriptions(0) , cBudgetFiles(0))
    END IF
     
  END SUBROUTINE GetBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BUDGET COLUMNS (EXCLUDES TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(AppLake,iLocationIndex,iNCols,iStat)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLocationIndex          
    INTEGER,INTENT(OUT)               :: iNCols,iStat       
    
    IF (AppLake%LakeBudRawFile_Defined) THEN
        CALL AppLake%LakeBudRawFile%GetNDataColumns(iLocationIndex,iNCols,iStat)  !Includes Time column 
        iNCols = iNCols - 1  !Exclude Time column
    ELSE
        iStat  = 0
        iNCols = 0
    END IF
     
  END SUBROUTINE GetBudget_NColumns


  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(AppLake,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(BaseAppLakeType),INTENT(IN)        :: AppLake
    INTEGER,INTENT(IN)                       :: iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER                                       :: iNCols,iErrorCode
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    IF (AppLake%LakeBudRawFile_Defined) THEN
        !Get number of columns (includes Time column)
        CALL AppLake%LakeBudRawFile%GetNDataColumns(iLocationIndex,iNCols,iStat)  
        IF (iStat .NE. 0) RETURN
        
        !Get column titles (includes (Time column)
        ALLOCATE (cColTitles_Local(iNCols))
        cColTitles_Local = AppLake%LakeBudRawFile%GetFullColumnHeaders(iLocationIndex,iNCols)
        
        !Insert units
        CALL AppLake%LakeBudRawFile%ModifyFullColumnHeaders(cUnitLT,cUnitAR,cUnitVL,cColTitles_Local)
        
        !Remove time column
        iNCols = iNCols - 1
        ALLOCATE (cColTitles(iNCols))
        cColTitles = ADJUSTL(cColTitles_Local(2:))
        
        !Clear memory
        DEALLOCATE (cColTitles_Local , STAT=iErrorCode)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    END IF
     
  END SUBROUTINE GetBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM AppLake OBJECT FOR A SPECIFED LAKE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenAppLake(AppLake,iLakeIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(BaseAppLakeType),INTENT(IN)        :: AppLake
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iLakeIndex
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER :: ID
    
    IF (AppLake%LakeBudRawFile_Defined) THEN
        ID = AppLake%Lakes(iLakeIndex)%ID
        CALL GetBudget_MonthlyFlows_GivenFile(AppLake%LakeBudRawFile,ID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)  
    ELSE
        iStat = 0
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
    END IF
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenAppLake


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE FOR A SPECIFED LAKE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iLakeID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iLakeID
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER,PARAMETER   :: iReadCols(11) = [1,2,3,4,5,6,7,8,9,10,11]
    INTEGER             :: iDimActual,iNTimeSteps
    REAL(8),ALLOCATABLE :: rValues(:,:)
    
    !Get simulation time steps and allocate array to read data
    iNTimeSteps = Budget%GetNTimeSteps()
    ALLOCATE (rValues(12,iNTimeSteps)) !Adding 1 to the first dimension for Time column; it will be removed later
    
    !Read data
    CALL Budget%ReadData(iLakeID,iReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Store values in return argument
    ALLOCATE (rFlows(10,iDimActual) , cFlowNames(10))
    rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(3,1:iDimActual)   !Change in storage
    rFlows(2,:)  = rValues(4,1:iDimActual)                             !Flow from Upstream Lake (+)  
    rFlows(3,:)  = rValues(5,1:iDimActual)                             !Flow from Streams (+)      
    rFlows(4,:)  = rValues(6,1:iDimActual)                             !Flow from Bypasses (+)    
    rFlows(5,:)  = rValues(7,1:iDimActual)                             !Runoff (+)                
    rFlows(6,:)  = rValues(8,1:iDimActual)                             !Return Flow (+)           
    rFlows(7,:)  = rValues(9,1:iDimActual)                             !Precipitation (+)         
    rFlows(8,:)  = rValues(10,1:iDimActual)                            !Gain from Groundwater (+) 
    rFlows(9,:)  = -rValues(11,1:iDimActual)                           !Lake Evaporation (-)      
    rFlows(10,:) = -rValues(12,1:iDimActual)                           !Lake Outflow (-)          
    
    !Flow names
    cFlowNames     = ''
    cFlowNames(1)  = 'Change in Storage'      
    cFlowNames(2)  = 'Flow from Upstream Lakes' 
    cFlowNames(3)  = 'Flow from Streams'         
    cFlowNames(4)  = 'Flow from Bypasses'        
    cFlowNames(5)  = 'Runoff'                    
    cFlowNames(6)  = 'Return Flow'               
    cFlowNames(7)  = 'Precipitation'             
    cFlowNames(8)  = 'Gain from Groundwater'      
    cFlowNames(9)  = 'Evaporation'          
    cFlowNames(10) = 'Outflow'              
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile


  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AppLake,iLakeIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLakeIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)       :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)               :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)               :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER :: indx,ID
    
    IF (AppLake%LakeBudRawFile_Defined) THEN
        !Read data
        ID = AppLake%Lakes(iLakeIndex)%ID
        DO indx=1,SIZE(iCols)
            CALL AppLake%LakeBudRawFile%ReadData(ID,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
        END DO
    ELSE
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
    END IF
                       
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetLakeIDs(AppLake,iLakeIDs)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(OUT)               :: iLakeIDs(:)
    
    iLakeIDs = AppLake%Lakes%ID
    
  END SUBROUTINE GetLakeIDs
  
    
  ! -------------------------------------------------------------
  ! --- GET LAKE ID GIVEN INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION GetLakeID(AppLake,indx) RESULT(ID)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: indx
    INTEGER                           :: ID
    
    ID = AppLake%LAkes(indx)%ID
    
  END FUNCTION GetLakeID
  
      
  ! -------------------------------------------------------------
  ! --- GET LAKE INDEX GIVEN ID
  ! -------------------------------------------------------------
  PURE FUNCTION GetLakeIndex(AppLake,iLakeID) RESULT(iIndex)
    CLASS(BaseAppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)                :: iLakeID
    INTEGER                           :: iIndex
    
    iIndex = LocateInList(iLakeID,AppLake%Lakes%ID)
    
  END FUNCTION GetLakeIndex
  
      
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
                CALL SetLastMessage('Element '//TRIM(IntToText(iElem))//' is listed more than once as a lake element!' ,f_iFatal,ThisProcedure)
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
  PURE FUNCTION GetNElementsInLake(AppLake,iLake) RESULT(NElems)
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
        CALL SetLastMessage('Error in allocating memory for application lakes!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Lake data
    DO indxLake=1,NLakes
        ASSOCIATE (pLake => AppLake%Lakes(indxLake))
            CALL InFile%ReadData(pLake%ID,iStat)         ;  IF (iStat .EQ. -1) RETURN
            CALL InFile%ReadData(pLake%NElements,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL InFile%ReadData(pLake%NNodes,iStat)     ;  IF (iStat .EQ. -1) RETURN
            ALLOCATE (pLake%Elements(pLake%NElements) , &
                      pLake%Nodes(pLake%NNodes)       , &
                      pLake%NodeAreas(pLake%NNodes)   , &
                      STAT=ErrorCode                  )
            IF (ErrorCode .NE. 0) THEN
                CALL SetLastMessage('Error in allocating memory for lake '//TRIM(IntToText(pLake%ID))//'!',f_iFatal,ThisProcedure)
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
        CALL OutFile%WriteData(pLake%ID)
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
    REAL(8)                           :: DummyArray(f_iNLakeBudColumns,AppLake%NLakes)
    REAL(8),DIMENSION(AppLake%NLakes) :: Error,StrmInflows,LakeGWFlows,LakePrecip,BypassInflows
    
    !Return if raw budget output file is not defined
    IF (.NOT. AppLake%LakeBudRawFile_Defined) RETURN
    
    !Echo progress
    CALL EchoProgress('Printing results of lake simulation')

    !Initialize
    LakeGWFlows = LakeGWConnector%GetFlowAtLakes()
    LakePrecip  = AppLake%Lakes%PrecipRate * AppLake%Lakes%Area
    DO indxLake=1,AppLake%NLakes
      StrmInflows(indxLake)   = StrmLakeConnector%GetFlow(f_iStrmToLakeFlow,indxLake)   
      BypassInflows(indxLake) = StrmLakeConnector%GetFlow(f_iBypassToLakeFlow,indxLake)
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
        IF (AppLake%lFinalElevFile_Defined) CALL PrintFinalElevs(AppLake%Lakes%ID,AppLake%Lakes%Elev,TimeStep,AppLake%FinalElevFile)
    END IF

  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT END-OF-SIMULATION LAKE ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalElevs(iLakeIDs,Elevs,TimeStep,OutFile) 
    INTEGER,INTENT(IN)            :: iLakeIDs(:)
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
        WRITE (Text,'(I8,100F16.6)') iLakeIDs(indxLake),Elevs(indxLake)
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
  ! --- CONVERT IDs (MAINLY STREAM NODE IDs) TO INDICES
  ! -------------------------------------------------------------
  SUBROUTINE DestinationIDs_To_Indices(AppLake,iStrmNodeIDs,iStat)
    CLASS(BaseAppLakeType) :: AppLake
    INTEGER,INTENT(IN)     :: iStrmNodeIDs(:)
    INTEGER,INTENT(OUT)    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'DestinationIDs_To_Indices'
    INTEGER                                :: indx,iLakeID,iDestID,iDest
    
    !Initialize
    iStat = 0
    
    !Convert destination stream node IDs to indices
    DO indx=1,AppLake%NLakes
        IF (AppLake%Lakes(indx)%OutflowDestType .EQ. f_iFlowDest_StrmNode) THEN
            iDestID = AppLake%Lakes(indx)%OutflowDest
            iDest   = LocateInList(iDestID,iStrmNodeIDs)
            IF (iDest .EQ. 0) THEN
                iLakeID = AppLake%Lakes(indx)%ID
                CALL SetLastMessage('Stream node '//TRIM(IntToText(iDestID))//' that receive outflow from lake '//TRIM(IntToText(iLakeID))//' is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            AppLake%Lakes(indx)%OutflowDest = iDest
        END IF
    END DO
    
  END SUBROUTINE DestinationIDs_To_Indices
  
  
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
    CALL Matrix%AddComponent(f_iLakeComp,AppLake%NLakes,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Add connectivity
    DO indxLake=1,AppLake%NLakes
        LakeNode(1) = indxLake
        CALL Matrix%AddConnectivity(f_iLakeComp,indxLake,f_iLakeComp,LakeNode,iStat)
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
    CHARACTER(LEN=6)  :: CParts(f_iNLakeBudColumns) = ['VOLUME' , &
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
                                                       'ELEV'   ]
    CHARACTER(LEN=17),PARAMETER :: FParts(f_iNLakeBudColumns) = ['BEGIN_STORAGE'      ,&
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
                                                                 'SURFACE_ELEV'       ]
    
    !Initialize
    NLakes = SIZE(Lakes)
    Text1  = '('//TRIM(f_cLengthUnitMarker)//')'

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
        pASCIIOutput%cTitles(2)         = ArrangeText('LAKE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('LAKE AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
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
    ALLOCATE (Header%Locations(1)                                                             , &
              Header%Locations(1)%cFullColumnHeaders(f_iNLakeBudColumns+1)                    , &
              Header%Locations(1)%iDataColumnTypes(f_iNLakeBudColumns)                        , &
              Header%Locations(1)%iColWidth(f_iNLakeBudColumns+1)                             , &
              Header%Locations(1)%cColumnHeaders(f_iNLakeBudColumns+1,NColumnHeaderLines)     , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)                )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns                             = f_iNLakeBudColumns
      pLocation%cFullColumnHeaders(1)                    = 'Time'                                           
      pLocation%cFullColumnHeaders(2:)                   = f_cBudgetColumnTitles
      pLocation%cFullColumnHeaders(f_iNLakeBudColumns+1) = TRIM(pLocation%cFullColumnHeaders(f_iNLakeBudColumns+1)) // ' ('//f_cLengthUnitMarker//')'
      pLocation%iDataColumnTypes                         = [f_iVLB,&  !Beginning storage
                                                            f_iVLE,&  !Ending storage
                                                            f_iVR ,&  !Flow from upstream lake
                                                            f_iVR ,&  !Flow from streams
                                                            f_iVR ,&  !Flow from bypasses
                                                            f_iVR ,&  !Runoff into lake
                                                            f_iVR ,&  !return flow into lake
                                                            f_iVR ,&  !Precip
                                                            f_iVR ,&  !Gain from groundwater
                                                            f_iVR ,&  !Lake evaporation
                                                            f_iVR ,&  !Lake outflow
                                                            f_iVR ,&  !Discrepency
                                                            f_iLT ]  !Lake surface elevation
      pLocation%iColWidth                             = [17,(12,indx=1,f_iNLakeBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        pColumnHeaders(:,1) = (/'                 ','    Beginning','    Ending   ','  Flow from  ','  Flow from  ','  Flow from  ','             ','    Return   ','             ','   Gain from ','     Lake    ','      Lake   ','             ','Lake Surface '/)
        pColumnHeaders(:,2) = (/'      Time       ','     Storage ','    Storage  ','Upstream Lake','   Streams   ','   Bypasses  ','     Runoff  ','     Flow    ','Precipitation','  Groundwater','  Evaporation','     Outflow ',' Discrepancy ','  Elevation  '/)
        pColumnHeaders(:,3) = (/      TextTime     ,'       (+)   ','      (-)    ','     (+)     ','     (+)     ','     (+)     ','      (+)    ','      (+)    ','      (+)    ','      (+)    ','     (-)     ','       (-)   ','     (=)     ',          Text1/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,13A13)'
        pFormatSpecs(2)     = '(A17,13A13)'
        pFormatSpecs(3)     = '(A17,13A13)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(f_iNLakeBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNLakeBudColumns*NLakes) , pDSSOutput%iDataTypes(f_iNLakeBudColumns))
      iCount = 1
      DO indxLocation=1,NLakes
        DO indxCol=1,f_iNLakeBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_LAKE_BUD/'                                      //  &  !A part
                                          UpperCase(TRIM(Lakes(indxLocation)%cName))//'/'        //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                             //  &  !C part
                                          '/'                                                    //  &  !D part
                                          TRIM(TimeStep%Unit)//'/'                               //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                    !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [(f_iPER_CUM,indxCol=1,f_iNLakeBudColumns-1),f_iPER_AVER]
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
        NVertex               = AppGrid%NVertex(ElemNo)
        iVertex               = AppGrid%Vertex(:,ElemNo)
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