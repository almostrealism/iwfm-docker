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
MODULE Class_AppStream_v50
  USE Class_Version                 , ONLY: ReadVersion
  USE Class_BaseAppStream           , ONLY: BaseAppStreamType                  , &
                                            ReadFractionsForGW                 , &
                                            f_iBudgetType_StrmNode             , &
                                            f_iBudgetType_StrmReach            , & 
                                            f_iBudgetType_DiverDetail   
  USE MessageLogger                 , ONLY: SetLastMessage                     , &
                                            LogMessage                         , &
                                            EchoProgress                       , &
                                            MessageArray                       , &
                                            f_iFILE                            , &
                                            f_iFatal                           , &
                                            f_iMessage
  USE GeneralUtilities              , ONLY: StripTextUntilCharacter            , &
                                            CleanSpecialCharacters             , &
                                            ArrangeText                        , &
                                            UpperCase                          , &
                                            IntToText                          , &
                                            EstablishAbsolutePathFilename      , &
                                            LocateInList                       , &
                                            ShellSort                          , &
                                            GetArrayData                       , &
                                            ConvertID_To_Index
  USE TimeSeriesUtilities           , ONLY: TimeStepType                       , &
                                            TimeIntervalConversion             , &
                                            IncrementTimeStamp
  USE IOInterface                   , ONLY: GenericFileType                    , &
                                            f_iUNKNOWN
  USE Class_StrmNode_v50            , ONLY: StrmNode_v50_Type                  , &
                                            StrmNode_v50_ReadPreprocessedData  , &
                                            StrmNode_v50_WritePreprocessedData
  USE Class_StrmReach               , ONLY: StrmReach_New                      , &
                                            StrmReach_GetReachNumber           , &
                                            StrmReach_WritePreprocessedData    , &
                                            StrmReach_CompileReachNetwork
  USE Package_ComponentConnectors   , ONLY: StrmGWConnectorType                , &
                                            StrmLakeConnectorType              , &
                                            f_iStrmToLakeFlow                  , &
                                            f_iLakeToStrmFlow
  USE Package_Discretization        , ONLY: AppGridType                        , &
                                            StratigraphyType
  USE Package_Misc                  , ONLY: f_iFlowDest_Outside                , &
                                            f_iFlowDest_Lake                   , &
                                            f_iFlowDest_StrmNode               , &
                                            f_iStrmComp                        , &
                                            f_iLocationType_StrmReach          , &
                                            f_iLocationType_StrmNode           , &
                                            f_rSmoothMaxP
  USE Package_Budget                , ONLY: BudgetType                         , &
                                            BudgetHeaderType                   , &
                                            f_iColumnHeaderLen                 , &
                                            f_cVolumeUnitMarker                , &
                                            f_cLocationNameMarker              , &
                                            f_iMaxLocationNameLen              , &
                                            f_iVR                              , &
                                            f_iPER_CUM
  USE Package_Matrix                , ONLY: MatrixType
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
  PUBLIC :: AppStream_v50_Type                                              
 
  
  ! -------------------------------------------------------------
  ! --- APPLICATION STREAMS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseAppStreamType) :: AppStream_v50_Type
    PRIVATE
    REAL(8)                             :: DeltaT        = 0.0   !Simulation timestep
    REAL(8),ALLOCATABLE                 :: StorChange(:)         !Change in storage at each node
    TYPE(StrmNode_v50_Type),ALLOCATABLE :: Nodes(:)
    TYPE(GenericFileType)               :: FinalFlowFile         !File that stores flows at the end of simulation
  CONTAINS
    PROCEDURE,PASS   :: SetStaticComponent                    => AppStream_v50_SetStaticComponent
    PROCEDURE,PASS   :: SetStaticComponentFromBinFile         => ReadPreprocessedData
    PROCEDURE,PASS   :: SetDynamicComponent                   => AppStream_v50_SetDynamicComponent
    PROCEDURE,PASS   :: SetAllComponents                      => AppStream_v50_SetAllComponents
    PROCEDURE,PASS   :: SetAllComponentsWithoutBinFile        => AppStream_v50_SetAllComponentsWithoutBinFile
    PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenAppStream => AppStream_v50_GetBudget_MonthlyFlows_GivenAppStream                     !Overriding the method defined in the base class
    PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile      => AppStream_v50_GetBudget_MonthlyFlows_GivenFile                     !Overriding the method defined in the base class
    PROCEDURE,PASS   :: GetStrmNodeIDs                        => AppStream_v50_GetStrmNodeIDs
    PROCEDURE,PASS   :: GetStrmNodeID                         => AppStream_v50_GetStrmNodeID
    PROCEDURE,PASS   :: GetStrmNodeIndex                      => AppStream_v50_GetStrmNodeIndex
    PROCEDURE,PASS   :: GetNUpstrmNodes                       => AppStream_v50_GetNUpstrmNodes
    PROCEDURE,PASS   :: GetUpstrmNodes                        => AppStream_v50_GetUpstrmNodes
    PROCEDURE,PASS   :: GetStageFlowRatingTable               => AppStream_v50_GetStageFlowRatingTable
    PROCEDURE,PASS   :: GetVersion                            => AppStream_v50_GetVersion
    PROCEDURE,PASS   :: GetBottomElevations                   => AppStream_v50_GetBottomElevations
    PROCEDURE,PASS   :: GetNRatingTablePoints                 => AppStream_v50_GetNRatingTablePoints
    PROCEDURE,PASS   :: KillImplementation                    => AppStream_v50_Kill
    PROCEDURE,PASS   :: WritePreprocessedData                 => AppStream_v50_WritePreprocessedData
    PROCEDURE,PASS   :: WriteDataToTextFile                   => AppStream_v50_WriteDataToTextFile
    PROCEDURE,PASS   :: UpdateHeads                           => AppStream_v50_UpdateHeads
    PROCEDURE,PASS   :: ConvertTimeUnit                       => AppStream_v50_ConvertTimeUnit
    PROCEDURE,PASS   :: ConvertFlowToElev                     => AppStream_v50_ConvertFlowToElev
    PROCEDURE,PASS   :: Simulate                              => AppStream_v50_Simulate
    PROCEDURE,PASS   :: PrintResults                          => AppStream_v50_PrintResults        !Overriding the method defined in the base class
    PROCEDURE,PASS   :: AdvanceState                          => AppSTream_v50_AdvanceState        !Overriding the method defined in the base class
  END TYPE AppStream_v50_Type
    
    
  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iVersion    = 50
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '5.0.0000'
  INCLUDE 'AppStream_v50_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iNStrmBudColumns = 15
  CHARACTER(LEN=30),PARAMETER :: f_cBudgetColumnTitles(f_iNStrmBudColumns) = ['Upstream Inflow (+)'             , &
                                                                              'Downstream Outflow (-)'          , &
                                                                              'Change in Storage (-)'           , &
                                                                              'Tributary Inflow (+)'            , &
                                                                              'Tile Drain (+)'                  , &
                                                                              'Runoff (+)'                      , &
                                                                              'Return Flow (+)'                 , &
                                                                              'Gain from GW_Inside Model (+)'   , &
                                                                              'Gain from GW_Outside Model (+)'  , &
                                                                              'Gain from Lake (+)'              , &
                                                                              'Riparian ET (-)'                 , &
                                                                              'Diversion (-)'                   , &
                                                                              'By-pass Flow (-)'                , &
                                                                              'Discrepancy (=)'                 , &
                                                                              'Diversion Shortage'              ]
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen      = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName         = 'Class_AppStream_v50::'
  
  
  
  
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
  ! --- READ RAW STREAM DATA (GENERALLY CALLED IN PRE-PROCESSOR)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_SetStaticComponent(AppStream,cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    CLASS(AppStream_v50_Type),INTENT(OUT) :: AppStream
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    TYPE(AppGridType),INTENT(IN)          :: AppGrid          !Not used in this version
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    LOGICAL,INTENT(IN)                    :: IsRoutedStreams
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'AppStream_v50_SetStaticComponent'
    CHARACTER                    :: ALine*100
    INTEGER                      :: ErrorCode,iGWNodeIDs(AppGrid%NNodes)
    TYPE(GenericFileType)        :: DataFile
    
    !Initialize
    iStat      = 0
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Inform user
    CALL EchoProgress('Instantiating streams')
    
    !Set the flag to check if routed or non-routed streams
    AppStream%lRouted = IsRoutedStreams
    
    !Open file
    CALL DataFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Stream configuration data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read away the first line that holds the version number and set the version number using internal variables
    CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AppStream%Version = AppStream%Version%New(iLenVersion,cVersion,cRevision)
    
    !Read dimensions
    CALL DataFile%ReadData(AppStream%NReaches,iStat)    ;  IF (iStat .EQ. -1) RETURN
    
    !Compile the total number of stream nodes
    CALL CalculateNStrmNodes(DataFile,AppStream%NReaches,AppStream%NStrmNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (AppStream%Nodes(AppStream%NStrmNodes) , &
              AppStream%Reaches(AppStream%NReaches) , &
              STAT = ErrorCode                      )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream configuration data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read stream configuration
    CALL ReadStreamConfigData(DataFile,Stratigraphy,iGWNodeIDs,StrmGWConnector,StrmLakeConnector,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read stream nodes and fraction of stream-aquifer interaction to be applied to corresponding gw nodes
    CALL ReadFractionsForGW(DataFile,AppStream%Nodes%ID,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL DataFile%Kill()
    
  END SUBROUTINE AppStream_v50_SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC PART OF STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type)         :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33) :: ThisProcedure = ModName // 'AppStream_v50_SetDynamicComponent'
    INTEGER                      :: indxNode,ICType,ErrorCode,iReachIDs(AppStream%NReaches),iStrmNodeIDs(AppStream%NStrmNodes), &
                                    iGWNodeIDs(AppGrid%NNodes),iStrmNodeID,iStrmNode,indx
    TYPE(GenericFileType)        :: MainFile
    CHARACTER(LEN=1000)          :: ALine,DiverFileName,DiverSpecFileName,BypassSpecFileName,DiverDetailBudFileName,ReachBudRawFileName
    CHARACTER                    :: TimeUnitFlow*6
    TYPE(BudgetHeaderType)       :: BudHeader
    CHARACTER(:),ALLOCATABLE     :: cVersionSim,cVersionPre,cAbsPathFileName
    INTEGER,ALLOCATABLE          :: GWNodes(:)
    REAL(8)                      :: FACTH,DummyArray(AppStream%NStrmNodes,2),TimeFactor
    LOGICAL                      :: lProcessed(AppStream%NStrmNodes)
    INTEGER,PARAMETER            :: ICType_H      = 0                     , &
                                    ICType_Q      = 1                     , &
                                    ICTypeList(2) = [ICType_H , ICType_Q]
    
    !Initialize
    iStat        = 0
    iStrmNodeIDs = AppStream%Nodes%ID
    iGWNodeIDs   = AppGrid%AppNode%ID
  
    !Open main file
    CALL MainFile%New(FileName=cFileName,InputFile=.TRUE.,Descriptor='main stream data file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that version numbers from Pre-processor and Simulation match
    cVersionPre = AppStream%Version%GetVersion()  ;  cVersionPre = StripTextUntilCharacter(cVersionPre,'.',Back=.TRUE.)
    CALL ReadVersion(MainFile,'STREAM',cVersionSim,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (TRIM(cVersionSim) .NE. TRIM(cVersionPre)) THEN
        MessageArray(1) = 'Stream Component versions used in Pre-Processor and Simulation must match!'
        MessageArray(2) = 'Version number in Pre-Processor = ' // TRIM(cVersionPre)
        MessageArray(3) = 'Version number in Simulation    = ' // TRIM(cVersionSim)
        CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Set the simulation time step
    AppStream%DeltaT = TimeStep%DeltaT
    
    !Allocate memory for stream states
    IF (.NOT. ALLOCATED(AppStream%State)) ALLOCATE (AppStream%State(AppStream%NStrmNodes))
    ALLOCATE (AppStream%StorChange(AppStream%NStrmNodes))  ;  AppStream%StorChange = 0.0
    
    !Initialize related files
    !-------------------------
    
    !Stream inflow file
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AppStream%lRouted) THEN
        ALine = StripTextUntilCharacter(ALine,'/') 
        CALL CleanSpecialCharacters(ALine)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL AppStream%StrmInflowData%New(cAbsPathFileName,cWorkingDirectory,TimeStep,AppStream%NStrmNodes,iStrmNodeIDs,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Diversion specs file name
    CALL MainFile%ReadData(DiverSpecFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AppStream%lRouted) THEN
        DiverSpecFileName = StripTextUntilCharacter(DiverSpecFileName,'/') 
        CALL CleanSpecialCharacters(DiverSpecFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(DiverSpecFileName)),cWorkingDirectory,cAbsPathFileName)
        DiverSpecFileName = cAbsPathFileName
    END IF
    
    !Bypass specs file name
    CALL MainFile%ReadData(BypassSpecFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AppStream%lRouted) THEN
        BypassSpecFileName = StripTextUntilCharacter(BypassSpecFileName,'/') 
        CALL CleanSpecialCharacters(BypassSpecFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(BypassSpecFileName)),cWorkingDirectory,cAbsPathFileName)
        BypassSpecFileName = cAbsPathFileName
    END IF
    
    !Diversions file name
    CALL MainFile%ReadData(DiverFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AppStream%lRouted) THEN
        DiverFileName = StripTextUntilCharacter(DiverFileName,'/') 
        CALL CleanSpecialCharacters(DiverFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(DiverFileName)),cWorkingDirectory,cAbsPathFileName)
        DiverFileName = cAbsPathFileName
    END IF
    
    !Stream reach budget raw file
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AppStream%lRouted) THEN
        ReachBudRawFileName = StripTextUntilCharacter(ALine,'/') 
        CALL CleanSpecialCharacters(ReachBudRawFileName)
        IF (ReachBudRawFileName .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ReachBudRawFileName)),cWorkingDirectory,cAbsPathFileName)
            ReachBudRawFileName = cAbsPathFileName 
        END IF
    END IF
    
    !Diversion details raw file
    CALL MainFile%ReadData(DiverDetailBudFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AppStream%lRouted) THEN
        DiverDetailBudFileName = StripTextUntilCharacter(DiverDetailBudFileName,'/') 
        CALL CleanSpecialCharacters(DiverDetailBudFileName)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(DiverDetailBudFileName)),cWorkingDirectory,cAbsPathFileName)
        DiverDetailBudFileName = cAbsPathFileName
    END IF
    
    !Diversions and bypasses
    CALL AppStream%AppDiverBypass%New(IsForInquiry,DiverSpecFileName,BypassSpecFileName,DiverFileName,DiverDetailBudFileName,cWorkingDirectory,AppStream%GetVersion(),NTIME,TimeStep,AppStream%NStrmNodes,iStrmNodeIDs,iLakeIDs,AppStream%Reaches,AppGrid,StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN

    !Reach IDs 
    iReachIDs = AppStream%Reaches%ID

    !Prepare reach budget output file
    IF (ReachBudRawFileName .NE. '') THEN
        IF (IsForInquiry) THEN
            CALL AppStream%StrmReachBudRawFile%New(ReachBudRawFileName,iStat)
            IF (iStat .EQ. -1) RETURN
        ELSE
            !Sort reach IDs for budget printing in order
            ALLOCATE (AppStream%iPrintReachBudgetOrder(AppStream%NReaches))
            AppStream%iPrintReachBudgetOrder = [(indx,indx=1,AppStream%NReaches)]
            CALL ShellSort(iReachIDs,AppStream%iPrintReachBudgetOrder)
            !Restore messed iReachID array
            iReachIDs = AppStream%Reaches%ID
            !Prepare budget header
            BudHeader = PrepareStreamBudgetHeader(AppStream%NReaches,AppStream%iPrintReachBudgetOrder,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,AppStream%GetVersion(),cReachNames=AppStream%Reaches%cName)
            CALL AppStream%StrmReachBudRawFile%New(ReachBudRawFileName,BudHeader,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL BudHeader%Kill()
        END IF
        AppStream%StrmReachBudRawFile_Defined = .TRUE.
    END IF

    !End-of-simulation flows file
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AppStream%lRouted) THEN
        ALine = StripTextUntilCharacter(ALine,'/') 
        CALL CleanSpecialCharacters(ALine)
        IF (ALine .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
            IF (IsForInquiry) THEN
                CALL AppStream%FinalFlowFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='end-of-simulation stream flows data',iStat=iStat)
            ELSE
                CALL AppStream%FinalFlowFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='end-of-simulation stream flows data',iStat=iStat)
            END IF
            IF (iStat .EQ. -1) RETURN
        END IF
    END IF
    
    !Hydrograph printing
    CALL AppStream%StrmHyd%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,AppStream%NStrmNodes,iStrmNodeIDs,TimeStep,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream budget at selected segments
    CALL AppStream%StrmNodeBudget%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,AppStream%GetVersion(),PrepareStreamBudgetHeader,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream bed parameters for stream-gw connectivity
    CALL StrmGWConnector%CompileConductance(MainFile,AppGrid,Stratigraphy,AppStream%NStrmNodes,iStrmNodeIDs,AppStream%Reaches%UpstrmNode,AppStream%Reaches%DownstrmNode,AppStream%Nodes%BottomElev,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream channel parameters
    CALL ReadCrossSectionData(Stratigraphy,iGWNodeIDs,iStrmNodeIDs,StrmGWConnector,MainFile,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Calculate bed slope, distance between node and the next node, and length of the corresponding segments
    CALL StrmGWConnector%GetAllGWNodes(GWNodes)
    CALL CompileDistanceLengthSlope(GWNodes,AppGrid,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !If non-routed streams, return at this point
    IF (.NOT. AppStream%lRouted) THEN
        DEALLOCATE (GWNodes , STAT=ErrorCode)
        CALL MainFile%Kill() 
        RETURN
    END IF
    
    !Initial conditions
    CALL MainFile%ReadData(ICType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (LocateInList(ICType,ICTypeList) .EQ. 0) THEN
        CALL SetLastMessage('Initial condition type '//TRIM(IntToText(ICType))//' is not recognized!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    TimeUnitFlow = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    !Make sure time unit for flow initial conditions is specified
    IF (ICType .EQ. ICType_Q) THEN
        IF (TimeUnitFlow .EQ. '') THEN
            CALL SetLastMessage('The time unit for stream initial conditions must be specified if initial conditions are given as flows!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL MainFile%ReadData(FACTH,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL MainFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxNode=1,AppStream%NStrmNodes
        !Make sure that node is recognized
        iStrmNodeID = INT(DummyArray(indxNode,1))
        CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,iStrmNode)
        IF (iStrmNode .EQ. 0) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' listed for initial conditions is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure node is not defined more then once
        IF (lProcessed(iStrmNode)) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntTotext(iStrmNodeID))//' is listed more than once for initail conditions!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iStrmNode) = .TRUE.
        
        !Assign initial conditions
        SELECT CASE (ICType)
            CASE (ICType_H)
                AppStream%State(iStrmNode)%Head   = MAX(AppStream%Nodes(iStrmNode)%BottomElev + DummyArray(indxNode,2) * FACTH   ,   AppStream%Nodes(iStrmNode)%BottomElev)
                AppStream%State(iStrmNode)%Head_P = AppStream%State(iStrmNode)%Head
                AppStream%State(iStrmNode)%Flow   = AppStream%Nodes(iStrmNode)%Flow(AppStream%State(iStrmNode)%Head)
            
            CASE (ICType_Q)
                !At this point Manning's roughness time unit is seconds. Convert flow time unit to seconds and 
                !compute corresponding head accordingly. Later, flow time unit will be converted to simulation 
                !time unit along with all time units of the parameters in this component.
                TimeFactor                       = TimeIntervalConversion(TimeUnitFlow,'1MIN') * 60D0 !Must multiply with 60 to convert minute to seconds
                AppStream%State(iStrmNode)%Flow   = DummyArray(indxNode,2) * FACTH / TimeFactor
                AppStream%State(iStrmNode)%Head   = MAX(AppStream%Nodes(iStrmNode)%Head(AppStream%State(iStrmNode)%Flow)   ,   AppStream%Nodes(iStrmNode)%BottomElev)
                AppStream%State(iStrmNode)%Head_P = AppStream%State(iStrmNode)%Head
                IF (AppStream%State(iStrmNode)%Head .EQ. -9999.9999d0) THEN
                    CALL SetLastMessage('There was a convergence problem in converting the initial flow for stream node '//TRIM(IntToText(iStrmNodeID))//' to stream flow depth!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
        END SELECT
            
        !Flow area
        AppStream%Nodes(iStrmNode)%Area_P = AppStream%Nodes(iStrmNode)%Area(AppStream%State(iStrmNode)%Head_P)
        
    END DO 
    
    !Clear memory
    DEALLOCATE (GWNodes , STAT=ErrorCode)
    
    !Close main file
    CALL MainFile%Kill() 
  
  END SUBROUTINE AppStream_v50_SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                    :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(GenericFileType)                 :: BinFile
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(StrmGWConnectorType)             :: StrmGWConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'AppStream_v50_SetAllComponents'
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Instantiating streams')
    
    !Read the preprocessed data for streams
    CALL ReadPreprocessedData(AppStream,BinFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Set the dynamic part of AppStream
    CALL AppStream_v50_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that if static part is defined, so is the dynamic part
    IF (AppStream%NStrmNodes .GT. 0) THEN
      IF (SIZE(AppStream%State) .EQ. 0) THEN
        MessageArray(1) = 'For proper simulation of streams, relevant stream data files must'
        MessageArray(2) = 'be specified when stream nodes are defined in Pre-Processor.'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
    END IF 
    
  END SUBROUTINE AppStream_v50_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                    :: IsRoutedStreams,IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSimFileName,cSimWorkingDirectory
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME,iLakeIDs(:)
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+44) :: ThisProcedure = ModName // 'AppStream_v50_SetAllComponentsWithoutBinFile'
    
    !Initialize
    iStat = 0
    
    !Instantiate the static components of the AppStream data
    CALL AppStream_v50_SetStaticComponent(AppStream,cPPFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the dynamic component of the AppStream data
    CALL AppStream_v50_SetDynamicComponent(AppStream,IsForInquiry,cSimFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that if static part is defined, so is the dynamic part
    IF (AppStream%NStrmNodes .GT. 0) THEN
      IF (SIZE(AppStream%State) .EQ. 0) THEN
        MessageArray(1) = 'For proper simulation of streams, relevant stream data files must'
        MessageArray(2) = 'be specified when stream nodes are defined in Pre-Processor.'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
    END IF 
  
  END SUBROUTINE AppStream_v50_SetAllComponentsWithoutBinFile

  

  
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
  ! --- KILL STREAM DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_Kill(AppStream)
    CLASS(AppStream_v50_Type) :: AppStream
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Deallocate array attributes
    DEALLOCATE (AppStream%Nodes , AppStream%StorChange , STAT=ErrorCode)
    
  END SUBROUTINE AppStream_v50_Kill
    
  
  
  
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
  ! --- GET AVERAGE MONTHLY BUDGET FLOWS FROM BaseAppStream OBJECT
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_GetBudget_MonthlyFlows_GivenAppStream(AppStream,iBudgetType,iLocationIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(AppStream_v50_Type),TARGET,INTENT(IN) :: AppStream
    CHARACTER(LEN=*),INTENT(IN)                 :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                          :: iBudgetType,iLocationIndex  !Location can be stream node, reach or diversion
    REAL(8),INTENT(IN)                          :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)             :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)    :: cFlowNames(:)
    INTEGER,INTENT(OUT)                         :: iStat
    
    !Local variables
    INTEGER                  :: ID
    TYPE(BudgetType),POINTER :: pBudget
    
    !Initialize
    iStat   =  0
    pBudget => NULL()
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_StrmNode)
            ID = AppStream%GetStrmNodeID(iLocationIndex)
            IF (AppStream%StrmNodeBudget%StrmNodeBudRawFile_Defined) pBudget => AppStream%StrmNodeBudget%StrmNodeBudRawFile
            
        CASE (f_iBudgetType_StrmReach)
            ID = AppStream%Reaches(iLocationIndex)%ID
            IF (AppStream%StrmReachBudRawFile_Defined) pBudget => AppStream%StrmReachBudRawFile
            
        CASE (f_iBudgetType_DiverDetail)
            !CALL AppStream%AppDiverBypass%GetBudget_MonthlyFlows(AppDiverBypass,iLocationID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
            RETURN
    END SELECT
        
    !Return if no Budget file is available
    IF (.NOT.ASSOCIATED(pBudget)) THEN
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
        RETURN
    END IF
    
    !Retrieve flows
    CALL AppStream_v50_GetBudget_MonthlyFlows_GivenFile(pBudget,iBudgetType,ID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
        
    !Clear memory
    NULLIFY(pBudget)
    
  END SUBROUTINE AppStream_v50_GetBudget_MonthlyFlows_GivenAppStream

  
  ! -------------------------------------------------------------
  ! --- GET AVERAGE MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! --- (REDEFINES THE PROCEDURE IN Class_BaseAppStream)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLocationID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLocationID  !Location can be stream node, reach or diversion
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER,PARAMETER   :: iReadCols(13) = [1,2,3,4,5,6,7,8,9,10,11,12,13]
    INTEGER             :: iDimActual,iNTimeSteps
    REAL(8),ALLOCATABLE :: rValues(:,:)
    
    !Get simulation time steps and allocate array to read data
    iNTimeSteps = Budget%GetNTimeSteps()
    ALLOCATE (rValues(14,iNTimeSteps)) !Adding 1 to the first dimension for Time column; it will be removed later
    
    !Read data
    CALL Budget%ReadData(iLocationID,iReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Store values in return argument
    ALLOCATE (rFlows(12,iDimActual) , cFlowNames(12))
    rFlows(1,:)  = rValues(2,1:iDimActual)                             !Upstream Inflow              
    rFlows(2,:)  = -rValues(3,1:iDimActual)                            !Downstream Outflow
    rFlows(3,:)  = -rValues(4,1:iDimActual)                            !Change in Storage           
    rFlows(4,:)  = rValues(5,1:iDimActual)                             !Tributary Inflow            
    rFlows(5,:)  = rValues(6,1:iDimActual)                             !Tile Drain                  
    rFlows(6,:)  = rValues(7,1:iDimActual)                             !Runoff                      
    rFlows(7,:)  = rValues(8,1:iDimActual)                             !Return Flow                  
    rFlows(8,:)  = rValues(9,1:iDimActual) + rValues(10,1:iDimActual)  !Gain from GW    
    rFlows(9,:)  = rValues(11,1:iDimActual)                            !Gain from Lake               
    rFlows(10,:) = -rValues(12,1:iDimActual)                           !Riparian ET                  
    rFlows(11,:) = -rValues(13,1:iDimActual)                           !Diversion                    
    rFlows(12,:) = -rValues(14,1:iDimActual)                           !By-pass Flow                 
    
    !Flow names
    cFlowNames     = ''
    cFlowNames(1)  = 'Upstream Inflow'    
    cFlowNames(2)  = 'Downstream Outflow' 
    cFlowNames(3)  = 'Change in Storage'   
    cFlowNames(4)  = 'Tributary Inflow'   
    cFlowNames(5)  = 'Tile Drain'         
    cFlowNames(6)  = 'Runoff'             
    cFlowNames(7)  = 'Return Flow'        
    cFlowNames(8)  = 'Gain from GW'    
    cFlowNames(9)  = 'Gain from Lake'     
    cFlowNames(10) = 'Riparian ET'        
    cFlowNames(11) = 'Diversion'          
    cFlowNames(12) = 'Bypass Flow'       
    
  END SUBROUTINE AppStream_v50_GetBudget_MonthlyFlows_GivenFile

  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER 
  ! -------------------------------------------------------------
  FUNCTION AppStream_v50_GetVersion(AppStream) RESULT(cVrs)
    CLASS(AppStream_v50_Type) :: AppStream
    CHARACTER(:),ALLOCATABLE  :: cVrs
    
    IF (.NOT. AppStream%Version%IsDefined())   &
        AppStream%Version = AppStream%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = AppStream%Version%GetVersion()
    
  END FUNCTION AppStream_v50_GetVersion
  

  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_v50_GetStrmNodeIDs(AppStream,iStrmNodeIDs)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)                  :: iStrmNodeIDs(:)
    
    iStrmNodeIDs = AppStream%Nodes%ID
    
  END SUBROUTINE AppStream_v50_GetStrmNodeIDs


  ! -------------------------------------------------------------
  ! --- GET STREAM NODE ID GIVEN INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v50_GetStrmNodeID(AppStream,indx) RESULT(iStrmNodeID)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                   :: indx
    INTEGER                              :: iStrmNodeID
    
    iStrmNodeID = AppStream%Nodes(indx)%ID
    
  END FUNCTION AppStream_v50_GetStrmNodeID


  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDEX GIVEN ID
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v50_GetStrmNodeIndex(AppStream,ID) RESULT(Index)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                   :: ID
    INTEGER                              :: Index
    
    !Local variables
    INTEGER :: indx
    
    Index = 0
    DO indx=1,SIZE(AppStream%Nodes)
        IF (ID .EQ. AppStream%Nodes(indx)%ID) THEN
            Index = indx
            EXIT
        END IF
    END DO
    
  END FUNCTION AppStream_v50_GetStrmNodeIndex


  ! -------------------------------------------------------------
  ! --- GET RATING TABLE (STAGE VS. FLOW) AT A NODE
  ! --- *** Note: This procedure is not used in this version of AppStream package
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_GetStageFlowRatingTable(AppStream,iNode,Stage,Flow)
    CLASS(AppStream_v50_Type),TARGET,INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                          :: iNode            !Not used in this version
    REAL(8),INTENT(OUT)                         :: Stage(:),Flow(:)
    
    !Gather content
    Stage = 0.0
    Flow  = 0.0
    
  END SUBROUTINE AppStream_v50_GetStageFlowRatingTable


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF RATING TABLE POINTS AT A STREAM NODE
  ! --- ***Note: This procedure is not used in this version of AppStream package
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v50_GetNRatingTablePoints(AppStream,iStrmNode) RESULT(N)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream  !Not used in this version
    INTEGER,INTENT(IN)                   :: iStrmNode  !Not used in this version
    INTEGER                              :: N
    
    N = 0
    
  END FUNCTION AppStream_v50_GetNRatingTablePoints


  ! -------------------------------------------------------------
  ! --- GET BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v50_GetBottomElevations(AppStream) RESULT(BottomElev)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream
    REAL(8)                              :: BottomElev(AppStream%NStrmNodes)
    
    BottomElev = AppStream%Nodes%BottomElev
    
  END FUNCTION AppStream_v50_GetBottomElevations
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  FUNCTION AppStream_v50_GetNUpstrmNodes(AppStream,iStrmNode) RESULT(iNNodes)
    CLASS(AppStream_v50_Type),INTENT(IN)  :: AppStream
    INTEGER,INTENT(IN)                    :: iStrmNode
    INTEGER                               :: iNNodes
    
    iNNodes = AppStream%Nodes(iStrmNode)%Connectivity%nConnectedNodes
    
  END FUNCTION AppStream_v50_GetNUpstrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_GetUpstrmNodes(AppStream,iNode,UpstrmNodes)
    CLASS(AppStream_v50_Type),INTENT(IN)  :: AppStream
    INTEGER,INTENT(IN)                    :: iNode
    INTEGER,ALLOCATABLE,INTENT(OUT)       :: UpstrmNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (UpstrmNodes , STAT=ErrorCode)

    ALLOCATE (UpstrmNodes(AppStream%Nodes(iNode)%Connectivity%nConnectedNodes))
    UpstrmNodes = AppStream%Nodes(iNode)%Connectivity%ConnectedNodes
    
  END SUBROUTINE AppStream_v50_GetUpstrmNodes
  
  
  

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
  ! --- READ PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadPreprocessedData(AppStream,BinFile,iStat)
    CLASS(AppStream_v50_Type),INTENT(OUT) :: AppStream
    TYPE(GenericFileType)                 :: BinFile
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadPreprocessedData'
    INTEGER                      :: ErrorCode,iLenVersion
    CHARACTER(:),ALLOCATABLE     :: cVrs
    
    !Initialize
    iStat = 0
       
    !Read version number
    CALL BinFile%ReadData(iLenVersion,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (CHARACTER(iLenVersion) :: cVrs)
    CALL BinFile%ReadData(cVrs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AppStream%Version = AppStream%Version%New(cVrs)
    
    !Routed/non-routed flag
    CALL BinFile%ReadData(AppStream%lRouted,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read dimensions
    CALL BinFile%ReadData(AppStream%NStrmNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL BinFile%ReadData(AppStream%NReaches,iStat)    ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (AppStream%Nodes(AppStream%NStrmNodes) , AppStream%Reaches(AppStream%NReaches) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read stream node data
    CALL StrmNode_v50_ReadPreprocessedData(AppStream%Nodes,BinFile,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read stream reach data
    CALL StrmReach_New(AppStream%NReaches,Binfile,AppStream%Reaches,iStat) 
    
  END SUBROUTINE ReadPreprocessedData
  

  ! -------------------------------------------------------------
  ! --- READ STREAM REACH CONFIGURATION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadStreamConfigData(DataFile,Stratigraphy,iGWNodeIDs,StrmGWConnector,StrmLakeConnector,AppStream,iStat)
    TYPE(GenericFileType)                 :: DataFile
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    INTEGER,INTENT(IN)                    :: iGWNodeIDs(:)
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(AppStream_v50_Type)              :: AppStream
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadStreamConfigData'
    INTEGER                      :: indxReach,DummyIntArray3(3),indxNode,DummyIntArray2(2),indxStrmNode, &
                                    iDestNode,NNodes,iGWNodes(AppStream%NStrmNodes),iReachID,indxNode1,  &
                                    indxReach1,iStrmNodeID,iLayers(AppStream%NStrmNodes)
    CHARACTER                    :: ALine*2000
    
    !Initialize
    iStat = 0
    
    !Iterate over reaches
    indxStrmNode = 0
    DO indxReach=1,AppStream%NReaches
        ASSOCIATE (pReach => AppStream%Reaches(indxReach))
            CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            READ (ALine,*) pReach%ID
            CALL GetArrayData(ALine,DummyIntArray3,'stream reach '//TRIM(IntToText(pReach%ID)),iStat)  ;  IF (iStat .EQ. -1) RETURN
            pReach%cName = ALine(1:20)
            
            !Make sure reach ID is not used more than once
            DO indxReach1=1,indxReach-1
                IF (pReach%ID .EQ. AppStream%Reaches(indxReach1)%ID) THEN
                    CALL SetLastMessage('Stream reach ID '//TRIM(IntToText(pReach%ID))//' is used more than once!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
                        
            !Store data in persistent arrays
            pReach%UpstrmNode   = indxStrmNode + 1
            pReach%DownstrmNode = indxStrmNode + DummyIntArray3(2)
            IF (DummyIntArray3(3) .GT. 0) THEN
                pReach%OutflowDest  = DummyIntArray3(3)
            ELSEIF (DummyIntArray3(3) .EQ. 0) THEN
                pReach%OutflowDestType = f_iFlowDest_Outside
                pReach%OutflowDest     = 0
            ELSE
                pReach%OutflowDestType = f_iFlowDest_Lake
                pReach%OutflowDest     = -DummyIntArray3(3)
                CALL StrmLakeConnector%AddData(f_iStrmToLakeFlow , pReach%DownstrmNode , pReach%OutflowDest)
            END IF
            
            !Make sure there are at least 2 stream nodes defined for the reach
            NNodes = DummyIntArray3(2)
            IF (NNodes .LT. 2) THEN
                MessageArray(1) = 'There should be at least 2 stream nodes for each reach.'
                MessageArray(2) = 'Reach '//TRIM(IntToText(pReach%ID))//' has less than 2 stream nodes!'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Read stream node IDs and corresponding the gw nodes
            DO indxNode=1,NNodes
                indxStrmNode = indxStrmNode + 1
                
                CALL DataFile%ReadData(DummyIntArray2,iStat)  ;  IF (iStat .EQ. -1) RETURN
                iStrmNodeID                      = DummyIntArray2(1)
                AppStream%Nodes(indxStrmNode)%ID = iStrmNodeID
                
                !Make sure stream node ID is not repeated
                DO indxNode1=1,indxStrmNode-1
                    IF (iStrmNodeID .EQ.  AppStream%Nodes(indxNode1)%ID) THEN
                        CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' is used more than once!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END DO
                
                !Check gw node ID and store the corresponding index
                CALL ConvertID_To_Index(DummyIntArray2(2),iGWNodeIDs,iGWNodes(indxStrmNode))
                IF (iGWNodes(indxStrmNode) .EQ. 0) THEN
                    CALL SetLastMessage('Groundwater node '//TRIM(IntToText(DummyIntArray2(2)))//' listed in stream reach '//TRIM(IntToText(pReach%ID))//' ('//TRIM(pReach%cName)//') for stream node '//TRIM(IntToText(iStrmNodeID))//' is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Top active aquifer layer at stream node
                iLayers(indxStrmNode) = Stratigraphy%TopActiveLayer(iGWNodes(indxStrmNode))
            END DO          
        END ASSOCIATE
    END DO
    
    !Store GW nodes for each stream node in strm-gw connector database
    CALL StrmGWConnector%New(iVersion,iGWNodes,iLayers,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Convert outflow destination stream node IDs to indices and make sure that reach numbers are set properly
    DO indxReach=1,AppStream%NReaches
        ASSOCIATE (pReach => AppStream%Reaches(indxReach))
            IF (pReach%OutFlowDestType .NE. f_iFlowDest_StrmNode) CYCLE
            iReachID    = pReach%ID
            iStrmNodeID = pReach%OutFlowDest
            CALL ConvertID_To_Index(iStrmNodeID,AppStream%Nodes%ID,iDestNode)
            IF (iDestNode .EQ. 0) THEN
                CALL SetLastMessage('Outflow stream node '//TRIM(IntToText(iStrmNodeID))//' for reach '//TRIM(IntToText(iReachID))//' ('//TRIM(pReach%cName)//') is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (iDestNode .LE. pReach%DownstrmNode) THEN
                IF (iDestNode .GE. pReach%UpstrmNode) THEN
                    CALL SetLastMessage('Stream reach '//TRIM(IntToText(iReachID))//' ('//TRIM(pReach%cName)//') is outflowing back into itself!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END IF
            pReach%OutFlowDest = iDestNode
        END ASSOCIATE
    END DO
    
    !Compile reach network from upstream to downstream
    CALL StrmReach_CompileReachNetwork(AppStream%NReaches,AppStream%Reaches,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile upstream nodes for each node
    CALL CompileUpstrmNodes(AppStream)
    
  END SUBROUTINE ReadStreamConfigData
  
  
  ! -------------------------------------------------------------
  ! --- READ CROSS SECTION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadCrossSectionData(Stratigraphy,iGWNodeIDs,iStrmNodeIDs,StrmGWConnector,DataFile,AppStream,iStat)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: iGWNodeIDs(:),iStrmNodeIDs(:)
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    TYPE(GenericFileType)             :: DataFile
    TYPE(AppStream_v50_Type)          :: AppStream
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadCrossSectionData'
    INTEGER                      :: indxNode,iStrmNode,iGWNode,iLayer,ErrorCode,iStrmNodeID
    REAL(8)                      :: FACTN,FACTLT,AquiferBottomElev,DummyArray(6)
    INTEGER,ALLOCATABLE          :: iGWNodes(:)
    LOGICAL                      :: lProcessed(AppStream%NStrmNodes)
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    CALL StrmGWConnector%GetAllGWNodes(iGWNodes)
    
    !Read units conversion factors
    CALL DataFile%ReadData(FACTN,iStat)   ;  IF (iStat .EQ. -1) RETURN   ;   FACTN = 1D0 / (FACTN**(1D0/3D0))
    CALL DataFile%ReadData(FACTLT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read cross section data
    ASSOCIATE (pNodes => AppStream%Nodes)
        DO indxNode=1,AppStream%NStrmNodes
            iGWNode = iGWNodes(indxNode)
            iLayer  = Stratigraphy%TopActiveLayer(iGWNode)
            
            !Read data
            CALL DataFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
            
            !Make sure stream node is legit
            iStrmNodeID = INT(DummyArray(1))
            CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,iStrmNode)
            IF (iStrmNode .EQ. 0) THEN
                CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' listed for cross-section parameter defintion is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Make sure stream node is not entered more than once
            IF (lProcessed(iStrmNode)) THEN
                CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' is listed more than once for cross-section parameter definition!',f_iFatal,Thisprocedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iStrmNode) = .TRUE.
            
            !Stream bottom elevation
            pNodes(iStrmNode)%BottomElev = DummyArray(2) * FACTLT
            AquiferBottomElev            = Stratigraphy%BottomElev(iGWNode,iLayer)
            IF (pNodes(iStrmNode)%BottomElev .LT. AquiferBottomElev) THEN
                MessageArray(1) = 'Aquifer bottom elevation at a stream node should be'
                MessageArray(2) = 'less than or equal to the stream bed elevation!'
                WRITE (MessageArray(3),'(A,F10.2)') ' Stream node = '//TRIM(IntToText(iStrmNodeID))        //'   Stream bed elevation    = ',pNodes(iStrmNode)%BottomElev
                WRITE (MessageArray(4),'(A,F10.2)') ' GW node     = '//TRIM(IntToText(iGWNodeIDs(iGWNode)))//'   Aquifer bottom elevation= ',AquiferBottomElev
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Maximum elevation
            IF (DummyArray(6) .LT. 0.0) THEN
                CALL SetLastMessage('Maximum flow depth at stream node '//TRIM(IntToText(iStrmNode))//' cannot be less than zero!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            pNodes(iStrmNode)%MaxElev = pNodes(iStrmNode)%BottomElev + DummyArray(6) * FACTLT 
            
            !Cross section data
            pNodes(iStrmNode)%CrossSection%B0 = DummyArray(3) * FACTLT
            pNodes(iStrmNode)%CrossSection%s  = DummyArray(4) * FACTLT
            pNodes(iStrmNode)%CrossSection%n  = DummyArray(5) * FACTN
            
            !Make sure that cross section data is specified properly
            IF (pNodes(iStrmNode)%CrossSection%B0 .EQ. 0.0   .AND.   pNodes(iStrmNode)%CrossSection%s .EQ. 0.0) THEN
                CALL SetLastMessage('B0 and s at stream node '//TRIM(IntToText(iStrmNodeID))//' cannot be both zero!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        
        END DO
    
        !Set the hydraulic disconnect elevations in the Stream-gw connector
        CALL StrmGWConnector%SetDisconnectElevations(pNodes%BottomElev)
    
    END ASSOCIATE
    
    !Clear memory
    DEALLOCATE (iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE ReadCrossSectionData   




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
  ! --- PRINT OUT SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_PrintResults(AppStream,TimeStep,lEndOfSimulation,QTRIB,QROFF,QRTRN,QDRAIN,QRVET,BottomElev,StrmGWConnector,StrmLakeConnector)
    CLASS(AppStream_v50_Type)              :: AppStream
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    LOGICAL,INTENT(IN)                     :: lEndOfSimulation
    REAL(8),INTENT(IN)                     :: QTRIB(:),QROFF(:),QRTRN(:),QDRAIN(:),QRVET(:),BottomElev(:)
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN) :: StrmLakeConnector
  
    !Echo progress
    CALL EchoProgress('Printing results of stream simulation')
    
    !Print stream flow hydrographs
    IF (AppStream%StrmHyd%IsOutFileDefined()) &
      CALL AppStream%StrmHyd%PrintResults(AppStream%State,BottomElev,TimeStep,lEndOfSimulation)
    
    !Print stream reach budget
    IF (AppStream%StrmReachBudRawFile_Defined) CALL WriteStrmReachFlowsToBudRawFile(QTRIB,QROFF,QRTRN,QDRAIN,QRVET,StrmGWConnector,StrmLakeConnector,AppStream)
    
    !Print stream node budget
    IF (AppStream%StrmNodeBudget%StrmNodeBudRawFile_Defined) CALL WriteStrmNodeFlowsToBudRawFile(QTRIB,QROFF,QRTRN,QDRAIN,QRVET,StrmGWConnector,StrmLakeConnector,AppStream)
    
    !Print diversion details
    CALL AppStream%AppDiverBypass%PrintResults()
    
    !Print end-of-simulation flows
    IF (lEndOfSimulation) THEN
        IF (AppStream%FinalFlowFile%iGetFileType() .NE. f_iUNKNOWN) CALL PrintFinalFlows(AppStream%State%Flow,TimeStep,AppStream%FinalFlowFile)
    END IF
    
  END SUBROUTINE AppStream_v50_PrintResults

  
  ! -------------------------------------------------------------
  ! ---PRINT END-OF-SIMULATION FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalFlows(Flows,TimeStep,OutFile)
    REAL(8),INTENT(IN)            :: Flows(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER   :: indxNode
    CHARACTER :: SimulationTime*21,Text*500
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF
    
    !Prepare time unit of flow line
    WRITE (Text,'(5X,A6,15X,A8)') TimeStep%Unit,'/ TUNITQ'
    
    !Print header
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C ***** STREAM FLOWS AT '//TRIM(SimulationTime))
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C')    
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    CALL OutFile%WriteData('     1                    / ICTYPE')
    CALL OutFile%WriteData(TRIM(Text))
    CALL OutFile%WriteData('     1.0                  / FACTHQ')
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    CALL OutFile%WriteData('C     IR               HQS')
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    
    !Print final flows
    DO indxNode=1,SIZE(Flows)
        WRITE (Text,'(I8,F18.3)') indxNode,Flows(indxNode)
        CALL OutFile%WriteData(TRIM(Text))
    END DO

  END SUBROUTINE PrintFinalFlows

  
  ! -------------------------------------------------------------
  ! --- WRITE PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_WritePreprocessedData(AppStream,OutFile)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream
    TYPE(GenericFileType)                :: OutFile
    
    !LOcal variables
    CHARACTER(:),ALLOCATABLE :: cVersionLocal
    
    !Write version number
    cVersionLocal = AppStream%Version%GetVersion()
    CALL OutFile%WriteData(LEN(cVersionLocal))
    CALL OutFile%WriteData(cVersionLocal)
    
    !Routed/non-routed flag
    CALL OutFile%WriteData(AppStream%lRouted)
    
    !Write dimensions
    CALL OutFile%WriteData(AppStream%NStrmNodes)
    CALL OutFile%WriteData(AppStream%NReaches)
    
    !Write node data
    CALL StrmNode_v50_WritePreprocessedData(AppStream%Nodes,OutFile)
    
    !Write reach data
    CALL StrmReach_WritePreprocessedData(AppStream%Reaches,OutFile)
        
  END SUBROUTINE AppStream_v50_WritePreprocessedData
  
  
  ! -------------------------------------------------------------
  ! --- WRITE PREPROCESSED DATA TO TEXT FILE
  ! --- Note: Assumes Standard Output File is opened
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_WriteDataToTextFile(AppStream,iGWNodeIDs,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                   :: iGWNodeIDs(:)
    CHARACTER(LEN=*),INTENT(IN)          :: UNITLTOU
    REAL(8),INTENT(IN)                   :: FACTLTOU
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    INTEGER             :: indxReach,indxNode,iGWNode,iLayer,ErrorCode,iGWNodeID,iStrmNodeIDs(AppStream%NStrmNodes)
    REAL(8)             :: GSElev,AquiferBottom,StrmBottom,DELZ,DELA
    INTEGER,ALLOCATABLE :: UpstrmNodes(:),iGWNodes(:)
    CHARACTER           :: ALine*1000
    
    !Initialize
    iStat        = 0
    iStrmNodeIDs = AppStream%Nodes%ID
    
    !If there are no streams, write relevant information and return
    IF (AppStream%NStrmNodes .EQ. 0) THEN
      CALL LogMessage('***** THERE ARE NO STREAM NODES *****',f_iMessage,'',f_iFILE) 
      RETURN
    END IF
    
    !Initialize
    CALL StrmGWConnector%GetAllGWNodes(iGWNodes)
    
    !Write titles
    CALL LogMessage(' REACH STREAM GRID     GROUND   INVERT             AQUIFER   ALLUVIAL    UPSTREAM',f_iMessage,'',f_iFILE)
    CALL LogMessage('   NO.   NO.   NO.     ELEV.     ELEV.     DEPTH    BOTTOM  THICKNESS      NODES',f_iMessage,'',f_iFILE)
    CALL LogMessage('                           (ALL UNITS ARE IN '//TRIM(UNITLTOU)//')',f_iMessage,'',f_iFILE)
    
    !Write stream reach data
    DO indxReach=1,AppStream%NReaches
        DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
            iGWNode       = iGWNodes(indxNode)
            iGWNodeID     = iGWNodeIDs(iGWNode)
            iLayer        = Stratigraphy%TopActiveLayer(iGWNode)
            GSElev        = Stratigraphy%GSElev(iGWNode)
            AquiferBottom = Stratigraphy%BottomElev(iGWNode,iLayer)
            StrmBottom    = AppStream%Nodes(indxNode)%BottomElev
            DELZ          = GSElev - StrmBottom
            DELA          = StrmBottom - AquiferBottom
            CALL AppStream_v50_GetUpstrmNodes(AppStream,indxNode,UpstrmNodes)
            WRITE (ALine,'(1X,3I6,5F10.1,5X,10(I4,1X))') AppStream%Reaches(indxReach)%ID           , &
                                                         iStrmNodeIDs(indxNode)                    , &
                                                         iGWNodeID                                 , &
                                                         GSElev*FACTLTOU                           , &
                                                         StrmBottom*FACTLTOU                       , &
                                                         DELZ*FACTLTOU                             , &
                                                         AquiferBottom*FACTLTOU                    , &
                                                         DELA*FACTLTOU                             , &
                                                         iStrmNodeIDs(UpstrmNodes)
            CALL LogMessage(TRIM(ALine),f_iMessage,'',f_iFILE)
        END DO
        CALL LogMessage('',f_iMessage,'',f_iFILE)
    END DO
    
    !Clear memory
    DEALLOCATE (UpstrmNodes , iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE AppStream_v50_WriteDataToTextFile
  
  
  ! -------------------------------------------------------------
  ! --- WRITE RAW STREAM NODE BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteStrmNodeFlowsToBudRawFile(QTRIB,QROFF,QRTRN,QDRAIN,QRVET,StrmGWConnector,StrmLakeConnector,AppStream)
    REAL(8),DIMENSION(:),INTENT(IN)        :: QTRIB,QROFF,QRTRN,QDRAIN,QRVET
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN) :: StrmLakeConnector
    TYPE(AppStream_v50_Type)               :: AppStream
   
    !Local variables
    INTEGER                                               :: iNode,indxNode
    REAL(8)                                               :: DummyArray(f_iNStrmBudColumns,AppStream%StrmNodeBudget%NBudNodes)
    REAL(8),DIMENSION(AppStream%StrmNodeBudget%NBudNodes) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,                 &
                                                             Runoff,ReturnFlows,StrmGWFlows_InModel,LakeInflows,Error,              &
                                                             Diversions,Bypasses,DiversionShorts,RiparianET,StorChange,             &
                                                             StrmGWFlows_OutModel
    INTEGER,ALLOCATABLE                                   :: UpstrmNodes(:)
    
    ASSOCIATE (pNodes => AppStream%Nodes  , &
               pState => AppStream%State  )
        
        !Iterate over nodes
        DO indxNode=1,AppStream%StrmNodeBudget%NBudNodes
          iNode = AppStream%StrmNodeBudget%iBudNodes(indxNode)
      
          !Upstream flows
          CALL AppStream%GetUpstrmNodes(iNode,UpstrmNodes)
          UpstrmFlows(indxNode) = SUM(pState(UpStrmNodes)%Flow)
          IF (AppStream%StrmInflowData%lDefined) UpstrmFlows(indxNode) = UpstrmFlows(indxNode) + AppStream%StrmInflowData%Inflows(iNode)
      
          !Downstream flows
          DownstrmFlows(indxNode) = AppStream%State(iNode)%Flow
      
          !Change in storage
          StorChange(indxNode) = AppStream%StorChange(iNode)
      
          !Tributary flows
          TributaryFlows(indxNode) = QTRIB(iNode)
      
          !Inflows from tile drains
          DrainInflows(indxNode) = QDRAIN(iNode)
      
          !Runoff
          Runoff(indxNode) = QROFF(iNode)

          !Return flow
          ReturnFlows(indxNode) = QRTRN(iNode)
      
          !Stream-gw interaction occuring inside the model
          !(+: flow from stream to groundwater, so multiply with - to represent Gain from GW)
          StrmGWFlows_InModel(indxNode) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iNode,iNode,lInsideModel=.TRUE.)
          
          !Stream-gw interaction occuring outside the model
          !(+: flow from stream to groundwater, so multiply with - to represent Gain from GW)
          StrmGWFlows_OutModel(indxNode) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iNode,iNode,lInsideModel=.FALSE.)
    
          !Inflow from lakes
          LakeInflows(indxNode) = StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,iNode)
      
          !Riparian ET
          RiparianET(indxNode) = QRVET(iNode)
      
        END DO
    END ASSOCIATE
    
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetNodeDiversions(AppStream%StrmNodeBudget%iBudNodes)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetNodeNetBypass(AppStream%StrmNodeBudget%iBudNodes)
    
    !Error
    Error =  UpstrmFlows            &
           - DownstrmFlows          &
           - StorChange             &
           + TributaryFlows         &
           + DrainInflows           &
           + Runoff                 &
           + ReturnFlows            &
           + StrmGWFlows_InModel    &
           + StrmGWFlows_OutModel   &
           + LakeInflows            &
           - RiparianET             &
           - Diversions             &
           - Bypasses
           
    !Diversion shortages
    DiversionShorts = AppStream%AppDiverBypass%GetNodeDiversionShort(AppStream%StrmNodeBudget%iBudNodes)
           
    !Compile data in array
    DummyArray(1,:)  = UpstrmFlows
    DummyArray(2,:)  = DownstrmFlows
    DummyArray(3,:)  = StorChange
    DummyArray(4,:)  = TributaryFlows
    DummyArray(5,:)  = DrainInflows
    DummyArray(6,:)  = Runoff
    DummyArray(7,:)  = ReturnFlows
    DummyArray(8,:)  = StrmGWFlows_InModel
    DummyArray(9,:)  = StrmGWFlows_OutModel
    DummyArray(10,:)  = LakeInflows
    DummyArray(11,:) = RiparianET
    DummyArray(12,:) = Diversions
    DummyArray(13,:) = Bypasses
    DummyArray(14,:) = Error
    DummyArray(15,:) = DiversionShorts
    
    !Print out values to binary file
    CALL AppStream%StrmNodeBudget%StrmNodeBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteStrmNodeFlowsToBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- WRITE RAW STREAM REACH BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteStrmReachFlowsToBudRawFile(QTRIB,QROFF,QRTRN,QDRAIN,QRVET,StrmGWConnector,StrmLakeConnector,AppStream)
    REAL(8),DIMENSION(:),INTENT(IN)        :: QTRIB,QROFF,QRTRN,QDRAIN,QRVET
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN) :: StrmLakeConnector
    TYPE(AppStream_v50_Type)               :: AppStream
    
    !Local variables
    INTEGER                               :: indxReach,indxReach1,iNode,iUpstrmReach,iUpstrmNode,       &
                                             iDownstrmNode,indx,iReach
    REAL(8)                               :: DummyArray(f_iNStrmBudColumns,AppStream%NReaches) 
    REAL(8),DIMENSION(AppStream%NReaches) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,     &
                                             Runoff,ReturnFlows,StrmGWFlows_InModel,LakeInflows,Error,  &
                                             Diversions,Bypasses,DiversionShorts,RiparianET,StorChange, &
                                             StrmGWFlows_OutModel
    
    !Initialize           
    UpstrmFlows = 0.0
    
    !Iterate over reaches
    DO indxReach=1,AppStream%NReaches
        iReach        = AppStream%iPrintReachBudgetOrder(indxReach)
        iUpstrmNode   = AppStream%Reaches(iReach)%UpstrmNode
        iDownstrmNode = AppStream%Reaches(iReach)%DownstrmNode
        !Upstream flows
        DO indxReach1=1,AppStream%Reaches(iReach)%NUpstrmReaches
            iUpstrmReach        = AppStream%Reaches(iReach)%UpstrmReaches(indxReach1)
            iNode               = AppStream%Reaches(iUpstrmReach)%DownstrmNode
            UpstrmFlows(iReach) = UpstrmFlows(iReach) + AppStream%State(iNode)%Flow
        END DO
        IF (AppStream%StrmInflowData%lDefined) UpstrmFlows(iReach) = UpstrmFlows(iReach) + SUM(AppStream%StrmInflowData%Inflows(iUpstrmNode:iDownstrmNode))
        
        !Change in storage
        StorChange(iReach) = SUM(AppStream%StorChange(iUpstrmNode:iDownstrmNode))
        
        !Tributary flows
        TributaryFlows(iReach) = SUM(QTRIB(iUpstrmNode:iDownstrmNode))
        
        !Inflows from tile drains
        DrainInflows(iReach) = SUM(QDRAIN(iUpstrmNode:iDownstrmNode))
        
        !Runoff
        Runoff(iReach) = SUM(QROFF(iUpstrmNode:iDownstrmNode))
        
        !Return flow
        ReturnFlows(iReach) = SUM(QRTRN(iUpstrmNode:iDownstrmNode))
        
        !Stream-gw interaction inside the model
        !(+: flow from stream to groundwater)
        StrmGWFlows_InModel(iReach) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iUpstrmNode,iDownstrmNode,lInsideModel=.TRUE.)
        
        !Stream-gw interaction outside the model
        !(+: flow from stream to groundwater)
        StrmGWFlows_OutModel(iReach) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iUpstrmNode,iDownstrmNode,lInsideModel=.FALSE.)
        
        !Inflow from lakes
        LakeInflows(iReach) = 0.0
        DO indx=iUpstrmNode,iDownstrmNode
            LakeInflows(iReach) = LakeInflows(iReach) + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indx)
        END DO
        
        !Riparian ET
        RiparianET(iReach) = SUM(QRVET(iUpstrmNode:iDownstrmNode))
      
    END DO
    
    !Downstream flows
    DownstrmFlows = AppStream%State(AppStream%Reaches%DownStrmNode)%Flow
      
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetReachDiversions(AppStream%NReaches,AppStream%Reaches)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetReachNetBypass(AppStream%NStrmNodes,AppStream%NReaches,AppStream%Reaches)
    
    !Error
    Error =  UpstrmFlows            &
           - DownstrmFlows          &
           - StorChange             &
           + TributaryFlows         &
           + DrainInflows           &
           + Runoff                 &
           + ReturnFlows            &
           + StrmGWFlows_InModel    &
           + StrmGWFlows_OutModel   &
           + LakeInflows            &
           - RiparianET             &
           - Diversions             &
           - Bypasses
           
    !Diversion shortages
    DiversionShorts = AppStream%AppDiverBypass%GetReachDiversionShort(AppStream%NStrmNodes,AppStream%NReaches,AppStream%Reaches)
           
    !Compile data in array
    DummyArray(1,:)  = UpstrmFlows
    DummyArray(2,:)  = DownstrmFlows
    DummyArray(3,:)  = StorChange
    DummyArray(4,:)  = TributaryFlows
    DummyArray(5,:)  = DrainInflows
    DummyArray(6,:)  = Runoff
    DummyArray(7,:)  = ReturnFlows
    DummyArray(8,:)  = StrmGWFlows_InModel
    DummyArray(9,:)  = StrmGWFlows_OutModel
    DummyArray(10,:)  = LakeInflows
    DummyArray(11,:) = RiparianET
    DummyArray(12,:) = Diversions
    DummyArray(13,:) = Bypasses
    DummyArray(14,:) = Error
    DummyArray(15,:) = DiversionShorts
    
    !Print out values to binary file
    CALL AppStream%StrmReachBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteStrmReachFlowsToBudRawFile
  
  
  

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
  ! --- ADVANCE STATE OF STREAMS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_AdvanceState(AppStream)
    CLASS(AppStream_v50_Type) :: AppStream
    
    !Local variables
    INTEGER :: indxNode
    
    AppStream%State%Head_P = AppStream%State%Head
    
    DO indxNode=1,AppStream%NStrmNodes
        AppStream%Nodes(indxNode)%Area_P = AppStream%Nodes(indxNode)%Area(AppStream%State(indxNode)%Head_P)
    END DO
    
  END SUBROUTINE AppStream_v50_AdvanceState
  

  ! -------------------------------------------------------------
  ! --- CONVERT STREAM FLOWS TO STREAM SURFACE ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_ConvertFlowToElev(AppStream)
    CLASS(AppStream_v50_Type) :: AppStream
    
    !Local variables
    INTEGER :: indxNode
    
    DO indxNode=1,AppStream%NStrmNodes
        AppStream%State(indxNode)%Head = AppStream%Nodes(indxNode)%Head(AppStream%State(indxNode)%Flow)
    END DO
    
  END SUBROUTINE AppStream_v50_ConvertFlowToElev

  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF STREAMS RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_ConvertTimeUnit(AppStream,NewUnit)
    CLASS(AppStream_v50_Type)   :: AppStream
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    INTEGER :: indxNode
    REAL(8) :: Factor
    
    !Convert bypass rating table time units
    CALL AppStream%AppDiverBypass%ConvertTimeUnit(NewUnit)
    
    !Convert time unit of Manning's roughness coefficient (second) to simulation unit of time
    Factor = TimeIntervalConversion(NewUnit,'1MIN') * 60D0 !Must multiply with 60 to convert minute to seconds
    IF (Factor .NE. 1.0) THEN
        DO indxNode=1,AppStream%NStrmNodes
            AppStream%Nodes(indxNode)%CrossSection%n = AppStream%Nodes(indxNode)%CrossSection%n / Factor
            !Update the initial flows
            AppStream%State(indxNode)%Flow = AppStream%State(indxNode)%Flow * Factor
        END DO
    END IF
    
  END SUBROUTINE AppStream_v50_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE STREAM FLOWS 
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_Simulate(AppStream,GWHeads,Runoff,ReturnFlow,TributaryFlow,DrainInflows,RiparianET,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v50_Type)   :: AppStream
    REAL(8),INTENT(IN)          :: GWHeads(:,:),Runoff(:),ReturnFlow(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    REAL(8),INTENT(OUT)         :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    TYPE(MatrixType)            :: Matrix
    
    !Local variables
    INTEGER                                 :: indx,indxReach,indxNode,iUpstrmNode,indxUpstrmNode,iNodes_Connect(20),        &
                                               iUpNode,ErrorCode,iDownstrmNode,inConnectedNodes,NNodes,NDiver,iDim
    REAL(8)                                 :: rHead,rInflow,rOutflow,rBypass_Recieved,rUpdateCoeff(20),rCoeff,rDeltaT,      &         
                                               rFlow,RHSMin,rBypassOut,rRipET
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: rBCInflows,rUpdateRHS,HRG,rAvailableFlows,rdArea_dStage,rdFlow_dStage,rArea,  &
                                               rStrmGWFlow_AtMinHead
    INTEGER,ALLOCATABLE                     :: iStrmNodes(:),iLakes(:)
    INTEGER,PARAMETER                       :: iCompIDs_Connect(20) = f_iStrmComp
    
    !Inform user about simulation progress
    CALL EchoProgress('Simulating stream flows')
    
    !Initialize
    NNodes     = SIZE(GWHeads , DIM=1)
    NDiver     = AppStream%AppDiverBypass%NDiver
    rDeltaT    = AppStream%DeltaT
    rBCInflows = AppStream%StrmInflowData%GetInflows_AtAllNodes(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Get groundwater heads at stream nodes
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
    
    !Stream-gw interaction at minimum stream head (= stream bottom elevation)
    CALL StrmGWConnector%ComputeStrmGWFlow_AtMinHead(AppStream%Nodes%BottomElev,HRG,AppStream%Nodes%MaxElev,AppStream%Nodes,rStrmGWFlow_AtMinHead)
  
    !Initialize bypass flows to zero (only for those that originate within the model)
    DO indx=1,AppStream%AppDiverBypass%NBypass
        IF (AppStream%AppDiverBypass%Bypasses(indx)%iNode_Exp .GT. 0) THEN
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Out      = 0.0
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Received = 0.0
        END IF
    END DO
    
    !Calcuate area, flow, derivates of area and flow for all nodes
    DO indxNode=1,AppStream%NStrmNodes
        rHead                          = AppStream%State(indxNode)%Head
        AppStream%State(indxNode)%Flow = AppStream%Nodes(indxNode)%Flow(rHead)
        rdArea_dStage(indxNode)        = AppStream%Nodes(indxNode)%dArea(rHead)
        rdFlow_dStage(indxNode)        = AppStream%Nodes(indxNode)%dFlow(rHead)
        rArea(indxNode)                = AppStream%Nodes(indxNode)%Area(rHead)
    END DO
    
    !Update the matrix equation
    DO indxReach=1,AppStream%NReaches
        iUpstrmNode   = AppStream%Reaches(indxReach)%UpstrmNode
        iDownstrmNode = AppStream%Reaches(indxReach)%DownstrmNode
        DO indxNode=iUpstrmNode,iDownstrmNode
                
            !Initialize
            rFlow    = AppStream%State(indxNode)%Flow
            rInflow  = 0.0
            rOutflow = 0.0
            
            !Recieved bypass
            rBypass_Recieved = AppStream%AppDiverBypass%GetBypassReceived_AtADestination(f_iFlowDest_StrmNode,indxNode)
                
            !Inflows at the stream node with known values
            rInflow = rBCInflows(indxNode)                                  &    !Inflow as defined by the user
                    + Runoff(indxNode)                                      &    !Direct runoff of precipitation 
                    + ReturnFlow(indxNode)                                  &    !Return flow of applied water 
                    + TributaryFlow(indxNode)                               &    !Tributary inflows from small watersheds and creeks
                    + DrainInflows(indxNode)                                &    !Inflow from tile drains
                    + rBypass_Recieved                                      &    !Received by-pass flows 
                    + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indxNode)      !Flows from lake outflow
            
            !Inflows from upstream nodes
            DO indxUpstrmNode=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iUpNode = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indxUpstrmNode)
                rInflow = rInflow + AppStream%State(iUpNode)%Flow
            END DO 
            
            !Initial estimate of available flow for outflow terms
            IF (indxNode .EQ. iUpstrmNode) THEN
                rCoeff                    = AppStream%Nodes(indxNode)%Length / rDeltaT
                rAvailableFlows(indxNode) = rInflow - rCoeff * (rArea(indxNode) - AppStream%Nodes(indxNode)%Area_P)
                rAvailableFlows(indxNode) = MAX(rAvailableFlows(indxNode) , 0.0)
            ELSE
                rCoeff                    = 0.5d0 * AppStream%Nodes(indxNode)%Length / rDeltaT
                rAvailableFlows(indxNode) = rInflow - rCoeff * (rArea(indxNode) + rArea(indxNode-1) - AppStream%Nodes(indxNode)%Area_P - AppStream%Nodes(indxNode-1)%Area_P)
                rAvailableFlows(indxNode) = MAX(rAvailableFlows(indxNode) , 0.0)
            END IF
            
            !Diversion
            IF (NDiver .GT. 0) THEN
                rOutFlow                                            = MIN(rAvailableFlows(indxNode) , AppStream%AppDiverBypass%NodalDiverRequired(indxNode))
                AppStream%AppDiverBypass%NodalDiverActual(indxNode) = rOutflow
                rAvailableFlows(indxNode)                           = rAvailableFlows(indxNode) - rOutflow
            END IF
                
            !Bypass
            CALL AppStream%AppDiverBypass%ComputeBypass(indxNode,rAvailableFlows(indxNode),StrmLakeConnector,rBypassOut)
            rOutflow                  = rOutflow + rBypassOut
            rAvailableFlows(indxNode) = rAvailableFlows(indxNode) - rBypassOut
            
            !Riparian ET outflow
            IF (RiparianET(indxNode) .GT. 0.0) THEN
                rRipET                    = MIN(RiparianET(indxNode) , rAvailableFlows(indxNode))
                RiparianETFrac(indxNode)  = rRipET / RiparianET(indxNode)
                rOutflow                  = rOutflow + rRipET
                rAvailableFlows(indxNode) = rAvailableFlows(indxNode) - rRipET
            ELSE
                RiparianETFrac(indxNode) = 0.0
            END IF
            
            !Compute the matrix rhs function and its derivatives w.r.t. stream elevation
            !----------------------------------------------------------------------------
            
            !First node of each reach is treated as boundary node
            IF (indxNode .EQ. iUpstrmNode) THEN               
                !RHS function at minimum stream head (to be used for storage correction)
                RHSMin = -rCoeff * AppStream%Nodes(indxNode)%Area_P - rInflow + rStrmGWFlow_AtMinHead(indxNode)
                RHSMin = MAX(RHSMin , 0.0)
                
                !Rate of change in storage
                AppStream%StorChange(indxNode) = rCoeff * (rArea(indxNode) - AppStream%Nodes(indxNode)%Area_P) - RHSMin
                
                !RHS function
                rUpdateRHS(indxNode) = AppStream%StorChange(indxNode) + rFlow - rInflow + rOutFlow
                
                !Update Jacobian - entries for stream node
                iNodes_Connect(1) = indxNode
                rUpdateCoeff(1)   = rCoeff * rdArea_dStage(indxNode) + rdFlow_dStage(indxNode) 
                inConnectedNodes  = AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                IF (rFlow .EQ. 0.0) THEN
                    !If flow is zero because of outflows or correction in the storage, do not consider the effect of upstream nodes on the gradient
                    IF (rOutFlow .GT. 0.0  .OR.  RHSMin .GT. 0.0) THEN
                        iNodes_Connect(2:1+inConnectedNodes) = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes
                        rUpdateCoeff(2:1+inConnectedNodes)   = 0.0
                    !Otherwise, normal calculations for the derivative w.r.t. upstream nodes
                    ELSE
                        DO indxUpstrmNode=1,inConnectedNodes
                            iUpNode                          = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indxUpstrmNode)
                            iNodes_Connect(1+indxUpstrmNode) = iUpNode
                            rUpdateCoeff(1+indxUpstrmNode)   = -rdFlow_dStage(iUpNode)
                        END DO 
                    END IF
                ELSE
                    !Normal calculations for the derivative w.r.t. upstream nodes
                    DO indxUpstrmNode=1,inConnectedNodes
                        iUpNode                          = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indxUpstrmNode)
                        iNodes_Connect(1+indxUpstrmNode) = iUpNode
                        rUpdateCoeff(1+indxUpstrmNode)   = -rdFlow_dStage(iUpNode)
                    END DO 
                END IF
                iDim = inConnectedNodes + 1
                CALL Matrix%UpdateCOEFF(f_iStrmComp,indxNode,iDim,iCompIDs_Connect(1:iDim),iNodes_Connect(1:iDim),rUpdateCoeff(1:iDim)) 
                
            !Otherwise, treat node normal
            ELSE
                !RHS function at minimum stream head (to be used for storage correction)
                RHSMin = rCoeff * (rArea(indxNode-1) - AppStream%Nodes(indxNode)%Area_P - AppStream%Nodes(indxNode-1)%Area_P) &
                       - rInflow + rStrmGWFlow_AtMinHead(indxNode)
                RHSMin = MAX(RHSMin , 0.0)
                
                !Rate of change in storage
                AppStream%StorChange(indxNode) = rCoeff * (rArea(indxNode) + rArea(indxNode-1) - AppStream%Nodes(indxNode)%Area_P - AppStream%Nodes(indxNode-1)%Area_P) - RHSMin
                
                !RHS function
                rUpdateRHS(indxNode) = AppStream%StorChange(indxNode) + rFlow - rInflow + rOutflow 

                !Update Jacobian - entries for stream node
                iNodes_Connect(1) = indxNode
                rUpdateCoeff(1)   = rCoeff * rdArea_dStage(indxNode) + rdFlow_dStage(indxNode)
                inConnectedNodes  = AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                IF (rFlow .EQ. 0.0) THEN
                    !If flow is zero because of outflows or correction in the storage, do not consider the effect of upstream nodes on the gradient
                    IF (rOutFlow .GT. 0.0  .OR.  RHSMin .GT. 0.0) THEN
                        iNodes_Connect(2:1+inConnectedNodes) = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes
                        rUpdateCoeff(2:1+inConnectedNodes)   = 0.0
                    !Otherwise, normal calculations for the derivative w.r.t. upstream nodes
                    ELSE
                        DO indxUpstrmNode=1,inConnectedNodes
                            iUpNode                          = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indxUpstrmNode)
                            iNodes_Connect(1+indxUpstrmNode) = iUpNode
                            IF (iUpNode .EQ. indxNode-1) THEN
                                rUpdateCoeff(1+indxUpstrmNode) = rCoeff * rdArea_dStage(indxNode-1) - rdFlow_dStage(indxNode-1)
                            ELSE
                                rUpdateCoeff(1+indxUpstrmNode) = -rdFlow_dStage(iUpNode) 
                            END IF
                        END DO
                    END IF
                ELSE
                    !Normal calculations for the derivative w.r.t. upstream nodes
                    DO indxUpstrmNode=1,inConnectedNodes
                        iUpNode                          = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indxUpstrmNode)
                        iNodes_Connect(1+indxUpstrmNode) = iUpNode
                        IF (iUpNode .EQ. indxNode-1) THEN
                            rUpdateCoeff(1+indxUpstrmNode) = rCoeff * rdArea_dStage(indxNode-1) - rdFlow_dStage(indxNode-1)
                        ELSE
                            rUpdateCoeff(1+indxUpstrmNode) = -rdFlow_dStage(iUpNode) 
                        END IF
                    END DO
                END IF
                iDim = inConnectedNodes + 1
                CALL Matrix%UpdateCOEFF(f_iStrmComp,indxNode,iDim,iCompIDs_Connect(1:iDim),iNodes_Connect(1:iDim),rUpdateCoeff(1:iDim)) 
                
            END IF
        END DO
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iStrmComp,1,rUpdateRHS)
        
    !Stream flows to lakes
    CALL StrmLakeConnector%GetSourceIDs(f_iStrmToLakeFlow,iStrmNodes)
    CALL StrmLakeConnector%GetDestinationIDs(f_iStrmToLakeFlow,iLakes)
    DO indxNode=1,SIZE(iStrmNodes)
      CALL StrmLakeConnector%SetFlow(f_iStrmToLakeFlow,iStrmNodes(indxNode),iLakes(indxNode),AppStream%State(iStrmNodes(indxNode))%Flow)
    END DO
        
    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate(NNodes,HRG,AppStream%State%Head,rAvailableFlows,Matrix,AppStream%Nodes,AppStream%Nodes%MaxElev)  
    
    !Clear memory
    DEALLOCATE (iStrmNodes , iLakes , STAT=ErrorCode)  
  
  END SUBROUTINE AppStream_v50_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE LIST OF NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  SUBROUTINE CompileUpstrmNodes(AppStream)
    TYPE(AppStream_v50_Type) :: AppStream
    
    !Local variables
    INTEGER :: indxReach,iCount,TempNodes(50),indxNode,indxReach1,iUpstrmNode,iDownstrmNode
    
    !Iterate over each reach and node
    DO indxReach=AppStream%NReaches,1,-1
        iUpstrmNode   = AppStream%Reaches(indxReach)%UpstrmNode
        iDownstrmNode = AppStream%Reaches(indxReach)%DownstrmNode
        DO indxNode=iUpstrmNode,iDownstrmNode
        
            !Initialize counter
            iCount = 0
            
            !indxNode is the first node in reach
            IF (indxNode .EQ. iUpstrmNode) THEN
                DO indxReach1=1,indxReach-1
                    IF (AppStream%Reaches(indxReach1)%OutflowDestType .NE. f_iFlowDest_StrmNode) CYCLE
                    IF (AppStream%Reaches(indxReach1)%OutflowDest .EQ. indxNode) THEN
                        iCount            = iCount + 1
                        TempNodes(iCount) = AppStream%Reaches(indxReach1)%DownstrmNode
                    END IF
                END DO
                ALLOCATE (AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(iCount))
                AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes = iCount
                AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes  = TempNodes(1:iCount)
            
            !indxNode is not the first node in the reach
            ELSE
                iCount       = 1
                TempNodes(1) = indxNode - 1
                DO indxReach1=1,indxReach-1
                    IF (AppStream%Reaches(indxReach1)%OutflowDestType .NE. f_iFlowDest_StrmNode) CYCLE
                    IF (AppStream%Reaches(indxReach1)%OutflowDest .EQ. indxNode) THEN
                        iCount            = iCount + 1
                        TempNodes(iCount) = AppStream%Reaches(indxReach1)%DownstrmNode
                    END IF
                END DO
                ALLOCATE (AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(iCount))
                AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes = iCount
                AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes  = TempNodes(1:iCount)
            END IF
            
            !Order the upstream nodes
            CALL ShellSort(AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes)
            
        END DO
    END DO
    
  END SUBROUTINE CompileUpstrmNodes 
  

  ! -------------------------------------------------------------
  ! --- MODIFY HEADS USING DELTA_HEADS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_UpdateHeads(AppStream,HDelta)
    CLASS(AppStream_v50_Type) :: AppStream
    REAL(8),INTENT(IN)        :: HDelta(:)
    
    AppStream%State%Head = MIN(MAX(AppStream%State%Head-HDelta , AppStream%Nodes%BottomElev)  ,  AppStream%Nodes%MaxElev)
    
  END SUBROUTINE AppStream_v50_UpdateHeads
   

  ! -------------------------------------------------------------
  ! --- COMPILE DISTANCE BETWEEN NODES, SLOPE AND LENGTH ASSOCIATED WITH EACH NODE
  ! -------------------------------------------------------------
  SUBROUTINE CompileDistanceLengthSlope(GWNodes,AppGrid,AppStream,iStat)
    INTEGER,INTENT(IN)           :: GWNodes(:)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(AppStream_v50_Type)     :: AppStream
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure = ModName // 'CompileDistanceLengthSlope' 
    INTEGER                      :: indxReach,indxNode,iUpstrmNode,iDownstrmNode,iGWDownstrmNode,iGWNode
    REAL(8)                      :: B_Distance,F_Distance,CA,CB
    
    !Initialize
    iStat = 0
    
    ASSOCIATE (pReaches => AppStream%Reaches , &
               pNodes   => AppStream%Nodes   )
        !Loop over reaches
        DO indxReach=1,AppStream%NReaches
            iUpstrmNode   = pReaches(indxReach)%UpstrmNode
            iDownstrmNode = pReaches(indxReach)%DownstrmNode
            B_Distance    = 0.0
            iGWNode       = GWNodes(iUpstrmNode)
            !Loop over nodes
            DO indxNode=iUpstrmNode,iDownstrmNode-1
                !Corresponding GW nodes
                iGWDownstrmNode = GWNodes(indxNode+1)
                
                !Distance to the next node node
                CA         = AppGrid%X(iGWDownstrmNode) - AppGrid%X(iGWNode)
                CB         = AppGrid%Y(iGWDownstrmNode) - AppGrid%Y(iGWNode)
                F_Distance = SQRT(CA*CA + CB*CB)
                
                !Stream segment length
                pNodes(indxNode)%Length = (F_Distance + B_Distance) / 2d0
                
                !Slope
                IF (indxNode .GT. iUpstrmNode) THEN
                    pNodes(indxNode)%Slope = (pNodes(indxNode-1)%BottomElev - pNodes(indxNode)%BottomElev) / B_Distance
                    IF (pNodes(indxNode)%Slope .LE. 0.0) THEN
                        CALL SetLastMessage('Slope at stream node '//TRIM(IntToText(AppStream%Nodes(indxNode)%ID))//' must be greater than zero!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
                
                !Advance variables
                iGwNode    = iGWDownstrmNode
                B_Distance = F_Distance 
                
            END DO
            pNodes(iDownstrmNode)%Length = B_Distance / 2d0
            
            !Slope at the first and last node of reach
            pNodes(iDownstrmNode)%Slope = (pNodes(iDownstrmNode-1)%BottomElev - pNodes(iDownstrmNode)%BottomElev) / B_Distance
            pNodes(iUpstrmNode)%Slope   = pNodes(iUpstrmNode+1)%Slope
            IF (pNodes(iUpstrmNode)%Slope .LE. 0.0) THEN
                CALL SetLastMessage('Slope at stream node '//TRIM(IntToText(AppStream%Nodes(iUpstrmNode)%ID))//' must be greater than zero!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (pNodes(iDownstrmNode)%Slope .LE. 0.0) THEN
                CALL SetLastMessage('Slope at stream node '//TRIM(IntToText(AppStream%Nodes(iDownstrmNode)%ID))//' must be greater than zero!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END ASSOCIATE
    
  END SUBROUTINE CompileDistanceLengthSlope
  
  
  ! -------------------------------------------------------------
  ! --- FUNCTION TO PREPARE THE BUDGET HEADER DATA FOR STREAM BUDGETS
  ! --- (REDEFINES THE PROCEDURE IN Class_BaseAppStream WITH THE SAME NAME)
  ! -------------------------------------------------------------
  FUNCTION PrepareStreamBudgetHeader(NLocations,iPrintReachBudgetOrder,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,cReachNames,iBudNodes) RESULT(Header)
    INTEGER,INTENT(IN)                   :: NLocations,iPrintReachBudgetOrder(:),iReachIDs(:),iStrmNodeIDs(:),NTIME
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)          :: cVersion
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cReachNames(:)
    INTEGER,OPTIONAL,INTENT(IN)          :: iBudNodes(:)
    TYPE(BudgetHeaderType)               :: Header
    
    !Local variables
    INTEGER,PARAMETER           :: TitleLen           = 212  , &
                                   NTitles            = 3    , &
                                   NColumnHeaderLines = 4    
    INTEGER                     :: iCount,indxLocation,indxCol,indx,I,ID,iReach
    TYPE(TimeStepType)          :: TimeStepLocal
    CHARACTER                   :: UnitT*10,TextTime*17
    LOGICAL                     :: lNodeBudOutput
    CHARACTER(LEN=21),PARAMETER :: FParts(f_iNStrmBudColumns)=['UPSTRM_INFLOW'         , &
                                                               'DOWNSTRM_OUTFLOW'      , &
                                                               'STORAGE_CHANGE'        , &
                                                               'TRIB_INFLOW'           , & 
                                                               'TILE_DRN'              , & 
                                                               'RUNOFF'                , & 
                                                               'RETURN_FLOW'           , & 
                                                               'GAIN_FROM_GW_INMODEL'  , &
                                                               'GAIN_FROM_GW_OUTMODEL' , &
                                                               'GAIN_FROM_LAKE'        , & 
                                                               'RIPARIAN_ET'           , &
                                                               'DIVERSION'             , & 
                                                               'BYPASS'                , & 
                                                               'DISCREPANCY'           , & 
                                                               'DIVER_SHORTAGE'        ]
                                                             
    !Initialize flag for budget type 
    IF (PRESENT(iBudNodes)) THEN
      lNodeBudOutput = .TRUE.
    ELSE
      lNodeBudOutput = .FALSE.
    END IF
   
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
    IF (lNodeBudOutput) THEN
      Header%cBudgetDescriptor = 'stream node budget'
    ELSE
      Header%cBudgetDescriptor = 'stream reach budget'
    END IF

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    Header%NAreas = 0
    ALLOCATE (Header%Areas(0))

    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM STREAM PACKAGE (v'//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('STREAM FLOW BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:2) = .TRUE.
        pASCIIOutput%lTitlePersist(3)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,50(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    Header%NLocations = NLocations
    ALLOCATE (Header%cLocationNames(NLocations))
    IF (lNodeBudOutput) THEN
      DO indx=1,NLocations
          ID                          = iStrmNodeIDs(iBudNodes(indx))
          Header%cLocationNames(indx) = 'NODE '//TRIM(IntToText(ID)) 
      END DO
    ELSE
      DO indx=1,NLocations
          iReach                      = iPrintReachBudgetOrder(indx)
          ID                          = iReachIDs(iReach)
          Header%cLocationNames(indx) = TRIM(cReachNames(iReach)) // '(REACH '// TRIM(IntToText(ID)) // ')' 
      END DO
    END IF
    
    !Locations
    ALLOCATE (Header%Locations(1)                                                             , &
              Header%Locations(1)%cFullColumnHeaders(f_iNStrmBudColumns+1)                    , &
              Header%Locations(1)%iDataColumnTypes(f_iNStrmBudColumns)                        , &
              Header%Locations(1)%iColWidth(f_iNStrmBudColumns+1)                             , &
              Header%Locations(1)%cColumnHeaders(f_iNStrmBudColumns+1,NColumnHeaderLines)     , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)                )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns           = f_iNStrmBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                       
      pLocation%cFullColumnHeaders(2:) = f_cBudgetColumnTitles                         
      pLocation%iDataColumnTypes       = [f_iVR ,&  !Upstream inflow
                                          f_iVR ,&  !Downstream outflow
                                          f_iVR ,&  !Change in storage
                                          f_iVR ,&  !Tributary inflow
                                          f_iVR ,&  !Tile drain
                                          f_iVR ,&  !Runoff
                                          f_iVR ,&  !Return flow
                                          f_iVR ,&  !Gain from GW inside model
                                          f_iVR ,&  !Gain from GW outside model
                                          f_iVR ,&  !Gain from lake
                                          f_iVR ,&  !Riparian ET
                                          f_iVR ,&  !Diversion
                                          f_iVR ,&  !By-pass flow
                                          f_iVR ,&  !Discrepancy
                                          f_iVR ]   !Diversion shortage
      pLocation%iColWidth              = [17,(13,I=1,f_iNStrmBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        TextTime            = ArrangeText(TRIM(UnitT),17)
        pColumnHeaders(:,1) = ['                 ','     Upstream','   Downstream','   Change in ','    Tributary','        Tile ','             ','       Return','Gain from GW ',' Gain from GW','    Gain from','   Riparian ','             ','      By-pass','             ','    Diversion']
        pColumnHeaders(:,2) = ['      Time       ','      Inflow ','    Outflow  ','    Storage  ','     Inflow  ','        Drain','       Runoff','        Flow ','inside Model ','outside Model','      Lake   ','      ET    ','    Diversion','        Flow ','  Discrepancy','    Shortage ']
        pColumnHeaders(:,3) = [           TextTime,'       (+)   ','      (-)    ','      (-)    ','      (+)    ','         (+) ','        (+)  ','        (+)  ','     (+)     ','      (+)    ','       (+)   ','      (-)   ','       (-)   ','        (-)  ','      (=)    ','             ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,15A13)'
        pFormatSpecs(2)     = '(A17,15A13)'
        pFormatSpecs(3)     = '(A17,15A13)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(f_iNStrmBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
                                                   
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNStrmBudColumns*(Header%NLocations)) , pDSSOutput%iDataTypes(1))
      iCount = 1
      IF (lNodeBudOutput) THEN
        DO indxLocation=1,Header%NLocations
          DO indxCol=1,f_iNStrmBudColumns
            pDSSOutput%cPathNames(iCount) = '/IWFM_STRMNODE_BUD/'                                          //  &  !A part
                                            TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                            'VOLUME/'                                                      //  &  !C part
                                            '/'                                                            //  &  !D part
                                            TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                            TRIM(FParts(indxCol))//'/'                                            !F part
            iCount = iCount+1
          END DO
        END DO
      ELSE
        DO indxLocation=1,Header%NLocations
          DO indxCol=1,f_iNStrmBudColumns
            pDSSOutput%cPathNames(iCount) = '/IWFM_STRMRCH_BUD/'                                           //  &  !A part
                                            TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                            'VOLUME/'                                                      //  &  !C part
                                            '/'                                                            //  &  !D part
                                            TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                            TRIM(FParts(indxCol))//'/'                                            !F part
            iCount = iCount+1
          END DO
        END DO
      END IF
      pDSSOutput%iDataTypes = f_iPER_CUM
    END ASSOCIATE
    
  END FUNCTION PrepareStreamBudgetHeader
     

  ! -------------------------------------------------------------
  ! --- OBTAIN TOTAL NUMBER OF STREAM NODES (REDEFINED THE METHOD LISTED IN BaseAppStream CLASS)
  ! -------------------------------------------------------------
  SUBROUTINE CalculateNStrmNodes(DataFile,NReaches,NStrmNodes,iStat)
    TYPE(GenericFileType) :: DataFile
    INTEGER,INTENT(IN)    :: NReaches
    INTEGER,INTENT(OUT)   :: NStrmNodes,iStat
    
    !Local variables
    INTEGER   :: indxReach,indxNode,iDummyArray(2),iDummy
    CHARACTER :: ALine*7
    
    !Initialize
    iStat      = 0
    NStrmNodes = 0
    
    !Read and accumulate number of stream nodes
    DO indxReach=1,NReaches
        CALL DataFile%ReadData(iDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        NStrmNodes = NStrmNodes + iDummyArray(2)
        DO indxNode=1,iDummyArray(2)
            CALL DataFile%ReadData(ALine,iStat)    ;  IF (iStat .EQ. -1) RETURN
        END DO
    END DO
    
    !Rewind the data file back to where it was
    CALL DataFile%RewindFile()
    CALL DataFile%ReadData(ALine,iStat)   ;  IF (iStat .EQ. -1) RETURN   !Stream component version
    CALL DataFile%ReadData(iDummy,iStat)  ;  IF (iStat .EQ. -1) RETURN   !Number of reaches
    
  END SUBROUTINE CalculateNStrmNodes

END MODULE