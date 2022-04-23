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
MODULE Class_AppStream_v50
  USE Class_Version                 , ONLY: ReadVersion
  USE Class_BaseAppStream           , ONLY: BaseAppStreamType                  , &
                                            cDataList_AtStrmReach              , &
                                            cDataList_AtStrmNode
  USE MessageLogger                 , ONLY: SetLastMessage                     , &
                                            LogMessage                         , &
                                            EchoProgress                       , &
                                            FILE                               , &
                                            MessageArray                       , &
                                            iFatal                             , &
                                            iMessage
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE IOInterface
  USE Class_StrmNode_v50            , ONLY: StrmNode_v50_Type                  , &
                                            StrmNode_v50_ReadPreprocessedData  , &
                                            StrmNode_v50_WritePreprocessedData
  USE Class_StrmReach
  USE Package_ComponentConnectors
  USE Package_Discretization
  USE Package_Misc                  , ONLY: FlowDest_Outside                   , &
                                            FlowDest_Lake                      , &
                                            FlowDest_StrmNode                  , &
                                            iStrmComp                          , &
                                            iLocationType_StrmReach            , &
                                            iLocationType_StrmNode
  USE Package_Budget                , ONLY: BudgetHeaderType                   , &
                                            VolumeUnitMarker                   , &
                                            LocationNameMarker                 , &
                                            VR                                 , &
                                            PER_CUM
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
    PROCEDURE,PASS :: SetStaticComponent             => AppStream_v50_SetStaticComponent
    PROCEDURE,PASS :: SetStaticComponentFromBinFile  => ReadPreprocessedData
    PROCEDURE,PASS :: SetDynamicComponent            => AppStream_v50_SetDynamicComponent
    PROCEDURE,PASS :: SetAllComponents               => AppStream_v50_SetAllComponents
    PROCEDURE,PASS :: SetAllComponentsWithoutBinFile => AppStream_v50_SetAllComponentsWithoutBinFile
    PROCEDURE,PASS :: GetSubDataList_AtLocation      => AppStream_v50_GetSubDataList_AtLocation      !!Overriding the method defined in the base class
    PROCEDURE,PASS :: GetUpstrmNodes                 => AppStream_v50_GetUpstrmNodes
    PROCEDURE,PASS :: GetStageFlowRatingTable        => AppStream_v50_GetStageFlowRatingTable
    PROCEDURE,PASS :: GetVersion                     => AppStream_v50_GetVersion
    PROCEDURE,PASS :: GetBottomElevations            => AppStream_v50_GetBottomElevations
    PROCEDURE,PASS :: GetNRatingTablePoints          => AppStream_v50_GetNRatingTablePoints
    PROCEDURE,PASS :: KillImplementation             => AppStream_v50_Kill
    PROCEDURE,PASS :: ReadTSData                     => AppStream_v50_ReadTSData
    PROCEDURE,PASS :: WritePreprocessedData          => AppStream_v50_WritePreprocessedData
    PROCEDURE,PASS :: WriteDataToTextFile            => AppStream_v50_WriteDataToTextFile
    PROCEDURE,PASS :: UpdateHeads                    => AppStream_v50_UpdateHeads
    PROCEDURE,PASS :: ConvertTimeUnit                => AppStream_v50_ConvertTimeUnit
    PROCEDURE,PASS :: ConvertFlowToElev              => AppStream_v50_ConvertFlowToElev
    PROCEDURE,PASS :: Simulate                       => AppStream_v50_Simulate
    PROCEDURE,PASS :: RegisterWithMatrix             => AppStream_v50_RegisterWithMatrix  !Overriding the method defined in the base class
    PROCEDURE,PASS :: PrintResults                   => AppStream_v50_PrintResults        !Overriding the method defined in the base class
    PROCEDURE,PASS :: AdvanceState                   => AppSTream_v50_AdvanceState        !Overriding the method defined in the base class
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
  INTEGER,PARAMETER           :: NStrmBudColumns = 14
  CHARACTER(LEN=25),PARAMETER :: cBudgetColumnTitles(NStrmBudColumns) = ['Upstream Inflow (+)'        , &
                                                                         'Downstream Outflow (-)'     , &
                                                                         'Change in Storage (-)'      , &
                                                                         'Tributary Inflow (+)'       , &
                                                                         'Tile Drain (+)'             , &
                                                                         'Runoff (+)'                 , &
                                                                         'Return Flow (+)'            , &
                                                                         'Gain from Groundwater (+)'  , &
                                                                         'Gain from Lake (+)'         , &
                                                                         'Riparian ET (-)'            , &
                                                                         'Diversion (-)'              , &
                                                                         'By-pass Flow (-)'           , &
                                                                         'Discrepancy (=)'            , &
                                                                         'Diversion Shortage'         ]
  

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
    INTEGER                      :: ErrorCode
    TYPE(GenericFileType)        :: DataFile
    
    !Initialize
    iStat = 0
    
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
    CALL DataFile%ReadData(AppStream%NStrmNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (AppStream%Nodes(AppStream%NStrmNodes) , &
              AppStream%Reaches(AppStream%NReaches) , &
              STAT = ErrorCode                      )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream configuration data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read stream configuration
    CALL ReadStreamConfigData(DataFile,Stratigraphy,StrmGWConnector,StrmLakeConnector,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL DataFile%Kill()
    
  END SUBROUTINE AppStream_v50_SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC PART OF STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type)         :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33) :: ThisProcedure = ModName // 'AppStream_v50_SetDynamicComponent'
    INTEGER                      :: indxNode,ICType,ErrorCode
    TYPE(GenericFileType)        :: MainFile
    CHARACTER(LEN=1000)          :: ALine,DiverFileName,DiverSpecFileName,BypassSpecFileName,DiverDetailBudFileName
    CHARACTER                    :: TimeUnitFlow*6
    TYPE(BudgetHeaderType)       :: BudHeader
    CHARACTER(:),ALLOCATABLE     :: cVersionSim,cVersionPre,cAbsPathFileName
    INTEGER,ALLOCATABLE          :: GWNodes(:)
    REAL(8)                      :: FACTH,DummyArray(AppStream%NStrmNodes,2),TimeFactor
    INTEGER,PARAMETER            :: ICType_H      = 0                     , &
                                    ICType_Q      = 1                     , &
                                    ICTypeList(2) = [ICType_H , ICType_Q]
    
    !Initialize
    iStat = 0
  
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
        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
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
        CALL AppStream%StrmInflowData%New(cAbsPathFileName,TimeStep,AppStream%NStrmNodes,iStat)
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
        ALine = StripTextUntilCharacter(ALine,'/') 
        CALL CleanSpecialCharacters(ALine)
        IF (ALine .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
            IF (IsForInquiry) THEN
                CALL AppStream%StrmReachBudRawFile%New(cAbsPathFileName,iStat)
                IF (iStat .EQ. -1) RETURN
            ELSE
                BudHeader = PrepareStreamBudgetHeader(AppStream%NReaches,NTIME,TimeStep,AppStream%GetVersion())
                CALL AppStream%StrmReachBudRawFile%New(cAbsPathFileName,BudHeader,iStat)
                IF (iStat .EQ. -1) RETURN
                CALL BudHeader%Kill()
            END IF
            AppStream%StrmReachBudRawFile_Defined = .TRUE.
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
    CALL AppStream%StrmHyd%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,AppStream%NStrmNodes,TimeStep,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream budget at selected segments
    CALL AppStream%StrmNodeBudget%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,NTIME,TimeStep,AppStream%GetVersion(),PrepareStreamBudgetHeader,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream bed parameters for stream-gw connectivity
    CALL StrmGWConnector%CompileConductance(MainFile,AppGrid,AppStream%NStrmNodes,AppStream%Reaches%UpstrmNode,AppStream%Reaches%DownstrmNode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream channel parameters
    CALL ReadCrossSectionData(Stratigraphy,StrmGWConnector,MainFile,AppStream,iStat)
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
        CALL SetLastMessage('Initial condition type '//TRIM(IntToText(ICType))//' is not recognized!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    TimeUnitFlow = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    !Make sure time unit for flow initial conditions is specified
    IF (ICType .EQ. ICType_Q) THEN
        IF (TimeUnitFlow .EQ. '') THEN
            CALL SetLastMessage('The time unit for stream initial conditions must be specified if initial conditions are given as flows!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    CALL MainFile%ReadData(FACTH,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL MainFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indxNode=1,AppStream%NStrmNodes
        !Make sure that nodes are entered sequentially
        IF (INT(DummyArray(indxNode,1)) .NE. indxNode) THEN
            MessageArray(1) = 'Initial conditions for stream nodes must be entered sequentially!'
            MessageArray(2) = 'Expected stream node = '//TRIM(IntToText(indxNode))
            MessageArray(3) = 'Entered stream node  = '//TRIM(IntToText(INT(DummyArray(indxNode,1)))) 
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Assign initial conditions
        SELECT CASE (ICType)
            CASE (ICType_H)
                AppStream%State(indxNode)%Head   = MAX(AppStream%Nodes(indxNode)%BottomElev + DummyArray(indxNode,2) * FACTH   ,   AppStream%Nodes(indxNode)%BottomElev)
                AppStream%State(indxNode)%Head_P = AppStream%State(indxNode)%Head
                AppStream%State(indxNode)%Flow   = AppStream%Nodes(indxNode)%Flow(AppStream%State(indxNode)%Head)
            
            CASE (ICType_Q)
                !At this point Manning's roughness time unit is seconds. Convert flow time unit to seconds and 
                !compute corresponding head accordingly. Later, flow time unit will be converted to simulation 
                !time unit along with all time units of the parameters in this component.
                TimeFactor                       = TimeIntervalConversion(TimeUnitFlow,'1MIN') * 60D0 !Must multiply with 60 to convert minute to seconds
                AppStream%State(indxNode)%Flow   = DummyArray(indxNode,2) * FACTH / TimeFactor
                AppStream%State(indxNode)%Head   = MAX(AppStream%Nodes(indxNode)%Head(AppStream%State(indxNode)%Flow)   ,   AppStream%Nodes(indxNode)%BottomElev)
                AppStream%State(indxNode)%Head_P = AppStream%State(indxNode)%Head
                IF (AppStream%State(indxNode)%Head .EQ. -9999.9999d0) THEN
                    CALL SetLastMessage('There was a convergence problem in converting the initial flow for stream node '//TRIM(IntToText(indxNode))//' to stream flow depth!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
        END SELECT
            
        !Flow area
        AppStream%Nodes(indxNode)%Area_P = AppStream%Nodes(indxNode)%Area(AppStream%State(indxNode)%Head_P)
        
    END DO 
    
    !Diversions and bypasses
    CALL AppStream%AppDiverBypass%New(IsForInquiry,DiverSpecFileName,BypassSpecFileName,DiverFileName,DiverDetailBudFileName,AppStream%GetVersion(),NTIME,TimeStep,AppStream%NStrmNodes,AppStream%Reaches,AppGrid,StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN

    !Clear memory
    DEALLOCATE (GWNodes , STAT=ErrorCode)
    
    !Close main file
    CALL MainFile%Kill() 
  
  END SUBROUTINE AppStream_v50_SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                    :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME
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
    CALL AppStream_v50_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that if static part is defined, so is the dynamic part
    IF (AppStream%NStrmNodes .GT. 0) THEN
      IF (SIZE(AppStream%State) .EQ. 0) THEN
        MessageArray(1) = 'For proper simulation of streams, relevant stream data files must'
        MessageArray(2) = 'be specified when stream nodes are defined in Pre-Processor.'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
    END IF 
    
  END SUBROUTINE AppStream_v50_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                    :: IsRoutedStreams,IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSimFileName,cSimWorkingDirectory
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME
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
    CALL AppStream_v50_SetDynamicComponent(AppStream,IsForInquiry,cSimFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that if static part is defined, so is the dynamic part
    IF (AppStream%NStrmNodes .GT. 0) THEN
      IF (SIZE(AppStream%State) .EQ. 0) THEN
        MessageArray(1) = 'For proper simulation of streams, relevant stream data files must'
        MessageArray(2) = 'be specified when stream nodes are defined in Pre-Processor.'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
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
  ! --- GET SUB-COMPONENTS OF A DATA TYPE FOR POST-PROCESSING AT A LOCATION TYPE
  ! --- (REDEFINES THE PROCEDURE IN Class_BaseAppStream)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_GetSubDataList_AtLocation(AppStream,iLocationType,iLocationID,cDataType,cSubDataList) 
    CLASS(AppStream_v50_Type),INTENT(IN)     :: AppStream
    INTEGER,INTENT(IN)                       :: iLocationType,iLocationID
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_StrmReach)
            !Only stream reach budget has sub-data
            IF (TRIM(cDataType) .EQ. cDataList_AtStrmReach) THEN
                IF (AppStream%StrmReachBudRawFile_Defined) THEN
                    ALLOCATE (cSubDataList(NStrmBudColumns))
                    cSubDataList = cBudgetColumnTitles
                END IF
            END IF
            
            
        CASE (iLocationType_StrmNode)
            !Only stream node budget has sub-data
            IF (TRIM(cDataType) .EQ. cDataList_AtStrmNode) THEN
                IF (AppStream%StrmNodeBudget%StrmNodeBudRawFile_Defined) THEN
                    IF (AppStream%StrmHyd%IsHydrographAtNodeRequested(iLocationID)) THEN
                        ALLOCATE (cSubDataList(NStrmBudColumns))
                        cSubDataList = cBudgetColumnTitles
                    END IF
                END IF
            END IF
            
    END SELECT
    
  END SUBROUTINE AppStream_v50_GetSubDataList_AtLocation
  
  
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

    ALLOCATE (UpstrmNodes(AppStream%Nodes(iNode)%NUpstrmNodes))
    UpstrmNodes = AppStream%Nodes(iNode)%UpstrmNodes
    
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
        CALL SetLastMessage('Error allocating memory for stream data!',iFatal,ThisProcedure)
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
  ! --- READ APPLICATION STREAMS RELATED DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_ReadTSData(AppStream,lDiverAdjusted,TimeStep,iStat,DiversionsOverwrite)
    CLASS(AppStream_v50_Type)     :: AppStream
    LOGICAL,INTENT(IN)            :: lDiverAdjusted
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    REAL(8),OPTIONAL,INTENT(IN)   :: DiversionsOverwrite(:)
    
    CALL AppStream%StrmInflowData%ReadTSData(TimeStep,iStat)                 ;  IF (iStat .EQ. -1) RETURN
    IF (PRESENT(DiversionsOverwrite)) THEN
        CALL AppStream%AppDiverBypass%ReadTSData(lDiverAdjusted,TimeStep,iStat,DiversionsOverwrite)  
    ELSE
        CALL AppStream%AppDiverBypass%ReadTSData(lDiverAdjusted,TimeStep,iStat)  
    END IF
    IF (iStat .EQ. -1) RETURN
    
    !Update the diversions from each stream node
    CALL AppStream%AppDiverBypass%CompileNodalDiversions()
    
  END SUBROUTINE AppStream_v50_ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- READ STREAM REACH CONFIGURATION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadStreamConfigData(DataFile,Stratigraphy,StrmGWConnector,StrmLakeConnector,AppStream,iStat)
    TYPE(GenericFileType)                 :: DataFile
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(AppStream_v50_Type)              :: AppStream
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadStreamConfigData'
    INTEGER                      :: indxReach,DummyIntArray(4),ID,indxNode,DummyIntArray1(2), &
                                    DestNode,DestReach,NNodes,iGWNodes(AppStream%NStrmNodes)
    CHARACTER                    :: ALine*2000
    
    !Initialize
    iStat    = 0
    iGWNodes = 0
    
    !Iterate over reaches
    DO indxReach=1,AppStream%NReaches
        ASSOCIATE (pReach => AppStream%Reaches(indxReach))
            CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL GetArrayData(ALine,DummyIntArray,'stream reach '//TRIM(IntToText(indxReach)),iStat)  ;  IF (iStat .EQ. -1) RETURN
            pReach%cName = ALine(1:20)
            
            !Make sure reach data is entered sequentially
            ID = DummyIntArray(1)
            IF (ID .NE. indxReach) THEN
                MessageArray(1)='Stream reaches should be entered sequentially!'
                MessageArray(2)='Reach number expected = '//TRIM(IntToText(indxReach))
                MessageArray(3)='Reach number entered  = '//TRIM(IntToText(ID))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Store data in persistent arrays
            pReach%UpstrmNode   = DummyIntArray(2)
            pReach%DownstrmNode = DummyIntArray(3)
            IF (DummyIntArray(4) .GT. 0) THEN
                pReach%OutflowDest  = DummyIntArray(4)
            ELSEIF (DummyIntArray(4) .EQ. 0) THEN
                pReach%OutflowDestType = FlowDest_Outside
                pReach%OutflowDest     = 0
            ELSE
                pReach%OutflowDestType = FlowDest_Lake
                pReach%OutflowDest     = -DummyIntArray(4)
                CALL StrmLakeConnector%AddData(iStrmToLakeType , pReach%DownstrmNode , pReach%OutflowDest)
            END IF
            
            !Make sure there are at least 2 stream nodes defined for the reach
            NNodes = pReach%DownstrmNode - pReach%UpstrmNode + 1
            IF (NNodes .LT. 2) THEN
                MessageArray(1) = 'There should be at least 2 stream nodes for each reach.'
                MessageArray(2) = 'Reach '//TRIM(IntToText(indxReach))//' has less than 2 stream nodes!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Read the reach nodes
            DO indxNode=pReach%UpstrmNode,pReach%DownstrmNode
                CALL DataFile%ReadData(DummyIntArray1,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Make sure the stream nodes are entered sequentially
                IF (DummyIntArray1(1) .NE. indxNode) THEN
                    MessageArray(1)='Stream nodes for reach '//TRIM(IntToText(indxReach))//' should be entered sequentially!'
                    MessageArray(2)='Stream node expected = '//TRIM(IntToText(indxNode))
                    MessageArray(3)='Stream node entered  = '//TRIM(IntToText(DummyIntArray1(1)))
                    CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure that stream node entered is not larger than the maximum node number
                IF (DummyIntArray1(1) .GT. AppStream%NStrmNodes) THEN
                    MessageArray(1) = 'Stream node number '//TRIM(IntToText(DummyIntArray1(1)))//' in reach '//TRIM(IntToText(indxReach))
                    MessageArray(2) = ' is larger than the maximum stream node number specified ('//TRIM(IntToText(AppStream%NStrmNodes))//')!'
                    CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure the stream node has not been entered before
                IF (iGWNodes(indxNode) .NE. 0) THEN
                    CALL SetLastMessage('Stream node '//TRIM(IntToText(indxNode))//' in reach '//TRIM(IntToText(indxReach))//'has already been used in another reach.',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Store gw node 
                iGWNodes(indxNode) = DummyIntArray1(2)
              
            END DO
          
        END ASSOCIATE
    END DO
    
    !Store GW nodes for each stream node in strm-gw connector database
    CALL StrmGWConnector%New(iVersion,iGWNodes,Stratigraphy%TopActiveLayer(iGWNodes),iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that reach numbers are set properly
    DO indxReach=1,AppStream%NReaches
        IF (AppStream%Reaches(indxReach)%OutFlowDestType .NE. FlowDest_StrmNode) CYCLE
        DestNode  = AppStream%Reaches(indxReach)%OutFlowDest
        DestReach = StrmReach_GetReachNumber(DestNode,AppStream%Reaches)
        IF (DestReach .EQ. 0) THEN
            CALL SetLastMessage('Outflow stream node '//TRIM(IntToText(DestNode))//' for reach '//TRIM(IntToText(indxReach))//' does not belong to any stream reaches!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (DestReach .LE. indxReach) THEN
            MessageArray(1) = 'Stream reach number should be less than the reach number into which it flows!'
            MessageArray(2) = ' Upstream reach   = '//TRIM(IntToText(indxReach))
            MessageArray(3) = ' Downstream reach = '//TRIM(IntToText(DestReach))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO
    
    !Compile upstream reaches for each reach
    CALL StrmReach_CompileUpstrmReaches(AppStream%Reaches)
    
    !Compile upstream nodes for each node
    CALL CompileUpstrmNodes(AppStream)
    
  END SUBROUTINE ReadStreamConfigData
  
  
  ! -------------------------------------------------------------
  ! --- READ CROSS SECTION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadCrossSectionData(Stratigraphy,StrmGWConnector,DataFile,AppStream,iStat)
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(GenericFileType)                :: DataFile
    TYPE(AppStream_v50_Type)             :: AppStream
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadCrossSectionData'
    INTEGER                      :: indxNode,iStrmNode,iGWNode,iLayer,ErrorCode
    REAL(8)                      :: FACTN,FACTLT,AquiferBottomElev,DummyArray(6)
    INTEGER,ALLOCATABLE          :: iGWNodes(:)
    
    !Initialize
    iStat = 0
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
            
            !Make sure stream nodes are entered sequentially
            iStrmNode = INT(DummyArray(1))
            IF (iStrmNode .NE. indxNode) THEN
                MessageArray(1) = 'Rating tables for stream nodes should be entered sequentailly!'
                MessageArray(2) = 'Stream node expected = ' // TRIM(IntToText(indxNode))
                MessageArray(3) = 'Stream node entered  = ' // TRIM(IntToText(iStrmNode))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Stream bottom elevation
            pNodes(indxNode)%BottomElev = DummyArray(2) * FACTLT
            AquiferBottomElev           = Stratigraphy%BottomElev(iGWNode,iLayer)
            IF (pNodes(indxNode)%BottomElev .LT. AquiferBottomElev) THEN
                MessageArray(1) = 'Aquifer bottom elevation at a stream node should be'
                MessageArray(2) = 'less than or equal to the stream bed elevation!'
                WRITE (MessageArray(3),'(A,F10.2)') ' Stream node = '//TRIM(IntToText(indxNode)) //'   Stream bed elevation    = ',pNodes(indxNode)%BottomElev
                WRITE (MessageArray(4),'(A,F10.2)') ' GW node     = '//TRIM(IntToText(iGWNode))  //'   Aquifer bottom elevation= ',AquiferBottomElev
                CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Maximum elevation
            IF (DummyArray(6) .LT. 0.0) THEN
                CALL SetLastMessage('Maximum flow depth at stream node '//TRIM(IntToText(indxNode))//' cannot be less than zero!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            pNodes(indxNode)%MaxElev = pNodes(indxNode)%BottomElev + DummyArray(6) * FACTLT 
            
            !Cross section data
            pNodes(indxNode)%CrossSection%B0 = DummyArray(3) * FACTLT
            pNodes(indxNode)%CrossSection%s  = DummyArray(4) * FACTLT
            pNodes(indxNode)%CrossSection%n  = DummyArray(5) * FACTN
            
            !Make sure that cross section data is specified properly
            IF (pNodes(indxNode)%CrossSection%B0 .EQ. 0.0   .AND.   pNodes(indxNode)%CrossSection%s .EQ. 0.0) THEN
                CALL SetLastMessage('B0 and s at stream node '//TRIM(IntToText(indxNode))//' cannot be both zero!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        
        END DO
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
        IF (AppStream%FinalFlowFile%iGetFileType() .NE. UNKNOWN) CALL PrintFinalFlows(AppStream%State%Flow,TimeStep,AppStream%FinalFlowFile)
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
  SUBROUTINE AppStream_v50_WriteDataToTextFile(AppStream,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
    CLASS(AppStream_v50_Type),INTENT(IN)  :: AppStream
    CHARACTER(LEN=*),INTENT(IN)          :: UNITLTOU
    REAL(8),INTENT(IN)                   :: FACTLTOU
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    INTEGER             :: indxReach,indxNode,iGWNode,iLayer,ErrorCode
    REAL(8)             :: GSElev,AquiferBottom,StrmBottom,DELZ,DELA
    INTEGER,ALLOCATABLE :: UpstrmNodes(:),iGWNodes(:)
    CHARACTER           :: ALine*1000
    
    !Initialize
    iStat = 0
    
    !If there are no streams, write relevant information and return
    IF (AppStream%NStrmNodes .EQ. 0) THEN
      CALL LogMessage('***** THERE ARE NO STREAM NODES *****',iMessage,'',FILE) 
      RETURN
    END IF
    
    !Initialize
    CALL StrmGWConnector%GetAllGWNodes(iGWNodes)
    
    !Write titles
    CALL LogMessage(' REACH STREAM GRID     GROUND   INVERT             AQUIFER   ALLUVIAL    UPSTREAM',iMessage,'',FILE)
    CALL LogMessage('   NO.   NO.   NO.     ELEV.     ELEV.     DEPTH    BOTTOM  THICKNESS      NODES',iMessage,'',FILE)
    CALL LogMessage('                           (ALL UNITS ARE IN '//TRIM(UNITLTOU)//')',iMessage,'',FILE)
    
    !Write stream reach data
    DO indxReach=1,AppStream%NReaches
      DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
        iGWNode       = iGWNodes(indxNode)
        iLayer        = Stratigraphy%TopActiveLayer(iGWNode)
        GSElev        = Stratigraphy%GSElev(iGWNode)
        AquiferBottom = Stratigraphy%BottomElev(iGWNode,iLayer)
        StrmBottom    = AppStream%Nodes(indxNode)%BottomElev
        DELZ          = GSElev - StrmBottom
        DELA          = StrmBottom - AquiferBottom
        CALL AppStream_v50_GetUpstrmNodes(AppStream,indxNode,UpstrmNodes)
        WRITE (ALine,'(1X,3I6,5F10.1,5X,10(I4,1X))') indxReach                                 , &
                                                     indxNode                                  , &
                                                     iGWNode                                   , &
                                                     GSElev*FACTLTOU                           , &
                                                     StrmBottom*FACTLTOU                       , &
                                                     DELZ*FACTLTOU                             , &
                                                     AquiferBottom*FACTLTOU                    , &
                                                     DELA*FACTLTOU                             , &
                                                     UpstrmNodes
        CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
      END DO
      CALL LogMessage('',iMessage,'',FILE)
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
    REAL(8)                                               :: DummyArray(NStrmBudColumns,AppStream%StrmNodeBudget%NBudNodes)
    REAL(8),DIMENSION(AppStream%StrmNodeBudget%NBudNodes) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,                 &
                                                             Runoff,ReturnFlows,StrmGWFlows,LakeInflows,Error,                      &
                                                             Diversions,Bypasses,DiversionShorts,RiparianET,StorChange
    INTEGER,ALLOCATABLE                                   :: UpstrmNodes(:)
    LOGICAL                                               :: lUpstrmNode
    
    ASSOCIATE (pNodes => AppStream%Nodes  , &
               pState => AppStream%State  )
        
        !Iterate over nodes
        DO indxNode=1,AppStream%StrmNodeBudget%NBudNodes
          iNode = AppStream%StrmNodeBudget%iBudNodes(indxNode)
      
          !Is this the most upstrem node for the reach it is in?
          IF (pNodes(iNode)%DeltaX .EQ. 0.0) THEN
              lUpstrmNode = .TRUE.
          ELSE
              lUpstrmNode = .FALSE.
          END IF

          !Upstream flows
          !IF (lUpstrmNode) THEN  !Upstream node for reach
              CALL AppStream%GetUpstrmNodes(iNode,UpstrmNodes)
              !UpstrmFlows(indxNode) = 0.0
              UpstrmFlows(indxNode) = SUM(pState(UpStrmNodes)%Flow)
          !ELSE
          !    UpstrmFlows(indxNode) = pState(iNode-1)%Flow
          !END IF
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
      
          !Stream-gw interaction
          !(+: flow from stream to groundwater)
          IF (lUpstrmNode) THEN
              StrmGWFlows(indxNode) = 0.0
          ELSE
              StrmGWFlows(indxNode) = (- (StrmGWConnector%GetFlowAtSomeStrmNodes(iNode,iNode) / pNodes(iNode)%Length)                                        &
                                       - (StrmGWConnector%GetFlowAtSomeStrmNodes(iNode-1,iNode-1) / pNodes(iNode-1)%Length) ) * pNodes(iNode)%DeltaX * 0.5D0
          END IF
    
          !Inflow from lakes
          LakeInflows(indxNode) = StrmLakeConnector%GetFlow(iLakeToStrmType,iNode)
      
          !Riparian ET
          RiparianET(indxNode) = QRVET(iNode)
      
        END DO
    END ASSOCIATE
    
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetNodeDiversions(AppStream%StrmNodeBudget%iBudNodes)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetNodeNetBypass(AppStream%StrmNodeBudget%iBudNodes)
    
    !Error
    Error =  UpstrmFlows    &
           - DownstrmFlows  &
           - StorChange     &
           + TributaryFlows &
           + DrainInflows   &
           + Runoff         &
           + ReturnFlows    &
           + StrmGWFlows    &
           + LakeInflows    &
           - RiparianET     &
           - Diversions     &
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
    DummyArray(8,:)  = StrmGWFlows
    DummyArray(9,:)  = LakeInflows
    DummyArray(10,:) = RiparianET
    DummyArray(11,:) = Diversions
    DummyArray(12,:) = Bypasses
    DummyArray(13,:) = Error
    DummyArray(14,:) = DiversionShorts
    
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
    INTEGER                               :: indxReach,indxReach1,iNode,iUpstrmReach,iUpstrmNode,     &
                                             iDownstrmNode,indx
    REAL(8)                               :: DummyArray(NStrmBudColumns,AppStream%NReaches) 
    REAL(8),DIMENSION(AppStream%NReaches) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,   &
                                             Runoff,ReturnFlows,StrmGWFlows,LakeInflows,Error,        &
                                             Diversions,Bypasses,DiversionShorts,RiparianET,StorChange
    
    !Initialize           
    UpstrmFlows = 0.0
    
    !Iterate over reaches
    DO indxReach=1,AppStream%NReaches
      iUpstrmNode   = AppStream%Reaches(indxReach)%UpstrmNode
      iDownstrmNode = AppStream%Reaches(indxReach)%DownstrmNode
      !Upstream flows
      DO indxReach1=1,AppStream%Reaches(indxReach)%NUpstrmReaches
        iUpstrmReach           = AppStream%Reaches(indxReach)%UpstrmReaches(indxReach1)
        iNode                  = AppStream%Reaches(iUpstrmReach)%DownstrmNode
        UpstrmFlows(indxReach) = UpstrmFlows(indxReach) + AppStream%State(iNode)%Flow
      END DO
      IF (AppStream%StrmInflowData%lDefined) UpstrmFlows(indxReach) = UpstrmFlows(indxReach) + SUM(AppStream%StrmInflowData%Inflows(iUpstrmNode:iDownstrmNode))
    
      !Change in storage
      StorChange(indxReach) = SUM(AppStream%StorChange(iUpstrmNode:iDownstrmNode))
      
      !Tributary flows
      TributaryFlows(indxReach) = SUM(QTRIB(iUpstrmNode:iDownstrmNode))
      
      !Inflows from tile drains
      DrainInflows(indxReach) = SUM(QDRAIN(iUpstrmNode:iDownstrmNode))
      
      !Runoff
      Runoff(indxReach) = SUM(QROFF(iUpstrmNode:iDownstrmNode))

      !Return flow
      ReturnFlows(indxReach) = SUM(QRTRN(iUpstrmNode:iDownstrmNode))
      
      !Stream-gw interaction
      !(+: flow from stream to groundwater)
      StrmGWFlows(indxReach) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iUpstrmNode,iDownstrmNode)
    
      !Inflow from lakes
      LakeInflows(indxReach) = 0.0
      DO indx=iUpstrmNode,iDownstrmNode
        LakeInflows(indxReach) = LakeInflows(indxReach) + StrmLakeConnector%GetFlow(iLakeToStrmType,indx)
      END DO
      
      !Riparian ET
      RiparianET(indxReach) = SUM(QRVET(iUpstrmNode:iDownstrmNode))
      
    END DO
    
    !Downstream flows
    DownstrmFlows = AppStream%State(AppStream%Reaches%DownStrmNode)%Flow
      
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetReachDiversions(AppStream%NReaches,AppStream%Reaches)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetReachNetBypass(AppStream%NReaches,AppStream%Reaches)
    
    !Error
    Error =  UpstrmFlows    &
           - DownstrmFlows  &
           - StorChange     &
           + TributaryFlows &
           + DrainInflows   &
           + Runoff         &
           + ReturnFlows    &
           + StrmGWFlows    &
           + LakeInflows    &
           - RiparianET     &
           - Diversions     &
           - Bypasses
           
    !Diversion shortages
    DiversionShorts = AppStream%AppDiverBypass%GetReachDiversionShort(AppStream%NReaches,AppStream%Reaches)
           
    !Compile data in array
    DummyArray(1,:)  = UpstrmFlows
    DummyArray(2,:)  = DownstrmFlows
    DummyArray(3,:)  = StorChange
    DummyArray(4,:)  = TributaryFlows
    DummyArray(5,:)  = DrainInflows
    DummyArray(6,:)  = Runoff
    DummyArray(7,:)  = ReturnFlows
    DummyArray(8,:)  = StrmGWFlows
    DummyArray(9,:)  = LakeInflows
    DummyArray(10,:) = RiparianET
    DummyArray(11,:) = Diversions
    DummyArray(12,:) = Bypasses
    DummyArray(13,:) = Error
    DummyArray(14,:) = DiversionShorts
    
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
    INTEGER                                 :: indxNode,indxReach,NBypass,ErrorCode,iNodeIDs(1),iUpstrmNode,iDownstrmNode,&
                                               indxUpstrmNode,iUpNode,NNodes
    REAL(8)                                 :: rInflow,rOutflow,Bypass_Recieved,dBypass_dFlow,rUpdateValues(1),Coeff,     &
                                               dFlow_dStage,rNetInflow,dArea_dStage,DeltaT,dFlow_dStage_Prev,rFlow,Area,  &
                                               rBypassFlow,rCheckGradient,RHSMin,Area_Prev,dArea_dStage_Prev
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: BCInflows,rUpdateRHS,StrmGWFlow_AtMinHead,HRG
    INTEGER,ALLOCATABLE                     :: iStrmIDs(:),iLakeIDs(:)
    INTEGER,PARAMETER                       :: iCompIDs(1) = iStrmComp
    
    !Inform user about simulation progress
    CALL EchoProgress('Simulating stream flows')
    
    !Initialize
    NNodes = SIZE(GWHEads, DIM=1)
    
    !Get groundwater heads at stream nodes
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
    
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate(NNodes,HRG,AppStream%State%Head,AppStream%Nodes%BottomElev,AppStream%Nodes,Matrix,AppStream%Nodes%DeltaX,AppStream%Nodes%MaxElev)
    
    !Stream-gw interaction at minimum stream head (= stream bottom elevation)
    CALL StrmGWConnector%ComputeStrmGWFlow_AtMinHead(StrmGWFlow_AtMinHead,HRG,AppStream%State%Head,AppStream%Nodes%BottomElev,AppStream%Nodes,AppStream%Nodes%DeltaX)
  
    !Initialize
    DeltaT    = AppStream%DeltaT
    NBypass   = AppStream%AppDiverBypass%NBypass
    BCInflows = AppStream%StrmInflowData%GetInflows(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Initialize bypass flows to zero
    AppStream%AppDiverBypass%Bypasses%Bypass_Out      = 0.0
    AppStream%AppDiverBypass%Bypasses%Bypass_Recieved = 0.0
    
    !Update the matrix equation
    ASSOCIATE (pReaches     => AppStream%Reaches          , &
               pNodes       => AppStream%Nodes            , &
               pStorChange  => AppStream%StorChange       , &
               pHeads       => AppStream%State%Head       , &
               pHeads_P     => AppStream%State%Head_P     , &
               pFlows       => AppStream%State%Flow       , &
               pArea_P      => AppStream%Nodes%Area_P     , &
               pDeltaX      => AppStream%Nodes%DeltaX     , &
               pBottomElev  => AppStream%Nodes%BottomElev , &
               pDiverBypass => AppStream%AppDiverBypass   )
        DO indxReach=1,AppStream%NReaches
            iUpstrmNode   = pReaches(indxReach)%UpstrmNode
            iDownstrmNode = pReaches(indxReach)%DownstrmNode
            DO indxNode=iUpstrmNode,iDownstrmNode
                
                !Compute derivates based on rating tables
                dArea_dStage     = pNodes(indxNode)%dArea(pHeads(indxNode))
                dFlow_dStage     = pNodes(indxNode)%dFlow(pHeads(indxNode))
                pFlows(indxNode) = pNodes(indxNode)%Flow(pHeads(indxNode))
                Area             = pNodes(indxNode)%Area(pHeads(indxNode))
                
                !Initialize
                rInflow  = 0.0
                rOutflow = 0.0
                
                !Coefficient to be used several times
                Coeff = 0.5d0 * pDeltaX(indxNode) / DeltaT
                
                !Received bypass
                Bypass_Recieved = pDiverBypass%GetBypassRecieved(FlowDest_StrmNode,indxNode)
                
                !Inflows at the stream node with known values
                rInflow = BCInflows(indxNode)                                   &    !Inflow as defined by the user
                        + Runoff(indxNode)                                      &    !Direct runoff of precipitation 
                        + ReturnFlow(indxNode)                                  &    !Return flow of applied water 
                        + TributaryFlow(indxNode)                               &    !Tributary inflows from small watersheds and creeks
                        + DrainInflows(indxNode)                                &    !Inflow from tile drains
                        + Bypass_Recieved                                       &    !Received by-pass flows 
                        + StrmLakeConnector%GetFlow(iLakeToStrmType,indxNode)        !Flows from lake outflow
                
                !Inflows from upstream nodes
                IF (indxNode .EQ. iUpstrmNode) THEN
                    DO indxUpstrmNode=1,pNodes(indxNode)%NUpstrmNodes
                        iUpNode = pNodes(indxNode)%UpstrmNodes(indxUpstrmNode)
                        rInflow = rInflow + pFlows(iUpNode)
                    END DO       
                ELSE
                    DO indxUpstrmNode=1,pNodes(indxNode)%NUpstrmNodes
                        iUpNode = pNodes(indxNode)%UpstrmNodes(indxUpstrmNode)
                        IF (iUpNode .EQ. indxNode-1) CYCLE
                        rInflow = rInflow + pFlows(iUpNode)
                    END DO 
                END IF
                
                !Diversion
                IF (pDiverBypass%NDiver .GT. 0) THEN
                  rOutFlow                                = MIN(pFlows(indxNode) , pDiverBypass%NodalDiverRequired(indxNode))
                  pDiverBypass%NodalDiverActual(indxNode) = rOutflow
                END IF
                
                !Riparian ET outflow
                IF (RiparianET(indxNode) .GT. 0.0) THEN
                    RiparianETFrac(indxNode) = MIN(RiparianET(indxNode) , pFlows(indxNode)-rOutflow) / RiparianET(indxNode)
                    rOutflow                 = rOutflow + RiparianET(indxNode) * RiparianETFrac(indxNode)
                ELSE
                    RiparianETFrac(indxNode) = 0.0
                END IF
                
                !Bypass
                IF (pDiverBypass%IsBypassNode(indxNode)) THEN
                    rBypassFlow = 0.0
                    IF (indxNode .EQ. iUpstrmNode) THEN
                        rFlow = MAX(0.0 , rInflow - rOutflow) 
                    ELSE
                        rFlow =  pFlows(indxNode-1)                                                                                                    &
                               - Coeff * (Area_Prev - pArea_P(indxNode-1))                                                                             &
                               + rInflow - rOutflow                                                                                                    &
                               - 0.5D0 * pDeltaX(indxNode) * StrmGWConnector%GetFlowAtSomeStrmNodes(indxNode-1,indxNode-1) / pNodes(indxNode-1)%Length 
                        rFlow = MAX(0.0 , rFlow)
                    END IF
                    CALL pDiverBypass%ComputeBypass(indxNode,rFlow,rBypassFlow,StrmLakeConnector,.TRUE.,dBypass_dFlow)
                    rOutflow = rOutflow + rBypassFlow
                    IF (.NOT. pDiverBypass%IsRatingTableTypeBypassNode(indxNode)) dBypass_dFlow = 0.0 
                ELSE
                    dBypass_dFlow = 0.0
                END IF
                
                !Net inflow
                rNetInflow = rInflow - rOutflow
                
                !Compute the matrix rhs function and its derivatives w.r.t. stream elevation
                !----------------------------------------------------------------------------
                
                !First node of each reach is treated as boundary node
                IF (indxNode .EQ. iUpstrmNode) THEN
                    rUpdateRHS(indxNode) = pFlows(indxNode) - rNetInflow
                    !Make sure we don't overshoot the maximum stream stage
                    IF (pHeads(indxNode) .LT. pNodes(indxNode)%MaxElev) THEN
                        rCheckGradient = -rUpdateRHS(indxNode) / (pNodes(indxNode)%MaxElev - pHeads(indxNode))
                        IF (rCheckGradient .GT. dFlow_dStage) THEN
                            dFlow_dStage = rCheckGradient
                        END IF
                    END IF
                    dFlow_dStage_Prev = dFlow_dStage
                    dArea_dStage_Prev = dArea_dStage
                    Area_Prev         = Area
                    iNodeIDs(1)       = indxNode
                    rUpdateValues(1)  = dFlow_dStage
                    CALL Matrix%UpdateCOEFF(iStrmComp,indxNode,iCompIDs,iNodeIDs,rUpdateValues) 
                    CYCLE
                END IF

                !RHS function at minimum stream head
                RHSMin = Coeff * (Area_Prev-pArea_P(indxNode)-pArea_P(indxNode-1)) - pFlows(indxNode-1) - rInflow + StrmGWFlow_AtMinHead(indxNode)
                RHSMin = MAX(RHSMin , 0.0)
                
                !Rate of change in storage
                pStorChange(indxNode) = Coeff * (Area-pArea_P(indxNode)+Area_Prev-pArea_P(indxNode-1)) - RHSMin
                
                !RHS function
                rUpdateRHS(indxNode) = pStorChange(indxNode) + pFlows(indxNode) - pFlows(indxNode-1) - rNetInflow
                
                !Derivative of function w.r.t. stream elevation
                iNodeIDs(1)      = indxNode
                rUpdateValues(1) = Coeff * dArea_dStage + dFlow_dStage
                CALL Matrix%UpdateCOEFF(iStrmComp,indxNode,iCompIDs,iNodeIDs,rUpdateValues)
                
                !Derivate of function w.r.t. stream elevation in the previous node
                iNodeIDs(1)      = indxNode - 1
                rUpdateValues(1) = Coeff * (1D0 - dBypass_dFlow) * dArea_dStage_Prev + (dBypass_dFlow - 1D0) * dFlow_dStage_Prev 
                CALL Matrix%UpdateCOEFF(iStrmComp,indxNode,iCompIDs,iNodeIDs,rUpdateValues)
                
                !Update matrix for the effect of strm-gw interaction at rating table type bypass nodes
                IF (pDiverBypass%IsRatingTableTypeBypassNode(indxNode)) CALL StrmGWConnector%UpdateMatrix_ForBypass(indxNode,pHeads,HRG,pDeltaX,pBottomElev,pNodes%MaxElev,dBypass_dFlow,pNodes,Matrix)
                
                !Shift relevant values from this node to previous node
                dFlow_dStage_Prev = dFlow_dStage
                dArea_dStage_Prev = dArea_dStage
                Area_Prev         = Area
                
            END DO
        END DO
        
        !Update RHS vector
        CALL Matrix%UpdateRHS(iStrmComp,1,rUpdateRHS)
        
        !Stream flows to lakes
        CALL StrmLakeConnector%GetSourceIDs(iStrmToLakeType,iStrmIDs)
        CALL StrmLakeConnector%GetDestinationIDs(iStrmToLakeType,iLakeIDs)
        DO indxNode=1,SIZE(iStrmIDs)
          CALL StrmLakeConnector%SetFlow(iStrmToLakeType,iStrmIDs(indxNode),iLakeIDs(indxNode),pFlows(iStrmIDs(indxNode)))
        END DO
        
    END ASSOCIATE
    
    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
    
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
                    IF (AppStream%Reaches(indxReach1)%OutflowDestType .NE. FlowDest_StrmNode) CYCLE
                    IF (AppStream%Reaches(indxReach1)%OutflowDest .EQ. indxNode) THEN
                        iCount            = iCount + 1
                        TempNodes(iCount) = AppStream%Reaches(indxReach1)%DownstrmNode
                    END IF
                END DO
                ALLOCATE (AppStream%Nodes(indxNode)%UpstrmNodes(iCount))
                AppStream%Nodes(indxNode)%NUpstrmNodes = iCount
                AppStream%Nodes(indxNode)%UpstrmNodes  = TempNodes(1:iCount)
            
            !indxNode is not the first node in the reach
            ELSE
                iCount       = 1
                TempNodes(1) = indxNode - 1
                DO indxReach1=1,indxReach-1
                    IF (AppStream%Reaches(indxReach1)%OutflowDestType .NE. FlowDest_StrmNode) CYCLE
                    IF (AppStream%Reaches(indxReach1)%OutflowDest .EQ. indxNode) THEN
                        iCount            = iCount + 1
                        TempNodes(iCount) = AppStream%Reaches(indxReach1)%DownstrmNode
                    END IF
                END DO
                ALLOCATE (AppStream%Nodes(indxNode)%UpstrmNodes(iCount))
                AppStream%Nodes(indxNode)%NUpstrmNodes = iCount
                AppStream%Nodes(indxNode)%UpstrmNodes  = TempNodes(1:iCount)
            END IF
            
            !Order the upstream nodes
            CALL ShellSort(AppStream%Nodes(indxNode)%UpstrmNodes)
            
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
                
                !Distance to the previous node
                pNodes(indxNode)%DeltaX = B_Distance
                                
                !Distance to the next node node
                CA         = AppGrid%Node(iGWDownstrmNode)%X - AppGrid%Node(iGWNode)%X
                CB         = AppGrid%Node(iGWDownstrmNode)%Y - AppGrid%Node(iGWNode)%Y
                F_Distance = SQRT(CA*CA + CB*CB)
                
                !Stream segment length
                pNodes(indxNode)%Length = (F_Distance + B_Distance) / 2d0
                
                !Slope
                IF (indxNode .GT. iUpstrmNode) THEN
                    pNodes(indxNode)%Slope = (pNodes(indxNode-1)%BottomElev - pNodes(indxNode)%BottomElev) / B_Distance
                    IF (pNodes(indxNode)%Slope .LE. 0.0) THEN
                        CALL SetLastMessage('Slope at stream node '//TRIM(IntToText(indxNode))//' must be greater than zero!',iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
                
                !Advance variables
                iGwNode    = iGWDownstrmNode
                B_Distance = F_Distance 
                
            END DO
            pNodes(iDownstrmNode)%DeltaX = B_Distance
            pNodes(iDownstrmNode)%Length = B_Distance / 2d0
            
            !Slope at the first and last node of reach
            pNodes(iDownstrmNode)%Slope = (pNodes(iDownstrmNode-1)%BottomElev - pNodes(iDownstrmNode)%BottomElev) / B_Distance
            pNodes(iUpstrmNode)%Slope   = pNodes(iUpstrmNode+1)%Slope
            IF (pNodes(iUpstrmNode)%Slope .LE. 0.0) THEN
                CALL SetLastMessage('Slope at stream node '//TRIM(IntToText(iUpstrmNode))//' must be greater than zero!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (pNodes(iDownstrmNode)%Slope .LE. 0.0) THEN
                CALL SetLastMessage('Slope at stream node '//TRIM(IntToText(iDownstrmNode))//' must be greater than zero!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END ASSOCIATE
    
  END SUBROUTINE CompileDistanceLengthSlope
  
  
  ! -------------------------------------------------------------
  ! --- OVER-WRITE THE METHOD TO REGISTER STREAM COMPONENT WITH MATRIX AND TO ADD CONNECTIVITY
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v50_RegisterWithMatrix(AppStream,Matrix,iStat)
    CLASS(AppStream_v50_Type),INTENT(IN) :: AppStream
    TYPE(MatrixType)                     :: Matrix
    INTEGER,INTENT(OUT)                  :: iStat
     
    !Local variables
    INTEGER             :: indxNode,iDim,indxReach,iUpstrmNOde,iDownstrmNode,ConnectedNodes(50),indx
    INTEGER,ALLOCATABLE :: Temp_ConnectedNodes(:)
     
    !Add component to matrix
    CALL Matrix%AddComponent(iStrmComp,AppStream%NStrmNodes,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Add connectivity list to Matrix
    DO indxReach=1,AppStream%NReaches
        iUpstrmNode   = AppStream%Reaches(indxReach)%UpstrmNode
        iDownstrmNode = AppStream%Reaches(indxReach)%DownstrmNode
        
        DO indxNode=iUpstrmNode,iDownstrmNode
            ConnectedNodes(1) = indxNode
            IF (indxNode .EQ. iUpstrmNode) THEN
                CALL AppStream%GetUpstrmNodes(indxNode,Temp_ConnectedNodes)
                iDim                   = SIZE(Temp_ConnectedNodes)
                indx                   = iDim+1
                ConnectedNodes(2:indx) = Temp_ConnectedNodes                
            ELSE
                ConnectedNodes(2) = indxNode - 1
                indx              = 2
            END IF
                
            CALL Matrix%AddConnectivity(iStrmComp,indxNode,iStrmComp,ConnectedNodes(1:indx),iStat)
            IF (iStat .EQ. -1) RETURN
    
        END DO
    END DO
     
  END SUBROUTINE AppStream_v50_RegisterWithMatrix
  
  
  ! -------------------------------------------------------------
  ! --- FUNCTION TO PREPARE THE BUDGET HEADER DATA FOR STREAM BUDGETS
  ! --- (REDEFINES THE PROCEDURE IN Class_BaseAppStream WITH THE SAME NAME)
  ! -------------------------------------------------------------
  FUNCTION PrepareStreamBudgetHeader(NLocations,NTIME,TimeStep,cVersion,iBudNodes) RESULT(Header)
    INTEGER,INTENT(IN)            :: NLocations,NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    INTEGER,OPTIONAL,INTENT(IN)   :: iBudNodes(:)
    TYPE(BudgetHeaderType)        :: Header
    
    !Local variables
    INTEGER,PARAMETER           :: TitleLen           = 199  , &
                                   NTitles            = 3    , &
                                   NColumnHeaderLines = 4    
    INTEGER                     :: iCount,indxLocation,indxCol,indx,I
    TYPE(TimeStepType)          :: TimeStepLocal
    CHARACTER                   :: UnitT*10,TextTime*17
    LOGICAL                     :: lNodeBudOutput
    CHARACTER(LEN=16),PARAMETER :: FParts(NStrmBudColumns)=(/'UPSTRM_INFLOW'   ,&
                                                             'DOWNSTRM_OUTFLOW',&
                                                             'STORAGE_CHANGE'  ,&
                                                             'TRIB_INFLOW'     ,& 
                                                             'TILE_DRN'        ,& 
                                                             'RUNOFF'          ,& 
                                                             'RETURN_FLOW'     ,& 
                                                             'GAIN_FROM_GW'    ,& 
                                                             'GAIN_FROM_LAKE'  ,& 
                                                             'RIPARIAN_ET'     ,&
                                                             'DIVERSION'       ,& 
                                                             'BYPASS'          ,& 
                                                             'DISCREPANCY'     ,& 
                                                             'DIVER_SHORTAGE'  /)
                                                             
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
        pASCIIOutput%cTitles(2)         = ArrangeText('STREAM FLOW BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
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
        Header%cLocationNames(indx) = 'NODE '//TRIM(IntToText(iBudNodes(indx))) 
      END DO
    ELSE
      DO indx=1,NLocations
        Header%cLocationNames(indx) = 'REACH '//TRIM(IntToText(indx)) 
      END DO
    END IF
    
    !Locations
    ALLOCATE (Header%Locations(1)                                                          , &
              Header%Locations(1)%cFullColumnHeaders(NStrmBudColumns+1)                    , &
              Header%Locations(1)%iDataColumnTypes(NStrmBudColumns)                        , &
              Header%Locations(1)%iColWidth(NStrmBudColumns+1)                             , &
              Header%Locations(1)%cColumnHeaders(NStrmBudColumns+1,NColumnHeaderLines)     , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns           = NStrmBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                       
      pLocation%cFullColumnHeaders(2:) = cBudgetColumnTitles                         
      pLocation%iDataColumnTypes       = [VR ,&  !Upstream inflow
                                          VR ,&  !Downstream outflow
                                          VR ,&  !Change in storage
                                          VR ,&  !Tributary inflow
                                          VR ,&  !Tile drain
                                          VR ,&  !Runoff
                                          VR ,&  !Return flow
                                          VR ,&  !Gain from GW
                                          VR ,&  !Gain from lake
                                          VR ,&  !Riparian ET
                                          VR ,&  !Diversion
                                          VR ,&  !By-pass flow
                                          VR ,&  !Discrepancy
                                          VR ]   !Diversion shortage
      pLocation%iColWidth              = [17,(13,I=1,NStrmBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        TextTime            = ArrangeText(TRIM(UnitT),17)
        pColumnHeaders(:,1) = (/'                 ','     Upstream','   Downstream','   Change in ','    Tributary','        Tile ','             ','       Return','   Gain from ','    Gain from','   Riparian ','             ','      By-pass','             ','    Diversion'/)
        pColumnHeaders(:,2) = (/'      Time       ','      Inflow ','    Outflow  ','    Storage  ','     Inflow  ','        Drain','       Runoff','        Flow ','  Groundwater','      Lake   ','      ET    ','    Diversion','        Flow ','  Discrepancy','    Shortage '/)
        pColumnHeaders(:,3) = (/           TextTime,'       (+)   ','      (-)    ','      (-)    ','      (+)    ','         (+) ','        (+)  ','        (+)  ','      (+)    ','       (+)   ','      (-)   ','       (-)   ','        (-)  ','      (=)    ','             '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,14A13)'
        pFormatSpecs(2)     = '(A17,14A13)'
        pFormatSpecs(3)     = '(A17,14A13)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NStrmBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
                                                   
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NStrmBudColumns*(Header%NLocations)) , pDSSOutput%iDataTypes(1))
      iCount = 1
      IF (lNodeBudOutput) THEN
        DO indxLocation=1,Header%NLocations
          DO indxCol=1,NStrmBudColumns
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
          DO indxCol=1,NStrmBudColumns
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
      pDSSOutput%iDataTypes = PER_CUM
    END ASSOCIATE
    
  END FUNCTION PrepareStreamBudgetHeader
     
 
END MODULE