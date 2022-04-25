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
MODULE Class_AppStream_v40
  USE Class_Version                 , ONLY: ReadVersion
  USE Class_BaseAppStream           , ONLY: BaseAppStreamType               , &
                                            PrepareStreamBudgetHeader       , &
                                            CalculateNStrmNodes             , &
                                            ReadFractionsForGW
  USE MessageLogger                 , ONLY: SetLastMessage                  , &
                                            LogMessage                      , &
                                            EchoProgress                    , &
                                            MessageArray                    , &
                                            f_iFILE                         , &
                                            f_iFatal                        , &
                                            f_iWarn                         , &
                                            f_iMessage                        
  USE GeneralUtilities              , ONLY: StripTextUntilCharacter         , &
                                            IntToText                       , &
                                            CleanSpecialCharacters          , &
                                            GetArrayData                    , &
                                            ShellSort                       , &
                                            EstablishAbsolutePathFilename   , &
                                            ConvertID_To_Index              
  USE TimeSeriesUtilities           , ONLY: TimeStepType                    , &
                                            TimeIntervalConversion          , &
                                            IsTimeIntervalValid             
  USE IOInterface                   , ONLY: GenericFileType                 
  USE Class_StrmNode                , ONLY: StrmNodeType                    , &
                                            StrmNode_New                    , &
                                            StrmNode_WritePreprocessedData  
  USE Class_StrmReach               , ONLY: StrmReach_New                   , &
                                            StrmReach_WritePreprocessedData , &
                                            StrmReach_CompileReachNetwork   
  USE Package_ComponentConnectors   , ONLY: StrmGWConnectorType             , &
                                            StrmLakeConnectortype           , &
                                            f_iStrmToLakeFlow               , &
                                            f_iLakeToStrmFlow                 
  USE Package_Discretization        , ONLY: AppGridType                     , &
                                            StratigraphyType                 
  USE Package_Misc                  , ONLY: PairedDataType                  , &
                                            f_iFlowDest_Outside             , &
                                            f_iFlowDest_Lake                , &
                                            f_iFlowDest_StrmNode            , &
                                            f_iStrmComp
  USE Package_Budget                , ONLY: BudgetHeaderType
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
  PUBLIC :: AppStream_v40_Type                                              
 
  
  ! -------------------------------------------------------------
  ! --- APPLICATION STREAMS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseAppStreamType) :: AppStream_v40_Type
    PRIVATE
    TYPE(StrmNodeType),ALLOCATABLE  :: Nodes(:)
  CONTAINS
    PROCEDURE,PASS :: SetStaticComponent             => AppStream_v40_SetStaticComponent
    PROCEDURE,PASS :: SetStaticComponentFromBinFile  => ReadPreprocessedData
    PROCEDURE,PASS :: SetDynamicComponent            => AppStream_v40_SetDynamicComponent
    PROCEDURE,PASS :: SetAllComponents               => AppStream_v40_SetAllComponents
    PROCEDURE,PASS :: SetAllComponentsWithoutBinFile => AppStream_v40_SetAllComponentsWithoutBinFile
    PROCEDURE,PASS :: GetStrmNodeIDs                 => AppStream_v40_GetStrmNodeIDs
    PROCEDURE,PASS :: GetStrmNodeID                  => AppStream_v40_GetStrmNodeID
    PROCEDURE,PASS :: GetStrmNodeIndex               => AppStream_v40_GetStrmNodeIndex
    PROCEDURE,PASS :: GetNUpstrmNodes                => AppStream_v40_GetNUpstrmNodes
    PROCEDURE,PASS :: GetUpstrmNodes                 => AppStream_v40_GetUpstrmNodes
    PROCEDURE,PASS :: GetStageFlowRatingTable        => AppStream_v40_GetStageFlowRatingTable
    PROCEDURE,PASS :: GetVersion                     => AppStream_v40_GetVersion
    PROCEDURE,PASS :: GetBottomElevations            => AppStream_v40_GetBottomElevations
    PROCEDURE,PASS :: GetNRatingTablePoints          => AppStream_v40_GetNRatingTablePoints
    PROCEDURE,PASS :: KillImplementation             => AppStream_v40_Kill
    PROCEDURE,PASS :: WritePreprocessedData          => AppStream_v40_WritePreprocessedData
    PROCEDURE,PASS :: WriteDataToTextFile            => AppStream_v40_WriteDataToTextFile
    PROCEDURE,PASS :: UpdateHeads                    => AppStream_v40_UpdateHeads
    PROCEDURE,PASS :: ConvertTimeUnit                => AppStream_v40_ConvertTimeUnit
    PROCEDURE,PASS :: ConvertFlowToElev              => AppStream_v40_ConvertFlowToElev
    PROCEDURE,PASS :: Simulate                       => AppStream_v40_Simulate
  END TYPE AppStream_v40_Type
    
    
  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iVersion    = 40
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '4.0.0000'
  INCLUDE 'AppStream_v40_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen      = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName         = 'Class_AppStream_v40::'
  INTEGER,PARAMETER                   :: NStrmBudColumns = 13
  
  
  
  
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
  SUBROUTINE AppStream_v40_SetStaticComponent(AppStream,cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    CLASS(AppStream_v40_Type),INTENT(OUT) :: AppStream
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    TYPE(AppGridType),INTENT(IN)          :: AppGrid         !Not used in this version! Only included to comply with the interface definition.
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    LOGICAL,INTENT(IN)                    :: IsRoutedStreams
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'AppStream_v40_SetStaticComponent'
    CHARACTER                    :: ALine*100
    INTEGER                      :: NRTB,ErrorCode,iGWNodeIDs(AppGrid%NNodes)
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
    CALL DataFile%ReadData(NRTB ,iStat)                 ;  IF (iStat .EQ. -1) RETURN
    
    !Make sure that NRTB is greater than 1
    IF (NRTB .LE. 1) THEN
        CALL SetLastMessage('Number of data points in stream rating tables should be greater than 1!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
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
    
    !Read rating tables
    CALL ReadStreamRatingTables(iGWNodeIDs,NRTB,Stratigraphy,StrmGWConnector,DataFile,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read stream nodes and fraction of stream-aquifer interaction to be applied to corresponding gw nodes
    CALL ReadFractionsForGW(DataFile,AppStream%Nodes%ID,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL DataFile%Kill()
    
  END SUBROUTINE AppStream_v40_SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC PART OF STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v40_Type)         :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy       !Not used in this version
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33) :: ThisProcedure = ModName // 'AppStream_v40_SetDynamicComponent'
    INTEGER                      :: indxNode,iStrmNodeIDs(AppStream%NStrmNodes),iReachIDs(AppStream%NReaches),indx
    TYPE(GenericFileType)        :: MainFile
    CHARACTER(LEN=1000)          :: ALine,DiverFileName,DiverSpecFileName,BypassSpecFileName,DiverDetailBudFileName,ReachBudRawFileName
    TYPE(BudgetHeaderType)       :: BudHeader
    CHARACTER(:),ALLOCATABLE     :: cVersionSim,cVersionPre,cAbsPathFileName
    
    !Initialzie
    iStat        = 0
    iStrmNodeIDs = AppStream%Nodes%ID
  
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
    
    !Allocate memory for stream states
    IF (.NOT. ALLOCATED(AppStream%State)) ALLOCATE (AppStream%State(AppStream%NStrmNodes))
    
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

    !Stream reach budget raw filename
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
    
    !Hydrograph printing
    CALL AppStream%StrmHyd%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,AppStream%NStrmNodes,iStrmNodeIDs,TimeStep,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream budget at selected nodes
    CALL AppStream%StrmNodeBudget%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,AppStream%GetVersion(),PrepareStreamBudgetHeader,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream bed parameters for stream-gw connectivity
    CALL StrmGWConnector%CompileConductance(MainFile,AppGrid,Stratigraphy,AppStream%NStrmNodes,iStrmNodeIDs,AppStream%Reaches%UpstrmNode,AppStream%Reaches%DownstrmNode,AppStream%Nodes%BottomElev,iStat)
    IF (iStat .EQ. -1) RETURN
        
    !If non-routed streams, return
    IF (.NOT. AppStream%lRouted) THEN
        CALL MainFile%Kill()
        RETURN
    END IF
    
    !Set the heads to the bottom elevation
    DO indxNode=1,AppStream%NStrmNodes
        AppStream%State(indxNode)%Head   = AppStream%Nodes(indxNode)%BottomElev
        AppStream%State(indxNode)%Head_P = AppStream%State(indxNode)%Head
    END DO
    
    !Close main file
    CALL MainFile%Kill() 
  
  END SUBROUTINE AppStream_v40_SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v40_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                    :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy       !Not used in this version
    TYPE(GenericFileType)                 :: BinFile
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(StrmGWConnectorType)             :: StrmGWConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'AppStream_v40_SetAllComponents'
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Instantiating streams')
    
    !Read the preprocessed data for streams
    CALL ReadPreprocessedData(AppStream,BinFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Set the dynamic part of AppStream
    CALL AppStream_v40_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
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
    
  END SUBROUTINE AppStream_v40_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v40_Type),INTENT(OUT) :: AppStream
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
    CHARACTER(LEN=ModNameLen+44) :: ThisProcedure = ModName // 'AppStream_v40_SetAllComponentsWithoutBinFile'
    
    !Initialize
    iStat = 0
    
    !Instantiate the static components of the AppStream data
    CALL AppStream_v40_SetStaticComponent(AppStream,cPPFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the dynamic component of the AppStream data
    CALL AppStream_v40_SetDynamicComponent(AppStream,IsForInquiry,cSimFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
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
  
  END SUBROUTINE AppStream_v40_SetAllComponentsWithoutBinFile

  

  
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
  SUBROUTINE AppStream_v40_Kill(AppStream)
    CLASS(AppStream_v40_Type) :: AppStream
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Deallocate array attributes
    DEALLOCATE (AppStream%Nodes , STAT=ErrorCode)
    
  END SUBROUTINE AppStream_v40_Kill
    
  
  
  
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
  ! --- GET VERSION NUMBER 
  ! -------------------------------------------------------------
  FUNCTION AppStream_v40_GetVersion(AppStream) RESULT(cVrs)
    CLASS(AppStream_v40_Type) :: AppStream
    CHARACTER(:),ALLOCATABLE  :: cVrs
    
    IF (.NOT. AppStream%Version%IsDefined())   &
        AppStream%Version = AppStream%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = AppStream%Version%GetVersion()
    
  END FUNCTION AppStream_v40_GetVersion
  

  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_v40_GetStrmNodeIDs(AppStream,iStrmNodeIDs)
    CLASS(AppStream_v40_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)                  :: iStrmNodeIDs(:)
    
    iStrmNodeIDs = AppStream%Nodes%ID
    
  END SUBROUTINE AppStream_v40_GetStrmNodeIDs


  ! -------------------------------------------------------------
  ! --- GET STREAM NODE ID GIVEN INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v40_GetStrmNodeID(AppStream,indx) RESULT(iStrmNodeID)
    CLASS(AppStream_v40_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                   :: indx
    INTEGER                              :: iStrmNodeID
    
    iStrmNodeID = AppStream%Nodes(indx)%ID
    
  END FUNCTION AppStream_v40_GetStrmNodeID


  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDEX GIVEN ID
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v40_GetStrmNodeIndex(AppStream,ID) RESULT(Index)
    CLASS(AppStream_v40_Type),INTENT(IN) :: AppStream
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
    
  END FUNCTION AppStream_v40_GetStrmNodeIndex


  ! -------------------------------------------------------------
  ! --- GET RATING TABLE (STAGE VS. FLOW) AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_GetStageFlowRatingTable(AppStream,iNode,Stage,Flow)
    CLASS(AppStream_v40_Type),TARGET,INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                          :: iNode
    REAL(8),INTENT(OUT)                         :: Stage(:),Flow(:)
    
    !Local variables
    TYPE(StrmNodeType),POINTER :: pStrmNode
    
    !Initialize
    pStrmNode => AppStream%Nodes(iNode)
    
    !Gather content
    Stage = pStrmNode%RatingTable%XPoint - pStrmNode%BottomElev
    Flow  = pStrmNode%RatingTable%YPoint
    
    !Relase memory
    NULLIFY(pStrmNode)
    
  END SUBROUTINE AppStream_v40_GetStageFlowRatingTable


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF RATING TABLE POINTS AT A STREAM NODE
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v40_GetNRatingTablePoints(AppStream,iStrmNode) RESULT(N)
    CLASS(AppStream_v40_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                   :: iStrmNode
    INTEGER                              :: N
    
    IF (iStrmNode .GT. 0) THEN
      IF (iStrmNode.LE.AppStream%NStrmNodes) N = AppStream%Nodes(iStrmNode)%RatingTable%NPoints
    ELSE
      N = 0
    END IF
    
  END FUNCTION AppStream_v40_GetNRatingTablePoints


  ! -------------------------------------------------------------
  ! --- GET BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v40_GetBottomElevations(AppStream) RESULT(BottomElev)
    CLASS(AppStream_v40_Type),INTENT(IN) :: AppStream
    REAL(8)                              :: BottomElev(AppStream%NStrmNodes)
    
    BottomElev = AppStream%Nodes%BottomElev
    
  END FUNCTION AppStream_v40_GetBottomElevations
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  FUNCTION AppStream_v40_GetNUpstrmNodes(AppStream,iStrmNode) RESULT(iNNodes)
    CLASS(AppStream_v40_Type),INTENT(IN)  :: AppStream
    INTEGER,INTENT(IN)                    :: iStrmNode
    INTEGER                               :: iNNodes
    
    iNNodes = AppStream%Nodes(iStrmNode)%Connectivity%nConnectedNodes
    
  END FUNCTION AppStream_v40_GetNUpstrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_GetUpstrmNodes(AppStream,iNode,UpstrmNodes)
    CLASS(AppStream_v40_Type),INTENT(IN)  :: AppStream
    INTEGER,INTENT(IN)                    :: iNode
    INTEGER,ALLOCATABLE,INTENT(OUT)       :: UpstrmNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (UpstrmNodes , STAT=ErrorCode)

    ALLOCATE (UpstrmNodes(AppStream%Nodes(iNode)%Connectivity%nConnectedNodes))
    UpstrmNodes = AppStream%Nodes(iNode)%Connectivity%ConnectedNodes
    
  END SUBROUTINE AppStream_v40_GetUpstrmNodes

  
  

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
    CLASS(AppStream_v40_Type),INTENT(OUT) :: AppStream
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
    CALL BinFile%ReadData(AppStream%NStrmNodes,iStat)               ;  IF (iStat .EQ. -1) RETURN
    CALL BinFile%ReadData(AppStream%NReaches,iStat)                 ;  IF (iStat .EQ. -1) RETURN
    CALL BinFile%ReadData(AppStream%TimeUnitRatingTableFlow,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (AppStream%Nodes(AppStream%NStrmNodes) , AppStream%Reaches(AppStream%NReaches) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read stream node data
    CALL StrmNode_New(AppStream%NStrmNodes,BinFile,AppStream%Nodes,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read stream reach data
    CALL StrmReach_New(AppStream%NReaches,BinFile,AppStream%Reaches,iStat) 
    
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
    TYPE(AppStream_v40_Type)              :: AppStream
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadStreamConfigData'
    INTEGER                      :: indxReach,DummyIntArray3(3),indxNode,DummyIntArray2(2), &
                                    NNodes,iGWNodes(AppStream%NStrmNodes),indxStrmNode,     &
                                    indxReach1,iStrmNodeID,indxNode1,iReachID,iDestNode,    &
                                    iLayers(AppStream%NStrmNodes)
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
            
            !Read stream node IDs and corresponding gw nodes
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
  ! --- READ STREAM RATING TABLES
  ! -------------------------------------------------------------
  SUBROUTINE ReadStreamRatingTables(iGWNodeIDs,NRTB,Stratigraphy,StrmGWConnector,DataFile,AppStream,iStat)
    INTEGER,INTENT(IN)                   :: iGWNodeIDs(:),NRTB
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(GenericFileType)                :: DataFile
    TYPE(AppStream_v40_Type)             :: AppStream
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'ReadStreamRatingTables'
    INTEGER                      :: indxNode,iGWNode,iLayer,ErrorCode,indx,iNode,iStrmNodeID,iStrmNodeIDs(AppStream%NStrmNodes)
    REAL(8)                      :: FACTLT,FACTQ,HRTB(NRTB),QRTB(NRTB),DummyArray(4),  &
                                    DummyArray2D(NRTB-1,2),AquiferBottomElev
    CHARACTER                    :: ALine*500
    LOGICAL                      :: lProcessed(AppStream%NStrmNodes)
    INTEGER,ALLOCATABLE          :: iGWNodes(:)
    
    !Initialize
    iStat        = 0
    iStrmNodeIDs = AppStream%Nodes%ID
    lProcessed   = .FALSE.
    CALL StrmGWConnector%GetAllGWNodes(iGWNodes)
    
    !Read units conversion factors
    CALL DataFile%ReadData(FACTLT,iStat)  ;  IF (iStat .EQ. -1) RETURN    
    CALL DataFile%ReadData(FACTQ,iStat)   ;  IF (iStat .EQ. -1) RETURN    
    CALL DataFile%ReadData(ALine,iStat)   ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    AppStream%TimeUnitRatingTableFlow = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Make sure that time unit of rating tables is recognized
    IF (IsTimeIntervalValid(TRIM(AppStream%TimeUnitRatingTableFlow)) .EQ. 0) THEN
        CALL SetLastMessage('Time unit for the stream rating tables ('//TRIM(AppStream%TimeUnitRatingTableFlow)//') is not recognized!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read rating tables
    DO indxNode=1,AppStream%NStrmNodes
        CALL DataFile%ReadData(DummyArray,iStat)    ;  IF (iStat .EQ. -1) RETURN
        CALL DataFile%ReadData(DummyArray2D,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Make sure stream node ID is recognized
        iStrmNodeID = INT(DummyArray(1))
        CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,iNode)
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' listed for stream rating tables is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure the node has not been processed before
        IF (lProcessed(iNode)) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' has been assigned rating tables more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iNode) = .TRUE.

        !Corresponding gw node and active top layer
        iGWNode = iGWNodes(iNode)
        iLayer  = Stratigraphy%TopActiveLayer(iGWNode)
        
        !Stream bottom elevation
        AppStream%Nodes(iNode)%BottomElev = DummyArray(2) * FACTLT
        AquiferBottomElev                 = Stratigraphy%BottomElev(iGWNode,iLayer)
        IF (AppStream%Nodes(iNode)%BottomElev .LT. AquiferBottomElev) THEN
            MessageArray(1) = 'Aquifer bottom elevation at a stream node should be'
            MessageArray(2) = 'less than or equal to the stream bed elevation!'
            WRITE (MessageArray(3),'(A,F10.2)') ' Stream node = '//TRIM(IntToText(iStrmNodeID))         //'   Stream bed elevation    = ',AppStream%Nodes(iNode)%BottomElev
            WRITE (MessageArray(4),'(A,F10.2)') ' GW node     = '//TRIM(IntToText(iGWNodeIDs(iGWNode))) //'   Aquifer bottom elevation= ',AquiferBottomElev
            CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Rating table
        HRTB(1)  = DummyArray(3) * FACTLT
        QRTB(1)  = DummyArray(4) * FACTQ
        HRTB(2:) = DummyArray2D(:,1) * FACTLT
        QRTB(2:) = DummyArray2D(:,2) * FACTQ
        HRTB     = HRTB + AppStream%Nodes(iNode)%BottomElev
        CALL AppStream%Nodes(iNode)%RatingTable%New(NRTB,HRTB,QRTB,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Make sure rating table is specified properly
        DO indx=2,NRTB
            IF (HRTB(indx) .LE. HRTB(indx-1)) THEN
                MessageArray(1) = 'The flow depths specified in the rating table for stream node '//TRIM(IntToText(iStrmNodeID))
                MessageArray(2) = 'must monotonically increase!'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (QRTB(indx) .LE. QRTB(indx-1)) THEN
                MessageArray(1) = 'The flows specified in the rating table for stream node '//TRIM(IntToText(iStrmNodeID))
                MessageArray(2) = 'must monotonically increase!'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !If the rating table is problematic, warn the user
        IF (AppStream%Nodes(iNode)%RatingTable%CheckGradientMonotonicity() .EQ. .FALSE.) THEN
            MessageArray(1) = 'The gradient of the rating table at stream node '//TRIM(IntToText(iStrmNodeID))//' is not monotonicaly increasing or decreasing!'
            MessageArray(2) = 'This may lead to problems with the iterative solution!'
            CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
        END IF
        
    END DO
    
    !Clear memeory
    DEALLOCATE (iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE ReadStreamRatingTables   

  
  

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
  ! --- WRITE PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_WritePreprocessedData(AppStream,OutFile)
    CLASS(AppStream_v40_Type),INTENT(IN) :: AppStream
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
    CALL OutFile%WriteData(AppStream%TimeUnitRatingTableFlow)
    
    !Write node data
    CALL StrmNode_WritePreprocessedData(AppStream%Nodes,OutFile)
    
    !Write reach data
    CALL StrmReach_WritePreprocessedData(AppStream%Reaches,OutFile)
        
  END SUBROUTINE AppStream_v40_WritePreprocessedData
  
  
  ! -------------------------------------------------------------
  ! --- WRITE PREPROCESSED DATA TO TEXT FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_WriteDataToTextFile(AppStream,iGWNodeIDs,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
    CLASS(AppStream_v40_Type),INTENT(IN) :: AppStream
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
            CALL AppStream_v40_GetUpstrmNodes(AppStream,indxNode,UpstrmNodes)
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
    
    !Clear Memeory 
    DEALLOCATE (iGWNodes , UpstrmNodes , STAT=ErrorCode)
    
  END SUBROUTINE AppStream_v40_WriteDataToTextFile
  
  
  

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
  ! --- CONVERT STREAM FLOWS TO STREAM SURFACE ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_ConvertFlowToElev(AppStream)
    CLASS(AppStream_v40_Type) :: AppStream
    
    !Local variables
    INTEGER :: indxNode
    
    DO indxNode=1,AppStream%NStrmNodes
        AppStream%State(indxNode)%Head = AppStream%Nodes(indxNode)%RatingTable%InverseEvaluate(AppStream%State(indxNode)%Flow)
    END DO
    
  END SUBROUTINE AppStream_v40_ConvertFlowToElev

  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF STREAMS RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_ConvertTimeUnit(AppStream,NewUnit)
    CLASS(AppStream_v40_Type)   :: AppStream
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    INTEGER :: indxNode
    REAL(8) :: Factor
    
    !Convert rating table flow time unit
    Factor                            = TimeIntervalConversion(NewUnit,AppStream%TimeUnitRatingTableFlow)
    AppStream%TimeUnitRatingTableFlow = NewUnit
    DO indxNode=1,AppStream%NStrmNodes
      AppStream%Nodes(indxNode)%RatingTable%YPoint = AppStream%Nodes(indxNode)%RatingTable%YPoint * Factor
    END DO
    
    !Convert bypass rating table time units
    CALL AppStream%AppDiverBypass%ConvertTimeUnit(NewUnit)
    
  END SUBROUTINE AppStream_v40_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v40_Simulate(AppStream,GWHeads,Runoff,ReturnFlow,TributaryFlow,DrainInflows,RiparianET,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v40_Type)   :: AppStream
    REAL(8),INTENT(IN)          :: GWHeads(:,:),Runoff(:),ReturnFlow(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    REAL(8),INTENT(OUT)         :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    TYPE(MatrixType)            :: Matrix
    
    !Local variables
    INTEGER                                 :: indxNode,indx,iNode,indxReach,ErrorCode,iNodeIDs_Connect(1),NNodes,NDiver
    REAL(8)                                 :: rInflow,rOutflow,Bypass_Recieved,rBypassOut,rRipET,rValue,rUpdateValues(1)
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: dFlow_dStage,Inflows,rUpdateRHS,HRG,rNetInflows
    INTEGER,ALLOCATABLE                     :: iStrmIDs(:),iLakeIDs(:)
    INTEGER,PARAMETER                       :: iCompIDs_Connect(1) = [f_iStrmComp] 
    
    !Inform user about simulation progress
    CALL EchoProgress('Simulating stream flows')
    
    !Initialize
    NNodes  = SIZE(GWHeads , DIM=1)
    NDiver  = AppStream%AppDiverBypass%NDiver
    Inflows = AppStream%StrmInflowData%GetInflows_AtAllNodes(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Get groundwater heads at stream nodes
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
    
    !Compute stream flows based on rating table
    dFlow_dStage = 0.0
    DO indxNode=1,AppStream%NStrmNodes
      ASSOCIATE (pRatingTable => AppStream%Nodes(indxNode)%RatingTable , &
                 pState       => AppStream%State(indxNode)             )
        CALL pRatingTable%EvaluateAndDerivative(pState%Head,pState%Flow,dFlow_dStage(indxNode))
        pState%Flow = MAX(pState%Flow , 0.0)
      END ASSOCIATE
    END DO
    
    !Initialize bypass flows to zero (only for those that originate within the model)
    DO indx=1,AppStream%AppDiverBypass%NBypass
        IF (AppStream%AppDiverBypass%Bypasses(indx)%iNode_Exp .GT. 0) THEN
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Out      = 0.0
            AppStream%AppDiverBypass%Bypasses(indx)%Bypass_Received = 0.0
        END IF
    END DO
    
    !Update the matrix equation
    DO indxReach=1,AppStream%NReaches
        DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
          
            !Initialize
            rInflow  = 0.0
            rOutflow = 0.0
            
            !Recieved bypass
            Bypass_Recieved = AppStream%AppDiverBypass%GetBypassReceived_AtADestination(f_iFlowDest_StrmNode,indxNode)
            
            !Inflows at the stream node with known values
            rInflow = Inflows(indxNode)                                       &    !Inflow as defined by the user
                    + Runoff(indxNode)                                        &    !Direct runoff of precipitation 
                    + ReturnFlow(indxNode)                                    &    !Return flow of applied water 
                    + TributaryFlow(indxNode)                                 &    !Tributary inflows from small watersheds and creeks
                    + DrainInflows(indxNode)                                  &    !Inflow from tile drains
                    + Bypass_Recieved                                         &    !Received by-pass flows 
                    + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indxNode)        !Flows from lake outflow
            
            !Inflows from upstream nodes
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode   = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                rInflow = rInflow + AppStream%State(iNode)%Flow
            END DO
            
            !Diversion
            IF (NDiver .GT. 0) THEN
                rOutflow                                            = MIN(rInflow , AppStream%AppDiverBypass%NodalDiverRequired(indxNode))
                AppStream%AppDiverBypass%NodalDiverActual(indxNode) = rOutflow
            END IF
            
            !Bypass
            CALL AppStream%AppDiverBypass%ComputeBypass(indxNode,rInflow-rOutflow,StrmLakeConnector,rBypassOut)
            rOutflow = rOutflow + rBypassOut
            
            !Riparian ET outflow
            IF (RiparianET(indxNode) .GT. 0.0) THEN
                rRipET                   = MIN(RiparianET(indxNode) , rInflow-rOutflow)
                RiparianETFrac(indxNode) = rRipET / RiparianET(indxNode)
                rOutflow                 = rOutflow + rRipET
            ELSE
                RiparianETFrac(indxNode) = 0.0
            END IF
            
            !Net inflow at strm node
            rNetInflows(indxNode) = rInflow - rOutflow 
                
            !Compute the matrix rhs function and its derivatives w.r.t. stream elevation
            !----------------------------------------------------------------------------
            
            !RHS function
            rUpdateRHS(indxNode) = AppStream%State(indxNode)%Flow - rNetInflows(indxNode)
            
            !Derivative of function w.r.t. stream elevation
            iNodeIDs_Connect(1) = indxNode
            rUpdateValues(1)    = dFlow_dStage(indxNode)
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxNode,1,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
             
            !Derivative of function w.r.t. stream elevation at upstream nodes that directly feed into stream node in consideration
            DO indx=1,AppStream%Nodes(indxNode)%Connectivity%nConnectedNodes
                iNode  = AppStream%Nodes(indxNode)%Connectivity%ConnectedNodes(indx)
                rValue = -dFlow_dStage(iNode) 
                CALL Matrix%SetCOEFF(f_iStrmComp,indxNode,f_iStrmComp,iNode,rValue)
            END DO
         
        END DO
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iStrmComp,1,rUpdateRHS)
    
    !Stream flows to lakes
    CALL StrmLakeConnector%GetSourceIDs(f_iStrmToLakeFlow,iStrmIDs)
    CALL StrmLakeConnector%GetDestinationIDs(f_iStrmToLakeFlow,iLakeIDs)
    DO indxNode=1,SIZE(iStrmIDs)
      CALL StrmLakeConnector%SetFlow(f_iStrmToLakeFlow,iStrmIDs(indxNode),iLakeIDs(indxNode),AppStream%State(iStrmIDs(indxNode))%Flow)
    END DO

    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate(NNodes,HRG,AppStream%State%Head,rNetInflows,Matrix)  
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
        
  END SUBROUTINE AppStream_v40_Simulate
   
  
  ! -------------------------------------------------------------
  ! --- COMPILE LIST OF NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  SUBROUTINE CompileUpstrmNodes(AppStream)
    TYPE(AppStream_v40_Type) :: AppStream
    
    !Local variables
    INTEGER :: indxReach,iCount,TempNodes(50),indxNode,indxReach1
    
    !Iterate over each reach and node
    DO indxReach=AppStream%NReaches,1,-1
      DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode

        !Initialize counter
        iCount =0
        
        !indxNode is the first node in reach
        IF (indxNode .EQ. AppStream%Reaches(indxReach)%UpstrmNode) THEN
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
  SUBROUTINE AppStream_v40_UpdateHeads(AppStream,HDelta)
    CLASS(AppStream_v40_Type) :: AppStream
    REAL(8),INTENT(IN)        :: HDelta(:)
    
    AppStream%State%Head = MAX(AppStream%State%Head-HDelta , AppStream%Nodes%BottomElev)

  END SUBROUTINE AppStream_v40_UpdateHeads
    
END MODULE