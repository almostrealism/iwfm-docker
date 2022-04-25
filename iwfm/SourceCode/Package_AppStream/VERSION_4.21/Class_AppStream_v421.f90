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
MODULE Class_AppStream_v421
  USE Class_Version                 , ONLY: ReadVersion
  USE Class_PairedData              , ONLY: PairedDataType         
  USE MessageLogger                 , ONLY: SetLastMessage                  , &
                                            LogMessage                      , &
                                            EchoProgress                    , &
                                            MessageArray                    , &
                                            f_iWarn                         , &
                                            f_iFatal                          
  USE GeneralUtilities              , ONLY: StripTextUntilCharacter         , &
                                            CleanSpecialCharacters          , &
                                            FirstLocation                   , &
                                            IntToText                       , &
                                            EstablishAbsolutePathFileName   , &
                                            GetArrayData                    , &
                                            ShellSort                       , &
                                            LocateInList                    , &
                                            ConvertID_To_Index              
  USE TimeSeriesUtilities           , ONLY: TimeStepType                    , &
                                            IsTimeIntervalValid             
  USE Package_Misc                  , ONLY: PairedDataType                  , &
                                            f_iFlowDest_Outside             , &
                                            f_iFlowDest_Lake                , &
                                            f_iFlowDest_StrmNode            , & 
                                            f_iStrmComp                     
  USE IOInterface                   , ONLY: GenericFileType                 
  USE Package_ComponentConnectors   , ONLY: StrmGWConnectorType             , &
                                            StrmLakeConnectorType           , &
                                            f_iStrmToLakeFlow               , &
                                            f_iLakeToStrmFlow                 
  USE Class_BaseAppStream           , ONLY: BaseAppStreamType               , &
                                            PrepareStreamBudgetHeader       , &
                                            CalculateNStrmNodes             
  USE Class_StrmNode                , ONLY: StrmNode_New                    , &
                                            StrmNode_WritePreprocessedData
  USE Class_StrmReach               , ONLY: StrmReach_New                   , &
                                            StrmReach_CompileReachNetwork   , &
                                            StrmReach_WritePreprocessedData
  USE Class_AppStream_v42           , ONLY: AppStream_v42_Type              , &
                                            ReadFractionsForGW              , &
                                            CompileUpstrmNodes 
  USE Package_Discretization        , ONLY: AppGridType                     , &
                                            StratigraphyType                
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
  PUBLIC :: AppStream_v421_Type                                              
 
  
  ! -------------------------------------------------------------
  ! --- APPLICATION STREAMS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AppStream_v42_Type) :: AppStream_v421_Type
      PRIVATE
      TYPE(PairedDataType),ALLOCATABLE :: RatingTable_WetPerimeter(:)
  CONTAINS
      PROCEDURE,PASS :: SetStaticComponent             => AppStream_v421_SetStaticComponent
      PROCEDURE,PASS :: SetStaticComponentFromBinFile  => ReadPreprocessedData
      PROCEDURE,PASS :: SetDynamicComponent            => AppStream_v421_SetDynamicComponent
      PROCEDURE,PASS :: SetAllComponents               => AppStream_v421_SetAllComponents
      PROCEDURE,PASS :: SetAllComponentsWithoutBinFile => AppStream_v421_SetAllComponentsWithoutBinFile
      PROCEDURE,PASS :: KillImplementation             => AppStream_v421_Kill
      PROCEDURE,PASS :: GetVersion                     => AppStream_v421_GetVersion
      PROCEDURE,PASS :: WritePreprocessedData          => AppStream_v421_WritePreprocessedData
      PROCEDURE,PASS :: Simulate                       => AppStream_v421_Simulate
  END TYPE AppStream_v421_Type
    
    
  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iVersion    = 421
  INTEGER,PARAMETER                    :: iLenVersion = 9
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '4.21.0000'
  INCLUDE 'AppStream_v421_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppStream_v421::'
  
  
  
  
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
  SUBROUTINE AppStream_v421_SetStaticComponent(AppStream,cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    CLASS(AppStream_v421_Type),INTENT(OUT) :: AppStream
    CHARACTER(LEN=*),INTENT(IN)            :: cFileName
    TYPE(AppGridType),INTENT(IN)           :: AppGrid         !Not used in this version! Only included to comply with the interface definition.
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy
    LOGICAL,INTENT(IN)                     :: IsRoutedStreams
    TYPE(StrmGWConnectorType),INTENT(OUT)  :: StrmGWConnector
    TYPE(StrmLakeConnectorType)            :: StrmLakeConnector
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33) :: ThisProcedure = ModName // 'AppStream_v421_SetStaticComponent'
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
    CALL DataFile%ReadData(NRTB,iStat)                  ;  IF (iStat .EQ. -1) RETURN
    
    !Make sure that NRTB is greater than 1
    IF (NRTB .LE. 1) THEN
        CALL SetLastMessage('Number of data points in stream rating tables should be greater than 1!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Compile the total number of stream nodes
    CALL CalculateNStrmNodes(DataFile,AppStream%NReaches,AppStream%NStrmNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (AppStream%Reaches(AppStream%NReaches)                     , &
              AppStream%Nodes(AppStream%NStrmNodes)                     , &
              AppStream%RatingTable_WetPerimeter(AppStream%NStrmNodes)  , &
              STAT = ErrorCode                                          )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream configuration data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read stream configuration
    CALL ReadStreamConfigData(DataFile,AppGrid,Stratigraphy,iGWNodeIDs,StrmGWConnector,StrmLakeConnector,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read rating tables
    CALL ReadStreamRatingTables(iGWNodeIDs,NRTB,Stratigraphy,StrmGWConnector,DataFile,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read stream nodes and fraction of stream-aquifer interaction to be applied to corresponding gw nodes
    CALL ReadFractionsForGW(DataFile,AppStream%Nodes%ID,StrmGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL DataFile%Kill()
    
  END SUBROUTINE AppStream_v421_SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC PART OF STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v421_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v421_Type)        :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy      !Not used in this versions
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+34) :: ThisProcedure = ModName // 'AppStream_v421_SetDynamicComponent'
    INTEGER                      :: indxNode,iReachIDs(AppStream%NReaches),iStrmNodeIDs(AppStream%NStrmNodes),indx
    TYPE(GenericFileType)        :: MainFile
    CHARACTER(LEN=1000)          :: ALine,DiverFileName,DiverSpecFileName,BypassSpecFileName,DiverDetailBudFileName,ReachBudRawFileName
    TYPE(BudgetHeaderType)       :: BudHeader
    CHARACTER(:),ALLOCATABLE     :: cVersionSim,cVersionPre,cAbsPathFileName
    
    !Initailize
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

    !Hydrograph printing
    CALL AppStream%StrmHyd%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,AppStream%NStrmNodes,iStrmNodeIDs,TimeStep,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream budget at selected nodes
    CALL AppStream%StrmNodeBudget%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,AppStream%GetVersion(),PrepareStreamBudgetHeader,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream bed parameters for stream-gw connectivity
    CALL StrmGWConnector%CompileConductance(MainFile,AppGrid,Stratigraphy,AppStream%NStrmNodes,iStrmNodeIDs,AppStream%Reaches%UpstrmNode,AppStream%Reaches%DownstrmNode,AppStream%Nodes%BottomElev,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !If non-routed streams, return at this point
    IF (.NOT. AppStream%lRouted) THEN
        CALL MainFile%Kill()
        RETURN
    END IF
    
    !Set the heads to the bottom elevation
    DO indxNode=1,AppStream%NStrmNodes
        AppStream%State(indxNode)%Head   = AppStream%Nodes(indxNode)%BottomElev + 2.0
        AppStream%State(indxNode)%Head_P = AppStream%State(indxNode)%Head
    END DO
    
    !Close main file
    CALL MainFile%Kill() 
  
  END SUBROUTINE AppStream_v421_SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v421_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v421_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                     :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)            :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(IN)                     :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy      !Not used in this version
    TYPE(GenericFileType)                  :: BinFile
    TYPE(StrmLakeConnectorType)            :: StrmLakeConnector
    TYPE(StrmGWConnectorType)              :: StrmGWConnector
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31) :: ThisProcedure = ModName // 'AppStream_v421_SetAllComponents'
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Instantiating streams')
    
    !Read the preprocessed data for streams
    CALL AppStream%SetStaticComponentFromBinFile(BinFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Set the dynamic part of AppStream
    CALL AppStream_v421_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
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
    
  END SUBROUTINE AppStream_v421_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v421_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v421_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                     :: IsRoutedStreams,IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)            :: cPPFileName,cSimFileName,cSimWorkingDirectory
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(IN)                     :: NTIME,iLakeIDs(:)
    TYPE(StrmLakeConnectorType)            :: StrmLakeConnector
    TYPE(StrmGWConnectorType),INTENT(OUT)  :: StrmGWConnector
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+45) :: ThisProcedure = ModName // 'AppStream_v421_SetAllComponentsWithoutBinFile'
    
    !Initialize
    iStat = 0
    
    !Instantiate the static components of the AppStream data
    CALL AppStream_v421_SetStaticComponent(AppStream,cPPFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the dynamic component of the AppStream data
    CALL AppStream_v421_SetDynamicComponent(AppStream,IsForInquiry,cSimFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
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
  
  END SUBROUTINE AppStream_v421_SetAllComponentsWithoutBinFile
  
  
  

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
  SUBROUTINE AppStream_v421_Kill(AppStream)
    CLASS(AppStream_v421_Type) :: AppStream
    
    !Local variables
    INTEGER                   :: ErrorCode
    TYPE(AppStream_v421_Type) :: Dummy
    
    CALL AppStream%AppStream_v42_Type%KillImplementation()
    
    !Deallocate array attributes
    DEALLOCATE (AppStream%RatingTable_WetPerimeter , STAT=ErrorCode)
    
    SELECT TYPE (p => AppStream)
        TYPE IS (AppStream_v421_Type)
            p = Dummy
    END SELECT
    
  END SUBROUTINE AppStream_v421_Kill
    
  
  
  
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
  FUNCTION AppStream_v421_GetVersion(AppStream) RESULT(cVrs)
    CLASS(AppStream_v421_Type) :: AppStream
    CHARACTER(:),ALLOCATABLE  :: cVrs
    
    IF (.NOT. AppStream%Version%IsDefined())   &
        AppStream%Version = AppStream%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = AppStream%Version%GetVersion()
    
  END FUNCTION AppStream_v421_GetVersion
  
  
  

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
    CLASS(AppStream_v421_Type),INTENT(OUT) :: AppStream
    TYPE(GenericFileType)                  :: BinFile
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadPreprocessedData'
    INTEGER                      :: ErrorCode,iLenVersion,indxNode
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
    ALLOCATE (AppStream%Nodes(AppStream%NStrmNodes) , AppStream%Reaches(AppStream%NReaches) , AppStream%RatingTable_WetPerimeter(AppStream%NStrmNodes) , STAT=ErrorCode)
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
    
    !Read wetted perimeter rating tables
    DO indxNode=1,AppStream%NStrmNodes
        CALL AppStream%RatingTable_WetPerimeter(indxNode)%New(BinFile,iStat)
        IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE ReadPreprocessedData
  

  ! -------------------------------------------------------------
  ! --- READ STREAM REACH CONFIGURATION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadStreamConfigData(DataFile,AppGrid,Stratigraphy,iGWNodeIDs,StrmGWConnector,StrmLakeConnector,AppStream,iStat)
    TYPE(GenericFileType)                 :: DataFile
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    INTEGER,INTENT(IN)                    :: iGWNodeIDs(:)
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(AppStream_v421_Type)             :: AppStream
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadStreamConfigData'
    INTEGER                      :: indxReach,DummyIntArray3(3),indxNode,DummyIntArray2(2),      &
                                    iDestNode,NNodes,iGWNodes(1000),iLoc,iNData,indx,indxNode1,  &
                                    iLayers(1000),nGWNodes,iStrmNodeID,indxStrmNode,iReachID,    &
                                    indxReach1,iRanks(1000),iRanksKeep(1000),iGWNodeID,iGWNode1, &
                                    iGWNode2,indxFace,iFace 
    LOGICAL                      :: lGoodNode(1000)
    CHARACTER                    :: ALine*30000,ALineWork*30000
    
    !Initailize
    iStat = 0
    
    !Iterate over reaches and read the data 
    indxStrmNode = 0
    DO indxReach=1,AppStream%NReaches
        ASSOCIATE (pReach => AppStream%Reaches(indxReach))
            CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL CleanSpecialCharacters(ALine)
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
                nGWNodes     = 1
                
                CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
                CALL CleanSpecialCharacters(ALine)
                ALine = StripTextUntilCharacter(ALine,'/')
                ALine = StripTextUntilCharacter(ALine,'!')
                READ (ALine,*) iStrmNodeID 
                AppStream%Nodes(indxStrmNode)%ID = iStrmNodeID
                CALL GetArrayData(ALine,DummyIntArray2,'groundwater nodes corresponding to stream node '//TRIM(IntToText(iStrmNodeID)),iStat)  
                IF (iStat .EQ. -1) RETURN
                
                !Make sure stream node ID is not repeated
                DO indxNode1=1,indxStrmNode-1
                    IF (iStrmNodeID .EQ.  AppStream%Nodes(indxNode1)%ID) THEN
                        CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' is used more than once!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END DO
                
                !First gw node index and corresponding layer
                CALL ConvertID_To_Index(DummyIntArray2(2),iGWNodeIDs,iGWNodes(1))
                IF (iGWNodes(1) .EQ. 0) THEN
                    CALL SetLastMessage('Groundwater node '//TRIM(IntToText(DummyIntArray2(2)))//' listed in stream reach '//TRIM(IntToText(pReach%ID))//' ('//TRIM(pReach%cName)//') for stream node '//TRIM(IntToText(iStrmNodeID))//' is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                iLayers(1) = Stratigraphy%TopActiveLayer(iGWNodes(1))
                
                !Figure out the number of gw nodes associated with the stream node
                ALineWork = ADJUSTL(ALine)
                iNData    = 1
                DO
                    !If no other entry is available then user didn't enter inundation rank and there is only one gw node
                    IF (LEN_TRIM(ALineWork) .EQ. 0) THEN
                        iRanks(1) = 1
                        EXIT
                    END IF
                    
                    !Count the next data
                    iNData    = iNData + 1
                    iLoc      = FirstLocation(' ',ALineWork)
                    ALineWork = ADJUSTL(ALineWork(iLoc+1:))
                    
                    !Exit if there is no more data
                    IF (LEN_TRIM(ALineWork) .EQ. 0) EXIT
                END DO
                
                !Read the rest of the gw nodes, ranking of the gw nodes and compile layer information
                IF (iNData .GT. 1) THEN
                    !Make sure that there are even numbers of data (gw node and ranking)
                    IF (MOD(iNData,2) .NE. 0) THEN
                        CALL SetLastMessage('Check that each groundwater node listed for stream node '//TRIM(IntToText(iStrmNodeID))//' is also given a rank for inundation order.',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    
                    !Number of gw nodes
                    nGWNodes = iNData / 2
                    
                    !Read gw nodes and ranking
                    READ (ALine,*) iGWNodes(2:nGWNodes),iRanks(1:nGWNodes)
                    
                    !Process gw nodes and find active layers 
                    DO indx=2,nGWNodes
                        iGWNodeID = iGWNodes(indx)
                        CALL ConvertID_To_Index(iGWNodeID,iGWNodeIDs,iGWNodes(indx))
                        IF (iGWNodes(indx) .EQ. 0) THEN
                            CALL SetLastMessage('Groundwater node '//TRIM(IntToText(iGWNodeID))//' listed in stream reach '//TRIM(IntToText(pReach%ID))//' ('//TRIM(pReach%cName)//') for stream node '//TRIM(IntToText(iStrmNodeID))//' is not in the model!',f_iFatal,ThisProcedure)
                            iStat = -1
                            RETURN
                        END IF
                        iLayers(indx) = Stratigraphy%TopActiveLayer(iGWNodes(indx))
                    END DO
                    
                    !Make sure that gw nodes follow element edges and calculate lengths associated with each gw node along the cross-section
                    IF (nGWNodes .GT. 1) THEN
                        lGoodNode(1:nGWNodes) = .FALSE.
                        DO indx=1,nGWNodes
                            iGWNode1 = iGWNodes(indx)
                            DO indxFace=1,AppGrid%AppNode(iGWNode1)%NFaceID
                                iFace = AppGrid%AppNode(iGWNode1)%FaceID(indxFace)
                                IF (AppGrid%AppFace%Node(1,iFace) .EQ. iGWNode1) THEN
                                    iGWNode2 = AppGrid%AppFace%Node(2,iFace)
                                ELSE
                                    iGWNode2 = AppGrid%AppFace%Node(1,iFace)
                                END IF
                                iLoc = LocateInList(iGWNode2,iGWNodes(1:nGWNodes))
                                IF (iLoc .GT. 0) THEN
                                    lGoodNode(indx) = .TRUE.
                                    lGoodNode(iLoc) = .TRUE.
                                END IF
                            END DO
                        END DO
                        IF (.NOT. ALL(lGoodNode(1:nGWNodes))) THEN
                            CALL SetLastMessage('Groundwater nodes listed for stream node '//TRIM(IntToText(iStrmNodeID))//' are not aligned along element faces!',f_iFatal,ThisProcedure)
                            iStat = -1
                            RETURN
                        END IF
                    END IF
                    
                    !Order gw nodes according to ranking
                    iRanksKeep(1:nGWNodes) = iRanks(1:nGWNodes)
                    CALL ShellSort(iRanks(1:nGWNodes)     , iGWNodes(1:nGWNodes))
                    CALL ShellSort(iRanksKeep(1:nGWNodes) , iLayers(1:nGWNodes))

                END IF
                          
                !Store gw nodes and layers 
                CALL StrmGWConnector%AddGWNodesToStrmNode(iVersion,AppStream%NStrmNodes,indxStrmNode,iGWNodes(1:nGWNodes),iLayers(1:nGWNodes),iRanks(1:nGWNodes),iStat)
                IF (iStat .EQ. -1) RETURN
                
            END DO
          
        END ASSOCIATE
    END DO
    
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
    CALL CompileUpstrmNodes(AppStream%AppStream_v42_Type)
    
  END SUBROUTINE ReadStreamConfigData
  
  
  ! -------------------------------------------------------------
  ! --- READ STREAM RATING TABLES
  ! -------------------------------------------------------------
  SUBROUTINE ReadStreamRatingTables(iGWNodeIDs,NRTB,Stratigraphy,StrmGWConnector,DataFile,AppStream,iStat)
    INTEGER,INTENT(IN)                   :: iGWNodeIDs(:),NRTB
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(GenericFileType)                :: DataFile
    TYPE(AppStream_v421_Type)            :: AppStream
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'ReadStreamRatingTables'
    INTEGER                      :: indxNode,iStrmNodeID,iGWNode,iLayer,ErrorCode,indx,iNodeIDs(AppStream%NStrmNodes),iNode
    REAL(8)                      :: FACTLT,FACTQ,HRTB(NRTB),QRTB(NRTB),WPTB(NRTB),DummyArray(5),  &
                                    DummyArray2D(NRTB-1,3),AquiferBottomElev
    CHARACTER                    :: ALine*500
    LOGICAL                      :: lProcessed(AppStream%NStrmNodes)
    INTEGER,ALLOCATABLE          :: iGWNodes(:)
    
    !Initialize
    iStat      = 0
    iNodeIDs   = AppStream%Nodes%ID
    lProcessed = .FALSE.
    
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
        CALL ConvertID_To_Index(iStrmNodeID,iNodeIDs,iNode)
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
        
        !GW nodes corresponding to stream node
        CALL StrmGWConnector%GetGWNodesAtStrmNode(iNode,iGWNodes,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Stream bottom elevation
        AppStream%Nodes(iNode)%BottomElev = DummyArray(2) * FACTLT
        DO indx=1,SIZE(iGWNodes)
            iGWNode           = iGWNodes(indx)
            iLayer            = Stratigraphy%TopActiveLayer(iGWNode)
            AquiferBottomElev = Stratigraphy%BottomElev(iGWNode,iLayer)
            IF (AppStream%Nodes(iNode)%BottomElev .LT. AquiferBottomElev) THEN
                MessageArray(1) = 'Aquifer bottom elevation at a stream node should be'
                MessageArray(2) = 'less than or equal to the stream bed elevation!'
                WRITE (MessageArray(3),'(A,F10.2)') ' Stream node = '//TRIM(IntToText(iStrmNodeID))        //'   Stream bed elevation    = ',AppStream%Nodes(iNode)%BottomElev
                WRITE (MessageArray(4),'(A,F10.2)') ' GW node     = '//TRIM(IntToText(iGWNodeIDs(iGWNode)))//'   Aquifer bottom elevation= ',AquiferBottomElev
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Rating table
        HRTB(1)  = DummyArray(3) * FACTLT
        QRTB(1)  = DummyArray(4) * FACTQ
        WPTB(1)  = DummyArray(5) * FACTLT
        HRTB(2:) = DummyArray2D(:,1) * FACTLT
        QRTB(2:) = DummyArray2D(:,2) * FACTQ
        WPTB(2:) = DummyArray2D(:,3) * FACTLT
        HRTB     = HRTB + AppStream%Nodes(iNode)%BottomElev
        CALL AppStream%Nodes(iNode)%RatingTable%New(NRTB,HRTB,QRTB,iStat)         ;  IF (iStat .EQ. -1) RETURN
        CALL AppStream%RatingTable_WetPerimeter(iNode)%New(NRTB,HRTB,WPTB,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
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
    
    !Clear memory
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
  SUBROUTINE AppStream_v421_WritePreprocessedData(AppStream,OutFile)
    CLASS(AppStream_v421_Type),INTENT(IN) :: AppStream
    TYPE(GenericFileType)                 :: OutFile
    
    !Local variables
    INTEGER                  :: indx   
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
    
    !Write wetted perimeter rating tables
    DO indx=1,AppStream%NStrmNodes
      CALL AppStream%RatingTable_WetPerimeter(indx)%WriteToFile(Outfile)
    END DO
        
  END SUBROUTINE AppStream_v421_WritePreprocessedData

  
  

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
  ! --- CALCULATE STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v421_Simulate(AppStream,GWHeads,Runoff,ReturnFlow,TributaryFlow,DrainInflows,RiparianET,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v421_Type)  :: AppStream
    REAL(8),INTENT(IN)          :: GWHeads(:,:),Runoff(:),ReturnFlow(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    REAL(8),INTENT(OUT)         :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    TYPE(MatrixType)            :: Matrix
   
    !Local variables
    INTEGER                                 :: indxNode,indx,iNode,indxReach,ErrorCode,iNodeIDs_Connect(1),NNodes,NDiver
    REAL(8)                                 :: rInflow,rOutflow,Bypass_Recieved,rUpdateValues(1), &
                                               rValue,rBypassOut,rRipET
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: dFlow_dStage,Inflows,rUpdateRHS,rNetInflows
    REAL(8),ALLOCATABLE                     :: HRG(:)
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
    ALLOCATE (HRG(StrmGWConnector%GetnTotalGWNodes()))
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
        
            !Net inflow
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
    CALL StrmGWConnector%Simulate(NNodes,HRG,AppStream%State%Head,rNetInflows,Matrix,AppStream%RatingTable_WetPerimeter)
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
        
  END SUBROUTINE AppStream_v421_Simulate
      
END MODULE