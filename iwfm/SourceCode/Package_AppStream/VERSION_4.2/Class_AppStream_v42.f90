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
MODULE Class_AppStream_v42
  USE Class_Version                 , ONLY: ReadVersion
  USE Class_BaseAppStream           , ONLY: BaseAppStreamType             , &
                                            PrepareStreamBudgetHeader     
  USE MessageLogger                 , ONLY: SetLastMessage                , &
                                            LogMessage                    , &
                                            EchoProgress                  , &
                                            FILE                          , &
                                            MessageArray                  , &
                                            iFatal                        , &
                                            iWarn                         , &
                                            iMessage
  USE GeneralUtilities              , ONLY: StripTextUntilCharacter       , &
                                            IntToText                     , &
                                            FirstLocation                 , &
                                            CleanSpecialCharacters        , &
                                            EstablishAbsolutePathFileName , &
                                            GetArrayData                  , &
                                            ShellSort
  USE TimeSeriesUtilities
  USE IOInterface
  USE Class_StrmNode
  USE Class_StrmReach
  USE Package_ComponentConnectors
  USE Package_Discretization
  USE Package_Misc                  , ONLY: PairedDataType                , &
                                            FlowDest_Outside              , &
                                            FlowDest_Lake                 , &
                                            FlowDest_StrmNode             , &
                                            iStrmComp
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
  PUBLIC :: AppStream_v42_Type                                              
 
  
  ! -------------------------------------------------------------
  ! --- APPLICATION STREAMS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseAppStreamType) :: AppStream_v42_Type
    PRIVATE
    TYPE(StrmNodeType),ALLOCATABLE :: Nodes(:)
  CONTAINS
    PROCEDURE,PASS :: SetStaticComponent             => AppStream_v42_SetStaticComponent
    PROCEDURE,PASS :: SetStaticComponentFromBinFile  => ReadPreprocessedData
    PROCEDURE,PASS :: SetDynamicComponent            => AppStream_v42_SetDynamicComponent
    PROCEDURE,PASS :: SetAllComponents               => AppStream_v42_SetAllComponents
    PROCEDURE,PASS :: SetAllComponentsWithoutBinFile => AppStream_v42_SetAllComponentsWithoutBinFile
    PROCEDURE,PASS :: GetUpstrmNodes                 => AppStream_v42_GetUpstrmNodes
    PROCEDURE,PASS :: GetStageFlowRatingTable        => AppStream_v42_GetStageFlowRatingTable
    PROCEDURE,PASS :: GetVersion                     => AppStream_v42_GetVersion
    PROCEDURE,PASS :: GetBottomElevations            => AppStream_v42_GetBottomElevations
    PROCEDURE,PASS :: GetNRatingTablePoints          => AppStream_v42_GetNRatingTablePoints
    PROCEDURE,PASS :: KillImplementation             => AppStream_v42_Kill
    PROCEDURE,PASS :: ReadTSData                     => AppStream_v42_ReadTSData
    PROCEDURE,PASS :: WritePreprocessedData          => AppStream_v42_WritePreprocessedData
    PROCEDURE,PASS :: WriteDataToTextFile            => AppStream_v42_WriteDataToTextFile
    PROCEDURE,PASS :: UpdateHeads                    => AppStream_v42_UpdateHeads
    PROCEDURE,PASS :: ConvertTimeUnit                => AppStream_v42_ConvertTimeUnit
    PROCEDURE,PASS :: ConvertFlowToElev              => AppStream_v42_ConvertFlowToElev
    PROCEDURE,PASS :: Simulate                       => AppStream_v42_Simulate
  END TYPE AppStream_v42_Type
    
    
  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iVersion    = 42
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '4.2.0000'
  INCLUDE 'AppStream_v42_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen      = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName         = 'Class_AppStream_v42::'
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
  SUBROUTINE AppStream_v42_SetStaticComponent(AppStream,cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    CLASS(AppStream_v42_Type),INTENT(OUT) :: AppStream
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    TYPE(AppGridType),INTENT(IN)          :: AppGrid         !Not used in this version! Only included to comply with the interface definition.
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    LOGICAL,INTENT(IN)                    :: IsRoutedStreams
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'AppStream_v42_SetStaticComponent'
    CHARACTER                    :: ALine*100
    INTEGER                      :: NRTB,ErrorCode
    TYPE(GenericFileType)        :: DataFile
    
    !Initialzie
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
    CALL DataFile%ReadData(NRTB,iStat)                  ;  IF (iStat .EQ. -1) RETURN
    
    !Make sure that NRTB is greater than 1
    IF (NRTB .LE. 1) THEN
        CALL SetLastMessage('Number of data points in stream rating tables should be greater than 1!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
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
    
    !Read rating tables
    CALL ReadStreamRatingTables(NRTB,Stratigraphy,StrmGWConnector,DataFile,AppStream,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL DataFile%Kill()
    
  END SUBROUTINE AppStream_v42_SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC PART OF STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v42_Type)         :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy      !Not used in this versions
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33) :: ThisProcedure = ModName // 'AppStream_v42_SetDynamicComponent'
    INTEGER                      :: indxNode
    TYPE(GenericFileType)        :: MainFile
    CHARACTER(LEN=1000)          :: ALine,DiverFileName,DiverSpecFileName,BypassSpecFileName,DiverDetailBudFileName
    TYPE(BudgetHeaderType)       :: BudHeader
    CHARACTER(:),ALLOCATABLE     :: cVersionSim,cVersionPre,cAbsPathFileName
    
    !Initailize
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
    
    !Hydrograph printing
    CALL AppStream%StrmHyd%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,AppStream%NStrmNodes,TimeStep,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream budget at selected nodes
    CALL AppStream%StrmNodeBudget%New(AppStream%lRouted,IsForInquiry,cWorkingDirectory,NTIME,TimeStep,AppStream%GetVersion(),PrepareStreamBudgetHeader,MainFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Stream bed parameters for stream-gw connectivity
    CALL StrmGWConnector%CompileConductance(MainFile,AppGrid,AppStream%NStrmNodes,AppStream%Reaches%UpstrmNode,AppStream%Reaches%DownstrmNode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !If non-routed streams, return at this point
    IF (.NOT. AppStream%lRouted) THEN
        CALL MainFile%Kill()
        RETURN
    END IF
    
    !Diversions and bypasses
    CALL AppStream%AppDiverBypass%New(IsForInquiry,DiverSpecFileName,BypassSpecFileName,DiverFileName,DiverDetailBudFileName,AppStream%GetVersion(),NTIME,TimeStep,AppStream%NStrmNodes,AppStream%Reaches,AppGrid,StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN

    !Set the heads to the bottom elevation
    DO indxNode=1,AppStream%NStrmNodes
        AppStream%State(indxNode)%Head   = AppStream%Nodes(indxNode)%BottomElev
        AppStream%State(indxNode)%Head_P = AppStream%State(indxNode)%Head
    END DO
    
    !Close main file
    CALL MainFile%Kill() 
  
  END SUBROUTINE AppStream_v42_SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v42_Type),INTENT(OUT) :: AppStream
    LOGICAL,INTENT(IN)                    :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy      !Not used in this version
    TYPE(GenericFileType)                 :: BinFile
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(StrmGWConnectorType)             :: StrmGWConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'AppStream_v42_SetAllComponents'
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Instantiating streams')
    
    !Read the preprocessed data for streams
    CALL ReadPreprocessedData(AppStream,BinFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Set the dynamic part of AppStream
    CALL AppStream_v42_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
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
    
  END SUBROUTINE AppStream_v42_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStream_v42_Type),INTENT(OUT) :: AppStream
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
    CHARACTER(LEN=ModNameLen+44) :: ThisProcedure = ModName // 'AppStream_v42_SetAllComponentsWithoutBinFile'
    
    !Initialize
    iStat = 0
    
    !Instantiate the static components of the AppStream data
    CALL AppStream_v42_SetStaticComponent(AppStream,cPPFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the dynamic component of the AppStream data
    CALL AppStream_v42_SetDynamicComponent(AppStream,IsForInquiry,cSimFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
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
  
  END SUBROUTINE AppStream_v42_SetAllComponentsWithoutBinFile

  

  
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
  SUBROUTINE AppStream_v42_Kill(AppStream)
    CLASS(AppStream_v42_Type) :: AppStream
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Deallocate array attributes
    DEALLOCATE (AppStream%Nodes , STAT=ErrorCode)
    
  END SUBROUTINE AppStream_v42_Kill
    
  
  
  
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
  FUNCTION AppStream_v42_GetVersion(AppStream) RESULT(cVrs)
    CLASS(AppStream_v42_Type) :: AppStream
    CHARACTER(:),ALLOCATABLE  :: cVrs
    
    IF (.NOT. AppStream%Version%IsDefined())   &
        AppStream%Version = AppStream%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = AppStream%Version%GetVersion()
    
  END FUNCTION AppStream_v42_GetVersion
  

  ! -------------------------------------------------------------
  ! --- GET RATING TABLE (STAGE VS. FLOW) AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_GetStageFlowRatingTable(AppStream,iNode,Stage,Flow)
    CLASS(AppStream_v42_Type),TARGET,INTENT(IN) :: AppStream
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
    
  END SUBROUTINE AppStream_v42_GetStageFlowRatingTable


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF RATING TABLE POINTS AT A STREAM NODE
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v42_GetNRatingTablePoints(AppStream,iStrmNode) RESULT(N)
    CLASS(AppStream_v42_Type),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                   :: iStrmNode
    INTEGER                              :: N
    
    IF (iStrmNode .GT. 0) THEN
      IF (iStrmNode.LE.AppStream%NStrmNodes) N = AppStream%Nodes(iStrmNode)%RatingTable%NPoints
    ELSE
      N = 0
    END IF
    
  END FUNCTION AppStream_v42_GetNRatingTablePoints


  ! -------------------------------------------------------------
  ! --- GET BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_v42_GetBottomElevations(AppStream) RESULT(BottomElev)
    CLASS(AppStream_v42_Type),INTENT(IN) :: AppStream
    REAL(8)                              :: BottomElev(AppStream%NStrmNodes)
    
    BottomElev = AppStream%Nodes%BottomElev
    
  END FUNCTION AppStream_v42_GetBottomElevations
  
  
  ! -------------------------------------------------------------
  ! --- GET NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_GetUpstrmNodes(AppStream,iNode,UpstrmNodes)
    CLASS(AppStream_v42_Type),INTENT(IN)  :: AppStream
    INTEGER,INTENT(IN)                    :: iNode
    INTEGER,ALLOCATABLE,INTENT(OUT)       :: UpstrmNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (UpstrmNodes , STAT=ErrorCode)

    ALLOCATE (UpstrmNodes(AppStream%Nodes(iNode)%NUpstrmNodes))
    UpstrmNodes = AppStream%Nodes(iNode)%UpstrmNodes
    
  END SUBROUTINE AppStream_v42_GetUpstrmNodes
  
  
  

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
    CLASS(AppStream_v42_Type),INTENT(OUT) :: AppStream
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
        CALL SetLastMessage('Error allocating memory for stream data!',iFatal,ThisProcedure)
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
  ! --- READ APPLICATION STREAMS RELATED DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_ReadTSData(AppStream,lDiverAdjusted,TimeStep,iStat,DiversionsOverwrite)
    CLASS(AppStream_v42_Type)     :: AppStream
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
    
  END SUBROUTINE AppStream_v42_ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- READ STREAM REACH CONFIGURATION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadStreamConfigData(DataFile,Stratigraphy,StrmGWConnector,StrmLakeConnector,AppStream,iStat)
    TYPE(GenericFileType)                 :: DataFile
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(AppStream_v42_Type)              :: AppStream
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadStreamConfigData'
    INTEGER                      :: indxReach,DummyIntArray4(4),ID,indxNode,DummyIntArray2(2),     &
                                    DestNode,DestReach,NNodes,iGWNodes(100),iLayers(100),nGWNodes, &
                                    iLoc,indx
    CHARACTER                    :: ALine*2000
    
    !Initailize
    iStat = 0
    
    !Iterate over reaches
    DO indxReach=1,AppStream%NReaches
        ASSOCIATE (pReach => AppStream%Reaches(indxReach))
            CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL GetArrayData(ALine,DummyIntArray4,'stream reach '//TRIM(IntToText(indxReach)),iStat)  ;  IF (iStat .EQ. -1) RETURN
            pReach%cName = ALine(1:20)
            
            !Make sure reach data is entered sequentially
            ID = DummyIntArray4(1)
            IF (ID .NE. indxReach) THEN
                MessageArray(1)='Stream reaches should be entered sequentially!'
                MessageArray(2)='Reach number expected = '//TRIM(IntToText(indxReach))
                MessageArray(3)='Reach number entered  = '//TRIM(IntToText(ID))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Store data in persistent arrays
            pReach%UpstrmNode   = DummyIntArray4(2)
            pReach%DownstrmNode = DummyIntArray4(3)
            IF (DummyIntArray4(4) .GT. 0) THEN
                pReach%OutflowDest  = DummyIntArray4(4)
            ELSEIF (DummyIntArray4(4) .EQ. 0) THEN
                pReach%OutflowDestType = FlowDest_Outside
                pReach%OutflowDest     = 0
            ELSE
                pReach%OutflowDestType = FlowDest_Lake
                pReach%OutflowDest     = -DummyIntArray4(4)
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
            
            !Read the gw nodes
            DO indxNode=pReach%UpstrmNode,pReach%DownstrmNode
                nGWNodes = 1
                
                CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
                CALL GetArrayData(ALine,DummyIntArray2,'groundwater nodes corresponding to stream node '//TRIM(IntToText(indxNode)),iStat)  
                IF (iStat .EQ. -1) RETURN
                
                !Make sure the stream nodes are entered sequentially
                IF (DummyIntArray2(1) .NE. indxNode) THEN
                    MessageArray(1)='Stream nodes for reach '//TRIM(IntToText(indxReach))//' should be entered sequentially!'
                    MessageArray(2)='Stream node expected = '//TRIM(IntToText(indxNode))
                    MessageArray(3)='Stream node entered  = '//TRIM(IntToText(DummyIntArray2(1)))
                    CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure that stream node entered is not larger than the maximum node number
                IF (DummyIntArray2(1) .GT. AppStream%NStrmNodes) THEN
                    MessageArray(1) = 'Stream node number '//TRIM(IntToText(DummyIntArray2(1)))//' in reach '//TRIM(IntToText(indxReach))
                    MessageArray(2) = ' is larger than the maximum stream node number specified ('//TRIM(IntToText(AppStream%NStrmNodes))//')!'
                    CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !First gw node and corresponding layer
                iGWNodes(1) = DummyIntArray2(2)
                iLayers(1)  = Stratigraphy%TopActiveLayer(iGWNodes(1))
                
                !Read the rest of the gw nodes and compile layer information
                DO indx=1,99
                    ALine = ADJUSTL(ALine)
                    IF (LEN_TRIM(ALine) .EQ. 0) EXIT
                    nGWNodes           = nGWNodes + 1
                    READ (ALine,*) iGWNodes(nGWNodes)  ;  iLoc = FirstLocation(' ',ALine)  ;  ALine = ALine(iLoc+1:)
                    iLayers(nGWNodes)  = Stratigraphy%TopActiveLayer(iGWNodes(nGWNodes))
                END DO
                          
                !Store gw nodes and layers 
                CALL StrmGWConnector%AddGWNodesToStrmNode(iVersion,AppStream%NStrmNodes,indxNode,iGWNodes(1:nGWNodes),iLayers(1:nGWNodes),iStat)
                IF (iStat .EQ. -1) RETURN
                
            END DO
          
        END ASSOCIATE
    END DO
    
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
  ! --- READ STREAM RATING TABLES
  ! -------------------------------------------------------------
  SUBROUTINE ReadStreamRatingTables(NRTB,Stratigraphy,StrmGWConnector,DataFile,AppStream,iStat)
    INTEGER,INTENT(IN)                   :: NRTB
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(GenericFileType)                :: DataFile
    TYPE(AppStream_v42_Type)             :: AppStream
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'ReadStreamRatingTables'
    INTEGER                      :: indxNode,iStrmNode,iGWNode,iLayer,ErrorCode,indx
    REAL(8)                      :: FACTLT,FACTQ,HRTB(NRTB),QRTB(NRTB),DummyArray(4),  &
                                    DummyArray2D(NRTB-1,2),AquiferBottomElev
    CHARACTER                    :: ALine*500
    INTEGER,ALLOCATABLE          :: iGWNodes(:)
    
    !Initialize
    iStat = 0
    
    !Read units conversion factors
    CALL DataFile%ReadData(FACTLT,iStat)  ;  IF (iStat .EQ. -1) RETURN    
    CALL DataFile%ReadData(FACTQ,iStat)   ;  IF (iStat .EQ. -1) RETURN     
    CALL DataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    AppStream%TimeUnitRatingTableFlow = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Make sure that time unit of rating tables is recognized
    IF (IsTimeIntervalValid(TRIM(AppStream%TimeUnitRatingTableFlow)) .EQ. 0) THEN
        CALL SetLastMessage('Time unit for the stream rating tables ('//TRIM(AppStream%TimeUnitRatingTableFlow)//') is not recognized!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read rating tables
    DO indxNode=1,AppStream%NStrmNodes
        CALL StrmGWConnector%GetGWNodesAtStrmNode(indxNode,iGWNodes,iStat)
        IF (iStat .EQ. -1) RETURN
        
        CALL DataFile%ReadData(DummyArray,iStat)    ;  IF (iStat .EQ. -1) RETURN
        CALL DataFile%ReadData(DummyArray2D,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
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
        AppStream%Nodes(indxNode)%BottomElev = DummyArray(2) * FACTLT
        DO indx=1,SIZE(iGWNodes)
            iGWNode           = iGWNodes(indx)
            iLayer            = Stratigraphy%TopActiveLayer(iGWNode)
            AquiferBottomElev = Stratigraphy%BottomElev(iGWNode,iLayer)
            IF (AppStream%Nodes(indxNode)%BottomElev .LT. AquiferBottomElev) THEN
                MessageArray(1) = 'Aquifer bottom elevation at a stream node should be'
                MessageArray(2) = 'less than or equal to the stream bed elevation!'
                WRITE (MessageArray(3),'(A,F10.2)') ' Stream node = '//TRIM(IntToText(indxNode)) //'   Stream bed elevation    = ',AppStream%Nodes(indxNode)%BottomElev
                WRITE (MessageArray(4),'(A,F10.2)') ' GW node     = '//TRIM(IntToText(iGWNode))  //'   Aquifer bottom elevation= ',AquiferBottomElev
                CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Rating table
        HRTB(1)  = DummyArray(3) * FACTLT
        QRTB(1)  = DummyArray(4) * FACTQ
        HRTB(2:) = DummyArray2D(:,1) * FACTLT
        QRTB(2:) = DummyArray2D(:,2) * FACTQ
        HRTB     = HRTB + AppStream%Nodes(indxNode)%BottomElev
        CALL AppStream%Nodes(indxNode)%RatingTable%New(NRTB,HRTB,QRTB,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Make sure rating table is specified properly
        DO indx=2,NRTB
            IF (HRTB(indx) .LE. HRTB(indx-1)) THEN
                MessageArray(1) = 'The flow depths specified in the rating table for stream node '//TRIM(IntToText(indxNode))
                MessageArray(2) = 'must monotonically increase!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (QRTB(indx) .LE. QRTB(indx-1)) THEN
                MessageArray(1) = 'The flows specified in the rating table for stream node '//TRIM(IntToText(indxNode))
                MessageArray(2) = 'must monotonically increase!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !If the rating table is problematic, warn the user
        IF (AppStream%Nodes(indxNode)%RatingTable%CheckGradientMonotonicity() .EQ. .FALSE.) THEN
            MessageArray(1) = 'The gradient of the rating table at stream node '//TRIM(IntToText(indxNode))//' is not monotonicaly increasing or decreasing!'
            MessageArray(2) = 'This may lead to problems with the iterative solution!'
            CALL LogMessage(MessageArray(1:2),iWarn,ThisProcedure)
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
  SUBROUTINE AppStream_v42_WritePreprocessedData(AppStream,OutFile)
    CLASS(AppStream_v42_Type),INTENT(IN) :: AppStream
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
        
  END SUBROUTINE AppStream_v42_WritePreprocessedData
  
  
  ! -------------------------------------------------------------
  ! --- WRITE PREPROCESSED DATA TO TEXT FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_WriteDataToTextFile(AppStream,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
    CLASS(AppStream_v42_Type),INTENT(IN)  :: AppStream
    CHARACTER(LEN=*),INTENT(IN)          :: UNITLTOU
    REAL(8),INTENT(IN)                   :: FACTLTOU
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    INTEGER             :: indxReach,indxNode,iGWNode,iLayer,ErrorCode,indx
    REAL(8)             :: GSElev,AquiferBottom,StrmBottom,DELZ,DELA
    INTEGER,ALLOCATABLE :: UpstrmNodes(:),iGWNodes(:)
    CHARACTER           :: ALine*1000
    
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
            CALL StrmGWConnector%GetGWNodesAtStrmNode(indxNode,iGWNodes,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL AppStream_v42_GetUpstrmNodes(AppStream,indxNode,UpstrmNodes)
            StrmBottom = AppStream%Nodes(indxNode)%BottomElev
            DO indx=1,SIZE(iGWNodes)
              iGWNode       = iGWNodes(indx)
              iLayer        = Stratigraphy%TopActiveLayer(iGWNode)
              GSElev        = Stratigraphy%GSElev(iGWNode)
              AquiferBottom = Stratigraphy%BottomElev(iGWNode,iLayer)
              DELZ          = GSElev - StrmBottom
              DELA          = StrmBottom - AquiferBottom
              IF (indx .EQ. 1) THEN
                  WRITE (ALine,'(1X,3I6,5F10.1,5X,10(I4,1X))') indxReach                                 , &
                                                               indxNode                                  , &
                                                               iGWNode                                   , &
                                                               GSElev*FACTLTOU                           , &
                                                               StrmBottom*FACTLTOU                       , &
                                                               DELZ*FACTLTOU                             , &
                                                               AquiferBottom*FACTLTOU                    , &
                                                               DELA*FACTLTOU                             , &
                                                               UpstrmNodes
              ELSE
                  WRITE (ALine,'(13X,I6,5F10.1)')              iGWNode                                   , &
                                                               GSElev*FACTLTOU                           , &
                                                               StrmBottom*FACTLTOU                       , &
                                                               DELZ*FACTLTOU                             , &
                                                               AquiferBottom*FACTLTOU                    , &
                                                               DELA*FACTLTOU                             
              END IF
              CALL LogMessage(TRIM(ALine),iMessage,'',FILE)
            END DO
        END DO
        CALL LogMessage('',iMessage,'',FILE)
    END DO
    
    !Clear memory
    DEALLOCATE (UpstrmNodes , iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE AppStream_v42_WriteDataToTextFile

  
  

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
  SUBROUTINE AppStream_v42_ConvertFlowToElev(AppStream)
    CLASS(AppStream_v42_Type) :: AppStream
    
    !Local variables
    INTEGER :: indxNode
    
    DO indxNode=1,AppStream%NStrmNodes
        AppStream%State(indxNode)%Head = AppStream%Nodes(indxNode)%RatingTable%InverseEvaluate(AppStream%State(indxNode)%Flow)
    END DO
    
  END SUBROUTINE AppStream_v42_ConvertFlowToElev

  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF STREAMS RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_ConvertTimeUnit(AppStream,NewUnit)
    CLASS(AppStream_v42_Type)   :: AppStream
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
    
  END SUBROUTINE AppStream_v42_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_v42_Simulate(AppStream,GWHeads,Runoff,ReturnFlow,TributaryFlow,DrainInflows,RiparianET,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStream_v42_Type)   :: AppStream
    REAL(8),INTENT(IN)          :: GWHeads(:,:),Runoff(:),ReturnFlow(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    REAL(8),INTENT(OUT)         :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    TYPE(MatrixType)            :: Matrix
   
    !Local variables
    INTEGER                                 :: indxNode,indx,iNode,indxReach,NBypass,ErrorCode,iNodeIDs(1),NNodes
    REAL(8)                                 :: rInflow,rOutflow,CoeffDiver,Bypass_Recieved,CoeffBypass,rUpdateValues(1), &
                                               rValue,RHSMin,rDummyArray(1)
    REAL(8),DIMENSION(AppStream%NStrmNodes) :: dFlow_dStage,Inflows,rUpdateRHS,StrmGWFlow_AtMinHead
    LOGICAL                                 :: lDiverMet
    REAL(8),ALLOCATABLE                     :: HRG(:)
    INTEGER,ALLOCATABLE                     :: iStrmIDs(:),iLakeIDs(:)
    INTEGER,PARAMETER                       :: iCompIDs(1)         = iStrmComp , &
                                               iCompIDs_Connect(1) = iStrmComp
    TYPE(PairedDataType)                    :: DummyWetPerimeter(1)
    
    !Inform user about simulation progress
    CALL EchoProgress('Simulating stream flows')
    
    !Initialize
    NNodes = SIZE(GWHeads , DIM=1)
    
    !Get groundwater heads at stream nodes
    ALLOCATE (HRG(StrmGWConnector%GetnTotalGWNodes()))
    CALL StrmGWConnector%GetGWHeadsAtStrmNodes(GWHeads,HRG)
       
    !Simulate stream-gw interaction
    CALL StrmGWConnector%Simulate(NNodes,HRG,AppStream%State%Head,AppStream%Nodes%BottomElev,DummyWetPerimeter,Matrix)
    
    !Stream-gw interaction at minimum stream head (= stream bottom elevation)
    CALL StrmGWConnector%ComputeStrmGWFlow_AtMinHead(StrmGWFlow_AtMinHead,HRG,rDummyArray,AppStream%Nodes%BottomElev,DummyWetPerimeter)
    
    !Initialize
    NBypass = AppStream%AppDiverBypass%NBypass
    Inflows = AppStream%StrmInflowData%GetInflows(AppStream%NStrmNodes)
    CALL StrmLakeConnector%ResetStrmToLakeFlows()
    
    !Compute stream flows based on rating table
    dFlow_dStage = 0.0
    DO indxNode=1,AppStream%NStrmNodes
        ASSOCIATE (pRatingTable => AppStream%Nodes(indxNode)%RatingTable , &
                   pState       => AppStream%State(indxNode)             )
            CALL pRatingTable%EvaluateAndDerivative(pState%Head,pState%Flow,dFlow_dStage(indxNode))
            pState%Flow = MAX(pState%Flow , 0.0)
        END ASSOCIATE
    END DO
    
    !Initialize bypass flows to zero
    AppStream%AppDiverBypass%Bypasses%Bypass_Out      = 0.0
    AppStream%AppDiverBypass%Bypasses%Bypass_Recieved = 0.0

    !Update the matrix equation
    DO indxReach=1,AppStream%NReaches
        DO indxNode=AppStream%Reaches(indxReach)%UpstrmNode,AppStream%Reaches(indxReach)%DownstrmNode
          
            !Initialize
            rInflow  = 0.0
            rOutflow = 0.0
            
            !Recieved bypass
            Bypass_Recieved = AppStream%AppDiverBypass%GetBypassRecieved(FlowDest_StrmNode,indxNode)
            
            !Inflows at the stream node with known values
            rInflow = Inflows(indxNode)                                       &    !Inflow as defined by the user
                    + Runoff(indxNode)                                        &    !Direct runoff of precipitation 
                    + ReturnFlow(indxNode)                                    &    !Return flow of applied water 
                    + TributaryFlow(indxNode)                                 &    !Tributary inflows from small watersheds and creeks
                    + DrainInflows(indxNode)                                  &    !Inflow from tile drains
                    + Bypass_Recieved                                         &    !Received by-pass flows 
                    + StrmLakeConnector%GetFlow(iLakeToStrmType,indxNode)          !Flows from lake outflow
            
            !Inflows from upstream nodes
            DO indx=1,AppStream%Nodes(indxNode)%NUpstrmNodes
                iNode   = AppStream%Nodes(indxNode)%UpstrmNodes(indx)
                rInflow = rInflow + AppStream%State(iNode)%Flow
            END DO
            
            !Diversion
            lDiverMet = .TRUE.
            IF (AppStream%AppDiverBypass%NDiver .GT. 0) THEN
              rOutflow                                            = MIN(rInflow , AppStream%AppDiverBypass%NodalDiverRequired(indxNode))
              AppStream%AppDiverBypass%NodalDiverActual(indxNode) = rOutflow
              IF (rOutflow .LT. AppStream%AppDiverBypass%NodalDiverRequired(indxNode)) lDiverMet = .FALSE.
            END IF
            
            !Bypass
            CALL AppStream%AppDiverBypass%ComputeBypass(indxNode,rInflow,rOutflow,StrmLakeConnector,lDiverMet,CoeffBypass)
        
            !Riparian ET outflow
            IF (RiparianET(indxNode) .GT. 0.0) THEN
                RiparianETFrac(indxNode) = MIN(RiparianET(indxNode) , rInflow-rOutflow) / RiparianET(indxNode)
                rOutflow = rOutflow + RiparianET(indxNode) * RiparianETFrac(indxNode)
            ELSE
                RiparianETFrac(indxNode) = 0.0
            END IF
        
            !Compute the matrix rhs function and its derivatives w.r.t. stream elevation
            !----------------------------------------------------------------------------
            
            !RHS function at minimum stream head
            RHSMin = -rInflow + rOutflow + StrmGWFlow_AtMinHead(indxNode)
            RHSMin = MAX(RHSMin , 0.0)
            
            !RHS function
            rUpdateRHS(indxNode) = AppStream%State(indxNode)%Flow - rInflow + rOutflow - RHSMin
            
            !Derivative of function w.r.t. stream elevation
            iNodeIDs(1)      = indxNode
            rUpdateValues(1) = dFlow_dStage(indxNode)
            CALL Matrix%UpdateCOEFF(iStrmComp,indxNode,iCompIDs_Connect,iNodeIDs,rUpdateValues)
             
            !Derivative of function w.r.t. stream elevation at upstream nodes that directly feed into stream node in consideration
            CoeffDiver  = 0.0
            IF (lDiverMet .EQ. .FALSE.) CoeffDiver = 1.0
            DO indx=1,AppStream%Nodes(indxNode)%NUpstrmNodes
                iNode  = AppStream%Nodes(indxNode)%UpstrmNodes(indx)
                rValue = (CoeffDiver+CoeffBypass-1.0) * dFlow_dStage(iNode) 
                CALL Matrix%SetCOEFF(iStrmComp,indxNode,iStrmComp,iNode,rValue)
            END DO
            
        END DO
    END DO

    !Update RHS vector
    CALL Matrix%UpdateRHS(iStrmComp,1,rUpdateRHS)
    
    !Stream flows to lakes
    CALL StrmLakeConnector%GetSourceIDs(iStrmToLakeType,iStrmIDs)
    CALL StrmLakeConnector%GetDestinationIDs(iStrmToLakeType,iLakeIDs)
    DO indxNode=1,SIZE(iStrmIDs)
      CALL StrmLakeConnector%SetFlow(iStrmToLakeType,iStrmIDs(indxNode),iLakeIDs(indxNode),AppStream%State(iStrmIDs(indxNode))%Flow)
    END DO

    !Simulate diversion related flows
    CALL AppStream%AppDiverBypass%ComputeDiversions(AppStream%NStrmNodes)
    
    !Clear memory
    DEALLOCATE (iStrmIDs , iLakeIDs , STAT=ErrorCode)
        
  END SUBROUTINE AppStream_v42_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE LIST OF NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  SUBROUTINE CompileUpstrmNodes(AppStream)
    TYPE(AppStream_v42_Type) :: AppStream
    
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
  SUBROUTINE AppStream_v42_UpdateHeads(AppStream,HDelta)
    CLASS(AppStream_v42_Type) :: AppStream
    REAL(8),INTENT(IN)        :: HDelta(:)
    
    AppStream%State%Head = MAX(AppStream%State%Head-HDelta  ,  AppStream%Nodes%BottomElev)

  END SUBROUTINE AppStream_v42_UpdateHeads
   
  
END MODULE