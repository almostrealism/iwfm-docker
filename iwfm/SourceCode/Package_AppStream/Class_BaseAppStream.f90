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
MODULE Class_BaseAppStream
  USE Class_Version               , ONLY: VersionType
  USE MessageLogger               , ONLY: EchoProgress                           , &
                                          SetLastMessage                         , &
                                          f_iWarn                                , &
                                          f_iFatal
  USE IOInterface                 , ONLY: GenericFileType                        
  USE GeneralUtilities            , ONLY: StripTextUntilCharacter                , &
                                          ArrangeText                            , &
                                          IntToText                              , &
                                          UpperCase                              , &
                                          ConvertID_To_Index
  USE TimeSeriesUtilities         , ONLY: TimeStepType                           , &
                                          IncrementTimeStamp                     
  USE Package_Misc                , ONLY: f_iFlowDest_Outside                    , &
                                          f_iFlowDest_Element                    , &
                                          f_iFlowDest_Subregion                  , &
                                          f_iStrmComp                            , &
                                          f_iLocationType_StrmReach              , &
                                          f_iLocationType_StrmNode               , &
                                          f_iLocationType_StrmHydObs             , &
                                          f_iLocationType_Bypass                 , &
                                          f_iLocationType_Diversion              , &
                                          f_iLocationType_StrmNodeBud            , &
                                          f_iAllLocationIDsListed                
  USE Package_Discretization      , ONLY: StratigraphyType                       , &
                                          AppGridType
  USE Package_ComponentConnectors , ONLY: StrmLakeConnectorType                  , &
                                          f_iLakeToStrmFlow                      , &
                                          StrmGWConnectorType                    , &
                                          SupplyType                             , &
                                          SupplyDestinationConnectorType         , &
                                          SupplyToDestinationType                , &
                                          Supply_GetSupply                       , &
                                          Supply_SetSupplySpecs                  , & 
                                          Supply_ResetIrigFracs                  , &
                                          Supply_CheckSupplyDestinationConnection
  USE Package_Matrix              , ONLY: MatrixType                             , &
                                          ConnectivityListType
  USE Package_Budget              , ONLY: BudgetType                             , &
                                          BudgetHeaderType                       , &
                                          f_iColumnHeaderLen                     , &
                                          f_cVolumeUnitMarker                    , &
                                          f_cLocationNameMarker                  , &
                                          f_iMaxLocationNameLen                  , &
                                          f_iVR                                  , &
                                          f_iPER_CUM
  USE Class_StrmReach             , ONLY: StrmReachType                          , &
                                          StrmReach_IsUpstreamNode               , &
                                          StrmReach_DestinationIDs_To_Indices    , &
                                          StrmReach_ID_To_Index                  , &
                                          StrmReach_GetNReaches_InUpstrmNetwork  , &
                                          StrmReach_GetReaches_InUpstrmNetwork
  USE Class_StrmState             , ONLY: StrmStateType
  USE Class_StrmInflow            , ONLY: StrmInflowType
  USE Class_StrmNodeBudget        , ONLY: StrmNodeBudgetType
  USE StrmHydrograph              , ONLY: StrmHydrographType                     , &
                                          iHydFlow                               , &
                                          iHydStage                              , &
                                          iHydBoth
  USE Class_ElemToRecvLoss        , ONLY: ElemToRecvLoss_GetElems  
  USE Class_AppDiverBypass        , ONLY: AppDiverBypassType                     , &
                                          f_iDiverRecvLoss                       , &
                                          f_iBypassRecvLoss                      , &
                                          f_iAllRecvLoss
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: BaseAppStreamType         , &
            PrepareStreamBudgetHeader , &
            CalculateNStrmNodes       , &
            ReadFractionsForGW        , &
            f_iBudgetType_StrmNode    , &
            f_iBudgetType_StrmReach   , &
            f_iBudgetType_DiverDetail 
  

  ! -------------------------------------------------------------
  ! --- ABSTRACT BASE APPLICATION STREAM DATA TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: BaseAppStreamType
      TYPE(VersionType)               :: Version                                             !Stream component version number
      LOGICAL                         :: lRouted                      = .TRUE.               !Flag to check if stream flows are actually routed (used when stream flows are recieved from another model)
      INTEGER                         :: NStrmNodes                   = 0                    !Number of stream nodes
      INTEGER                         :: NReaches                     = 0                    !Number of reaches
      CHARACTER(LEN=6)                :: TimeUnitRatingTableFlow      = ''                   !Time unit of flow in flow vs. head rating table
      TYPE(StrmReachType),ALLOCATABLE :: Reaches(:)                                          !Stream reach data
      TYPE(StrmStateType),ALLOCATABLE :: State(:)                                            !Stream head and flows at each node
      TYPE(AppDiverBypassType)        :: AppDiverBypass                                      !Diversion and bypass related data
      TYPE(StrmInflowType)            :: StrmInflowData                                      !Stream inflow boundary conditions
      TYPE(StrmNodeBudgetType)        :: StrmNodeBudget                                      !Water budget output at selected nodes
      LOGICAL                         :: StrmReachBudRawFile_Defined  = .FALSE.              !Flag to check if the stream reach budget output is defined
      TYPE(BudgetType)                :: StrmReachBudRawFile                                 !Stream reach budget binary output
      INTEGER,ALLOCATABLE             :: iPrintReachBudgetOrder(:)                           !Reaches ordered w.r.t. ID numbers for budget printing  
      TYPE(StrmHydrographType)        :: StrmHyd                                             !Output for stream hydrograph
  CONTAINS
      PROCEDURE(Abstract_SetStaticComponent),PASS,DEFERRED             :: SetStaticComponent
      PROCEDURE(Abstract_SetStaticComponentFromBinFile),PASS,DEFERRED  :: SetStaticComponentFromBinFile
      PROCEDURE(Abstract_SetAllComponents),PASS,DEFERRED               :: SetAllComponents
      PROCEDURE(Abstract_SetAllComponentsWithoutBinFile),PASS,DEFERRED :: SetAllComponentsWithoutBinFile
      PROCEDURE(Abstract_SetDynamicComponent),PASS,DEFERRED            :: SetDynamicComponent
      PROCEDURE(Abstract_GetStrmNodeIDs),PASS,DEFERRED                 :: GetStrmNodeIDs
      PROCEDURE(Abstract_GetStrmNodeID),PASS,DEFERRED                  :: GetStrmNodeID
      PROCEDURE(Abstract_GetStrmNodeIndex),PASS,DEFERRED               :: GetStrmNodeIndex
      PROCEDURE(Abstract_GetStageFlowRatingTable),PASS,DEFERRED        :: GetStageFlowRatingTable
      PROCEDURE(Abstract_GetVersion),PASS,DEFERRED                     :: GetVersion
      PROCEDURE(Abstract_GetBottomElevations),PASS,DEFERRED            :: GetBottomElevations
      PROCEDURE(Abstract_GetNRatingTablePoints),PASS,DEFERRED          :: GetNRatingTablePoints
      PROCEDURE(Abstract_GetNUpstrmNodes),PASS,DEFERRED                :: GetNUpstrmNodes
      PROCEDURE(Abstract_GetUpstrmNodes),PASS,DEFERRED                 :: GetUpstrmNodes
      PROCEDURE(Abstract_KillImplementation),PASS,DEFERRED             :: KillImplementation
      PROCEDURE(Abstract_WritePreprocessedData),PASS,DEFERRED          :: WritePreprocessedData
      PROCEDURE(Abstract_WriteDataToTextFile),PASS,DEFERRED            :: WriteDataToTextFile
      PROCEDURE(Abstract_UpdateHeads),PASS,DEFERRED                    :: UpdateHeads
      PROCEDURE(Abstract_ConvertTimeUnit),PASS,DEFERRED                :: ConvertTimeUnit
      PROCEDURE(Abstract_ConvertFlowToElev),PASS,DEFERRED              :: ConvertFlowToElev
      PROCEDURE(Abstract_Simulate),PASS,DEFERRED                       :: Simulate
      PROCEDURE,PASS                                                   :: Kill
      PROCEDURE,PASS                                                   :: GetBudget_List
      PROCEDURE,PASS                                                   :: GetBudget_NColumns
      PROCEDURE,PASS                                                   :: GetBudget_ColumnTitles
      PROCEDURE,PASS                                                   :: GetBudget_MonthlyFlows_GivenAppStream
      PROCEDURE,NOPASS                                                 :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS                                                   :: GetBudget_TSData
      PROCEDURE,PASS                                                   :: GetHydrographTypeList
      PROCEDURE,PASS                                                   :: GetHydOutputFileName
      PROCEDURE,PASS                                                   :: GetNStrmNodes_WithBudget
      PROCEDURE,PASS                                                   :: GetStrmNodeIDs_WithBudget
      PROCEDURE,PASS                                                   :: GetNStrmNodes
      PROCEDURE,PASS                                                   :: GetNReaches
      PROCEDURE,PASS                                                   :: GetReachIDs
      PROCEDURE,PASS                                                   :: GetReachIndex
      PROCEDURE,PASS                                                   :: GetReachNNodes
      PROCEDURE,PASS                                                   :: GetInflow_AtANode
      PROCEDURE,PASS                                                   :: GetInflows_AtSomeNodes
      PROCEDURE,PASS                                                   :: GetInflows_AtSomeInflows
      PROCEDURE,PASS                                                   :: GetNInflows
      PROCEDURE,PASS                                                   :: GetInflowNodes
      PROCEDURE,PASS                                                   :: GetInflowIDs
      PROCEDURE,PASS                                                   :: GetFlow
      PROCEDURE,PASS                                                   :: GetFlows
      PROCEDURE,PASS                                                   :: GetHeads
      PROCEDURE,PASS                                                   :: GetHead_AtOneNode
      PROCEDURE,PASS                                                   :: GetNDiver
      PROCEDURE,PASS                                                   :: GetDiversionIDs
      PROCEDURE,PASS                                                   :: GetDiversionPurpose
      PROCEDURE,PASS                                                   :: GetDeliveryAtDiversion
      PROCEDURE,PASS                                                   :: GetDiversionsForDeliveries
      PROCEDURE,PASS                                                   :: GetActualDiversions_AtSomeDiversions
      PROCEDURE,PASS                                                   :: GetActualDiversions_AtSomeNodes
      PROCEDURE,PASS                                                   :: GetDiversionsExportNodes
      PROCEDURE,PASS                                                   :: GetNBypass
      PROCEDURE,PASS                                                   :: GetBypassIDs
      PROCEDURE,PASS                                                   :: GetBypassDiverOriginDestData
      PROCEDURE,PASS                                                   :: GetBypassReceived_FromABypass
      PROCEDURE,PASS                                                   :: GetNetBypassInflows
      PROCEDURE,PASS                                                   :: GetStrmBypassInflows
      PROCEDURE,PASS                                                   :: GetBypassOutflows
      PROCEDURE,PASS                                                   :: GetReachDownstrmNode
      PROCEDURE,PASS                                                   :: GetReachUpstrmNode
      PROCEDURE,PASS                                                   :: GetReachOutflowDestType
      PROCEDURE,PASS                                                   :: GetReachOutflowDest
      PROCEDURE,PASS                                                   :: GetReachStrmNodes
      PROCEDURE,PASS                                                   :: GetReaches_ForStrmNodes
      PROCEDURE,PASS                                                   :: GetNReaches_InUpstrmNetwork
      PROCEDURE,PASS                                                   :: GetReaches_InUpstrmNetwork
      PROCEDURE,PASS                                                   :: GetReachNUpstrmReaches
      PROCEDURE,PASS                                                   :: GetReachUpstrmReaches
      PROCEDURE,PASS                                                   :: GetNames
      PROCEDURE,PASS                                                   :: GetUpstrmNodeFlags
      PROCEDURE,PASS                                                   :: GetStrmConnectivityInGWNodes
      PROCEDURE,PASS                                                   :: GetiColAdjust
      PROCEDURE,PASS                                                   :: GetSupplyAdjustData
      PROCEDURE,PASS                                                   :: GetSupply
      PROCEDURE,PASS                                                   :: GetSupplySpecs
      PROCEDURE,PASS                                                   :: GetMaxDiversionRank
      PROCEDURE,PASS                                                   :: GetElemRecvLosses
      PROCEDURE,PASS                                                   :: GetSubregionalRecvLosses
      PROCEDURE,PASS                                                   :: GetElemsWithRecvLoss
      PROCEDURE,PASS                                                   :: SetSupplySpecs
      PROCEDURE,PASS                                                   :: SetIrigFracsRead
      PROCEDURE,PASS                                                   :: SetStreamFlow
      PROCEDURE,PASS                                                   :: SetStreamInflow
      PROCEDURE,PASS                                                   :: SetBypassFlows_AtABypass
      PROCEDURE,PASS                                                   :: SetDiversionRead
      PROCEDURE,PASS                                                   :: ReadTSData
      PROCEDURE,PASS                                                   :: ResetIrigFracs
      PROCEDURE,PASS                                                   :: ReadRestartData
      PROCEDURE,PASS                                                   :: PrintResults
      PROCEDURE,PASS                                                   :: PrintRestartData
      PROCEDURE,PASS                                                   :: CheckSupplyDestinationConnection
      PROCEDURE,PASS                                                   :: ResetHeads
      PROCEDURE,PASS                                                   :: AdvanceState
      PROCEDURE,PASS                                                   :: IsDiversionToModelDomain
      PROCEDURE,PASS                                                   :: IsUpstreamNode
      PROCEDURE,PASS                                                   :: RegisterWithMatrix
      PROCEDURE,PASS                                                   :: TransferOutputToHDF
      PROCEDURE,PASS                                                   :: DestinationIDs_To_Indices
      PROCEDURE,PASS                                                   :: AddBypass
      GENERIC                                                          :: New                     => SetStaticComponent                       , &
                                                                                                     SetStaticComponentFromBinFile            , &
                                                                                                     SetAllComponents                         , &
                                                                                                     SetAllComponentsWithoutBinFile           , &
                                                                                                     SetDynamicComponent                      
  END TYPE BaseAppStreamType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iBudgetType_StrmNode           = f_iStrmComp*1000 + 1 , &
                                 f_iBudgetType_StrmReach          = f_iStrmComp*1000 + 2 , &
                                 f_iBudgetType_DiverDetail        = f_iStrmComp*1000 + 3
  CHARACTER(LEN=18),PARAMETER :: f_cDescription_StrmNodeBudget    = 'Stream node budget'
  CHARACTER(LEN=19),PARAMETER :: f_cDescription_StrmReachBudget   = 'Stream reach budget'
  CHARACTER(LEN=23),PARAMETER :: f_cDescription_DiverDetailBudget = 'Diversion detail budget'
  CHARACTER(LEN=24),PARAMETER :: f_cDescription_StrmHyd_Flow      = 'Stream hydrograph (flow)'
  CHARACTER(LEN=25),PARAMETER :: f_cDescription_StrmHyd_Stage     = 'Stream hydrograph (stage)'

  
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iNStrmBudColumns = 14
  CHARACTER(LEN=30),PARAMETER :: f_cBudgetColumnTitles(f_iNStrmBudColumns) = ['Upstream Inflow (+)'             , &
                                                                              'Downstream Outflow (-)'          , &
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
  INTEGER,PARAMETER                   :: ModNameLen = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName = 'Class_BaseAppStream::' 
  
  
  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE

     SUBROUTINE Abstract_SetStaticComponent(AppStream,cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
        IMPORT                                :: BaseAppStreamType,AppGridType,StratigraphyType,StrmGWConnectorType,StrmLakeConnectorType
        CLASS(BaseAppStreamType),INTENT(OUT)  :: AppStream
        CHARACTER(LEN=*),INTENT(IN)           :: cFileName
        TYPE(AppGridType),INTENT(IN)          :: AppGrid
        TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
        LOGICAL,INTENT(IN)                    :: IsRoutedStreams
        TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
        TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
        INTEGER,INTENT(OUT)                   :: iStat
     END SUBROUTINE Abstract_SetStaticComponent
     
     
     SUBROUTINE Abstract_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
        IMPORT                            :: BaseAppStreamType,TimeStepType,AppGridType,StratigraphyType,StrmLakeConnectorType,StrmGWConnectorType
        CLASS(BaseAppStreamType)          :: AppStream
        LOGICAL,INTENT(IN)                :: IsForInquiry
        CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
        TYPE(TimeStepType),INTENT(IN)     :: TimeStep
        INTEGER,INTENT(IN)                :: NTIME,iLakeIDs(:)
        TYPE(AppGridType),INTENT(IN)      :: AppGrid
        TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
        TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
        TYPE(StrmGWConnectorType)         :: StrmGWConnector
        INTEGER,INTENT(OUT)               :: iStat
     END SUBROUTINE Abstract_SetDynamicComponent

     
     SUBROUTINE Abstract_SetStaticComponentFromBinFile(AppStream,BinFile,iStat)
        IMPORT                               :: BaseAppStreamType,GenericFileType
        CLASS(BaseAppStreamType),INTENT(OUT) :: AppStream
        TYPE(GenericFileType)                :: BinFile
        INTEGER,INTENT(OUT)                  :: iStat
     END SUBROUTINE Abstract_SetStaticComponentFromBinFile
     
    
     SUBROUTINE Abstract_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
        IMPORT                               :: BaseAppStreamType,TimeStepType,StratigraphyType,AppGridType,GenericFileType,StrmLakeConnectorType,StrmGWConnectorType
        CLASS(BaseAppStreamType),INTENT(OUT) :: AppStream
        LOGICAL,INTENT(IN)                   :: IsForInquiry
        CHARACTER(LEN=*),INTENT(IN)          :: cFileName,cSimWorkingDirectory
        TYPE(TimeStepType),INTENT(IN)        :: TimeStep
        INTEGER,INTENT(IN)                   :: NTIME,iLakeIDs(:)
        TYPE(AppGridType),INTENT(IN)         :: AppGrid
        TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
        TYPE(GenericFileType)                :: BinFile
        TYPE(StrmLakeConnectorType)          :: StrmLakeConnector
        TYPE(StrmGWConnectorType)            :: StrmGWConnector
        INTEGER,INTENT(OUT)                  :: iStat
     END SUBROUTINE Abstract_SetAllComponents

     
     SUBROUTINE Abstract_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
        IMPORT                                :: BaseAppStreamType,TimeStepType,StratigraphyType,AppGridType,StrmLakeConnectorType,StrmGWConnectorType
        CLASS(BaseAppStreamType),INTENT(OUT)  :: AppStream
        LOGICAL,INTENT(IN)                    :: IsRoutedStreams,IsForInquiry
        CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSimFileName,cSimWorkingDirectory
        TYPE(AppGridType),INTENT(IN)          :: AppGrid
        TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
        TYPE(TimeStepType),INTENT(IN)         :: TimeStep
        INTEGER,INTENT(IN)                    :: NTIME,iLakeIDs(:)
        TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
        TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
        INTEGER,INTENT(OUT)                   :: iStat
     END SUBROUTINE Abstract_SetAllComponentsWithoutBinFile
     
     
     PURE SUBROUTINE Abstract_GetStrmNodeIDs(AppStream,iStrmNodeIDs)
        IMPORT                              :: BaseAppStreamType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        INTEGER,INTENT(OUT)                 :: iStrmNodeIDs(:)
     END SUBROUTINE Abstract_GetStrmNodeIDs
     
     
     PURE FUNCTION Abstract_GetStrmNodeID(AppStream,indx) RESULT(iStrmNodeID)
        IMPORT                              :: BaseAppStreamType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        INTEGER,INTENT(IN)                  :: indx
        INTEGER                             :: iStrmNodeID
     END FUNCTION Abstract_GetStrmNodeID
     
     
     PURE FUNCTION Abstract_GetStrmNodeIndex(AppStream,ID) RESULT(Index)
        IMPORT                              :: BaseAppStreamType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        INTEGER,INTENT(IN)                  :: ID
        INTEGER                             :: Index
     END FUNCTION Abstract_GetStrmNodeIndex
     
     
     SUBROUTINE Abstract_GetStageFlowRatingTable(AppStream,iNode,Stage,Flow)
        IMPORT                                     :: BaseAppStreamType
        CLASS(BaseAppStreamType),TARGET,INTENT(IN) :: AppStream
        INTEGER,INTENT(IN)                         :: iNode
        REAL(8),INTENT(OUT)                        :: Stage(:),Flow(:)
     END SUBROUTINE Abstract_GetStageFlowRatingTable
     
     
     PURE FUNCTION Abstract_GetNRatingTablePoints(AppStream,iStrmNode) RESULT(N)
        IMPORT                              :: BaseAppStreamType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        INTEGER,INTENT(IN)                  :: iStrmNode
        INTEGER                             :: N
     END FUNCTION Abstract_GetNRatingTablePoints
     
     
     PURE FUNCTION Abstract_GetBottomElevations(AppStream) RESULT(BottomElev)
        IMPORT                              :: BaseAppStreamType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        REAL(8)                             :: BottomElev(AppStream%NStrmNodes)
     END FUNCTION Abstract_GetBottomElevations
     
     
     FUNCTION Abstract_GetVersion(AppStream) RESULT(cVrs)
        IMPORT                   :: BaseAppStreamType
        CLASS(BaseAppStreamType) :: AppStream
        CHARACTER(:),ALLOCATABLE :: cVrs
     END FUNCTION Abstract_GetVersion
     
     
     FUNCTION Abstract_GetNUpstrmNodes(AppStream,iStrmNode) RESULT(iNNodes)
        IMPORT                              :: BaseAppStreamType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        INTEGER,INTENT(IN)                  :: iStrmNode
        INTEGER                             :: iNNodes
     END FUNCTION Abstract_GetNUpstrmNodes
     
     
     SUBROUTINE Abstract_GetUpstrmNodes(AppStream,iNode,UpstrmNodes)
        IMPORT                              :: BaseAppStreamType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        INTEGER,INTENT(IN)                  :: iNode
        INTEGER,ALLOCATABLE,INTENT(OUT)     :: UpstrmNodes(:)
     END SUBROUTINE Abstract_GetUpstrmNodes
     
     
     SUBROUTINE Abstract_KillImplementation(AppStream)
        IMPORT                   :: BaseAppStreamType
        CLASS(BaseAppStreamType) :: AppStream
     END SUBROUTINE Abstract_KillImplementation
          

     SUBROUTINE Abstract_WritePreprocessedData(AppStream,OutFile)
        IMPORT                              :: BaseAppStreamType,GenericFileType
        CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
        TYPE(GenericFileType)               :: OutFile
     END SUBROUTINE Abstract_WritePreprocessedData
     
     
     SUBROUTINE Abstract_WriteDataToTextFile(AppStream,iGWNodeIDs,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
        IMPORT                               :: BaseAppStreamType,StratigraphyType,StrmGWConnectorType
        CLASS(BaseAppStreamType),INTENT(IN)  :: AppStream
        INTEGER,INTENT(IN)                   :: iGWNodeIDs(:)
        CHARACTER(LEN=*),INTENT(IN)          :: UNITLTOU
        REAL(8),INTENT(IN)                   :: FACTLTOU
        TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
        TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
        INTEGER,INTENT(OUT)                  :: iStat
     END SUBROUTINE Abstract_WriteDataToTextFile
    
     
     SUBROUTINE Abstract_UpdateHeads(AppStream,HDelta)
        IMPORT                   :: BaseAppStreamType
        CLASS(BAseAppStreamType) :: AppStream
        REAL(8),INTENT(IN)       :: HDelta(:)
     END SUBROUTINE Abstract_UpdateHeads
     
     
     SUBROUTINE Abstract_ConvertTimeUnit(AppStream,NewUnit)
        IMPORT                      :: BaseAppStreamType
        CLASS(BaseAppStreamType)    :: AppStream
        CHARACTER(LEN=*),INTENT(IN) :: NewUnit
     END SUBROUTINE Abstract_ConvertTimeUnit
     
     
     SUBROUTINE Abstract_ConvertFlowToElev(AppStream)
        IMPORT                      :: BaseAppStreamType
        CLASS(BaseAppStreamType)    :: AppStream
     END SUBROUTINE Abstract_ConvertFlowToElev
     
     
     SUBROUTINE Abstract_Simulate(AppStream,GWHeads,Runoff,ReturnFlow,TributaryFlow,DrainInflows,RiparianET,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
        IMPORT                      :: BaseAppStreamType,StrmGWConnectorType,StrmLakeConnectorType,MatrixType
        CLASS(BaseAppStreamType)    :: AppStream
        REAL(8),INTENT(IN)          :: GWHeads(:,:),Runoff(:),ReturnFlow(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
        REAL(8),INTENT(OUT)         :: RiparianETFrac(:)
        TYPE(StrmGWConnectorType)   :: StrmGWConnector
        TYPE(StrmLakeConnectorType) :: StrmLakeConnector
        TYPE(MatrixType)            :: Matrix
     END SUBROUTINE Abstract_Simulate
     
     
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
  ! --- KILL AppStream OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppStream)
    CLASS(BaseAppStreamType) :: AppStream
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Kill components
    CALL AppStream%Version%Kill()
    DEALLOCATE(AppStream%Reaches , AppStream%State , STAT=ErrorCode)
    CALL AppStream%StrmInflowData%Kill()
    CALL AppStream%AppDiverBypass%Kill()
    CALL AppStream%StrmNodeBudget%Kill()
    CALL AppStream%StrmHyd%Kill()
    IF (AppStream%StrmReachBudRawFile_Defined) CALL AppStream%StrmReachBudRawFile%Kill()            
    
    !Kill specific implementation of AppStream 
    CALL AppStream%KillImplementation()
    
    !Assign attribute defaults
    AppStream%lRouted                     = .TRUE.
    AppStream%NStrmNodes                  = 0                
    AppStream%NReaches                    = 0                  
    AppStream%TimeUnitRatingTableFlow     = ''    
    AppStream%StrmReachBudRawFile_Defined = .FALSE.
    
    
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
  ! --- GET NUMBER OF STREAM NODES WITH BUDGET OUTPUT 
  ! -------------------------------------------------------------
  FUNCTION GetNStrmNodes_WithBudget(AppStream) RESULT(iNStrmNodeBud)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER                             :: iNStrmNodeBud
    
    !Local variables
    INTEGER             :: iErrorCode
    INTEGER,ALLOCATABLE :: iBudNodes(:)
    
    CALL AppStream%StrmNodeBudget%GetBudNodes(iBudNodes)
    iNStrmNodeBud = SIZE(iBudNodes)
    
    DEALLOCATE (iBudNodes , STAT=iErrorCode)
    
  END FUNCTION GetNStrmNodes_WithBudget
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDs WITH BUDGET OUTPUT 
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmNodeIDs_WithBudget(AppStream,iIDs)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,ALLOCATABLE,INTENT(OUT)     :: iIDs(:)
    
    !Local variables
    INTEGER :: indx
    
    !Get stream node indices
    CALL AppStream%StrmNodeBudget%GetBudNodes(iIDs)
    
    !Convert stream node indices to IDs
    DO indx=1,SIZE(iIDs)
        iIDs(indx) = AppStream%GetStrmNodeID(iIDs(indx))
    END DO
    
  END SUBROUTINE GetStrmNodeIDs_WithBudget
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(AppStream,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(BaseAppStreamType),INTENT(IN)      :: AppStream
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    !Local variables
    INTEGER                  :: iCount,iErrorCode,iTypeList(3),iLocationList(3)
    CHARACTER(LEN=500)       :: cFiles(3)
    CHARACTER(LEN=100)       :: cDescription(3)
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    iCount = 0
    DEALLOCATE (iBudgetTypeList , iBudgetLocationTypeList , cBudgetDescriptions , cBudgetFiles , STAT=iErrorCode)
         
    !Stream node budget
    IF (AppStream%StrmNodeBudget%StrmNodeBudRawFile_Defined) THEN
        CALL AppStream%StrmNodeBudget%StrmNodeBudRawFile%GetFileName(cFileName)
        cFiles(iCount+1)        = cFileName
        iTypeList(iCount+1)     = f_iBudgetType_StrmNode
        iLocationList(iCount+1) = f_iLocationType_StrmNodeBud
        cDescription(iCount+1)  = f_cDescription_StrmNodeBudget
        iCount                  = iCount + 1 
    END IF
    
    !Stream reach budget
    IF (AppStream%StrmReachBudRawFile_Defined) THEN
        CALL AppStream%StrmReachBudRawFile%GetFileName(cFileName)
        cFiles(iCount+1)        = cFileName
        iTypeList(iCount+1)     = f_iBudgetType_StrmReach
        iLocationList(iCount+1) = f_iLocationType_StrmReach
        cDescription(iCount+1)  = f_cDescription_StrmReachBudget
        iCount                  = iCount + 1 
    END IF
     
    !Diversion details (ignore for now)
    !IF (AppStream%AppDiverBypass%DiverDetailsBudRawFile_Defined) THEN
    !    CALL AppStream%AppDiverBypass%DiverDetailsBudRawFile%GetFileName(cFileName)
    !    cFiles(iCount+1)        = cFileName
    !    iTypeList(iCount+1)     = f_iBudgetType_DiverDetail
    !    iLocationList(iCount+1) = f_iLocationType_Diversion
    !    cDescription(iCount+1)  = f_cDescription_DiverDetailBudget
    !    iCount                  = iCount + 1 
    !END IF
    
    !Copy info to retrun arguments
    ALLOCATE (iBudgetTypeList(iCount) , iBudgetLocationTypeList(iCount) , cBudgetDescriptions(iCount) , cBudgetFiles(iCount))
    iBudgetTypeList         = iTypeList(1:iCount)
    iBudgetLocationTypeList = iLocationList(1:iCount)
    cBudgetDescriptions     = cDescription(1:iCount)
    cBudgetFiles            = cFiles(1:iCount)
    
  END SUBROUTINE GetBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(AppStream,iBudgetType,iLocationIndex,iNCols,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iBudgetType,iLocationIndex
    INTEGER,INTENT(OUT)                 :: iNCols,iStat  
    
    !Initialize
    iStat   =  0
    iNCols  =  0
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_StrmNode)
            CALL AppStream%StrmNodeBudget%GetBudget_NColumns(iLocationIndex,iNCols,iStat)

        CASE (f_iBudgetType_StrmReach)
            IF (AppStream%StrmReachBudRawFile_Defined) THEN
                !Get number of columns (includes Time column)
                CALL AppStream%StrmReachBudRawFile%GetNDataColumns(iLocationIndex,iNCols,iStat) 
                !Exclude Time column
                iNCols = iNCols - 1
            END IF
            
        CASE (f_iBudgetType_DiverDetail)
            CALL AppStream%AppDiverBypass%GetBudget_NColumns(iLocationIndex,iNCols,iStat)
            
    END SELECT
        
  END SUBROUTINE GetBudget_NColumns


  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN TITLES (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(AppStream,iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(BaseAppStreamType),TARGET,INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                         :: iBudgetType,iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)                :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)   :: cColTitles(:) 
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22)                  :: ThisProcedure = ModName // 'GetBudget_ColumnTitles'
    INTEGER                                       :: iNCols,iErrorCode
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    !Initialize
    iStat = 0
        
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_StrmNode)
            CALL AppStream%StrmNodeBudget%GetBudget_ColumnTitles(iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)

        CASE (f_iBudgetType_StrmReach)
            IF (AppStream%StrmReachBudRawFile_Defined) THEN
                !Number of columns (includes Time column)
                CALL AppStream%StrmReachBudRawFile%GetNDataColumns(iLocationIndex,iNCols,iStat)
                IF (iStat .NE. 0) RETURN
                
                !Get column titles (includes Time column)
                ALLOCATE (cColTitles_Local(iNCols))
                cColTitles_Local = AppStream%StrmReachBudRawFile%GetFullColumnHeaders(iLocationIndex,iNCols)
                
                !Insert units
                CALL AppStream%StrmReachBudRawFile%ModifyFullColumnHeaders(cUnitLT,cUnitAR,cUnitVL,cColTitles_Local)
                
                !Remove Time column
                iNCols = iNCols - 1
                ALLOCATE (cColTitles(iNCols))
                cColTitles = ADJUSTL(cColTitles_Local(2:))
                
                !Clear memory
                DEALLOCATE (cColTitles_Local , STAT=iErrorCode)
            ELSE
                CALL SetLastMessage('Stream reach budget is not defined to retreive budget column titles!',f_iFatal,ThisProcedure)
                iStat = -1
            END IF    
            
        CASE (f_iBudgetType_DiverDetail)
            CALL AppStream%AppDiverBypass%GetBudget_ColumnTitles(iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
            
    END SELECT
        
  END SUBROUTINE GetBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM BaseAppStream OBJECT
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenAppStream(AppStream,iBudgetType,iLocationIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(BaseAppStreamType),TARGET,INTENT(IN) :: AppStream
    CHARACTER(LEN=*),INTENT(IN)                :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                         :: iBudgetType,iLocationIndex  !Location can be stream node, reach or diversion
    REAL(8),INTENT(IN)                         :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)            :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)   :: cFlowNames(:)
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+37) :: ThisProcedure = ModName // 'GetBudget_MonthlyFlows_GivenAppStream'
    INTEGER                      :: ID
    
    !Initialize
    iStat =  0
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_StrmNode)
            ID = AppStream%GetStrmNodeID(iLocationIndex)
            CALL AppStream%StrmNodeBudget%GetBudget_MonthlyFlows(ID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
            
        CASE (f_iBudgetType_StrmReach)
            IF (AppStream%StrmReachBudRawFile_Defined) THEN
                ID = AppStream%Reaches(iLocationIndex)%ID
                CALL GetBudget_MonthlyFlows_GivenFile(AppStream%StrmReachBudRawFile,iBudgetType,ID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
            ELSE
                CALL SetLastMessage('Stream reach budget is not defined to retrieve monthly budget flows!',f_iFatal,ThisProcedure)
                iStat = -1
            END IF
            
        CASE (f_iBudgetType_DiverDetail)
            CALL SetLastMessage('Monthly budget values cannot be retrieved from Diversion Details file!',f_iWarn,ThisProcedure)
            ALLOCATE (rFlows(0,0) , cFlowNames(0))
            iStat = -1
    END SELECT
        
  END SUBROUTINE GetBudget_MonthlyFlows_GivenAppStream

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLocationID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLocationID  !Location can be stream node, reach or diversion
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'GetBudget_MonthlyFlows_GivenFile'
    INTEGER,PARAMETER            :: iReadCols(12) = [1,2,3,4,5,6,7,8,9,10,11,12]
    INTEGER                      :: iDimActual,iNTimeSteps
    REAL(8),ALLOCATABLE          :: rValues(:,:)
    TYPE(StrmNodeBudgetType)     :: StrmNodeBudget
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_StrmNode)
            CALL StrmNodeBudget%GetBudget_MonthlyFlows(Budget,iLocationID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
  
        CASE (f_iBudgetType_DiverDetail)
            CALL SetLastMessage('Monthly budget values cannot be retrieved from Diversion Details file!',f_iWarn,ThisProcedure)
            iStat = -1
            
        CASE (f_iBudgetType_StrmReach)
            !Get simulation time steps and allocate array to read data
            iNTimeSteps = Budget%GetNTimeSteps()
            ALLOCATE (rValues(13,iNTimeSteps)) !Adding 1 to the first dimension for Time column; it will be removed later
            
            !Read data
            CALL Budget%ReadData(iLocationID,iReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
            IF (iStat .NE. 0) RETURN
            
            !Store values in return argument
            ALLOCATE (rFlows(11,iDimActual) , cFlowNames(11))
            rFlows(1,:)  = rValues(2,1:iDimActual)                             !Upstream Inflow              
            rFlows(2,:)  = -rValues(3,1:iDimActual)                            !Downstream Outflow          
            rFlows(3,:)  = rValues(4,1:iDimActual)                             !Tributary Inflow            
            rFlows(4,:)  = rValues(5,1:iDimActual)                             !Tile Drain                  
            rFlows(5,:)  = rValues(6,1:iDimActual)                             !Runoff                      
            rFlows(6,:)  = rValues(7,1:iDimActual)                             !Return Flow                  
            rFlows(7,:)  = rValues(8,1:iDimActual) + rValues(9,1:iDimActual)   !Gain from GW    
            rFlows(8,:)  = rValues(10,1:iDimActual)                            !Gain from Lake               
            rFlows(9,:) = -rValues(11,1:iDimActual)                            !Riparian ET                  
            rFlows(10,:) = -rValues(12,1:iDimActual)                           !Diversion                    
            rFlows(11,:) = -rValues(13,1:iDimActual)                           !By-pass Flow                 
            
            !Flow names
            cFlowNames     = ''
            cFlowNames(1)  = 'Upstream Inflow'    
            cFlowNames(2)  = 'Downstream Outflow' 
            cFlowNames(3)  = 'Tributary Inflow'   
            cFlowNames(4)  = 'Tile Drain'         
            cFlowNames(5)  = 'Runoff'             
            cFlowNames(6)  = 'Return Flow'        
            cFlowNames(7)  = 'Gain from GW'    
            cFlowNames(8)  = 'Gain from Lake'     
            cFlowNames(9)  = 'Riparian ET'        
            cFlowNames(10) = 'Diversion'          
            cFlowNames(11) = 'Bypass Flow'       
    END SELECT
        
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AppStream,iBudgetType,iLocationIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iBudgetType,iLocationIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)         :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                  :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                 :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                 :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER :: indx,ID
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_StrmNode)
            ID = AppStream%GetStrmNodeID(iLocationIndex)
            CALL AppStream%StrmNodeBudget%GetBudget_TSData(ID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
            
        CASE (f_iBudgetType_StrmReach)
            IF (AppStream%StrmReachBudRawFile_Defined) THEN
                ID = AppStream%Reaches(iLocationIndex)%ID
                !Read data
                DO indx=1,SIZE(iCols)
                    CALL AppStream%StrmReachBudRawFile%ReadData(ID,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
                    IF (iStat .NE. 0) RETURN
                END DO
            ELSE
                iStat          = 0
                inActualOutput = 0
                iDataTypes     = -1
                rOutputDates   = 0.0
                rOutputValues  = 0.0
            END IF
            
        CASE (f_iBudgetType_DiverDetail)
            CALL AppStream%AppDiverBypass%GetBudget_TSData(iLocationIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)

    END SELECT
           
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF HYDROGRAPH TYPES AVAILABLE FOR RETRIEVAL
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographTypeList(AppStream,cHydTypeList,iHydLocationTypeList,cHydFileList)
    CLASS(BaseAppStreamType),INTENT(IN)      :: AppStream
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cHydTypeList(:),cHydFileList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iHydLocationTypeList(:)
    
    !Local variables
    INTEGER                  :: iErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initailize
    DEALLOCATE (cHydTypeList , iHydLocationTypeList , cHydFileList , STAT=iErrorCode)
    
    !Is hydrograph print-out defined?
    IF (AppStream%StrmHyd%IsOutFileDefined()) THEN
        CALL AppStream%StrmHyd%GetFileName(cFileName)
        IF (AppStream%StrmHyd%iHydType .EQ. iHydBoth) THEN
            ALLOCATE (cHydTypeList(2) , cHydFileList(2) , iHydLocationTypeList(2))
            cHydTypeList(1)      = f_cDescription_StrmHyd_Flow
            cHydTypeList(2)      = f_cDescription_StrmHyd_Stage
            iHydLocationTypeList = f_iLocationType_StrmHydObs
            cHydFileList         = ''
            cHydFileList         = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have been copied into an HDF file
        ELSE
            ALLOCATE (cHydTypeList(1) , cHydFileList(1) , iHydLocationTypeList(1))
            IF (AppStream%StrmHyd%iHydType .EQ. iHydFlow) THEN
                cHydTypeList = f_cDescription_StrmHyd_Flow
            ELSE
                cHydTypeList = f_cDescription_StrmHyd_Stage
            END IF
            iHydLocationTypeList = f_iLocationType_StrmHydObs
            cHydFileList         = ''
            cHydFileList         = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have been copied into an HDF file
        END IF
    ELSE
        ALLOCATE (cHydTypeList(0) , cHydFileList(0) , iHydLocationTypeList(0))
    END IF
        
  END SUBROUTINE GetHydrographTypeList
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM FLOW HYDROGRAPH OUTPUT FILENAME
  ! -------------------------------------------------------------
  SUBROUTINE GetHydOutputFileName(AppStream,cFileName)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName
    
    CALL AppStream%StrmHyd%GetFileName(cFileName)
    
  END SUBROUTINE GetHydOutputFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET PURPOSE OF DIVERSIONS (IF THEY SERVE AG, URBAN OR BOTH) BEFORE ANY ADJUSTMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetDiversionPurpose(AppStream,iDivers,iAgOrUrban,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iDivers(:)
    INTEGER,INTENT(OUT)                 :: iAgOrUrban(:),iStat
    
    CALL AppStream%AppDiverBypass%GetDiversionPurpose(iDivers,iAgOrUrban,iStat)
    
  END SUBROUTINE GetDiversionPurpose

  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL DIVERSIONS FROM SOME NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetActualDiversions_AtSomeNodes(AppStream,iNodes,rDivers,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iNodes(:)
    REAL(8),INTENT(OUT)                 :: rDivers(:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    iStat   = 0
    rDivers = AppStream%AppDiverBypass%GetNodeDiversions(iNodes)
    
  END SUBROUTINE GetActualDiversions_AtSomeNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL DIVERSIONS FOR SOME DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetActualDiversions_AtSomeDiversions(AppStream,iDivers,rDivers,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iDivers(:)
    REAL(8),INTENT(OUT)                 :: rDivers(:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    CALL AppStream%AppDiverBypass%GetActualDiversions_AtSomeDiversions(iDivers,rDivers,iStat)
    
  END SUBROUTINE GetActualDiversions_AtSomeDiversions
  
  
  ! -------------------------------------------------------------
  ! --- GET DIVERSIONS FOR A SPECIFIED DELIVERIES; I.E. ADD LOSSES TO DELIVERIES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetDiversionsForDeliveries(AppStream,iDivers,rDelis,rDivers)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iDivers(:)
    REAL(8),INTENT(IN)                  :: rDelis(:)
    REAL(8),INTENT(OUT)                 :: rDivers(:)
    
    CALL AppStream%AppDiverBypass%GetDiversionsForDeliveries(iDivers,rDelis,rDivers)
    
  END SUBROUTINE GetDiversionsForDeliveries
  
  
  ! -------------------------------------------------------------
  ! --- GET DELIVERY RELATED TO A DIVERSION
  ! -------------------------------------------------------------
  PURE FUNCTION GetDeliveryAtDiversion(AppStream,iDiver) RESULT(rDeli)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iDiver
    REAL(8)                             :: rDeli
    
    rDeli = AppStream%AppDiverBypass%GetDeliveryAtDiversion(iDiver)
    
  END FUNCTION GetDeliveryAtDiversion
  
  
  ! -------------------------------------------------------------
  ! --- GET DIVERSION SUPPLY SPECS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSupplySpecs(AppStream,DiverSpecs)
    CLASS(BaseAppStreamType),INTENT(IN)      :: AppStream
    TYPE(SupplyType),ALLOCATABLE,INTENT(OUT) :: DiverSpecs(:)
    
    ALLOCATE (DiverSpecs(AppStream%GetNDiver()))
    
    DiverSpecs = AppStream%AppDiverBypass%Diver%Deli%SupplyType
    
  END SUBROUTINE GetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM FLOWS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetFlows(AppStream,Flows)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)               :: Flows(:)
    
    Flows = AppStream%State%Flow
    
  END SUBROUTINE GetFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM FLOW AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetFlow(AppStream,iStrmNode) RESULT(Flow)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iStrmNode
    REAL(8)                             :: Flow
    
    Flow = AppStream%State(iStrmNode)%Flow
    
  END FUNCTION GetFlow
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetInflow_AtANode(AppStream,iStrmNode) RESULT(rInflow)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iStrmNode
    REAL(8)                             :: rInflow
    
    rInflow = AppStream%StrmInflowData%GetInflow_AtANode(iStrmNode)
    
  END FUNCTION GetInflow_AtANode
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOWS AT A SET OF NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInflows_AtSomeNodes(AppStream,iStrmNodes,rInflows)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iStrmNodes(:)
    REAL(8),INTENT(OUT)                 :: rInflows(:)
    
    !Local variables
    REAL(8) :: rInflowsAll(AppStream%NStrmNodes)
    
    rInflowsAll = AppStream%StrmInflowData%GetInflows_AtAllNodes(AppStream%NStrmNodes)
    rInflows    = rInflowsAll(iStrmNodes)
    
  END SUBROUTINE GetInflows_AtSomeNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOWS AT SOME INFLOWS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInflows_AtSomeInflows(AppStream,iInflows,rInflows)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iInflows(:)
    REAL(8),INTENT(OUT)                 :: rInflows(:)
    
    !Local variables
    REAL(8) :: rInflowsAll(AppStream%StrmInflowData%iSize)
    
    CALL AppStream%StrmInflowData%GetInflows_AtAllInflows(rInflowsAll)
    rInflows    = rInflowsAll(iInflows)
       
  END SUBROUTINE GetInflows_AtSomeInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF INFLOWS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNInflows(AppStream) RESULT(iNInflows)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER                             :: iNInflows
    
    iNInflows = AppStream%StrmInflowData%GetNInflows()
    
  END FUNCTION GetNInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOW NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetInflowNodes(AppStream,iNodes)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,ALLOCATABLE                 :: iNodes(:)
    
    CALL AppStream%StrmInflowData%GetInflowNodes(iNodes)
    
  END SUBROUTINE GetInflowNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOW IDs
  ! -------------------------------------------------------------
  SUBROUTINE GetInflowIDs(AppStream,IDs)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,ALLOCATABLE                 :: IDs(:)
    
    CALL AppStream%StrmInflowData%GetInflowIDs(IDs)
    
  END SUBROUTINE GetInflowIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM HEADS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHeads(AppStream,Heads)
    CLASS(BAseAppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)               :: Heads(:)
    
    Heads = AppStream%State%Head
    
  END SUBROUTINE GetHeads
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM HEAD AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetHead_AtOneNode(AppStream,iNode,lPrevious) RESULT(Head)
    CLASS(BAseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iNode
    LOGICAL,INTENT(IN)                  :: lPrevious
    REAL(8)                             :: Head
    
    IF (lPrevious) THEN
        Head = AppStream%State(iNode)%Head_P
    ELSE
        Head = AppStream%State(iNode)%Head
    END IF
    
  END FUNCTION GetHead_AtOneNode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNStrmNodes(AppStream) RESULT(NStrmNodes)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER                             :: NStrmNodes
    
    NStrmNodes = AppStream%NStrmNodes
    
  END FUNCTION GetNStrmNodes
  

  ! -------------------------------------------------------------
  ! --- GET NUMBER OF REACHES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNReaches(AppStream) RESULT(NReaches)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER                             :: NReaches
    
    NReaches = AppStream%NReaches
    
  END FUNCTION GetNReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET DIVERSION/DELIVERY IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetDiversionIDs(AppStream,IDs)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)                 :: IDs(:)
    
    CALL AppStream%AppDiverBypass%GetDiversionIDs(IDs)
    
  END SUBROUTINE GetDiversionIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassIDs(AppStream,IDs)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)                 :: IDs(:)
    
    CALL AppStream%AppDiverBypass%GetBypassIDs(IDs)
    
  END SUBROUTINE GetBypassIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET REACH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetReachIDs(AppStream,IDs)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)                 :: IDs(AppStream%NReaches)
    
    IDs = AppStream%Reaches%ID
    
  END SUBROUTINE GetReachIDs
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT REACH ID TO INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachIndex(AppStream,iReachID) RESULT(Index)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReachID
    INTEGER                             :: Index
    
    Index = StrmReach_ID_To_Index(AppStream%Reaches,iReachID)
    
  END FUNCTION GetReachIndex
  
  
  ! -------------------------------------------------------------
  ! --- GET REACHES FOR SOME STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetReaches_ForStrmNodes(AppStream,iStrmNodes,iReaches,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iStrmNodes(:)
    INTEGER,INTENT(OUT)                 :: iReaches(:),iStat
    
    !Local variables
    INTEGER :: indxNode,indxReach,iStrmNode
    
    !Initialize
    iStat = 0
    
    !Get the reach indices for stream nodes
    DO indxNode=1,SIZE(iStrmNodes)
        iStrmNode = iStrmNodes(indxNode)
        DO indxReach=1,AppStream%NReaches
            IF (iStrmNode .GE. AppStream%Reaches(indxReach)%UpstrmNode) THEN
                IF (iStrmNode .LE. AppStream%Reaches(indxReach)%DownstrmNode) THEN
                    iReaches(indxNode) = indxReach
                    EXIT
                END IF
            END IF
        END DO
    END DO
    
  END SUBROUTINE GetReaches_ForStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DIVERSIONS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNDiver(AppStream) RESULT(NDiver)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER                             :: NDiver
    
    NDiver = AppStream%AppDiverBypass%NDiver
    
  END FUNCTION GetNDiver


  ! -------------------------------------------------------------
  ! --- GET STREAM NODES FOR A GIVEN SET OF DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetDiversionsExportNodes(AppStream,iDivList,iStrmNodeList)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iDivList(:)
    INTEGER,INTENT(OUT)                 :: iStrmNodeList(:)
    
    !Get the stream node index where diversions are originating
    CALL AppStream%AppDiverBypass%GetDiversionsExportNodes(iDivList,iStrmNodeList)
    
  END SUBROUTINE GetDiversionsExportNodes


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BYPASSES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNBypass(AppStream) RESULT(NBypass)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER                             :: NBypass
    
    NBypass = AppStream%AppDiverBypass%NBypass
    
  END FUNCTION GetNBypass


  ! -------------------------------------------------------------
  ! --- GET BYPASS/DIVERSION ORIGIN AND DESTINATION DATA
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassDiverOriginDestData(AppStream,lIsBypass,iBypassOrDiver,iNodeExport,iDestType,iDest)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    LOGICAL,INTENT(IN)                  :: lIsBypass
    INTEGER,INTENT(IN)                  :: iBypassOrDiver
    INTEGER,INTENT(OUT)                 :: iNodeExport,iDestType,iDest
    
    CALL AppStream%AppDiverBypass%GetBypassDiverOriginDestData(lIsBypass,iBypassOrDiver,iNodeExport,iDestType,iDest)
    
  END SUBROUTINE GetBypassDiverOriginDestData
  

  ! -------------------------------------------------------------
  ! --- GET NET FLOW FROM A BYPASS (AFTER RECOVERABLE AND NON-RECOVERABLE LOSSES ARE TAKEN OUT)
  ! -------------------------------------------------------------
  PURE FUNCTION GetBypassReceived_FromABypass(AppStream,iBypass) RESULT(rFlow)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iBypass
    REAL(8)                             :: rFlow
    
    rFlow = AppStream%AppDiverBypass%GetBypassReceived_FromABypass(iBypass)
    
  END FUNCTION GetBypassReceived_FromABypass
  
  
  ! -------------------------------------------------------------
  ! --- GET NET BYPASS INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetNetBypassInflows(AppStream,rBPInflows)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(OUT)                 :: rBPInflows(:)
    
    !Local variables
    INTEGER :: indx,iNodeArray(AppStream%NStrmNodes)
    
    !Initialize
    iNodeArray = [(indx,indx=1,AppStream%NStrmNodes)]
    
    !Multiply bypasses by -1 because original values are bypasses as outflow, and we are converting them to inflow
    rBPInflows = -AppStream%AppDiverBypass%GetNodeNetBypass(iNodeArray)
    
  END SUBROUTINE GetNetBypassInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmBypassInflows(AppStream,rBPInflows)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(OUT)                 :: rBPInflows(:)
    
    CALL AppStream%AppDiverBypass%GetStrmBypassInflows(rBPInflows)
    
  END SUBROUTINE GetStrmBypassInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS OUTFLOWS FOR ALL BYPASSES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassOutflows(AppStream,rOutflows)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(OUT)                 :: rOutflows(:)
    
    CALL AppStream%AppDiverBypass%GetBypassOutflows(rOutflows)
    
  END SUBROUTINE GetBypassOutflows
  
  
  ! -------------------------------------------------------------
  ! --- GET FEATURE NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetNames(AppStream,iLocationType,cNamesList)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iLocationType
    CHARACTER(LEN=*),INTENT(OUT)        :: cNamesList(:)  !Assumes array is previously dimensioned based on the number of reaches
    
    !Local variables
    INTEGER             :: indx,ID 
    INTEGER,ALLOCATABLE :: iBudNodes(:)
    
    SELECT CASE (iLocationType)
            CASE (f_iLocationType_StrmReach)
                cNamesList = Appstream%Reaches%cName
            
            CASE (f_iLocationType_StrmHydObs)
                CALL AppStream%StrmHyd%GetNames(cNamesList)
                
            CASE (f_iLocationType_Bypass)
                cNamesList = AppStream%AppDiverBypass%Bypasses%cName
                
            CASE (f_iLocationType_StrmNodeBud)
                cNamesList = ''
                CALL AppStream%StrmNodeBudget%GetBudNodes(iBudNodes)
                DO indx=1,SIZE(iBudNodes)
                    ID               = AppStream%GetStrmNodeID(iBudNodes(indx))
                    cNamesList(indx) = TRIM(IntToText(ID))
                END DO
    END SELECT
    
  END SUBROUTINE GetNames
  
  
  ! -------------------------------------------------------------
  ! --- GET DOWNSTREAM NODE FOR A REACH
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachDownstrmNode(AppStream,iReach) RESULT(iNode)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER                             :: iNode
    
    iNode = Appstream%Reaches(iReach)%DownstrmNode
    
  END FUNCTION GetReachDownstrmNode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM NODES IN A REACH DEFINED WITH ITS INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachNNodes(AppStream,iReach) RESULT(iNNodes)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER                             :: iNNodes
    
    !Number of nodes
    iNNodes = Appstream%Reaches(iReach)%DownstrmNode - AppStream%Reaches(iReach)%UpstrmNode + 1
    
  END FUNCTION GetReachNNodes

  
  ! -------------------------------------------------------------
  ! --- GET UPSTREAM NODE FOR A REACH
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachUpstrmNode(AppStream,iReach) RESULT(iNode)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER                             :: iNode
    
    iNode = Appstream%Reaches(iReach)%UpstrmNode
    
  END FUNCTION GetReachUpstrmNode

  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODES FOR A GIVEN REACH 
  ! -------------------------------------------------------------
  SUBROUTINE GetReachStrmNodes(AppStream,iReach,iStrmNodes,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER,ALLOCATABLE                 :: iStrmNodes(:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    INTEGER :: iNNodes,ErrorCode,iOffset,indx
    
    !Initialize
    iStat = 0
    
    !Number of reach nodes
    iNNodes = AppStream%Reaches(iReach)%DownstrmNode - AppStream%Reaches(iReach)%UpstrmNode + 1
    
    !Stream nodes for the reach
    DEALLOCATE (iStrmNodes ,STAT=ErrorCode)
    ALLOCATE (iStrmNodes(iNNodes))
    iOffset = AppStream%Reaches(iReach)%UpstrmNode - 1 
    DO indx=1,iNNodes
        iStrmNodes(indx) = indx + iOffset
    END DO
    
  END SUBROUTINE GetReachStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ALL REACHES WITHIN THE NETWORK UPSTREAM FROM A REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  FUNCTION GetNReaches_InUpstrmNetwork(AppStream,iReach) RESULT(iNReaches)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER                             :: iNReaches
    
    iNReaches = StrmReach_GetNReaches_InUpstrmNetwork(AppStream%Reaches,iReach)
    
  END FUNCTION GetNReaches_InUpstrmNetwork
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL REACH NETWORK UPSTREAM FROM A REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  SUBROUTINE GetReaches_InUpstrmNetwork(AppStream,iReach,iReaches)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER,ALLOCATABLE                 :: iReaches(:)
    
    CALL StrmReach_GetReaches_InUpstrmNetwork(AppStream%Reaches,iReach,iReaches)
    
  END SUBROUTINE GetReaches_InUpstrmNetwork
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF REACHES IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  FUNCTION GetReachNUpstrmReaches(AppStream,iReach) RESULT(iNReaches)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER                             :: iNReaches
    
    !Get number of reaches upstream
    iNReaches = AppStream%Reaches(iReach)%NUpstrmReaches
    
  END FUNCTION GetReachNUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET REACHES IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  SUBROUTINE GetReachUpstrmReaches(AppStream,iReach,iUpstrmReaches)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER,ALLOCATABLE                 :: iUpstrmReaches(:)
    
    !Get reaches upstream
    ALLOCATE (iUpstrmReaches(AppStream%Reaches(iReach)%NUpstrmReaches))
    iUpstrmReaches = AppStream%Reaches(iReach)%UpstrmReaches
    
  END SUBROUTINE GetReachUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET UPSTREAM NODE FLAGS FOR ALL STREAM NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetUpstrmNodeFlags(AppStream) RESULT(lUpstrmNode)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    LOGICAL                             :: lUpstrmNode(AppStream%NStrmNodes)
    
    !Local variables
    INTEGER :: indxReach
    
    !Initialize
    lUpstrmNode = .FALSE.
    
    !Iterate over reaches and set the flag for the upstream node
    DO indxReach=1,AppStream%NReaches
        lUpstrmNode(AppStream%GetReachUpstrmNode(indxReach)) = .TRUE.
    END DO
    
  END FUNCTION GetUpstrmNodeFlags

  
  ! -------------------------------------------------------------
  ! --- GET REACH DESTINATION TYPE
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachOutflowDestType(AppStream,iReach) RESULT(iDestType)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER                             :: iDestType
    
    IF (iReach .GT. 0) THEN
      IF (iReach.LE.AppStream%NReaches) iDestType = AppStream%Reaches(iReach)%OutflowDestType
    ELSE
      iDestType = f_iFlowDest_Outside
    END IF
    
  END FUNCTION GetReachOutflowDestType


  ! -------------------------------------------------------------
  ! --- GET REACH DESTINATION
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachOutflowDest(AppStream,iReach) RESULT(iDest)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iReach
    INTEGER                             :: iDest
    
    IF (iReach .GT. 0) THEN
      IF (iReach.LE.AppStream%NReaches) iDest = AppStream%Reaches(iReach)%OutflowDest
    ELSE
      iDest = 0
    END IF
    
  END FUNCTION GetReachOutflowDest


  ! -------------------------------------------------------------
  ! --- GET STREAM CONNECTIVITY IN TERMS OF GW NODES AS A SET OF COMPLEX NUMBERS 
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmConnectivityInGWNodes(AppStream,StrmGWConnector,Connectivity)
    CLASS(BaseAppStreamType),INTENT(IN)  :: AppStream
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    COMPLEX,ALLOCATABLE,INTENT(OUT)      :: Connectivity(:)
    
    !Local variables
    INTEGER             :: ErrorCode,indx,iUpstrmNode,iDownStrmNode,NODER,iCount,Node1,Node2
    INTEGER,ALLOCATABLE :: GWNodes(:)
       
    !Initialize
    DEALLOCATE (Connectivity ,STAT=ErrorCode)
    ALLOCATE (Connectivity(AppStream%NStrmNodes))
    
    !Return if streams are not modeled
    IF (AppStream%NStrmNodes .EQ. 0) RETURN
    
    !GW nodes corresponding to stream nodes
    CALL StrmGWConnector%GetAllGWNodes(GWNodes) 
    
    !Compile stream connectivity into a set of complex numbers
    iCount  = 0
    DO indx=1,AppStream%NReaches
      iUpstrmNode   = AppStream%GetReachUpstrmNode(indx)
      iDownstrmNode = AppStream%GetReachDownstrmNode(indx)
      DO NODER=iUpstrmNode,iDownstrmNode-1
          iCount               = iCount + 1
          Node1                = GWNodes(NODER)
          Node2                = GWNodes(NODER+1)
          Connectivity(iCount) = CMPLX(Node1,Node2)
      END DO
      iCount               = iCount + 1
      Node1                = GWNodes(iDownstrmNode)
      Connectivity(iCount) = CMPLX(Node1,Node1)
    END DO
    
    !Clear memory
    DEALLOCATE (GWNodes , STAT=ErrorCode)

  END SUBROUTINE GetStrmConnectivityInGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAGS
  ! -------------------------------------------------------------
  SUBROUTINE GetiColAdjust(AppStream,iColAdjust)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)                 :: iColAdjust(:)
    
    iColAdjust = AppStream%AppDiverBypass%Diver%Deli%SupplyType%iColAdjust
        
  END SUBROUTINE GetiColAdjust


  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT RELATED DATA
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyAdjustData(AppStream,iDiverRank,iColAdjust,DeliRequired,DeliMax,DeliActual,IrigFracs)
    CLASS(BaseAppStreamType),INTENT(IN)       :: AppStream
    INTEGER,INTENT(OUT)                       :: iDiverRank(:),iColAdjust(:)
    REAL(8),INTENT(OUT)                       :: DeliRequired(:),DeliMax(:),DeliActual(:),IrigFracs(:)
    
    !Store values to arguments
    ASSOCIATE (pDivers => AppStream%AppDiverBypass%Diver)
      iDiverRank   = pDivers%Rank
      iColAdjust   = pDivers%Deli%iColAdjust
      DeliRequired = pDivers%Deli%SupplyRequired
      DeliMax      = pDivers%MaxDiver * (1d0 - pDivers%Ratio_RecvLoss - pDivers%Ratio_NonRecvLoss)
      DeliActual   = pDivers%Deli%SupplyActual
      IrigFracs    = pDivers%Deli%IrigFrac
    END ASSOCIATE
  
  END SUBROUTINE GetSupplyAdjustData
  
  
  ! -------------------------------------------------------------
  ! --- GET DELIVERY SUPPLY TO ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE GetSupply(AppStream,SupplyDestConnector,Supply_Ag,Supply_Urb)
    CLASS(BaseAppStreamType),INTENT(IN)             :: AppStream
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: SupplyDestConnector
    REAL(8),INTENT(OUT)                             :: Supply_Ag(:),Supply_Urb(:)
    
    CALL Supply_GetSupply(AppStream%AppDiverBypass%Diver%Deli,SupplyDestConnector,Supply_Ag,Supply_Urb)
    
  END SUBROUTINE GetSupply
  
  
  ! -------------------------------------------------------------
  ! --- GET MAXIMUM VALUE OF THE DELIVERY RANKS
  ! -------------------------------------------------------------
  PURE FUNCTION GetMaxDiversionRank(AppStream) RESULT(iMaxRank)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER                             :: iMaxRank
    
    !Get maximum value of the diversion ranks
    iMaxRank = MAXVAL(AppStream%AppDiverBypass%Diver%Rank)
    
  END FUNCTION GetMaxDiversionRank
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT LEVEL RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  PURE FUNCTION GetElemRecvLosses(AppStream,NElements,iSource) RESULT(ElemRecvLosses)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: NElements,iSource
    REAL(8)                             :: ElemRecvLosses(NElements)
    
    ElemRecvLosses = AppStream%AppDiverBypass%GetElemRecvLosses(NElements,iSource)
    
  END FUNCTION GetElemRecvLosses
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  PURE FUNCTION GetSubregionalRecvLosses(AppStream,AppGrid) RESULT(RecvLosses)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: RecvLosses(AppGrid%NSubregions)
    
    RecvLosses = AppStream%AppDiverBypass%GetSubregionalRecvLosses(AppGrid)
    
  END FUNCTION GetSubregionalRecvLosses
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTS THAT RECEIVE RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  SUBROUTINE GetElemsWithRecvLoss(AppStream,iSource,Elems)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: iSource
    INTEGER,ALLOCATABLE,INTENT(OUT)     :: Elems(:)
    
    !Local variables
    INTEGER             :: ErrorCode
    INTEGER,ALLOCATABLE :: ElemsDiv(:),ElemsBP(:)
    
    SELECT CASE (iSource)
        !Due to diversions
        CASE (f_iDiverRecvLoss)
            CALL ElemToRecvLoss_GetElems(AppStream%AppDiverBypass%ElemToDiverRecvLoss,Elems)
          
        !Due to bypasses
        CASE (f_iBypassRecvLoss)
            CALL ElemToRecvLoss_GetElems(AppStream%AppDiverBypass%ElemToBypassRecvLoss,Elems)
          
        !Due to both bypasses and diversions
        CASE (f_iAllRecvLoss)
            CALL ElemToRecvLoss_GetElems(AppStream%AppDiverBypass%ElemToDiverRecvLoss,ElemsDiv)
            CALL ElemToRecvLoss_GetElems(AppStream%AppDiverBypass%ElemToBypassRecvLoss,ElemsBP)
            ALLOCATE (Elems(SIZE(ElemsDiv)+SIZE(ElemsBP)))
            Elems(1:SIZE(ElemsDiv))  = ElemsDiv
            Elems(SIZE(ElemsDiv)+1:) = ElemsBP
            DEALLOCATE (ElemsDiv,ElemsBP,STAT=ErrorCode)
            
        !Otherwise
        CASE DEFAULT
            ALLOCATE (Elems(0))
          
    END SELECT
    
  END SUBROUTINE GetElemsWithRecvLoss
  
  
  
  
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
  ! --- SET DIVERSION READ
  ! -------------------------------------------------------------
  SUBROUTINE SetDiversionRead(AppStream,iDiver,rDiversion)
    CLASS(BaseAppStreamType) :: AppStream
    INTEGER,INTENT(IN)       :: iDiver
    REAL(8),INTENT(IN)       :: rDiversion
    
    CALL AppStream%AppDiverBypass%SetDiversionRead(iDiver,rDiversion) 
    
  END SUBROUTINE SetDiversionRead
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY SPECS
  ! -------------------------------------------------------------
  SUBROUTINE SetSupplySpecs(AppStream,DeliDestConnector,DeliRequired,IrigFracs,DeliToDest)
    CLASS(BaseAppStreamType)                 :: AppStream
    TYPE(SupplyDestinationConnectorType)     :: DeliDestConnector
    REAL(8),INTENT(IN)                       :: DeliRequired(:),IrigFracs(:)
    TYPE(SupplyToDestinationType),INTENT(IN) :: DeliToDest(:)
    
    CALL Supply_SetSupplySpecs(AppStream%AppDiverBypass%Diver%Deli,DeliDestConnector,DeliRequired,IrigFracs,DeliToDest)
        
    CALL AppStream%AppDiverBypass%SetDiverRequired(DeliRequired)
        
  END SUBROUTINE SetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- SET DELIVERY IRRIGATION FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE SetIrigFracsRead(AppStream,IrigFrac)
    CLASS(BaseAppStreamType) :: AppStream
    REAL(8),INTENT(IN)       :: IrigFrac(:)
    
    !Local variables
    INTEGER :: indxDiver,iColIrigFrac
    
    DO indxDiver=1,AppStream%AppDiverBypass%NDiver
      ASSOCIATE (pDeli => AppStream%AppDiverBypass%Diver(indxDiver)%Deli)
        iColIrigFrac = pDeli%iColIrigFrac
        IF (iColIrigFrac .GT. 0) THEN
          pDeli%IrigFracRead = IrigFrac(iColIrigFrac)
          pDeli%IrigFrac     = pDeli%IrigFracRead
        END IF
      END ASSOCIATE
    END DO
    
  END SUBROUTINE SetIrigFracsRead


  ! -------------------------------------------------------------
  ! --- SET STREAM FLOW AT A NODE (ALLOWED ONLY WHEN STREAMS ARE NON-ROUTED)
  ! -------------------------------------------------------------
  SUBROUTINE SetStreamFlow(AppStream,iStrmNode,rFlow)
    CLASS(BaseAppStreamType) :: AppStream
    INTEGER,INTENT(IN)       :: iStrmNode
    REAL(8),INTENT(IN)       :: rFlow
    
    AppStream%State(iStrmNode)%Flow = rFlow
    
  END SUBROUTINE SetStreamFlow

  
  ! -------------------------------------------------------------
  ! --- SET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE SetStreamInflow(AppStream,iStrmNode,rFlow,lAdd)
    CLASS(BaseAppStreamType) :: AppStream
    INTEGER,INTENT(IN)       :: iStrmNode
    REAL(8),INTENT(IN)       :: rFlow
    LOGICAL,INTENT(IN)       :: lAdd
    
    CALL AppStream%StrmInflowData%SetInflow(iStrmNode,rFlow,lAdd)
    
  END SUBROUTINE SetStreamInflow

  
  ! -------------------------------------------------------------
  ! --- SET BYPASS ORIGINATING FLOW AS WELL AS OTHER RELATED FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE SetBypassFlows_AtABypass(AppStream,iBypass,rOriginatingFlow)
    CLASS(BaseAppStreamType) :: AppStream
    INTEGER,INTENT(IN)       :: iBypass
    REAL(8),INTENT(IN)       :: rOriginatingFlow
    
    CALL AppStream%AppDiverBypass%SetBypassFlows_AtABypass(iBypass,rOriginatingFlow) 
    
  END SUBROUTINE SetBypassFlows_AtABypass

  
  
  
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
  SUBROUTINE ReadTSData(AppStream,lDiverAdjusted,TimeStep,iDiversions,rDiversions,iStrmInflows,rStrmInflows,iBypasses,rBypasses,iStat)
     CLASS(BaseAppStreamType)      :: AppStream
     LOGICAL,INTENT(IN)            :: lDiverAdjusted
     TYPE(TimeStepType),INTENT(IN) :: TimeStep
     INTEGER,INTENT(IN)            :: iDiversions(:),iStrmInflows(:),iBypasses(:)
     REAL(8),INTENT(IN)            :: rDiversions(:),rStrmInflows(:),rBypasses(:)
     INTEGER,INTENT(OUT)           :: iStat
     
     !Read stream boundary inflows
     CALL AppStream%StrmInflowData%ReadTSData(TimeStep,iStrmInflows,rStrmInflows,iStat)
     IF (iStat .EQ. -1) RETURN
     
     !Read diversion/bypass related timeseries data
     CALL AppStream%AppDiverBypass%ReadTSData(lDiverAdjusted,TimeStep,iDiversions,rDiversions,iBypasses,rBypasses,iStat)  
     IF (iStat .EQ. -1) RETURN
     
     !Update the diversions from each stream node
     CALL AppStream%AppDiverBypass%CompileNodalDiversions()
    
  END SUBROUTINE ReadTSData
     
     
  ! -------------------------------------------------------------
  ! --- READ RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(AppStream,InFile,iStat)
    CLASS(BaseAppStreamType) :: AppStream
    TYPE(GenericFileType)    :: InFile
    INTEGER,INTENT(OUT)      :: iStat
    
    CALL InFile%ReadData(AppStream%State%Head_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppStream%State%Head,iStat)
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ STREAM NODES AND FRACTIONS OF STREAM_AQUIFER INTERACTION TO APPLY TO GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE ReadFractionsForGW(DataFile,iStrmNodeIDs,StrmGWConnector,iStat)
    TYPE(GenericFileType)     :: DataFile
    INTEGER,INTENT(IN)        :: iStrmNodeIDs(:)
    TYPE(StrmGWConnectorType) :: StrmGWConnector
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'ReadFractionsForGW'
    INTEGER                                :: iNStrmNodes,indx,iStrmNode
    REAL(8)                                :: rDummy(2)
    INTEGER,ALLOCATABLE                    :: iStrmNodes(:)
    REAL(8),ALLOCATABLE                    :: rFractions(:)
    
    !Initilalize
    iStat = 0
    
    !First check if the data is even supplied (backward compatibility)
    CALL DataFile%ReadData(iNStrmNodes,iStat)
    IF (iStat .EQ. -1) THEN  !If in error state; data is not supplied; return
        iStat = 0
        RETURN
    END IF
    
    !Allocate memory
    ALLOCATE (iStrmNodes(iNStrmNodes) , rFractions(iNStrmNodes))
    
    !Read data
    DO indx=1,iNStrmNodes
        CALL DataFile%ReadData(rDummy,iStat)  ;  IF (iStat .EQ. -1) RETURN
        iStrmNode        = rDummy(1)
        rFractions(indx) = rDummy(2)
        CALL ConvertID_To_Index(iStrmNode,iStrmNodeIDs,iStrmNodes(indx))
        IF (iStrmNodes(indx) .EQ. 0) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNode))//' listed for partal stream-aquifer interaction is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO
    
    !Pass the information to StrmGWConnector
    CALL StrmGWConnector%SetFractionsForGW(iStrmNodes,rFractions,iStat)
    
  END SUBROUTINE ReadFractionsForGW

  
  
  
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
  ! --- PRINT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppStream,OutFile)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    TYPE(GenericFileType)               :: OutFile
    
    CALL OutFile%WriteData(AppStream%State%Head_P)
    CALL OutFile%WriteData(AppStream%State%Head)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppStream,TimeStep,lEndOfSimulation,QTRIB,QROFF,QRTRN,QDRAIN,QRVET,BottomElev,StrmGWConnector,StrmLakeConnector)
    CLASS(BaseAppStreamType)               :: AppStream
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
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- WRITE RAW STREAM REACH BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteStrmReachFlowsToBudRawFile(QTRIB,QROFF,QRTRN,QDRAIN,QRVET,StrmGWConnector,StrmLakeConnector,AppStream)
    CLASS(BaseAppStreamType)                           :: AppStream
    REAL(8),DIMENSION(AppStream%NStrmNodes),INTENT(IN) :: QTRIB,QROFF,QRTRN,QDRAIN,QRVET
    TYPE(StrmGWConnectorType),INTENT(IN)               :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN)             :: StrmLakeConnector
    
    !Local variables
    INTEGER                               :: indxReach,indxReach1,iNode,iUpstrmReach,iUpstrmNode,indx,     &
                                             iDownstrmNode,iReach
    REAL(8)                               :: DummyArray(f_iNStrmBudColumns,AppStream%NReaches) 
    REAL(8),DIMENSION(AppStream%NReaches) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,Runoff, &
                                             ReturnFlows,StrmGWFlows_InModel,StrmGWFlows_OutModel,Error,   &
                                             LakeInflows,Diversions,Bypasses,DiversionShorts,RiparianET
    
    !Initialize           
    UpstrmFlows = 0.0
    
    !Iterate over reaches
    DO indxReach=1,AppStream%NReaches
        iReach        = AppStream%iPrintReachBudgetOrder(indxReach)
        iUpstrmNode   = AppStream%Reaches(iReach)%UpstrmNode
        iDownstrmNode = AppStream%Reaches(iReach)%DownstrmNode
        !Upstream flows
        DO indxReach1=1,AppStream%Reaches(iReach)%NUpstrmReaches
            iUpstrmReach           = AppStream%Reaches(iReach)%UpstrmReaches(indxReach1)
            iNode                  = AppStream%Reaches(iUpstrmReach)%DownstrmNode
            UpstrmFlows(indxReach) = UpstrmFlows(indxReach) + AppStream%State(iNode)%Flow
        END DO
        IF (AppStream%StrmInflowData%lDefined) UpstrmFlows(indxReach) = UpstrmFlows(indxReach) + SUM(AppStream%StrmInflowData%Inflows(iUpstrmNode:iDownstrmNode))
        
        !Tributary flows
        TributaryFlows(indxReach) = SUM(QTRIB(iUpstrmNode:iDownstrmNode))
        
        !Inflows from tile drains
        DrainInflows(indxReach) = SUM(QDRAIN(iUpstrmNode:iDownstrmNode))
        
        !Runoff
        Runoff(indxReach) = SUM(QROFF(iUpstrmNode:iDownstrmNode))
        
        !Return flow
        ReturnFlows(indxReach) = SUM(QRTRN(iUpstrmNode:iDownstrmNode))
        
        !Stream-gw interaction occuring inside model domain
        !(+: flow from stream to groundwater)
        StrmGWFlows_InModel(indxReach) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iUpstrmNode,iDownstrmNode,lInsideModel=.TRUE.)
        
        !Stream-gw interaction occuring ousideside model domain
        !(+: flow from stream to groundwater)
        StrmGWFlows_OutModel(indxReach) = - StrmGWConnector%GetFlowAtSomeStrmNodes(iUpstrmNode,iDownstrmNode,lInsideModel=.FALSE.)
        
        !Inflow from lakes
        LakeInflows(indxReach) = 0.0
        DO indx=iUpstrmNode,iDownstrmNode
            LakeInflows(indxReach) = LakeInflows(indxReach) + StrmLakeConnector%GetFlow(f_iLakeToStrmFlow,indx)
        END DO
        
        !Riparian ET
        RiparianET(indxReach) = SUM(QRVET(iUpstrmNode:iDownstrmNode))
      
        !Downstream flows
        DownstrmFlows(indxReach) = AppStream%State(AppStream%Reaches(iReach)%DownStrmNode)%Flow
      
    END DO
    
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetReachDiversions(AppStream%NReaches,AppStream%Reaches)
    Diversions = Diversions(AppStream%iPrintReachBudgetOrder)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetReachNetBypass(AppStream%NStrmNodes,AppStream%NReaches,AppStream%Reaches)
    Bypasses = Bypasses(AppStream%iPrintReachBudgetOrder)
    
    !Error
    Error =  UpstrmFlows          &
           - DownstrmFlows        &
           + TributaryFlows       &
           + DrainInflows         &
           + Runoff               &
           + ReturnFlows          &
           + StrmGWFlows_InModel  &
           + StrmGWFlows_OutModel &
           + LakeInflows          &
           - RiparianET           &
           - Diversions           &
           - Bypasses
           
    !Diversion shortages
    DiversionShorts = AppStream%AppDiverBypass%GetReachDiversionShort(AppStream%NStrmNodes,AppStream%NReaches,AppStream%Reaches)
    DiversionShorts = DiversionShorts(AppStream%iPrintReachBudgetOrder)
           
    !Compile data in array
    DummyArray(1,:)  = UpstrmFlows
    DummyArray(2,:)  = DownstrmFlows
    DummyArray(3,:)  = TributaryFlows
    DummyArray(4,:)  = DrainInflows
    DummyArray(5,:)  = Runoff
    DummyArray(6,:)  = ReturnFlows
    DummyArray(7,:)  = StrmGWFlows_InModel
    DummyArray(8,:)  = StrmGWFlows_OutModel
    DummyArray(9,:)  = LakeInflows
    DummyArray(10,:) = RiparianET
    DummyArray(11,:) = Diversions
    DummyArray(12,:) = Bypasses
    DummyArray(13,:) = Error
    DummyArray(14,:) = DiversionShorts
    
    !Print out values to binary file
    CALL AppStream%StrmReachBudRawFile%WriteData(DummyArray)

 END SUBROUTINE WriteStrmReachFlowsToBudRawFile
 
 
  ! -------------------------------------------------------------
  ! --- WRITE RAW STREAM NODE BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteStrmNodeFlowsToBudRawFile(QTRIB,QROFF,QRTRN,QDRAIN,QRVET,StrmGWConnector,StrmLakeConnector,AppStream)
    CLASS(BaseAppStreamType)                           :: AppStream
    REAL(8),DIMENSION(AppStream%NStrmNodes),INTENT(IN) :: QTRIB,QROFF,QRTRN,QDRAIN,QRVET
    TYPE(StrmGWConnectorType),INTENT(IN)               :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN)             :: StrmLakeConnector
    
    !Local variables
    INTEGER                                               :: iNode,indxNode
    REAL(8)                                               :: DummyArray(f_iNStrmBudColumns,AppStream%StrmNodeBudget%NBudNodes) 
    REAL(8),DIMENSION(AppStream%StrmNodeBudget%NBudNodes) :: UpstrmFlows,DownstrmFlows,TributaryFlows,DrainInflows,LakeInflows,  &
                                                             Runoff,ReturnFlows,StrmGWFlows_InModel,StrmGWFlows_OutModel,Error,  &
                                                             Diversions,Bypasses,DiversionShorts,RiparianET
    INTEGER,ALLOCATABLE                                   :: UpstrmNodes(:)
    
    !Iterate over nodes
    DO indxNode=1,AppStream%StrmNodeBudget%NBudNodes
      iNode = AppStream%StrmNodeBudget%iBudNodes(indxNode)

      !Upstream flows
      CALL AppStream%GetUpstrmNodes(iNode,UpstrmNodes)
      UpstrmFlows(indxNode) = SUM(AppStream%State(UpStrmNodes)%Flow)
      IF (AppStream%StrmInflowData%lDefined) UpstrmFlows(indxNode) = UpstrmFlows(indxNode) + AppStream%StrmInflowData%Inflows(iNode)
    
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
      
      !Downstream flows
      DownstrmFlows(indxNode) = AppStream%State(iNode)%Flow
      
    END DO
    
    !Diversions
    Diversions = AppStream%AppDiverBypass%GetNodeDiversions(AppStream%StrmNodeBudget%iBudNodes)
    
    !Bypasses
    Bypasses = AppStream%AppDiverBypass%GetNodeNetBypass(AppStream%StrmNodeBudget%iBudNodes)
    
    !Error
    Error =  UpstrmFlows          &
           - DownstrmFlows        &
           + TributaryFlows       &
           + DrainInflows         &
           + Runoff               &
           + ReturnFlows          &
           + StrmGWFlows_InModel  &
           + StrmGWFlows_OutModel &
           + LakeInflows          &
           - RiparianET           &
           - Diversions           &
           - Bypasses
           
    !Diversion shortages
    DiversionShorts = AppStream%AppDiverBypass%GetNodeDiversionShort(AppStream%StrmNodeBudget%iBudNodes)
           
    !Compile data in array
    DummyArray(1,:)  = UpstrmFlows
    DummyArray(2,:)  = DownstrmFlows
    DummyArray(3,:)  = TributaryFlows
    DummyArray(4,:)  = DrainInflows
    DummyArray(5,:)  = Runoff
    DummyArray(6,:)  = ReturnFlows
    DummyArray(7,:)  = StrmGWFlows_InModel
    DummyArray(8,:)  = StrmGWFlows_OutModel
    DummyArray(9,:)  = LakeInflows
    DummyArray(10,:) = RiparianET
    DummyArray(11,:) = Diversions
    DummyArray(12,:) = Bypasses
    DummyArray(13,:) = Error
    DummyArray(14,:) = DiversionShorts
    
    !Print out values to binary file
    CALL AppStream%StrmNodeBudget%StrmNodeBudRawFile%WriteData(DummyArray)

 END SUBROUTINE WriteStrmNodeFlowsToBudRawFile

  

  
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
  ! --- CONVERT REACH DESTINATION IDs TO INDICES (MAINLY FOR LAKE DESTINATIONS)
  ! -------------------------------------------------------------
  SUBROUTINE DestinationIDs_To_Indices(AppStream,iLakeIDs,iStat)
    CLASS(BaseAppStreamType) :: AppStream
    INTEGER,INTENT(IN)       :: iLakeIDs(:)
    INTEGER,INTENT(OUT)      :: iStat
    
    CALL StrmReach_DestinationIDs_To_Indices(AppStream%Reaches,iLakeIDs,iStat)
    
  END SUBROUTINE DestinationIDs_To_Indices
  
  
  ! -------------------------------------------------------------
  ! --- RESET IRRIGATION FRACTIONS TO THOSE READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE ResetIrigFracs(AppStream)
    CLASS(BaseAppStreamType) :: AppStream
    
    CALL Supply_ResetIrigFracs(AppStream%AppDiverBypass%Diver%Deli%SupplyType)

  END SUBROUTINE ResetIrigFracs
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE SUPPLY TO MEET DEMAND GOES TO MODELED DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE CheckSupplyDestinationConnection(AppStream,DeliDestConnector,iStat)
    CLASS(BaseAppStreamType),INTENT(IN)             :: AppStream
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: DeliDestConnector
    INTEGER,INTENT(OUT)                             :: iStat
    
  
    IF (AppStream%AppDiverBypass%NDiver .GT. 0) THEN
        CALL Supply_CheckSupplyDestinationConnection(AppStream%AppDiverBypass%Diver%Deli,DeliDestConnector,"diversion",iStat)
    ELSE
        iStat = 0
    END IF
      
  END SUBROUTINE CheckSupplyDestinationConnection
  
  
  ! -------------------------------------------------------------
  ! --- RESET THE STREAM HEADS TO HEADS FROM PREVIOUS TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE ResetHeads(AppStream)
    CLASS(BaseAppStreamType) :: AppStream
    
    AppStream%State%Head = AppStream%State%Head_P

  END SUBROUTINE ResetHeads
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF STREAMS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppStream)
    CLASS(BaseAppStreamType) :: AppStream
    
    AppStream%State%Head_P = AppStream%State%Head
    
  END SUBROUTINE AdvanceState
  

  ! -------------------------------------------------------------
  ! --- CHECK IF ANY OF THE DIVERSIONS GO TO MODEL DOMAIN
  ! -------------------------------------------------------------
  PURE FUNCTION IsDiversionToModelDomain(AppStream) RESULT(lDest)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    LOGICAL                             :: lDest
    
    !Local variables
    INTEGER :: indx
    
    !Initialize
    lDest = .FALSE.
    
    DO indx=1,AppStream%AppDiverBypass%NDiver
      IF (AppStream%AppDiverBypass%Diver(indx)%Deli%Destination%iDestType .NE. f_iFlowDest_Outside) THEN
        lDest = .TRUE.
        RETURN
      END IF
    END DO
    
  END FUNCTION IsDiversionToModelDomain 
  
  
  ! -------------------------------------------------------------
  ! --- FUNCTION TO PREPARE THE BUDGET HEADER DATA FOR STREAM BUDGETS
  ! -------------------------------------------------------------
  FUNCTION PrepareStreamBudgetHeader(NLocations,iPrintReachBudgetOrder,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,cReachNames,iBudNodes) RESULT(Header)
    INTEGER,INTENT(IN)                   :: NLocations,iPrintReachBudgetOrder(:),iReachIDs(:),iStrmNodeIDs(:),NTIME
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)          :: cVersion
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cReachNames(:)
    INTEGER,OPTIONAL,INTENT(IN)          :: iBudNodes(:)
    TYPE(BudgetHeaderType)               :: Header
    
    !Local variables
    INTEGER,PARAMETER           :: TitleLen           = 199  , &
                                   NTitles            = 3    , &
                                   NColumnHeaderLines = 4    
    INTEGER                     :: iCount,indxLocation,indxCol,indx,I,ID,iReach,iMaxPathnameLen
    TYPE(TimeStepType)          :: TimeStepLocal
    CHARACTER                   :: UnitT*10,TextTime*17
    LOGICAL                     :: lNodeBudOutput
    CHARACTER(LEN=21),PARAMETER :: FParts(f_iNStrmBudColumns) = ['UPSTRM_INFLOW'         , &
                                                                 'DOWNSTRM_OUTFLOW'      , & 
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
                                          f_iVR ]  !Diversion shortage
      pLocation%iColWidth              = [17,(13,I=1,f_iNStrmBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        TextTime            = ArrangeText(TRIM(UnitT),17)
        pColumnHeaders(:,1) = ['                 ','     Upstream','   Downstream','    Tributary','        Tile ','             ','     Return  ','Gain from GW ',' Gain from GW','    Gain from','   Riparian ','             ','      By-pass','             ','    Diversion']
        pColumnHeaders(:,2) = ['      Time       ','      Inflow ','    Outflow  ','     Inflow  ','        Drain','       Runoff','      Flow   ','inside Model ','outside Model','      Lake   ','      ET    ','    Diversion','        Flow ','  Discrepancy','    Shortage ']
        pColumnHeaders(:,3) = [           TextTime,'       (+)   ','      (-)    ','      (+)    ','         (+) ','        (+)  ','      (+)    ','     (+)     ','      (+)    ','       (+)   ','      (-)   ','       (-)   ','        (-)  ','      (=)    ','             ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,14A13)'
        pFormatSpecs(2)     = '(A17,14A13)'
        pFormatSpecs(3)     = '(A17,14A13)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(f_iNStrmBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
                                                   
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
        ALLOCATE (pDSSOutput%cPathNames(f_iNStrmBudColumns*(Header%NLocations)) , pDSSOutput%iDataTypes(1))
        iMaxPathnameLen = LEN(pDSSOutput%cPathNames(1))
        iCount          = 1
        IF (lNodeBudOutput) THEN
            DO indxLocation=1,Header%NLocations
                DO indxCol=1,f_iNStrmBudColumns
                    pDSSOutput%cPathNames(iCount) = '/IWFM_STRMNODE_BUD/'                                          //  &  !A part
                                                    TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                                    'VOLUME/'                                                      //  &  !C part
                                                    '/'                                                            //  &  !D part
                                                    TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                                    TRIM(FParts(indxCol))//'/'                                            !F part
                    IF (LEN_TRIM(pDSSOutput%cPathNames(iCount)) .EQ. iMaxPathnameLen)   &
                        pDSSOutput%cPathNames(iCount)(iMaxPathnameLen:iMaxPathnameLen) = '/'
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
                    IF (LEN_TRIM(pDSSOutput%cPathNames(iCount)) .EQ. iMaxPathnameLen)   &
                        pDSSOutput%cPathNames(iCount)(iMaxPathnameLen:iMaxPathnameLen) = '/'
                    iCount = iCount+1
                END DO
            END DO
        END IF
        pDSSOutput%iDataTypes = f_iPER_CUM
    END ASSOCIATE
    
  END FUNCTION PrepareStreamBudgetHeader
  
  
  ! -------------------------------------------------------------
  ! --- REGISTER STREAM COMPONENT WITH MATRIX AND ADD CONNECTIVITY
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(AppStream,Matrix,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    TYPE(MatrixType)                    :: Matrix
    INTEGER,INTENT(OUT)                 :: iStat
     
    !Local variables
    INTEGER                    :: indxNode,iDim
    INTEGER,ALLOCATABLE        :: Temp_ConnectedNodes(:)
    TYPE(ConnectivityListType) :: ConnectivityLists(AppStream%NStrmNodes)
     
    !Add component to matrix
    CALL Matrix%AddComponent(f_iStrmComp,AppStream%NStrmNodes,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile connectivity list for each node
    DO indxNode=1,AppStream%NStrmNodes
        CALL AppStream%GetUpstrmNodes(indxNode,Temp_ConnectedNodes)
        iDim                                        = SIZE(Temp_ConnectedNodes) + 1
        ConnectivityLists(indxNode)%nConnectedNodes = iDim
        ALLOCATE (ConnectivityLists(indxNode)%ConnectedNodes(iDim))
        ConnectivityLists(indxNode)%ConnectedNodes(1)      = indxNode
        ConnectivityLists(indxNode)%ConnectedNodes(2:iDim) = Temp_ConnectedNodes       
    END DO
    
    !Add connectivity lists to Matrix
    CALL Matrix%AddConnectivity(f_iStrmComp,1,AppStream%NStrmNodes,f_iStrmComp,ConnectivityLists,iStat)   
    
  END SUBROUTINE RegisterWithMatrix
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ONE STREAM NODE (Node1) IS UPSTREAM OF ANOTHER (Node2)
  ! --- Note: If lBypass is TRUE then, Node2 is the bypass ID number
  ! -------------------------------------------------------------
  SUBROUTINE IsUpstreamNode(AppStream,Node1,Node2,lBypass,lUpstream,iStat)
    CLASS(BaseAppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)                  :: Node1,Node2
    LOGICAL,INTENT(IN)                  :: lBypass
    LOGICAL,INTENT(OUT)                 :: lUpstream
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    INTEGER :: BypassExportNode
    
    !Initialize
    iStat = 0
    
    IF (lBypass) THEN
        CALL AppStream%AppDiverBypass%GetBypassExportNode(Node2,BypassExportNode, iStat)
        IF (iStat .EQ. -1) RETURN
        lUpstream = StrmReach_IsUpstreamNode(AppStream%Reaches,Node1,BypassExportNode,AppStream%AppDiverBypass%Bypasses%iNode_Exp,AppStream%AppDiverBypass%Bypasses%FlowDestinationType)
    ELSE   
        lUpstream = StrmReach_IsUpstreamNode(AppStream%Reaches,Node1,Node2,AppStream%AppDiverBypass%Bypasses%iNode_Exp,AppStream%AppDiverBypass%Bypasses%FlowDestinationType)
    END IF
    
  END SUBROUTINE IsUpstreamNode
  

  ! -------------------------------------------------------------
  ! ---TRANSFER ANY TEXT/DSS OUTPUT TO HDF FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(AppStream,TimeStep,NTIME,iStat)
    CLASS(BaseAppStreamType)      :: AppStream
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    INTEGER,INTENT(OUT)           :: iStat
    
    !Stream flow hydrographs
    CALL AppStream%StrmHyd%Transfer_To_HDF(NTIME,TimeStep,iStat)
        
  END SUBROUTINE TransferOutputToHDF
  
  
  ! -------------------------------------------------------------
  ! --- OBTAIN TOTAL NUMBER OF STREAM NODES
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
    CALL DataFile%ReadData(iDummy,iStat)  ;  IF (iStat .EQ. -1) RETURN   !Number of points in rating table
    
  END SUBROUTINE CalculateNStrmNodes
   

  ! -------------------------------------------------------------
  ! --- ADD BYPASS
  ! -------------------------------------------------------------
  SUBROUTINE AddBypass(AppStream,ID,iNode_Exp,iColBypass,cName,rFracRecvLoss,rFracNonRecvLoss,iNRechargeElems,iRechargeElems,rRechargeFractions,iDestType,iDest,StrmLakeConnector,iStat)
    CLASS(BaseAppStreamType)    :: AppStream
    INTEGER,INTENT(IN)          :: ID,iNode_Exp,iColBypass,iNRechargeElems,iRechargeElems(iNRechargeElems),iDestType,iDest
    CHARACTER(LEN=*),INTENT(IN) :: cName
    REAL(8),INTENT(IN)          :: rFracRecvLoss,rFracNonRecvLoss,rRechargeFractions(iNRechargeElems)
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    INTEGER,INTENT(OUT)         :: iStat
    
    CALL AppStream%AppDiverBypass%AddBypass(ID,iNode_Exp,iColBypass,cName,rFracRecvLoss,rFracNonRecvLoss,iNRechargeElems,iRechargeElems,rRechargeFractions,iDestType,iDest,StrmLakeConnector,iStat)
    
  END SUBROUTINE AddBypass


END MODULE
