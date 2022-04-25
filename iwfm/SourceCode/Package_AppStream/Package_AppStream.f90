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
MODULE Package_AppStream
  USE Class_Version               , ONLY: VersionType                                      , &
                                          ReadVersion                                      
  USE MessageLogger               , ONLY: SetLastMessage                                   , &
                                          EchoProgress                                     , &
                                          MessageArray                                     , &
                                          f_iFatal                                         , &
                                          f_iWarn
  USE GeneralUtilities            , ONLY: IntToText                                        , &
                                          FindSubStringInString                            , &
                                          StripTextUntilCharacter 
  USE TimeSeriesUtilities         , ONLY: TimeStepType                                     , &
                                          NPeriods                                         , &
                                          IncrementTimeStamp                               , & 
                                          CTimeStep_To_RTimeStep                           , &
                                          GetJulianDatesBetweenTimeStampsWithTimeIncrement , &
                                          f_iTimeStampLength 
  USE IOInterface                 , ONLY: GenericFileType                                  , &
                                          DoesFileExist                                    , &
                                          f_iUNKNOWN
  USE Package_Discretization      , ONLY: StratigraphyType                                 , &
                                          AppGridType                                      
  USE Package_Misc                , ONLY: FlowDestinationType                              , &
                                          f_iDataUnitType_Length                           , &
                                          f_iDataUnitType_Volume                           
  USE Package_ComponentConnectors , ONLY: StrmLakeConnectorType                            , &
                                          StrmGWConnectorType                              , &
                                          SupplyType                                       , &
                                          SupplyToDestinationType                          , &
                                          SupplyDestinationConnectorType                   , &
                                          Supply_GetDestination                            
  USE StrmHydrograph              , ONLY: iHydFlow                                         
  USE Class_BaseAppStream         , ONLY: BaseAppStreamType                                , &                       
                                          f_iBudgetType_StrmNode                           , &
                                          f_iBudgetType_StrmReach                          , &
                                          f_iBudgetType_DiverDetail 
  USE Class_AppStream_v40         , ONLY: AppStream_v40_Type                               
  USE Class_AppStream_v41         , ONLY: AppStream_v41_Type                               
  USE Class_AppStream_v42         , ONLY: AppStream_v42_Type                               
  USE Class_AppStream_v421        , ONLY: AppStream_v421_Type                               
  USE Class_AppStream_v50         , ONLY: AppStream_v50_Type                               
  USE Class_AppDiverBypass        , ONLY: f_iDiverRecvLoss                                 , &
                                          f_iBypassRecvLoss                                , &
                                          f_iAllRecvLoss                                     
  USE Class_Diversion             , ONLY: DiversionType                                    , & 
                                          DeliveryType   
  USE Class_StrmNodeBudget        , ONLY: StrmNodeBudgetType   
  USE Package_Matrix              , ONLY: MatrixType                                       , &
                                          ConnectivityListType
  USE Package_Budget              , ONLY: BudgetType
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
  PUBLIC :: AppStreamType             , &
            StrmNodeBudgetType        , & 
            f_iDiverRecvLoss          , &
            f_iBypassRecvLoss         , &
            f_iAllRecvLoss            , &
            f_iBudgetType_StrmNode    , &
            f_iBudgetType_StrmReach   , &
            f_iBudgetType_DiverDetail 
  
  
  ! -------------------------------------------------------------
  ! --- STREAM COMPONENT FACADE DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppStreamType
      PRIVATE
      INTEGER                              :: iVersion = 0
      LOGICAL                              :: lDefined = .FALSE.
      CLASS(BaseAppStreamType),ALLOCATABLE :: Me
  CONTAINS
      PROCEDURE,PASS   :: AppStream_SetStaticComponent
      PROCEDURE,PASS   :: AppStream_SetStaticComponentFromBinFile
      PROCEDURE,PASS   :: AppStream_SetDynamicComponent
      PROCEDURE,PASS   :: AppStream_SetAllComponents
      PROCEDURE,PASS   :: AppStream_SetAllComponentsWithoutBinFile           
      PROCEDURE,PASS   :: Kill                            
      PROCEDURE,PASS   :: GetHydrograph_GivenAppStream
      PROCEDURE,NOPASS :: GetHydrograph_GivenFile
      PROCEDURE,PASS   :: GetBudget_List
      PROCEDURE,PASS   :: GetBudget_NColumns
      PROCEDURE,PASS   :: GetBudget_ColumnTitles
      PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenAppStream
      PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetBudget_TSData
      PROCEDURE,PASS   :: GetStrmNodeIDs                   
      PROCEDURE,PASS   :: GetStrmNodeID                    
      PROCEDURE,PASS   :: GetStrmNodeIndex
      PROCEDURE,PASS   :: GetReaches_ForStrmNodes
      PROCEDURE,PASS   :: GetNames                         
      PROCEDURE,PASS   :: GetHydrographTypeList
      PROCEDURE,PASS   :: GetNHydrographs 
      PROCEDURE,PASS   :: GetHydrographIDs                 
      PROCEDURE,PASS   :: GetHydrographCoordinates   
      PROCEDURE,PASS   :: GetNStrmNodes_WithBudget
      PROCEDURE,PASS   :: GetStrmNodeIDs_WithBudget
      PROCEDURE,PASS   :: GetNStrmNodes                                    
      PROCEDURE,PASS   :: GetNReaches                                        
      PROCEDURE,PASS   :: GetNDiver                         
      PROCEDURE,PASS   :: GetDiversionIDs
      PROCEDURE,PASS   :: GetDeliveryAtDiversion
      PROCEDURE,PASS   :: GetDiversionsForDeliveries
      PROCEDURE,PASS   :: GetActualDiversions_AtSomeDiversions
      PROCEDURE,PASS   :: GetActualDiversions_AtSomeNodes
      PROCEDURE,PASS   :: GetDiversionsExportNodes
      PROCEDURE,PASS   :: GetDiversionPurpose
      PROCEDURE,PASS   :: GetDiversionDestination          
      PROCEDURE,PASS   :: GetNBypass                       
      PROCEDURE,PASS   :: GetBypassDiverOriginDestData
      PROCEDURE,PASS   :: GetBypassIDs
      PROCEDURE,PASS   :: GetBypassReceived_FromABypass
      PROCEDURE,PASS   :: GetNetBypassInflows
      PROCEDURE,PASS   :: GetStrmBypassInflows
      PROCEDURE,PASS   :: GetBypassOutflows
      PROCEDURE,PASS   :: GetReachIDs  
      PROCEDURE,PASS   :: GetReachNNodes
      PROCEDURE,PASS   :: GetReachIndex
      PROCEDURE,PASS   :: GetUpstrmNodeFlags
      PROCEDURE,PASS   :: GetNUpstrmNodes
      PROCEDURE,PASS   :: GetUpstrmNodes
      PROCEDURE,PASS   :: GetStrmConnectivity              
      PROCEDURE,PASS   :: GetReachUpstrmNode                        
      PROCEDURE,PASS   :: GetReachDownstrmNode 
      PROCEDURE,PASS   :: GetNReaches_InUpstrmNetwork
      PROCEDURE,PASS   :: GetReaches_InUpstrmNetwork
      PROCEDURE,PASS   :: GetReachNUpstrmReaches
      PROCEDURE,PASS   :: GetReachUpstrmReaches
      PROCEDURE,PASS   :: GetReachStrmNodes
      PROCEDURE,PASS   :: GetReachOutflowDestType           
      PROCEDURE,PASS   :: GetReachOutflowDest                       
      PROCEDURE,PASS   :: GetStageFlowRatingTable           
      PROCEDURE,PASS   :: GetSupply                        => AppStream_GetSupply                    
      PROCEDURE,PASS   :: GetSupplySpecs                   => AppStream_GetSupplySpecs                    
      PROCEDURE,PASS   :: GetSupplyAdjustData              => AppStream_GetSupplyAdjustData          
      PROCEDURE,PASS   :: GetElemRecvLosses                => AppStream_GetElemRecvLosses            
      PROCEDURE,PASS   :: GetElemsWithRecvLoss             => AppStream_GetElemsWithRecvLoss         
      PROCEDURE,PASS   :: GetMaxDiversionRank              => AppStream_GetMaxDiversionRank          
      PROCEDURE,PASS   :: GetiColAdjust                     
      PROCEDURE,PASS   :: GetInflow_AtANode
      PROCEDURE,PASS   :: GetInflows_AtSomeNodes
      PROCEDURE,PASS   :: GetInflows_AtSomeInflows
      PROCEDURE,PASS   :: GetNInflows
      PROCEDURE,PASS   :: GetInflowNodes
      PROCEDURE,PASS   :: GetInflowIDs
      PROCEDURE,PASS   :: GetFlow                                               
      PROCEDURE,PASS   :: GetFlows                                              
      PROCEDURE,PASS   :: GetHeads                          
      PROCEDURE,PASS   :: GetStages                        
      PROCEDURE,PASS   :: GetHead_AtOneNode                                     
      PROCEDURE,NOPASS :: GetVersion                                          
      PROCEDURE,PASS   :: GetBottomElevations              => AppStream_GetBottomElevations          
      PROCEDURE,PASS   :: GetSubregionalRecvLosses         => AppStream_GetSubregionalRecvLosses     
      PROCEDURE,PASS   :: GetStrmConnectivityInGWNodes     => AppStream_GetStrmConnectivityInGWNodes 
      PROCEDURE,PASS   :: GetNRatingTablePoints                    
      PROCEDURE,PASS   :: IsDefined                        => AppStream_IsDefined
      PROCEDURE,PASS   :: IsRouted                         => AppStream_IsRouted
      PROCEDURE,PASS   :: IsDiversionsDefined              => AppStream_IsDiversionsDefined
      PROCEDURE,PASS   :: IsDiversionToModelDomain         => AppStream_IsDiversionToModelDomain
      PROCEDURE,PASS   :: IsUpstreamNode                   
      PROCEDURE,PASS   :: ReadTSData                                         
      PROCEDURE,PASS   :: ReadRestartData                  => AppStream_ReadRestartData                   
      PROCEDURE,PASS   :: PrintResults                     => AppStream_PrintResults 
      Procedure,PASS   :: PrintRestartData                 => AppStream_PrintRestartData
      PROCEDURE,PASS   :: WritePreprocessedData            => AppStream_WritePreprocessedData        
      PROCEDURE,PASS   :: WriteDataToTextFile              => AppStream_WriteDataToTextFile          
      PROCEDURE,PASS   :: ResetIrigFracs                   => AppStream_ResetIrigFracs               
      PROCEDURE,PASS   :: SetIrigFracsRead                 => AppStream_SetIrigFracsRead             
      PROCEDURE,PASS   :: SetSupplySpecs                   => AppStream_SetSupplySpecs               
      PROCEDURE,PASS   :: SetStreamFlow                    => AppStream_SetStreamFlow              
      PROCEDURE,PASS   :: SetStreamInflow                 
      PROCEDURE,PASS   :: SetBypassFlows_AtABypass
      PROCEDURE,PASS   :: SetDiversionRead
      PROCEDURE,PASS   :: UpdateHeads                      => AppStream_UpdateHeads                  
      PROCEDURE,PASS   :: ConvertTimeUnit                  => AppStream_ConvertTimeUnit
      PROCEDURE,PASS   :: ConvertFlowToElev                => AppStream_ConvertFlowToElev
      PROCEDURE,PASS   :: CheckSupplyDestinationConnection => AppStream_CheckSupplyDestinationConnection 
      PROCEDURE,PASS   :: ResetHeads                       => AppStream_ResetHeads                   
      PROCEDURE,PASS   :: AdvanceState                     => AppStream_AdvanceState                 
      PROCEDURE,PASS   :: Simulate                         => AppStream_Simulate 
      PROCEDURE,PASS   :: RegisterWithMatrix               => AppStream_RegisterWithMatrix
      PROCEDURE,PASS   :: TransferOutputToHDF              
      PROCEDURE,PASS   :: DestinationIDs_To_Indices                   
      PROCEDURE,PASS   :: AddBypass
      GENERIC          :: New                              => AppStream_SetStaticComponent                       , &
                                                              AppStream_SetStaticComponentFromBinFile            , &
                                                              AppStream_SetDynamicComponent                      , &
                                                              AppStream_SetAllComponents                         , &
                                                              AppStream_SetAllComponentsWithoutBinFile
      GENERIC          :: GetBudget_MonthlyFlows           => GetBudget_MonthlyFlows_GivenFile                   , &
                                                              GetBudget_MonthlyFlows_GivenAppStream
      GENERIC          :: GetHydrograph                    => GetHydrograph_GivenFile                            , &
                                                              GetHydrograph_GivenAppStream
  END TYPE AppStreamType
  

  ! -------------------------------------------------------------
  ! --- STREAM COMPONENT FACADE VERSION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion ='4.0.0000'
  INCLUDE 'Package_AppStream_Revision.fi'
 
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 19
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_AppStream::'

  

  
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
  SUBROUTINE AppStream_SetStaticComponent(AppStream,cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
    CLASS(AppStreamType),INTENT(OUT)      :: AppStream
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    LOGICAL,INTENT(IN)                    :: IsRoutedStreams
    TYPE(StrmGWConnectorType),INTENT(OUT) :: StrmGWConnector
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+28)         :: ThisProcedure = ModName // 'AppStream_SetStaticComponent'
    TYPE(GenericFileType)                :: AppStreamMainFile
    CHARACTER(:),ALLOCATABLE             :: cVersionLocal
    
    !Initialize
    iStat = 0

    !Return if no filename is defined
    IF (cFileName .EQ. '') RETURN

    !Open main control file and retrieve version number
    CALL AppStreamMainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(AppStreamMainFile,'STREAM',cVersionLocal,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file to reset it
    CALL AppStreamMainFile%Kill()
    
    !Instantiate stream component based on version
    SELECT CASE (TRIM(cVersionLocal))
        CASE ('4.0')
            ALLOCATE(AppStream_v40_Type :: AppStream%Me)
            CALL AppStream%Me%New(cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 40
            AppStream%lDefined = .TRUE.
        CASE ('4.1')
            ALLOCATE(AppStream_v41_Type :: AppStream%Me)
            CALL AppStream%Me%New(cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 41
            AppStream%lDefined = .TRUE.
        CASE ('4.2')
            ALLOCATE(AppStream_v42_Type :: AppStream%Me)
            CALL AppStream%Me%New(cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 42
            AppStream%lDefined = .TRUE.
        CASE ('4.21')
            ALLOCATE(AppStream_v421_Type :: AppStream%Me)
            CALL AppStream%Me%New(cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 421
            AppStream%lDefined = .TRUE.
        CASE ('5.0')
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(cVersionLocal)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT

  END SUBROUTINE AppStream_SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC COMPONENT STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmGWConnector,StrmLakeConnector,iStat)
    CLASS(AppStreamType)              :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+29) :: ThisProcedure = ModName // 'AppStream_SetDynamicComponent'
    TYPE(GenericFileType)        :: AppStreamMainFile
    INTEGER                      :: ErrorCode
    REAL(8)                      :: rVersionPre
    CHARACTER(:),ALLOCATABLE     :: cVersionSim
    
    !Initailize
    iStat = 0
    
    !Return if no filename is defined
    IF (cFileName .EQ. '') THEN
        !If static component of streams are defined, dynamic component must be defined as well
        IF (AppStream%lDefined) THEN
            MessageArray(1) = 'For proper simulation of streams, relevant stream data files must'
            MessageArray(2) = 'be specified when stream nodes are defined in Pre-Processor.'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        ELSE
            RETURN
        END IF
    END IF

    !Open main control file and retrieve version number
    CALL AppStreamMainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(AppStreamMainFile,'STREAM',cVersionSim,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file to reset it
    CALL AppStreamMainFile%Kill()
    
    !Make sure versions from static and dynamic components are the same
    ErrorCode   = 0
    rVersionPre = REAL(AppStream%iVersion)/10.0
    SELECT CASE (TRIM(cVersionSim))
        CASE ('4.0')
            IF (AppStream%iVersion .NE. 40) ErrorCode = 1
        CASE ('4.1')
            IF (AppStream%iVersion .NE. 41) ErrorCode = 1
        CASE ('4.2')
            IF (AppStream%iVersion .NE. 42) ErrorCode = 1
        CASE ('4.21')
            IF (AppStream%iVersion .NE. 421) ErrorCode = 1
        CASE ('5.0')
            IF (AppStream%iVersion .NE. 50) ErrorCode = 1
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(cVersionSim)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    IF (ErrorCode .EQ. 1) THEN
        MessageArray(1) = 'Stream Component versions used in Pre-Processor and Simulation must match!'
        WRITE(MessageArray(2),'(A,F3.1)') 'Version number in Pre-Processor = ',rVersionPre
        MessageArray(3) = 'Version number in Simulation    = ' // TRIM(cVersionSim)
        CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Instantiate the dynamic component
    CALL AppStream%Me%New(IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
        
  END SUBROUTINE AppStream_SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- READ PRE-PROCESSED STATIC COMPONENT FROM BINARY FILE 
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetStaticComponentFromBinFile(AppStream,BinFile,iStat)
    CLASS(AppStreamType),INTENT(OUT) :: AppStream
    TYPE(GenericFileType)            :: BinFile
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+39) :: ThisProcedure = ModName // 'AppStream_SetStaticComponentFromBinFile'
    INTEGER                      :: iVersion
    
    !Read version number from binary file
    CALL BinFile%ReadData(iVersion,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Return if version number is zero (streams are not simulated)
    IF (iVersion .EQ. 0) RETURN
    
    !If AppStream is already instantiated kill first
    IF (ALLOCATED(AppStream%Me)) CALL AppStream%Kill()
  
    !Instantiate stream component based on version
    SELECT CASE (iVersion)
        CASE (40)
            ALLOCATE(AppStream_v40_Type :: AppStream%Me)
            CALL AppStream%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 40
            AppStream%lDefined = .TRUE.
        CASE (41)
            ALLOCATE(AppStream_v41_Type :: AppStream%Me)
            CALL AppStream%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 41
            AppStream%lDefined = .TRUE.
        CASE (42)
            ALLOCATE(AppStream_v42_Type :: AppStream%Me)
            CALL AppStream%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 42
            AppStream%lDefined = .TRUE.
        CASE (421)
            ALLOCATE(AppStream_v421_Type :: AppStream%Me)
            CALL AppStream%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 421
            AppStream%lDefined = .TRUE.
        CASE (50)
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(IntToText(iVersion))//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE AppStream_SetStaticComponentFromBinFile
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStreamType),INTENT(OUT)  :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME,iLakeIDs(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(GenericFileType)             :: BinFile
    TYPE(StrmLakeConnectorType)       :: StrmLakeConnector
    TYPE(StrmGWConnectorType)         :: StrmGWConnector
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure = ModName // 'AppStream_SetAllComponents'
    INTEGER                      :: iVersion
    
    !Initialize
    iStat = 0
    
    !If a binary file is supplied, read the flag to see if streams are simulated 
    IF (BinFile%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL BinFile%ReadData(iVersion,iStat)  
        IF (iStat .EQ. -1) RETURN
        IF (iVersion .EQ. 0) RETURN
    END IF

    !Return if a Simulation filename is not specified
    IF (cFileName .EQ. ''  .OR.  BinFile%iGetFileType() .EQ. f_iUNKNOWN) RETURN
    
    !If AppStream is already instantiated, kill first
    IF (ALLOCATED(AppStream%Me)) CALL AppStream%Kill()
    
    !Instantiate stream component based on version
    SELECT CASE (iVersion)
        CASE (40)
            ALLOCATE(AppStream_v40_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 40
            AppStream%lDefined = .TRUE.
        CASE (41)
            ALLOCATE(AppStream_v41_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 41
            AppStream%lDefined = .TRUE.
        CASE (42)
            ALLOCATE(AppStream_v42_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 42
            AppStream%lDefined = .TRUE.
        CASE (421)
            ALLOCATE(AppStream_v421_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 421
            AppStream%lDefined = .TRUE.
        CASE (50)
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,iLakeIDs,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(IntToText(iVersion))//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE AppStream_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStreamType),INTENT(OUT)      :: AppStream
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
    CHARACTER(LEN=ModNameLen+40) :: ThisProcedure = ModName // 'AppStream_SetAllComponentsWithoutBinFile'
    TYPE(GenericFileType)        :: MainFile
    CHARACTER(:),ALLOCATABLE     :: cVersionPre
    
    !Initialize
    iStat = 0
    
    !Return if a Simulation filename is not specified
    IF (cSimFileName .EQ. ''  .OR.  cPPFileName .EQ. '') RETURN
    
    !Open file and read the version number line to decide which component to instantiate
    CALL MainFile%New(FileName=cPPFileName,InputFile=.TRUE.,Descriptor='main stream data file',iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(MainFile,'STREAM',cVersionPre,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close main input file to reset it
    CALL MainFile%Kill()
    
    !Instantiate stream component based on version
    SELECT CASE (TRIM(cVersionPre))
        CASE ('4.0')
            ALLOCATE(AppStream_v40_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 40
            AppStream%lDefined = .TRUE.
        CASE ('4.1')
            ALLOCATE(AppStream_v41_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 41
            AppStream%lDefined = .TRUE.
        CASE ('4.2')
            ALLOCATE(AppStream_v42_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 42
            AppStream%lDefined = .TRUE.
        CASE ('4.21')
            ALLOCATE(AppStream_v421_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 421
            AppStream%lDefined = .TRUE.
        CASE ('5.0')
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,iLakeIDs,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(cVersionPre)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE AppStream_SetAllComponentsWithoutBinFile


  
  
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
    CLASS(AppStreamType) :: AppStream
    
    !Local variables
    INTEGER :: ErrorCode
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%Kill()
        DEALLOCATE (AppStream%Me , STAT=ErrorCode)
        AppStream%iVersion = 0
        AppStream%lDefined = .FALSE.
    END IF
    
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
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: iNStrmNodeBud
    
    IF (ALLOCATED(AppStream%Me)) THEN
        iNStrmNodeBud = AppStream%Me%GetNStrmNodes_WithBudget()
    ELSE
        iNStrmNodeBud = 0
    END IF
    
  END FUNCTION GetNStrmNodes_WithBudget
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDS WITH BUDGET OUTPUT 
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmNodeIDs_WithBudget(AppStream,iIDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iIDs(:)
    
    IF (ALLOCATED(AppStream%Me)) THEN
        CALL AppStream%Me%GetStrmNodeIDs_WithBudget(iIDs)
    ELSE
        ALLOCATE (iIDs(0))
    END IF
    
  END SUBROUTINE GetStrmNodeIDs_WithBudget
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL DIVERSIONS FROM SOME NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetActualDiversions_AtSomeNodes(AppStream,iNodes,rDivers,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iNodes(:)
    REAL(8),INTENT(OUT)             :: rDivers(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    CALL AppStream%Me%GetActualDiversions_AtSomeNodes(iNodes,rDivers,iStat)
    
  END SUBROUTINE GetActualDiversions_AtSomeNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL DIVERSIONS FOR SOME DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetActualDiversions_AtSomeDiversions(AppStream,iDivers,rDivers,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iDivers(:)
    REAL(8),INTENT(OUT)             :: rDivers(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    CALL AppStream%Me%GetActualDiversions_AtSomeDiversions(iDivers,rDivers,iStat)
    
  END SUBROUTINE GetActualDiversions_AtSomeDiversions
  
  
  ! -------------------------------------------------------------
  ! --- GET DIVERSIONS FOR A SPECIFIED DELIVERIES; I.E. ADD LOSSES TO DELIVERIES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetDiversionsForDeliveries(AppStream,iDivers,rDelis,rDivers)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iDivers(:)
    REAL(8),INTENT(IN)              :: rDelis(:)
    REAL(8),INTENT(OUT)             :: rDivers(:)
    
    CALL AppStream%Me%GetDiversionsForDeliveries(iDivers,rDelis,rDivers)
    
  END SUBROUTINE GetDiversionsForDeliveries
  
  
  ! -------------------------------------------------------------
  ! --- GET DELIVERY RELATED TO A DIVERSION
  ! -------------------------------------------------------------
  PURE FUNCTION GetDeliveryAtDiversion(AppStream,iDiver) RESULT(rDeli)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iDiver
    REAL(8)                         :: rDeli
    
    IF (AppStream%lDefined) THEN
        rDeli = AppStream%Me%GetDeliveryAtDiversion(iDiver)
    ELSE
        rDeli = 0.0
    END IF
    
  END FUNCTION GetDeliveryAtDiversion
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES FOR A GIVEN SET OF DIVERSION INDICES
  ! -------------------------------------------------------------
  SUBROUTINE GetDiversionsExportNodes(AppStream,iDivList,iStrmNodeList)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iDivList(:)
    INTEGER,INTENT(OUT)             :: iStrmNodeList(:)
    
    !If streams are not defined, return
    IF (AppStream%iVersion .EQ. 0) THEN
        iStrmNodeList = 0
        RETURN
    END IF
    
    !Get the stream nodes for diversions
    CALL AppStream%Me%GetDiversionsExportNodes(iDivList,iStrmNodeList)
    
  END SUBROUTINE GetDiversionsExportNodes


  ! -------------------------------------------------------------
  ! --- GET PURPOSE OF DIVERSIONS (IF THEY SERVE AG, URBAN OR BOTH) BEFORE ANY ADJUSTMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetDiversionPurpose(AppStream,iDivers,iAgOrUrban,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iDivers(:)
    INTEGER,INTENT(OUT)             :: iAgOrUrban(:),iStat
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetDiversionPurpose(iDivers,iAgOrUrban,iStat)
    END IF
    
  END SUBROUTINE GetDiversionPurpose

  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH FROM AppStream OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrograph_GivenAppStream(AppStream,TimeStep,iHydIndex,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat) 
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: iHydIndex
    REAL(8),INTENT(IN)              :: rFact_LT,rFact_VL
    CHARACTER(LEN=*),INTENT(IN)     :: cBeginDate,cEndDate,cInterval
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rDates(:),rValues(:)
    INTEGER,INTENT(OUT)             :: iDataUnitType,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+28) :: ThisProcedure = ModName // 'GetHydrograph_GivenAppStream'
    CHARACTER(:),ALLOCATABLE     :: cFileName
    
    !Return if AppStream is not defined
    IF (.NOT. AppStream%lDefined) THEN
       ALLOCATE (rDates(0) , rValues(0))
       iDataUnitType = -1
       iStat         = 0
       RETURN
    END IF
    
    !Get filename
    CALL AppStream%Me%GetHydOutputFileName(cFileName)
    
    !If filename is empty, return with error
    IF (LEN_TRIM(cFileName) .EQ. 0) THEN
        CALL SetLastMessage('Stream hydrographs are not part of model output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Replace filename extension with hdf
    cFileName = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf' 
    
    !Retrieve data
    CALL GetHydrograph_GivenFile(cFileName,TimeStep,iHydIndex,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat)

  END SUBROUTINE GetHydrograph_GivenAppStream
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH FROM A GIVEN FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrograph_GivenFile(cFileName,TimeStep,iHydIndex,rFact_LT,rFact_VL,cBeginDate,cEndDate,cInterval,iDataUnitType,rDates,rValues,iStat) 
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: iHydIndex
    REAL(8),INTENT(IN)              :: rFact_LT,rFact_VL
    CHARACTER(LEN=*),INTENT(IN)     :: cBeginDate,cEndDate,cInterval
    REAL(8),ALLOCATABLE,INTENT(OUT) :: rDates(:),rValues(:)
    INTEGER,INTENT(OUT)             :: iDataUnitType,iStat
    
    !Local variables
    INTEGER               :: iHydType,iFileReadCode,iNData,iErrorCode,iNDataReturn,iDeltaT_InMinutes,iNIntervals
    REAL(8)               :: rFactor,rDummy
    CHARACTER             :: cDate*f_iTimeStampLength
    REAL(8),ALLOCATABLE   :: rDates_Local(:),rValues_Local(:)
    TYPE(GenericFileType) :: InFile
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read hydrograph type (flow or stream surface elevation?)
    CALL InFile%ReadData('/Stream_Hydrographs','HydrographType',ScalarAttrData=iHydType,iStat=iStat)
    
    !Data type and conversion factor
    IF (iHydType .EQ. iHydFlow) THEN
        iDataUnitType = f_iDataUnitType_Volume
        rFactor       = rFact_VL
    ELSE
        iDataUnitType = f_iDataUnitType_Length
        rFactor       = rFact_LT
    END IF
    
    !Number of timesteps for which data will be read
    iNData = NPeriods(TimeStep%DELTAT_InMinutes,cBeginDate,cEndDate) + 1
    ALLOCATE (rValues_Local(iNData) , rDates_Local(iNData))
    
    !Julian dates for data
    CALL GetJulianDatesBetweenTimeStampsWithTimeIncrement(TimeStep%DeltaT_InMinutes,cBeginDate,cEndDate,rDates_Local)
    
    !Read data
    CALL InFile%ReadData(cBeginDate,1,iHydIndex,rValues_Local,iFileReadCode,iStat)
    IF (iStat .NE. 0) GOTO 10
    
    !Number of timesteps to sample data
    CALL CTimeStep_To_RTimeStep(cInterval,rDummy,iDeltaT_InMinutes,iStat)  
    IF (iStat .NE. 0) GOTO 10
    iNDataReturn = NPeriods(iDELTAT_InMinutes,cBeginDate,cEndDate) + 1
    ALLOCATE (rDates(iNDataReturn) , rValues(iNDataReturn))
    
    !Sample read data into return argument
    IF (iNData .EQ. iNDataReturn) THEN
        rValues = rValues_Local * rFactor
        rDates  = rDates_Local
    ELSE
        cDate       = IncrementTimeStamp(cBeginDate,iDeltaT_InMinutes,1)
        iNIntervals = NPeriods(TimeStep%DELTAT_InMinutes,cBeginDate,cDate)
        rValues     = rValues_Local(iNIntervals::iNIntervals) * rFactor
        rDates      = rDates_Local(iNIntervals::iNIntervals)
    END IF
    
    !Close file
10  CALL InFile%Kill()
            
    !Clear memory
    DEALLOCATE (rValues_Local , rDates_Local , STAT=iErrorCode)
    
  END SUBROUTINE GetHydrograph_GivenFile
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(AppStream,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(AppStreamType),INTENT(IN)          :: AppStream
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetBudget_List(iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    ELSE
        ALLOCATE (iBudgetTypeList(0),iBudgetLocationTypeList(0),cBudgetDescriptions(0),cBudgetFiles(0))
    ENDIF
    
  END SUBROUTINE GetBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(AppStream,iBudgetType,iLocationIndex,iNCols,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iBudgetType,iLocationIndex
    INTEGER,INTENT(OUT)             :: iNCols,iStat       
        
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetBudget_NColumns(iBudgetType,iLocationIndex,iNCols,iStat)
    ELSE
        iStat  = 0
        iNCols = 0
    ENDIF
        
  END SUBROUTINE GetBudget_NColumns


  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN TITLES (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(AppStream,iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(AppStreamType),INTENT(IN)          :: AppStream
    INTEGER,INTENT(IN)                       :: iBudgetType,iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)       
    INTEGER,INTENT(OUT)                      :: iStat
        
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetBudget_ColumnTitles(iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    ENDIF
        
  END SUBROUTINE GetBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM AppStream OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenAppStream(AppStream,iBudgetType,iLocationIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(AppStreamType),INTENT(IN)          :: AppStream
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLocationIndex !Location can be stream node, reach or diversion
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)     !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    IF (AppStream%lDefined) THEN    
        CALL AppStream%Me%GetBudget_MonthlyFlows_GivenAppStream(iBudgetType,iLocationIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    ELSE
        iStat = 0
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
    END IF
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenAppStream
  
  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLocationID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget          !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLocationID !Location can be stream node, reach or diversion
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)     !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'GetBudget_MonthlyFlows_GivenFile'
    INTEGER                      :: iErrorCode  
    CHARACTER(:),ALLOCATABLE     :: cVersion
    TYPE(AppStreamType)          :: AppStream
    
    !Get version number
    CALL GetPackageVersion(Budget,cVersion)
    
    !Based on component version, allocate base stream type
    SELECT CASE (cVersion)
        CASE ('4.0')
            ALLOCATE(AppStream_v40_Type :: AppStream%Me)
        CASE ('4.1')
            ALLOCATE(AppStream_v41_Type :: AppStream%Me)
        CASE ('4.2')
            ALLOCATE(AppStream_v42_Type :: AppStream%Me)
        CASE ('4.21')
            ALLOCATE(AppStream_v421_Type :: AppStream%Me)
        CASE ('5.0')
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(cVersion)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Get monthly data    
    CALL AppStream%Me%GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLocationID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    
    !Clear memory
    DEALLOCATE (AppStream%Me , cVersion , STAT=iErrorCode)
    
  CONTAINS
   
    !#######################################################
    !### FIND THE VERSION NUMBER OF THE STREAM PACKAGE USED
    !#######################################################
    SUBROUTINE GetPackageVersion(Budget,cVersion)
      TYPE(BudgetType),INTENT(IN) :: Budget
      CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cVersion
      
      !Local variables
      INTEGER                      :: iNTitles,iLenTitles,indx,iLoc
      CHARACTER(LEN=:),ALLOCATABLE :: cTitles(:)
      CHARACTER(:),ALLOCATABLE     :: cTitlesConc
     
      !Get the ASCII titles from the Budget file; one of these titles include component version number
      iNTitles   = Budget%GetNPersistentTitles()
      iLenTitles = Budget%GetTitleLen()
      ALLOCATE (CHARACTER(iLenTitles) :: cTitles(iNTitles))
      cTitles = Budget%GetPersistentTitles(iNTitles)
      
      !Concotonate titles
      ALLOCATE (CHARACTER(LEN=iNTitles*iLenTitles) :: cTitlesConc)
      cTitlesConc = ''
      DO indx=1,iNTitles
          cTitlesConc = TRIM(cTitlesConc) // TRIM(ADJUSTL(cTitles(indx)))
      END DO  
      
      !Check for version 4.0
      CALL FindSubStringInString('v4.0.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '4.0'
          RETURN
      END IF 
      
      !Check for version 4.1
      CALL FindSubStringInString('v4.1.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '4.1'
          RETURN
      END IF  

      !Check for version 4.2
      CALL FindSubStringInString('v4.2.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '4.2'
          RETURN
      END IF  

      !Check for version 4.21
      CALL FindSubStringInString('v4.21.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(4) :: cVersion)
          cVersion = '4.21'
          RETURN
      END IF  

      !Check for version 5.0
      CALL FindSubStringInString('v5.0.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '5.0'
          RETURN
      END IF 
      
      !If made to this point, something is wrong
      ALLOCATE (CHARACTER(3) :: cVersion)
      cVersion = '0.0'

    END SUBROUTINE GetPackageVersion
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AppStream,iBudgetType,iLocationIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iBudgetType,iLocationIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)     :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)              :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)             :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)             :: iDataTypes(:),inActualOutput,iStat
    
    IF (AppStream%lDefined) THEN    
        CALL AppStream%Me%GetBudget_TSData(iBudgetType,iLocationIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    ELSE
        iStat          = 0
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
    END IF
           
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetStrmNodeIDs(AppStream,iStrmNodeIDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: iStrmNodeIDs(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetStrmNodeIDs(iStrmNodeIDs)
    ENDIF
    
  END SUBROUTINE GetStrmNodeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE ID GIVEN INDEX
  ! -------------------------------------------------------------
  FUNCTION GetStrmNodeID(AppStream,indx) RESULT(iStrmNodeID)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: indx
    INTEGER                         :: iStrmNodeID
    
    IF (AppStream%lDefined) THEN
        iStrmNodeID = AppStream%Me%GetStrmNodeID(indx)
    ELSE
        iStrmNodeID = 0
    ENDIF
    
  END FUNCTION GetStrmNodeID
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INDEX GIVEN ID
  ! -------------------------------------------------------------
  PURE FUNCTION GetStrmNodeIndex(AppStream,iNodeID) RESULT(Index)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iNodeID
    INTEGER                         :: Index
    
    IF (AppStream%lDefined) THEN
        Index = AppStream%Me%GetStrmNodeIndex(iNodeID)
    ELSE
        Index = 0
    END IF
    
  END FUNCTION GetStrmNodeIndex
  
  
  ! -------------------------------------------------------------
  ! --- GET NAMES OF LOCATIONS FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetNames(AppStream,iLocationType,cNamesList)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iLocationType
    CHARACTER(LEN=*),INTENT(OUT)    :: cNamesList(:)  !Assumes array is previously dimensioned according to the number of locations
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetNames(iLocationType,cNamesList)
    ELSE
        cNamesList = ''
    END IF
    
  END SUBROUTINE GetNames
  
    
  ! -------------------------------------------------------------
  ! --- GET A LIST OF HYDROGRAPH TYPES AVAILABLE FOR RETRIEVAL
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographTypeList(AppStream,cHydTypeList,iHydLocationTypeList,cHydFileList)
    CLASS(AppStreamType),INTENT(IN)          :: AppStream
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cHydTypeList(:),cHydFileList(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iHydLocationTypeList(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetHydrographTypeList(cHydTypeList,iHydLocationTypeList,cHydFileList)
    ELSE
        ALLOCATE(cHydTypeList(0) , iHydLocationTypeList(0) , cHydFileList(0))
    END IF
        
  END SUBROUTINE GetHydrographTypeList
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF HYDROGRAPHS PRINTED
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(AppStream) RESULT(NHydrographs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NHydrographs
    
    IF (AppStream%lDefined) THEN
        NHydrographs = AppStream%Me%StrmHyd%GetNHydrographs()
    ELSE
        NHydrographs = 0
    END IF

  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE IDS WHERE HYDROGRAPHS AREPRINTED
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(AppStream,IDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: IDs(:)
    
    !Localk variables
    INTEGER,ALLOCATABLE :: iStrmNodeIDs(:)
    
    IF (AppStream%lDefined) THEN
        !Return if the size of IDs array is zero
        IF (SIZE(IDs) .EQ. 0) RETURN
    
        ALLOCATE (iStrmNodeIDs(AppStream%Me%NStrmNodes))
        CALL AppStream%Me%GetStrmNodeIDs(iStrmNodeIDs)
        CALL AppStream%Me%StrmHyd%GetHydrographNodes(IDs)
        
        IDs = iStrmNodeIDs(IDs)
        
    END IF

  END SUBROUTINE GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET COORDINATES OF HYDROGRAPHS PRINTED
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(AppStream,StrmGWConnector,GridX,GridY,XHyd,YHyd)
    CLASS(AppStreamType),INTENT(IN)      :: AppStream
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    REAL(8),INTENT(IN)                   :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)                  :: XHyd(:),YHyd(:)
    
    !Local variables
    INTEGER,ALLOCATABLE :: iGWNodes(:)
    
    !Return if streams are not modeled
    IF (.NOT. AppStream%lDefined) RETURN
        
    !Get the corresponding gw nodes
    CALL StrmGWConnector%GetAllGWNodes(iGWNodes)
    
    CALL AppStream%Me%StrmHyd%GetCoordinates(iGWNodes,GridX,GridY,XHyd,YHyd)

  END SUBROUTINE GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET DESTINATION FOR DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetDiversionDestination(AppStream,Destination)
    CLASS(AppStreamType),INTENT(IN)       :: AppStream
    TYPE(FlowDestinationType),ALLOCATABLE :: Destination(:)
    
    IF (AppStream%lDefined) CALL Supply_GetDestination(AppStream%Me%AppDiverBypass%Diver%Deli , Destination)
    
  END SUBROUTINE GetDiversionDestination
  
  
  ! -------------------------------------------------------------
  ! --- GET DIVERSION IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_GetDiversionIDs(AppStream,IDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: IDs(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetDiversionIDs(IDs)
    
  END SUBROUTINE AppStream_GetDiversionIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET DIVERSION SUPPLY SPECS
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_GetSupplySpecs(AppStream,DiverSpecs)
    CLASS(AppStreamType),INTENT(IN)          :: AppStream
    TYPE(SupplyType),ALLOCATABLE,INTENT(OUT) :: DiverSpecs(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetSupplySpecs(DiverSpecs)
    ELSE
        ALLOCATE (DiverSpecs(0))
    END IF
    
  END SUBROUTINE AppStream_GetSupplySpecs
  
    
  ! -------------------------------------------------------------
  ! --- GET RATING TABLE (STAGE VS. FLOW) AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE GetStageFlowRatingTable(AppStream,iNode,Stage,Flow)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iNode
    REAL(8),INTENT(OUT)             :: Stage(:),Flow(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetStageFlowRatingTable(iNode,Stage,Flow)
    ELSE
        Stage = 0.0
        Flow  = 0.0
    END IF
    
  END SUBROUTINE GetStageFlowRatingTable
  
  
  ! -------------------------------------------------------------
  ! --- GET REACH DESTINATION
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachOutflowDest(AppStream,iReach) RESULT(iDest)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iDest

    IF (AppStream%lDefined) THEN
        iDest = AppStream%Me%GetReachOutflowDest(iReach)
    ELSE
        iDest = -1
    END IF
    
  END FUNCTION GetReachOutflowDest

  
  ! -------------------------------------------------------------
  ! --- GET REACH DESTINATION TYPE
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachOutflowDestType(AppStream,iReach) RESULT(iDestType)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iDestType
    
    IF (AppStream%lDefined) THEN
        iDestType = AppStream%Me%GetReachOutflowDestType(iReach)
    ELSE
        iDestType = -1
    END IF
    
  END FUNCTION GetReachOutflowDestType


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM NODES IN A REACH DEFINED WITH ITS INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachNNodes(AppStream,iReach) RESULT(iNNodes)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iNNodes
    
    IF (AppStream%lDefined) THEN
        iNNodes = Appstream%Me%GetReachNNodes(iReach)
    ELSE
        iNNodes = 0
    END IF
    
  END FUNCTION GetReachNNodes

  
  ! -------------------------------------------------------------
  ! --- GET DOWNSTREAM NODE FOR A REACH
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachDownstrmNode(AppStream,iReach) RESULT(iNode)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iNode

    IF (AppStream%lDefined) THEN
        iNode = Appstream%Me%GetReachDownstrmNode(iReach)
    ELSE
        iNode = 0
    END IF
    
  END FUNCTION GetReachDownstrmNode
    

  ! -------------------------------------------------------------
  ! --- GET UPSTREAM NODE FLAGS FOR ALL STREAM NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetUpstrmNodeFlags(AppStream,lUpstrmNode)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL,INTENT(OUT)             :: lUpstrmNode(:)
    
    IF (AppStream%lDefined) THEN
        lUpstrmNode = Appstream%Me%GetUpstrmNodeFlags()
    ELSE
        lUpstrmNode = .FALSE.
    END IF
    
  END SUBROUTINE GetUpstrmNodeFlags

  
  ! -------------------------------------------------------------
  ! --- GET UPSTREAM NODE FOR A REACH
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachUpstrmNode(AppStream,iReach) RESULT(iNode)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iNode
    
    IF (AppStream%lDefined) THEN
        iNode = Appstream%Me%GetReachUpstrmNode(iReach)
    ELSE
        iNode = 0
    END IF
    
  END FUNCTION GetReachUpstrmNode
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODES FOR A GIVEN REACH 
  ! -------------------------------------------------------------
  SUBROUTINE GetReachStrmNodes(AppStream,iReach,iStrmNodes,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER,ALLOCATABLE             :: iStrmNodes(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'GetReachStrmNodes'
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetReachStrmNodes(iReach,iStrmNodes,iStat)
    ELSE
        CALL SetLastMessage('Streams are not defined to retrieve stream node IDs for a given reach!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
  END SUBROUTINE GetReachStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ALL REACHES WITHIN THE NETWORK UPSTREAM FROM A REACH GIVEN WITH ITS INDEX
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  FUNCTION GetNReaches_InUpstrmNetwork(AppStream,iReach) RESULT(iNReaches)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iNReaches
    
    !Return if AppStream object is not instantiated
    IF (.NOT. AppStream%lDefined) THEN
        iNReaches = 0
        RETURN
    END IF
    
    !Get number of reaches upstream
    iNReaches = AppStream%Me%GetNReaches_InUpstrmNetwork(iReach)
    
  END FUNCTION GetNReaches_InUpstrmNetwork
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL REACH NETWORK UPSTREAM FROM A REACH GIVEN WITH ITS INDEX
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  SUBROUTINE GetReaches_InUpstrmNetwork(AppStream,iReach,iUpstrmReaches)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER,ALLOCATABLE             :: iUpstrmReaches(:)
    
    !Return if AppStream object is not instantiated
    IF (.NOT. AppStream%lDefined) THEN
        ALLOCATE(iUpstrmReaches(0))
        RETURN
    END IF
    
    !Get reaches upstream
    CALL AppStream%Me%GetReaches_InUpstrmNetwork(iReach,iUpstrmReaches)
    
  END SUBROUTINE GetReaches_InUpstrmNetwork
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF REACHES IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  FUNCTION GetReachNUpstrmReaches(AppStream,iReach) RESULT(iNReaches)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iNReaches
    
    !Return if AppStream object is not instantiated
    IF (.NOT. AppStream%lDefined) THEN
        iNReaches = 0
        RETURN
    END IF
    
    !Get number of reaches upstream
    iNReaches = AppStream%Me%GetReachNUpstrmReaches(iReach)
    
  END FUNCTION GetReachNUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET REACHES IMMEDIATELY UPSTREAM OF A GIVEN REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  SUBROUTINE GetReachUpstrmReaches(AppStream,iReach,iUpstrmReaches)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER,ALLOCATABLE             :: iUpstrmReaches(:)
    
    !Return if AppStream object is not instantiated
    IF (.NOT. AppStream%lDefined) THEN
        ALLOCATE(iUpstrmReaches(0))
        RETURN
    END IF
    
    !Get reaches upstream
    CALL AppStream%Me%GetReachUpstrmReaches(iReach,iUpstrmReaches)
    
  END SUBROUTINE GetReachUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE CONNECTIVITY
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmConnectivity(AppStream,StrmConnectivity)
    CLASS(AppStreamType),INTENT(IN)        :: AppStream
    TYPE(ConnectivityListType),ALLOCATABLE :: StrmConnectivity(:)
    
    !Local variables
    INTEGER :: indxNode
    
    IF (AppStream%lDefined) THEN
        ALLOCATE (StrmConnectivity(AppStream%Me%NStrmNodes))
        DO indxNode=1,AppStream%Me%NStrmNodes
            CALL AppStream%Me%GetUpstrmNodes(indxNode,StrmConnectivity(indxNode)%ConnectedNodes)
            StrmConnectivity(indxNode)%nConnectedNodes = SIZE(StrmConnectivity(indxNode)%ConnectedNodes)
        END DO
    ELSE
        ALLOCATE (StrmConnectivity(0))
    END IF
    
  END SUBROUTINE GetStrmConnectivity
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF STREAM NODES DRAINING INTO A NODE
  ! -------------------------------------------------------------
  FUNCTION GetNUpstrmNodes(AppStream,iStrmNode) RESULT(iNNodes)
     CLASS(AppStreamType),INTENT(IN) :: AppStream
     INTEGER,INTENT(IN)              :: iStrmNode
     INTEGER                         :: iNNodes
     
    IF (AppStream%lDefined) THEN
        iNNodes = AppStream%Me%GetNUpstrmNodes(iStrmNode) 
    ELSE
        iNNodes = 0
    END IF
    
  END FUNCTION GetNUpstrmNodes
     
     
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES FLOWING INTO ANOTHER NODE
  ! -------------------------------------------------------------
  SUBROUTINE GetUpstrmNodes(AppStream,iStrmNode,iUpstrmNodes)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iStrmNode
    INTEGER,ALLOCATABLE             :: iUpstrmNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initailize
    DEALLOCATE (iUpstrmNodes , STAT=ErrorCode)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetUpstrmNodes(iStrmNode,iUpstrmNodes) 
    ELSE
        ALLOCATE (iUpstrmNodes(0))
    END IF
    
  END SUBROUTINE GetUpstrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT RELATED DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetSupplyAdjustData(AppStream,iDiverRank,iColAdjust,DeliRequired,DeliMax,DeliActual,IrigFracs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: iDiverRank(:),iColAdjust(:)
    REAL(8),INTENT(OUT)             :: DeliRequired(:),DeliMax(:),DeliActual(:),IrigFracs(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetSupplyAdjustData(iDiverRank,iColAdjust,DeliRequired,DeliMax,DeliActual,IrigFracs)
    
  END SUBROUTINE AppStream_GetSupplyAdjustData
  
  
  ! -------------------------------------------------------------
  ! --- GET DELIVERY SUPPLY TO ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetSupply(AppStream,DeliDestConnector,Supply_Ag,Supply_Urb)
    CLASS(AppStreamType),INTENT(IN)                 :: AppStream
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: DeliDestConnector
    REAL(8),INTENT(OUT)                             :: Supply_Ag(:),Supply_Urb(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetSupply(DeliDestConnector,Supply_Ag,Supply_Urb)
    
  END SUBROUTINE AppStream_GetSupply
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM CONNECTIVITY IN TERMS OF GW NODES AS A SET OF COMPLEX NUMBERS 
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetStrmConnectivityInGWNodes(AppStream,StrmGWConnector,Connectivity)
    CLASS(AppStreamType),INTENT(IN)      :: AppStream
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    COMPLEX,ALLOCATABLE,INTENT(OUT)      :: Connectivity(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetStrmConnectivityInGWNodes(StrmGWConnector,Connectivity)
    
  END SUBROUTINE AppStream_GetStrmConnectivityInGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTS THAT RECEIVE RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetElemsWithRecvLoss(AppStream,iSource,Elems)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iSource
    INTEGER,ALLOCATABLE,INTENT(OUT) :: Elems(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetElemsWithRecvLoss(iSource,Elems)
    
  END SUBROUTINE AppStream_GetElemsWithRecvLoss
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetSubregionalRecvLosses(AppStream,AppGrid) RESULT(RecvLosses)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8)                         :: RecvLosses(AppGrid%NSubregions)
    
    IF (AppStream%lDefined) THEN
        RecvLosses = AppStream%Me%GetSubregionalRecvLosses(AppGrid)
    ELSE
        RecvLosses = 0.0
    END IF
    
  END FUNCTION AppStream_GetSubregionalRecvLosses
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF RATING TABLE POINTS AT A STREAM NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetNRatingTablePoints(AppStream,iStrmNode) RESULT(N)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iStrmNode
    INTEGER                         :: N
    
    IF (.NOT. AppStream%lDefined) THEN
        N = 0
        RETURN
    END IF
    
    IF (iStrmNode .GT. 0) THEN
      IF (iStrmNode .LE. AppStream%Me%NStrmNodes) N = AppStream%Me%GetNRatingTablePoints(iStrmNode)
    ELSE
      N = 0
    END IF
    
  END FUNCTION GetNRatingTablePoints


  ! -------------------------------------------------------------
  ! --- GET BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetBottomElevations(AppStream,lModel_ForInquiry_Defined,BottomElev,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL,INTENT(IN)              :: lModel_ForInquiry_Defined
    REAL(8),INTENT(INOUT)           :: BottomElev(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+29),PARAMETER :: ThisProcedure = ModName // 'AppStream_GetBottomElevations'
    
    IF (AppStream%lDefined)  THEN
        IF (lModel_ForInquiry_Defined) THEN
            SELECT TYPE (p => AppStream%Me)
                TYPE IS (AppStream_v50_Type)
                    CALL SetLastMessage('Model is instantiated only partially. Stream bottom elevations for Stream Package Component v5.0 cannot be retrieved from a partially instantiated model.',f_iWarn,ThisProcedure)
                    BottomElev = 0.0
                    iStat      = -1
                CLASS DEFAULT
                    BottomElev = AppStream%Me%GetBottomElevations()
                    iStat      = 0
            END SELECT
        ELSE
            BottomElev = AppStream%Me%GetBottomElevations()
            iStat      = 0
        END IF
    END IF
    
  END SUBROUTINE AppStream_GetBottomElevations
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE GetFlows(AppStream,Flows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)           :: Flows(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetFlows(Flows)
    
  END SUBROUTINE GetFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM FLOW AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetFlow(AppStream,iStrmNode) RESULT(Flow)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iStrmNode
    REAL(8)                         :: Flow
    
    IF (AppStream%lDefined) THEN
        Flow = AppStream%Me%GetFlow(iStrmNode)
    ELSE
        Flow = 0.0
    END IF
    
  END FUNCTION GetFlow
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOWS AT SOME INFLOWS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInflows_AtSomeInflows(AppStream,iInflows,rInflows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iInflows(:)
    REAL(8),INTENT(OUT)             :: rInflows(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetInflows_AtSomeInflows(iInflows,rInflows)
    ELSE
        rInflows = 0.0
    END IF
       
  END SUBROUTINE GetInflows_AtSomeInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOWS AT A SET OF NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInflows_AtSomeNodes(AppStream,iStrmNodes,rInflows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iStrmNodes(:)
    REAL(8),INTENT(OUT)             :: rInflows(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetInflows_AtSomeNodes(iStrmNodes,rInflows)
    ELSE
        rInflows = 0.0
    END IF
    
  END SUBROUTINE GetInflows_AtSomeNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetInflow_AtANode(AppStream,iStrmNode) RESULT(rInflow)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iStrmNode
    REAL(8)                         :: rInflow
    
    IF (AppStream%lDefined) THEN
        rInflow = AppStream%Me%GetInflow_AtANode(iStrmNode)
    ELSE
        rInflow = 0.0
    END IF
    
  END FUNCTION GetInflow_AtANode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF INFLOWS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNInflows(AppStream) RESULT(iNInflows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: iNInflows
    
    IF (AppStream%lDefined) THEN
        iNInflows = AppStream%Me%GetNInflows()
    ELSE
        iNInflows = 0
    END IF
    
  END FUNCTION GetNInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOW NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetInflowNodes(AppStream,iNodes)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,ALLOCATABLE             :: iNodes(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetInflowNodes(iNodes)
    ELSE
        ALLOCATE (iNodes(0))
    END IF
    
  END SUBROUTINE GetInflowNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOW IDs
  ! -------------------------------------------------------------
  SUBROUTINE GetInflowIDs(AppStream,IDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,ALLOCATABLE             :: IDs(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetInflowIDs(IDs)
    ELSE
        ALLOCATE (IDs(0))
    END IF
    
  END SUBROUTINE GetInflowIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM HEADS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHeads(AppStream,Heads)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)           :: Heads(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetHeads(Heads)
    
  END SUBROUTINE GetHeads
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM STAGES
  ! -------------------------------------------------------------
  SUBROUTINE GetStages(AppStream,lModel_ForInquiry_Defined,Stages,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL,INTENT(IN)              :: lModel_ForInquiry_Defined
    REAL(8),INTENT(INOUT)           :: Stages(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+9),PARAMETER :: ThisProcedure = ModName // 'GetStages'
    INTEGER                               :: NStrmNodes
    REAL(8),ALLOCATABLE                   :: Heads(:),BottomElev(:)
    
    IF (AppStream%lDefined) THEN
        IF (lModel_ForInquiry_Defined) THEN
            SELECT TYPE (p => AppStream%Me)
                TYPE IS (AppStream_v50_Type)
                    CALL SetLastMessage('Model is instantiated only partially. Stream stages for Stream Package Component v5.0 cannot be retrieved from a partially instantiated model.',f_iWarn,ThisProcedure)
                    Stages = 0.0
                    iStat  = -1
                CLASS DEFAULT
                    NStrmNodes = AppStream%GetNStrmNodes()
                    ALLOCATE (Heads(NStrmNodes) , BottomElev(NStrmNodes))
                    CALL AppStream%GetHeads(Heads)
                    CALL AppStream%GetBottomElevations(lModel_ForInquiry_Defined,BottomElev,iStat)
                    Stages = Heads - BottomElev
                    iStat  = 0
            END SELECT
        ELSE
            NStrmNodes = AppStream%GetNStrmNodes()
            ALLOCATE (Heads(NStrmNodes) , BottomElev(NStrmNodes))
            CALL AppStream%GetHeads(Heads)
            CALL AppStream%GetBottomElevations(lModel_ForInquiry_Defined,BottomElev,iStat)
            Stages = Heads - BottomElev
        END IF
    END IF
    
  END SUBROUTINE GetStages
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM HEAD AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetHead_AtOneNode(AppStream,iNode,lPrevious) RESULT(Head)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iNode
    LOGICAL,INTENT(IN)              :: lPrevious
    REAL(8)                         :: Head
    
    IF (AppStream%lDefined) Head = AppStream%Me%GetHead_AtOneNode(iNode,lPrevious)
    
  END FUNCTION GetHead_AtOneNode
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAGS
  ! -------------------------------------------------------------
  SUBROUTINE GetiColAdjust(AppStream,iColAdjust)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: iColAdjust(:)
    
    CALL AppStream%Me%GetiColAdjust(iColAdjust)
        
  END SUBROUTINE GetiColAdjust


  ! -------------------------------------------------------------
  ! --- GET MAXIMUM VALUE OF THE DELIVERY RANKS
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetMaxDiversionRank(AppStream) RESULT(iMaxRank)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: iMaxRank
    
    iMaxRank = AppStream%Me%GetMaxDiversionRank()
    
  END FUNCTION AppStream_GetMaxDiversionRank
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT LEVEL RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  FUNCTION AppStream_GetElemRecvLosses(AppStream,NElements,iSource) RESULT(ElemRecvLosses)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: NElements,iSource
    REAL(8)                         :: ElemRecvLosses(NElements)
    
    IF (AppStream%lDefined) THEN
        ElemRecvLosses = AppStream%Me%GetElemRecvLosses(NElements,iSource)
    ELSE
        ElemRecvLosses = 0.0
    END IF
    
  END FUNCTION AppStream_GetElemRecvLosses
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF DIVERSIONS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNDiver(AppStream) RESULT(NDiver)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NDiver
    
    IF (AppStream%lDefined) THEN
        NDiver = AppStream%Me%GetNDiver()
    ELSE
        NDiver = 0
    END IF
    
  END FUNCTION GetNDiver
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF BYPASSES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNBypass(AppStream) RESULT(NBypass)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NBypass
    
    IF (AppStream%lDefined) THEN
        NBypass = AppStream%Me%GetNBypass()
    ELSE
        NBypass = 0
    END IF
    
  END FUNCTION GetNBypass
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF REACHES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNReaches(AppStream) RESULT(NReaches)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NReaches
    
    IF (AppStream%lDefined) THEN
        NReaches = AppStream%Me%GetNReaches()
    ELSE
        NReaches = 0
    END IF
    
  END FUNCTION GetNReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF STREAM NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNStrmNodes(AppStream) RESULT(NStrmNodes)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NStrmNodes
    
    IF (AppStream%lDefined) THEN
        NStrmNodes = AppStream%Me%GetNStrmNodes()
    ELSE
        NStrmNodes = 0
    END IF
    
  END FUNCTION GetNStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET DIVERSION/DELIVERY IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetDiversionIDs(AppStream,IDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: IDs(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetDiversionIDs(IDs)
    
  END SUBROUTINE GetDiversionIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassIDs(AppStream,IDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: IDs(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetBypassIDs(IDs)
    
  END SUBROUTINE GetBypassIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS/DIVERSION ORIGIN AND DESTINATION DATA
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassDiverOriginDestData(AppStream,lIsBypass,iBypassOrDiver,iNodeExport,iDestType,iDest)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL,INTENT(IN)              :: lIsBypass
    INTEGER,INTENT(IN)              :: iBypassOrDiver
    INTEGER,INTENT(OUT)             :: iNodeExport,iDestType,iDest
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetBypassDiverOriginDestData(lIsBypass,iBypassOrDiver,iNodeExport,iDestType,iDest)
    
  END SUBROUTINE GetBypassDiverOriginDestData
  

  ! -------------------------------------------------------------
  ! --- GET NET FLOW FROM A BYPASS (AFTER RECOVERABLE AND NON-RECOVERABLE LOSSES ARE TAKEN OUT)
  ! -------------------------------------------------------------
  PURE FUNCTION GetBypassReceived_FromABypass(AppStream,iBypass) RESULT(rFlow)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iBypass
    REAL(8)                         :: rFlow
    
    IF (AppStream%lDefined) THEN
        rFlow = AppStream%Me%GetBypassReceived_FromABypass(iBypass)
    ELSE
        rFlow = 0.0
    END IF
    
  END FUNCTION GetBypassReceived_FromABypass
  
  
  ! -------------------------------------------------------------
  ! --- GET NET BYPASS INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetNetBypassInflows(AppStream,rBPInflows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(OUT)             :: rBPInflows(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetNetBypassInflows(rBPInflows)
    ELSE
        rBPInflows = 0.0
    END IF
    
  END SUBROUTINE GetNetBypassInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS INFLOWS AT ALL STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStrmBypassInflows(AppStream,rBPInflows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(OUT)             :: rBPInflows(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetStrmBypassInflows(rBPInflows)
    ELSE
        rBPInflows = 0.0
    END IF
    
  END SUBROUTINE GetStrmBypassInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET BYPASS OUTFLOWS FOR ALL BYPASSES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetBypassOutflows(AppStream,rOutflows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(OUT)             :: rOutflows(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetBypassOutflows(rOutflows)
    ELSE
        rOutflows = 0.0
    END IF
    
  END SUBROUTINE GetBypassOutflows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM REACH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetReachIDs(AppStream,IDs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: IDs(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetReachIDs(IDs)
    
  END SUBROUTINE GetReachIDs
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT STREAM REACH ID TO INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION GetReachIndex(AppStream,iReachID) RESULT(Index)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReachID
    INTEGER                         :: Index
    
    IF (AppStream%lDefined) THEN
        Index = AppStream%Me%GetReachIndex(iReachID)
    ELSE
        Index = 0
    END IF
    
  END FUNCTION GetReachIndex
  
  
  ! -------------------------------------------------------------
  ! --- GET REACHES FOR SOME STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetReaches_ForStrmNodes(AppStream,iStrmNodes,iReaches,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iStrmNodes(:)
    INTEGER,INTENT(OUT)             :: iReaches(:),iStat
    
    !Return if no streams are defined
    IF (AppStream%iVersion .EQ. 0) THEN
        iReaches = 0
        iStat    = 0
        RETURN
    END IF
    
    !Get reaches for stream nodes
    CALL AppStream%Me%GetReaches_ForStrmNodes(iStrmNodes,iReaches,iStat)
    
  END SUBROUTINE GetReaches_ForStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(AppStream_v40_Type)  :: v40
    TYPE(AppStream_v41_Type)  :: v41
    TYPE(AppStream_v42_Type)  :: v42
    TYPE(AppStream_v421_Type) :: v421
    TYPE(AppStream_v50_Type)  :: v50
    TYPE(VersionType)         :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) // ' (Interface) ; ' // TRIM(v40%GetVersion()) // ', ' // TRIM(v41%GetVersion()) // ', ' // TRIM(v42%GetVersion()) // ', '  // TRIM(v421%GetVersion()) // ', ' // TRIM(v50%GetVersion()) // ' (Components)'
    
  END FUNCTION GetVersion

    

  
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
    CLASS(AppStreamType) :: AppStream
    INTEGER,INTENT(IN)   :: iDiver
    REAL(8),INTENT(IN)   :: rDiversion
    
    IF (AppStream%lDefined) CALL AppStream%Me%SetDiversionRead(iDiver,rDiversion) 
    
  END SUBROUTINE SetDiversionRead
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY SPECS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetSupplySpecs(AppStream,DeliDestConnector,DeliRequired,IrigFracs,DeliToDest)
    CLASS(AppStreamType)                     :: AppStream
    TYPE(SupplyDestinationConnectorType)     :: DeliDestConnector
    REAL(8),INTENT(IN)                       :: DeliRequired(:),IrigFracs(:)
    TYPE(SupplyToDestinationType),INTENT(IN) :: DeliToDest(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%SetSupplySpecs(DeliDestConnector,DeliRequired,IrigFracs,DeliToDest)
    
  END SUBROUTINE AppStream_SetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- SET DELIVERY IRRIGATION FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetIrigFracsRead(AppStream,IrigFrac)
    CLASS(AppStreamType) :: AppStream
    REAL(8),INTENT(IN)   :: IrigFrac(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%SetIrigFracsRead(IrigFrac)
    
  END SUBROUTINE AppStream_SetIrigFracsRead
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM FLOW IF STREAMS ARE NON-ROUTED
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetStreamFlow(AppStream,iStrmNode,rFlow,iStat)
    CLASS(AppStreamType) :: AppStream
    INTEGER,INTENT(IN)   :: iStrmNode
    REAL(8),INTENT(IN)   :: rFlow
    INTEGER,INTENT(OUT)  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'AppStream_SetStreamFlow'
    
    !Initialize
    iStat = 0
    
    IF (AppStream%lDefined) THEN
        !Make sure this is done only when streams are non-routed
        IF (AppStream%Me%lRouted) THEN
            CALL SetLastMessage('Stream flows can only be assigned when streams are simulated as non-routed streams!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL AppStream%Me%SetStreamFlow(iStrmNode,rFlow)
    END IF
    
  END SUBROUTINE AppStream_SetStreamFlow
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE SetStreamInflow(AppStream,iStrmNode,rFlow,lAdd,iStat)
    CLASS(AppStreamType) :: AppStream
    INTEGER,INTENT(IN)   :: iStrmNode
    REAL(8),INTENT(IN)   :: rFlow
    LOGICAL,INTENT(IN)   :: lAdd
    INTEGER,INTENT(OUT)  :: iStat
    
    !Initialize
    iStat = 0
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%SetStreamInflow(iStrmNode,rFlow,lAdd)
    END IF
    
  END SUBROUTINE SetStreamInflow
  
  
  ! -------------------------------------------------------------
  ! --- SET BYPASS ORIGINATING FLOW AS WELL AS OTHER RELATED FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE SetBypassFlows_AtABypass(AppStream,iBypass,rOriginatingFlow)
    CLASS(AppStreamType) :: AppStream
    INTEGER,INTENT(IN)   :: iBypass
    REAL(8),INTENT(IN)   :: rOriginatingFlow
    
    IF (AppStream%lDefined) CALL AppStream%Me%SetBypassFlows_AtABypass(iBypass,rOriginatingFlow) 
    
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
  SUBROUTINE AppStream_ReadRestartData(AppStream,InFile,iStat)
    CLASS(AppStreamType)  :: AppStream
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(OUT)   :: iStat
    
    IF (AppStream%Me%lRouted) THEN
        CALL AppStream%Me%ReadRestartData(InFile,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE AppStream_ReadRestartData

  
  ! -------------------------------------------------------------
  ! --- READ APPLICATION STREAMS RELATED DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AppStream,lDiverAdjusted,TimeStep,iDiversions,rDiversions,iStrmInflows,rStrmInflows,iBypasses,rBypasses,iStat)
    CLASS(AppStreamType)          :: AppStream
    LOGICAL,INTENT(IN)            :: lDiverAdjusted
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: iDiversions(:),iStrmInflows(:),iBypasses(:)
    REAL(8),INTENT(IN)            :: rDiversions(:),rStrmInflows(:),rBypasses(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    iStat = 0
    
    IF (AppStream%lDefined) THEN
        IF (AppStream%Me%lRouted) THEN 
            CALL AppStream%Me%ReadTSData(lDiverAdjusted,TimeStep,iDiversions,rDiversions,iStrmInflows,rStrmInflows,iBypasses,rBypasses,iStat)
        END IF
    END IF
    
  END SUBROUTINE ReadTSData
  



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
  SUBROUTINE AppStream_PrintRestartData(AppStream,OutFile)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    TYPE(GenericFileType)           :: OutFile
    
    IF (AppStream%Me%lRouted) CALL AppStream%Me%PrintRestartData(OutFile)
    
  END SUBROUTINE AppStream_PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_PrintResults(AppStream,TimeStep,lEndOfSimulation,QTRIB,QROFF,QRTRN,QDRAIN,QRVET,StrmGWConnector,StrmLakeConnector)
    CLASS(AppStreamType)                   :: AppStream
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    LOGICAL,INTENT(IN)                     :: lEndOfSimulation
    REAL(8),INTENT(IN)                     :: QTRIB(:),QROFF(:),QRTRN(:),QDRAIN(:),QRVET(:)
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN) :: StrmLakeConnector
    
    !Local variables
    REAL(8) :: BottomElevs(SIZE(QTRIB))
    
    IF (AppStream%lDefined) THEN
        IF (AppStream%Me%lRouted) THEN
            BottomElevs = AppStream%Me%GetBottomElevations()
            CALL AppStream%Me%PrintResults(TimeStep,lEndOfSimulation,QTRIB,QROFF,QRTRN,QDRAIN,QRVET,BottomElevs,StrmGWConnector,StrmLakeConnector)
        END IF
    END IF
    
  END SUBROUTINE AppStream_PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- WRITE PREPROCESSED DATA TO TEXT FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_WriteDataToTextFile(AppStream,iGWNodeIDs,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
    CLASS(AppStreamType),INTENT(IN)      :: AppStream
    INTEGER,INTENT(IN)                   :: iGWNodeIDs(:)
    CHARACTER(LEN=*),INTENT(IN)          :: UNITLTOU
    REAL(8),INTENT(IN)                   :: FACTLTOU
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    INTEGER,INTENT(OUT)                  :: iStat
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%WriteDataToTextFile(iGWNodeIDs,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE AppStream_WriteDataToTextFile
  
  
  ! -------------------------------------------------------------
  ! --- WRITE PRE-PROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_WritePreprocessedData(AppStream,OutFile)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    TYPE(GenericFileType)           :: OutFile
    
    IF (AppStream%lDefined) THEN
        CALL Outfile%WriteData(AppStream%iVersion)
        CALL AppStream%Me%WritePreprocessedData(OutFile)
    ELSE
        CALL Outfile%WriteData(0)
    END IF
    
  END SUBROUTINE AppStream_WritePreprocessedData

  
  
  
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
    CLASS(AppStreamType) :: AppStream
    INTEGER,INTENT(IN)   :: iLakeIDs(:)
    INTEGER,INTENT(OUT)  :: iStat
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%DestinationIDs_To_Indices(iLakeIDs,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE DestinationIDs_To_Indices
  
  
  ! -------------------------------------------------------------
  ! --- MODIFY HEADS USING DELTA_HEADS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_UpdateHeads(AppStream,HDelta)
    CLASS(AppStreamType) :: AppStream
    REAL(8),INTENT(IN)   :: HDelta(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%UpdateHeads(HDelta)
    
  END SUBROUTINE AppStream_UpdateHeads
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF DIVERSIONS ARE DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_IsDiversionsDefined(AppStream) RESULT(lDefined)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL                         :: lDefined
    
    IF (AppStream%GetNDiver() .GT. 0) THEN
        lDefined = .TRUE.
    ELSE
        lDefined = .FALSE.
    END IF

  END FUNCTION AppStream_IsDiversionsDefined
  

  ! -------------------------------------------------------------
  ! --- CHECK IF ANY OF THE DIVERSIONS GO TO MODEL DOMAIN
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_IsDiversionToModelDomain(AppStream) RESULT(lDest)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL                         :: lDest
    
    IF (AppStream%lDefined) THEN
        lDest = AppStream%Me%IsDiversionToModelDomain()
    ELSE
        lDest = .FALSE.
    END IF
  
  END FUNCTION AppStream_IsDiversionToModelDomain
  

  ! -------------------------------------------------------------
  ! --- CHECK IF STREAMS ARE DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_IsDefined(AppStream) RESULT(lDefined)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL                         :: lDefined
    
    lDefined = AppStream%lDefined
  
  END FUNCTION AppStream_IsDefined
  

  ! -------------------------------------------------------------
  ! --- CHECK IF STREAMS ARE ROUTED STREAMS
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_IsRouted(AppStream) RESULT(lRouted)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL                         :: lRouted
    
    lRouted = AppStream%Me%lRouted
  
  END FUNCTION AppStream_IsRouted
  

  ! -------------------------------------------------------------
  ! --- RESET IRRIGATION FRACTIONS TO THOSE READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_ResetIrigFracs(AppStream)
    CLASS(AppStreamType) :: AppStream
    
    IF (AppStream%lDefined) CALL AppStream%Me%ResetIrigFracs()

  END SUBROUTINE AppStream_ResetIrigFracs
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT STREAM FLOWS TO HEADS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_ConvertFlowToElev(AppStream)
    CLASS(AppStreamType) :: AppStream
    
    IF (AppStream%lDefined) CALL AppStream%Me%ConvertFlowToElev()
    
  END SUBROUTINE AppStream_ConvertFlowToElev
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF STREAMS RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_ConvertTimeUnit(AppStream,NewUnit)
    CLASS(AppStreamType)        :: AppStream
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    IF (NewUnit .EQ. '') RETURN

    IF (AppStream%lDefined) CALL AppStream%Me%ConvertTimeUnit(NewUnit)
    
  END SUBROUTINE AppStream_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE SUPPLY TO MEET DEMAND GOES TO MODELED DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_CheckSupplyDestinationConnection(AppStream,DeliDestConnector,iStat)
    CLASS(AppStreamType),INTENT(IN)                 :: AppStream
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: DeliDestConnector
    INTEGER,INTENT(OUT)                             :: iStat

    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%CheckSupplyDestinationConnection(DeliDestConnector,iStat)
    ELSE
        iStat = 0
    END IF
 
  END SUBROUTINE AppStream_CheckSupplyDestinationConnection
  
  
  ! -------------------------------------------------------------
  ! --- RESET THE STREAM HEADS TO HEADS FROM PREVIOUS TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_ResetHeads(AppStream)
    CLASS(AppStreamType) :: AppStream
    
    IF (AppStream%lDefined) CALL AppStream%Me%ResetHeads()

  END SUBROUTINE AppStream_ResetHeads


  ! -------------------------------------------------------------
  ! --- CALCULATE STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_Simulate(AppStream,GWHeads,Runoff,ReturnFlow,TributaryFlow,DrainInflows,RiparianET,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppStreamType)        :: AppStream
    REAL(8),INTENT(IN)          :: GWHeads(:,:),Runoff(:),ReturnFlow(:),TributaryFlow(:),DrainInflows(:),RiparianET(:)
    REAL(8),INTENT(OUT)         :: RiparianETFrac(:)
    TYPE(StrmGWConnectorType)   :: StrmGWConnector
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    TYPE(MatrixType)            :: Matrix
   
    IF (AppStream%lDefined) THEN
        IF (AppStream%Me%lRouted) CALL AppStream%Me%Simulate(GWHeads,Runoff,ReturnFlow,TributaryFlow,DrainInflows,RiparianET,RiparianETFrac,StrmGWConnector,StrmLakeConnector,Matrix)
    END IF
    
  END SUBROUTINE AppStream_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF STREAMS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_AdvanceState(AppStream)
    CLASS(AppStreamType) :: AppStream
    
    IF (AppStream%lDefined) THEN
        IF (AppStream%Me%lRouted) CALL AppStream%Me%AdvanceState()
    END IF
    
  END SUBROUTINE AppStream_AdvanceState


  ! -------------------------------------------------------------
  ! --- ADD STREAM COMPONENT AND ITS CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_RegisterWithMatrix(AppStream,Matrix,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    TYPE(MatrixType)                :: Matrix
    INTEGER,INTENT(OUT)             :: iStat
    
    !Initialize
    iStat = 0
    
    IF (AppStream%lDefined) THEN
        IF (AppStream%Me%lRouted) THEN
            CALL EchoProgress('Registering stream component with matrix...')
            CALL AppStream%Me%RegisterWithMatrix(Matrix,iStat)
        END IF
    END IF
    
  END SUBROUTINE AppStream_RegisterWithMatrix
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ONE STREAM NODE (Node1) IS UPSTREAM OF ANOTHER (Node2)
  ! --- Note: If lBypass is TRUE then, Node2 is the bypass ID number
  ! -------------------------------------------------------------
  SUBROUTINE IsUpstreamNode(AppStream,Node1,Node2,lBypass,lUpstream,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: Node1,Node2
    LOGICAL,INTENT(IN)              :: lBypass
    LOGICAL,INTENT(OUT)             :: lUpstream
    INTEGER,INTENT(OUT)             :: iStat
    
    iStat = 0
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%IsUpstreamNode(Node1,Node2,lBypass,lUpstream,iStat)
    ELSE
        lUpstream = .FALSE.
    END IF
    
  END SUBROUTINE IsUpstreamNode
  
  
  ! -------------------------------------------------------------
  ! ---TRANSFER ANY TEXT/DSS OUTPUT TO HDF FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(AppStream,TimeStep,NTIME,iStat)
    CLASS(AppStreamType)          :: AppStream
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    INTEGER,INTENT(OUT)           :: iStat
    
    iStat = 0
    
    IF (AppStream%lDefined) CALL AppStream%Me%TransferOutputToHDF(TimeStep,NTIME,iStat)
        
  END SUBROUTINE TransferOutputToHDF


  ! -------------------------------------------------------------
  ! --- ADD BYPASS
  ! -------------------------------------------------------------
  SUBROUTINE AddBypass(AppStream,ID,iNode_Exp,iColBypass,cName,rFracRecvLoss,rFracNonRecvLoss,iNRechargeElems,iRechargeElems,rRechargeFractions,iDestType,iDest,StrmLakeConnector,iStat)
    CLASS(AppStreamType)        :: AppStream
    INTEGER,INTENT(IN)          :: ID,iNode_Exp,iColBypass,iNRechargeElems,iRechargeElems(iNRechargeElems),iDestType,iDest
    CHARACTER(LEN=*),INTENT(IN) :: cName
    REAL(8),INTENT(IN)          :: rFracRecvLoss,rFracNonRecvLoss,rRechargeFractions(iNRechargeElems)
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+9),PARAMETER :: ThisProcedure = ModName // 'AddBypass'
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%AddBypass(ID,iNode_Exp,iColBypass,cName,rFracRecvLoss,rFracNonRecvLoss,iNRechargeElems,iRechargeElems,rRechargeFractions,iDestType,iDest,StrmLakeConnector,iStat)
    ELSE
        CALL SetLastMessage('Streams not not defined to add bypass!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
  END SUBROUTINE AddBypass

  
END MODULE