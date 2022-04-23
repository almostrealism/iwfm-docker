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
MODULE Package_AppStream
  USE Class_Version               , ONLY: VersionType                                      , &
                                          ReadVersion                                      
  USE MessageLogger               , ONLY: SetLastMessage                                   , &
                                          EchoProgress                                     , &
                                          MessageArray                                     , &
                                          iFatal                                           
  USE GeneralUtilities                                                                     
  USE TimeSeriesUtilities         , ONLY: TimeStepType                                     , &
                                          NPeriods                                         , &
                                          GetJulianDatesBetweenTimeStampsWithTimeIncrement 
  USE IOInterface                 
  USE Package_Discretization      , ONLY: NodeType                                         , &
                                          StratigraphyType                                 , &
                                          AppGridType                                      
  USE Package_Misc                , ONLY: FlowDestinationType                              , &
                                          iDataUnitType_Length                             , &
                                          iDataUnitType_Volume                             , &
                                          iLocationType_StrmReach                          , &
                                          iLocationType_StrmHydObs                         
  USE Package_ComponentConnectors , ONLY: StrmLakeConnectorType                            , &
                                          StrmGWConnectorType                              , &
                                          SupplyType                                       , &
                                          SupplyToDestinationType                          , &
                                          SupplyDestinationConnectorType                   , &
                                          Supply_GetDestination                            
  USE StrmHydrograph              , ONLY: iHydFlow                                         
  USE Class_BaseAppStream         , ONLY: BaseAppStreamType                                
  USE Class_AppStream_v40         , ONLY: AppStream_v40_Type                               
  USE Class_AppStream_v41         , ONLY: AppStream_v41_Type                               
  USE Class_AppStream_v42         , ONLY: AppStream_v42_Type                               
  USE Class_AppStream_v50         , ONLY: AppStream_v50_Type                               
  USE Class_AppDiverBypass        , ONLY: iDiverRecvLoss                                   , &
                                          iBypassRecvLoss                                  , &
                                          iAllRecvLoss                                     
  USE Class_Diversion             , ONLY: DiversionType                                    , & 
                                          DeliveryType                                     
  USE Package_Matrix              , ONLY: MatrixType
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
  PUBLIC :: AppStreamType   , &
            iDiverRecvLoss  , &
            iBypassRecvLoss , &
            iAllRecvLoss  
  
  
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
      PROCEDURE,PASS   :: Kill                             => AppStream_Kill
      PROCEDURE,PASS   :: GetNDataList_AtLocationType      
      PROCEDURE,PASS   :: GetDataList_AtLocationType       => AppStream_GetDataList_AtLocationType
      PROCEDURE,PASS   :: GetLocationsWithData             => AppStream_GetLocationsWithData
      PROCEDURE,PASS   :: GetSubDataList_AtLocation        => AppStream_GetSubDataList_AtLocation
      PROCEDURE,PASS   :: GetModelData_AtLocation_FromFullModel
      PROCEDURE,NOPASS :: GetModelData_AtLocation_FromInquiryModel
      PROCEDURE,PASS   :: GetNames                         => AppStream_GetNames
      PROCEDURE,PASS   :: GetNHydrographs                  => AppStream_GetNHydrographs
      PROCEDURE,PASS   :: GetHydrographCoordinates         => AppStream_GetHydrographCoordinates
      PROCEDURE,PASS   :: GetNStrmNodes                    => AppStream_GetNStrmNodes                 
      PROCEDURE,PASS   :: GetNReaches                      => AppStream_GetNReaches                  
      PROCEDURE,PASS   :: GetNDiver                        => AppStream_GetNDiver                    
      PROCEDURE,PASS   :: GetNBypass                       => AppStream_GetNBypass
      PROCEDURE,PASS   :: GetUpstrmNodeFlags               => AppStream_GetUpstrmNodeFlags
      PROCEDURE,PASS   :: GetReachUpstrmNode               => AppStream_GetReachUpstrmNode           
      PROCEDURE,PASS   :: GetReachDownstrmNode             => AppStream_GetReachDownstrmNode         
      PROCEDURE,PASS   :: GetReachOutflowDestType          => AppStream_GetReachOutflowDestType      
      PROCEDURE,PASS   :: GetReachOutflowDest              => AppStream_GetReachOutflowDest          
      PROCEDURE,PASS   :: GetStageFlowRatingTable          => AppStream_GetStageFlowRatingTable 
      PROCEDURE,PASS   :: GetDiversionDestination          => AppStream_GetDiversionDestination
      PROCEDURE,PASS   :: GetSupply                        => AppStream_GetSupply                    
      PROCEDURE,PASS   :: GetSupplySpecs                   => AppStream_GetSupplySpecs                    
      PROCEDURE,PASS   :: GetSupplyAdjustData              => AppStream_GetSupplyAdjustData          
      PROCEDURE,PASS   :: GetElemRecvLosses                => AppStream_GetElemRecvLosses            
      PROCEDURE,PASS   :: GetElemsWithRecvLoss             => AppStream_GetElemsWithRecvLoss         
      PROCEDURE,PASS   :: GetMaxDiversionRank              => AppStream_GetMaxDiversionRank          
      PROCEDURE,PASS   :: GetiColAdjust                    => AppStream_GetiColAdjust                
      PROCEDURE,PASS   :: GetFlows                         => AppStream_GetFlows                     
      PROCEDURE,PASS   :: GetHeads                         => AppStream_GetHeads 
      PROCEDURE,PASS   :: GetStages                        => AppStream_GetStages
      PROCEDURE,PASS   :: GetHead_AtOneNode                => AppStream_GetHead_AtOneNode                     
      PROCEDURE,NOPASS :: GetVersion                       => AppStream_GetVersion                   
      PROCEDURE,PASS   :: GetBottomElevations              => AppStream_GetBottomElevations          
      PROCEDURE,PASS   :: GetSubregionalRecvLosses         => AppStream_GetSubregionalRecvLosses     
      PROCEDURE,PASS   :: GetStrmConnectivityInGWNodes     => AppStream_GetStrmConnectivityInGWNodes 
      PROCEDURE,PASS   :: GetNRatingTablePoints            => AppStream_GetNRatingTablePoints        
      PROCEDURE,PASS   :: IsDefined                        => AppStream_IsDefined
      PROCEDURE,PASS   :: IsRouted                         => AppStream_IsRouted
      PROCEDURE,PASS   :: IsDiversionsDefined              => AppStream_IsDiversionsDefined
      PROCEDURE,PASS   :: IsDiversionToModelDomain         => AppStream_IsDiversionToModelDomain
      PROCEDURE,PASS   :: IsUpstreamNode                   => AppStream_IsUpstreamNode
      PROCEDURE,PASS   :: ReadTSData                       => AppStream_ReadTSData                   
      PROCEDURE,PASS   :: ReadRestartData                  => AppStream_ReadRestartData                   
      PROCEDURE,PASS   :: PrintResults                     => AppStream_PrintResults 
      Procedure,PASS   :: PrintRestartData                 => AppStream_PrintRestartData
      PROCEDURE,PASS   :: WritePreprocessedData            => AppStream_WritePreprocessedData        
      PROCEDURE,PASS   :: WriteDataToTextFile              => AppStream_WriteDataToTextFile          
      PROCEDURE,PASS   :: ResetIrigFracs                   => AppStream_ResetIrigFracs               
      PROCEDURE,PASS   :: SetIrigFracsRead                 => AppStream_SetIrigFracsRead             
      PROCEDURE,PASS   :: SetSupplySpecs                   => AppStream_SetSupplySpecs               
      PROCEDURE,PASS   :: SetStreamFlow                    => AppStream_SetStreamFlow              
      PROCEDURE,PASS   :: UpdateHeads                      => AppStream_UpdateHeads                  
      PROCEDURE,PASS   :: ConvertTimeUnit                  => AppStream_ConvertTimeUnit
      PROCEDURE,PASS   :: ConvertFlowToElev                => AppStream_ConvertFlowToElev
      PROCEDURE,PASS   :: CheckSupplyDestinationConnection => AppStream_CheckSupplyDestinationConnection 
      PROCEDURE,PASS   :: ResetHeads                       => AppStream_ResetHeads                   
      PROCEDURE,PASS   :: AdvanceState                     => AppStream_AdvanceState                 
      PROCEDURE,PASS   :: Simulate                         => AppStream_Simulate 
      PROCEDURE,PASS   :: RegisterWithMatrix               => AppStream_RegisterWithMatrix
      PROCEDURE,PASS   :: TransferOutputToHDF              => AppStream_TransferOutputToHDF
      GENERIC          :: New                              => AppStream_SetStaticComponent                       , &
                                                              AppStream_SetStaticComponentFromBinFile            , &
                                                              AppStream_SetDynamicComponent                      , &
                                                              AppStream_SetAllComponents                         , &
                                                              AppStream_SetAllComponentsWithoutBinFile
      GENERIC          :: GetModelData_AtLocation          => GetModelData_AtLocation_FromFullModel              , &
                                                              GetModelData_AtLocation_FromInquiryModel
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
        CASE ('5.0')
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(cFileName,AppGrid,Stratigraphy,IsRoutedStreams,StrmGWConnector,StrmLakeConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(cVersionLocal)//')!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT

  END SUBROUTINE AppStream_SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC COMPONENT STREAM DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetDynamicComponent(AppStream,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmGWConnector,StrmLakeConnector,iStat)
    CLASS(AppStreamType)              :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME
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
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
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
        CASE ('5.0')
            IF (AppStream%iVersion .NE. 50) ErrorCode = 1
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(cVersionSim)//')!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    IF (ErrorCode .EQ. 1) THEN
        MessageArray(1) = 'Stream Component versions used in Pre-Processor and Simulation must match!'
        WRITE(MessageArray(2),'(A,F3.1)') 'Version number in Pre-Processor = ',rVersionPre
        MessageArray(3) = 'Version number in Simulation    = ' // TRIM(cVersionSim)
        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Instantiate the dynamic component
    CALL AppStream%Me%New(IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,StrmLakeConnector,StrmGWConnector,iStat)
        
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
        CASE (50)
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(IntToText(iVersion))//')!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE AppStream_SetStaticComponentFromBinFile
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetAllComponents(AppStream,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStreamType),INTENT(OUT)  :: AppStream
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NTIME
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
    IF (BinFile%iGetFileType() .NE. UNKNOWN) THEN
        CALL BinFile%ReadData(iVersion,iStat)  
        IF (iStat .EQ. -1) RETURN
        IF (iVersion .EQ. 0) RETURN
    END IF

    !Return if a Simulation filename is not specified
    IF (cFileName .EQ. ''  .OR.  BinFile%iGetFileType() .EQ. UNKNOWN) RETURN
    
    !If AppStream is already instantiated, kill first
    IF (ALLOCATED(AppStream%Me)) CALL AppStream%Kill()
    
    !Instantiate stream component based on version
    SELECT CASE (iVersion)
        CASE (40)
            ALLOCATE(AppStream_v40_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 40
            AppStream%lDefined = .TRUE.
        CASE (41)
            ALLOCATE(AppStream_v41_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 41
            AppStream%lDefined = .TRUE.
        CASE (42)
            ALLOCATE(AppStream_v42_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 42
            AppStream%lDefined = .TRUE.
        CASE (50)
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,Stratigraphy,BinFile,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(IntToText(iVersion))//')!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE AppStream_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE STREAM DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_SetAllComponentsWithoutBinFile(AppStream,IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,StrmGWConnector,iStat)
    CLASS(AppStreamType),INTENT(OUT)      :: AppStream
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
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 40
            AppStream%lDefined = .TRUE.
        CASE ('4.1')
            ALLOCATE(AppStream_v41_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 41
            AppStream%lDefined = .TRUE.
        CASE ('4.2')
            ALLOCATE(AppStream_v42_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 42
            AppStream%lDefined = .TRUE.
        CASE ('5.0')
            ALLOCATE(AppStream_v50_Type :: AppStream%Me)
            CALL AppStream%Me%New(IsRoutedStreams,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,StrmGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            AppStream%iVersion = 50
            AppStream%lDefined = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Stream Component version number is not recognized ('//TRIM(cVersionPre)//')!',iFatal,ThisProcedure)
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
  SUBROUTINE AppStream_Kill(AppStream)
    CLASS(AppStreamType) :: AppStream
    
    !Local variables
    INTEGER :: ErrorCode
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%Kill()
        DEALLOCATE (AppStream%Me , STAT=ErrorCode)
        AppStream%iVersion = 0
        AppStream%lDefined = .FALSE.
    END IF
    
  END SUBROUTINE AppStream_Kill
  
  


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
  ! --- GET NUMBER OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(AppStream,iLocationType) RESULT(NData)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iLocationType
    INTEGER                         :: NData
    
    IF (AppStream%lDefined) THEN
        NData = AppStream%Me%GetNDataList_AtLocationType(iLocationType)
    ELSE
        NData = 0
    END IF
    
  END FUNCTION GetNDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetDataList_AtLocationType(AppStream,iLocationType,cDataList,cFileList,lBudgetType) 
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE    :: cDataList(:),cFileList(:)
    LOGICAL,ALLOCATABLE             :: lBudgetType(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    
  END SUBROUTINE AppStream_GetDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATIONS THAT HAS A DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetLocationsWithData(AppStream,iLocationType,cDataType,iLocations) 
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)     :: cDataType
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iLocations(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetLocationsWithData(iLocationType,cDataType,iLocations)
    
  END SUBROUTINE AppStream_GetLocationsWithData
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF SUB-DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE, GIVEN A LOCATION ID
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetSubDataList_AtLocation(AppStream,iLocationType,iLocationID,cDataType,cSubDataList) 
    CLASS(AppStreamType),INTENT(IN)          :: AppStream
    INTEGER,INTENT(IN)                       :: iLocationType,iLocationID
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetSubDataList_AtLocation(iLocationType,iLocationID,cDataType,cSubDataList)
    
  END SUBROUTINE AppStream_GetSubDataList_AtLocation

  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING FROM FULLY INSTANTIATED MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromFullModel(AppStream,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iLocationType,iLocationID,iCol
    CHARACTER(LEN=*),INTENT(IN)     :: cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)              :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)             :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)             :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    iStat = 0
    IF (AppStream%lDefined) CALL AppStream%Me%GetModelData_AtLocation(iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    
  END SUBROUTINE GetModelData_AtLocation_FromFullModel
  
    
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING FROM THE INQUIRY MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation_FromInquiryModel(cFileName,TimeStep,iLocationType,iLocationID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat) 
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: iLocationType,iLocationID
    CHARACTER(LEN=*),INTENT(IN)   :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)            :: rFact_LT,rFact_VL
    INTEGER,INTENT(OUT)           :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER               :: iHydType,FileReadCode
    REAL(8)               :: rFactor
    TYPE(GenericFileType) :: InFile
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_StrmHydObs)
            !Open file
            CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,iStat=iStat)
            IF (iStat .EQ. -1) RETURN
            
            !Read hydrograph type (flow or stream surface elevation?)
            CALL InFile%ReadData('/Stream_Hydrographs','HydrographType',ScalarAttrData=iHydType,iStat=iStat)
            
            !Data type and conversion factor
            IF (iHydType .EQ. iHydFlow) THEN
                iDataUnitType = iDataUnitType_Volume
                rFactor       = rFact_VL
            ELSE
                iDataUnitType = iDataUnitType_Length
                rFactor       = rFact_LT
            END IF
            
            !Number of timesteps for which data will be read
            nActualOutput = NPeriods(TimeStep%DELTAT_InMinutes,cOutputBeginDateAndTime,cOutputEndDateAndTime)
            
            !Julian dates for data
            CALL GetJulianDatesBetweenTimeStampsWithTimeIncrement(TimeStep%DeltaT_InMinutes,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputDates(1:nActualOutput))
            
            !Read data
            CALL InFile%ReadData(cOutputBeginDateAndTime,1,iLocationID,rOutputValues(1:nActualOutput),FileReadCode,iStat)
            rOutputValues(1:nActualOutput) = rOutputValues(1:nActualOutput) * rFactor
            
            !Close file
            CALL InFile%Kill()
            
    END SELECT
        
  END SUBROUTINE GetModelData_AtLocation_FromInquiryModel
  
  
  ! -------------------------------------------------------------
  ! --- GET NAMES OF LOCATIONS FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetNames(AppStream,iLocationType,cNamesList)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iLocationType
    CHARACTER(LEN=*),INTENT(OUT)    :: cNamesList(:)  !Assumes array is previously dimensioned according to the number of locations
    
    IF (AppStream%lDefined) THEN
      SELECT CASE (iLocationType)
          CASE (iLocationType_StrmReach)
              CALL AppStream%Me%GetReachNames(cNamesList)
          
          CASE (iLocationType_StrmHydObs)
              CALL AppStream%Me%StrmHyd%GetNames(cNamesList)
      END SELECT
    END IF
    
  END SUBROUTINE AppStream_GetNames
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF HYDROGRAPHS PRINTED
  ! -------------------------------------------------------------
  FUNCTION AppStream_GetNHydrographs(AppStream) RESULT(NHydrographs)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NHydrographs
    
    IF (AppStream%lDefined) THEN
        NHydrographs = AppStream%Me%StrmHyd%GetNHydrographs()
    ELSE
        NHydrographs = 0
    END IF

  END FUNCTION AppStream_GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET COORDINATES OF HYDROGRAPHS PRINTED
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetHydrographCoordinates(AppStream,StrmGWConnector,GridNodeCoordinates,X,Y)
    CLASS(AppStreamType),INTENT(IN)      :: AppStream
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    TYPE(NodeType),INTENT(IN)            :: GridNodeCoordinates(:)
    REAL(8),INTENT(OUT)                  :: X(:),Y(:)
    
    !Local variables
    INTEGER,ALLOCATABLE :: iGWNodes(:)
    
    !Return if streams are not modeled
    IF (.NOT. AppStream%lDefined) RETURN
        
    !Get the corresponding gw nodes
    CALL StrmGWConnector%GetAllGWNodes(iGWNodes)
    
    CALL AppStream%Me%StrmHyd%GetCoordinates(iGWNodes,GridNodeCoordinates,X,Y)

  END SUBROUTINE AppStream_GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET DESTINATION FOR DIVERSIONS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetDiversionDestination(AppStream,Destination)
    CLASS(AppStreamType),INTENT(IN)       :: AppStream
    TYPE(FlowDestinationType),ALLOCATABLE :: Destination(:)
    
    IF (AppStream%lDefined) CALL Supply_GetDestination(AppStream%Me%AppDiverBypass%Diver%Deli , Destination)
    
  END SUBROUTINE AppStream_GetDiversionDestination
  
  
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
  SUBROUTINE AppStream_GetStageFlowRatingTable(AppStream,iNode,Stage,Flow)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iNode
    REAL(8),INTENT(OUT)             :: Stage(:),Flow(:)
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%GetStageFlowRatingTable(iNode,Stage,Flow)
    ELSE
        Stage = 0.0
        Flow  = 0.0
    END IF
    
  END SUBROUTINE AppStream_GetStageFlowRatingTable
  
  
  ! -------------------------------------------------------------
  ! --- GET REACH DESTINATION
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetReachOutflowDest(AppStream,iReach) RESULT(iDest)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iDest

    IF (AppStream%lDefined) THEN
        iDest = AppStream%Me%GetReachOutflowDest(iReach)
    ELSE
        iDest = -1
    END IF
    
  END FUNCTION AppStream_GetReachOutflowDest

  
  ! -------------------------------------------------------------
  ! --- GET REACH DESTINATION TYPE
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetReachOutflowDestType(AppStream,iReach) RESULT(iDestType)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iDestType
    
    IF (AppStream%lDefined) THEN
        iDestType = AppStream%Me%GetReachOutflowDestType(iReach)
    ELSE
        iDestType = -1
    END IF
    
  END FUNCTION AppStream_GetReachOutflowDestType


  ! -------------------------------------------------------------
  ! --- GET DOWNSTREAM NODE FOR A REACH
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetReachDownstrmNode(AppStream,iReach) RESULT(iNode)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iNode

    IF (AppStream%lDefined) THEN
        iNode = Appstream%Me%GetReachDownstrmNode(iReach)
    ELSE
        iNode = 0
    END IF
    
  END FUNCTION AppStream_GetReachDownstrmNode
    

  ! -------------------------------------------------------------
  ! --- GET UPSTREAM NODE FLAGS FOR ALL STREAM NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_GetUpstrmNodeFlags(AppStream,lUpstrmNode)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    LOGICAL,INTENT(OUT)             :: lUpstrmNode(:)
    
    IF (AppStream%lDefined) THEN
        lUpstrmNode = Appstream%Me%GetUpstrmNodeFlags()
    ELSE
        lUpstrmNode = .FALSE.
    END IF
    
  END SUBROUTINE AppStream_GetUpstrmNodeFlags

  
  ! -------------------------------------------------------------
  ! --- GET UPSTREAM NODE FOR A REACH
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetReachUpstrmNode(AppStream,iReach) RESULT(iNode)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iReach
    INTEGER                         :: iNode
    
    IF (AppStream%lDefined) THEN
        iNode = Appstream%Me%GetReachUpstrmNode(iReach)
    ELSE
        iNode = 0
    END IF
    
  END FUNCTION AppStream_GetReachUpstrmNode
  
  
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
  PURE FUNCTION AppStream_GetNRatingTablePoints(AppStream,iStrmNode) RESULT(N)
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
    
  END FUNCTION AppStream_GetNRatingTablePoints


  ! -------------------------------------------------------------
  ! --- GET BOTTOM ELEVATIONS
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_GetBottomElevations(AppStream,BottomElev)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)           :: BottomElev(:)
    
    IF (AppStream%lDefined)  BottomElev = AppStream%Me%GetBottomElevations()
    
  END SUBROUTINE AppStream_GetBottomElevations
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetFlows(AppStream,Flows)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)           :: Flows(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetFlows(Flows)
    
  END SUBROUTINE AppStream_GetFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM HEADS
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_GetHeads(AppStream,Heads)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)           :: Heads(:)
    
    IF (AppStream%lDefined) CALL AppStream%Me%GetHeads(Heads)
    
  END SUBROUTINE AppStream_GetHeads
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM STAGES
  ! -------------------------------------------------------------
  PURE SUBROUTINE AppStream_GetStages(AppStream,Stages)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    REAL(8),INTENT(INOUT)           :: Stages(:)
    
    !Local variables
    INTEGER             :: NStrmNodes
    REAL(8),ALLOCATABLE :: Heads(:),BottomElev(:)
    
    IF (AppStream%lDefined) THEN
        NStrmNodes = AppStream%GetNStrmNodes()
        ALLOCATE (Heads(NStrmNodes) , BottomElev(NStrmNodes))
        CALL AppStream%GetHeads(Heads)
        CALL AppStream%GetBottomElevations(BottomElev)
        Stages = Heads - BottomElev
    END IF
    
  END SUBROUTINE AppStream_GetStages
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM HEAD AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetHead_AtOneNode(AppStream,iNode,lPrevious) RESULT(Head)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: iNode
    LOGICAL,INTENT(IN)              :: lPrevious
    REAL(8)                         :: Head
    
    IF (AppStream%lDefined) Head = AppStream%Me%GetHead_AtOneNode(iNode,lPrevious)
    
  END FUNCTION AppStream_GetHead_AtOneNode
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAGS
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_GetiColAdjust(AppStream,iColAdjust)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(OUT)             :: iColAdjust(:)
    
    CALL AppStream%Me%GetiColAdjust(iColAdjust)
        
  END SUBROUTINE AppStream_GetiColAdjust


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
  PURE FUNCTION AppStream_GetNDiver(AppStream) RESULT(NDiver)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NDiver
    
    IF (AppStream%lDefined) THEN
        NDiver = AppStream%Me%GetNDiver()
    ELSE
        NDiver = 0
    END IF
    
  END FUNCTION AppStream_GetNDiver
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF BYPASSES
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetNBypass(AppStream) RESULT(NBypass)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NBypass
    
    IF (AppStream%lDefined) THEN
        NBypass = AppStream%Me%GetNBypass()
    ELSE
        NBypass = 0
    END IF
    
  END FUNCTION AppStream_GetNBypass
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF REACHES
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetNReaches(AppStream) RESULT(NReaches)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NReaches
    
    IF (AppStream%lDefined) THEN
        NReaches = AppStream%Me%GetNReaches()
    ELSE
        NReaches = 0
    END IF
    
  END FUNCTION AppStream_GetNReaches
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF STREAM NODES
  ! -------------------------------------------------------------
  PURE FUNCTION AppStream_GetNStrmNodes(AppStream) RESULT(NStrmNodes)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER                         :: NStrmNodes
    
    IF (AppStream%lDefined) THEN
        NStrmNodes = AppStream%Me%GetNStrmNodes()
    ELSE
        NStrmNodes = 0
    END IF
    
  END FUNCTION AppStream_GetNStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION AppStream_GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(AppStream_v40_Type) :: v40
    TYPE(AppStream_v41_Type) :: v41
    TYPE(AppStream_v42_Type) :: v42
    TYPE(AppStream_v50_Type) :: v50
    TYPE(VersionType)        :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) // ' (Interface) ; ' // TRIM(v40%GetVersion()) // ', ' // TRIM(v41%GetVersion()) // ', ' // TRIM(v42%GetVersion()) // ', ' // TRIM(v50%GetVersion()) // ' (Components)'
    
  END FUNCTION AppStream_GetVersion

    

  
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
            CALL SetLastMessage('Stream flows can only be assigned when streams are simulated as non-routed streams!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL AppStream%Me%SetStreamFlow(iStrmNode,rFlow)
    END IF
    
  END SUBROUTINE AppStream_SetStreamFlow
  
  
  
  
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
  SUBROUTINE AppStream_ReadTSData(AppStream,lDiverAdjusted,TimeStep,iStat,DiversionsOverwrite)
    CLASS(AppStreamType)          :: AppStream
    LOGICAL,INTENT(IN)            :: lDiverAdjusted
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    REAL(8),OPTIONAL,INTENT(IN)   :: DiversionsOverwrite(:)
    
    iStat = 0
    
    IF (AppStream%lDefined) THEN
        IF (AppStream%Me%lRouted) THEN
            IF (PRESENT(DiversionsOverwrite)) THEN
                CALL AppStream%Me%ReadTSData(lDiverAdjusted,TimeStep,iStat,DiversionsOverwrite)
            ELSE
                CALL AppStream%Me%ReadTSData(lDiverAdjusted,TimeStep,iStat)
            END IF
        END IF
    END IF
    
  END SUBROUTINE AppStream_ReadTSData
  



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
  SUBROUTINE AppStream_WriteDataToTextFile(AppStream,UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
    CLASS(AppStreamType),INTENT(IN)      :: AppStream
    CHARACTER(LEN=*),INTENT(IN)          :: UNITLTOU
    REAL(8),INTENT(IN)                   :: FACTLTOU
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    TYPE(StrmGWConnectorType),INTENT(IN) :: StrmGWConnector
    INTEGER,INTENT(OUT)                  :: iStat
    
    IF (AppStream%lDefined) THEN
        CALL AppStream%Me%WriteDataToTextFile(UNITLTOU,FACTLTOU,Stratigraphy,StrmGWConnector,iStat)
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
  FUNCTION AppStream_IsUpstreamNode(AppStream,Node1,Node2,lBypass,iStat) RESULT(lUpstream)
    CLASS(AppStreamType),INTENT(IN) :: AppStream
    INTEGER,INTENT(IN)              :: Node1,Node2
    LOGICAL,INTENT(IN)              :: lBypass
    INTEGER,INTENT(OUT)             :: iStat
    LOGICAL                         :: lUpstream
    
    iStat = 0
    
    IF (AppStream%lDefined) lUpstream = AppStream%Me%IsUpstreamNode(Node1,Node2,lBypass,iStat)
    
  END FUNCTION AppStream_IsUpstreamNode
  
  
  ! -------------------------------------------------------------
  ! ---TRANSFER ANY TEXT/DSS OUTPUT TO HDF FOR POST_PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE AppStream_TransferOutputToHDF(AppStream,TimeStep,NTIME,iStat)
    CLASS(AppStreamType)          :: AppStream
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    INTEGER,INTENT(OUT)           :: iStat
    
    iStat = 0
    
    IF (AppStream%lDefined) CALL AppStream%Me%TransferOutputToHDF(TimeStep,NTIME,iStat)
        
  END SUBROUTINE AppStream_TransferOutputToHDF



  
END MODULE