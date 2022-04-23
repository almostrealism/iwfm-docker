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
MODULE RootZone_v50
  USE MessageLogger                    , ONLY: SetLastMessage                          , &
                                               EchoProgress                            , &
                                               MessageArray                            , &
                                               iFatal
  USE TimeSeriesUtilities              , ONLY: TimeStepType                            , &
                                               IncrementTimeStamp                      , &
                                               TimeIntervalConversion
  USE GeneralUtilities                 , ONLY: StripTextUntilCharacter                 , &
                                               CleanSpecialCharacters                  , &
                                               ArrangeText                             , &
                                               IntToText                               , &
                                               UpperCase                               , &
                                               LocateInList                            , &
                                               NormalizeArray                          , &
                                               EstablishAbsolutePathFileName
  USE IOInterface                      , ONLY: GenericFileType                        
  USE Package_Discretization           , ONLY: AppGridType                             
  USE Package_Misc                     , ONLY: SolverDataType                          , &
                                               RealTSDataInFileType                    , &
                                               IntTSDataInFileType                     , &
                                               ReadTSData                              , &
                                               FlowDest_Outside                        , &
                                               FlowDest_StrmNode                       , &
                                               FlowDest_Lake                           , &
                                               FlowDest_Subregion                      , &
                                               FlowDest_GWElement                      , &
                                               Supply_Diversion_Ag                     , &
                                               Supply_Diversion_Urb                    , &
                                               Supply_Pumping_Ag                       , &
                                               Supply_Pumping_Urb                      , &
                                               Supply_UpstrmElemRunoff                 , &
                                               iLocationType_Subregion                 , &
                                               iLocationType_Zone                      , &
                                               iAllLocationIDsListed                   , &
                                               iDataUnitType_Length
  USE Package_PrecipitationET          , ONLY: PrecipitationType                       , &
                                               ETType
  USE Package_UnsatZone                , ONLY: RootZoneSoilType                        , &
                                               KunsatMethodList
  USE Package_ZBudget                  , ONLY: ZBudgetType                             , &
                                               ZBudgetHeaderType                       , &
                                               ZoneListType                            , &
                                               SystemDataType                          , &
                                               iElemDataType                           , &
                                               ZBud_MarkerChar     => MarkerChar       , &
                                               ZBud_AreaUnitMarker => AreaUnitMarker
  USE Package_Budget                   , ONLY: BudgetType                              , &
                                               BudgetHeaderType                        , &
                                               MaxLocationNameLen                      , &
                                               VolumeUnitMarker                        , &
                                               LocationNameMarker                      , &
                                               AreaMarker                              , &
                                               AreaUnitMarker                          , &
                                               AR                                      , &
                                               VR                                      , &
                                               VLB                                     , &
                                               VLE                                     , &
                                               VR_lwu_PotCUAW                          , &
                                               VR_lwu_AgSupplyReq                      , &
                                               VR_lwu_AgPump                           , &
                                               VR_lwu_AgDiv                            , &
                                               VR_lwu_AgOthIn                          , &
                                               VR_lwu_AgShort                          , &
                                               PER_AVER                                , &
                                               PER_CUM
  USE Package_ComponentConnectors      , ONLY: SupplyType
  USE Class_BaseRootZone               , ONLY: BaseRootZoneType                        , &
                                               FlagsType                               , &
                                               ElemSurfaceFlowToDestType               , &
                                               CompileElemSurfaceFlowToDestinationList 
  USE Class_AgLandUse_v50             , ONLY: AgDatabase_v50_Type
  USE Class_UrbanLandUse_v50          , ONLY: UrbanDatabase_v50_Type
  USE Class_NativeRiparianLandUse_v50 , ONLY: NativeRiparianDatabase_v50_Type
  USE Class_GenericMoistureData        , ONLY: GenericMoistureDataType  
  USE Util_Package_RootZone            , ONLY: WaterSupplyType                         , &
                                               AddStringToStringList
  USE Class_GenericLandUse             , ONLY: GenericLandUSeType
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
  PUBLIC :: RootZone_v50_Type                                  
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(FlagsType) :: Flags_v50_Type
    PRIVATE
    LOGICAL :: lAg_Defined = .FALSE.    !Flag to show if ag lands are simulated
  END TYPE Flags_v50_Type
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE DATA TYPE
  ! --- *** All flow terms are kept in unit rates except WaterSupply ***
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseRootZoneType) :: RootZone_v50_Type
    PRIVATE
    INTEGER                                   :: NSoils                       = 0                       !Number of soil types simulated
    TYPE(Flags_v50_Type)                      :: Flags                                                  !Flags that affect the simulation of root zone
    TYPE(RootZoneSoilType),ALLOCATABLE        :: SubregionSoilsData(:,:)                                !Soils data for each (soil,subregion) combination
    INTEGER,ALLOCATABLE                       :: ElemSoilType(:)                                        !Soil type ID for each (element)
    REAL(8),ALLOCATABLE                       :: SoilRegionPrecip(:,:)                                  !Unit precipitation rate for each (soil,subregion) combination
    REAL(8),ALLOCATABLE                       :: SoilRegionArea(:,:)                                    !Area of each (soil,subregion) combination
    TYPE(AgDatabase_v50_Type)                 :: AgRootZone                                             !Ag lands database
    TYPE(UrbanDatabase_v50_Type)              :: UrbanRootZone                                          !Urban database
    TYPE(NativeRiparianDatabase_v50_Type)     :: NVRVRootZone                                           !Native/riparian database
    TYPE(WaterSupplyType),ALLOCATABLE         :: WaterSupply(:)                                         !Categorized water supply to each (subregion) in units of volume
    TYPE(GenericMoistureDataType)             :: GenericMoistureData                                    !Data to simulate generic moisture inflow (e.g. levee seepage, fog)
    TYPE(SolverDataType)                      :: SolverData                                             !Data for iterative solution of soil moisture
    TYPE(ZBudgetType)                         :: LWUZoneBudRawFile                                      !Raw land and water use zone budget file
    TYPE(ZBudgetType)                         :: RootZoneZoneBudRawFile                                 !Raw root zone zone budget file
  CONTAINS
    PROCEDURE,PASS :: New                                   => RootZone_v50_New
    PROCEDURE,PASS :: KillRZImplementation                  => RootZone_v50_Kill
    PROCEDURE,PASS :: IsLandUseUpdated                      => RootZone_v50_IsLandUseUpdated
    PROCEDURE,PASS :: GetNDataList_AtLocationType           => RootZone_v50_GetNDataList_AtLocationType
    PROCEDURE,PASS :: GetDataList_AtLocationType            => RootZone_v50_GetDataList_AtLocationType
    PROCEDURE,PASS :: GetLocationsWithData                  => RootZone_v50_GetLocationsWithData
    PROCEDURE,PASS :: GetSubDataList_AtLocation             => RootZone_v50_GetSubDataList_AtLocation 
    PROCEDURE,PASS :: GetModelData_AtLocation               => RootZone_v50_GetModelData_AtLocation 
    PROCEDURE,PASS :: GetNAgCrops                           => RootZone_v50_GetNAgCrops 
    PROCEDURE,PASS :: GetNDemandLocations                   => RootZone_v50_GetNDemandLocations 
    PROCEDURE,PASS :: GetElementPrecipInfilt                => RootZone_v50_GetElementPrecipInfilt 
    PROCEDURE,PASS :: GetElementActualET                    => RootZone_v50_GetElementActualET 
    PROCEDURE,PASS :: GetWaterDemand_Ag                     => RootZone_v50_GetWaterDemand_Ag 
    PROCEDURE,PASS :: GetWaterDemand_Urb                    => RootZone_v50_GetWaterDemand_Urb 
    PROCEDURE,PASS :: GetWaterSupply_Ag                     => RootZone_v50_GetWaterSupply_Ag 
    PROCEDURE,PASS :: GetWaterSupply_Urb                    => RootZone_v50_GetWaterSupply_Urb 
    PROCEDURE,PASS :: GetElementAgAreas                     => RootZone_v50_GetElementAgAreas
    PROCEDURE,PASS :: GetElementUrbanAreas                  => RootZone_v50_GetElementUrbanAreas
    PROCEDURE,PASS :: GetElementNativeVegAreas              => RootZone_v50_GetElementNativeVegAreas
    PROCEDURE,PASS :: GetElementRiparianVegAreas            => RootZone_v50_GetElementRiparianVegAreas
    PROCEDURE,PASS :: GetSubregionAgAreas                   => RootZone_v50_GetSubregionAgAreas
    PROCEDURE,PASS :: GetSubregionUrbanAreas                => RootZone_v50_GetSubregionUrbanAreas
    PROCEDURE,PASS :: GetSubregionNativeVegAreas            => RootZone_v50_GetSubregionNativeVegAreas
    PROCEDURE,PASS :: GetSubregionRiparianVegAreas          => RootZone_v50_GetSubregionRiparianVegAreas
    PROCEDURE,PASS :: GetDemandAgAreas                      => RootZone_v50_GetDemandAgAreas
    PROCEDURE,PASS :: GetdemandUrbanAreas                   => RootZone_v50_GetDemandUrbanAreas
    PROCEDURE,PASS :: GetElementSoilMVolume                 => RootZone_v50_GetElementSoilMVolume
    PROCEDURE,PASS :: GetPercAll                            => RootZone_v50_GetPercAll
    PROCEDURE,PASS :: GetPercElement                        => RootZone_v50_GetPercElement
    PROCEDURE,PASS :: GetFlowsToStreams                     => RootZone_v50_GetFlowsToStreams
    PROCEDURE,PASS :: GetFlowsToLakes                       => RootZone_v50_GetFlowsToLakes
    PROCEDURE,PASS :: GetRatio_DestSupplyToRegionSupply_Ag  => RootZone_v50_GetRatio_DestSupplyToRegionSupply_Ag
    PROCEDURE,PASS :: GetRatio_DestSupplyToRegionSupply_Urb => RootZone_v50_GetRatio_DestSupplyToRegionSupply_Urb
    PROCEDURE,PASS :: SetLakeElemFlag                       => RootZone_v50_SetLakeElemFlag
    PROCEDURE,PASS :: SetSupply                             => RootZone_v50_SetSupplyToSubregion
    PROCEDURE,PASS :: ConvertTimeUnit                       => RootZone_v50_ConvertTimeUnit
    PROCEDURE,PASS :: ReadTSData                            => RootZone_v50_ReadTSData
    PROCEDURE,PASS :: ReadRestartData                       => RootZone_v50_ReadRestartData
    PROCEDURE,PASS :: AdvanceState                          => RootZone_v50_AdvanceState
    PROCEDURE,PASS :: ComputeWaterDemand                    => RootZone_v50_ComputeWaterDemand 
    PROCEDURE,PASS :: ZeroSupply                            => RootZone_v50_ZeroSupply
    PROCEDURE,PASS :: Simulate                              => RootZone_v50_Simulate
    PROCEDURE,PASS :: RegionalPerc                          => RootZone_v50_RegionalPerc
    PROCEDURE,PASS :: RegionalReturnFlow_Ag                 => RootZone_v50_RegionalReturnFlow_Ag
    PROCEDURE,PASS :: RegionalReturnFlow_Urb                => RootZone_v50_RegionalReturnFlow_Urb
    PROCEDURE,PASS :: PrintResults                          => RootZone_v50_PrintResults
    PROCEDURE,PASS :: PrintRestartData                      => RootZone_v50_PrintRestartData
    PROCEDURE,PASS :: GetVersion                            => RootZone_v50_GetVersion
  END TYPE RootZone_v50_Type


  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '5.0.0000'
  INCLUDE 'RootZone_v50_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- BUDGET OUTPUT RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: NLWUseBudColumns                                 = 16  , &
                                 NRootZoneBudColumns                              = 45
  CHARACTER(LEN=30),PARAMETER :: cLWUseBudgetColumnTitles(NLWUseBudColumns)       = ['Ag. Area'                           , &
                                                                                     'Potential CUAW'                     , &
                                                                                     'Ag. Supply Requirement'             , &
                                                                                     'Ag. Pumping'                        , &
                                                                                     'Ag. Deliveries'                     , &
                                                                                     'Ag. Inflow as Surface Runoff'       , &
                                                                                     'Ag. Shortage'                       , &
                                                                                     'Ag. ETAW'                           , &
                                                                                     'Ag. Effective Precipitation'        , &
                                                                                     'Ag. ET from Other Sources'          , &
                                                                                     'Urban Area'                         , &
                                                                                     'Urban Supply Requirement'           , &
                                                                                     'Urban Pumping'                      , &
                                                                                     'Urban Deliveries'                   , &
                                                                                     'Urban Inflow as Surface Runoff'     , &
                                                                                     'Urban Shortage'                     ]
  CHARACTER(LEN=53),PARAMETER :: cRootZoneBudgetColumnTitles(NRootZoneBudColumns) = ['Ag. Area'                                               , &
                                                                                     'Ag. Potential ET'                                       , &
                                                                                     'Ag. Precipitation'                                      , &
                                                                                     'Ag. Runoff'                                             , &
                                                                                     'Ag. Prime Applied Water'                                , &
                                                                                     'Ag. Inflow as Surface Runoff'                           , &
                                                                                     'Ag. Reused Water'                                       , &
                                                                                     'Ag. Net Return Flow'                                    , &
                                                                                     'Ag. Beginning Storage (+)'                              , &
                                                                                     'Ag. Net Gain from Land Expansion (+)'                   , &
                                                                                     'Ag. Infiltration (+)'                                   , &
                                                                                     'Ag. Other Inflow (+)'                                   , &
                                                                                     'Ag. Actual ET (-)'                                      , &
                                                                                     'Ag. Percolation (-)'                                    , &
                                                                                     'Ag. Ending Storage (-)'                                 , &
                                                                                     'Ag. Discrepancy (=)'                                    , &
                                                                                     'Urban Area'                                             , &
                                                                                     'Urban Potential ET'                                     , &
                                                                                     'Urban Precipitation'                                    , &
                                                                                     'Urban Runoff'                                           , &
                                                                                     'Urban Prime Applied Water'                              , &
                                                                                     'Urban Inflow as Surface Runoff'                         , &
                                                                                     'Urban Reused Water'                                     , &
                                                                                     'Urban Net Return Flow'                                  , &
                                                                                     'Urban Beginning Storage (+)'                            , &
                                                                                     'Urban Net Gain from Land Expansion (+)'                 , &
                                                                                     'Urban Infiltration (+)'                                 , &
                                                                                     'Urban Other Inflow (+)'                                 , &
                                                                                     'Urban Actual ET (-)'                                    , &
                                                                                     'Urban Percolation (-)'                                  , &
                                                                                     'Urban Ending Storage (-)'                               , &
                                                                                     'Urban Discrepancy (=)'                                  , &
                                                                                     'Native&Riparian Veg. Area'                              , &
                                                                                     'Native&Riparian Veg. Potential ET'                      , &
                                                                                     'Native&Riparian Veg. Precipitation'                     , &
                                                                                     'Native&Riparian Veg. Inflow as Surface Runoff'          , &
                                                                                     'Native&Riparian Veg. Runoff'                            , &
                                                                                     'Native&Riparian Veg. Beginning Storage (+)'             , &
                                                                                     'Native&Riparian Veg. Net Gain from Land Expansion (+)'  , &
                                                                                     'Native&Riparian Veg. Infiltration (+)'                  , &
                                                                                     'Native&Riparian Veg. Other Inflow (+)'                  , &
                                                                                     'Native&Riparian Veg. Actual ET (-)'                     , &
                                                                                     'Native&Riparian Veg. Percolation (-)'                   , &
                                                                                     'Native&Riparian Veg. Ending Storage (-)'                , &
                                                                                     'Native&Riparian Veg. Discrepancy (=)'                   ]

    
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: nData_AtSubregion                        = 2 , &
                                 iLWU_AtSubregion                         = 1 , &
                                 iRootZone_AtSubregion                    = 2 
  CHARACTER(LEN=25),PARAMETER :: cDataList_AtSubregion(nData_AtSubregion) = ['Land and water use budget' , &
                                                                             'Root zone budget'          ]
  INTEGER,PARAMETER           :: nData_AtZone                         = 2 , &
                                 iLWU_AtZone                          = 1 , &
                                 iRootZone_AtZone                     = 2 
  CHARACTER(LEN=30),PARAMETER :: cDataList_AtZone(nData_AtZone) = ['Land and water use zone budget' , &
                                                                   'Root zone zone budget'          ]                      
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: NLands         = 4 , &  !Ag, urban, native and riparian
                                         NGroupLandUse  = 3 , &  !Ag, urban and native-riparian
                                         iLandUse_Ag    = 1 , &
                                         iLandUse_Urban = 2 , &
                                         iLandUse_NVRV  = 3 
  INTEGER,PARAMETER                   :: ModNameLen    = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName       = 'RootZone_v50::'
  
  
  
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
  ! --- NEW ROOT ZONE DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
    CLASS(RootZone_v50_Type)           :: RootZone
    LOGICAL,INTENT(IN)                 :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    INTEGER,INTENT(IN)                 :: NStrmNodes,NLakes,NTIME
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16)                :: ThisProcedure = ModName // 'RootZone_v50_New'
    CHARACTER(LEN=1000)                         :: ALine,AgDataFile,UrbanDataFile,NVRVFile,GenericMoistureFile
    CHARACTER                                   :: cVersionLocal*20
    REAL(8)                                     :: FACTK,FACTCN,RegionArea(AppGrid%NSubregions+1),DummyFactor(1)
    REAL(8),ALLOCATABLE                         :: DummyRealArray(:,:)
    INTEGER                                     :: NElements,NRegion,ErrorCode,indxElem,NSoils,indxRegion,indxSoil,iSoilType,iRegion, &
                                                   SurfaceFlowDest(AppGrid%NElements),SurfaceFlowDestType(AppGrid%NElements)
    INTEGER,ALLOCATABLE                         :: iColGenericMoisture(:,:)
    TYPE(GenericFileType)                       :: RootZoneParamFile
    LOGICAL                                     :: TrackTime,lElemFlowToSubregions
    CHARACTER(LEN=MaxLocationNameLen)           :: RegionNames(AppGrid%NSubregions+1)
    TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToOutside(:),ElemFlowToGW(:)
    CHARACTER(:),ALLOCATABLE                    :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN
    
    !Print progress
    CALL EchoProgress('Instantiating root zone')
    
    !Initialize
    RootZone%Version       = RootZone%Version%New(iLenVersion,cVersion,cRevision)
    cVersionLocal          = ADJUSTL('v' // TRIM(RootZone%Version%GetVersion()))
    NElements              = AppGrid%NElements
    NRegion                = AppGrid%NSubregions
    TrackTime              = TimeStep%TrackTime
    RegionArea(1:NRegion)  = AppGrid%GetSubregionArea()
    RegionArea(NRegion+1)  = SUM(RegionArea(1:NRegion))
    RegionNames            = ''  ;  RegionNames(1:NRegion) = AppGrid%GetSubregionNames()
    RegionNames(NRegion+1) = 'ENTIRE MODEL AREA'
    
    !Open file
    CALL RootZoneParamFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read away the first version number line to avoid any errors
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Read solution scheme controls
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%IterMax,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(NSoils,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  RootZone%NSoils = NSoils
    CALL RootZoneParamFile%ReadData(FactCN,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Allocate memory
    ALLOCATE (RootZone%SubregionSoilsData(NSoils,NRegion)  , &
              RootZone%SoilRegionPrecip(NSoils,NRegion)    , &
              RootZone%SoilRegionArea(NSoils,NRegion)      , &
              RootZone%WaterSupply(NRegion)                , &
              RootZone%RSoilM_P(NRegion+1,NGroupLandUse)   , &
              RootZone%RSoilM(NRegion+1,NGroupLandUse)     , &
              RootZone%ElemSoilType(NElements)             , &
              RootZone%ElemPrecipData(NElements)           , &
              RootZone%Flags%lLakeElems(NElements)         , &
              STAT=ErrorCode                               )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for root zone soils data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Initialize lake element flag
    RootZone%Flags%lLakeElems = .FALSE.
    
    !Initialize related files
    !-------------------------
    
    !Agricultural data file
    CALL RootZoneParamFile%ReadData(AgDataFile,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    AgDataFile = StripTextUntilCharacter(AgDataFile,'/') 
    CALL CleanSpecialCharacters(AgDataFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(AgDataFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%AgRootZone%New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,AppGrid,FactCN,NSoils,TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
       
    !Urban data file
    CALL RootZoneParamFile%ReadData(UrbanDataFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    UrbanDataFile = StripTextUntilCharacter(UrbanDataFile,'/') 
    CALL CleanSpecialCharacters(UrbanDataFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(UrbanDataFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%UrbanRootZone%New(cAbsPathFileName,cWorkingDirectory,AppGrid,FactCN,NSoils,NLakes,NStrmNodes,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Native/riparian veg. data file
    CALL RootZoneParamFile%ReadData(NVRVFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    NVRVFile = StripTextUntilCharacter(NVRVFile,'/') 
    CALL CleanSpecialCharacters(NVRVFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(NVRVFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%NVRVRootZone%New(cAbsPathFileName,cWorkingDirectory,FactCN,NSoils,NElements,NRegion,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Check if at least one type of land use is specified
    IF ( AgDataFile        .EQ. ''   .AND.   &
         UrbanDataFile     .EQ. ''   .AND.   &
         NVRVFile          .EQ. ''           )  THEN
      MessageArray(1) = 'At least one type of land use and related data should '
      MessageArray(2) = 'be specified for the simulation of root zone processes!' 
      CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
      iStat = -1
      RETURN
    END IF
    
    !Define the component simulation flags
    ASSOCIATE (pFlags => RootZone%Flags)
      IF (AgDataFile .NE. '')      pFlags%lAg_Defined    = .TRUE.
      IF (UrbanDataFile .NE. '')   pFlags%lUrban_Defined = .TRUE.
      IF (NVRVFile .NE. '')        pFlags%lNVRV_Defined  = .TRUE.
    END ASSOCIATE
    
    !Return flow data file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        IF (RootZone%Flags%lAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
            CALL SetLastMessage('Missing return flow fractions data file!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%ReturnFracFile%Init(cAbsPathFileName,'Return flow fractions data file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
        
    !Re-use data file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        IF (RootZone%Flags%lAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
            CALL SetLastMessage('Missing irrigation water re-use factors data file!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%ReuseFracFile%Init(cAbsPathFileName,'Irrigation water re-use factors file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Generic moisture data file
    CALL RootZoneParamFile%ReadData(GenericMoistureFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    GenericMoistureFile = StripTextUntilCharacter(GenericMoistureFile,'/') 
    CALL CleanSpecialCharacters(GenericMoistureFile)
    IF (GenericMoistureFile .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(GenericMoistureFile)),cWorkingDirectory,cAbsPathFileName)
        GenericMoistureFile = cAbsPathFileName
        RootZone%Flags%lGenericMoistureFile_Defined = .TRUE.
    END IF
    
    !Land and water use budget HDF5 output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LWUseBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,NRegion+1,RegionArea,RegionNames,'land and water use budget',cVersionLocal,RootZone%LWUseBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%LWUseBudRawFile_Defined = .TRUE.      
    END IF

    !Root zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,NRegion+1,RegionArea,RegionNames,'root zone budget',cVersionLocal,RootZone%RootZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%RootZoneBudRawFile_Defined = .TRUE.
    END IF
       
    !Are there any flows between elements?
    IF (SIZE(RootZone%ElemFlowToSubregions) .EQ. 0) THEN
        lElemFlowToSubregions = .FALSE.
    ELSE
        lElemFlowToSubregions = .TRUE.
    END IF
    
    !Land and water use zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LWUseZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,cVersionLocal,lElemFlowToSubregions,RootZone%Flags,AppGrid,RootZone%LWUZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%LWUseZoneBudRawFile_Defined = .TRUE.      
    END IF

    !Root zone zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZoneZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,cVersionLocal,lElemFlowToSubregions,RootZone%Flags,AppGrid,RootZone%RootZoneZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%RootZoneZoneBudRawFile_Defined = .TRUE.
    END IF
       
    !End-of-simulation moisture results output
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL RootZone%FinalMoistureOutFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  
        ELSE
            CALL RootZone%FinalMoistureOutFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,iStat=iStat)  
        END IF
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%FinalMoistureOutFile_Defined = .TRUE.
    END IF
 
    !Read soil parameters
    CALL RootZoneParamFile%ReadData(FACTK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)
    RootZone%VarTimeUnit = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    ASSOCIATE (pSoilsData => RootZone%SubregionSoilsData)
        ALLOCATE (DummyRealArray(NSoils,7) , iColGenericMoisture(NSoils,NRegion))
        DO indxRegion=1,NRegion
            CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            READ (ALine,*) iRegion,DummyRealArray(1,:)
            CALL RootZoneParamFile%ReadData(DummyRealArray(2:,:),iStat)  ;  IF (iStat .EQ. -1) RETURN
            
            !Make sure subregions are entered sequentially
            IF (iRegion .NE. indxRegion) THEN
                MessageArray(1) = 'Root zone soil parameters must be entered for each subregion sequentially!'
                MessageArray(2) = 'Expected subregion number = ' // TRIM(IntToText(indxRegion))
                MessageArray(3) = 'Entered subregion number  = ' // TRIM(IntToText(iRegion))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Store parameters in persistent arrays
            pSoilsData(:,indxRegion)%WiltingPoint  =     DummyRealArray(:,1)     
            pSoilsData(:,indxRegion)%FieldCapacity =     DummyRealArray(:,2)
            pSoilsData(:,indxRegion)%TotalPorosity =     DummyRealArray(:,3)
            pSoilsData(:,indxRegion)%Lambda        =     DummyRealArray(:,4)
            pSoilsData(:,indxRegion)%HydCond       =     DummyRealArray(:,5) * FACTK * TimeStep%DeltaT
            pSoilsData(:,indxRegion)%KunsatMethod  = INT(DummyRealArray(:,6))
            iColGenericMoisture(:,indxRegion)      = INT(DummyRealArray(:,7))
        END DO
      
        !Instantiate generic moisture data
        CALL RootZone%GenericMoistureData%New(GenericMoistureFile,NSoils,NRegion,iColGenericMoisture,TrackTime,iStat)
        IF (iStat .EQ. -1) RETURN

        !Check for errors
        DO indxRegion=1,NRegion
            DO indxSoil=1,NSoils
                !Method to compute Kunsat must be recognized
                IF (LocateInList(pSoilsData(indxSoil,indxRegion)%KunsatMethod,KunsatMethodList) .LT. 1) THEN
                    CALL SetLastMessage('Method to compute unsaturated hydraulic conductivity for soil type ' // TRIM(IntTotext(indxSoil)) //' at subregion ' // TRIM(IntToText(indxRegion)) // ' is not recognized!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
        
                !Wilting point should be less than field capacity
                IF (pSoilsData(indxSoil,indxRegion)%WiltingPoint .GE. pSoilsData(indxSoil,indxRegion)%FieldCapacity) THEN
                    CALL SetLastMessage('For soil type ' // TRIM(IntToText(indxSoil)) // ' at subregion ' // TRIM(IntToText(indxregion)) // ' wilting point is greater than or equal to field capacity!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
        
                !Field capacity should be less than or equal to total porosity
                IF (pSoilsData(indxSoil,indxRegion)%FieldCapacity .GT. pSoilsData(indxSoil,indxRegion)%TotalPorosity) THEN
                    CALL SetLastMessage('For soil type ' // TRIM(IntToText(indxSoil)) // ' at subregion ' // TRIM(IntToText(indxRegion)) // ' field capacity is greater than total porosity!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
        END DO
    END ASSOCIATE
    
    !Read soil type, precipitation and runoff destination information
    DEALLOCATE (DummyRealArray)
    ALLOCATE (DummyRealArray(NElements,6))
    CALL RootZoneParamFile%ReadData(DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ASSOCIATE (pPrecipData     => RootZone%ElemPrecipData  , &
               pSoilType       => RootZone%ElemSoilType    , &
               pSoilRegionArea => RootZone%SoilRegionArea  )
        pSoilType                = INT(DummyRealArray(:,2))
        pPrecipData%iColPrecip   = INT(DummyRealArray(:,3))
        pPrecipData%PrecipFactor =     DummyRealArray(:,4)
        SurfaceFlowDestType      = INT(DummyRealArray(:,5))
        SurfaceFlowDest          = INT(DummyRealArray(:,6))
        
        !Initialize (soil,region) areas
        pSoilRegionArea = 0.0
        
        !Check for errors and calculate areas for each (soil,region) combination
        DO indxElem=1,NElements
            iSoilType = pSoilType(indxElem)
            ASSOCIATE (pDestType => SurfaceFlowDestType(indxElem))
                !Make sure soil type is modeled
                IF (iSoilType.LT.1  .OR.  iSoilType.GT.NSoils) THEN
                    CALL SetLastMessage('Soil type at element ' // TRIM(IntToText(indxElem)) // ' is not recognized!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                
                !Make sure that destination types are recognized
                IF (pDestType .NE. FlowDest_Outside    .AND.   &
                    pDestType .NE. FlowDest_StrmNode   .AND.   &
                    pDestType .NE. FlowDest_Lake       .AND.   &
                    pDestType .NE. FlowDest_Subregion  .AND.   &
                    pDestType .NE. FlowDest_GWElement       )  THEN
                    CALL SetLastMessage('Surface flow destination type for element ' // TRIM(IntToText(indxElem)) // ' is not recognized!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
          
                !Make sure destination locations are modeled and acceptable
                SELECT CASE (pDestType)
                   CASE (FlowDest_StrmNode)
                       IF (SurfaceFlowDest(indxElem) .GT. NStrmNodes  .OR.  SurfaceFlowDest(indxElem) .LT. 1) THEN
                           CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(indxElem))//' flows into a stream node ('//TRIM(IntToText(SurfaceFlowDest(indxElem)))//') that is not modeled!',iFatal,ThisProcedure)
                           iStat = -1
                           RETURN
                       END IF
              
                   CASE (FlowDest_Lake)
                       IF (SurfaceFlowDest(indxElem) .GT. NLakes  .OR.  SurfaceFlowDest(indxElem) .LT. 1) THEN
                           CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(indxElem))//' flows into a lake ('//TRIM(IntToText(SurfaceFlowDest(indxElem)))//') that is not modeled!',iFatal,ThisProcedure)
                           iStat = -1
                           RETURN
                       END IF
                  
                   CASE (FlowDest_Subregion)
                       IF (SurfaceFlowDest(indxElem) .GT. NRegion  .OR.  SurfaceFlowDest(indxElem) .LT. 1) THEN
                           CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(indxElem))//' goes to a subregion ('//TRIM(IntToText(SurfaceFlowDest(indxElem)))//') that is not modeled!',iFatal,ThisProcedure)
                           iStat = -1
                           RETURN
                       END IF
                       IF (SurfaceFlowDest(indxElem) .EQ. AppGrid%AppElement(indxElem)%Subregion) THEN
                           CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(indxElem))//' cannot go to the same subregion which the element belongs to!',iFatal,ThisProcedure)
                           iStat = -1
                           RETURN
                       END IF
                END SELECT              
            END ASSOCIATE
            
            !If made to this point, add element area to its corresponding (soil,region) area
            iRegion                            = AppGrid%AppElement(indxElem)%Subregion
            pSoilRegionArea(iSoilType,iRegion) = pSoilRegionArea(iSoilType,iRegion) + AppGrid%AppElement(indxElem)%Area
        END DO
        
        !Compile element-flow-to-outside connection list
        CALL CompileElemSurfaceFlowToDestinationList(FlowDest_Outside,SurfaceFlowDest,SurfaceFlowDestType,ElemFlowToOutside,iStat)  ;  IF (iStat .EQ. -1) RETURN
        ALLOCATE (RootZone%ElemFlowToOutside(SIZE(ElemFlowToOutside)))
        RootZone%ElemFlowToOutside = ElemFlowToOutside%iElement
        
        !Compile element-flow-to-stream-node connection list
        CALL CompileElemSurfaceFlowToDestinationList(FlowDest_StrmNode,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToStreams,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
        !Compile element-flow-to-lake connection list
        CALL CompileElemSurfaceFlowToDestinationList(FlowDest_Lake,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToLakes,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
        !Compile element-flow-to-subregion connection list
        CALL CompileElemSurfaceFlowToDestinationList(FlowDest_Subregion,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToSubregions,iStat)  ;  IF (iStat .EQ. -1) RETURN

        !Compile element-flow-to-groundwater connection list
        CALL CompileElemSurfaceFlowToDestinationList(FlowDest_GWElement,SurfaceFlowDest,SurfaceFlowDestType,ElemFlowToGW,iStat)  ;  IF (iStat .EQ. -1) RETURN
        ALLOCATE (RootZone%ElemFlowToGW(SIZE(ElemFlowToGW)))
        RootZone%ElemFlowToGW = ElemFlowToGW%iElement
        
    END ASSOCIATE
    
    !Check if data column pointers are referring to existing data columns
    CALL CheckTSDataPointers(RootZone,Precip,ET,iStat)
    IF (iStat .EQ. -1) RETURN

    !Close file
    CALL RootZoneParamFile%Kill()

    !Clear memory
    DEALLOCATE (DummyRealArray , iColGenericMoisture , ElemFlowToOutside , ElemFlowToGW , cAbsPathFileName , STAT=ErrorCode)
    
  END SUBROUTINE RootZone_v50_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW HDF5 LAND AND WATER USE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE LWUseZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,cVersion,lElemFlowToSubregions,Flags,AppGrid,ZBudFile,iStat)
    LOGICAL,INTENT(IN)              :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: NTIME
    CHARACTER(LEN=*),INTENT(IN)     :: cVersion
    LOGICAL,INTENT(IN)              :: lElemFlowToSubregions
    TYPE(Flags_v50_Type),INTENT(IN) :: Flags
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    TYPE(ZBudgetType)               :: ZBudFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'LWUseZoneBudRawFile_New'
    CHARACTER(LEN=13),PARAMETER            :: cArea = ZBud_MarkerChar // '        (' // ZBud_AreaUnitMarker // ')' // ZBud_MarkerChar
    INTEGER                                :: indxElem,indxVertex,ErrorCode,indxFace
    TYPE(TimeStepType)                     :: TimeStepLocal
    TYPE(ZBudgetHeaderType)                :: Header
    TYPE(SystemDataType)                   :: SystemData
    
    !Initialize
    iStat = 0
    
    !If this is for inquiry, open file for reading and return
    IF (IsForInquiry) THEN
        IF (cFileName .NE. '') CALL ZBudFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Time step received shows the timestamp at t=0; advance time to show that Z-Budget output is at t = 1
    TimeStepLocal                    = TimeStep
    TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes,1)
    TimeStepLocal%CurrentTimeStep    = 1
    
    !Compile system data
    SystemData%NNodes    = AppGrid%NNodes
    SystemData%NElements = AppGrid%NElements
    SystemData%NLayers   = 1
    SystemData%NFaces    = AppGrid%NFaces
    ALLOCATE (SystemData%iElementNNodes(AppGrid%NElements)                , &
              SystemData%iElementNodes(4,AppGrid%NElements)               , &
              SystemData%iFaceElems(2,AppGrid%NFaces)                     , &
              SystemData%lBoundaryFace(AppGrid%NFaces)                    , &
              SystemData%lActiveNode(AppGrid%NNodes,1)                    , &
              SystemData%rNodeAreas(AppGrid%NNodes)                       , &
              SystemData%rElementAreas(AppGrid%NElements)                 , &
              SystemData%rElementNodeAreas(4,AppGrid%NElements)           , &
              SystemData%rElementNodeAreaFractions(4,AppGrid%NElements)   )
    SystemData%rNodeAreas     = AppGrid%AppNode%Area
    SystemData%rElementAreas  = AppGrid%AppElement%Area
    SystemData%iElementNNodes = AppGrid%Element%NVertex
    DO indxElem=1,AppGrid%NElements
        SystemData%iElementNodes(:,indxElem) = AppGrid%Element(indxElem)%Vertex
        DO indxVertex=1,AppGrid%Element(indxElem)%NVertex
            SystemData%rElementNodeAreas(indxVertex,indxElem)         = AppGrid%AppElement(indxElem)%VertexArea(indxVertex)
            SystemData%rElementNodeAreaFractions(indxVertex,indxElem) = AppGrid%AppElement(indxElem)%VertexAreaFraction(indxVertex)
        END DO
        IF (AppGrid%Element(indxElem)%NVertex .EQ. 3) THEN
            SystemData%rElementNodeAreas(4,indxElem)         = 0.0
            SystemData%rElementNodeAreaFractions(4,indxElem) = 0.0
        END IF
    END DO
    DO indxFace=1,AppGrid%NFaces
        SystemData%iFaceElems(:,indxFace) = AppGrid%AppFace(indxFace)%Element
    END DO
    SystemData%lBoundaryFace = AppGrid%AppFace%BoundaryFace
    SystemData%lActiveNode   = .TRUE.
    
    !Compile Header data
    Header%cSoftwareVersion   = 'IWFM ROOT ZONE PACKAGE (' // TRIM(cVersion) // ')'
    Header%cDescriptor        = 'Land and water use zone budget'
    Header%lFaceFlows_Defined = .FALSE.
    Header%lStorages_Defined  = .FALSE.
    Header%lComputeError      = .FALSE.
    Header%iNData             = NLWUseBudColumns
    ALLOCATE (Header%iDataTypes(NLWUseBudColumns)                           , &
              Header%cFullDataNames(NLWUseBudColumns)                       , &
              Header%cDataHDFPaths(NLWUseBudColumns)                        , &
              Header%iNDataElems(NLWUseBudColumns,1)                        , &
              Header%iElemDataColumns(AppGrid%NElements,NLWUseBudColumns,1) , &
              !Header%iErrorInCols()                                        , &  ! Since mass balance error is not calcuated no need
              !Header%iErrorOutCols()                                       , &  !  to allocate these arrays
              Header%cDSSFParts(NLWUseBudColumns)                           , &
              Header%ASCIIOutput%cColumnTitles(5)                           , &
              STAT = ErrorCode                                              )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for land and water use Z-Budget file!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Header%iDataTypes = [AR                 ,&  !Ag area
                         VR_lwu_PotCUAW     ,&  !Potential CUAW
                         VR_lwu_AgSupplyReq ,&  !Ag supply req.
                         VR_lwu_AgPump      ,&  !Pumping for ag
                         VR_lwu_AgDiv       ,&  !Diversion for ag
                         VR_lwu_AgOthIn     ,&  !Ag inflow as surface runoff from upstream elements
                         VR_lwu_AgShort     ,&  !Ag supply shortage
                         VR                 ,&  !ETAW
                         VR                 ,&  !ETP
                         VR                 ,&  !ETOth
                         AR                 ,&  !Urban area
                         VR                 ,&  !Urban supply req.
                         VR                 ,&  !Pumping for urban
                         VR                 ,&  !Diversion for urban
                         VR                 ,&  !Urban inflow as surface runoff from upstream elements
                         VR                 ]   !Urban supply shortage
    Header%cFullDataNames    = cLWUseBudgetColumnTitles
    Header%cFullDataNames(1)  = TRIM(Header%cFullDataNames(1)) // cArea
    Header%cFullDataNames(11) = TRIM(Header%cFullDataNames(11)) // cArea
    Header%cDataHDFPaths      = cLWUseBudgetColumnTitles
    Header%iNDataElems        = 0
    Header%iElemDataColumns   = 0
    IF (Flags%lAg_Defined) THEN
        Header%iNDataElems(1:5,:)  = AppGrid%NElements
        Header%iNDataElems(7:10,:) = AppGrid%NElements
        IF (lElemFlowToSubregions) Header%iNDataElems(6,:) = AppGrid%NElements     !Not all applications will have surface inflow for ag and urban water use; update accordingly
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,1:5,:)  = indxElem
            Header%iElemDataColumns(indxElem,7:10,:) = indxElem
            IF (lElemFlowToSubregions) Header%iElemDataColumns(indxElem,6,:) = indxElem
        END DO
    END IF
    IF (Flags%lUrban_Defined) THEN
        Header%iNDataElems(11:14,:) = AppGrid%NElements
        Header%iNDataElems(16,:)    = AppGrid%NElements
        IF (lElemFlowToSubregions) Header%iNDataElems(15,:) = AppGrid%NElements     !Not all applications will have surface inflow for ag and urban water use; update accordingly
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,11:14,:)  = indxElem
            Header%iElemDataColumns(indxElem,16,:)     = indxElem
            IF (lElemFlowToSubregions) Header%iElemDataColumns(indxElem,15,:) = indxElem
        END DO
    END IF
    Header%ASCIIOutput%iNTitles         = 5
    Header%ASCIIOutput%iLenColumnTitles = 228
    Header%ASCIIOutput%cColumnTitles(1) = '                                                                         Agricultural Area                                                                                              Urban Area                                   '
    Header%ASCIIOutput%cColumnTitles(2) = '                 ----------------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------------------------------ '
    Header%ASCIIOutput%cColumnTitles(3) = '                                            Agricultural                            Inflow as                                               ET                          Urban                               Inflow as                '
    Header%ASCIIOutput%cColumnTitles(4) = '      Time               Area    Potential     Supply         Pumping  Deliveries  Srfc. Runoff     Shortage                Effective   from Other            Area      Supply        Pumping  Deliveries  Srfc. Runoff     Shortage '
    Header%ASCIIOutput%cColumnTitles(5) = '                 '//cArea//'     CUAW      Requirement        (-)        (-)          (-)            (=)          ETAW      Precip      Sources     '//cArea//'   Requirement       (-)        (-)          (-)            (=)    '
    Header%ASCIIOutput%cNumberFormat    = '(A16,10(2X,F11.1),3X,6(2X,F11.1))'
    Header%cDSSFParts = ['AG_AREA'         ,&
                         'AG_POTNL_CUAW'   ,&
                         'AG_SUP_REQ'      ,&    
                         'AG_PUMPING'      ,&
                         'AG_DELIVERY'     ,&
                         'AG_SR_INFLOW'    ,&
                         'AG_SHORTAGE'     ,&
                         'AG_ETAW'         ,&
                         'AG_EFF_PRECIP'   ,&
                         'AG_ET_OTH'       ,&
                         'URB_AREA'        ,&
                         'URB_SUP_REQ'     ,&       
                         'URB_PUMPING'     ,&
                         'URB_DELIVERY'    ,&
                         'URB_SR_INFLOW'   ,&
                         'URB_SHORTAGE'    ]
                             
    !Instantiate Z-Budget file
    CALL ZBudFile%New(cFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE LWUseZoneBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW HDF5 LAND AND WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE LWUseBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType)              :: RawFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: NTitles            = 6   , &
                              TitleLen           = 229 , &        
                              NColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(NLWUseBudColumns) = (/'AREA'   , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'AREA'   , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME'/)
    CHARACTER(LEN=13)      :: FParts(NLWUseBudColumns) = (/'AG_AREA'         ,&
                                                           'AG_POTNL_CUAW'   ,&
                                                           'AG_SUP_REQ'      ,&    
                                                           'AG_PUMPING'      ,&
                                                           'AG_DELIVERY'     ,&
                                                           'AG_SR_INFLOW'    ,&
                                                           'AG_SHORTAGE'     ,&
                                                           'AG_ETAW'         ,&
                                                           'AG_EFF_PRECIP'   ,&
                                                           'AG_ET_OTH'       ,&
                                                           'URB_AREA'        ,&
                                                           'URB_SUP_REQ'     ,&       
                                                           'URB_PUMPING'     ,&
                                                           'URB_DELIVERY'    ,&
                                                           'URB_SR_INFLOW'   ,&
                                                           'URB_SHORTAGE'    /)
    
    !Initialize
    iStat = 0

    !Instantiate the land and water use raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    
    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen           = TitleLen
      pASCIIOutput%NTitles            = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
      pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(5)         = REPEAT(' ',73)//'Agricultural Area'//REPEAT(' ',94)//'Urban Area'
      pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',130)//REPEAT(' ',3)//REPEAT('-',78)
      pASCIIOutput%lTitlePersist(1:3) = .TRUE.
      pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,10(F12.1,1X),3X,6(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                       , &
              OutputData%Locations(1)%cFullColumnHeaders(NLWUseBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(NLWUseBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(NLWUseBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(NLWUseBudColumns+1,NColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)          )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns           = NLWUseBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                               
      pLocation%cFullColumnHeaders(2:) = cLWUseBudgetColumnTitles
      pLocation%cFullColumnHeaders(2)  = TRIM(pLocation%cFullColumnHeaders(2))  // ' ('//AreaUnitMarker//')'
      pLocation%cFullColumnHeaders(12) = TRIM(pLocation%cFullColumnHeaders(12)) // ' ('//AreaUnitMarker//')'
      pLocation%iDataColumnTypes       = [AR                 ,&  !Ag area
                                          VR_lwu_PotCUAW     ,&  !Potential CUAW
                                          VR_lwu_AgSupplyReq ,&  !Ag supply req.
                                          VR_lwu_AgPump      ,&  !Pumping for ag
                                          VR_lwu_AgDiv       ,&  !Deliveries for ag
                                          VR_lwu_AgOthIn     ,&  !Ag inflow as surface runoff from upstream elements
                                          VR_lwu_AgShort     ,&  !Ag supply shortage
                                          VR                 ,&  !ETAW
                                          VR                 ,&  !ETP
                                          VR                 ,&  !ETOth
                                          AR                 ,&  !Urban area
                                          VR                 ,&  !Urban supply req.
                                          VR                 ,&  !Pumping for urban
                                          VR                 ,&  !Deliveries for urban
                                          VR                 ,&  !Urban inflow as surface runoff from upstream elements
                                          VR                 ]  !Urban supply shortage
      pLocation%iColWidth             = [17,12,14,(13,indxCol=1,7),12,14,(13,indxCol=1,3)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(AreaUnitMarker)//')'
        pColumnHeaders(:,1) = (/'                 ','            ','    Potential ',' Agricultural','             ','             ','  Inflow as  ','             ','             ','             ','      ET     ','            ','     Urban    ','             ','             ','  Inflow as  ','             '/)
        pColumnHeaders(:,2) = (/'      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping',' Deliveries  ',' Srfc. Runoff','     Shortage','             ','   Effective ','  from Other ','        Area','     Supply   ','      Pumping',' Deliveries  ',' Srfc. Runoff','     Shortage'/)
        pColumnHeaders(:,3) = (/               Text,         Text1,'              ','  Requirement','        (-)  ','     (-)     ','     (-)     ','       (=)   ','       ETAW  ','    Precip   ','   Sources   ',         Text1,'   Requirement','        (-)  ','     (-)     ','     (-)     ','       (=)   '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',TitleLen)//'",'//TRIM(IntToText(NLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(NLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,NLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = (/PER_AVER,(PER_CUM,indxCol=1,8),PER_AVER,(PER_CUM,indxCol=1,4)/)
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE LWUseBudRawFile_New

  
  ! -------------------------------------------------------------
  ! --- NEW HDF5 ROOT ZONE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZoneZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,cVersion,lElemFlowToSubregions,Flags,AppGrid,ZBudFile,iStat)
    LOGICAL,INTENT(IN)              :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    INTEGER,INTENT(IN)              :: NTIME
    CHARACTER(LEN=*),INTENT(IN)     :: cVersion
    LOGICAL,INTENT(IN)              :: lElemFlowToSubregions
    TYPE(Flags_v50_Type),INTENT(IN) :: Flags
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    TYPE(ZBudgetType)               :: ZBudFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26),PARAMETER :: ThisProcedure = ModName // 'RootZoneZoneBudRawFile_New'
    CHARACTER(LEN=15),PARAMETER            :: cArea = ZBud_MarkerChar // '          (' // ZBud_AreaUnitMarker // ')' // ZBud_MarkerChar
    INTEGER                                :: indxElem,indxVertex,ErrorCode,indxFace
    TYPE(TimeStepType)                     :: TimeStepLocal
    TYPE(ZBudgetHeaderType)                :: Header
    TYPE(SystemDataType)                   :: SystemData
    
    !Initialize
    iStat = 0
    
    !If this is for inquiry, open file for reading and return
    IF (IsForInquiry) THEN
        IF (cFileName .NE. '') CALL ZBudFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Time step received shows the timestamp at t=0; advance time to show that Z-Budget output is at t = 1
    TimeStepLocal                    = TimeStep
    TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes,1)
    TimeStepLocal%CurrentTimeStep    = 1
    
    !Compile system data
    SystemData%NNodes    = AppGrid%NNodes
    SystemData%NElements = AppGrid%NElements
    SystemData%NLayers   = 1
    SystemData%NFaces    = AppGrid%NFaces
    ALLOCATE (SystemData%iElementNNodes(AppGrid%NElements)                , &
              SystemData%iElementNodes(4,AppGrid%NElements)               , &
              SystemData%iFaceElems(2,AppGrid%NFaces)                     , &
              SystemData%lBoundaryFace(AppGrid%NFaces)                    , &
              SystemData%lActiveNode(AppGrid%NNodes,1)                    , &
              SystemData%rNodeAreas(AppGrid%NNodes)                       , &
              SystemData%rElementAreas(AppGrid%NElements)                 , &
              SystemData%rElementNodeAreas(4,AppGrid%NElements)           , &
              SystemData%rElementNodeAreaFractions(4,AppGrid%NElements)   )
    SystemData%rNodeAreas     = AppGrid%AppNode%Area
    SystemData%rElementAreas  = AppGrid%AppElement%Area
    SystemData%iElementNNodes = AppGrid%Element%NVertex
    DO indxElem=1,AppGrid%NElements
        SystemData%iElementNodes(:,indxElem) = AppGrid%Element(indxElem)%Vertex
        DO indxVertex=1,AppGrid%Element(indxElem)%NVertex
            SystemData%rElementNodeAreas(indxVertex,indxElem)         = AppGrid%AppElement(indxElem)%VertexArea(indxVertex)
            SystemData%rElementNodeAreaFractions(indxVertex,indxElem) = AppGrid%AppElement(indxElem)%VertexAreaFraction(indxVertex)
        END DO
        IF (AppGrid%Element(indxElem)%NVertex .EQ. 3) THEN
            SystemData%rElementNodeAreas(4,indxElem)         = 0.0
            SystemData%rElementNodeAreaFractions(4,indxElem) = 0.0
        END IF
    END DO
    DO indxFace=1,AppGrid%NFaces
        SystemData%iFaceElems(:,indxFace) = AppGrid%AppFace(indxFace)%Element
    END DO
    SystemData%lBoundaryFace = AppGrid%AppFace%BoundaryFace
    SystemData%lActiveNode   = .TRUE.
    
    !Compile Header data
    Header%cSoftwareVersion   = 'IWFM ROOT ZONE PACKAGE (' // TRIM(cVersion) // ')'
    Header%cDescriptor        = 'Root zone zone budget'
    Header%lFaceFlows_Defined = .FALSE.
    Header%lStorages_Defined  = .FALSE.
    Header%lComputeError      = .FALSE.
    Header%iNData             = NRootZoneBudColumns
    ALLOCATE (Header%iDataTypes(NRootZoneBudColumns)                           , &
              Header%cFullDataNames(NRootZoneBudColumns)                       , &
              Header%cDataHDFPaths(NRootZoneBudColumns)                        , &
              Header%iNDataElems(NRootZoneBudColumns,1)                        , &
              Header%iElemDataColumns(AppGrid%NElements,NRootZoneBudColumns,1) , &
              !Header%iErrorInCols()                                           , &  ! Since mass balance error is not calcuated no need
              !Header%iErrorOutCols()                                          , &  !  to allocate these arrays
              Header%cDSSFParts(NRootZoneBudColumns)                           , &
              Header%ASCIIOutput%cColumnTitles(5)                              , &
              STAT = ErrorCode                                                 )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for root zone Z-Budget file!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Header%iDataTypes = [AR ,&  !Ag area
                         VR ,&  !Ag potential ET
                         VR ,&  !Ag precipitation
                         VR ,&  !Ag runoff
                         VR ,&  !Ag prime applied water
                         VR ,&  !Ag applied water from upstream element surface runoff
                         VR ,&  !Ag re-used water
                         VR ,&  !Ag return flow
                         VLB,&  !Ag beginning storage
                         VR ,&  !Ag net gain from land expansion
                         VR ,&  !Ag infiltration
                         VR ,&  !Ag generic inflow
                         VR ,&  !Ag actual ET
                         VR ,&  !Ag percolation
                         VLE,&  !Ag ending storage
                         VR ,&  !Ag discrepancy
                         AR ,&  !Urban area
                         VR ,&  !Urban potential ET
                         VR ,&  !Urban precipitation
                         VR ,&  !Urban runoff
                         VR ,&  !Urban prime applied water
                         VR ,&  !Urban applied water due to upstream element surface runoff
                         VR ,&  !Urban re-used water
                         VR ,&  !Urban return flow
                         VLB,&  !Urban beginning storage
                         VR ,&  !Urban net gain from land expansion
                         VR ,&  !Urban infiltration
                         VR ,&  !Urban generic inflow
                         VR ,&  !Urban actual ET
                         VR ,&  !Urban percolation
                         VLE,&  !Urban ending storage
                         VR ,&  !Urban discrepancy
                         AR ,&  !NV&RV area
                         VR ,&  !NV&RV potential ET
                         VR ,&  !NV&RV precipitation
                         VR ,&  !NV&RV surface runoff from upstream elements/subregions
                         VR ,&  !NV&RV runoff
                         VLB,&  !NV&RV beginning storage
                         VR ,&  !NV&RV net gain from land expansion
                         VR ,&  !NV&RV infiltration
                         VR ,&  !NV&RV generic inflow
                         VR ,&  !NV&RV actual ET
                         VR ,&  !NV&RV percolation
                         VLE,&  !NV&RV ending storage
                         VR ]   !NV&RV discrepancy
    Header%cFullDataNames     = cRootZoneBudgetColumnTitles
    Header%cFullDataNames(1)  = TRIM(Header%cFullDataNames(1)) // cArea
    Header%cFullDataNames(17) = TRIM(Header%cFullDataNames(17)) // cArea
    Header%cFullDataNames(33) = TRIM(Header%cFullDataNames(33)) // cArea
    Header%cDataHDFPaths      = cRootZoneBudgetColumnTitles
    Header%iNDataElems        = 0
    Header%iElemDataColumns   = 0
    IF (Flags%lAg_Defined) THEN
        Header%iNDataElems(1:16,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,1:16,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(6,:)        = 0
            Header%iElemDataColumns(:,6,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(12,:)        = 0
            Header%iElemDataColumns(:,12,:) = 0
        END IF
    END IF
    IF (Flags%lUrban_Defined) THEN
        Header%iNDataElems(17:32,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,17:32,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(22,:)        = 0
            Header%iElemDataColumns(:,22,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(28,:)        = 0
            Header%iElemDataColumns(:,28,:) = 0
        END IF
    END IF
    IF (Flags%lNVRV_Defined) THEN
        Header%iNDataElems(33:,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,33:,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(36,:)        = 0
            Header%iElemDataColumns(:,36,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(41,:)        = 0
            Header%iElemDataColumns(:,41,:) = 0
        END IF
    END IF
    Header%ASCIIOutput%iNTitles         = 5
    Header%ASCIIOutput%iLenColumnTitles = 698
    Header%ASCIIOutput%cColumnTitles(1) = '                                                                                                                                Agricultural Area                                                                                                                                                                                                                                     Urban Area                                                                                                                                                                                                       Native & Riparian Vegetation Area                                                                                  ' 
    Header%ASCIIOutput%cColumnTitles(2) = '                 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
    Header%ASCIIOutput%cColumnTitles(3) = '                                                                                    Prime      Inflow as            Reused          Net       Beginning  Net Gain from                        Other           Actual                       Ending                                                                                      Prime      Inflow as            Reused          Net       Beginning  Net Gain from                        Other           Actual                       Ending                                                                   Inflow as                       Beginning  Net Gain from                        Other           Actual                       Ending                '
    Header%ASCIIOutput%cColumnTitles(4) = '      Time                 Area      Potential   Precipitation         Runoff      Applied   Surface Runoff         Water          Return      Storage   Land Expansion   Infiltration        Inflow            ET      Percolation        Storage    Discrepancy             Area      Potential   Precipitation         Runoff      Applied   Surface Runoff         Water          Return      Storage   Land Expansion   Infiltration        Inflow            ET      Percolation        Storage    Discrepancy             Area      Potential  Precipitation  Surface Runoff        Runoff       Storage   Land Expansion   Infiltration        Inflow            ET      Percolation        Storage    Discrepancy'
    Header%ASCIIOutput%cColumnTitles(5) = '                 '    //cArea//'        ET                                          Water                                           Flow         (+)           (+)             (+)              (+)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                          Water                                           Flow         (+)           (+)             (+)              (+)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                                          (+)           (+)             (+)              (+)             (-)        (-)               (-)          (=)    '
    Header%ASCIIOutput%cNumberFormat    = '(A16,16(2X,F13.1),3X,16(2X,F13.1),3X,13(2X,F13.1))'
    Header%cDSSFParts = ['AG_AREA'           ,&
                         'AG_POT_ET'         ,&   
                         'AG_PRECIP'         ,&   
                         'AG_RUNOFF'         ,&   
                         'AG_PRM_H2O'        ,&
                         'AG_SR_INFLOW'      ,&
                         'AG_RE-USE'         ,&   
                         'AG_NT_RTRN_FLOW'   ,&   
                         'AG_BEGIN_STOR'     ,&   
                         'AG_GAIN_EXP'       ,&   
                         'AG_INFILTR'        ,& 
                         'AG_OTHER_INFLOW'   ,&
                         'AG_ET'             ,&   
                         'AG_PERC'           ,&   
                         'AG_END_STOR'       ,&  
                         'AG_DISCREPANCY'    ,& 
                         'URB_AREA'          ,&  
                         'URB_POT_ET'        ,&  
                         'URB_PRECIP'        ,&  
                         'URB_RUNOFF'        ,&  
                         'URB_PRM_H2O'       ,& 
                         'URB_SR_INFLOW'     ,&
                         'URB_RE-USE'        ,&     
                         'URB_NT_RTRN_FLOW'  ,&     
                         'URB_BEGIN_STOR'    ,&     
                         'URB_GAIN_EXP'      ,&     
                         'URB_INFILTR'       ,&     
                         'URB_OTHER_INFLOW'  ,&
                         'URB_ET'            ,&     
                         'URB_PERC'          ,&     
                         'URB_END_STOR'      ,& 
                         'URB_DISCREPANCY'   ,&    
                         'NRV_AREA'          ,&  
                         'NRV_POT_ET'        ,&
                         'NRV_PRECIP'        ,&
                         'NRV_SR_INFLOW'     ,&  
                         'NRV_RUNOFF'        ,&  
                         'NRV_BEGIN_STOR'    ,&     
                         'NRV_GAIN_EXP'      ,&     
                         'NRV_INFILTR'       ,&     
                         'NRV_OTHER_INFLOW'  ,&
                         'NRV_ET'            ,&     
                         'NRV_PERC'          ,&     
                         'NRV_END_STOR'      ,&
                         'NRV_DISCREPANCY'   ]
                             
    !Instantiate Z-Budget file
    CALL ZBudFile%New(cFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE RootZoneZoneBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW HDF5 ROOT ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType)              :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: NTitles            = 6   , &
                              TitleLen           = 698 , &        
                              NColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(NRootZoneBudColumns) = (/'AREA'   , &
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
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'AREA'   , &
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
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'AREA'   , &
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
                                                              'VOLUME'/)
    CHARACTER(LEN=15)      :: FParts(NRootZoneBudColumns) = (/'AG_AREA'           ,&
                                                              'AG_POT_ET'         ,&   
                                                              'AG_PRECIP'         ,&   
                                                              'AG_RUNOFF'         ,&   
                                                              'AG_PRM_H2O'        ,&
                                                              'AG_SR_INFLOW'      ,&
                                                              'AG_RE-USE'         ,&   
                                                              'AG_NT_RTRN_FLOW'   ,&   
                                                              'AG_BEGIN_STOR'     ,&   
                                                              'AG_GAIN_EXP'       ,&   
                                                              'AG_INFILTR'        ,& 
                                                              'AG_OTHER_INFLOW'   ,&
                                                              'AG_ET'             ,&   
                                                              'AG_PERC'           ,&   
                                                              'AG_END_STOR'       ,&  
                                                              'AG_DISCREPANCY'    ,& 
                                                              'URB_AREA'          ,&  
                                                              'URB_POT_ET'        ,&  
                                                              'URB_PRECIP'        ,&  
                                                              'URB_RUNOFF'        ,&  
                                                              'URB_PRM_H2O'       ,& 
                                                              'URB_SR_INFLOW'     ,&
                                                              'URB_RE-USE'        ,&     
                                                              'URB_NT_RTRN_FLOW'  ,&     
                                                              'URB_BEGIN_STOR'    ,&     
                                                              'URB_GAIN_EXP'      ,&     
                                                              'URB_INFILTR'       ,&     
                                                              'URB_OTHER_INFLOW'  ,&
                                                              'URB_ET'            ,&     
                                                              'URB_PERC'          ,&     
                                                              'URB_END_STOR'      ,& 
                                                              'URB_DISCREPANCY'   ,&    
                                                              'NRV_AREA'          ,&  
                                                              'NRV_POT_ET'        ,&
                                                              'NRV_PRECIP'        ,&
                                                              'NRV_SR_INFLOW'     ,&  
                                                              'NRV_RUNOFF'        ,&  
                                                              'NRV_BEGIN_STOR'    ,&     
                                                              'NRV_GAIN_EXP'      ,&     
                                                              'NRV_INFILTR'       ,&     
                                                              'NRV_OTHER_INFLOW'  ,&
                                                              'NRV_ET'            ,&     
                                                              'NRV_PERC'          ,&     
                                                              'NRV_END_STOR'      ,&
                                                              'NRV_DISCREPANCY'   /) 
    
    !Initialize
    iStat = 0
                                                  
    !Instantiate the root zone budget raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF

    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
        ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(5)         = REPEAT(' ',128)//'Agricultural Area'//REPEAT(' ',229)//'Urban Area'//REPEAT(' ',199)//'Native & Riparian Vegetation Area'
        pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',240)//REPEAT(' ',3)//REPEAT('-',240)//REPEAT(' ',3)//REPEAT('-',195)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,16(F14.1,1X),3X,16(F14.1,1X),3X,13(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                          , &
              OutputData%Locations(1)%cFullColumnHeaders(NRootZoneBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(NRootZoneBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(NRootZoneBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(NRootZoneBudColumns+1,NColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns           = NRootZoneBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                                                   
      pLocation%cFullColumnHeaders(2:) = cRootZoneBudgetColumnTitles
      pLocation%cFullColumnHeaders(2)  = TRIM(pLocation%cFullColumnHeaders(2))  // ' ('//AreaUnitMarker//')'                        
      pLocation%cFullColumnHeaders(18) = TRIM(pLocation%cFullColumnHeaders(18)) // ' ('//AreaUnitMarker//')'                        
      pLocation%cFullColumnHeaders(34) = TRIM(pLocation%cFullColumnHeaders(34)) // ' ('//AreaUnitMarker//')'                        
      pLocation%iDataColumnTypes       = [AR ,&  !Ag area
                                          VR ,&  !Ag potential ET
                                          VR ,&  !Ag precipitation
                                          VR ,&  !Ag runoff
                                          VR ,&  !Ag prime applied water
                                          VR ,&  !Ag applied water from upstream element surface runoff
                                          VR ,&  !Ag re-used water
                                          VR ,&  !Ag return flow
                                          VLB,&  !Ag beginning storage
                                          VR ,&  !Ag net gain from land expansion
                                          VR ,&  !Ag infiltration
                                          VR ,&  !Ag generic inflow
                                          VR ,&  !Ag actual ET
                                          VR ,&  !Ag percolation
                                          VLE,&  !Ag ending storage
                                          VR ,&  !Ag discrepancy
                                          AR ,&  !Urban area
                                          VR ,&  !Urban potential ET
                                          VR ,&  !Urban precipitation
                                          VR ,&  !Urban runoff
                                          VR ,&  !Urban prime applied water
                                          VR ,&  !Urban applied water due to upstream element surface runoff
                                          VR ,&  !Urban re-used water
                                          VR ,&  !Urban return flow
                                          VLB,&  !Urban beginning storage
                                          VR ,&  !Urban net gain from land expansion
                                          VR ,&  !Urban infiltration
                                          VR ,&  !Urban generic inflow
                                          VR ,&  !Urban actual ET
                                          VR ,&  !Urban percolation
                                          VLE,&  !Urban ending storage
                                          VR ,&  !Urban discrepancy
                                          AR ,&  !NV&RV area
                                          VR ,&  !NV&RV potential ET
                                          VR ,&  !NV&RV precipitation
                                          VR ,&  !NV&RV surface runoff from upstream elements/subregions
                                          VR ,&  !NV&RV runoff
                                          VLB,&  !NV&RV beginning storage
                                          VR ,&  !NV&RV net gain from land expansion
                                          VR ,&  !NV&RV infiltration
                                          VR ,&  !NV&RV generic inflow
                                          VR ,&  !NV&RV actual ET
                                          VR ,&  !NV&RV percolation
                                          VLE,&  !NV&RV ending storage
                                          VR ]  !NV&RV discrepancy
      pLocation%iColWidth              = [17,14,15,16,(14,indxCol=1,14),14,15,16,(15,indxCol=1,13),14,15,16,(15,indxCol=1,10)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(AreaUnitMarker)//')'
        pColumnHeaders(:,1) = (/'                 ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','         Actual','               ','        Ending ','               ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','         Actual','               ','        Ending ','               ','              ','               ','                ','   Inflow as   ','               ','     Beginning ',' Net Gain from ','               ','        Other  ','         Actual','               ','        Ending ','               '/)
        pColumnHeaders(:,2) = (/'      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','  Precipitation ',' Surface Runoff','        Runoff ','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy'/)
        pColumnHeaders(:,3) = (/               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','               ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A14,A15,A16,13A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(2)     = '(A17,A14,A15,A16,13A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(3)     = '(A17,A14,A15,A16,13A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NRootZoneBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(NRootZoneBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,NRootZoneBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = (/PER_AVER,(PER_CUM,indxCol=1,15),PER_AVER,(PER_CUM,indxCol=1,15),PER_AVER,(PER_CUM,indxCol=1,12)/)
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
    !Free memory
    CALL OutputData%Kill()
    
  END SUBROUTINE RootZoneBudRawFile_New
  
  


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
  ! --- KILL ROOT ZONE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_Kill(RootZone)
    CLASS(RootZone_v50_Type) :: RootZone
    
    !Local variables
    INTEGER               :: ErrorCode
    TYPE(SolverDataType)  :: DummySolverData
    TYPE(Flags_v50_Type) :: DummyFlags
    
    !Deallocate arrays
    DEALLOCATE (RootZone%Flags%lLakeElems   , &
                RootZone%SubregionSoilsData , &
                RootZone%ElemSoilType       , &
                RootZone%SoilRegionPrecip   , &
                RootZone%SoilRegionArea     , &
                RootZone%WaterSupply        , &
                STAT = ErrorCode            )
    
    !Kill components
    CALL RootZone%AgRootZone%Kill()
    CALL RootZone%UrbanRootZone%Kill()
    CALL RootZone%NVRVRootZone%Kill()
    CALL RootZone%GenericMoistureData%Kill()
    CALL RootZone%LWUZoneBudRawFile%Kill()
    CALL RootZone%RootZoneZoneBudRawFile%Kill()
    
    !Default other components
    RootZone%NSoils     = 0
    RootZone%Flags      = DummyFlags
    RootZone%SolverData = DummySolverData
    
  END SUBROUTINE RootZone_v50_Kill
  
  


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- IS LAND USE UPDATED
  ! -------------------------------------------------------------
  FUNCTION RootZone_v50_IsLandUseUpdated(RootZone) RESULT(lUpdated)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    LOGICAL                             :: lUpdated
    
    lUpdated = RootZone%AgRootZone%SubregionCropAreaDataFile%lUpdated   .OR.  &
               RootZone%AgRootZone%ElemAgAreaDataFile%lUpdated          .OR.  &
               RootZone%UrbanRootZone%LandUseDataFile%lUpdated          .OR.  &
               RootZone%NVRVRootZone%LandUseDataFile%lUpdated
    
  END FUNCTION RootZone_v50_IsLandUseUpdated
  
  


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
  ! --- GET THE NUMBER OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  FUNCTION RootZone_v50_GetNDataList_AtLocationType(RootZone,iLocationType) RESULT(NData)
     CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                  :: iLocationType
     INTEGER                             :: NData
     
     !Initialize
     NData = 0
     
     SELECT CASE (iLocationType)
         CASE (iLocationType_Subregion)
             !Land and water use budget
             IF (RootZone%Flags%LWUseBudRawFile_Defined) THEN
                 NData = 1
             END IF
             
             !Root zone budget
             IF (RootZone%Flags%RootZoneBudRawFile_Defined) THEN
                 NData = NData + 1
             END IF
                          
             
         CASE (iLocationType_Zone)
             !Land and water use zone budget
             IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                 NData = 1
             END IF
             
             !Root zone zone budget
             IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                 NData = NData + 1
             END IF
                                       
    END SELECT
     
  END FUNCTION RootZone_v50_GetNDataList_AtLocationType


  ! -------------------------------------------------------------
  ! --- GET THE LIST OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetDataList_AtLocationType(RootZone,iLocationType,cDataList,cFileList,lBudgetType)
     CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                  :: iLocationType
     CHARACTER(LEN=*),ALLOCATABLE        :: cDataList(:),cFileList(:)
     LOGICAL,ALLOCATABLE                 :: lBudgetType(:)
     
     !Local variables
     INTEGER                  :: iCount,ErrorCode
     LOGICAL                  :: lBudgetType_Local(10)
     CHARACTER(LEN=500)       :: cFileList_Local(10),cDataList_Local(10)
     CHARACTER(:),ALLOCATABLE :: cFileName
     
     !Initialize
     iCount = 0
     DEALLOCATE (cDataList , cFileList , lBudgetType , STAT=ErrorCode)
     
     SELECT CASE (iLocationType)
         CASE (iLocationType_Subregion)
             !Land and water use budget
             IF (RootZone%Flags%LWUseBudRawFile_Defined) THEN
                 CALL RootZone%LWUseBudRawFile%GetFileName(cFileName)
                 iCount                    = iCount + 1
                 cDataList_Local(iCount)   = cDataList_AtSubregion(iLWU_AtSubregion)
                 cFileList_Local(iCount)   = cFileName
                 lBudgetType_Local(iCount) = .TRUE.
             END IF
             
             !Root zone budget
             IF (RootZone%Flags%RootZoneBudRawFile_Defined) THEN
                 CALL RootZone%RootZoneBudRawFile%GetFileName(cFileName)
                 iCount                    = iCount + 1
                 cDataList_Local(iCount)   = cDataList_AtSubregion(iRootZone_AtSubregion)
                 cFileList_Local(iCount)   = cFileName
                 lBudgetType_Local(iCount) = .TRUE.
             END IF
                          
             
         CASE (iLocationType_Zone)
             !Land and water use zone budget
             IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                 CALL RootZone%LWUZoneBudRawFile%GetFileName(cFileName)
                 iCount                    = iCount + 1
                 cDataList_Local(iCount)   = cDataList_AtZone(iLWU_AtZone)
                 cFileList_Local(iCount)   = cFileName
                 lBudgetType_Local(iCount) = .TRUE.
             END IF
             
             !Root zone zone budget
             IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                 CALL RootZone%RootZoneZoneBudRawFile%GetFileName(cFileName)
                 iCount                    = iCount + 1
                 cDataList_Local(iCount)   = cDataList_AtZone(iRootZone_AtZone)
                 cFileList_Local(iCount)   = cFileName
                 lBudgetType_Local(iCount) = .TRUE.
             END IF
             
    END SELECT
     
    !Store data in return variables
    ALLOCATE (cDataList(iCount) , cFileList(iCount) , lBudgetType(iCount))
    cDataList   = ''
    cDataList   = cDataList_Local(1:iCount)
    cFileList   = ''
    cFileList   = cFileList_Local(1:iCount)
    lBudgetType = lBudgetType_Local(1:iCount)
    
  END SUBROUTINE RootZone_v50_GetDataList_AtLocationType


  ! -------------------------------------------------------------
  ! --- GET THE LIST OF LOCATIONS THAT HAVE A DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetLocationsWithData(RootZone,iLocationType,cDataType,iLocations)
     CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                  :: iLocationType
     CHARACTER(LEN=*),INTENT(IN)         :: cDataType
     INTEGER,ALLOCATABLE,INTENT(OUT)     :: iLocations(:)
     
     SELECT CASE (iLocationType)
         CASE (iLocationType_Subregion)
             !Land and water use budget
             IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtSubregion(iLWU_AtSubregion))) THEN
                 IF (RootZone%Flags%LWUseBudRawFile_Defined) THEN
                    ALLOCATE (iLocations(1))
                    iLocations = iAllLocationIDsListed
                 END IF
                 
             !Root zone budget
             ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtSubregion(iRootZone_AtSubregion))) THEN
                 IF (RootZone%Flags%RootZoneBudRawFile_Defined) THEN
                    ALLOCATE (iLocations(1))
                    iLocations = iAllLocationIDsListed
                 END IF
             END IF
                          
             
         CASE (iLocationType_Zone)
             !Land and water use zone budget
             IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iLWU_AtZone))) THEN
                 IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                    ALLOCATE (iLocations(1))
                    iLocations = iAllLocationIDsListed
                 END IF
                 
             !Root zone zone budget
             ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iRootZone_AtZone))) THEN
                 IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                    ALLOCATE (iLocations(1))
                    iLocations = iAllLocationIDsListed
                 END IF
             END IF
             
    END SELECT
     
  END SUBROUTINE RootZone_v50_GetLocationsWithData


  ! -------------------------------------------------------------
  ! --- GET LIST OF SUB-DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetSubDataList_AtLocation(RootZone,iLocationType,cDataType,cSubDataList)
    CLASS(RootZone_v50_Type),INTENT(IN)      :: RootZone
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
     
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        !Sub-data for data at subregion
        CASE (iLocationType_Subregion)
             !Land and water use budget
             IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtSubregion(iLWU_AtSubregion))) THEN
                 IF (RootZone%Flags%LWUseBudRawFile_Defined) THEN
                    ALLOCATE (cSubDataList(NLWUseBudColumns))
                    cSubDataList = cLWUseBudgetColumnTitles
                 END IF
                 
             !Root zone budget
             ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtSubregion(iRootZone_AtSubregion))) THEN
                 IF (RootZone%Flags%RootZoneBudRawFile_Defined) THEN
                    ALLOCATE (cSubDataList(NRootZoneBudColumns))
                    cSubDataList = cRootZoneBudgetColumnTitles
                 END IF
             END IF
             
        !Sub-data for data at zone
        CASE (iLocationType_Zone)
             !Land and water use budget
             IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iLWU_AtZone))) THEN
                 IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                    ALLOCATE (cSubDataList(NLWUseBudColumns))
                    cSubDataList = cLWUseBudgetColumnTitles
                 END IF
                 
             !Root zone budget
             ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iRootZone_AtZone))) THEN
                 IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                    ALLOCATE (cSubDataList(NRootZoneBudColumns))
                    cSubDataList = cRootZoneBudgetColumnTitles
                 END IF
             END IF
    END SELECT
     
  END SUBROUTINE RootZone_v50_GetSubDataList_AtLocation


  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetModelData_AtLocation(RootZone,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(RootZone_v50_Type)    :: RootZone
    INTEGER,INTENT(IN)          :: iZExtent,iElems(:),iLayers(:),iZones(:),iZonesWithNames(:),iLocationType,iLocationID,iCol
    CHARACTER(LEN=*),INTENT(IN) :: cZoneNames(:),cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)          :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)         :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER            :: iReadCols(1),iDataUnitTypeArray(1)
    REAL(8)            :: rValues(2,SIZE(rOutputDates))
    TYPE(ZoneListType) :: ZoneList
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    !Proceed based on location type
    SELECT CASE (iLocationType)
        CASE (iLocationType_Subregion)
            !Land and water use budget
            IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtSubregion(iLWU_AtSubregion))) THEN
                IF (RootZone%Flags%LWUseBudRawFile_Defined) THEN
                    CALL RootZone%LWUseBudRawFile%ReadData(iLocationID,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                END IF
                
            !Root zone budget
            ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtSubregion(iRootZone_AtSubregion))) THEN
                IF (RootZone%Flags%RootZoneBudRawFile_Defined) THEN
                    CALL RootZone%RootZoneBudRawFile%ReadData(iLocationID,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                END IF
            END IF    
           
           
        CASE (iLocationType_Zone)
            iReadCols = iCol
            
            !Land and water use zone budget
            IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iLWU_AtZone))) THEN
                IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                    !Generate zone list
                    CALL ZoneList%New(RootZone%LWUZoneBudRawFile%Header%iNData,RootZone%LWUZoneBudRawFile%Header%lFaceFlows_Defined,RootZone%LWUZoneBudRawFile%SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Read data
                    CALL RootZone%LWUZoneBudRawFile%ReadData(ZoneList,iLocationID,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_AR,rFact_VL,iDataUnitTypeArray,nActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Populate return variables
                    rOutputDates(1:nActualOutput)  = rValues(1,1:nActualOutput)
                    rOutputValues(1:nActualOutput) = rValues(2,1:nActualOutput)
                    iDataUnitType                  = iDataUnitTypeArray(1)
                END IF
                
            !Root zone zone budget
            ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iRootZone_AtZone))) THEN
                IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                    !Generate zone list
                    CALL ZoneList%New(RootZone%LWUZoneBudRawFile%Header%iNData,RootZone%LWUZoneBudRawFile%Header%lFaceFlows_Defined,RootZone%LWUZoneBudRawFile%SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Read data
                    CALL RootZone%RootZoneZoneBudRawFile%ReadData(ZoneList,iLocationID,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_AR,rFact_VL,iDataUnitTypeArray,nActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Populate return variables
                    rOutputDates(1:nActualOutput)  = rValues(1,1:nActualOutput)
                    rOutputValues(1:nActualOutput) = rValues(2,1:nActualOutput)
                    iDataUnitType                  = iDataUnitTypeArray(1)
                END IF
            END IF    
    END SELECT

  END SUBROUTINE RootZone_v50_GetModelData_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET RATIO OF DESTINATION SUPPLIES TO REGIONAL SUPLLIES FOR AG 
  ! -------------------------------------------------------------
  PURE SUBROUTINE RootZone_v50_GetRatio_DestSupplyToRegionSupply_Ag(RootZone,Ratio)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone  !Not used in this version
    REAL(8),INTENT(OUT)                 :: Ratio(:)
    
    Ratio = 1.0
    
  END SUBROUTINE RootZone_v50_GetRatio_DestSupplyToRegionSupply_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET RATIO OF DESTINATION SUPPLIES TO REGIONAL SUPPLIES FOR URBAN 
  ! -------------------------------------------------------------
  PURE SUBROUTINE RootZone_v50_GetRatio_DestSupplyToRegionSupply_Urb(RootZone,Ratio)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone  !Not used in this version
    REAL(8),INTENT(OUT)                 :: Ratio(:)
    
    Ratio = 1.0
    
  END SUBROUTINE RootZone_v50_GetRatio_DestSupplyToRegionSupply_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL AG AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetSubregionAgAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid   !Not used in this version
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    !Return subregional ag areas
    IF (RootZone%Flags%lAg_Defined) THEN
        Areas(1:AppGrid%NSubregions) = RootZone%AgRootZone%SubregionalArea
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetSubregionAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetSubregionUrbanAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridTYpe),INTENT(IN)        :: AppGrid     !Not used in this version
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    !Return subregional urabn areas
    IF (RootZone%Flags%lUrban_Defined) THEN
        Areas(1:AppGrid%NSubregions) = RootZone%UrbanRootZone%SubregionalArea
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetSubregionUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetSubregionNativeVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas(1:AppGrid%NSubregions) = RootZone%NVRVRootZone%SubregionalArea_NV
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    ELSE
        Areas = 0.0
        RETURN
    END IF
    
  END SUBROUTINE RootZone_v50_GetSubregionNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RIPARIAN VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetSubregionRiparianVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas(1:AppGrid%NSubregions) = RootZone%NVRVRootZone%SubregionalArea_RV
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    ELSE
        Areas = 0.0
        RETURN
    END IF
    
  END SUBROUTINE RootZone_v50_GetSubregionRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL AG AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetElementAgAreas(RootZone,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    !Return elemental ag areas
    IF (RootZone%Flags%lAg_Defined) THEN
        Areas = RootZone%AgRootZone%ElementalArea
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetElementAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetElementUrbanAreas(RootZone,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    !Return elemental urabn areas
    IF (RootZone%Flags%lUrban_Defined) THEN
        Areas = RootZone%UrbanRootZone%ElementalArea
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetElementUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetElementNativeVegAreas(RootZone,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas = RootZone%NVRVRootZone%ElementalArea_NV
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetElementNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL RIPARIAN VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetElementRiparianVegAreas(RootZone,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas = RootZone%NVRVRootZone%ElementalArea_RV
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetElementRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET AG AREAS AT DEMAND LOCATION (SUBREGIONS)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetDemandAgAreas(RootZone,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8),ALLOCATABLE                 :: Areas(:)
    
    !Initialize
    IF (.NOT. ALLOCATED(Areas)) ALLOCATE(Areas(SIZE(RootZone%SubregionSoilsData,DIM=2)))
    
    !Return subregional ag areas
    IF (RootZone%Flags%lAg_Defined) THEN
        Areas = RootZone%AgRootZone%SubregionalArea
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetDemandAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN AREAS DEMAND LOCATION (SUBREGIONS)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetDemandUrbanAreas(RootZone,Areas)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8),ALLOCATABLE                 :: Areas(:)
    
    !Initialize
    IF (.NOT. ALLOCATED(Areas)) ALLOCATE(Areas(SIZE(RootZone%SubregionSoilsData,DIM=2)))
    
    !Return subregional urban areas
    IF (RootZone%Flags%lUrban_Defined) THEN
        Areas = RootZone%UrbanRootZone%SubregionalArea
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetDemandUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AG CROPS
  ! -------------------------------------------------------------
  PURE FUNCTION RootZone_v50_GetNAgCrops(RootZone) RESULT(NAgCrops)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER                             :: NAgCrops
    
    NAgCrops = RootZone%AgRootZone%NCrops
    
  END FUNCTION RootZone_v50_GetNAgCrops
  
  
  ! -------------------------------------------------------------
  ! --- GET PRECIP INFILTRATION AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetElementPrecipInfilt(RootZone,ElemRegion,PrecipInfilt)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: ElemRegion(:)
    REAL(8)                             :: PrecipInfilt(:)
    
    !Local variables
    INTEGER :: indxElem,iRegion,iSoil
    
    DO indxElem=1,SIZE(ElemRegion)
        iRegion = ElemRegion(indxElem)
        iSoil   = RootZone%ElemSoilType(indxElem)
        
        !From ag
        IF (RootZone%Flags%lAg_Defined) THEN
            PrecipInfilt(indxElem) = RootZone%AgRootZone%AgData(iSoil,iRegion)%PrecipInfilt * RootZone%AgRootZone%ElementalArea(indxElem)
        ELSE
            PrecipInfilt(indxElem) = 0.0
        END IF
    
        !From urban
        IF (RootZone%Flags%lUrban_Defined) &
            PrecipInfilt(indxElem) = PrecipInfilt(indxElem) + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%PrecipInfilt * RootZone%UrbanRootZone%ElementalArea(indxElem) * RootZone%UrbanRootZone%PerviousFrac(iRegion)
    
        !From native and riparian veg
        IF (RootZone%Flags%lNVRV_Defined) &
            PrecipInfilt(indxElem) = PrecipInfilt(indxElem) + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%PrecipInfilt   * RootZone%NVRVRootZone%ElementalArea_NV(indxElem)    &
                                                            + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%PrecipInfilt * RootZone%NVRVRootZone%ElementalArea_RV(indxElem)
    END DO
    
  END SUBROUTINE RootZone_v50_GetElementPrecipInfilt
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL ET AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetElementActualET(RootZone,ElemRegion,ET)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: ElemRegion(:)
    REAL(8)                             :: ET(:)
    
    !Local variables
    INTEGER :: indxElem,iRegion,iSoil
    
    DO indxElem=1,SIZE(ElemRegion)
        iRegion = ElemRegion(indxElem)
        iSoil   = RootZone%ElemSoilType(indxElem)
        
        !From ag
        IF (RootZone%Flags%lAg_Defined) THEN
            ET(indxElem) = RootZone%AgRootZone%AgData(iSoil,iRegion)%ETa * RootZone%AgRootZone%ElementalArea(indxElem)
        ELSE
            ET(indxElem) = 0.0
        END IF
    
        !From urban
        IF (RootZone%Flags%lUrban_Defined) &
            ET(indxElem) = ET(indxElem) + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%ETa * RootZone%UrbanRootZone%ElementalArea(indxElem) 
    
        !From native and riparian veg
        IF (RootZone%Flags%lNVRV_Defined) &
            ET(indxElem) = ET(indxElem) + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%ETa   * RootZone%NVRVRootZone%ElementalArea_NV(indxElem)    &
                                        + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%ETa * RootZone%NVRVRootZone%ElementalArea_RV(indxElem)
    END DO
    
  END SUBROUTINE RootZone_v50_GetElementActualET
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DEMAND CALCULATION LOCATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION RootZone_v50_GetNDemandLocations(RootZone) RESULT(NLocations)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER                             :: NLocations
    
    NLocations = SIZE(RootZone%WaterSupply)
    
  END FUNCTION RootZone_v50_GetNDemandLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL AG WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetWaterDemand_Ag(RootZone,AgDemand)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8)                             :: AgDemand(:)
    
    IF (RootZone%Flags%lAg_Defined) THEN
        CALL EchoProgress('Retrieving subregional agricultural water demand ... ',lAdvance=.FALSE.)
        AgDemand = RootZone%AgRootZone%SubregionalDemand
        CALL EchoProgress('DONE')
    ELSE
        AgDemand = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetWaterDemand_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL URBAN WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetWaterDemand_Urb(RootZone,UrbDemand)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8)                             :: UrbDemand(:)
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL EchoProgress('Retrieving subregional urban water demand ... ',lAdvance=.FALSE.)
        UrbDemand = RootZone%UrbanRootZone%Demand
        CALL EchoProgress('DONE')
    ELSE
        UrbDemand = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetWaterDemand_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL AG WATER SUPPLY
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetWaterSupply_Ag(RootZone,AppGrid,Supply)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid    
    REAL(8)                             :: Supply(:)
    
    IF (RootZone%Flags%lAg_Defined) THEN
        CALL EchoProgress('Retrieving subregional agricultural water supplies ... ',lAdvance=.FALSE.)
        Supply = RootZone%WaterSupply%Diversion_Ag + RootZone%WaterSupply%Pumping_Ag + UpstrmRunoffToLandUse(RootZone,AppGrid,iLandUse_Ag)
        CALL EchoProgress('DONE')
    ELSE
        Supply = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetWaterSupply_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL URBAN WATER SUPPLY
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetWaterSupply_Urb(RootZone,AppGrid,Supply)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid    
    REAL(8)                             :: Supply(:)
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL EchoProgress('Retrieving subregional urban water supplies ... ',lAdvance=.FALSE.)
        Supply = RootZone%WaterSupply%Diversion_Urb + RootZone%WaterSupply%Pumping_Urb + UpstrmRunoffToLandUse(RootZone,AppGrid,iLandUse_Urban)
        CALL EchoProgress('DONE')
    ELSE
        Supply = 0.0
    END IF

  END SUBROUTINE RootZone_v50_GetWaterSupply_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER 
  ! -------------------------------------------------------------
  FUNCTION RootZone_v50_GetVersion(RootZone) RESULT(cVrs)
    CLASS(RootZone_v50_Type) :: RootZone
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    IF (.NOT. RootZone%Version%IsDefined())   &
        RootZone%Version = RootZone%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = RootZone%Version%GetVersion()
    
  END FUNCTION RootZone_v50_GetVersion
  

  ! -------------------------------------------------------------
  ! ---GET ELEMENTAL VOLUMETRIC SOIL MOISTURE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetElementSoilMVolume(RootZone,AppGrid,SoilM)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: SoilM(:)
    
    !Local variables
    INTEGER :: indxElem,iSoil,iRegion
    
    !Soil moisture from ag lands
    IF (RootZone%Flags%lAg_Defined) THEN
        SoilM = 0.0
        DO indxElem=1,AppGrid%NElements
            iSoil           = RootZone%ElemSoilType(indxElem)
            iRegion         = AppGrid%AppElement(indxElem)%Subregion
            SoilM(indxElem) = SoilM(indxElem) + ( RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_Precip  &
                                                + RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_AW      &
                                                + RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_Oth   ) * RootZone%AgRootZone%ElementalArea(indxElem)  
        END DO
    ELSE
        SoilM = 0.0
    END IF
    
    !Soil moisture from urban lands
    IF (RootZone%Flags%lUrban_Defined) THEN
        DO indxElem=1,AppGrid%NElements
            iSoil           = RootZone%ElemSoilType(indxElem)
            iRegion         = AppGrid%AppElement(indxElem)%Subregion
            SoilM(indxElem) = SoilM(indxElem) + ( RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_Precip  &
                                                + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_AW      &
                                                + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_Oth   ) * RootZone%UrbanRootZone%ElementalArea(indxElem) * RootZone%UrbanRootZone%PerviousFrac(iRegion)
        END DO
    END IF

    !Soil moisture from native and riparian veg
    IF (RootZone%Flags%lNVRV_Defined) THEN
        DO indxElem=1,AppGrid%NElements
            iSoil           = RootZone%ElemSoilType(indxElem)
            iRegion         = AppGrid%AppElement(indxElem)%Subregion
            SoilM(indxElem) = SoilM(indxElem) + ( RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_Precip    &
                                                + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_AW        &
                                                + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_Oth     ) * RootZone%NVRVRootZone%ElementalArea_NV(indxElem)  &
                                              + ( RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_Precip  &
                                                + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_AW      &
                                                + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_Oth   ) * RootZone%NVRVRootZone%ElementalArea_RV(indxElem)
        END DO
    END IF

  END SUBROUTINE RootZone_v50_GetElementSoilMVolume
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT ALL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION RootZone_v50_GetPercAll(RootZone,AppGrid) RESULT(Perc)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: Perc(AppGrid%NElements)

    !Local variables
    INTEGER :: indxElem,iRegion,iSoil,indx,iElem
    
    !Initialize
    Perc = 0.0
    
    ASSOCIATE (pFlags       => RootZone%Flags                  , &
               pElemSoil    => RootZone%ElemSoilType           , &
               pAg          => RootZone%AgRootZone             , &
               pUrban       => RootZone%UrbanRootZone          , &
               pNVRV        => RootZone%NVRVRootZone           , &
               pUrbElemToGW => RootZone%UrbanRootZone%ElemToGW )
        
        DO indxElem=1,AppGrid%NElements
            iRegion = AppGrid%AppElement(indxElem)%Subregion
            iSoil   = pElemSoil(indxElem)
   
            !Ag lands
            IF (pFlags%lAg_Defined) Perc(indxElem) = (pAg%AgData(iSoil,iRegion)%Perc + pAg%AgData(iSoil,iRegion)%PercCh) * pAg%ElementalArea(indxElem)

            !Urban 
            IF (pFlags%lUrban_Defined) Perc(indxElem) = Perc(indxElem) + (pUrban%UrbData(iSoil,iRegion)%Perc + pUrban%UrbData(iSoil,iRegion)%PercCh) * pUrban%ElementalArea(indxElem)

            !Native and riparian vegetation areas
            IF (pFlags%lNVRV_Defined) Perc(indxElem) = Perc(indxElem) + (pNVRV%NativeVeg(iSoil,iRegion)%Perc   + pNVRV%NativeVeg(iSoil,iRegion)%PercCh) * pNVRV%ElementalArea_NV(indxElem)   &
                                                                      + (pNVRV%RiparianVeg(iSoil,iRegion)%Perc + pNVRV%RiparianVeg(iSoil,iRegion)%PercCh) * pNVRV%ElementalArea_RV(indxElem) 

        END DO
        
        !Include urban surface runoff in perc if it goes to groundwater
        IF (pFlags%lUrban_Defined) THEN
            DO indx=1,SIZE(pUrbElemToGW)
                iElem       = pUrbElemToGW(indx)
                IF (pUrban%ElementalArea(iElem) .EQ. 0.0) CYCLE
                iRegion     = AppGrid%AppElement(iElem)%Subregion
                iSoil       = pElemSoil(iElem)
                Perc(iElem) = Perc(iElem) + (pUrban%UrbData(iSoil,iRegion)%Runoff + pUrban%UrbData(iSoil,iRegion)%ReturnFlow) * pUrban%ElementalArea(iElem)
            END DO
        END IF
        
    END ASSOCIATE
               
  END FUNCTION RootZone_v50_GetPercAll
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT AN INDIVIDUAL ELEMENT
  ! -------------------------------------------------------------
  FUNCTION RootZone_v50_GetPercElement(RootZone,iElem,AppGrid) RESULT(Perc)
    CLASS(RootZone_v50_Type),INTENT(IN)   :: RootZone
    INTEGER,INTENT(IN)                    :: iElem
    TYPE(AppGridType),OPTIONAL,INTENT(IN) :: AppGrid
    REAL(8)                               :: Perc
    
    !local variables
    INTEGER :: iSoil,iRegion
    
    !Initialize
    Perc = 0.0
    
    iRegion = AppGrid%AppElement(iElem)%Subregion
    iSoil   = RootZone%ElemSoilType(iElem)
   
    !Ag lands
    IF (RootZone%Flags%lAg_Defined) Perc = (RootZone%AgRootZone%AgData(iSoil,iRegion)%Perc + RootZone%AgRootZone%AgData(iSoil,iRegion)%PercCh) * RootZone%AgRootZone%ElementalArea(iElem)

    !Urban
    IF (RootZone%Flags%lUrban_Defined) Perc = Perc+ (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Perc + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%PercCh) * RootZone%UrbanRootZone%ElementalArea(iElem)

    !Native and riparian vegetation areas
    IF (RootZone%Flags%lNVRV_Defined) Perc = Perc + (RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%Perc   + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%PercCh) * RootZone%NVRVRootZone%ElementalArea_NV(iElem)   &
                                                  + (RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%Perc + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%PercCh) * RootZone%NVRVRootZone%ElementalArea_RV(iElem) 
    
    !Include urban surface runoff in perc if it goes to groundwater
    IF (RootZone%Flags%lUrban_Defined) THEN
        IF (RootZone%UrbanRootZone%ElementalArea(iElem) .EQ. 0.0) RETURN
        IF (LocateInList(iElem,RootZone%UrbanRootZone%ElemToGW) .GT. 0) THEN
            iRegion  = AppGrid%AppElement(iElem)%Subregion
            iSoil    = RootZone%ElemSoilType(iElem)
            Perc     = Perc + (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Runoff + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%ReturnFlow) * RootZone%UrbanRootZone%ElementalArea(iElem)
        END IF
    END IF

  END FUNCTION RootZone_v50_GetPercElement
  
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetFlowsToStreams(RootZone,AppGrid,DirectRunoff,ReturnFlow,RiparianET)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: DirectRunoff(:),ReturnFlow(:)
    REAL(8),INTENT(INOUT)               :: RiparianET(:)                 !Included only to be consistent with the BaseRootZone template! This process is not simulated in this version.
    
    !Local variables
    INTEGER :: indx,iStrmNode,iElem,iSoil,iRegion
    LOGICAL :: lAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    DirectRunoff   = 0.0
    ReturnFlow     = 0.0
    lAg_Defined    = RootZone%Flags%lAg_Defined
    lUrban_Defined = RootZone%Flags%lUrban_Defined
    lNVRV_Defined  = RootZone%Flags%lNVRV_Defined
        
    !Flows into streams
    ASSOCIATE (pFlowData  => RootZone%ElemFlowToStreams              , &
               pSoilType  => RootZone%ElemSoilType                   , &
               pAg        => RootZone%AgRootZone%AgData              , &
               pAgArea    => RootZone%AgRootZone%ElementalArea       , &
               pUrban     => RootZone%UrbanRootZone%UrbData          , &
               pUrbanArea => RootZone%UrbanRootZone%ElementalArea    , &
               pNV        => RootZone%NVRVRootZone%NativeVeg         , &
               pRV        => RootZone%NVRVRootZone%RiparianVeg       , &
               pNVArea    => RootZone%NVRVRootZone%ElementalArea_NV  , &
               pRVArea    => RootZone%NVRVRootZone%ElementalArea_RV  )
    
      DO indx=1,SIZE(pFlowData)
        !Element ID
        iElem = pFlowData(indx)%iElement
        
        !Soil type and subregion number fo the element
        iSoil   = pSoilType(iElem)
        iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Destination stream node
        iStrmNode = pFlowData(indx)%iDestID
        
        !Flows from ag lands
        IF (lAg_Defined) THEN
            DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + pAg(iSoil,iRegion)%Runoff * pAgArea(iElem)
            ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + pAg(iSoil,iRegion)%ReturnFlow * pAgArea(iElem)
        END IF
        
        !Flows from urban lands
        IF (lUrban_Defined) THEN
            DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + pUrban(iSoil,iRegion)%Runoff * pUrbanArea(iElem)
            ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + pUrban(iSoil,iRegion)%ReturnFlow * pUrbanArea(iElem)
        END IF
        
        !Flows from native/riparian veg
        IF (lNVRV_Defined) &
            DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + pNV(iSoil,iRegion)%Runoff * pNVArea(iElem) &
                                                              + pRV(iSoil,iRegion)%Runoff * pRVArea(iElem)

      END DO
      
    END ASSOCIATE
    
  END SUBROUTINE RootZone_v50_GetFlowsToStreams
    
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO LAKES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_GetFlowsToLakes(RootZone,AppGrid,DirectRunoff,ReturnFlow)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: DirectRunoff(:),ReturnFlow(:)
    
    !Local variables
    INTEGER :: iElem,iLake,indx,iSoil,iRegion
    LOGICAL :: lAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    DirectRunoff      = 0.0
    ReturnFlow        = 0.0
    lAg_Defined       = RootZone%Flags%lAg_Defined
    lUrban_Defined    = RootZone%Flags%lUrban_Defined
    lNVRV_Defined     = RootZone%Flags%lNVRV_Defined
    
    
    !Flows into streams
    ASSOCIATE (pFlowData  => RootZone%ElemFlowToLakes                , &
               pSoilType  => RootZone%ElemSoilType                   , &
               pAg        => RootZone%AgRootZone%AgData              , &
               pAgArea    => RootZone%AgRootZone%ElementalArea       , &
               pUrban     => RootZone%UrbanRootZone%UrbData          , &
               pUrbanArea => RootZone%UrbanRootZone%ElementalArea    , &
               pNV        => RootZone%NVRVRootZone%NativeVeg         , &
               pRV        => RootZone%NVRVRootZone%RiparianVeg       , &
               pNVArea    => RootZone%NVRVRootZone%ElementalArea_NV  , &
               pRVArea    => RootZone%NVRVRootZone%ElementalArea_RV  )
    
      DO indx=1,SIZE(pFlowData)
        !Element ID
        iElem = pFlowData(indx)%iElement
        
        !Soil type and subregion number fo the element
        iSoil   = pSoilType(iElem)
        iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Destination lake ID
        iLake = pFlowData(indx)%iDestID
        
        !Flows from ag lands
        IF (lAg_Defined) THEN
          DirectRunoff(iLake) = DirectRunoff(iLake) + pAg(iSoil,iRegion)%Runoff * pAgArea(iElem)
          ReturnFlow(iLake)   = ReturnFlow(iLake)   + pAg(iSoil,iRegion)%ReturnFlow * pAgArea(iElem)
        END IF
        
        !Flows from urban lands
        IF (lUrban_Defined) THEN
            DirectRunoff(iLake) = DirectRunoff(iLake) + pUrban(iSoil,iRegion)%Runoff * pUrbanArea(iElem)
            ReturnFlow(iLake)   = ReturnFlow(iLake)   + pUrban(iSoil,iRegion)%ReturnFlow * pUrbanArea(iElem)
        END IF
        
        !Flows from native/riparian veg
        IF (lNVRV_Defined) &
            DirectRunoff(iLake) = DirectRunoff(iLake) + pNV(iSoil,iRegion)%Runoff * pNVArea(iElem) &
                                                      + pRV(iSoil,iRegion)%Runoff * pRVArea(iElem)

      END DO
      
    END ASSOCIATE
        
  END SUBROUTINE RootZone_v50_GetFlowsToLakes

  
  
  
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
  ! --- SET SUPPLY TO SUBREGIONS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_SetSupplyToSubregion(RootZone,Supply,SupplyType)
    CLASS(RootZone_v50_Type) :: RootZone
    REAL(8),INTENT(IN)       :: Supply(:)
    INTEGER,INTENT(IN)       :: SupplyType

    !Inform user
    CALL EchoProgress('Setting supply to subregions ... ',lAdvance=.FALSE.)
    
    !Set supply
    SELECT CASE(SupplyType)
        CASE (Supply_Diversion_Ag)
            RootZone%WaterSupply%Diversion_Ag = Supply
        
        CASE (Supply_Diversion_Urb)
            RootZone%WaterSupply%Diversion_Urb = Supply

        CASE (Supply_Pumping_Ag)
            RootZone%WaterSupply%Pumping_Ag = Supply

        CASE (Supply_Pumping_Urb)
            RootZone%WaterSupply%Pumping_Urb = Supply

        CASE (Supply_UpstrmElemRunoff)
            RootZone%WaterSupply%UpstrmRunoff = Supply

    END SELECT
      
    CALL EchoProgress('DONE')
   
  END SUBROUTINE RootZone_v50_SetSupplyToSubregion

  
! -------------------------------------------------------------
  ! --- SET THE LAKE ELEMENT FLAG
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_SetLakeElemFlag(RootZone,iLakeElem)
    CLASS(RootZone_v50_Type) :: RootZone
    INTEGER,INTENT(IN)       :: iLakeElem(:)
    
    RootZone%Flags%lLakeElems(iLakeElem)            = .TRUE.    
    RootZone%ElemPrecipData(iLakeElem)%PrecipFactor = 0.0
    
  END SUBROUTINE RootZone_v50_SetLakeElemFlag

  
  
  
  
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
  SUBROUTINE RootZone_v50_ReadRestartData(RootZone,InFile,iStat)
    CLASS(RootZone_v50_Type) :: RootZone
    TYPE(GenericFileType)    :: InFile
    INTEGER,INTENT(OUT)      :: iStat
    
    CALL InFile%ReadData(RootZone%RSoilM_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(RootZone%RSoilM,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    IF (RootZone%Flags%lAg_Defined) THEN
        CALL RootZone%AgRootZone%ReadRestartData(InFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%UrbanRootZone%ReadRestartData(InFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
        
    IF (RootZone%Flags%lNVRV_Defined) CALL RootZone%NVRVRootZone%ReadRestartData(InFile,iStat)  
    
  END SUBROUTINE RootZone_v50_ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ ROOT ZONE RELATED TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_ReadTSData(RootZone,AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas)
    CLASS(RootZone_v50_Type),TARGET    :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ETData
    INTEGER,INTENT(OUT)                :: iStat
    REAL(8),OPTIONAL,INTENT(IN)        :: RegionLUAreas(:,:)   !Subregional land use areas to overwrite the data read from the file
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24)      :: ThisProcedure = ModName // 'RootZone_v50_ReadTSData'
    INTEGER                           :: indxElem,NElements,iRegion,iSoil,indxRegion,indxSoil,NSubregions
    REAL(8)                           :: Area,LUArea(AppGrid%NSubregions)
    LOGICAL                           :: lReturnFracUpdated,lReuseFracUpdated
    CLASS(GenericLandUseType),POINTER :: pLandUse
    
    !Initialize
    iStat       = 0
    NElements   = AppGrid%NElements
    NSubregions = AppGrid%NSubregions

    !Read return flow fractions
    CALL ReadReturnFlowFractions(TimeStep,RootZone%ReturnFracFile,lReturnFracUpdated,iStat)
    IF (iStat .EQ. -1) RETURN

    !Read re-use fractions
    CALL ReadReuseFractions(TimeStep,RootZone%ReuseFracFile,lReuseFracUpdated,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read generic moisture data
    IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN
        CALL RootZone%GenericMoistureData%ReadTSData(TimeStep,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
      
    !Agricultural land use data
    IF (RootZone%Flags%lAg_Defined) THEN
        IF (PRESENT(RegionLUAreas)) THEN
            CALL RootZone%AgRootZone%ReadTSData(RootZone%NSoils                               , &
                                                RootZone%ElemSoilType                         , &
                                                RootZone%SoilRegionArea                       , &
                                                RootZone%SubregionSoilsData%WiltingPoint      , &
                                                RootZone%SubregionSoilsData%FieldCapacity     , &
                                                RootZone%Flags%lLakeElems                     , &
                                                ETData                                        , &
                                                TimeStep                                      , &
                                                AppGrid                                       , &
                                                RegionLUAreas(1:RootZone%AgRootZone%NCrops,:) , &
                                                iStat                                         )
        ELSE
            CALL RootZone%AgRootZone%ReadTSData(RootZone%NSoils                               , &
                                                RootZone%ElemSoilType                         , &
                                                RootZone%SoilRegionArea                       , &
                                                RootZone%SubregionSoilsData%WiltingPoint      , &
                                                RootZone%SubregionSoilsData%FieldCapacity     , &
                                                RootZone%Flags%lLakeElems                     , &
                                                ETData                                        , &
                                                TimeStep                                      , &
                                                AppGrid                                       , &
                                                iStat=iStat                                   )
        END IF
        IF (iStat .EQ. -1) RETURN
    END IF

    !Urban lands related data
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%UrbanRootZone%ReadTSData(RootZone%ElemSoilType                         , &
                                               RootZone%Flags%lLakeElems                     , &
                                               TimeStep                                      , &
                                               AppGrid                                       , &
                                               iStat                                         )
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Native and riparian veg data
    IF (RootZone%Flags%lNVRV_Defined) THEN
        CALL RootZone%NVRVRootZone%ReadTSData(RootZone%ElemSoilType     , &
                                              RootZone%Flags%lLakeElems , &
                                              TimeStep                  , &
                                              AppGrid                   , &
                                              iStat                     )
        IF (iStat .EQ. -1) RETURN
    END IF

    !Process land use areas
    CALL ProcessLandUseAreas(AppGrid,TimeStep,RootZone,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compute precipitation over each element and (soil,subregion) combination
    IF (Precip%IsUpdated()) THEN
        RootZone%ElemPrecipData%Precip = Precip%GetValues(RootZone%ElemPrecipData%iColPrecip) * RootZone%ElemPrecipData%PrecipFactor
        RootZone%SoilRegionPrecip = 0.0
        DO indxElem=1,NElements
            Area                                     = AppGrid%AppElement(indxElem)%Area
            iRegion                                  = AppGrid%AppElement(indxElem)%Subregion
            iSoil                                    = RootZone%ElemSoilType(indxElem)
            RootZone%SoilRegionPrecip(iSoil,iRegion) = RootZone%SoilRegionPrecip(iSoil,iRegion) + RootZone%ElemPrecipData(indxElem)%Precip * Area
        END DO
        !Convert (soil,region) precipitation to rates
        DO indxRegion=1,AppGrid%NSubregions
            DO indxSoil=1,RootZone%NSoils
                IF (RootZone%SoilRegionArea(indxSoil,indxRegion) .GT. 0.0)   &
                    RootZone%SoilRegionPrecip(indxSoil,indxRegion) = RootZone%SoilRegionPrecip(indxSoil,indxRegion) / RootZone%SoilRegionArea(indxSoil,indxRegion)
            END DO
        END DO
    END IF
    
    !Compute regional potential ET for each land use (athat for ag land is computed in AgRootZone class), if needed
    IF (ETData%IsUpdated()) THEN
        IF (RootZone%Flags%lUrban_Defined) CALL ComputeRegionalETPot(ETData,NSubregions,RootZone%UrbanRootZone%UrbData(1,:)%iColETc,RootZone%UrbanRootZone%SubregionalArea,RootZone%UrbanRootZone%RegionETPot)
        IF (RootZone%Flags%lNVRV_Defined) THEN
            LUArea = SUM(RootZone%NVRVRootZone%NativeVeg%Area , DIM=1) 
            CALL ComputeRegionalETPot(ETData,NSubregions,RootZone%NVRVRootZone%NativeVeg(1,:)%iColETc,LUArea,RootZone%NVRVRootZone%RegionETPot_NV)
            LUArea = SUM(RootZone%NVRVRootZone%RiparianVeg%Area , DIM=1)
            CALL ComputeRegionalETPot(ETData,NSubregions,RootZone%NVRVRootZone%RiparianVeg(1,:)%iColETc,LUArea,RootZone%NVRVRootZone%RegionETPot_RV)
        END IF
    END IF
    
    !Make sure that re-use fraction is not larger than return flow factor
    IF (lReturnFracUpdated .OR. lReuseFracUpdated) THEN
      ASSOCIATE (pReturnFrac => RootZone%ReturnFracFile%rValues          , &
                 pReuseFrac  => RootZone%ReuseFracFile%rValues           , &
                 pAg         => RootZone%AgRootZone                      , &
                 pUrban      => RootZone%UrbanRootZone                   ) 
      
        !Ag lands
        IF (RootZone%Flags%lAg_Defined) THEN
          DO indxRegion=1,NSubregions
              IF (pReturnFrac(pAg%iColReturnFrac(indxRegion)) .LT. pReuseFrac(pAg%iColReuseFrac(indxRegion))) THEN
                  MessageArray(1) = 'Agricultural re-use fraction for subregion ' //TRIM(IntTotext(indxRegion))//' is greater than return flow fraction!'
                  WRITE (MessageArray(2),'(A,F5.3)') 'Re-use fraction      = ',pReuseFrac(pAg%iColReuseFrac(indxRegion))
                  WRITE (MessageArray(3),'(A,F5.3)') 'Return flow fraction = ',pReturnFrac(pAg%iColReturnFrac(indxRegion))
                  CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
          END DO
        END IF
          
        !Urban lands
        IF (RootZone%Flags%lUrban_Defined) THEN
            DO indxRegion=1,NSubregions
                IF (pReturnFrac(pUrban%iColReturnFrac(indxRegion)) .LT. pReuseFrac(pUrban%iColReuseFrac(indxRegion))) THEN
                    MessageArray(1) = 'Urban re-use fraction at subregion '//TRIM(IntToText(indxRegion))//' is greater than return flow fraction!'
                    WRITE (MessageArray(2),'(A,F5.3)') 'Re-use fraction      = ',pReuseFrac(pUrban%iColReuseFrac(indxRegion))
                    WRITE (MessageArray(3),'(A,F5.3)') 'Return flow fraction = ',pReturnFrac(pUrban%iColReturnFrac(indxRegion))
                    CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                  END IF
            END DO
        END IF
        
      END ASSOCIATE
    END IF

    !Don't go beyond this point if it is not the first time step
    IF (TimeStep%CurrentTimeStep .GT. 1) RETURN
    
    !Computations for first time step only
    !Zero out initial soil moisture where area is zero
    DO indxRegion=1,NSubregions
        DO indxSoil=1,RootZone%NSoils
            !Ag lands
            IF (RootZone%Flags%lAg_Defined) THEN
                pLandUse => RootZone%AgRootZone%AgData(indxSoil,indxRegion)
                IF (pLandUse%Area .EQ. 0.0) THEN
                    pLandUse%SoilM_Precip_P = 0.0
                    pLandUse%SoilM_AW_P     = 0.0
                    pLandUse%SoilM_Precip   = 0.0
                    pLandUse%SoilM_AW       = 0.0
                END IF
            END IF
            !Urban lands
            IF (RootZone%Flags%lUrban_Defined) THEN
                pLandUse => RootZone%UrbanRootZone%UrbData(indxSoil,indxRegion)
                IF (pLandUse%Area .EQ. 0.0) THEN
                    pLandUse%SoilM_Precip_P = 0.0
                    pLandUse%SoilM_AW_P     = 0.0
                    pLandUse%SoilM_Precip   = 0.0
                    pLandUse%SoilM_AW       = 0.0
                END IF
            END IF  
            !Native and riparian vegetation
            IF (RootZone%Flags%lNVRV_Defined) THEN
                pLandUse => RootZone%NVRVRootZone%NativeVeg(indxSoil,indxRegion)
                IF (pLandUse%Area .EQ. 0.0) THEN
                    pLandUse%SoilM_Precip_P = 0.0
                    pLandUse%SoilM_Precip   = 0.0
                END IF  
                pLandUse => RootZone%NVRVRootZone%RiparianVeg(indxSoil,indxRegion)
                IF (pLandUse%Area .EQ. 0.0) THEN
                    pLandUse%SoilM_Precip_P = 0.0
                    pLandUse%SoilM_Precip   = 0.0
                END IF   
            END IF
        END DO
    END DO

    !Convert initial soil mositure contents to depths
    CALL RootZone%AgRootZone%SoilMContent_To_Depth(RootZone%NSoils,NSubregions,RootZone%SubregionSoilsData%TotalPorosity,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL RootZone%UrbanRootZone%SoilMContent_To_Depth(RootZone%NSoils,NSubregions,RootZone%SubregionSoilsData%TotalPorosity,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZone%NVRVRootZone%SoilMContent_To_Depth(RootZone%NSoils,NSubregions,RootZone%SubregionSoilsData%TotalPorosity,iStat)   ;  IF (iStat .EQ. -1) RETURN
        
    !Make sure that soil moisture at the end of previous timestep is saved, in case it is updated due to chnage in land use area
    RootZone%AgRootZone%AgData%SoilM_Precip_P_BeforeUpdate        = RootZone%AgRootZone%AgData%SoilM_Precip_P
    RootZone%AgRootZone%AgData%SoilM_AW_P_BeforeUpdate            = RootZone%AgRootZone%AgData%SoilM_AW_P
    RootZone%AgRootZone%AgData%SoilM_Oth_P_BeforeUpdate           = RootZone%AgRootZone%AgData%SoilM_Oth_P
    RootZone%UrbanRootZone%UrbData%SoilM_Precip_P_BeforeUpdate    = RootZone%UrbanRootZone%UrbData%SoilM_Precip_P
    RootZone%UrbanRootZone%UrbData%SoilM_AW_P_BeforeUpdate        = RootZone%UrbanRootZone%UrbData%SoilM_AW_P
    RootZone%UrbanRootZone%UrbData%SoilM_Oth_P_BeforeUpdate       = RootZone%UrbanRootZone%UrbData%SoilM_Oth_P
    RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P_BeforeUpdate   = RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P
    RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P_BeforeUpdate       = RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P
    RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P_BeforeUpdate      = RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P
    RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P_BeforeUpdate = RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P
    RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P_BeforeUpdate     = RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P
    RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P_BeforeUpdate    = RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P
    
    !Initial regional moisture storage
    RootZone%RSoilM_P = RegionalMoistStorage(NSubregions,RootZone)

  END SUBROUTINE RootZone_v50_ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- READ RETURN FLOW FRACTION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadReturnFlowFractions(TimeStep,ReturnFracFile,lReturnFracUpdated,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(RealTSDataInFileType)    :: ReturnFracFile
    LOGICAL,INTENT(OUT)           :: lReturnFracUpdated
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode

    !Inform user about progress
    CALL EchoProgress('Reading return flow fractions time series data')
    
    !Read data
    CALL ReadTSData(TimeStep,'Return flow fractions data',ReturnFracFile,FileReadCode,iStat)

    IF (FileReadCode .EQ. 0) THEN
        lReturnFracUpdated = .TRUE.
    ELSE
        lReturnFracUpdated = .FALSE.
    END IF

  END SUBROUTINE ReadReturnFlowFractions
  
  
  ! -------------------------------------------------------------
  ! --- READ RE-USE FRACTION DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadReuseFractions(TimeStep,ReuseFracFile,lReuseFracUpdated,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(RealTSDataInFileType)    :: ReuseFracFile
    LOGICAL,INTENT(OUT)           :: lReuseFracUpdated
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode

    !Inform user about progress
    CALL EchoProgress('Reading re-use fractions time series data')
    
    !Read data
    CALL ReadTSData(TimeStep,'Applied water re-use fractions data',ReuseFracFile,FileReadCode,iStat)

    IF (FileReadCode .EQ. 0) THEN
        lReuseFracUpdated = .TRUE.
    ELSE
        lReuseFracUpdated = .FALSE.
    END IF

  END SUBROUTINE ReadReuseFractions
  
  


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
  SUBROUTINE RootZone_v50_PrintRestartData(RootZone,OutFile)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(GenericFileType)               :: OutFile
    
    CALL  OutFile%WriteData(RootZone%RSoilM_P)
    CALL  OutFile%WriteData(RootZone%RSoilM)
    
    IF (RootZone%Flags%lAg_Defined)    CALL RootZone%AgRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lUrban_Defined) CALL RootZone%UrbanRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lNVRV_Defined)  CALL RootZone%NVRVRootZone%PrintRestartData(OutFile)
    
  END SUBROUTINE RootZone_v50_PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE TO PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation)
    CLASS(RootZone_v50_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETTYpe),INTENT(IN)       :: ETData          !Not used in this version
    TYPE(TimeStepType),INTENT(IN) :: TimeStep         
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    !Local variables
    INTEGER                                  :: NRegions
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,                                &
                                                RUpstrmRunoff_Ag,RUpstrmRunoff_Urb,RUpstrmRunoff_NV,                  &
                                                RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_Ag,RGenericMoist_Urb, &
                                                RGenericMoist_NV,RDemand_Urb
                                                
    !Echo progress
    CALL EchoProgress('Printing results of root zone simulation')
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    ASSOCIATE (pFlags            => RootZone%Flags                                , &
               prGenericMoisture => RootZone%GenericMoistureData%rGenericMoisture )
      !Compute variables necessary for land&water use budget and land&water use zone budget files
      IF (pFlags%LWUseBudRawFile_Defined .OR. pFlags%LWUseZoneBudRawFile_Defined) THEN
          RDemand_Urb = RegionalDemand(NRegions,RootZone,iLandUse_Urban)
      END IF

      !Compute variables necessary for both land&water use and root zone budget files
      IF (pFlags%LWUseBudRawFile_Defined .OR. pFlags%RootZoneBudRawFile_Defined .OR. pFlags%LWUseZoneBudRawFile_Defined .OR. pFlags%RootZoneZoneBudRawFile_Defined) THEN
        RPump_Ag                      = RegionalPumping(NRegions,RootZone,iLandUse_Ag)
        RPump_Urb                     = RegionalPumping(NRegions,RootZone,iLandUse_Urban)
        RDeli_Ag                      = RegionalDeliveries(NRegions,RootZone,iLandUse_Ag)
        RDeli_Urb                     = RegionalDeliveries(NRegions,RootZone,iLandUse_Urban)
        RUpstrmRunoff_Ag(1:NRegions)  = UpstrmRunoffToLandUse(RootZone,AppGrid,iLandUse_Ag)     ;  RUpstrmRunoff_Ag(NRegions+1) = SUM(RUpstrmRunoff_Ag(1:NRegions))
        RUpstrmRunoff_Urb(1:NRegions) = UpstrmRunoffToLandUse(RootZone,AppGrid,iLandUse_Urban)  ;  RUpstrmRunoff_Urb(NRegions+1) = SUM(RUpstrmRunoff_Urb(1:NRegions))
        RUpstrmRunoff_NV              = UpstrmRunoffToLandUse(RootZone,AppGrid,iLandUse_NVRV)   ;  RUpstrmRunoff_NV(NRegions+1) = SUM(RUpstrmRunoff_NV(1:NRegions))
        RLUArea_Ag                    = RegionalLUArea(NRegions,RootZone,iLandUse_Ag)
        RLUArea_Urb                   = RegionalLUArea(NRegions,RootZone,iLandUse_Urban)
        RLUArea_NV                    = RegionalLUArea(NRegions,RootZone,iLandUse_NVRV)
        IF (pFlags%lGenericMoistureFile_Defined) THEN
            RGenericMoist_Ag  = RegionalGenericMoistInflow(NRegions,RootZone,iLandUse_Ag)
            RGenericMoist_Urb = RegionalGenericMoistInflow(NRegions,RootZone,iLandUse_Urban)
            RGenericMoist_NV  = RegionalGenericMoistInflow(NRegions,RootZone,iLandUse_NVRV)
        ELSE
            RGenericMoist_Ag  = 0.0
            RGenericMoist_Urb = 0.0
            RGenericMoist_NV  = 0.0
        END IF
      END IF
          
      !Land and water use budget file
      IF (pFlags%LWUseBudRawFile_Defined) CALL WriteLWUseFlowsToBudRawFile(NRegions,RLUArea_Ag,RLUArea_Urb,RDemand_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmRunoff_Ag,RUpstrmRunoff_Urb,RootZone)
      
      !Root zone budget file
      IF (pFlags%RootZoneBudRawFile_Defined) CALL WriteRootZoneFlowsToBudRawFile(AppGrid,RPump_Ag,RDeli_Ag,RGenericMoist_Ag,RPump_Urb,RDeli_Urb,RGenericMoist_Urb,RUpstrmRunoff_Ag,RUpstrmRunoff_Urb,RUpstrmRunoff_NV,RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_NV,RootZone)
      
      !Land and water use zone budget
      IF (pFlags%LWUseZoneBudRawFile_Defined) CALL WriteLWUseFlowsToZoneBudRawFile(AppGrid,RDemand_Urb,RootZone)
      
      !Root zone zone budget
      IF (pFlags%RootZoneZoneBudRawFile_Defined) CALL WriteRootZoneFlowsToZoneBudRawFile(AppGrid,RPump_Ag+RDeli_Ag,RPump_Urb+RDeli_Urb,RootZone)
      
      !Ag results print-out
      IF (RootZone%Flags%lAg_Defined) CALL RootZone%AgRootZone%PrintResults(NRegions,TimeStep,lEndOfSimulation)

      !Final moisture print-out
      IF (lEndOfSimulation) THEN
        IF (pFlags%FinalMoistureOutFile_Defined) CALL WriteFinalMoistures(NRegions,RootZone)
      END IF

    END ASSOCIATE
    
  END SUBROUTINE RootZone_v50_PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FINAL MOISTURES
  ! -------------------------------------------------------------
  SUBROUTINE WriteFinalMoistures(NRegions,RootZone)
    INTEGER,INTENT(IN)      :: NRegions
    TYPE(RootZone_v50_Type) :: RootZone
    
    !Local variables
    INTEGER   :: indxRegion,indxSoil
    REAL(8)   :: RootDepth,rValue,rValue1
    CHARACTER :: cOutput*100
    
    ASSOCIATE (pOutFile => RootZone%FinalMoistureOutfile , &
               pFlags   => RootZone%Flags                , &
               pAg      => RootZone%AgRootZone           , &
               pUrban   => RootZone%UrbanRootZone        , &
               pNVRV    => RootZone%NVRVRootZone         )
    
        !Ag moistures
        IF (pFlags%lAg_Defined) THEN
         
            !Titles
            CALL pOutFile%WriteData('C*******************************************************************************')
            CALL pOutFile%WriteData('C                           Final Soil Moisture Contents')                       
            CALL pOutFile%WriteData('C                              For Agricultural Lands')
            CALL pOutFile%WriteData('C')                                                                              
            CALL pOutFile%WriteData('C   IR   ;   Subregion ID')                                                         
            CALL pOutFile%WriteData('C   IS   ;   Soil type ID')                                                         
            CALL pOutFile%WriteData('C   SOILM;   Final root zone moisture content; [L/L]')
            CALL pOutFile%WriteData('C') 
            CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
            CALL pOutFile%WriteData('C   IR    IS    SOILM')
            CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
        
            !Values
            DO indxRegion=1,NRegions
                RootDepth = pAg%AvgCrop(indxRegion)%RootDepth
                DO indxSoil=1,RootZone%NSoils
                    IF (pAg%AgData(indxSoil,indxRegion)%Area .EQ. 0.0) THEN
                        rValue = 0.0
                    ELSE
                        rValue = (pAg%AgData(indxSoil,indxRegion)%SoilM_Precip + pAg%AgData(indxSoil,indxRegion)%SoilM_AW + pAg%AgData(indxSoil,indxRegion)%SoilM_Oth) / RootDepth
                    END IF
                    WRITE (cOutput,'(I6,I6,F9.3)') indxRegion,indxSoil,rValue
                    CALL pOutFile%WriteData(cOutput)
                END DO
            END DO
        END IF
            
        !Urban moistures
        IF (pFlags%lUrban_Defined) THEN
         
            !Titles
            CALL pOutFile%WriteData('C*******************************************************************************')
            CALL pOutFile%WriteData('C                           Final Soil Moisture Contents')                       
            CALL pOutFile%WriteData('C                                For Urban Lands')
            CALL pOutFile%WriteData('C')                                                                              
            CALL pOutFile%WriteData('C   IR   ;   Subregion ID')                                                         
            CALL pOutFile%WriteData('C   IS   ;   Soil type ID')                                                         
            CALL pOutFile%WriteData('C   SOILM;   Final root zone moisture content; [L/L]')
            CALL pOutFile%WriteData('C') 
            CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
            CALL pOutFile%WriteData('C   IR    IS    SOILM')
            CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
        
            !Values
            DO indxRegion=1,NRegions
                DO indxSoil=1,RootZone%NSoils
                    IF (pUrban%UrbData(indxSoil,indxRegion)%Area .EQ. 0.0) THEN
                        rValue = 0.0
                    ELSE
                        rValue = (pUrban%UrbData(indxSoil,indxRegion)%SoilM_Precip + pUrban%UrbData(indxSoil,indxRegion)%SoilM_AW + pUrban%UrbData(indxSoil,indxRegion)%SoilM_Oth) / pUrban%RootDepth
                    END IF
                    WRITE (cOutput,'(I6,I6,F9.3)') indxRegion,indxSoil,rValue
                    CALL pOutFile%WriteData(cOutput)
                END DO
            END DO
        END IF
        
        !Native and riparian veg moistures
        IF (pFlags%lNVRV_Defined) THEN
         
            !Titles
            CALL pOutFile%WriteData('C*******************************************************************************')
            CALL pOutFile%WriteData('C                           Final Soil Moisture Contents')                       
            CALL pOutFile%WriteData('C                      For Native and Riparian Vegetation Lands')
            CALL pOutFile%WriteData('C')                                                                              
            CALL pOutFile%WriteData('C   IR   ;   Subregion ID')                                                         
            CALL pOutFile%WriteData('C   IS   ;   Soil type ID')                                                         
            CALL pOutFile%WriteData('C   SOILM;   Final root zone moisture content; [L/L]')
            CALL pOutFile%WriteData('C') 
            CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
            CALL pOutFile%WriteData('C   IR    IS    SOILM[NV]    SOILM[RV]')
            CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
        
            !Values
            DO indxRegion=1,NRegions
                DO indxSoil=1,RootZone%NSoils
                    IF (pNVRV%NativeVeg(indxSoil,indxRegion)%Area .EQ. 0.0) THEN
                        rValue = 0.0
                    ELSE
                        rValue = (pNVRV%NativeVeg(indxSoil,indxRegion)%SoilM_Precip + pNVRV%NativeVeg(indxSoil,indxRegion)%SoilM_AW + pNVRV%NativeVeg(indxSoil,indxRegion)%SoilM_Oth) / pNVRV%RootDepth_Native
                    END IF
                    IF (pNVRV%RiparianVeg(indxSoil,indxRegion)%Area .EQ. 0.0) THEN
                        rValue1 = 0.0
                    ELSE
                        rValue1 = (pNVRV%RiparianVeg(indxSoil,indxRegion)%SoilM_Precip + pNVRV%RiparianVeg(indxSoil,indxRegion)%SoilM_AW + pNVRV%RiparianVeg(indxSoil,indxRegion)%SoilM_Oth) / pNVRV%RootDepth_Riparian
                    END IF
                    WRITE (cOutput,'(I6,I6,F9.3,F9.3)') indxRegion,indxSoil,rValue,rValue1
                    CALL pOutFile%WriteData(cOutput)
                END DO
            END DO
        END IF
        
    END ASSOCIATE
    
  END SUBROUTINE WriteFinalMoistures
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO ROOT ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToBudRawFile(AppGrid,RPump_Ag,RDeli_Ag,RGenericMoist_Ag,RPump_Urb,RDeli_Urb,RGenericMoist_Urb,RUpstrmRunoff_Ag,RUpstrmRunoff_Urb,RUpstrmRunoff_NV,RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_NV,RootZone)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RPump_Ag,RDeli_Ag,RGenericMoist_Ag,RPump_Urb,RDeli_Urb,RGenericMoist_Urb,RUpstrmRunoff_Ag,RUpstrmRunoff_Urb,RUpstrmRunoff_NV,RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_NV
    TYPE(RootZone_v50_Type)         :: RootZone
    
    !Local variables
    INTEGER                                  :: NRegions
    REAL(8)                                  :: DummyArray(NRootZoneBudColumns,(AppGrid%NSubregions+1)) 
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RRunoff_Ag,RRunoff_Urb,RRunoff_NV,                   &
                                                RPrecip_Ag,RPrecip_Urb,RPrecip_NV,                   &
                                                RReuse_Ag,RReuse_Urb,                                &
                                                RReturn_Ag,RReturn_Urb,                              &
                                                RSoilMCh_Ag,RSoilMCh_Urb,RSoilMCh_NV,                &
                                                RInfilt_Ag,RInfilt_Urb,RInfilt_NV,                   &
                                                RETPot_Ag,RETPot_Urb,RETPot_NV,                      &
                                                RETa_Ag,RETa_Urb,RETa_NV,                            &
                                                RPerc_Ag,RPerc_Urb,RPerc_NV,             &
                                                Error_Ag,Error_Urb,Error_NV
    
    !Initialize
    NRegions      = AppGrid%NSubregions
    
    RETPot_Ag     = 0.0
    RPrecip_Ag    = 0.0
    RRunoff_Ag    = 0.0
    RReuse_Ag     = 0.0
    RReturn_Ag    = 0.0
    RSoilMCh_Ag   = 0.0
    RInfilt_Ag    = 0.0
    RETa_Ag       = 0.0
    RPerc_Ag      = 0.0
    Error_Ag      = 0.0
    RETPot_Urb    = 0.0
    RPrecip_Urb   = 0.0
    RRunoff_Urb   = 0.0
    RReuse_Urb    = 0.0
    RReturn_Urb   = 0.0
    RSoilMCh_Urb  = 0.0
    RInfilt_Urb   = 0.0
    RETa_Urb      = 0.0
    RPerc_Urb     = 0.0
    Error_Urb     = 0.0
    RETPot_NV     = 0.0
    RRunoff_NV    = 0.0
    RPrecip_NV    = 0.0
    RSoilMCh_NV   = 0.0
    RInfilt_NV    = 0.0
    RETa_NV       = 0.0
    RPerc_NV      = 0.0
    Error_NV      = 0.0
    
    !Regional moisture storages
    RootZone%RSoilM = RegionalMoistStorage(NRegions,RootZone)
    
    ASSOCIATE (pFlags => RootZone%Flags)
               
      !Compute subregional values
      !---------------------------
      !Ag lands
      IF (pFlags%lAg_Defined) THEN
          CALL RegionalETPot(NRegions,RootZone,iLandUse_Ag,RETPot_Ag)
          RPrecip_Ag   = RegionalPrecip(NRegions,RootZone,iLandUse_Ag)
          RRunoff_Ag   = RegionalRunoff(AppGrid,RootZone,iLandUse_Ag)
          RReuse_Ag    = RegionalReuse(NRegions,RootZone,iLandUse_Ag)
          RReturn_Ag   = RegionalReturn(AppGrid,RootZone,iLandUse_Ag)
          RSoilMCh_Ag  = RegionalSoilMChange(NRegions,RootZone,iLandUse_Ag) 
          RInfilt_Ag   = RegionalInfiltration(AppGrid,RootZone,iLandUse_Ag)
          RETa_Ag      = RegionalETa(NRegions,RootZone,iLandUse_Ag) 
          RPerc_Ag     = RegionalPerc(AppGrid,RootZone,iLandUse_Ag)
          Error_Ag     = RootZone%RSoilM_P(:,iLandUse_Ag) + RSoilMCh_Ag + RInfilt_Ag + RGenericMoist_Ag - RETa_Ag - RPerc_Ag - RootZone%RSoilM(:,iLandUse_Ag)
      END IF
            
      !Urban
      IF (pFlags%lUrban_Defined) THEN
          CALL RegionalETPot(NRegions,RootZone,iLandUse_Urban,RETPot_Urb)
          RPrecip_Urb   = RegionalPrecip(NRegions,RootZone,iLandUse_Urban)
          RRunoff_Urb   = RegionalRunoff(AppGrid,RootZone,iLandUse_Urban)
          RReuse_Urb    = RegionalReuse(NRegions,RootZone,iLandUse_Urban)
          RReturn_Urb   = RegionalReturn(AppGrid,RootZone,iLandUse_Urban)
          RSoilMCh_Urb  = RegionalSoilMChange(NRegions,RootZone,iLandUse_Urban) 
          RInfilt_Urb   = RegionalInfiltration(AppGrid,RootZone,iLandUse_Urban)
          RETa_Urb      = RegionalETa(NRegions,RootZone,iLandUse_Urban)
          RPerc_Urb     = RegionalPerc(AppGrid,RootZone,iLandUse_Urban)
          Error_Urb     = RootZone%RSoilM_P(:,iLandUse_Urban) + RSoilMCh_Urb + RInfilt_Urb + RGenericMoist_Urb - RETa_Urb - RPerc_Urb - RootZone%RSoilM(:,iLandUse_Urban)
      END IF
      
      !Native and riparian veg
      IF (pFlags%lNVRV_Defined) THEN
          CALL RegionalETPot(NRegions,RootZone,iLandUse_NVRV,RETPot_NV)
          RPrecip_NV   = RegionalPrecip(NRegions,RootZone,iLandUse_NVRV)       
          RRunoff_NV   = RegionalRunoff(AppGrid,RootZone,iLandUse_NVRV)
          RSoilMCh_NV  = RegionalSoilMChange(NRegions,RootZone,iLandUse_NVRV) 
          RInfilt_NV   = RegionalInfiltration(AppGrid,RootZone,iLandUse_NVRV)
          RETa_NV      = RegionalETa(NRegions,RootZone,iLandUse_NVRV)
          RPerc_NV     = RegionalPerc(AppGrid,RootZone,iLandUse_NVRV)
          Error_NV     = RootZone%RSoilM_P(:,iLandUse_NVRV) + RSoilMCh_NV + RInfilt_NV + RGenericMoist_NV - RETa_NV - RPerc_NV - RootZone%RSoilM(:,iLandUse_NVRV)
      END IF
      
    END ASSOCIATE
    
    !Store in temporary array
    DummyArray(1,:)  = RLUArea_Ag                                                  !Agricultural area
    DummyArray(2,:)  = RETPot_Ag                                                   !Potential ET on ag lands
    DummyArray(3,:)  = RPrecip_Ag                                                  !Precipitation on ag lands
    DummyArray(4,:)  = RRunoff_Ag                                                  !Runoff from ag lands
    DummyArray(5,:)  = RDeli_Ag + RPump_Ag                                         !Prime applied water on ag lands prior to application of re-use water
    DummyArray(6,:)  = RUpstrmRunoff_Ag                                            !Surface runoff from upstream elements/subregions used on ag lands
    DummyArray(7,:)  = RReuse_Ag                                                   !Applied recycled water on ag lands 
    DummyArray(8,:)  = RReturn_Ag                                                  !Return flow from ag lands
    DummyArray(9,:)  = RootZone%RSoilM_P(:,iLandUse_Ag)                            !Storage at the beginning of the time interval
    DummyArray(10,:) = RSoilMCh_Ag                                                 !Soil moisture change due to expansion/contraction of ag lands
    DummyArray(11,:) = RInfilt_Ag                                                  !Infiltration on ag lands
    DummyArray(12,:) = RGenericMoist_Ag                                            !Generic moisture inflow to ag lands
    DummyArray(13,:) = RETa_Ag                                                     !ET on ag lands
    DummyArray(14,:) = RPerc_Ag                                                    !Percolation on ag lands
    DummyArray(15,:) = RootZone%RSoilM(:,iLandUse_Ag)                              !Storage at the end of the time interval
    DummyArray(16,:) = Error_Ag                                                    !Mass balance error for ag lands
    DummyArray(17,:) = RLUArea_Urb                                                 !Urban area
    DummyArray(18,:) = RETPot_Urb                                                  !Potential ET on urban lands
    DummyArray(19,:) = RPrecip_Urb                                                 !Precipitation on urban lands
    DummyArray(20,:) = RRunoff_Urb                                                 !Runoff from urban lands
    DummyArray(21,:) = RDeli_Urb + RPump_Urb                                       !Prime applied water on urban lands prior to re-used water
    DummyArray(22,:) = RUpstrmRunoff_Urb                                           !Surface runoff from upstream elements/subregions used on urban lands
    DummyArray(23,:) = RReuse_Urb                                                  !Applied recycled water on urban indoors and outdoors
    DummyArray(24,:) = RReturn_Urb                                                 !Return flow from urban lands
    DummyArray(25,:) = RootZone%RSoilM_P(:,iLandUse_Urban)                         !Storage at the beginning of the time interval
    DummyArray(26,:) = RSoilMCh_Urb                                                !Soil moisture change due to expansion/contraction of urban lands
    DummyArray(27,:) = RInfilt_Urb                                                 !Infiltration on urban lands
    DummyArray(28,:) = RGenericMoist_Urb                                           !Generic moisture inflow to urban lands
    DummyArray(29,:) = RETa_Urb                                                    !ET on urban lands
    DummyArray(30,:) = RPerc_Urb                                                   !Percolation on urban lands
    DummyArray(31,:) = RootZone%RSoilM(:,iLandUse_Urban)                           !Storage at the end of the time interval     
    DummyArray(32,:) = Error_Urb                                                   !Mass balance error at urban lands
    DummyArray(33,:) = RLUArea_NV                                                  !Natural area
    DummyArray(34,:) = RETPot_NV                                                   !Potential ET on natural lands
    DummyArray(35,:) = RPrecip_NV                                                  !Precipitation on natural lands
    DummyArray(36,:) = RUpstrmRunoff_NV                                            !Runoff from upstream elements onto natural lands
    DummyArray(37,:) = RRunoff_NV                                                  !Total surface flow on natural lands
    DummyArray(38,:) = RootZone%RSoilM_P(:,iLandUse_NVRV)                          !Storage at the beginning of the time interval
    DummyArray(39,:) = RSoilMCh_NV                                                 !Soil moisture change due to expansion/contraction of natural lands
    DummyArray(40,:) = RInfilt_NV                                                  !Infiltration on natural lands
    DummyArray(41,:) = RGenericMoist_NV                                            !Generic moisture inflow to natural lands
    DummyArray(42,:) = RETa_NV                                                     !ET on natural lands
    DummyArray(43,:) = RPerc_NV                                                    !Percolation on natural lands
    DummyArray(44,:) = RootZone%RSoilM(:,iLandUse_NVRV)                            !Storage at the end of the time interval          
    DummyArray(45,:) = Error_NV                                                    !Mass balance error at native and riparian lands

    !Print out values to binary file
    CALL RootZone%RootZoneBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteRootZoneFlowsToBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO LAND & WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToBudRawFile(NRegions,RLUArea_Ag,RLUArea_Urb,RDemand_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmRunoff_Ag,RUpstrmRunoff_Urb,RootZone)
    INTEGER,INTENT(IN)              :: NRegions
    REAL(8),DIMENSION(:),INTENT(IN) :: RLUArea_Ag,RLUArea_Urb,RDemand_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmRunoff_Ag,RUpstrmRunoff_Urb
    TYPE(RootZone_v50_Type)        :: RootZone
    
    !Local variables
    REAL(8)                       :: DummyArray(NLWUseBudColumns,(NRegions+1))
    REAL(8),DIMENSION(NRegions+1) :: rRawDemand_Ag,RDemand_Ag,RDemandShort_Ag,RDemandShort_Urb,         &
                                     RETAW,RETP,RETOth
    
    !Compute budget terms
    IF (RootZone%Flags%lAg_Defined) THEN
        RRawDemand_Ag   = RegionalAgRawDemand(NRegions,RootZone)
        RDemand_Ag      = RegionalDemand(NRegions,RootZone,iLandUse_Ag)
        RDemandShort_Ag = RDemand_Ag - RPump_Ag - RDeli_Ag - RUpstrmRunoff_Ag
        RETAW           = RegionalETAW(NRegions,RootZone)
        RETP            = RegionalETP(NRegions,RootZone)
        RETOth          = RegionalETOth(NRegions,RootZone)
    ELSE
        RRawDemand_Ag   = 0.0
        RDemand_Ag      = 0.0
        RDemandShort_Ag = 0.0
        RETAW           = 0.0
        RETP            = 0.0
        RETOth          = 0.0
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        RDemandShort_Urb = RDemand_Urb - RPump_Urb - RDeli_Urb - RUpstrmRunoff_Urb
    ELSE
        RDEmandShort_Urb = 0.0
    END IF
    
    !Store in temporary array
    DummyArray(1,:)  = RLUArea_Ag
    DummyArray(2,:)  = RRawDemand_Ag
    DummyArray(3,:)  = RDemand_Ag
    DummyArray(4,:)  = RPump_Ag
    DummyArray(5,:)  = RDeli_Ag
    DummyArray(6,:)  = RUpstrmRunoff_Ag
    DummyArray(7,:)  = RDemandShort_Ag
    DummyArray(8,:)  = RETAW
    DummyArray(9,:)  = RETP
    DummyArray(10,:) = RETOth
    DummyArray(11,:) = RLUArea_Urb
    DummyArray(12,:) = RDemand_Urb
    DummyArray(13,:) = RPump_Urb
    DummyArray(14,:) = RDeli_Urb
    DummyArray(15,:) = RUpstrmRunoff_Urb
    DummyArray(16,:) = RDemandShort_Urb

    !Print out values to binary file
    CALL RootZone%LWUseBudRawFile%WriteData(DummyArray)
    
  END SUBROUTINE WriteLWUseFlowsToBudRawFile

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO LAND & WATER USE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToZoneBudRawFile(AppGrid,RDemand_Urb,RootZone)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)           :: RDemand_Urb(:)
    TYPE(RootZone_v50_Type)      :: RootZone
    
    !Local variables
    INTEGER,PARAMETER                      :: NLayers = 1 , &
                                              NFaces  = 0 , &
                                              iLayer  = 1
    REAL(8),DIMENSION(AppGrid%NElements,1) :: rAgArea,rCUAW,rAgSupReq,rAgPump,rAgDiver,rAgSrfcInflow,rAgShort,rETAW,rETP,rETOth, &
                                              rUrbArea,rUrbSupReq,rUrbPump,rUrbDiver,rUrbSrfcInflow,rUrbShort
    REAL(8)                                :: rElemFrac
    INTEGER                                :: iSoil,iRegion,indxElem
    
    !Ag area
    IF (RootZone%Flags%lAg_Defined)  CALL RootZone%GetElementAgAreas(rAgArea(:,1))
    
    !Urban area
    IF (RootZone%Flags%lUrban_Defined) CALL RootZone%GetElementUrbanAreas(rUrbArea(:,1))
            
    !Compile data
    DO indxElem=1,AppGrid%NElements
        iRegion = AppGrid%AppElement(indxElem)%Subregion
        iSoil   = RootZone%ElemSoilType(indxElem)
        
        !Ag data
        IF (RootZone%Flags%lAg_Defined) THEN
            rCUAW(indxElem,1)     = RootZone%AgRootZone%AgData(iSoil,iRegion)%DemandRaw / RootZone%AgRootZone%AgData(iSoil,iRegion)%Area * rAgArea(indxElem,1) !Potential CUAW
            rAgSupReq(indxElem,1) = RootZone%AgRootZone%AgData(iSoil,iRegion)%Demand / RootZone%AgRootZone%AgData(iSoil,iRegion)%Area * rAgArea(indxElem,1)    !Ag supply requirement
            rAgPump(indxElem,1)   = RootZone%WaterSupply(iRegion)%Pumping_Ag / RootZone%AgRootZone%SubregionalArea(iRegion) * rAgArea(indxElem,1)              !Ag pumping
            rAgDiver(indxElem,1)  = RootZone%WaterSupply(iRegion)%Diversion_Ag / RootZone%AgRootZone%SubregionalArea(iRegion) * rAgArea(indxElem,1)            !Ag diversion
            rAgShort(indxElem,1)  = rAgSupReq(indxElem,1) - rAgPump(indxElem,1) - rAgDiver(indxElem,1)                                                         !Ag supply shortage
            IF (SIZE(RootZone%ElemFlowToSubregions) .GT. 0) THEN                                                                                               
                rAgSrfcInflow(indxElem,1) = RootZone%WaterSupply(iRegion)%UpstrmRunoff / AppGrid%AppSubregion(iRegion)%Area / rAgArea(indxElem,1)              !Surface inflow as runoff into ag areas 
                rAgShort(indxElem,1)      = rAgShort(indxElem,1) - rAgSrfcInflow(indxElem,1)                                                                   !Adjust ag shortage
            END IF                                                                                                                                             
            rETAW(indxElem,1)  = RootZone%AgRootZone%AgData(iSoil,iRegion)%ETAW * rAgArea(indxElem,1)                                                          !ETAW
            rETP(indxElem,1)   = RootZone%AgRootZone%AgData(iSoil,iRegion)%ETP * rAgArea(indxElem,1)                                                           !Ag effective precipitation
            rETOth(indxElem,1) = RootZone%AgRootZone%AgData(iSoil,iRegion)%ETOth * rAgArea(indxElem,1)                                                         !Ag ET met from other sources
        END IF
        
        !Urban data
        IF (RootZone%Flags%lUrban_Defined) THEN
            rElemFrac              = rUrbArea(indxElem,1) / RootZone%UrbanRootZone%SubregionalArea(iRegion)
            rUrbSupReq(indxElem,1) = RDemand_Urb(iRegion) * rElemFrac                                                                               !Urban supply requirement
            rUrbPump(indxElem,1)   = RootZone%WaterSupply(iRegion)%Pumping_Urb * rElemFrac                                                          !Urban pumping
            rUrbDiver(indxElem,1)  = RootZone%WaterSupply(iRegion)%Diversion_Urb * rElemFrac                                                        !Urban diversion
            rUrbShort(indxElem,1)  = rUrbSupReq(indxElem,1) - rUrbPump(indxElem,1) - rUrbDiver(indxElem,1)                                          !Urban supply shortage
            IF (SIZE(RootZone%ElemFlowToSubregions) .GT. 0) THEN
                rUrbSrfcInflow(indxElem,1) = RootZone%WaterSupply(iRegion)%UpstrmRunoff /AppGrid%AppSubregion(iRegion)%Area / rUrbArea(indxElem,1)  !Surface inflow as runoff into urban areas 
                rUrbShort(indxElem,1)      = rUrbShort(indxElem,1) - rUrbSrfcInflow(indxElem,1)                                                     !Adjust urban shortage
            END IF
        END IF
    END DO
    
    !Print data
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,1,iLayer,rAgArea)             !Ag area
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,2,iLayer,rCUAW)               !Potential CUAW
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,3,iLayer,rAgSupReq)           !Ag supply requirement
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,4,iLayer,rAGPump)             !Ag pumping
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,5,iLayer,rAgDiver)            !Ag diversion
    IF (SIZE(RootZone%ElemFlowToSubregions) .GT. 0) &                                             !Ag inflow as surface runoff
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,6,iLayer,rAgSrfcInflow)   
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,7,iLayer,rAgShort)            !Ag shortage
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,8,iLayer,rETAW)               !ETAW
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,9,iLayer,rETP)                !Ag effective precipitation
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,10,iLayer,rETOth)             !Ag ET met from other sources
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,11,iLayer,rUrbArea)           !Urban area
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,12,iLayer,rUrbSupReq)         !Urban supply requirement
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,13,iLayer,rUrbPump)           !Urban pumping
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,14,iLayer,rUrbDiver)          !Urban diversion
    IF (SIZE(RootZone%ElemFlowToSubregions) .GT. 0) &                                             !Urban inflow as surface runoff
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,15,iLayer,rUrbSrfcInflow)       
    CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,16,iLayer,rUrbShort)          !Urban shortage
        
  END SUBROUTINE WriteLWUseFlowsToZoneBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO ROOT ZONE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToZoneBudRawFile(AppGrid,RAW_Ag,RAW_Urb,RootZone)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)           :: RAW_Ag(:),RAW_Urb(:)
    TYPE(RootZone_v50_Type)      :: RootZone
    
    !Local variables
    INTEGER,PARAMETER                      :: NLayers = 1 , &
                                              NFaces  = 0 , &
                                              iLayer  = 1
    REAL(8),DIMENSION(AppGrid%NElements,1) :: rAgArea,rAgPotET,rAgPrecip,rAgRunoff,rAgAW,rAgSrfcInflow,rAgReuse,rAgReturn,rAgBeginStor,rAgSoilMCh,rAgInfilt,rAgOthIn,rAgETa,rAgDP,rAgEndStor,rAgError,                   &
                                              rUrbArea,rUrbPotET,rUrbPrecip,rUrbRunoff,rUrbAW,rUrbSrfcInflow,rUrbReuse,rUrbReturn,rUrbBeginStor,rUrbSoilMCh,rUrbInfilt,rUrbOthIn,rUrbETa,rUrbDP,rUrbEndStor,rUrbError,   &
                                              rNVArea,rRVArea,rNVRVPotET,rNVRVPrecip,rNVRVRunoff,rNVRVSrfcInflow,rNVRVBeginStor,rNVRVSoilMCh,rNVRVInfilt,rNVRVOthIn,rNVRVETa,rNVRVDP,rNVRVEndStor,rNVRVError
    REAL(8)                                :: rElemFrac 
    INTEGER                                :: iRegion,indxElem,iSoil,iElem
    LOGICAL                                :: lElemToElemFlows_Defined
    
    !Initialize
    IF (SIZE(RootZone%ElemFlowToSubregions) .GT. 0) THEN
        lElemToElemFlows_Defined = .TRUE.
    ELSE
        lElemToElemFlows_Defined = .FALSE.
    END IF
    
    !Ag area
    IF (RootZone%Flags%lAg_Defined)  CALL RootZone%GetElementAgAreas(rAgArea(:,1))
    
    !Urban area
    IF (RootZone%Flags%lUrban_Defined) CALL RootZone%GetElementUrbanAreas(rUrbArea(:,1))
            
    !Native and riparian veg area
    IF (RootZone%Flags%lNVRV_Defined) THEN
        rNVArea(:,1) = RootZone%NVRVRootZone%ElementalArea_NV
        rRVArea(:,1) = RootZone%NVRVRootZone%ElementalArea_RV
    END IF
            
    !Compile data
    DO indxElem=1,AppGrid%NElements
        iRegion = AppGrid%AppElement(indxElem)%Subregion
        iSoil   = RootZone%ElemSoilType(indxElem)
        
        !Ag data
        IF (RootZone%Flags%lAg_Defined) THEN
            rElemFrac                = rAgArea(indxElem,1) / RootZone%AgRootZone%SubregionalArea(iRegion)
            rAgPotET(indxElem,1)     = RootZone%AgRootZone%AvgCrop(iRegion)%ETc * rAgArea(indxElem,1)                                                                    !Ag potential ET 
            rAgPrecip(indxElem,1)    = RootZone%SoilRegionPrecip(iSoil,iRegion) * rAgArea(indxElem,1)                                                                    !Ag precip
            rAgRunoff(indxElem,1)    = RootZone%AgRootZone%AgData(iSoil,iRegion)%Runoff * rAgArea(indxElem,1)                                                            !Ag runoff
            rAgAW(indxElem,1)        = RAW_Ag(iRegion) * rElemFrac                                                                                                       !Ag prime appliaed water
            IF (lElemToElemFlows_Defined) &                                                                                                                              !Ag surface inflow from upstream   
                rAgSrfcInflow(indxElem,1) = RootZone%WaterSupply(iRegion)%UpstrmRunoff / AppGrid%AppSubregion(iRegion)%Area * rAgArea(indxElem,1)                                                          
            rAgReuse(indxElem,1)     = RootZone%AgRootZone%AgData(iSoil,iRegion)%Reuse * rAgArea(indxElem,1)                                                             !Ag reuse
            rAgReturn(indxElem,1)    = RootZone%AgRootZone%AgData(iSoil,iRegion)%ReturnFlow * rAgArea(indxElem,1)                                                        !Ag return
            rAgBeginStor(indxElem,1) = (RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_Precip_P_BeforeUpdate  &                                                         !Ag beginning storage
                                      + RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_AW_P_BeforeUpdate      &                                                                      
                                      + RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_Oth_P_BeforeUpdate     ) * RootZone%AgRootZone%ElementalArea_P(indxElem) 
            IF (RootZone%AgRootZone%AgData(iSoil,iRegion)%Area .GT. 0.0) THEN                                                                                            !Ag change in soil moisture due to land expansion
                rAgSoilMCh(indxElem,1) = RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilMCh / RootZone%AgRootZone%AgData(iSoil,iRegion)%Area * rAgArea(indxElem,1)          
            ELSE
                rAgSoilMCh(indxElem,1) = 0.0
            END IF
            rAgInfilt(indxElem,1)    = (RootZone%AgRootZone%AgData(iSoil,iRegion)%PrecipInfilt + RootZone%AgRootZone%AgData(iSoil,iRegion)%IrigInfilt) * rAgArea(indxElem,1) !Ag infiltration
            IF (RootZone%Flags%lGenericMoistureFile_Defined)   &                                                                                                             !Ag other inflow
                rAgOthIn(indxElem,1) = (RootZone%GenericMoistureData%rGenericMoisture(iSoil,iRegion) * RootZone%AgRootZone%AvgCrop(iRegion)%RootDepth - RootZone%AgRootZone%AgData(iSoil,iRegion)%GMExcess) * rAgArea(indxElem,1) 
            rAgETa(indxElem,1)       = RootZone%AgRootZone%AgData(iSoil,iRegion)%ETa * rAgArea(indxElem,1)                                                               !Ag actual ET
            rAgDP(indxElem,1)        = (RootZone%AgRootZone%AgData(iSoil,iRegion)%Perc + RootZone%AgRootZone%AgData(iSoil,iRegion)%PercCh) * rAgArea(indxElem,1)         !Ag perc                                                             
            rAgEndStor(indxElem,1)   = (RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_Precip  &                                                                        !Ag ending storage
                                      + RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_AW      &                                                                      
                                      + RootZone%AgRootZone%AgData(iSoil,iRegion)%SoilM_Oth     ) * rAgArea(indxElem,1)
            rAgError(indxElem,1)     = rAgBeginStor(indxElem,1) + rAgSoilMCh(indxElem,1) + rAgInfilt(indxElem,1) - rAgETa(indxElem,1) - rAgDP(indxElem,1) - rAgEndStor(indxElem,1) !Ag error                                                              
            IF (RootZone%Flags%lGenericMoistureFile_Defined) rAgError(indxElem,1) = rAgError(indxElem,1) + rAgOthIn(indxElem,1)                                                                                                           
        END IF
        
        !Urban data
        IF (RootZone%Flags%lUrban_Defined) THEN
            rElemFrac                 = rUrbArea(indxElem,1) / RootZone%UrbanRootZone%SubregionalArea(iRegion)
            rUrbPotET(indxElem,1)     = RootZone%UrbanRootZone%RegionETPot(iRegion) * rElemFrac                                                                          !Urban potential ET
            rUrbPrecip(indxElem,1)    = RootZone%SoilRegionPrecip(iSoil,iRegion) * rUrbArea(indxElem,1)                                                                  !Urban precip
            rUrbRunoff(indxElem,1)    = RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Runoff * rUrbArea(indxElem,1)                                                      !Urban runoff
            rUrbAW(indxElem,1)        = RAW_Urb(iRegion) * rElemFrac                                                                                                     !Urban prime appliaed water
            IF (lElemToElemFlows_Defined) &                                                                                                                              !Urban surface inflow from upstream                                       
                rUrbSrfcInflow(indxElem,1) = RootZone%WaterSupply(iRegion)%UpstrmRunoff / AppGrid%AppSubregion(iRegion)%Area * rUrbArea(indxElem,1)                                                        
            rUrbReuse(indxElem,1)     = RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Reuse * rUrbArea(indxElem,1)                                                       !Urban reuse
            rUrbReturn(indxElem,1)    = RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%ReturnFlow * rUrbArea(indxElem,1)                                                  !Urban return
            rUrbBeginStor(indxElem,1) = (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_Precip_P_BeforeUpdate  &                                                    !Urban beginning storage
                                       + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_AW_P_BeforeUpdate      &
                                       + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_Oth_P_BeforeUpdate     ) * RootZone%UrbanRootZone%ElementalArea_P(indxElem)  &
                                                                                                                      * RootZone%UrbanRootZone%PerviousFrac(iRegion)                                   
            IF (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Area .GT. 0.0) THEN                                                                                        !Urban change in soil moisture due to land expansion
                rUrbSoilMCh(indxElem,1) = RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilMCh / RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Area * rUrbArea(indxElem,1)  
            ELSE
                rUrbSoilMCh(indxElem,1) = 0.0
            END IF
            rUrbInfilt(indxElem,1)    = (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%PrecipInfilt + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%IrigInfilt) * rUrbArea(indxElem,1) !Urban infiltration
            IF (RootZone%Flags%lGenericMoistureFile_Defined)   &                                                                                                                       !Urban other inflow
                rUrbOthIn(indxElem,1) = (RootZone%GenericMoistureData%rGenericMoisture(iSoil,iRegion) * RootZone%UrbanRootZone%RootDepth - RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%GMExcess) * rUrbArea(indxElem,1) * RootZone%UrbanRootZone%PerviousFrac(iRegion) 
            rUrbETa(indxElem,1)       = RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%ETa * rUrbArea(indxElem,1)                                                                       !Urban actual ET
            rUrbDP(indxElem,1)        = (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Perc + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%PercCh) * rUrbArea(indxElem,1)             !Urban perc                                                              ! Ag actual ET
            rUrbEndStor(indxElem,1)   = (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_Precip  &                                                                                 !Urban ending storage
                                       + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_AW      &
                                       + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%SoilM_Oth     ) * rUrbArea(indxElem,1) &
                                                                                                        * RootZone%UrbanRootZone%PerviousFrac(iRegion)                                    
            rUrbError(indxElem,1)    = rUrbBeginStor(indxElem,1) + rUrbSoilMCh(indxElem,1) + rUrbInfilt(indxElem,1) - rUrbETa(indxElem,1) - rUrbDP(indxElem,1) - rUrbEndStor(indxElem,1) !Urban error                                                              ! Ag actual ET
            IF (RootZone%Flags%lGenericMoistureFile_Defined) rUrbError(indxElem,1) = rUrbError(indxElem,1) + rUrbOthIn(indxElem,1)                                                                                                            !Ag other inflow
        END IF

        !Native and riparian veg. data
        IF (RootZone%Flags%lNVRV_Defined) THEN
            rNVRVPotET(indxElem,1)     = RootZone%NVRVRootZone%RegionETPot_NV(iRegion) / RootZone%NVRVRootZone%SubregionalArea_NV(iRegion) * rNVArea(indxElem,1)  &                  !Native and riparian potential ET
                                       + RootZone%NVRVRootZone%RegionETPot_RV(iRegion) / RootZone%NVRVRootZone%SubregionalArea_RV(iRegion) * rRVArea(indxElem,1)                                             
            rNVRVPrecip(indxElem,1)    = RootZone%SoilRegionPrecip(iSoil,iRegion) * (rNVArea(indxElem,1)+rRVArea(indxElem,1))                                                        !Native and riparian precip
            IF (SIZE(RootZone%ElemFlowToSubregions) .GT. 0) &                                                                                                                        !Surface inflow as runoff into native and riparian veg areas
                rNVRVSrfcInflow(indxElem,1) = RootZone%WaterSupply(iRegion)%UpstrmRunoff / AppGrid%AppSubregion(iRegion)%Area * (rNVArea(indxElem,1)+rRVArea(indxElem,1))                                                                           
            rNVRVRunoff(indxElem,1)    = RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%Runoff * rNVArea(indxElem,1)   &                                                             !Native and riparian runoff
                                       + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%Runoff * rRVArea(indxElem,1)                                                                                     
            rNVRVBeginStor(indxElem,1) = (RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_Precip_P_BeforeUpdate  &                                                              !Native and riparian beginning storage
                                        + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_AW_P_BeforeUpdate      &
                                        + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_Oth_P_BeforeUpdate     ) * RootZone%NVRVRootZone%ElementalArea_P_NV(indxElem) &                                   
                                        +(RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_Precip_P_BeforeUpdate  &
                                        + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_AW_P_BeforeUpdate      &
                                        + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_Oth_P_BeforeUpdate     ) * RootZone%NVRVRootZone%ElementalArea_P_RV(indxElem)                                  
            IF (RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%Area .GT. 0.0) THEN                                                                                                   !Native and riparian change in soil moisture due to land expansion
                rNVRVSoilMCh(indxElem,1) = RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilMCh / RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%Area * rNVArea(indxElem,1)     
            ELSE
                rNVRVSoilMCh(indxElem,1) = 0.0
            END IF
            IF (RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%Area .GT. 0.0) THEN
                rNVRVSoilMCh(indxElem,1) = rNVRVSoilMCh(indxElem,1) + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilMCh / RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%Area * rRVArea(indxElem,1)
            END IF
            rNVRVInfilt(indxElem,1)    = RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%PrecipInfilt * rNVArea(indxElem,1)   &                                                           !Native and riparian infiltration
                                       + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%PrecipInfilt * rRVArea(indxElem,1)                                                             
            IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                                                        !Native and riparian other inflow
                rNVRVOthIn(indxElem,1) = (RootZone%GenericMoistureData%rGenericMoisture(iSoil,iRegion) * RootZone%NVRVRootZone%RootDepth_Native   - RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%GMExcess) * rNVArea(indxElem,1)  &
                                        +(RootZone%GenericMoistureData%rGenericMoisture(iSoil,iRegion) * RootZone%NVRVRootZone%RootDepth_Riparian - RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%GMExcess) * rRVArea(indxElem,1) 
            END IF
            rNVRVETa(indxElem,1)       = RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%ETa * rNVArea(indxElem,1)  &                                                                     !Native and riparian actual ET
                                       + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%ETa * rRVArea(indxElem,1) 
            rNVRVDP(indxElem,1)        = (RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%Perc + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%PercCh) * rNVArea(indxElem,1)   &         !Native and riparian perc
                                       + (RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%Perc + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%PercCh) * rRVArea(indxElem,1) 
            rNVRVEndStor(indxElem,1)   = (RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_Precip    &                                                                               !Native and riparian ending storage
                                        + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_AW        &
                                        + RootZone%NVRVRootZone%NativeVeg(iSoil,iRegion)%SoilM_Oth       ) * rNVArea(indxElem,1) &                                   
                                        +(RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_Precip  &
                                        + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_AW      &
                                        + RootZone%NVRVRootZone%RiparianVeg(iSoil,iRegion)%SoilM_Oth     ) * rRVArea(indxElem,1)                                    
            rNVRVError(indxElem,1)     = rNVRVBeginStor(indxElem,1) + rNVRVSoilMCh(indxElem,1) + rNVRVInfilt(indxElem,1) - rNVRVETa(indxElem,1) - rNVRVDP(indxElem,1) - rNVRVEndStor(indxElem,1) !Urban error                                                              ! Ag actual ET
            IF (RootZone%Flags%lGenericMoistureFile_Defined) rNVRVError(indxElem,1) = rNVRVError(indxElem,1) + rNVRVOthIn(indxElem,1)                                                                                                            !Ag other inflow
        END IF
    END DO
    
    !Update urban infiltration and perc for those elements that surface flows go into groundwater
    DO indxElem=1,SIZE(RootZone%UrbanRootZone%ElemToGW)
        iElem               = RootZone%UrbanRootZone%ElemToGW(indxElem)
        rUrbInfilt(iElem,1) = rUrbInfilt(iElem,1) + rUrbRunoff(iElem,1) + rUrbReturn(iElem,1)
        rUrbDP(iElem,1)     = rUrbDP(iElem,1) + rUrbRunoff(iElem,1) + rUrbReturn(iElem,1)
    END DO
    
    !Print data
    IF (RootZone%Flags%lAg_Defined) THEN
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,1,iLayer,rAgArea)               !Ag area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,2,iLayer,rAgPotET)              !Ag potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,3,iLayer,rAgPrecip)             !Ag precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,4,iLayer,rAgRunoff)             !Ag runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,5,iLayer,rAgAW)                 !Ag prime applied water
        IF (lElemToElemFlows_Defined)  &                                                                     !Ag surface inflow from upstream
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,6,iLayer,rAgSrfcInflow)     
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,7,iLayer,rAgReuse)              !Ag reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,8,iLayer,rAgReturn)             !Ag return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,9,iLayer,rAgBeginStor)          !Ag beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,10,iLayer,rAgSoilMCh)           !Ag change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,11,iLayer,rAgInfilt)            !Ag infiltration
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                   !Ag other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,12,iLayer,rAgOthIn)             
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,13,iLayer,rAgETa)               !Ag actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,14,iLayer,rAgDP)                !Ag perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,15,iLayer,rAgEndStor)           !Ag end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,16,iLayer,rAgError)             !Ag error
    END IF                                                                                                   
                                                                                                             
    IF (RootZone%Flags%lUrban_Defined) THEN                                                                  
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,17,iLayer,rUrbArea)             !Urban area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,18,iLayer,rUrbPotET)            !Urban potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,19,iLayer,rUrbPrecip)           !Urban precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,20,iLayer,rUrbRunoff)           !Urban runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,21,iLayer,rUrbAW)               !Urban prime applied water
        IF (lElemToElemFlows_Defined)  &                                                                     !Urban surface inflow from upstream
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,22,iLayer,rUrbSrfcInflow)       
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,23,iLayer,rUrbReuse)            !Urban reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,24,iLayer,rUrbReturn)           !Urban return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,25,iLayer,rUrbBeginStor)        !Urban beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,26,iLayer,rUrbSoilMCh)          !Urban change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,27,iLayer,rUrbInfilt)           !Urban infiltration
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                   !Urban other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,28,iLayer,rUrbOthIn)             
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,29,iLayer,rUrbETa)              !Urban actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,30,iLayer,rUrbDP)               !Urban perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,31,iLayer,rUrbEndStor)          !Urban end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,32,iLayer,rUrbError)            !Urban error
    END IF                                                                                                   
                                                                                                             
    IF (RootZone%Flags%lNVRV_Defined) THEN                                                                   
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,33,iLayer,rNVArea+rRVArea)      !NVRV area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,34,iLayer,rNVRVPotET)           !NVRV potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,35,iLayer,rNVRVPrecip)          !NVRV precip
        IF (lElemToElemFlows_Defined)  &                                                                     !NVRV surface inflow from upstream elements
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,36,iLayer,rNVRVSrfcInflow)      
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,37,iLayer,rNVRVRunoff)          !NVRV runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,38,iLayer,rNVRVBeginStor)       !NVRV beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,39,iLayer,rNVRVSoilMCh)         !NVRV change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,40,iLayer,rNVRVInfilt)          !NVRV infiltration
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                   !NVRV other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,41,iLayer,rNVRVOthIn)             
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,42,iLayer,rNVRVETa)             !NVRV actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,43,iLayer,rNVRVDP)              !NVRV perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,44,iLayer,rNVRVEndStor)         !NVRV end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,45,iLayer,rNVRVError)           !NVRV error
    END IF
    
  END SUBROUTINE WriteRootZoneFlowsToZoneBudRawFile
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DEMAND COMPUTATIONS AND ROUTING
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMPUTE AGRICULTURAL WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_ComputeWaterDemand(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v50_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid   !Not used in this version
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData    !Not used in this version
    INTEGER,INTENT(OUT)           :: iStat
    
    !Initialize
    iStat = 0

    !Compute ag water demand
    IF (RootZone%Flags%lAg_Defined) THEN
        CALL EchoProgress('Computing agricultural water demand...',lAdvance=.FALSE.)
        CALL RootZone%AgRootZone%ComputeWaterDemand(TimeStep%DeltaT                               , &
                                                    RootZone%SoilRegionPrecip                     , &
                                                    RootZone%GenericMoistureData%rGenericMoisture , &
                                                    RootZone%SubregionSoilsData                   , &
                                                    RootZone%ReuseFracFile%rValues                , &
                                                    RootZone%ReturnFracFile%rValues               , &
                                                    RootZone%SolverData                           , &
                                                    iStat                                         )
        IF (iStat .EQ. -1) RETURN
        CALL EchoProgress('DONE')
    END IF

  END SUBROUTINE RootZone_v50_ComputeWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SOIL MOISTURE IN ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_Simulate(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v50_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21)           :: ThisProcedure = ModName // 'RootZone_v50_Simulate'
    INTEGER                                :: indxRegion,indxIter
    REAL(8)                                :: AchievedConv,Runoff,Runoff_P
    REAL(8),DIMENSION(AppGrid%NSubregions) :: IrigSupply_Ag,IrigSupply_Urb,UpstrmRunoff_P,InRunoff,Supply  
    
    !Initialize
    iStat = 0
                                                              
    ASSOCIATE (pFlags                   => RootZone%Flags                                   , &
               pWaterSupply             => RootZone%WaterSupply                             , &
               pElemSoilType            => RootZone%ElemSoilType                            , &
               pSoilsData               => RootZone%SubregionSoilsData                      , &
               pSoilRegionPrecip        => RootZone%SoilRegionPrecip                        , &
               prGenericMoisture        => RootZone%GenericMoistureData%rGenericMoisture    , &
               pReuseFracs              => RootZone%ReuseFracFile%rValues                   , &
               pReturnFracs             => RootZone%ReturnFracFile%rValues                  , &
               pSolverData              => RootZone%SolverData                              , &
               pElemToSubregions        => RootZone%ElemFlowToSubregions                    , &
               pAgLand                  => RootZone%AgRootZone                              , &
               pUrbanLand               => RootZone%UrbanRootZone                           , &
               pNVRV                    => RootZone%NVRVRootZone                            )
        
      !Initialize
      pWaterSupply%UpstrmRunoff = 0.0
      IF (pFlags%lAg_Defined)  &
          IrigSupply_Ag = (pWaterSupply%Diversion_Ag  + pWaterSupply%Pumping_Ag)  / RootZone%AgRootZone%SubregionalArea
      IF (pFlags%lUrban_Defined)  &
          IrigSupply_Urb = (pWaterSupply%Diversion_Urb + pWaterSupply%Pumping_Urb) / RootZone%UrbanRootZone%SubregionalArea
      
      !Check water supply vs. ag lands
      IF (pFlags%lAg_Defined) THEN
          DO indxRegion=1,AppGrid%NSubregions
              !Check ag area vs. ag water supply
              IF (IrigSupply_Ag(indxRegion) .GT. 0.0) THEN
                  IF (pAgLand%SubregionalArea(indxRegion) .EQ. 0.0) THEN
                      CALL SetLastMessage('Agricultural applied water in subregion '//TRIM(IntToText(indxRegion))// 'cannot be non-zero when agricultural area is zero!',iFatal,ThisProcedure)
                      iStat = -1
                      RETURN
                  END IF
              END IF
          END DO
      END IF
      
      !Check urban area vs. urban water supply
      IF (pFlags%lUrban_Defined) THEN
          DO indxRegion=1,AppGrid%NSubregions
              IF (IrigSupply_Urb(indxRegion) .GT. 0.0) THEN
                  IF (pUrbanLand%SubregionalArea(indxRegion) .EQ. 0.0) THEN
                      CALL SetLastMessage('Urban applied water in subregion '//TRIM(IntToText(indxRegion))//' cannot be non-zero when urban area is zero!',iFatal,ThisProcedure)
                      iStat = -1
                      RETURN
                  END IF
              END IF
          END DO
      END IF
      
      !Iterative solution
      DO indxIter=1,pSolverData%IterMax
          !Store UpstrmRunoff values in temporary storage and zero it out
          UpstrmRunoff_P            = pWaterSupply%UpstrmRunoff   !UpstrmRunoff is in units of volume
          pWaterSupply%UpstrmRunoff = 0.0
          InRunoff                  = UpstrmRunoff_P / AppGrid%AppSubregion%Area
        
          !Simulate ag lands
          IF (pFlags%lAg_Defined) THEN
              Supply = InRunoff + IrigSupply_Ag     
              CALL pAgLand%Simulate(TimeStep%DeltaT           , &
                                    pSoilRegionPrecip         , &
                                    prGenericMoisture         , &
                                    pSoilsData                , &
                                    Supply                    , &
                                    pReuseFracs               , &
                                    pReturnFracs              , &
                                    pSolverData               , &
                                    iStat                     )
              IF (iStat .EQ. -1) RETURN
              CALL FlowToSubregions(pAgLand%AgData%Runoff     , &
                                    pAgLand%ElementalArea     , &
                                    pElemToSubregions         , &
                                    pElemSoilType             , &
                                    AppGrid                   , &
                                    pWaterSupply%UpstrmRunoff )
          END IF
          
          !Simulate urban lands
          IF (pFlags%lUrban_Defined) THEN
              Supply = InRunoff + IrigSupply_Urb      
              CALL pUrbanLand%Simulate(ETData                    , &
                                       TimeStep%DeltaT           , &
                                       pSoilRegionPrecip         , &
                                       prGenericMoisture         , &
                                       pSoilsData                , &
                                       Supply                    , &
                                       pReuseFracs               , &
                                       pReturnFracs              , &
                                       pSolverData               , &
                                       iStat                     )
              IF (iStat .EQ. -1) RETURN
              CALL FlowToSubregions(pUrbanLand%UrbData%Runoff    , &
                                    pUrbanLand%ElementalArea     , &
                                    pUrbanLand%ElemToSubregions  , &
                                    pElemSoilType                , &
                                    AppGrid                      , &
                                    pWaterSupply%UpstrmRunoff    )
          END IF

          !Simulate native and riparian veg lands
          IF (pFlags%lNVRV_Defined) THEN
              CALL pNVRV%Simulate(ETData               , &
                                  TimeStep%DeltaT      , &
                                  pSoilRegionPrecip    , &
                                  prGenericMoisture    , &
                                  pSoilsData           , &
                                  InRunoff             , &
                                  pSolverData          , &
                                  iStat                )
              IF (iStat .EQ. -1) RETURN
              CALL FlowToSubregions(pNVRV%NativeVeg%Runoff     , &
                                    pNVRV%ElementalArea_NV     , &
                                    pElemToSubregions          , &
                                    pElemSoilType              , &
                                    AppGrid                    , &
                                    pWaterSupply%UpstrmRunoff  )
              CALL FlowToSubregions(pNVRV%RiparianVeg%Runoff   , &
                                    pNVRV%ElementalArea_RV     , &
                                    pElemToSubregions          , &
                                    pElemSoilType              , &
                                    AppGrid                    , &
                                    pWaterSupply%UpstrmRunoff  )
          END IF 
          
          !Check convergence
          AchievedConv = 0.0
          DO indxRegion=1,AppGrid%NSubregions
              Runoff   = pWaterSupply(indxRegion)%UpstrmRunoff
              Runoff_P = UpstrmRunoff_P(indxRegion)
              IF (Runoff .EQ. 0.0) THEN
                  IF (Runoff_P .EQ. 0.0) THEN
                      CYCLE
                  ELSE
                      AchievedConv = MAX(AchievedConv , ABS((Runoff - Runoff_P) / Runoff_P))
                  END IF
              ELSE
                  AchievedConv = MAX(AchievedConv,ABS((Runoff - Runoff_P) / Runoff))
              END IF
          END DO
          IF (AchievedConv .LT. RootZone%SolverData%Tolerance) THEN
              AchievedConv = 0.0
              EXIT
          END IF

      END DO

    END ASSOCIATE
               
  END SUBROUTINE RootZone_v50_Simulate




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
  ! --- COMPUTE REGIONAL DEMAND
  ! -------------------------------------------------------------
  FUNCTION RegionalDemand(NRegions,RootZone,LUIndex) RESULT(RDemand)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RDemand(NRegions+1)
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RDemand(1:NRegions) = RootZone%AgRootZone%SubregionalDemand
          ELSE
              RDemand = 0.0
              RETURN
          END IF
   
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RDemand(1:NRegions) = RootZone%UrbanRootZone%Demand
          ELSE
              RDemand = 0.0
              RETURN
          END IF
      
      !Otherwise
      CASE DEFAULT
        RDemand = 0.0
        RETURN
      
    END SELECT
    
    !Model-wide demand
    RDemand(NRegions+1) = SUM(RDemand(1:NRegions))

  END FUNCTION RegionalDemand
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RAW (BEFORE RETURN FLOW IS ADDED) AGRICULTURAL DEMAND
  ! -------------------------------------------------------------
  FUNCTION RegionalAgRawDemand(NRegions,RootZone) RESULT(RDemandRaw)
    INTEGER,INTENT(IN)                 :: NRegions
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RDemandRaw(NRegions+1)
    
    IF (RootZone%Flags%lAg_Defined) THEN
        RDemandRaw(1:NRegions) = SUM(RootZone%AgRootZone%AgData%DemandRaw , DIM=1)
        RDemandRaw(NRegions+1) = SUM(RDemandRaw(1:NRegions))
    ELSE
        RDemandRaw = 0.0
    END IF
    
  END FUNCTION RegionalAgRawDemand


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ETAW
  ! -------------------------------------------------------------
  FUNCTION RegionalETAW(NRegions,RootZone) RESULT(RETAW)
    INTEGER,INTENT(IN)                 :: NRegions
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RETAW(NRegions+1)
    
    IF (RootZone%Flags%lAg_Defined) THEN
        RETAW(1:NRegions) = SUM(RootZone%AgRootZone%AgData%ETAW * RootZone%AgRootZone%AgData%Area , DIM=1)
        RETAW(NRegions+1) = SUM(RETAW(1:NRegions))
    ELSE
        RETAW = 0.0
    END IF

  END FUNCTION RegionalETAW


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ETP
  ! -------------------------------------------------------------
  FUNCTION RegionalETP(NRegions,RootZone) RESULT(RETP)
    INTEGER,INTENT(IN)                 :: NRegions
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RETP(NRegions+1)
    
    IF (RootZone%Flags%lAg_Defined) THEN
        RETP(1:NRegions) = SUM(RootZone%AgRootZone%AgData%ETP * RootZone%AgRootZone%AgData%Area , DIM=1)
        RETP(NRegions+1) = SUM(RETP(1:NRegions))
    ELSE
        RETP = 0.0
    END IF

  END FUNCTION RegionalETP


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ETOth
  ! -------------------------------------------------------------
  FUNCTION RegionalETOth(NRegions,RootZone) RESULT(RETOth)
    INTEGER,INTENT(IN)                 :: NRegions
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RETOth(NRegions+1)
    
    IF (RootZone%Flags%lAg_Defined) THEN
        RETOth(1:NRegions) = SUM(RootZone%AgRootZone%AgData%ETOth * RootZone%AgRootZone%AgData%Area, DIM=1)
        RETOth(NRegions+1) = SUM(RETOth(1:NRegions))
    ELSE
        RETOth = 0.0
    END IF

  END FUNCTION RegionalETOth


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RE-USE
  ! -------------------------------------------------------------
  FUNCTION RegionalReuse(NRegions,RootZone,LUIndex) RESULT(RReuse)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RReuse(NRegions+1)
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
        IF (RootZone%Flags%lAg_Defined) THEN
            RReuse(1:NRegions) = SUM(RootZone%AgRootZone%AgData%Reuse * RootZone%AgRootZone%AgData%Area, DIM=1)
        ELSE
            RReuse = 0.0
            RETURN
        END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RReuse(1:NRegions) = SUM(RootZone%UrbanRootZone%UrbData%Reuse * RootZone%UrbanRootZone%UrbData%Area , DIM=1)
          ELSE
              RReuse = 0.0
              RETURN
          END IF
        
      !Otherwise
      CASE DEFAULT
          RReuse = 0.0
          RETURN
      
    END SELECT
        
    !Model-wide reuse
    RReuse(NRegions+1) = SUM(RReuse(1:NRegions))

  END FUNCTION RegionalReuse
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL PRECIP
  ! -------------------------------------------------------------
  FUNCTION RegionalPrecip(NRegions,RootZone,LUIndex) RESULT(RPrecip)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RPrecip(NRegions+1)
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RPrecip(1:NRegions) = SUM(RootZone%SoilRegionPrecip * RootZone%AgRootZone%AgData%Area , DIM=1)
          ELSE
              RPrecip = 0.0
              RETURN
          END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RPrecip(1:NRegions) = SUM(RootZone%SoilRegionPrecip * RootZone%UrbanRootZone%UrbData%Area , DIM=1)
          ELSE 
              RPrecip = 0.0
              RETURN
          END IF
      
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
          IF (RootZone%Flags%lNVRV_Defined) THEN
              RPrecip(1:NRegions) = SUM(RootZone%SoilRegionPrecip * (RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%Area), DIM=1)
          ELSE
              RPrecip = 0.0
              RETURN
          END IF
    END SELECT
    
    !Model-wide precip
    RPrecip(NRegions+1) = SUM(RPrecip(1:NRegions))

  END FUNCTION RegionalPrecip
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL GENERIC MOISTURE INFLOW
  ! -------------------------------------------------------------
  FUNCTION RegionalGenericMoistInflow(NRegions,RootZone,LUIndex) RESULT(RGenericMoist)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RGenericMoist(NRegions+1)
    
    !Local variables
    INTEGER :: indxSoil,indxRegion
    
    !Initialize
    RGenericMoist = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (.NOT. RootZone%Flags%lAg_Defined) RETURN
          ASSOCIATE (prGenericMoisture => RootZone%GenericMoistureData%rGenericMoisture , &
                     pRootDepth        => RootZone%AgRootZone%AvgCrop%RootDepth         , &
                     pAgData           => RootZone%AgRootZone%AgData                    )
              DO indxRegion=1,NRegions
                  DO indxSoil=1,RootZone%NSoils
                      RGenericMoist(indxRegion) = RGenericMoist(indxRegion) + (prGenericMoisture(indxSoil,indxRegion) * pRootDepth(indxRegion) - pAgData(indxSoil,indxRegion)%GMExcess) * pAgData(indxSoil,indxRegion)%Area
                  END DO
              END DO
          END ASSOCIATE
        
      !Urban
      CASE (iLandUse_Urban)
          IF (.NOT. RootZone%Flags%lUrban_Defined) RETURN
          ASSOCIATE (prGenericMoisture => RootZone%GenericMoistureData%rGenericMoisture , &
                     pRootDepth        => RootZone%UrbanRootZone%RootDepth              , &
                     pPerviousFrac     => RootZone%UrbanRootZone%PerviousFrac           , &
                     pUrbData          => RootZone%UrbanRootZone%UrbData                )
              DO indxRegion=1,NRegions
                  DO indxSoil=1,RootZone%NSoils
                      RGenericMoist(indxRegion) = RGenericMoist(indxRegion) + (prGenericMoisture(indxSoil,indxRegion) * pRootDepth - pUrbData(indxSoil,indxRegion)%GMExcess) * pUrbData(indxSoil,indxRegion)%Area * pPerviousFrac(indxRegion)
                  END DO
              END DO
          END ASSOCIATE
        
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
          IF (.NOT. RootZone%Flags%lNVRV_Defined) RETURN
          ASSOCIATE (prGenericMoisture => RootZone%GenericMoistureData%rGenericMoisture , &
                     pRootDepth_NV     => RootZone%NVRVRootZone%RootDepth_Native        , &
                     pRootDepth_RV     => RootZone%NVRVRootZone%RootDepth_Riparian      , &
                     pNativeVeg        => RootZone%NVRVRootZone%NativeVeg               , &
                     pRiparianVeg      => RootZone%NVRVRootZone%RiparianVeg             )
              DO indxRegion=1,NRegions
                  DO indxSoil=1,RootZone%NSoils
                      RGenericMoist(indxRegion) = RGenericMoist(indxRegion)                                                                                                                                   &
                                                + (prGenericMoisture(indxSoil,indxRegion) * pRootDepth_NV - pNativeVeg(indxSoil,indxRegion)%GMExcess)   * pNativeVeg(indxSoil,indxRegion)%Area   &
                                                + (prGenericMoisture(indxSoil,indxRegion) * pRootDepth_RV - pRiparianVeg(indxSoil,indxRegion)%GMExcess) * pRiparianVeg(indxSoil,indxRegion)%Area                          
                  END DO
              END DO
          END ASSOCIATE
        
    END SELECT
    
    !Accumulate generic moisture inflow for the entire model area
    RGenericMoist(NRegions+1) = SUM(RGenericMoist(1:NRegions))

  END FUNCTION RegionalGenericMoistInflow
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL SOIL MISTURE CHANGE DUE TO LAND USE CHANGE
  ! -------------------------------------------------------------
  FUNCTION RegionalSoilMChange(NRegions,RootZone,LUIndex) RESULT(RSoilMCh)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: Nregions,LUIndex
    REAL(8)                            :: RSoilMCh(NRegions+1)
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RSoilmCh(1:NRegions) = SUM(RootZone%AgRootZone%AgData%SoilMCh, DIM=1)
          ELSE
              RSoilmCh = 0.0
              RETURN
          END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RSoilmCh(1:NRegions) = SUM(RootZone%UrbanRootZone%UrbData%SoilMCh , DIM=1)
          ELSE
              RSoilmCh = 0.0
              RETURN
          END IF
      
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
          IF (RootZone%Flags%lNVRV_Defined) THEN
              RSoilmCh(1:NRegions) = SUM(RootZone%NVRVRootZone%NativeVeg%SoilMCh + RootZone%NVRVRootZone%RiparianVeg%SoilMCh, DIM=1)
          ELSE
              RSoilmCh = 0.0
              RETURN
          END IF
      
    END SELECT
    
    !Model-wide soil moisture change due to land use change
    RSoilMCh(NRegions+1) = SUM(RSoilMCh(1:NRegions))

  END FUNCTION RegionalSoilMChange
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ACTUAL ET
  ! -------------------------------------------------------------
  FUNCTION RegionalETa(NRegions,RootZone,LUIndex) RESULT(RETa)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RETa(NRegions+1)
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RETa(1:NRegions) = SUM(RootZone%AgRootZone%AgData%ETa * RootZone%AgRootZone%AgData%Area, DIM=1)
          ELSE
              RETa = 0.0
              RETURN
          END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RETa(1:NRegions) = SUM(RootZone%UrbanRootZone%UrbData%ETa * RootZone%UrbanRootZone%UrbData%Area, DIM=1)
          ELSE
              RETa = 0.0
              RETURN
          END IF
      
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
          IF (RootZone%Flags%lNVRV_Defined) THEN
              RETa(1:NRegions) = SUM(RootZone%NVRVRootZone%NativeVeg%ETa * RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%ETa * RootZone%NVRVRootZone%RiparianVeg%Area , DIM=1)
          ELSE
              RETa = 0.0
              RETURN
          END IF
      
    END SELECT
    
    !Model-wide actual ET
    RETa(NRegions+1) = SUM(RETa(1:NRegions))

  END FUNCTION RegionalETa
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL PERCOLATION
  ! -------------------------------------------------------------
  FUNCTION RegionalPerc(AppGrid,RootZone,LUIndex) RESULT(RPerc)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RPerc(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: indx,iElem,iSoil,iRegion,NRegions
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RPerc(1:NRegions) = SUM((RootZone%AgRootZone%AgData%Perc + RootZone%AgRootZone%AgData%PercCh) * RootZone%AgRootZone%AgData%Area , DIM=1)
          ELSE
              RPerc = 0.0
              RETURN
          END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RPerc(1:NRegions) = SUM((RootZone%UrbanRootZone%UrbData%Perc + RootZone%UrbanRootZone%UrbData%PercCh) * RootZone%UrbanRootZone%UrbData%Area , DIM=1)
              !Add surface runoff to perc if surface runoff goes to groundwater
              DO indx=1,SIZE(RootZone%UrbanRootZone%ElemToGW)
                  iElem = RootZone%UrbanRootZone%ElemToGW(indx)
                  IF (RootZone%UrbanRootZone%ElementalArea(iElem) .EQ. 0.0) CYCLE
                  iSoil          = RootZone%ElemSoilType(iElem)
                  iRegion        = AppGrid%AppElement(iElem)%Subregion
                  RPerc(iRegion) = RPerc(iRegion) + (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Runoff + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%ReturnFlow) * RootZone%UrbanRootZone%ElementalArea(iElem)
              END DO
          ELSE
              RPerc = 0.0
              RETURN
          END IF
      
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
          IF (RootZone%Flags%lNVRV_Defined) THEN
              RPerc(1:NRegions) = SUM( (RootZone%NVRVRootZone%NativeVeg%Perc   + RootZone%NVRVRootZone%NativeVeg%PercCh) * RootZone%NVRVRootZone%NativeVeg%Area             &
                                      +(RootZone%NVRVRootZone%RiparianVeg%Perc + RootZone%NVRVRootZone%RiparianVeg%PercCh) * RootZone%NVRVRootZone%RiparianVeg%Area  , DIM=1)
          ELSE
              RPerc = 0.0
              RETURN
          END IF
      
    END SELECT
    
    !Model-wide percolation
    RPerc(NRegions+1) = SUM(RPerc(1:NRegions))

  END FUNCTION RegionalPerc
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL INFILTRATION
  ! -------------------------------------------------------------
  FUNCTION RegionalInfiltration(AppGrid,RootZone,LUIndex) RESULT(RInfilt)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RInfilt(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: indx,iElem,iSoil,iRegion,NRegions
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RInfilt(1:NRegions) = SUM((RootZone%AgRootZone%AgData%PrecipInfilt + RootZone%AgRootZone%AgData%IrigInfilt) *  RootZone%AgRootZone%AgData%Area , DIM=1)
          ELSE
              RInfilt = 0.0
              RETURN
          END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RInfilt(1:NRegions) = SUM((RootZone%UrbanRootZone%UrbData%PrecipInfilt + RootZone%UrbanRootZone%UrbData%IrigInfilt) * RootZone%UrbanRootZone%UrbData%Area , DIM=1)
              !Add surface runoff to infiltration if surface runoff goes to groundwater
              DO indx=1,SIZE(RootZone%UrbanRootZone%ElemToGW)
                  iElem = RootZone%UrbanRootZone%ElemToGW(indx)
                  IF (RootZone%UrbanRootZone%ElementalArea(iElem) .EQ. 0.0) CYCLE
                  iSoil            = RootZone%ElemSoilType(iElem)
                  iRegion          = AppGrid%AppElement(iElem)%Subregion
                  RInfilt(iRegion) = RInfilt(iRegion) + (RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Runoff + RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%ReturnFlow) * RootZone%UrbanRootZone%ElementalArea(iElem)
              END DO
          ELSE
              RInfilt = 0.0
              RETURN
          END IF
      
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
          IF (RootZone%Flags%lNVRV_Defined) THEN
              RInfilt(1:NRegions) = SUM(RootZone%NVRVRootZone%NativeVeg%PrecipInfilt * RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%PrecipInfilt * RootZone%NVRVRootZone%RiparianVeg%Area , DIM=1)
          ELSE
              RInfilt = 0.0
              RETURN
          END IF
      
    END SELECT
    
    !Model-wide infiltration
    RInfilt(NRegions+1) = SUM(RInfilt(1:NRegions))

  END FUNCTION RegionalInfiltration
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL LAND USE AREAS
  ! -------------------------------------------------------------
  FUNCTION RegionalLUArea(NRegions,RootZone,LUIndex) RESULT(RLUArea)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RLUArea(NRegions+1)
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
        IF (RootZone%Flags%lAg_Defined) THEN
            RLUArea(1:NRegions) = RootZone%AgRootZone%SubregionalArea
        ELSE
            RLUArea = 0.0
            RETURN
        END IF
    
      !Urban
      CASE (iLandUse_Urban)
        IF (RootZone%Flags%lUrban_Defined) THEN
            RLUArea(1:NRegions) = RootZone%UrbanRootZone%SubregionalArea
        ELSE
            RLUArea = 0.0
            RETURN
        END IF
      
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
        IF (RootZone%Flags%lNVRV_Defined) THEN
            RLUArea(1:NRegions) = RootZone%NVRVRootZone%SubregionalArea_NV +  RootZone%NVRVRootZone%SubregionalArea_RV
        ELSE
            RLUArea = 0.0
            RETURN
        END IF
      
    END SELECT
    
    !Entire model area
    RLUArea(NRegions+1) = SUM(RLUArea(1:NRegions))

  END FUNCTION RegionalLUArea
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL LAND USE AREAS
  ! -------------------------------------------------------------
  FUNCTION RegionalLUArea_ForSingleRegion(iRegion,RootZone,LUIndex) RESULT(Area)
    INTEGER,INTENT(IN)                 :: iRegion,LUIndex
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: Area
    
    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
        IF (RootZone%Flags%lAg_Defined) THEN
            Area = RootZone%AgRootZone%SubregionalArea(iRegion)
        ELSE
            Area = 0.0
        END IF
    
      !Urban
      CASE (iLandUse_Urban)
        IF (RootZone%Flags%lUrban_Defined) THEN
            Area = RootZone%UrbanRootZone%SubregionalArea(iRegion)
        ELSE
            Area = 0.0
        END IF
      
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
        IF (RootZone%Flags%lNVRV_Defined) THEN
            Area = RootZone%NVRVRootZone%SubregionalArea_NV(iRegion) + RootZone%NVRVRootZone%SubregionalArea_RV(iRegion)
        ELSE
            Area = 0.0
        END IF
      
    END SELECT
    
  END FUNCTION RegionalLUArea_ForSingleRegion
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RUNOFF 
  ! -------------------------------------------------------------
  FUNCTION RegionalRunoff(AppGrid,RootZone,LUIndex) RESULT(RRunoff)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RRunoff(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegions,indx,iElem,iSoil,iRegion
    
    !Initialize
    NRegions = AppGrid%NSubregions

    SELECT CASE (LUIndex)
    
      !Ag lands
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RRunoff(1:NRegions) = SUM(RootZone%AgRootZone%AgData%Runoff * RootZone%AgRootZone%AgData%Area , DIM=1)
          ELSE
              RRunoff = 0.0
              RETURN
          END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RRunoff(1:NRegions) = SUM(RootZone%UrbanRootZone%UrbData%Runoff * RootZone%UrbanRootZone%UrbData%Area, DIM=1)
              !Substract runoff that becomes gw recharge
              DO indx=1,SIZE(RootZone%UrbanRootZone%ElemToGW)
                  iElem = RootZone%UrbanRootZone%ElemToGW(indx)
                  IF (RootZone%UrbanRootZone%ElementalArea(iElem) .EQ. 0.0) CYCLE
                  iSoil            = RootZone%ElemSoilType(iElem)
                  iRegion          = AppGrid%AppElement(iElem)%Subregion
                  RRunoff(iRegion) = RRunoff(iRegion) - RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%Runoff * RootZone%UrbanRootZone%ElementalArea(iElem)
              END DO
          ELSE
              RRunoff = 0.0
              RETURN
          END IF
          
      !Native and riparian vegetation
      CASE (iLandUse_NVRV)
          IF (RootZone%Flags%lNVRV_Defined) THEN
              RRunoff(1:NRegions) = SUM(RootZone%NVRVRootZone%NativeVeg%Runoff * RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%Runoff * RootZone%NVRVRootZone%RiparianVeg%Area  ,  DIM=1)
          ELSE
              RRunoff = 0.0
              RETURN
          END IF
      
    END SELECT
          
    !Accumulate to entire domain
    RRunoff(NRegions+1) = SUM(RRunoff(1:NRegions))
    
  END FUNCTION RegionalRunoff


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RETURN FLOW 
  ! -------------------------------------------------------------
  FUNCTION RegionalReturn(AppGrid,RootZone,LUIndex) RESULT(RReturn)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RReturn(AppGrid%NSubregions+1)

    !Local variables
    INTEGER :: iElem,indx,iSoil,iRegion,NRegions
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    SELECT CASE (LUIndex)
    
      !Ag land
      CASE (iLandUse_Ag)
          IF (RootZone%Flags%lAg_Defined) THEN
              RReturn(1:NRegions) = SUM(RootZone%AgRootZone%AgData%ReturnFlow * RootZone%AgRootZone%AgData%Area, DIM=1)
          ELSE
              RReturn = 0.0
              RETURN
          END IF
    
      !Urban
      CASE (iLandUse_Urban)
          IF (RootZone%Flags%lUrban_Defined) THEN
              RReturn(1:NRegions) = SUM(RootZone%UrbanRootZone%UrbData%ReturnFlow * RootZone%UrbanRootZone%UrbData%Area, DIM=1)
              !Subtract return flow that becomes recharge to groundwater
              DO indx=1,SIZE(RootZone%UrbanRootZone%ElemToGW)
                  iElem = RootZone%UrbanRootZone%ElemToGW(indx)
                  IF (RootZone%UrbanRootZone%ElementalArea(iElem) .EQ. 0.0) CYCLE
                  iSoil            = RootZone%ElemSoilType(iElem)
                  iRegion          = AppGrid%AppElement(iElem)%Subregion
                  RReturn(iRegion) = RReturn(iRegion) - RootZone%UrbanRootZone%UrbData(iSoil,iRegion)%ReturnFlow * RootZone%UrbanRootZone%ElementalArea(iElem)
              END DO
          ELSE
              RReturn = 0.0
              RETURN
          END IF
      
      !Otherwise
      CASE DEFAULT
          RReturn = 0.0
          RETURN
        
    END SELECT
          
    !Accumulate to entire domain
    RReturn(NRegions+1) = SUM(RReturn(1:NRegions))
    
  END FUNCTION RegionalReturn


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL PUMPING THAT IS DELIVERED TO MEET DEMAND
  ! -------------------------------------------------------------
  FUNCTION RegionalPumping(NRegions,RootZone,LUIndex) RESULT(RPump)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RPump(NRegions+1)
    
    SELECT CASE (LUIndex)
      
      !Ag
      CASE (iLandUse_Ag)
       RPump(1:NRegions) = RootZone%WaterSupply%Pumping_Ag
      
      !Urban
      CASE (iLandUse_Urban)
        RPump(1:NRegions) = RootZone%WaterSupply%Pumping_Urb
        
      !Otherwise
      CASE DEFAULT
        RPump(1:NRegions) = 0.0
        
    END SELECT
    
    !Accumulate to entire domain
    RPump(NRegions+1) = SUM(RPump(1:NRegions))
        
  END FUNCTION RegionalPumping
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL DELIVERIES THAT IS DELIVERED TO MEET DEMAND 
  ! -------------------------------------------------------------
  FUNCTION RegionalDeliveries(NRegions,RootZone,LUIndex) RESULT(RDeli)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8)                            :: RDeli(NRegions+1)
    
    SELECT CASE (LUIndex)
      
      !Ag lands
      CASE (iLandUse_Ag)
        RDeli(1:NRegions) = RootZone%WaterSupply%Diversion_Ag
      
      !Urban
      CASE (iLandUse_Urban)
        RDeli(1:NRegions) = RootZone%WaterSupply%Diversion_Urb
        
      !Otherwise
      CASE DEFAULT
        RDeli(1:NRegions) = 0.0
        
    END SELECT
    
    !Accumulate to entire domain
    RDeli(NRegions+1) = SUM(RDeli(1:NRegions))
    
  END FUNCTION RegionalDeliveries


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL MOISTURE STORAGE
  ! -------------------------------------------------------------
  FUNCTION RegionalMoistStorage(NSubregions,RootZone) RESULT(RSoilM)
    INTEGER,INTENT(IN)      :: NSubregions
    TYPE(RootZone_v50_Type) :: RootZone
    REAL(8)                 :: RSoilM(NSubregions+1,NGroupLandUse)
    
    !Local variables
    INTEGER :: indxSoil,indxRegion
    
    !Initialize
    RSoilM = 0.0
    
    ASSOCIATE (pFlags        => RootZone%Flags                      , &
               pAg           => RootZone%AgRootZone%AgData          , &
               pUrban        => RootZone%UrbanRootZone%UrbData      , &
               pPerviousFrac => RootZone%UrbanRootZone%PerviousFrac , &
               pNV           => RootZone%NVRVRootZone%NativeVeg     , &
               pRV           => RootZone%NVRVRootZone%RiparianVeg   ) 
        
      IF (pFlags%lAg_Defined) THEN
          RSoilM(1:NSubregions,iLandUse_Ag) = SUM((pAg%SoilM_Precip + pAg%SoilM_AW + pAg%SoilM_Oth) * pAg%Area , DIM=1)
      ELSE
          RSoilM(:,iLandUse_Ag) = 0.0
      END IF
      
      IF (pFlags%lUrban_Defined) THEN
          DO indxRegion=1,NSubregions
              DO indxSoil=1,RootZone%NSoils
                  RSoilM(indxRegion,iLandUse_Urban) = RSoilM(indxRegion,iLandUse_Urban)  & 
                                                    + (pUrban(indxSoil,indxRegion)%SoilM_Precip + pUrban(indxSoil,indxRegion)%SoilM_AW + pUrban(indxSoil,indxRegion)%SoilM_Oth) * pUrban(indxSoil,indxRegion)%Area * pPerviousFrac(indxRegion)
              END DO
          END DO
      ELSE
          RSoilM(:,iLandUse_Urban) = 0.0
      END IF
      
      IF (pFlags%lNVRV_Defined) THEN
          RSoilM(1:NSubregions,iLandUse_NVRV) = SUM((pNV%SoilM_Precip + pNV%SoilM_AW + pNV%SoilM_Oth) * pNV%Area + (pRV%SoilM_Precip + pRV%SoilM_AW + pRV%SoilM_Oth) * pRV%Area , DIM=1)
      ELSE
          RSoilM(:,iLandUse_NVRV) = 0.0
      END IF

    END ASSOCIATE 
    
    !Moisture storage for the entire domain
    RSoilM(NSubregions+1,:) = SUM(RSoilM(1:NSubregions,:) , DIM=1)
    
  END FUNCTION RegionalMoistStorage
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL POTENTIAL ET
  ! -------------------------------------------------------------
  SUBROUTINE RegionalETPot(NRegions,RootZone,LUIndex,RETp)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NRegions,LUIndex
    REAL(8),INTENT(OUT)                :: RETp(NRegions+1)
    
    SELECT CASE(LUIndex)
        !Agricultural lands
        CASE (iLandUse_Ag)
            IF (RootZone%Flags%lAg_Defined) THEN
                RETp(1:NRegions) = RootZone%AgRootZone%RegionETPot
                RETp(NRegions+1) = SUM(RETp(1:NRegions))
            ELSE
                RETp = 0.0
                RETURN
            END IF

        !Urban
        CASE (iLandUse_Urban)
            IF (RootZone%Flags%lUrban_Defined) THEN
                RETp(1:NRegions) = RootZone%UrbanRootZone%RegionETPot
                RETp(NRegions+1) = SUM(RETp(1:NRegions))
            ELSE
                RETp = 0.0
                RETURN
            END IF
            
        !Native and riparian
        CASE (iLandUse_NVRV)
            IF (RootZone%Flags%lNVRV_Defined) THEN
                RETp(1:NRegions) = RootZone%NVRVRootZone%RegionETPot_NV + RootZone%NVRVRootZone%RegionETPot_RV
                RETp(NRegions+1) = SUM(RETp(1:NRegions))
            ELSE
                RETp = 0.0
                RETURN
            END IF
    END SELECT
        
  END SUBROUTINE RegionalETPot
  

  ! -------------------------------------------------------------
  ! --- ZERO OUT WATER SUPPLY
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_ZeroSupply(RootZone)
    CLASS(RootZone_v50_Type) :: RootZone
    
    RootZone%WaterSupply%Diversion_Ag  = 0.0
    RootZone%WaterSupply%Diversion_Urb = 0.0
    RootZone%WaterSupply%Pumping_Ag    = 0.0
    RootZone%WaterSupply%Pumping_Urb   = 0.0
    RootZone%WaterSupply%UpstrmRunoff  = 0.0
    
  END SUBROUTINE RootZone_v50_ZeroSupply


  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF ROOT ZONE RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_ConvertTimeUnit(RootZone,NewUnit)
    CLASS(RootZone_v50_Type)    :: RootZone
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert
    Factor                              = TimeIntervalConversion(NewUnit,RootZone%VarTimeUnit)
    RootZone%VarTimeUnit                = NewUnit
    RootZone%SubregionSoilsData%HydCond = RootZone%SubregionSoilsData%HydCond * Factor
        
  END SUBROUTINE RootZone_v50_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- ZERO OUT SOIL OISTURE VARIABLES RELATED TO LAND USE AREA CHANGE
  ! -------------------------------------------------------------
  SUBROUTINE ZeroRedistributedMoist(RootZone)
    TYPE(RootZone_v50_Type) :: RootZone
    
    RootZone%AgRootZone%AgData%SoilmCh        = 0.0
    RootZone%UrbanRootZone%UrbData%SoilmCh    = 0.0
    RootZone%NVRVRootZone%NativeVeg%SoilmCh   = 0.0
    RootZone%NVRVRootZone%RiparianVeg%SoilmCh = 0.0
    
    RootZone%AgRootZone%AgData%PercCh         = 0.0
    RootZone%UrbanRootZone%UrbData%PercCh     = 0.0
    RootZone%NVRVRootZone%NativeVeg%PercCh    = 0.0
    RootZone%NVRVRootZone%RiparianVeg%PercCh  = 0.0
    
  END SUBROUTINE ZeroRedistributedMoist
  
  
  
  ! -------------------------------------------------------------
  ! --- REDISTRIBUTE MOISTURE BASED ON AREA CHANGES
  ! -------------------------------------------------------------
  SUBROUTINE RedistributeMoist(NElements,NRegions,ElemRegion,RootZone)
    INTEGER,INTENT(IN)      :: NElements,NRegions,ElemRegion(NElements)
    TYPE(RootZone_v50_Type) :: RootZone

    !Local variables
    INTEGER                                              :: indxElem,indxLU,iRegion,iSoil,indxRegion,indxSoil
    REAL(8)                                              :: ratio(3),SOILM_INT_Precip,SOILM_INT_AW,SOILM_INT_Oth,TotalReduc,  &
                                                            Factor_Precip,Factor_AW,Factor_Oth,TotalPorosity,PerviousFrac,    &
                                                            rValue,rTotalPorosityD,Area_Out
    REAL(8),DIMENSION(NLands+1)                          :: AreaExpand,AreaReduced,Area,Area_P,RootDepth,SM_Precip,SM_AW,SM_Oth
    REAL(8),DIMENSION(NLands+1,NElements)                :: SoilMCh_Precip,SoilMCh_AW,SoilMCh_Oth,SoilM_Precip,SoilM_AW,SoilM_Oth,PercCh
    REAL(8),DIMENSION(NLands+1,RootZone%NSoils,NRegions) :: SoilM_Precip_SoilRegion,SoilM_AW_SoilRegion,SoilM_Oth_SoilRegion,PercCh_SoilRegion , &
                                                            SoilMCh_Precip_SoilRegion,SoilMCh_AW_SoilRegion,SoilMCh_Oth_SoilRegion
    LOGICAL                                              :: lAg_Defined,lUrban_Defined,lNVRV_Defined
    INTEGER,PARAMETER                                    :: indxAg        = 1 , &
                                                            indxUrban_In  = 2 , &
                                                            indxUrban_Out = 3 , &
                                                            indxNV        = 4 , &
                                                            indxRV        = 5 , &
                                                            NLandsExt     = NLands + 1
    
    !Initialize
    lAg_Defined               = RootZone%Flags%lAg_Defined
    lUrban_Defined            = RootZone%Flags%lUrban_Defined
    lNVRV_Defined             = RootZone%Flags%lNVRV_Defined
    Area                      = 0.0
    Area_P                    = 0.0
    SM_Precip                 = 0.0
    SM_AW                     = 0.0
    SM_Oth                    = 0.0
    SoilM_Precip_SoilRegion   = 0.0
    SoilM_AW_SoilRegion       = 0.0
    SoilM_Oth_SoilRegion      = 0.0
    PercCh_SoilRegion         = 0.0
    SoilMCh_Precip_SoilRegion = 0.0
    SoilMCh_AW_SoilRegion     = 0.0
    SoilMCh_Oth_SoilRegion    = 0.0
    AreaExpand                = 0.0
    AreaReduced               = 0.0
    RootDepth                 = 0.0   ;   IF (lUrban_Defined) THEN
                                              RootDepth(indxUrban_In)  = RootZone%UrbanRootZone%RootDepth
                                              RootDepth(indxUrban_Out) = RootZone%UrbanRootZone%RootDepth
                                          END IF
                                          IF (lNVRV_Defined) THEN
                                              RootDepth(indxNV) = RootZone%NVRVRootZone%RootDepth_Native
                                              RootDepth(indxRV) = RootZone%NVRVRootZone%RootDepth_Riparian
                                          END IF
    
    ASSOCIATE (pAg        => RootZone%AgRootZone%AgData          , &
               pUrban     => RootZone%UrbanRootZone%UrbData      , &
               pNV        => RootZone%NVRVRootZone%NativeVeg     , &
               pRV        => RootZone%NVRVRootZone%RiparianVeg   , &
               pSoilsData => RootZone%SubregionSoilsData         )

        !Compute the details of land use area expansion and contraction to compute new soil moisture contents
        DO indxElem=1,NElements
            
            !Initialize
            iSoil         = RootZone%ElemSoilType(indxElem)
            iRegion       = ElemRegion(indxElem)
            TotalPorosity = pSoilsData(iSoil,iRegion)%TotalPorosity
            IF (lUrban_Defined) PerviousFrac = RootZone%UrbanRootZone%PerviousFrac(iRegion)
            IF (lAg_Defined) THEN
                RootDepth(indxAg) = RootZone%AgRootZone%AvgCrop(iRegion)%RootDepth
                Area(indxAg)      = RootZone%AgRootZone%ElementalArea(indxElem)
                Area_P(indxAg)    = RootZone%AgRootZone%ElementalArea_P(indxElem)
                SM_Precip(indxAg) = pAg(iSoil,iRegion)%SoilM_Precip_P
                SM_AW(indxAg)     = pAg(iSoil,iRegion)%SoilM_AW_P
                SM_Oth(indxAg)    = pAg(iSoil,iRegion)%SoilM_Oth_P
            END IF 
            IF (lUrban_Defined) THEN
                Area_P(indxUrban_Out)    = RootZone%UrbanRootZone%ElementalArea_P(indxElem) * PerviousFrac 
                Area_P(indxUrban_In)     = RootZone%UrbanRootZone%ElementalArea_P(indxElem) - Area_P(indxUrban_Out) 
                Area(indxUrban_Out)      = RootZone%UrbanRootZone%ElementalArea(indxElem) * PerviousFrac   
                Area(indxUrban_In)       = RootZone%UrbanRootZone%ElementalArea_P(indxElem) - Area(indxUrban_Out)   
                SM_Precip(indxUrban_Out) = pUrban(iSoil,iRegion)%SoilM_Precip_P
                SM_AW(indxUrban_Out)     = pUrban(iSoil,iRegion)%SoilM_AW_P
                SM_Oth(indxUrban_Out)    = pUrban(iSoil,iRegion)%SoilM_Oth_P
            END IF
            IF (lNVRV_Defined) THEN
                Area_P(indxNV)    = RootZone%NVRVRootZone%ElementalArea_P_NV(indxElem)
                Area_P(indxRV)    = RootZone%NVRVRootZone%ElementalArea_P_RV(indxElem)
                Area(indxNV)      = RootZone%NVRVRootZone%ElementalArea_NV(indxElem)
                Area(indxRV)      = RootZone%NVRVRootZone%ElementalArea_RV(indxElem)
                SM_Precip(indxNV) = pNV(iSoil,iRegion)%SoilM_Precip_P
                SM_Precip(indxRV) = pRV(iSoil,iRegion)%SoilM_Precip_P
                SM_AW(indxNV)     = pNV(iSoil,iRegion)%SoilM_AW_P        !Although there is no irrigation for native and riparian veg, they
                SM_AW(indxRV)     = pRV(iSoil,iRegion)%SoilM_AW_P        !  can inherit moisture due to irrigtaion when their area expands into ag and urban lands
                SM_Oth(indxNV)    = pNV(iSoil,iRegion)%SoilM_Oth_P
                SM_Oth(indxRV)    = pRV(iSoil,iRegion)%SoilM_Oth_P
            END IF 

            !Changes in element land use areas
            AreaExpand    = MAX(Area-Area_P,0.0)                        !Expansion in each land use area
            AreaReduced   = MAX(Area_P-Area,0.0)                        !Reduction in each land use area
            TotalReduc    = SUM(AreaReduced)                            !Total area reduction
            IF (TotalReduc .EQ. 0.0) THEN
                SoilM_Precip(:,indxElem)                 = SM_Precip
                SoilM_AW(:,indxElem)                     = SM_AW
                SoilM_Oth(:,indxElem)                    = SM_Oth
                SoilMCh_Precip(:,indxElem)               = 0.0
                SoilMCh_AW(:,indxElem)                   = 0.0
                SoilMCH_Oth(:,indxElem)                  = 0.0
                PercCh(:,indxElem)                       = 0.0
                SoilM_Precip_SoilRegion(:,iSoil,iRegion) = SoilM_Precip_SoilRegion(:,iSoil,iRegion) + SoilM_Precip(:,indxElem) * Area
                SoilM_AW_SoilRegion(:,iSoil,iRegion)     = SoilM_AW_SoilRegion(:,iSoil,iRegion)     + SoilM_AW(:,indxElem)     * Area
                SoilM_Oth_SoilRegion(:,iSoil,iRegion)    = SoilM_Oth_SoilRegion(:,iSoil,iRegion)    + SoilM_Oth(:,indxElem)    * Area
                CYCLE
            END IF
            Factor_Precip = SUM(SM_Precip/TotalReduc*AreaReduced)       !Scaling factor for moisture due to precip
            Factor_AW     = SUM(SM_AW/TotalReduc*AreaReduced)           !Scaling factor for moisture due to irrigation
            Factor_Oth    = SUM(SM_Oth/TotalReduc*AreaReduced)          !Scaling factor for moisture due to other generic sources

            !Compute new soil moisture volumes under new areas 
            DO indxLU=1,NLandsExt
                
                !Area did not expand; moisture content in the land use is the same
                IF (AreaExpand(indxLU) .EQ. 0.0) THEN
                  SOILM_INT_Precip = SM_Precip(indxLU) * Area(indxLU)
                  SOILM_INT_AW     = SM_AW(indxLU)     * Area(indxLU)
                  SOILM_INT_Oth    = SM_Oth(indxLU)    * Area(indxLU)

                !Area expanded; the moisture content will change to assimilate the new moisture 
                ELSE
                  SOILM_INT_Precip = SM_Precip(indxLU)*Area_P(indxLU) + Factor_Precip*AreaExpand(indxLU)
                  SOILM_INT_AW     = SM_AW(indxLU)    *Area_P(indxLU) + Factor_AW    *AreaExpand(indxLU)
                  SOILM_INT_Oth    = SM_Oth(indxLU)   *Area_P(indxLU) + Factor_Oth   *AreaExpand(indxLU)
                END IF
                
                !Volumetric change in soil moisture
                SoilMCh_Precip(indxLU,indxElem) = SOILM_INT_Precip - SM_Precip(indxLU)*Area_P(indxLU)
                SoilMCh_AW(indxLU,indxElem)     = SOILM_INT_AW     - SM_AW(indxLU)    *Area_P(indxLU)
                SoilMCh_Oth(indxLU,indxElem)    = SOILM_INT_Oth    - SM_Oth(indxLU)   *Area_P(indxLU)

                !Modify moisture content in the land use area
                IF (Area(indxLU) .GT. 0.0) THEN
                    SoilM_Precip(indxLU,indxElem) = SOILM_INT_Precip/Area(indxLU)
                    SoilM_AW(indxLU,indxElem)     = SOILM_INT_AW/Area(indxLU)
                    SoilM_Oth(indxLU,indxElem)    = SOILM_INT_Oth/Area(indxLU)
                    !If modified moisture exceeds total porosity turn the excess moisture to perc
                    PercCh(indxLU,indxElem)       = MAX(0.0  ,  SoilM_Precip(indxLU,indxElem) + SoilM_AW(indxLU,indxElem) + SoilM_Oth(indxLU,indxElem) - TotalPorosity*RootDepth(indxLU))
                    rValue                        = SoilM_Precip(indxLU,indxElem) + SoilM_AW(indxLU,indxElem) + SoilM_Oth(indxLU,indxElem)
                    IF (rValue .GT. 0.0) THEN
                        ratio                         = [SoilM_Precip(indxLU,indxElem) , SoilM_AW(indxLU,indxElem) , SoilM_Oth(indxLU,indxElem)]
                        CALL NormalizeArray(ratio)
                        SoilM_Precip(indxLU,indxElem) = SoilM_Precip(indxLU,indxElem) - PercCh(indxLU,indxElem) * ratio(1)
                        SoilM_AW(indxLU,indxElem)     = SoilM_AW(indxLU,indxElem)     - PercCh(indxLU,indxElem) * ratio(2)
                        SoilM_Oth(indxLU,indxElem)    = SoilM_Oth(indxLU,indxElem)    - PercCh(indxLU,indxElem) * ratio(3)
                     END IF
                ELSE
                     SoilM_Precip(indxLU,indxElem) = 0.0
                     SoilM_AW(indxLU,indxElem)     = 0.0
                     SoilM_Oth(indxLU,indxElem)    = 0.0
                     PercCh(indxLU,indxElem)       = 0.0
                END IF
                
                !Accumulate volumetric rates to soil-region combination
                SoilM_Precip_SoilRegion(indxLU,iSoil,iRegion)   = SoilM_Precip_SoilRegion(indxLU,iSoil,iRegion)   + SoilM_Precip(indxLU,indxElem) * Area(indxLU)
                SoilM_AW_SoilRegion(indxLU,iSoil,iRegion)       = SoilM_AW_SoilRegion(indxLU,iSoil,iRegion)       + SoilM_AW(indxLU,indxElem)     * Area(indxLU)
                SoilM_Oth_SoilRegion(indxLU,iSoil,iRegion)      = SoilM_Oth_SoilRegion(indxLU,iSoil,iRegion)      + SoilM_Oth(indxLU,indxElem)    * Area(indxLU)
                PercCh_SoilRegion(indxLU,iSoil,iRegion)         = PercCh_SoilRegion(indxLU,iSoil,iRegion)         + PercCh(indxLU,indxElem)   * Area(indxLU)
                SoilMCh_Precip_SoilRegion(indxLU,iSoil,iRegion) = SoilMCh_Precip_SoilRegion(indxLU,iSoil,iRegion) + SoilMCh_Precip(indxLU,indxElem)
                SoilMCh_AW_SoilRegion(indxLU,iSoil,iRegion)     = SoilMCh_AW_SoilRegion(indxLU,iSoil,iRegion)     + SoilMCh_AW(indxLU,indxElem)
                SoilMCh_Oth_SoilRegion(indxLU,iSoil,iRegion)    = SoilMCh_Oth_SoilRegion(indxLU,iSoil,iRegion)    + SoilMCh_Oth(indxLU,indxElem)
            END DO            
        END DO

        !Store data in persisting arrays
        !Also, update SoilM_P to reflect changes in the moisture due to land expansion/shrinking
        !*Note: SoilM_P is now the moisture at the beginning of time step after redistribution.
        !       It is necessary to be careful when computing the reporting variables
        IF (lAg_Defined) THEN
            DO indxRegion=1,NRegions
                DO indxSoil=1,RootZone%NSoils
                    IF (pAg(indxSoil,indxRegion)%Area .GT. 0.0) THEN
                        pAg(indxSoil,indxRegion)%SoilM_Precip = SoilM_Precip_SoilRegion(indxAg,indxSoil,indxRegion) / pAg(indxSoil,indxRegion)%Area 
                        pAg(indxSoil,indxRegion)%SoilM_AW     = SoilM_AW_SoilRegion(indxAg,indxSoil,indxRegion)     / pAg(indxSoil,indxRegion)%Area
                        pAg(indxSoil,indxRegion)%SoilM_Oth    = SoilM_Oth_SoilRegion(indxAg,indxSoil,indxRegion)    / pAg(indxSoil,indxRegion)%Area
                        pAg(indxSoil,indxRegion)%SoilMCh      = SoilMCh_Precip_SoilRegion(indxAg,indxSoil,indxRegion) + SoilMCh_AW_SoilRegion(indxAg,indxSoil,indxRegion) + SoilMCh_Oth_SoilRegion(indxAg,indxSoil,indxRegion)
                        pAg(indxSoil,indxRegion)%PercCh       = PercCh_SoilRegion(indxAg,indxSoil,indxRegion)       / pAg(indxSoil,indxRegion)%Area
                    ELSE
                        pAg(indxSoil,indxRegion)%SoilM_Precip = 0.0
                        pAg(indxSoil,indxRegion)%SoilM_AW     = 0.0
                        pAg(indxSoil,indxRegion)%SoilM_Oth    = 0.0
                        pAg(indxSoil,indxRegion)%SoilMCh      = 0.0
                        pAg(indxSoil,indxRegion)%PercCh       = 0.0
                    END IF
                END DO
            END DO
        END IF
        
        IF (lUrban_Defined) THEN
            !Consolidate urban values to urban outdoors
            DO indxRegion=1,NRegions
                PerviousFrac = RootZone%UrbanRootZone%PerviousFrac(indxRegion)
                DO indxSoil=1,RootZone%NSoils
                    IF (pUrban(indxSoil,indxRegion)%Area .GT. 0.0) THEN
                        Area_Out                                 = pUrban(indxSoil,indxRegion)%Area * PerviousFrac
                        pUrban(indxSoil,indxRegion)%SoilM_Precip = (SoilM_Precip_SoilRegion(indxUrban_Out,indxSoil,indxRegion) + SoilM_Precip_SoilRegion(indxUrban_In,indxSoil,indxRegion) * (1d0/PerviousFrac - 1d0)) / Area_Out
                        pUrban(indxSoil,indxRegion)%SoilM_AW     = (SoilM_AW_SoilRegion(indxUrban_Out,indxSoil,indxRegion)     + SoilM_AW_SoilRegion(indxUrban_In,indxSoil,indxRegion) * (1d0/PerviousFrac - 1d0))     / Area_Out 
                        pUrban(indxSoil,indxRegion)%SoilM_Oth    = (SoilM_Oth_SoilRegion(indxUrban_Out,indxSoil,indxRegion)    + SoilM_Oth_SoilRegion(indxUrban_In,indxSoil,indxRegion) * (1d0/PerviousFrac - 1d0))    / Area_Out
                        pUrban(indxSoil,indxRegion)%SoilMCh      = SoilmCh_Precip_SoilRegion(indxUrban_Out,indxSoil,indxRegion) + SoilmCh_Precip_SoilRegion(indxUrban_In,indxSoil,indxRegion) + SoilmCh_AW_SoilRegion(indxUrban_Out,indxSoil,indxRegion) + SoilmCh_AW_SOilRegion(indxUrban_In,indxSoil,indxRegion) + SoilmCh_Oth_SOilRegion(indxUrban_Out,indxSoil,indxRegion) + SoilmCh_Oth_SoilRegion(indxUrban_In,indxSoil,indxRegion) 
                        pUrban(indxSoil,indxRegion)%PercCh       = (PercCh_SoilRegion(indxUrban_Out,indxSoil,indxRegion)       + PercCh_SoilRegion(indxUrban_In,indxSoil,indxRegion) * (1d0/PerviousFrac - 1d0)) / Area_Out
                        
                        !Make sure soil moisture is not above total porosity
                        rTotalPorosityD = pSoilsData(indxSoil,indxRegion)%TotalPorosity * RootDepth(indxUrban_Out)
                        rValue          = pUrban(indxSoil,indxRegion)%SoilM_Precip + pUrban(indxSoil,indxRegion)%SoilM_AW + pUrban(indxSoil,indxRegion)%SoilM_Oth
                        IF (rValue .LE. rTotalPorosityD) CYCLE
                        pUrban(indxSoil,indxRegion)%PercCh       = pUrban(indxSoil,indxRegion)%PercCh + (rValue - rTotalPorosityD) * Area_Out
                        ratio                                    = [pUrban(indxSoil,indxRegion)%SoilM_Precip , pUrban(indxSoil,indxRegion)%SoilM_AW , pUrban(indxSoil,indxRegion)%SoilM_Oth]
                        CALL NormalizeArray(ratio)
                        pUrban(indxSoil,indxRegion)%SoilM_Precip = rTotalPorosityD * ratio(1)
                        pUrban(indxSoil,indxRegion)%SoilM_AW     = rTotalPorosityD * ratio(2)
                        pUrban(indxSoil,indxRegion)%SoilM_Oth    = rTotalPorosityD * ratio(3)
                    ELSE
                        pUrban(indxSoil,indxRegion)%SoilM_Precip = 0.0
                        pUrban(indxSoil,indxRegion)%SoilM_AW     = 0.0
                        pUrban(indxSoil,indxRegion)%SoilM_Oth    = 0.0
                        pUrban(indxSoil,indxRegion)%SoilMCh      = 0.0
                        pUrban(indxSoil,indxRegion)%PercCh       = 0.0
                    END IF
                END DO
            END DO
        END IF

        IF (lNVRV_Defined) THEN
            DO indxRegion=1,NRegions
                DO indxSoil=1,RootZone%NSoils
                    IF (pNV(indxSoil,indxRegion)%Area .GT. 0.0) THEN
                        pNV(indxSoil,indxRegion)%SoilM_Precip = SoilM_Precip_SoilRegion(indxNV,indxSoil,indxRegion) / pNV(indxSoil,indxRegion)%Area
                        pNV(indxSoil,indxRegion)%SoilM_AW     = SoilM_AW_SoilRegion(indxNV,indxSoil,indxRegion) / pNV(indxSoil,indxRegion)%Area
                        pNV(indxSoil,indxRegion)%SoilM_Oth    = SoilM_Oth_SoilRegion(indxNV,indxSoil,indxRegion) / pNV(indxSoil,indxRegion)%Area
                        pNV(indxSoil,indxRegion)%SoilMCh      = SoilmCh_Precip_SoilRegion(indxNV,indxSoil,indxRegion) + SoilmCh_AW_SoilRegion(indxNV,indxSoil,indxRegion) + SoilMCh_Oth_SoilRegion(indxNV,indxSoil,indxRegion)
                        pNV(indxSoil,indxRegion)%PercCh       = PercCh_SoilRegion(indxNV,indxSoil,indxRegion) / pNV(indxSoil,indxRegion)%Area
                    ELSE
                        pNV(indxSoil,indxRegion)%SoilM_Precip = 0.0
                        pNV(indxSoil,indxRegion)%SoilM_AW     = 0.0
                        pNV(indxSoil,indxRegion)%SoilM_Oth    = 0.0
                        pNV(indxSoil,indxRegion)%SoilMCh      = 0.0
                        pNV(indxSoil,indxRegion)%PercCh       = 0.0
                    END IF 
                    
                    IF (pRV(indxSoil,indxRegion)%Area .GT. 0.0) THEN
                        pRV(indxSoil,indxRegion)%SoilM_Precip = SoilM_Precip_SoilRegion(indxRV,indxSoil,indxRegion) / pRV(indxSoil,indxRegion)%Area
                        pRV(indxSoil,indxRegion)%SoilM_AW     = SoilM_AW_SoilRegion(indxRV,indxSoil,indxRegion) / pRV(indxSoil,indxRegion)%Area
                        pRV(indxSoil,indxRegion)%SoilM_Oth    = SoilM_Oth_SoilRegion(indxRV,indxSoil,indxRegion) / pRV(indxSoil,indxRegion)%Area
                        pRV(indxSoil,indxRegion)%SoilMCh      = SoilmCh_Precip_SoilRegion(indxRV,indxSoil,indxRegion) + SoilMCh_AW_SoilRegion(indxRV,indxSoil,indxRegion) + SoilMCh_Oth_SoilRegion(indxRV,indxSoil,indxRegion)
                        pRV(indxSoil,indxRegion)%PercCh       = PercCh_SoilRegion(indxRV,indxSoil,indxRegion) / pRV(indxSoil,indxRegion)%Area
                    ELSE
                        pRV(indxSoil,indxRegion)%SoilM_Precip = 0.0
                        pRV(indxSoil,indxRegion)%SoilM_AW     = 0.0
                        pRV(indxSoil,indxRegion)%SoilM_Oth    = 0.0
                        pRV(indxSoil,indxRegion)%SoilMCh      = 0.0
                        pRV(indxSoil,indxRegion)%PercCh       = 0.0
                    END IF
                END DO
            END DO
        END IF
        
        !Advance the soil moisture in time
        CALL AdvanceStateLocal(RootZone , lAdvanceArea=.FALSE.)  !Do not advance the area in time; we are only updating the soil moisture beacuse it changed

    END ASSOCIATE

END SUBROUTINE RedistributeMoist
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF ROOT ZONE IN TIME WITH A CHOICE TO ADVANCE AREA AS WELL
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceStateLocal(RootZone,lAdvanceArea)
    TYPE(RootZone_v50_Type) :: RootZone
    LOGICAL,INTENT(IN)      :: lAdvanceArea
    
    RootZone%RSoilM_P = RootZone%RSoilM

    IF (RootZone%Flags%lAg_Defined) THEN
        RootZone%AgRootZone%AgData%SoilM_Precip_P = RootZone%AgRootZone%AgData%SoilM_Precip
        RootZone%AgRootZone%AgData%SoilM_AW_P     = RootZone%AgRootZone%AgData%SoilM_AW
        RootZone%AgRootZone%AgData%SoilM_Oth_P    = RootZone%AgRootZone%AgData%SoilM_Oth
        IF (lAdvanceArea) CALL RootZone%AgRootZone%AdvanceAreas()
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        RootZone%UrbanRootZone%UrbData%SoilM_Precip_P = RootZone%UrbanRootZone%UrbData%SoilM_Precip
        RootZone%UrbanRootZone%UrbData%SoilM_AW_P     = RootZone%UrbanRootZone%UrbData%SoilM_AW
        RootZone%UrbanRootZone%UrbData%SoilM_Oth_P    = RootZone%UrbanRootZone%UrbData%SoilM_Oth
        IF (lAdvanceArea) CALL RootZone%UrbanRootZone%AdvanceAreas()
    END IF
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P    = RootZone%NVRVRootZone%NativeVeg%SoilM_Precip
        RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P        = RootZone%NVRVRootZone%NativeVeg%SoilM_AW
        RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P       = RootZone%NVRVRootZone%NativeVeg%SoilM_Oth
        RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P  = RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip
        RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P      = RootZone%NVRVRootZone%RiparianVeg%SoilM_AW
        RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P     = RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth
        IF (lAdvanceArea) CALL RootZone%NVRVRootZone%AdvanceAreas()
    END IF
    
  END SUBROUTINE AdvanceStateLocal
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF ROOT ZONE IN TIME INCLUDING AREA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_AdvanceState(RootZone)
    CLASS(RootZone_v50_Type) :: RootZone
    
    !Store previous moisture stoarge in special arrays before they may be updated
    IF (RootZone%Flags%lAg_Defined) THEN
        RootZone%AgRootZone%AgData%SoilM_Precip_P_BeforeUpdate = RootZone%AgRootZone%AgData%SoilM_Precip
        RootZone%AgRootZone%AgData%SoilM_AW_P_BeforeUpdate     = RootZone%AgRootZone%AgData%SoilM_AW
        RootZone%AgRootZone%AgData%SoilM_Oth_P_BeforeUpdate    = RootZone%AgRootZone%AgData%SoilM_Oth
    END IF
    IF (RootZone%Flags%lUrban_Defined) THEN
        RootZone%UrbanRootZone%UrbData%SoilM_Precip_P_BeforeUpdate = RootZone%UrbanRootZone%UrbData%SoilM_Precip
        RootZone%UrbanRootZone%UrbData%SoilM_AW_P_BeforeUpdate     = RootZone%UrbanRootZone%UrbData%SoilM_AW
        RootZone%UrbanRootZone%UrbData%SoilM_Oth_P_BeforeUpdate    = RootZone%UrbanRootZone%UrbData%SoilM_Oth
    END IF
    IF (RootZone%Flags%lNVRV_Defined) THEN
        RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P_BeforeUpdate   = RootZone%NVRVRootZone%NativeVeg%SoilM_Precip
        RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P_BeforeUpdate       = RootZone%NVRVRootZone%NativeVeg%SoilM_AW
        RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P_BeforeUpdate      = RootZone%NVRVRootZone%NativeVeg%SoilM_Oth
        RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P_BeforeUpdate = RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip
        RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P_BeforeUpdate     = RootZone%NVRVRootZone%RiparianVeg%SoilM_AW
        RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P_BeforeUpdate    = RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth
    END IF
    
    CALL AdvanceStateLocal(RootZone , lAdvanceArea=.TRUE.)
    
  END SUBROUTINE RootZone_v50_AdvanceState

  
  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL PERCOLATION
  ! -------------------------------------------------------------
  FUNCTION RootZone_v50_RegionalPerc(RootZone,AppGrid) RESULT(RPERC)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: RPERC(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegions

    !Initialize
    NRegions = AppGrid%NSubregions
    RPERC    = 0.0
    
    ASSOCIATE (pFlags => RootZone%Flags)
               
      !Ag lands
      IF (pFlags%lAg_Defined) RPERC(1:NRegions) = SUM(RootZone%AgRootZone%AgData%Perc * RootZone%AgRootZone%AgData%Area , DIM=1)    

      !Urban
      IF (pFlags%lUrban_Defined) RPERC(1:NRegions) = RPERC(1:NRegions) + SUM(RootZone%UrbanRootZone%UrbData%Perc * RootZone%UrbanRootZone%UrbData%Area , DIM=1)    
      
      !Native and riparian veg
      IF (pFlags%lNVRV_Defined) RPERC(1:NRegions) = RPERC(1:NRegions) + SUM(RootZone%NVRVRootZone%NativeVeg%Perc * RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%Perc * RootZone%NVRVRootZone%RiparianVeg%Area  ,  DIM=1)   
    
      !Compute perc for the entire model area
      RPERC(NRegions+1) = SUM(RPERC(1:NRegions))
      
    END ASSOCIATE

  END FUNCTION RootZone_v50_RegionalPerc
  

  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_RegionalReturnFlow_Ag(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: RReturnFlow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegions

    !Initialize
    NRegions = AppGrid%NSubregions
    
    !Ag lands
    IF (RootZone%Flags%lAg_Defined) THEN
        RReturnFlow(1:NRegions) = SUM(RootZone%AgRootZone%AgData%ReturnFlow * RootZone%AgRootZone%AgData%Area , DIM=1) 
        RReturnFlow(NRegions+1) = SUM(RReturnFlow(1:NRegions))
    ELSE
        RReturnFlow = 0.0
    END IF

  END SUBROUTINE RootZone_v50_RegionalReturnFlow_Ag
  

  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v50_RegionalReturnFlow_Urb(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: RReturnFlow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegions

    !Initialize
    NRegions = AppGrid%NSubregions
    
    !Urban
    IF (RootZone%Flags%lUrban_Defined) THEN
        RReturnFlow(1:NRegions) = SUM(RootZone%UrbanRootZone%UrbData%ReturnFlow * RootZone%UrbanRootZone%UrbData%Area , DIM=1)    
        RReturnFlow(NRegions+1) = SUM(RReturnFlow(1:NRegions))
    ELSE
        RReturnFlow = 0.0
    END IF
      
  END SUBROUTINE RootZone_v50_RegionalReturnFlow_Urb
  

  ! -------------------------------------------------------------
  ! --- PROCESS LAND USE AREA
  ! -------------------------------------------------------------
  SUBROUTINE ProcessLandUseAreas(AppGrid,TimeStep,RootZone,iStat)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(RootZone_v50_Type)       :: RootZone
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'ProcessLandUseAreas'
    INTEGER                      :: indxElem
    REAL(8)                      :: LUArea_Elem(NLands,AppGrid%NElements)
    
    !Initialize
    iStat = 0
    
    !Zero out the variables that hold information regarding soil moisture change due to land area change
    CALL ZeroRedistributedMoist(RootZone) 
    
    !Return if new data is not read
    IF (.NOT. RootZone_v50_IsLandUseUpdated(RootZone)) RETURN
        
    !Check for errors and process data
    LUArea_Elem = 0.0
    DO indxElem=1,AppGrid%NElements

        !If lake element, zero out areas and cycle
        IF (RootZone%Flags%lLakeElems(indxElem)) THEN
            LUArea_Elem(:,indxElem) = 0.0
            CYCLE
        END IF
          
        !Store data in the work array
        IF (RootZone%Flags%lAg_Defined)    LUArea_Elem(1,indxElem) = RootZone%AgRootZone%ElementalArea(indxElem)
        IF (RootZone%Flags%lUrban_Defined) LUArea_Elem(2,indxElem) = RootZone%UrbanRootZone%ElementalArea(indxElem)
        IF (RootZone%Flags%lNVRV_Defined) THEN
            LUArea_Elem(3,indxElem) = RootZone%NVRVRootZone%ElementalArea_NV(indxElem)
            LUArea_Elem(4,indxElem) = RootZone%NVRVRootZone%ElementalArea_RV(indxElem)
        END IF
        
        !Check for zero area
        IF (ALL(LUArea_Elem(:,indxElem) .LE. 0.0)) THEN
            CALL SetLastMessage('Total land use area is zero at element ' // TRIM(IntToText(indxElem)) // '!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF

        !Normalize the land use areas
        CALL NormalizeArray(LUArea_Elem(:,indxElem))

        !Compute final land use areas 
        LUArea_Elem(:,indxElem) = LUArea_Elem(:,indxElem) * AppGrid%AppElement(indxElem)%Area
        
    END DO

    ASSOCIATE (pAg    => RootZone%AgRootZone     , &
               pUrban => RootZone%UrbanRootZone  , &
               pNVRV  => RootZone%NVRVRootZone   )
               
        !Store the areas in persistent objects
        IF (RootZone%Flags%lAg_Defined) CALL pAg%SetAreas(AppGrid,RootZone%ElemSoilType,LUArea_Elem(1,:))
        IF (RootZone%Flags%lUrban_Defined) CALL pUrban%SetAreas(AppGrid,RootZone%ElemSoilType,LUArea_Elem(2,:)) 
        IF (RootZone%Flags%lNVRV_Defined) CALL pNVRV%SetAreas(AppGrid,RootZone%ElemSoilType,LUArea_Elem(3,:),LUArea_Elem(4,:))
      
        !If first time step, advance land use areas in time to equal land use areas at previous and current time steps
        IF (TimeStep%CurrentTimeStep .EQ. 1) THEN
            CALL pAg%AdvanceAreas()
            CALL pUrban%AdvanceAreas() 
            CALL pNVRV%AdvanceAreas() 
 
        !Otherwise redistribute soil moisture based on the decreased/increased land use area
        ELSE
            CALL RedistributeMoist(AppGrid%NElements,AppGrid%NSubregions,AppGrid%AppElement%Subregion,RootZone)
        END IF
      
    END ASSOCIATE
               
  END SUBROUTINE ProcessLandUseAreas
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF PROPER TIME-SERIES DATA COLUMNS ARE POINTED TO
  ! -------------------------------------------------------------
  SUBROUTINE CheckTSDataPointers(RootZone,Precip,ET,iStat)
    CLASS(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(PrecipitationType),INTENT(IN)  :: Precip
    TYPE(ETType),INTENT(IN)             :: ET
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'CheckTSDataPointers'
    INTEGER                      :: iElem(1),iETColMax,indxRegion,indxCrop,iReturnFlowCol(1),iReuseCol(1)
    LOGICAL                      :: lAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    iStat          = 0
    lAg_Defined    = RootZone%Flags%lAg_Defined
    lUrban_Defined = RootZone%Flags%lUrban_Defined
    lNVRV_Defined  = RootZone%Flags%lNVRV_Defined
    
    !Check precipitation
    IF (Precip%GetNDataColumns() .LT. MAXVAL(RootZone%ElemPrecipData%iColPrecip)) THEN
      iElem = MAXLOC(RootZone%ElemPrecipData%iColPrecip)
      MessageArray(1) = 'Precipitation data column for element '//TRIM(IntToText(iElem(1)))//' in the root zone component'
      MessageArray(2) = 'is greater than the available data columns in the Precipitation Data file!'
      CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
      iStat = -1
      RETURN
    END IF
    
    !Check ET, return flow and resue fractions
    iETColMax = ET%GetNDataColumns()
    DO indxRegion=1,SIZE(RootZone%SubregionSoilsData , DIM=2)
        !Ag lands
        IF (lAg_Defined) THEN
            DO indxCrop=1,RootZone%AgRootZone%NCrops
                IF (iETColMax .LT. RootZone%AgRootZone%iColETcCrop(indxCrop,indxRegion)) THEN
                    MessageArray(1) = 'Evapotranspiration data column for agriculrtural crop '//TRIM(IntToText(indxCrop))//' at subregion '//TRIM(IntToText(indxRegion))
                    MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                    CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
            iReturnFlowCol(1) = RootZone%AgRootZone%iColReturnFrac(indxRegion)
            iReuseCol(1)      = RootZone%AgRootZone%iColReuseFrac(indxRegion)
            CALL RootZone%ReturnFracFile%CheckColNum('Return flow fractions data file (referenced by agricultural main data file for subregion '//TRIM(IntToText(indxRegion))//')',iReturnFlowCol,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL RootZone%ReuseFracFile%CheckColNum('Re-use fractions data file (referenced by agricultural main data file for subregion '//TRIM(IntToText(indxRegion))//')',iReuseCol,.TRUE.,iStat)             ;  IF (iStat .EQ. -1) RETURN
        END IF
        
        !Urban
        IF (lUrban_Defined) THEN
            IF (iETColMax .LT. RootZone%UrbanRootZone%UrbData(1,indxRegion)%iColETc) THEN
                MessageArray(1) = 'Evapotranspiration data column for subregion '//TRIM(IntToText(indxRegion))//' at urban lands '
                MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF 
            iReturnFlowCol(1) = RootZone%UrbanRootZone%iColReturnFrac(indxRegion)
            iReuseCol(1)      = RootZone%UrbanRootZone%iColReuseFrac(indxRegion)
            CALL RootZone%ReturnFracFile%CheckColNum('Return flow fractions data file (referenced by urban main data file for subregion '//TRIM(IntToText(indxRegion))//')',iReturnFlowCol,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL RootZone%ReuseFracFile%CheckColNum('Re-use fractions data file (referenced by urban main data file for subregion '//TRIM(IntToText(indxRegion))//')',iReuseCol,.TRUE.,iStat)             ;  IF (iStat .EQ. -1) RETURN
        END IF
    
        IF (lNVRV_Defined) THEN
           !Native 
            IF (iETColMax .LT. RootZone%NVRVRootZone%NativeVeg(1,indxRegion)%iColETc) THEN
                MessageArray(1) = 'Evapotranspiration data column for subregion '//TRIM(IntToText(indxRegion))//' at native vegetation lands '
                MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF   
        
            !Riparian  
            IF (iETColMax .LT. RootZone%NVRVRootZone%RiparianVeg(1,indxRegion)%iColETc) THEN
                MessageArray(1) = 'Evapotranspiration data column for subregion '//TRIM(IntToText(indxRegion))//' at riparian vegetation lands '
                MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF   
    END DO
    
  END SUBROUTINE CheckTSDataPointers


  ! -------------------------------------------------------------
  ! --- COMPUTE TOTAL FLOWS DESTINED TO OTHER SUBREGIONS
  ! -------------------------------------------------------------
  SUBROUTINE FlowToSubregions(Flow,LUArea,ElemsToSubregions,ElemSoilType,AppGrid,ToSubregions)
    REAL(8),INTENT(IN)                         :: Flow(:,:)                    !In unit rate for each (soil,subregion)
    REAL(8),INTENT(IN)                         :: LUArea(:)                    !Land use area at each element
    TYPE(ElemSurfaceFlowToDestType),INTENT(IN) :: ElemsToSubregions(:)         !Element flow to subregion connection list
    INTEGER,INTENT(IN)                         :: ElemSoilType(:)              !Given for each element
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    REAL(8)                                    :: ToSubregions(:)              !In units of volume for each (subregion)
    
    !Local variables
    INTEGER :: iSoil,iElem,indx,iRegion,iDestRegion
    
    DO indx=1,SIZE(ElemsToSubregions)
        iElem       = ElemsToSubregions(indx)%iElement
        iSoil       = ElemSoilType(iElem)
        iRegion     = AppGrid%AppElement(iElem)%Subregion
        IF (Flow(iSoil,iRegion) .EQ. 0.0) CYCLE
        iDestRegion               = ElemsToSubregions(indx)%iDestID
        ToSubregions(iDestRegion) = ToSubregions(iDestRegion) + Flow(iSoil,iRegion) * LUArea(iElem)
    END DO
        
  END SUBROUTINE FlowToSubregions


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL POTENTIAL ET 
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRegionalETPot(ETData,NSubregions,iColETc,Area,ETPot)
    TYPE(ETType),INTENT(IN) :: ETData
    INTEGER,INTENT(IN)      :: NSubregions,iColETc(:)
    REAL(8),INTENT(IN)      :: Area(:)
    REAL(8),INTENT(OUT)     :: ETPot(:)
    
    !Local variables
    REAL(8) :: ETc(NSubregions)
    
    !Compile
    ETc   = ETData%GetValues(iColETc)
    ETPot = ETc * Area
    
  END SUBROUTINE ComputeRegionalETPot

  
  ! -------------------------------------------------------------
  ! --- COMPUTE UPSTREAM RUNOFF INTO SPECIED LAND USE AT EACH SUBREGION
  ! -------------------------------------------------------------
  FUNCTION UpstrmRunoffToLandUse(RootZone,AppGrid,iLandUseType) RESULT(UpstrmRunoff)
    TYPE(RootZone_v50_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    INTEGER,INTENT(IN)                 :: iLandUseType
    REAL(8)                            :: UpstrmRunoff(AppGrid%NSubregions)
    
    !Local variables
    INTEGER :: indxRegion
    REAL(8) :: AreaFrac(3,AppGrid%NSubregions)
    
    !Compute area fractions to make sure that they are calculated properly in case ther are lakes
    DO indxRegion=1,AppGrid%NSubregions
        AreaFrac(:,indxRegion) = [RootZone%AgRootZone%SubregionalArea(indxRegion) , RootZone%UrbanRootZone%SubregionalArea(indxRegion) , RootZone%NVRVRootZone%SubregionalArea_NV(indxRegion)+RootZone%NVRVRootZone%SubregionalArea_RV(indxRegion)]
        CALL NormalizeArray(AreaFrac(:,indxRegion))
    END DO
    
    SELECT CASE (iLandUseType)
        !Ag lands
        CASE (iLandUse_Ag)
            IF (RootZone%Flags%lAg_Defined) THEN
                UpstrmRunoff = RootZone%WaterSupply%UpstrmRunoff * AreaFrac(1,:)
            ELSE
                UpstrmRunoff = 0.0
            END IF
            
        !Urban lands
        CASE (iLandUse_Urban)
            IF (RootZone%Flags%lUrban_Defined) THEN
                UpstrmRunoff = RootZone%WaterSupply%UpstrmRunoff * AreaFrac(2,:)
            ELSE
                UpstrmRunoff = 0.0
            END IF
           
        !Native and riparian vegetation lands
        CASE (iLandUse_NVRV)
            IF (RootZone%Flags%lNVRV_Defined) THEN
                UpstrmRunoff = RootZone%WaterSupply%UpstrmRunoff * AreaFrac(3,:)
            ELSE
                UpstrmRunoff = 0.0
            END IF
    END SELECT
    
  END FUNCTION UpstrmRunoffToLandUse
    
END MODULE