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
MODULE RootZone_v411
  USE MessageLogger                  , ONLY: SetLastMessage                           , &
                                             EchoProgress                             , &
                                             MessageArray                             , &
                                             iFatal                                   
  USE GeneralUtilities               , ONLY: StripTextUntilCharacter                  , &
                                             IntToText                                , &
                                             CleanSpecialCharacters                   , &
                                             EstablishAbsolutePathFileName            , &
                                             LocateInList                             
  USE TimeSeriesUtilities            , ONLY: TimeStepType                             , &
                                             IncrementTimeStamp
  USE IOInterface                    , ONLY: GenericFileType
  USE Package_Discretization         , ONLY: AppGridType                              
  USE Package_PrecipitationET        , ONLY: PrecipitationType                        , &
                                             ETType                                   
  USE Package_Misc                   , ONLY: FlowDest_Outside                         , &
                                             FlowDest_StrmNode                        , &
                                             FlowDest_Element                         , &
                                             FlowDest_Lake                            , &
                                             FlowDest_Subregion                       , &
                                             FlowDest_GWElement                       , &
                                             iLocationType_Subregion                  , &
                                             iLocationType_Zone                       , &
                                             iAllLocationIDsListed   
  USE Package_ZBudget                , ONLY: ZBudgetType                              , &
                                             SystemDataType                           , &
                                             ZBudgetHeaderType                        , &
                                             iElemDataType                            , &
                                             ZoneListType                             , &
                                             MarkerChar                               , &
                                             AreaUnitMarker                           , &
                                             MaxLocationNameLen                       , &
                                             AR                                       , &
                                             VR                                       , &
                                             VLB                                      , &
                                             VLE                                      , &
                                             VR_lwu_PotCUAW                           , &
                                             VR_lwu_AgSupplyReq                       , &
                                             VR_lwu_AgPump                            , &
                                             VR_lwu_AgDiv                             , &
                                             VR_lwu_AgOthIn                           , &
                                             VR_lwu_AgShort         
  USE Package_UnsatZone              , ONLY: KUnsatMethodList
  USE Util_RootZone_v41              , ONLY: LWUseBudRawFile_New                      , &
                                             RootZoneBudRawFile_New                   , &
                                             cLWUseBudgetColumnTitles                 , &
                                             cRootZoneBudgetColumnTitles              , &
                                             NLWUseBudColumns                         , &
                                             NRootZoneBudColumns
  USE Class_NonPondedAgLandUseGW     , ONLY: NonPondedAgLandUse_New                  
  USE Class_PondedAgLandUseGW        , ONLY: PondedAgLandUse_New                      , &
                                             NPondedCrops                            
  USE Class_UrbanLandUseGW           , ONLY: UrbanLandUse_New                         
  USE Class_NativeRiparianLandUseGW  , ONLY: NativeRiparianLandUse_New                
  USE Class_BaseRootZone             , ONLY: FlagsType                                , &
                                             ElemSurfaceFlowToDestType                , &
                                             CompileElemSurfaceFlowToDestinationList
  USE RootZone_v41                   , ONLY: RootZone_v41_Type                        , &
                                             Flags_v41_Type                           , &
                                             CheckTSDataPointers                      , &
                                             NGroupLandUse
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
  PUBLIC :: RootZone_v411_Type                                  
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RootZone_v41_Type) :: RootZone_v411_Type
      PRIVATE
      TYPE(ZBudgetType) :: LWUZoneBudRawFile                  !Raw land and water use zone budget file
      TYPE(ZBudgetType) :: RootZoneZoneBudRawFile             !Raw root zone zone budget file
  CONTAINS
      PROCEDURE,PASS :: New                         => RootZone_v411_New
      PROCEDURE,PASS :: KillRZImplementation        => RootZone_v411_Kill
      PROCEDURE,PASS :: PrintResults                => RootZone_v411_PrintResults
      PROCEDURE,PASS :: GetVersion                  => RootZone_v411_GetVersion
      PROCEDURE,PASS :: GetNDataList_AtLocationType => RootZone_v411_GetNDataList_AtLocationType
      PROCEDURE,PASS :: GetDataList_AtLocationType  => RootZone_v411_GetDataList_AtLocationType
      PROCEDURE,PASS :: GetLocationsWithData        => RootZone_v411_GetLocationsWithData
      PROCEDURE,PASS :: GetSubDataList_AtLocation   => RootZone_v411_GetSubDataList_AtLocation 
      PROCEDURE,PASS :: GetModelData_AtLocation     => RootZone_v411_GetModelData_AtLocation 
  END TYPE RootZone_v411_Type


  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion          = 9
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion             = '4.11.0000'
  INCLUDE 'RootZone_v411_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: nData_AtZone                   = 2 , &
                                 iLWU_AtZone                    = 1 , &
                                 iRootZone_AtZone               = 2 
  CHARACTER(LEN=30),PARAMETER :: cDataList_AtZone(nData_AtZone) = ['Land and water use zone budget'  , &
                                                                   'Root zone zone budget'           ]
    

  ! -------------------------------------------------------------
  ! --- Z-BUDGET OUTPUT RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: NLWUseZBudColumns                                  = 39  , &
                                 NRootZoneZBudColumns                               = 85   
  CHARACTER(LEN=40),PARAMETER :: cLWUseZBudgetColumnTitles(NLWUseZBudColumns)       = ['Non-ponded Ag. Area'                           , &
                                                                                       'Non-ponded Potential CUAW'                     , &
                                                                                       'Non-ponded Ag. Supply Requirement'             , &
                                                                                       'Non-ponded Ag. Pumping'                        , &
                                                                                       'Non-ponded Ag. Deliveries'                     , &
                                                                                       'Non-ponded Ag. Inflow as Surface Runoff'       , &
                                                                                       'Non-ponded Ag. Shortage'                       , &
                                                                                       'Non-ponded Ag. ETAW'                           , &
                                                                                       'Non-ponded Ag. Effective Precipitation'        , &
                                                                                       'Non-ponded Ag. ET from Groundwater'            , &
                                                                                       'Non-ponded Ag. ET from Other Sources'          , &
                                                                                       'Rice Area'                                     , &
                                                                                       'Rice Potential CUAW'                           , &
                                                                                       'Rice Supply Requirement'                       , &
                                                                                       'Rice Pumping'                                  , &
                                                                                       'Rice Deliveries'                               , &
                                                                                       'Rice Inflow as Surface Runoff'                 , &
                                                                                       'Rice Shortage'                                 , &
                                                                                       'Rice ETAW'                                     , &
                                                                                       'Rice Effective Precipitation'                  , &
                                                                                       'Rice ET from Groundwater'                      , &
                                                                                       'Rice ET from Other Sources'                    , &
                                                                                       'Refuge Area'                                   , &
                                                                                       'Refuge Potential CUAW'                         , &
                                                                                       'Refuge Supply Requirement'                     , &
                                                                                       'Refuge Pumping'                                , &
                                                                                       'Refuge Deliveries'                             , &
                                                                                       'Refuge Inflow as Surface Runoff'               , &
                                                                                       'Refuge Shortage'                               , &
                                                                                       'Refuge ETAW'                                   , &
                                                                                       'Refuge Effective Precipitation'                , &
                                                                                       'Refuge ET from Groundwater'                    , &
                                                                                       'Refuge ET from Other Sources'                  , &
                                                                                       'Urban Area'                                    , &
                                                                                       'Urban Supply Requirement'                      , &
                                                                                       'Urban Pumping'                                 , &
                                                                                       'Urban Deliveries'                              , &
                                                                                       'Urban Inflow as Surface Runoff'                , &
                                                                                       'Urban Shortage'                                ]
  CHARACTER(LEN=53),PARAMETER :: cRootZoneZBudgetColumnTitles(NRootZoneZBudColumns) = ['Non-ponded Ag. Area'                                    , &
                                                                                       'Non-ponded Ag. Potential ET'                            , &
                                                                                       'Non-ponded Ag. Precipitation'                           , &
                                                                                       'Non-ponded Ag. Runoff'                                  , &
                                                                                       'Non-ponded Ag. Prime Applied Water'                     , &
                                                                                       'Non-ponded Ag. Inflow as Surface Runoff'                , &
                                                                                       'Non-ponded Ag. Reused Water'                            , &
                                                                                       'Non-ponded Ag. Net Return Flow'                         , &
                                                                                       'Non-ponded Ag. Beginning Storage (+)'                   , &
                                                                                       'Non-ponded Ag. Net Gain from Land Expansion (+)'        , &
                                                                                       'Non-ponded Ag. Infiltration (+)'                        , &
                                                                                       'Non-ponded Ag. Groundwater Inflow (+)'                  , &
                                                                                       'Non-ponded Ag. Other Inflow (+)'                        , &
                                                                                       'Non-ponded Ag. Actual ET (-)'                           , &
                                                                                       'Non-ponded Ag. Percolation (-)'                         , &
                                                                                       'Non-ponded Ag. Ending Storage (-)'                      , &
                                                                                       'Non-ponded Ag. Discrepancy (=)'                         , &
                                                                                       'Rice Area'                                              , &
                                                                                       'Rice Potential ET'                                      , &
                                                                                       'Rice Precipitation'                                     , &
                                                                                       'Rice Runoff'                                            , &
                                                                                       'Rice Prime Applied Water'                               , &
                                                                                       'Rice Inflow as Surface Runoff'                          , &
                                                                                       'Rice Reused Water'                                      , &
                                                                                       'Rice Net Return Flow'                                   , &
                                                                                       'Rice Beginning Storage (+)'                             , &
                                                                                       'Rice Net Gain from Land Expansion (+)'                  , &
                                                                                       'Rice Infiltration (+)'                                  , &
                                                                                       'Rice Groundwater Inflow (+)'                            , &
                                                                                       'Rice Other Inflow (+)'                                  , &
                                                                                       'Rice Pond Drain (-)'                                    , &
                                                                                       'Rice Actual ET (-)'                                     , &
                                                                                       'Rice Percolation (-)'                                   , &
                                                                                       'Rice Ending Storage (-)'                                , &
                                                                                       'Rice Discrepancy (=)'                                   , &
                                                                                       'Refuge Area'                                            , &
                                                                                       'Refuge Potential ET'                                    , &
                                                                                       'Refuge Precipitation'                                   , &
                                                                                       'Refuge Runoff'                                          , &
                                                                                       'Refuge Prime Applied Water'                             , &
                                                                                       'Refuge Inflow as Surface Runoff'                        , &
                                                                                       'Refuge Reused Water'                                    , &
                                                                                       'Refuge Net Return Flow'                                 , &
                                                                                       'Refuge Beginning Storage (+)'                           , &
                                                                                       'Refuge Net Gain from Land Expansion (+)'                , &
                                                                                       'Refuge Infiltration (+)'                                , &
                                                                                       'Refuge Groundwater Inflow (+)'                          , &
                                                                                       'Refuge Other Inflow (+)'                                , &
                                                                                       'Refuge Pond Drain (-)'                                  , &
                                                                                       'Refuge Actual ET (-)'                                   , &
                                                                                       'Refuge Percolation (-)'                                 , &
                                                                                       'Refuge Ending Storage (-)'                              , &
                                                                                       'Refuge Discrepancy (=)'                                 , &
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
                                                                                       'Urban Groundwater Inflow (+)'                           , &
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
                                                                                       'Native&Riparian Veg. Groundwater Inflow (+)'            , &
                                                                                       'Native&Riparian Veg. Other Inflow (+)'                  , &
                                                                                       'Native&Riparian Veg. Stream Inflow for ET (+)'          , &
                                                                                       'Native&Riparian Veg. Actual ET (-)'                     , &
                                                                                       'Native&Riparian Veg. Percolation (-)'                   , &
                                                                                       'Native&Riparian Veg. Ending Storage (-)'                , &
                                                                                       'Native&Riparian Veg. Discrepancy (=)'                   ]


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'RootZone_v411::'
  
  

    
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
  SUBROUTINE RootZone_v411_New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
    CLASS(RootZone_v411_Type)          :: RootZone
    LOGICAL,INTENT(IN)                 :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    INTEGER,INTENT(IN)                 :: NStrmNodes,NLakes,NTIME
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17)                :: ThisProcedure = ModName // 'RootZone_v411_New'
    CHARACTER(LEN=1000)                         :: ALine,NonPondedCropFile,RiceRefugeFile,UrbanDataFile,NVRVFile,AgWaterDemandFile,GenericMoistureFile
    CHARACTER                                   :: cVersionLocal*20
    REAL(8)                                     :: FACTK,FACTCN,RegionArea(AppGrid%NSubregions+1),DummyFactor(1),FACTEXDTH,rDummy(14)
    INTEGER                                     :: NElements,NRegion,ErrorCode,indxElem,iColGenericMoisture(AppGrid%NElements),iFlagETFromGW,          &
                                                   SurfaceFlowDest(AppGrid%NElements),SurfaceFlowDestType(AppGrid%NElements),nDataCols
    TYPE(GenericFileType)                       :: RootZoneParamFile
    LOGICAL                                     :: TrackTime,lElemFlowToSubregions
    CHARACTER(LEN=MaxLocationNameLen)           :: RegionNames(AppGrid%NSubregions+1)
    REAL(8),ALLOCATABLE                         :: DummyRealArray(:,:)
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
    RegionArea(1:NRegion)  = AppGrid%GetSubregionAreaForAll()
    RegionArea(NRegion+1)  = SUM(RegionArea(1:NRegion))
    RegionNames            = ''  ;  RegionNames(1:NRegion) = AppGrid%GetSubregionNames()
    RegionNames(NRegion+1) = 'ENTIRE MODEL AREA'
    
    !Allocate memory
    ALLOCATE (RootZone%ElemSoilsData(NElements)                      , &
              RootZone%HydCondPonded(NElements)                      , &
              RootZone%ElemPrecipData(NElements)                     , &
              RootZone%ElemSupply(NElements)                         , &
              RootZone%ElemDevelopedArea(NElements)                  , &
              RootZone%Ratio_ElemSupplyToRegionSupply_Ag(NElements)  , &
              RootZone%Ratio_ElemSupplyToRegionSupply_Urb(NElements) , &
              RootZone%RSoilM_P(NRegion+1,NGroupLandUse)             , &
              RootZone%RSoilM(NRegion+1,NGroupLandUse)               , &
              RootZone%Flags%lLakeElems(NElements)                   , &
              STAT=ErrorCode                                         )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for root zone soils data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Initialize lake element flag
    RootZone%Flags%lLakeElems = .FALSE.
    
    !Open file
    CALL RootZoneParamFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read away the first version number line to avoid any errors
    CALL RootZoneParamFile%ReadData(ALine,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Read solution scheme controls
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%IterMax,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(FACTCN,iStat)                         ;  IF (iStat .EQ. -1) RETURN
    
    !Read flag to see if ET from groundwater will be simulated
    CALL RootZoneParamFile%ReadData(iFlagETFromGW,iStat)  ;  IF (iStat .EQ. -1) RETURN
    SELECT CASE (iFlagETFromGW)
        CASE (0)
            RootZone%Flags%lComputeETFromGW = .FALSE.
        CASE (1)
            RootZone%Flags%lComputeETFromGW = .TRUE.
        CASE DEFAULT
            CALL SetLastMessage('Flag to simulate root water uptake from groundwater is not recognized!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT

    !Initialize related files
    !-------------------------
    
    !Non-ponded crops data file
    CALL RootZoneParamFile%ReadData(NonPondedCropFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    NonPondedCropFile = StripTextUntilCharacter(NonPondedCropFile,'/') 
    CALL CleanSpecialCharacters(NonPondedCropFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(NonPondedCropFile)),cWorkingDirectory,cAbsPathFileName)
    CALL NonPondedAgLandUse_New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,FactCN,AppGrid,TimeStep,NTIME,cVersionLocal,RootZone%NonPondedAgRootZone,iStat)
    IF (iStat .EQ. -1) RETURN
       
    !Rice/refuge data file
    CALL RootZoneParamFile%ReadData(RiceRefugeFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    RiceRefugeFile = StripTextUntilCharacter(RiceRefugeFile,'/') 
    CALL CleanSpecialCharacters(RiceRefugeFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(RiceRefugeFile)),cWorkingDirectory,cAbsPathFileName)
    CALL PondedAgLandUse_New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,FactCN,AppGrid,TimeStep,NTIME,cVersionLocal,RootZone%PondedAgRootZone,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Urban data file
    CALL RootZoneParamFile%ReadData(UrbanDataFile,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    UrbanDataFile = StripTextUntilCharacter(UrbanDataFile,'/') 
    CALL CleanSpecialCharacters(UrbanDataFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(UrbanDataFile)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbanLandUse_New(cAbsPathFileName,cWorkingDirectory,FactCN,NElements,NRegion,TrackTime,RootZone%UrbanRootZone,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Native/riparian veg. data file
    CALL RootZoneParamFile%ReadData(NVRVFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    NVRVFile = StripTextUntilCharacter(NVRVFile,'/') 
    CALL CleanSpecialCharacters(NVRVFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(NVRVFile)),cWorkingDirectory,cAbsPathFileName)
    CALL NativeRiparianLandUse_New(cAbsPathFileName,cWorkingDirectory,FactCN,NElements,NRegion,TrackTime,RootZone%NVRVRootZone,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Check if at least one type of land use is specified
    IF ( NonPondedCropFile .EQ. ''   .AND.   &
         RiceRefugeFile    .EQ. ''   .AND.   &
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
      IF (RootZone%NonPondedAgRootZone%NCrops .GT. 0) pFlags%lNonPondedAg_Defined = .TRUE.
      IF (RiceRefugeFile .NE. '')                     pFlags%lPondedAg_Defined    = .TRUE.
      IF (UrbanDataFile .NE. '')                      pFlags%lUrban_Defined       = .TRUE.
      IF (NVRVFile .NE. '')                           pFlags%lNVRV_Defined        = .TRUE.
    END ASSOCIATE
    
    !Total number of land uses
    RootZone%NLands = RootZone%NonPondedAgRootZone%NCrops + NPondedCrops + 3
    
    !Return flow data file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
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
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
            CALL SetLastMessage('Missing irrigation water re-use factors data file!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%ReuseFracFile%Init(cAbsPathFileName,'Irrigation water re-use factors file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Irrigation period data file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined) THEN
            CALL SetLastMessage('Missing irrigation period data file!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%IrigPeriodFile%Init(cAbsPathFileName,'Irrigation period data file',TrackTime,1,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Generic moisture data file
    CALL RootZoneParamFile%ReadData(GenericMoistureFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    GenericMoistureFile = StripTextUntilCharacter(GenericMoistureFile,'/') 
    CALL CleanSpecialCharacters(GenericMoistureFile)
    IF (GenericMoistureFile .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(GenericMoistureFile)),cWorkingDirectory,cAbsPathFileName)
        GenericMoistureFile = GenericMoistureFile
        RootZone%Flags%lGenericMoistureFile_Defined = .TRUE.
    END IF
    
    !Agricultural water demand file
    CALL RootZoneParamFile%ReadData(AgWaterDemandFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    AgWaterDemandFile = StripTextUntilCharacter(AgWaterDemandFile,'/') 
    CALL CleanSpecialCharacters(AgWaterDemandFile)
    IF (AgWaterDemandFile .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(AgWaterDemandFile)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%AgWaterDemandFile%Init(cAbsPathFileName,'Agricultural water supply requirement file',TrackTime,1,.TRUE.,DummyFactor,(/.TRUE./),iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        RootZone%AgWaterDemandFactor = DummyFactor(1)
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
    CALL RootZoneParamFile%ReadData(FACTEXDTH,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)
    RootZone%VarTimeUnit = ADJUSTL(StripTextUntilCharacter(ALine,'/'))

    !Backward compatibility: Check if the user entered KPonded values at all
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)  ;  ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/')) 
    READ (ALine,*,IOSTAT=ErrorCode) rDummy
    IF (ErrorCode .EQ. 0) THEN
        ALLOCATE (DummyRealArray(NElements,14))
        nDataCols = 14
    ELSE
        ALLOCATE (DummyRealArray(NElements,13))
        nDataCols = 13
    END IF
    CALL RootZoneParamFile%BackspaceFile()

    CALL RootZoneParamFile%ReadData(DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ASSOCIATE (pSoilsData  => RootZone%ElemSoilsData  , &
               pPrecipData => RootZone%ElemPrecipData )
      pSoilsData%WiltingPoint   =     DummyRealArray(:,2)      
      pSoilsData%FieldCapacity  =     DummyRealArray(:,3)
      pSoilsData%TotalPorosity  =     DummyRealArray(:,4)
      pSoilsData%Lambda         =     DummyRealArray(:,5)
      pSoilsData%HydCond        =     DummyRealArray(:,6) * FACTK * TimeStep%DeltaT
      pSoilsData%KunsatMethod   = INT(DummyRealArray(:,7))
      pSoilsData%CapillaryRise  =     DummyRealArray(:,8) * FACTEXDTH
      pPrecipData%iColPrecip    = INT(DummyRealArray(:,9))
      pPrecipData%PrecipFactor  =     DummyRealArray(:,10)
      iColGenericMoisture       = INT(DummyRealArray(:,11))
      SurfaceFlowDestType       = INT(DummyRealArray(:,12))
      SurfaceFlowDest           = INT(DummyRealArray(:,13))
      IF (nDataCols .EQ. 13) THEN
          RootZone%HydCondPonded = pSoilsData%HydCond
      ELSE
          DO indxElem=1,NElements
              IF (DummyRealArray(indxElem,14) .EQ. -1.0) THEN
                  RootZone%HydCondPonded(indxElem) = pSoilsData(indxElem)%HydCond
              ELSE
                  RootZone%HydCondPonded(indxElem) = DummyRealArray(indxElem,14) * FACTK * TimeStep%DeltaT
              END IF
          END DO
      END IF
      
      !Instantiate generic moisture data
      CALL RootZone%GenericMoistureData%New(GenericMoistureFile,1,NElements,iColGenericMoisture,TrackTime,iStat)
      IF (iStat .EQ. -1) RETURN
      
      !Check for errors
      DO indxElem=1,NElements
        ASSOCIATE (pDestType => SurfaceFlowDestType(indxElem))
          !Make sure that destination types are recognized
          IF (pDestType .NE. FlowDest_Outside    .AND.   &
              pDestType .NE. FlowDest_StrmNode   .AND.   &
              pDestType .NE. FlowDest_Element    .AND.   &
              pDestType .NE. FlowDest_Lake       .AND.   &
              pDestType .NE. FlowDest_Subregion  .AND.   &
              pDestType .NE. FlowDest_GWElement       )  THEN
              CALL SetLastMessage ('Surface flow destination type for element ' // TRIM(IntToText(indxElem)) // ' is not recognized!',iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
          
          !Make sure destination locations are modeled
          SELECT CASE (pDestType)
              CASE (FlowDest_StrmNode)
                  IF (SurfaceFlowDest(indxElem) .GT. NStrmNodes  .OR.  SurfaceFlowDest(indxElem) .LT. 1) THEN
                      CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(indxElem))//' flows into a stream node ('//TRIM(IntToText(SurfaceFlowDest(indxElem)))//') that is not modeled!',iFatal,ThisProcedure)
                      iStat = -1
                      RETURN
                  END IF
             
              CASE (FlowDest_Element)
                  IF (SurfaceFlowDest(indxElem) .GT. NElements  .OR.  SurfaceFlowDest(indxElem) .LT. 1) THEN
                      CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(indxElem))//' goes to an element ('//TRIM(IntToText(SurfaceFlowDest(indxElem)))//') that is not modeled!',iFatal,ThisProcedure)
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
             
              CASE (FlowDest_GWElement)
                  SurfaceFlowDest(indxElem) = indxElem
                  
              CASE (FlowDest_Outside)
                  SurfaceFlowDest(indxElem) = 0
                  
          END SELECT
              
        END ASSOCIATE
        
        !Method to compute Kunsat must be recognized
        IF (LocateInList(pSoilsData(indxElem)%KunsatMethod,KunsatMethodList) .LT. 1) THEN
            CALL SetLastMessage('Method to compute unsaturated hydraulic conductivity at element '//TRIM(IntToText(indxElem))//' is not recognized!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Wilting point should be less than field capacity
        IF (pSoilsData(indxElem)%WiltingPoint .GE. pSoilsData(indxElem)%FieldCapacity) THEN
            CALL SetLastMessage('At element ' // TRIM(IntToText(indxElem)) // ' wilting point is greater than or equal to field capacity!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Field capacity should be less than or equal to total porosity
        IF (pSoilsData(indxElem)%FieldCapacity .GT. pSoilsData(indxElem)%TotalPorosity) THEN
            CALL SetLastMessage('At element ' // TRIM(IntToText(indxElem)) // ' field capacity is greater than total porosity!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF

      END DO
      
      !Compile element-flow-to-outside connection list
      CALL CompileElemSurfaceFlowToDestinationList(FlowDest_Outside,SurfaceFlowDest,SurfaceFlowDestType,ElemFlowToOutside,iStat)  ;  IF (iStat .EQ. -1) RETURN
      ALLOCATE (RootZone%ElemFlowToOutside(SIZE(ElemFlowToOutside)))
      RootZone%ElemFlowToOutside = ElemFlowToOutside%iElement
      
      !Compile element-flow-to-stream-node connection list
      CALL CompileElemSurfaceFlowToDestinationList(FlowDest_StrmNode,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToStreams,iStat)  
      IF (iStat .EQ. -1) RETURN
      
      !Compile element-flow-to-lake connection list
      CALL CompileElemSurfaceFlowToDestinationList(FlowDest_Lake,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToLakes,iStat)  
      IF (iStat .EQ. -1) RETURN
      
      !Compile element-flow-to-subregion connection list
      CALL CompileElemSurfaceFlowToDestinationList(FlowDest_Subregion,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToSubregions,iStat)  
      IF (iStat .EQ. -1) RETURN

      !Compile element-flow-to-groundwater connection list
      CALL CompileElemSurfaceFlowToDestinationList(FlowDest_GWElement,SurfaceFlowDest,SurfaceFlowDestType,ElemFlowToGW,iStat)  ;  IF (iStat .EQ. -1) RETURN
      ALLOCATE (RootZone%ElemFlowToGW(SIZE(ElemFlowToGW)))
      RootZone%ElemFlowToGW = ElemFlowToGW%iElement

      !Compile element-flow-to-another-element connection list
      CALL CompileElemSurfaceFlowToDestinationList(FlowDest_Element,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToElements,iStat)  
      IF (iStat .EQ. -1) RETURN

    END ASSOCIATE
    
    !Flag to see if ag water demand will be computed or not, check for inconsistencies as well
    IF (RootZone%NonPondedAgRootZone%NCrops.GT.0  .OR.  RiceRefugeFile.NE.'') THEN
      !Check with non-ponded crops
      IF (.NOT. ALLOCATED(RootZone%NonPondedAgRootZone%iColAgDemand)) THEN
        RootZone%Flags%lComputeAgWaterDemand = .TRUE.
      ELSE
        IF (ANY(RootZone%NonPondedAgRootZone%iColAgDemand.EQ.0)) RootZone%Flags%lComputeAgWaterDemand       = .TRUE.
        IF (ANY(RootZone%NonPondedAgRootZone%iColAgDemand.GT.0)) RootZone%Flags%lReadNonPondedAgWaterDemand = .TRUE.
      END IF
      
      !Then, check with ponded crops
      IF (.NOT. ALLOCATED(RootZone%PondedAgRootZone%iColAgDemand)) THEN
        RootZone%Flags%lComputeAgWaterDemand = .TRUE.
      ELSE
        IF (ANY(RootZone%PondedAgRootZone%iColAgDemand.EQ.0)) RootZone%Flags%lComputeAgWaterDemand    = .TRUE.
        IF (ANY(RootZone%PondedAgRootZone%iColAgDemand.GT.0)) RootZone%Flags%lReadPondedAgWaterDemand = .TRUE.
      END IF
      
      !Are pointers defined without a defined ag water demand file?
      IF (AgWaterDemandFile .EQ. '' ) THEN
        IF (RootZone%Flags%lReadNonPondedAgWaterDemand  .OR. RootZone%Flags%lReadPondedAgWaterDemand) THEN 
            CALL SetLastMessage('Data columns from agricultural water supply requirement file is referenced but this file is not specified!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
      END IF
      
    END IF
    
    !Check if time series data column pointers are referring to existing data columns
    CALL CheckTSDataPointers(RootZone,Precip,ET,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL RootZoneParamFile%Kill()
    
    !Clear memory
    DEALLOCATE (ElemFlowToOutside , ElemFlowToGW , DummyRealArray , STAT=ErrorCode)

  END SUBROUTINE RootZone_v411_New

  
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
    TYPE(Flags_v41_Type),INTENT(IN) :: Flags
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    TYPE(ZBudgetType)               :: ZBudFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26),PARAMETER :: ThisProcedure = ModName // 'RootZoneZoneBudRawFile_New'
    CHARACTER(LEN=15),PARAMETER            :: cArea = MarkerChar // '          (' // AreaUnitMarker // ')' // MarkerChar
    INTEGER                                :: indxElem,indxVertex,ErrorCode,indxFace
    TYPE(TimeStepType)                     :: TimeStepLocal
    TYPE(ZBudgetHeaderType)                :: Header
    TYPE(SystemDataType)                   :: SystemData
    
    !INitialize
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
    Header%iNData             = NRootZoneZBudColumns
    ALLOCATE (Header%iDataTypes(NRootZoneZBudColumns)                           , &
              Header%cFullDataNames(NRootZoneZBudColumns)                       , &
              Header%cDataHDFPaths(NRootZoneZBudColumns)                        , &
              Header%iNDataElems(NRootZoneZBudColumns,1)                        , &
              Header%iElemDataColumns(AppGrid%NElements,NRootZoneZBudColumns,1) , &
              !Header%iErrorInCols()                                            , &  ! Since mass balance error is not calcuated no need
              !Header%iErrorOutCols()                                           , &  !  to allocate these arrays
              Header%cDSSFParts(NRootZoneZBudColumns)                           , &
              Header%ASCIIOutput%cColumnTitles(5)                               , &
              STAT = ErrorCode                                                  )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for root zone Z-Budget file!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Header%iDataTypes = [AR ,&  !Non-ponded ag area                   1
                         VR ,&  !Non-ponded ag potential ET           2
                         VR ,&  !Non-ponded ag precipitation          3
                         VR ,&  !Non-ponded ag runoff                 4
                         VR ,&  !Non-ponded ag prime applied water    5
                         VR ,&  !Non-ponded ag applied water from upstream element surface runoff   6
                         VR ,&  !Non-ponded ag re-used water          7
                         VR ,&  !Non-ponded ag return flow            8
                         VLB,&  !Non-ponded ag beginning storage      9
                         VR ,&  !Non-ponded ag net gain from land expansion 10
                         VR ,&  !Non-ponded ag infiltration           11
                         VR ,&  !Non-ponded ag groundwater inflow     12
                         VR ,&  !Non-ponded ag generic inflow         13
                         VR ,&  !Non-ponded ag actual ET              14
                         VR ,&  !Non-ponded ag perc                   15
                         VLE,&  !Non-ponded ag ending storage         16
                         VR ,&  !Non-ponded ag discrepancy            17
                         AR ,&  !Rice area                            18
                         VR ,&  !Rice potential ET                    19
                         VR ,&  !Rice precipitation                   20
                         VR ,&  !Rice runoff                          21
                         VR ,&  !Rice prime applied water             22
                         VR ,&  !Rice applied water from upstream element surface runoff   23
                         VR ,&  !Rice re-used water
                         VR ,&  !Rice return flow
                         VLB,&  !Rice beginning storage
                         VR ,&  !Rice net gain from land expansion
                         VR ,&  !Rice infiltration
                         VR ,&  !Rice groundwater inflow          12
                         VR ,&  !Rice generic inflow              13
                         VR ,&  !Rice pond drain
                         VR ,&  !Rice actual ET
                         VR ,&  !Rice perc
                         VLE,&  !Rice ending storage
                         VR ,&  !Rice discrepancy           18
                         AR ,&  !Rice area                   1
                         VR ,&  !Refuge potential ET
                         VR ,&  !Refuge precipitation
                         VR ,&  !Refuge runoff
                         VR ,&  !Refuge prime applied water
                         VR ,&  !Refuge applied water from upstream element surface runoff   6
                         VR ,&  !Refuge re-used water
                         VR ,&  !Refuge return flow
                         VLB,&  !Refuge beginning storage
                         VR ,&  !Refuge net gain from land expansion
                         VR ,&  !Refuge infiltration
                         VR ,&  !Refuge groundwater inflow          12
                         VR ,&  !Refuge generic inflow              13
                         VR ,&  !Refuge pond drain
                         VR ,&  !Refuge actual ET
                         VR ,&  !Refuge perc
                         VLE,&  !Refuge ending storage
                         VR ,&  !Refuge discrepancy           18
                         AR ,&  !Urban area               19
                         VR ,&  !Urban potential ET
                         VR ,&  !Urban precipitation
                         VR ,&  !Urban runoff
                         VR ,&  !Urban prime applied water
                         VR ,&  !Urban applied water due to upstream element surface runoff  24
                         VR ,&  !Urban re-used water
                         VR ,&  !Urban return flow
                         VLB,&  !Urban beginning storage
                         VR ,&  !Urban net gain from land expansion
                         VR ,&  !Urban infiltration
                         VR ,&  !Urban groundwater inflow      30
                         VR ,&  !Urban generic inflow          31
                         VR ,&  !Urban actual ET
                         VR ,&  !Urban perc
                         VLE,&  !Urban ending storage
                         VR ,&  !Urban discrepancy             35
                         AR ,&  !NV&RV area                    36
                         VR ,&  !NV&RV potential ET
                         VR ,&  !NV&RV precipitation
                         VR ,&  !NV&RV surface runoff from upstream elements/subregions  39
                         VR ,&  !NV&RV runoff
                         VLB,&  !NV&RV beginning storage
                         VR ,&  !NV&RV net gain from land expansion
                         VR ,&  !NV&RV infiltration
                         VR ,&  !NV&RV groundwater inflow     44
                         VR ,&  !NV&RV generic inflow         45
                         VR ,&  !NV&RV stream inflow for RV ET
                         VR ,&  !NV&RV actual ET
                         VR ,&  !NV&RV perc
                         VLE,&  !NV&RV ending storage
                         VR ]   !NV&RV discrepancy            50
    Header%cFullDataNames     = cRootZoneZBudgetColumnTitles
    Header%cFullDataNames(1)  = TRIM(Header%cFullDataNames(1)) // cArea
    Header%cFullDataNames(18) = TRIM(Header%cFullDataNames(18)) // cArea
    Header%cFullDataNames(36) = TRIM(Header%cFullDataNames(36)) // cArea
    Header%cFullDataNames(54) = TRIM(Header%cFullDataNames(54)) // cArea
    Header%cFullDataNames(71) = TRIM(Header%cFullDataNames(71)) // cArea
    Header%cDataHDFPaths      = cRootZoneZBudgetColumnTitles
    
    !Non-ponded ag data
    IF (Flags%lNonPondedAg_Defined) THEN
        Header%iNDataElems(1:17,:)  = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,1:17,:)  = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(6,:)        = 0
            Header%iElemDataColumns(:,6,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(12,:)        = 0
            Header%iElemDataColumns(:,12,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(13,:)        = 0
            Header%iElemDataColumns(:,13,:) = 0
        END IF
    ELSE
        Header%iNDataElems(1:17,:)        = 0
        Header%iElemDataColumns(:,1:17,:) = 0
    END IF
    
    !Ponded ag
    IF (Flags%lPondedAg_Defined) THEN
        !Rice
        Header%iNDataElems(18:35,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,18:35,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(23,:)        = 0
            Header%iElemDataColumns(:,23,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(29,:)        = 0
            Header%iElemDataColumns(:,29,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(30,:)        = 0
            Header%iElemDataColumns(:,30,:) = 0
        END IF
        
        !Refuge
        Header%iNDataElems(36:53,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,36:53,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(41,:)        = 0
            Header%iElemDataColumns(:,41,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(47,:)        = 0
            Header%iElemDataColumns(:,47,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(48,:)        = 0
            Header%iElemDataColumns(:,48,:) = 0
        END IF
    ELSE
        Header%iNDataElems(18:53,:)        = 0
        Header%iElemDataColumns(:,18:53,:) = 0
    END IF
    
    !Urban
    IF (Flags%lUrban_Defined) THEN
        Header%iNDataElems(54:70,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,54:70,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(59,:)        = 0
            Header%iElemDataColumns(:,59,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(65,:)        = 0
            Header%iElemDataColumns(:,65,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(66,:)        = 0
            Header%iElemDataColumns(:,66,:) = 0
        END IF
    ELSE
        Header%iNDataElems(54:70,:)        = 0
        Header%iElemDataColumns(:,54:70,:) = 0
    END IF
    
    !Native and riparian veg.
    IF (Flags%lNVRV_Defined) THEN
        Header%iNDataElems(71:,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,71:,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN
            Header%iNDataElems(74,:)        = 0
            Header%iElemDataColumns(:,74,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(79,:)        = 0
            Header%iElemDataColumns(:,79,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(80,:)        = 0
            Header%iElemDataColumns(:,80,:) = 0
        END IF
    ELSE
        Header%iNDataElems(71:85,:)        = 0
        Header%iElemDataColumns(:,71:85,:) = 0
    END IF

    !ASCII output titles
    Header%ASCIIOutput%iNTitles         = 5
    Header%ASCIIOutput%iLenColumnTitles = 1304
    Header%ASCIIOutput%cColumnTitles(1) = '                                                                                                                                   Non-Ponded Agricultural Area                                                                                                                                                                                                                                                      Rice Area                                                                                                                                                                                                                                                                       Refuge Area                                                                                                                                                                                                                                                               Urban Area                                                                                                                                                                                                                              Native & Riparian Vegetation Area                                                                                                ' 
    Header%ASCIIOutput%cColumnTitles(2) = '                 ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
    Header%ASCIIOutput%cColumnTitles(3) = '                                                                                    Prime      Inflow as            Reused          Net       Beginning  Net Gain from                    Groundwater        Other           Actual                       Ending                                                                                      Prime      Inflow as            Reused          Net       Beginning  Net Gain from                    Groundwater        Other           Pond           Actual                       Ending                                                                                      Prime      Inflow as            Reused          Net       Beginning  Net Gain from                    Groundwater        Other           Pond           Actual                       Ending                                                                                      Prime      Inflow as            Reused          Net       Beginning  Net Gain from                    Groundwater        Other           Actual                       Ending                                                                   Inflow as                       Beginning  Net Gain from                    Groundwater        Other   Stream Inflow          Actual                       Ending                '
    Header%ASCIIOutput%cColumnTitles(4) = '      Time                 Area      Potential   Precipitation         Runoff      Applied   Surface Runoff         Water          Return      Storage   Land Expansion   Infiltration       Inflow          Inflow            ET      Percolation        Storage    Discrepancy             Area      Potential   Precipitation         Runoff      Applied   Surface Runoff         Water          Return      Storage   Land Expansion   Infiltration       Inflow          Inflow          Drain            ET      Percolation        Storage    Discrepancy             Area      Potential   Precipitation         Runoff      Applied   Surface Runoff         Water          Return      Storage   Land Expansion   Infiltration       Inflow          Inflow          Drain            ET      Percolation        Storage    Discrepancy             Area      Potential   Precipitation         Runoff      Applied   Surface Runoff         Water          Return      Storage   Land Expansion   Infiltration       Inflow          Inflow            ET      Percolation        Storage    Discrepancy             Area      Potential  Precipitation  Surface Runoff        Runoff       Storage   Land Expansion   Infiltration       Inflow          Inflow     for ET                ET      Percolation        Storage    Discrepancy'
    Header%ASCIIOutput%cColumnTitles(5) = '                 '    //cArea//'        ET                                          Water                                           Flow         (+)           (+)             (+)             (+)             (+)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                          Water                                           Flow         (+)           (+)             (+)             (+)             (+)            (-)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                          Water                                           Flow         (+)           (+)             (+)             (+)             (+)            (-)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                          Water                                           Flow         (+)           (+)             (+)             (+)             (+)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                                          (+)           (+)             (+)             (+)             (+)       (+)                  (-)        (-)               (-)          (=)    '
    Header%ASCIIOutput%cNumberFormat    = '(A16,17(2X,F13.1),3X,18(2X,F13.1),3X,18(2X,F13.1),3X,17(2X,F13.1),3X,15(2X,F13.1))'
    
    !DSS pathanmes
    Header%cDSSFParts = ['NP_AG_AREA'               ,&
                         'NP_AG_POT_ET'             ,&
                         'NP_AG_PRECIP'             ,&   
                         'NP_AG_RUNOFF'             ,&   
                         'NP_AG_PRM_H2O'            ,&
                         'NP_AG_SR_INFLOW'          ,&
                         'NP_AG_RE-USE'             ,&   
                         'NP_AG_NT_RTRN_FLOW'       ,&   
                         'NP_AG_BEGIN_STOR'         ,&   
                         'NP_AG_GAIN_EXP'           ,&   
                         'NP_AG_INFILTR'            ,&
                         'NP_AG_GW_INFLOW'          ,&
                         'NP_AG_OTHER_INFLOW'       ,&
                         'NP_AG_ET'                 ,&   
                         'NP_AG_PERC'               ,&   
                         'NP_AG_END_STOR'           ,&  
                         'NP_AG_DISCREPANCY'        ,& 
                         'RICE_AREA'                ,&
                         'RICE_POT_ET'              ,&
                         'RICE_PRECIP'              ,&   
                         'RICE_RUNOFF'              ,&   
                         'RICE_PRM_H2O'             ,&
                         'RICE_SR_INFLOW'           ,&
                         'RICE_RE-USE'              ,&   
                         'RICE_NT_RTRN_FLOW'        ,&   
                         'RICE_BEGIN_STOR'          ,&   
                         'RICE_GAIN_EXP'            ,&   
                         'RICE_INFILTR'             ,&
                         'RICE_GW_INFLOW'           ,&
                         'RICE_OTHER_INFLOW'        ,&
                         'RICE_DRAIN'               ,&  
                         'RICE_ET'                  ,&   
                         'RICE_PERC'                ,&   
                         'RICE_END_STOR'            ,&  
                         'RICE_DISCREPANCY'         ,& 
                         'REFUGE_AREA'              ,&
                         'REFUGE_POT_ET'            ,&
                         'REFUGE_PRECIP'            ,&   
                         'REFUGE_RUNOFF'            ,&   
                         'REFUGE_PRM_H2O'           ,&
                         'REFUGE_SR_INFLOW'         ,&
                         'REFUGE_RE-USE'            ,&   
                         'REFUGE_NT_RTRN_FLOW'      ,&   
                         'REFUGE_BEGIN_STOR'        ,&   
                         'REFUGE_GAIN_EXP'          ,&   
                         'REFUGE_INFILTR'           ,&
                         'REFUGE_GW_INFLOW'         ,&
                         'REFUGE_OTHER_INFLOW'      ,&
                         'REFUGE_DRAIN'             ,&  
                         'REFUGE_ET'                ,&   
                         'REFUGE_PERC'              ,&   
                         'REFUGE_END_STOR'          ,&  
                         'REFUGE_DISCREPANCY'       ,& 
                         'URB_AREA'                 ,&
                         'URB_POT_ET'               ,&
                         'URB_PRECIP'               ,&  
                         'URB_RUNOFF'               ,&  
                         'URB_PRM_H2O'              ,& 
                         'URB_SR_INFLOW'            ,&
                         'URB_RE-USE'               ,&     
                         'URB_NT_RTRN_FLOW'         ,&     
                         'URB_BEGIN_STOR'           ,&     
                         'URB_GAIN_EXP'             ,&     
                         'URB_INFILTR'              ,&     
                         'URB_GW_INFLOW'            ,&
                         'URB_OTHER_INFLOW'         ,&
                         'URB_ET'                   ,&     
                         'URB_PERC'                 ,&     
                         'URB_END_STOR'             ,& 
                         'URB_DISCREPANCY'          ,&    
                         'NRV_AREA'                 ,&
                         'NRV_POT_ET'               ,&
                         'NRV_PRECIP'               ,&
                         'NRV_SR_INFLOW'            ,&  
                         'NRV_RUNOFF'               ,&  
                         'NRV_BEGIN_STOR'           ,&     
                         'NRV_GAIN_EXP'             ,&     
                         'NRV_INFILTR'              ,&     
                         'NRV_GW_INFLOW'            ,&
                         'NRV_OTHER_INFLOW'         ,&
                         'NRV_STRM_ET'              ,&
                         'NRV_ET'                   ,&     
                         'NRV_PERC'                 ,&     
                         'NRV_END_STOR'             ,&
                         'NRV_DISCREPANCY'          ] 
                             
    !Instantiate Z-Budget file
    CALL ZBudFile%New(cFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE RootZoneZoneBudRawFile_New
  
  
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
    TYPE(Flags_v41_Type),INTENT(IN) :: Flags
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    TYPE(ZBudgetType)               :: ZBudFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'LWUseZoneBudRawFile_New'
    CHARACTER(LEN=13),PARAMETER            :: cArea = MarkerChar // '        (' // AreaUnitMarker // ')' // MarkerChar
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
    Header%iNData             = NLWUseZBudColumns
    ALLOCATE (Header%iDataTypes(NLWUseZBudColumns)                           , &
              Header%cFullDataNames(NLWUseZBudColumns)                       , &
              Header%cDataHDFPaths(NLWUseZBudColumns)                        , &
              Header%iNDataElems(NLWUseZBudColumns,1)                        , &
              Header%iElemDataColumns(AppGrid%NElements,NLWUseZBudColumns,1) , &
              !Header%iErrorInCols()                                         , &  ! Since mass balance error is not calcuated no need
              !Header%iErrorOutCols()                                        , &  !  to allocate these arrays
              Header%cDSSFParts(NLWUseZBudColumns)                           , &
              Header%ASCIIOutput%cColumnTitles(5)                            , &
              STAT = ErrorCode                                               )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for land and water use Z-Budget file!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Header%iDataTypes = [AR                 ,&  !Non-ponded ag area
                         VR_lwu_PotCUAW     ,&  !Non-ponded potential CUAW
                         VR_lwu_AgSupplyReq ,&  !Non-ponded ag supply req.
                         VR_lwu_AgPump      ,&  !Non-ponded ag pumping 
                         VR_lwu_AgDiv       ,&  !Non-ponded ag deliveries 
                         VR_lwu_AgOthIn     ,&  !Non-ponded  ag inflow as surface runoff from upstream elements
                         VR_lwu_AgShort     ,&  !Non-ponded ag supply shortage
                         VR                 ,&  !Non-ponded ETAW
                         VR                 ,&  !Non-ponded ETP
                         VR                 ,&  !Non-ponded ETGW
                         VR                 ,&  !Non-ponded ETOth
                         AR                 ,&  !Rice area
                         VR_lwu_PotCUAW     ,&  !Rice potential CUAW
                         VR_lwu_AgSupplyReq ,&  !Rice supply req.
                         VR_lwu_AgPump      ,&  !Rice pumping 
                         VR_lwu_AgDiv       ,&  !Rice deliveries
                         VR_lwu_AgOthIn     ,&  !Rice inflow as surface runoff from upstream elements
                         VR_lwu_AgShort     ,&  !Rice supply shortage
                         VR                 ,&  !Rice ETAW
                         VR                 ,&  !Rice ETP
                         VR                 ,&  !Rice ETGW
                         VR                 ,&  !Rice ETOth
                         AR                 ,&  !Refuge area
                         VR_lwu_PotCUAW     ,&  !Refuge potential CUAW
                         VR_lwu_AgSupplyReq ,&  !Refuge supply req.
                         VR_lwu_AgPump      ,&  !Refuge pumping 
                         VR_lwu_AgDiv       ,&  !Refuge deliveries
                         VR_lwu_AgOthIn     ,&  !Refuge inflow as surface runoff from upstream elements
                         VR_lwu_AgShort     ,&  !Refuge supply shortage
                         VR                 ,&  !Refuge ETAW
                         VR                 ,&  !Refuge ETP
                         VR                 ,&  !Refuge ETGW
                         VR                 ,&  !Refuge ETOth
                         AR                 ,&  !Urban area
                         VR                 ,&  !Urban supply req.
                         VR                 ,&  !Pumping for urban
                         VR                 ,&  !Deliveries for urban
                         VR                 ,&  !Urban inflow as surface runoff from upstream elements
                         VR                 ]   !Urban supply shortage
    Header%cFullDataNames     = cLWUseZBudgetColumnTitles
    Header%cFullDataNames(1)  = TRIM(Header%cFullDataNames(1))  // cArea 
    Header%cFullDataNames(12) = TRIM(Header%cFullDataNames(12)) // cArea  
    Header%cFullDataNames(23) = TRIM(Header%cFullDataNames(23)) // cArea  
    Header%cFullDataNames(34) = TRIM(Header%cFullDataNames(34)) // cArea  
    Header%cDataHDFPaths      = cLWUseZBudgetColumnTitles
    
    !Non-ponded ag data
    IF (Flags%lNonPondedAg_Defined) THEN
        Header%iNDataElems(1:11,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,1:11,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN               
            Header%iNDataElems(6,:)        = 0
            Header%iElemDataColumns(:,6,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(10,:)        = 0
            Header%iElemDataColumns(:,10,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(11,:)        = 0
            Header%iElemDataColumns(:,11,:) = 0
        END IF
    ELSE
        Header%iNDataElems(1:11,:)        = 0
        Header%iElemDataColumns(:,1:11,:) = 0
    END IF
    
    !Rice and refuge data
    IF (Flags%lPondedAg_Defined) THEN
        !Rice
        Header%iNDataElems(12:22,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,12:22,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN               
            Header%iNDataElems(17,:)        = 0
            Header%iElemDataColumns(:,17,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(21,:)        = 0
            Header%iElemDataColumns(:,21,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(22,:)        = 0
            Header%iElemDataColumns(:,22,:) = 0
        END IF
        
        !Refuge
        Header%iNDataElems(23:33,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,23:33,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN               
            Header%iNDataElems(28,:)        = 0
            Header%iElemDataColumns(:,28,:) = 0
        END IF
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(32,:)        = 0
            Header%iElemDataColumns(:,32,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(33,:)        = 0
            Header%iElemDataColumns(:,33,:) = 0
        END IF
        
    ELSE
        Header%iNDataElems(12:33,:)        = 0
        Header%iElemDataColumns(:,12:33,:) = 0
    END IF
    
    !Urban data    
    IF (Flags%lUrban_Defined) THEN
        Header%iNDataElems(34:,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,34:,:) = indxElem
        END DO
        IF (.NOT. lElemFlowToSubregions) THEN               
            Header%iNDataElems(38,:)        = 0
            Header%iElemDataColumns(:,38,:) = 0
        END IF
    ELSE
        Header%iNDataElems(34:,:)        = 0
        Header%iElemDataColumns(:,34:,:) = 0
    END IF
    
    !ASCII output titles
    Header%ASCIIOutput%iNTitles         = 5
    Header%ASCIIOutput%iLenColumnTitles = 532
    Header%ASCIIOutput%cColumnTitles(1) = '                                                                          Non-Ponded Agricultural Area                                                                                                                               Rice Area                                                                                                                                        Refuge Area                                                                                                       Urban Area                                  '
    Header%ASCIIOutput%cColumnTitles(2) = '                 ----------------------------------------------------------------------------------------------------------------------------------------------   -----------------------------------------------------------------------------------------------------------------------------------------------   -----------------------------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------------------------------'
    Header%ASCIIOutput%cColumnTitles(3) = '                                            Agricultural                            Inflow as                                              ET            ET                                   Agricultural                            Inflow as                                              ET            ET                                      Water                                Inflow as                                              ET            ET                          Urban                               Inflow as              '
    Header%ASCIIOutput%cColumnTitles(4) = '      Time               Area    Potential     Supply         Pumping  Deliveries  Srfc. Runoff     Shortage                Effective     from       from Other            Area    Potential     Supply         Pumping  Deliveries  Srfc. Runoff     Shortage                Effective     from       from Other            Area    Potential     Supply         Pumping  Deliveries  Srfc. Runoff     Shortage                Effective     from       from Other            Area      Supply        Pumping  Deliveries  Srfc. Runoff    Shortage'
    Header%ASCIIOutput%cColumnTitles(5) = '                 '  //cArea//'     CUAW      Requirement        (-)        (-)          (-)            (=)          ETAW      Precip   Groundwater     Sources     '  //cArea//'     CUAW      Requirement        (-)        (-)          (-)            (=)          ETAW      Precip   Groundwater     Sources     '  //cArea//'     CUAW      Requirement        (-)        (-)          (-)            (=)          ETAW      Precip   Groundwater     Sources     '  //cArea//'   Requirement       (-)        (-)          (-)           (=)  '
    Header%ASCIIOutput%cNumberFormat    = '(A16,11(2X,F11.1),3X,11(2X,F11.1),3X,11(2X,F11.1),3X,6(2X,F11.1))'
    
    !DSS output pathnames
    Header%cDSSFParts = ['NP_AG_AREA'           ,&
                         'NP_AG_POTNL_CUAW'     ,&
                         'NP_AG_SUP_REQ'        ,&    
                         'NP_AG_PUMPING'        ,&
                         'NP_AG_DELIVERY'       ,&
                         'NP_AG_SR_INFLOW'      ,&
                         'NP_AG_SHORTAGE'       ,&
                         'NP_AG_ETAW'           ,&
                         'NP_AG_EFF_PRECIP'     ,&
                         'NP_AG_ET_GW'          ,&
                         'NP_AG_ET_OTH'         ,&
                         'RICE_AREA'            ,&
                         'RICE_POTNL_CUAW'      ,&
                         'RICE_SUP_REQ'         ,&    
                         'RICE_PUMPING'         ,&
                         'RICE_DELIVERY'        ,&
                         'RICE_SR_INFLOW'       ,&
                         'RICE_SHORTAGE'        ,&
                         'RICE_ETAW'            ,&
                         'RICE_EFF_PRECIP'      ,&
                         'RICE_ET_GW'           ,&
                         'RICE_ET_OTH'          ,&
                         'REFUGE_AREA'          ,&
                         'REFUGE_POTNL_CUAW'    ,&
                         'REFUGE_SUP_REQ'       ,&    
                         'REFUGE_PUMPING'       ,&
                         'REFUGE_DELIVERY'      ,&
                         'REFUGE_SR_INFLOW'     ,&
                         'REFUGE_SHORTAGE'      ,&
                         'REFUGE_ETAW'          ,&
                         'REFUGE_EFF_PRECIP'    ,&
                         'REFUGE_ET_GW'         ,&
                         'REFUGE_ET_OTH'        ,&
                         'URB_AREA'             ,&
                         'URB_SUP_REQ'          ,&       
                         'URB_PUMPING'          ,&
                         'URB_DELIVERY'         ,&
                         'URB_SR_INFLOW'        ,&
                         'URB_SHORTAGE'         ]
                             
    !Instantiate Z-Budget file
    CALL ZBudFile%New(cFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE LWUseZoneBudRawFile_New

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESCTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! ---KILL ROOT ZONE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v411_Kill(RootZone)
    CLASS(RootZone_v411_Type) :: RootZone
    
    CALL RootZone%LWUZoneBudRawFile%Kill()
    CALL RootZone%RootZoneZoneBudRawFile%Kill()

    CALL RootZone%RootZone_v41_Type%KillRZImplementation()
    
  END SUBROUTINE RootZone_v411_Kill
  
  
  
  
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
  FUNCTION RootZone_v411_GetVersion(RootZone) RESULT(cVrs)
    CLASS(RootZone_v411_Type) :: RootZone
    CHARACTER(:),ALLOCATABLE  :: cVrs
    
    IF (.NOT. RootZone%Version%IsDefined())   &
        RootZone%Version = RootZone%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = RootZone%Version%GetVersion()
    
  END FUNCTION RootZone_v411_GetVersion


  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  FUNCTION RootZone_v411_GetNDataList_AtLocationType(RootZone,iLocationType) RESULT(NData)
     CLASS(RootZone_v411_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                   :: iLocationType
     INTEGER                              :: NData
     
     !Initialize
     NData = 0
     
     SELECT CASE (iLocationType)
         CASE (iLocationType_Subregion)
             NData = RootZone%RootZone_v41_Type%GetNDataList_AtLocationType(iLocationType)
                          
             
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
     
  END FUNCTION RootZone_v411_GetNDataList_AtLocationType


  ! -------------------------------------------------------------
  ! --- GET THE LIST OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v411_GetDataList_AtLocationType(RootZone,iLocationType,cDataList,cFileList,lBudgetType)
     CLASS(RootZone_v411_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                   :: iLocationType
     CHARACTER(LEN=*),ALLOCATABLE         :: cDataList(:),cFileList(:)
     LOGICAL,ALLOCATABLE                  :: lBudgetType(:)
     
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
             CALL RootZone%RootZone_v41_Type%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
                          
             
         CASE (iLocationType_Zone)
             !Land and water use budget
             IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                 CALL RootZone%LWUZoneBudRawFile%GetFileName(cFileName)
                 iCount                    = iCount + 1
                 cDataList_Local(iCount)   = cDataList_AtZone(iLWU_AtZone)
                 cFileList_Local(iCount)   = cFileName
                 lBudgetType_Local(iCount) = .TRUE.
             END IF
             
             !Root zone budget
             IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                 CALL RootZone%RootZoneZoneBudRawFile%GetFileName(cFileName)
                 iCount                    = iCount + 1
                 cDataList_Local(iCount)   = cDataList_AtZone(iRootZone_AtZone)
                 cFileList_Local(iCount)   = cFileName
                 lBudgetType_Local(iCount) = .TRUE.
             END IF
             
             !Store data in return variables
             ALLOCATE (cDataList(iCount) , cFileList(iCount) , lBudgetType(iCount))
             cDataList   = ''
             cDataList   = cDataList_Local(1:iCount)
             cFileList   = ''
             cFileList   = cFileList_Local(1:iCount)
             lBudgetType = lBudgetType_Local(1:iCount)
             
    END SELECT
     
  END SUBROUTINE RootZone_v411_GetDataList_AtLocationType


  ! -------------------------------------------------------------
  ! --- GET THE LIST OF LOCATIONS THAT HAVE A DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v411_GetLocationsWithData(RootZone,iLocationType,cDataType,iLocations)
     CLASS(RootZone_v411_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                   :: iLocationType
     CHARACTER(LEN=*),INTENT(IN)          :: cDataType    
     INTEGER,ALLOCATABLE,INTENT(OUT)      :: iLocations(:)
     
     SELECT CASE (iLocationType)
     CASE (iLocationType_Subregion)
         CALL RootZone%RootZone_v41_Type%GetLocationsWithData(iLocationType,cDataType,iLocations)
                          
             
         CASE (iLocationType_Zone)
             !Land and water use budget
             IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iLWU_AtZone))) THEN
                 IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                     ALLOCATE (iLocations(1))
                     iLocations = iAllLocationIDsListed
                 END IF
                 
             !Root zone budget                                                                                                                                               
             ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iRootZone_AtZone))) THEN                                                                           
                 IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                     ALLOCATE (iLocations(1))
                     iLocations = iAllLocationIDsListed
                 END IF
             END IF    
             
    END SELECT
     
  END SUBROUTINE RootZone_v411_GetLocationsWithData
  
  
  ! -------------------------------------------------------------
  ! --- GET LIST OF SUB-DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v411_GetSubDataList_AtLocation(RootZone,iLocationType,cDataType,cSubDataList)
    CLASS(RootZone_v411_Type),INTENT(IN)     :: RootZone
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    SELECT CASE (iLocationType)
        !Use the parent method if the location type is subregion
        CASE (iLocationType_Subregion) 
            CALL RootZone%RootZone_v41_Type%GetSubDataList_AtLocation(iLocationType,cDataType,cSubDataList)

        !Otherwise, if the location type is zone
        CASE (iLocationType_Zone)
            !Land and water use z-budget
            IF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iLWU_AtZone))) THEN
                IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
                    ALLOCATE (cSubDataList(NLWUseZBudColumns))
                    cSubDataList = cLWUseZBudgetColumnTitles
                END IF
                
            !Root zone budget
            ELSEIF (TRIM(cDataType) .EQ. TRIM(cDataList_AtZone(iRootZone_AtZone))) THEN
                IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
                    ALLOCATE (cSubDataList(NRootZoneZBudColumns))
                    cSubDataList = cRootZoneZBudgetColumnTitles
                END IF
            END IF    
            
    END SELECT
    
  END SUBROUTINE RootZone_v411_GetSubDataList_AtLocation


  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v411_GetModelData_AtLocation(RootZone,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(RootZone_v411_Type)   :: RootZone
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
            CALL RootZone%RootZone_v41_Type%GetModelData_AtLocation(iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)   
           
           
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
                    CALL ZoneList%New(RootZone%RootZoneZoneBudRawFile%Header%iNData,RootZone%RootZoneZoneBudRawFile%Header%lFaceFlows_Defined,RootZone%RootZoneZoneBudRawFile%SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Read data
                    CALL RootZone%RootZoneZoneBudRawFile%ReadData(ZoneList,iLocationID,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_AR,rFact_VL,iDataUnitTypeArray,nActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Populate return variables
                    rOutputDates(1:nActualOutput)  = rValues(1,1:nActualOutput)
                    rOutputValues(1:nActualOutput) = rValues(2,1:nActualOutput)
                    iDataUnitType                  = iDataUnitTypeArray(1)
                END IF
            END IF    
                        
    END SELECT
         
  END SUBROUTINE RootZone_v411_GetModelData_AtLocation
  
  

  
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
  ! --- PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v411_PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation)
    CLASS(RootZone_v411_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETType),INTENT(IN)       :: ETData
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    !Local variables
    REAL(8),DIMENSION(AppGrid%NElements,1) :: rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgSrfcInflow_NP,rAgETGW_NP,                      &
                                              rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgSrfcInflow_Rice,rAgETGW_Rice,            &
                                              rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgSrfcInflow_Refuge,rAgETGW_Refuge,  &
                                              rUrbArea,rUrbPump,rUrbDeli,rUrbSrfcInflow
    REAL(8)                                :: DemandFrac(AppGrid%NElements)
    LOGICAL                                :: lFlowBetweenElements
    
    !Initialize
    IF (SIZE(RootZone%ElemFlowToElements).GT.0  .OR.  SIZE(RootZone%ElemFlowToSubregions).GT.0) THEN
        lFlowBetweenElements = .TRUE.
    ELSE
        lFlowBetweenElements = .FALSE.
    END IF
    
    !First print out results that are similar with the parent class (RootZone_v41)
    CALL RootZone%RootZone_v41_Type%PrintResults(AppGrid,ETData,TimeStep,lEndOfSimulation)
    
    !Compile data that will be used for both Z-Budget output
    IF (RootZone%Flags%LWUseZoneBudRawFile_Defined  .OR.  RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
        !Non-ponded ag data
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
            rAgArea_NP(:,1) = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)                         
            DemandFrac      = SUM(RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac_Ag , DIM=1)                 !Ratio of non-ponded crop demand to the total ag demand in the element
            rAgPump_NP(:,1) = RootZone%ElemSupply%Pumping_Ag   * DemandFrac
            rAgDeli_NP(:,1) = RootZone%ElemSupply%Diversion_Ag * DemandFrac
            IF (RootZone%Flags%lComputeETFromGW)  &
                rAgETGW_NP(:,1) = SUM(RootZone%NonPondedAgRootZone%Crops%ETFromGW_Actual , DIM=1)
            IF (lFlowBetweenElements) THEN
                DemandFrac            = SUM(RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac ,DIM=1)           !Ratio of ag demand to total element demand
                rAgSrfcInflow_NP(:,1) = RootZone%ElemSupply%UpstrmRunoff * DemandFrac                           !Surface inflow as runoff into non-ponded areas 
            END IF
        END IF
        
        !Rice and refuge
        IF (RootZone%Flags%lPondedAg_Defined) THEN
            !Rice
            rAgArea_Rice(:,1) = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%Area , DIM=1)
            DemandFrac        = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ElemDemandFrac_Ag , DIM=1)           !Ratio of rice demand to the total ag demand in the element
            rAgPump_Rice(:,1) = RootZone%ElemSupply%Pumping_Ag   * DemandFrac
            rAgDeli_Rice(:,1) = RootZone%ElemSupply%Diversion_Ag * DemandFrac
            IF (RootZone%Flags%lComputeETFromGW)  &
                rAgETGW_Rice(:,1) = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ETFromGW_Actual , DIM=1)
            IF (lFlowBetweenElements) THEN
                DemandFrac              = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ElemDemandFrac ,DIM=1)     !Ratio of rice demand to total element demand
                rAgSrfcInflow_Rice(:,1) = RootZone%ElemSupply%UpstrmRunoff * DemandFrac                         !Surface inflow as runoff into rice areas 
            END IF
            
            !Refuge
            rAgArea_Refuge(:,1) = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%Area , DIM=1)
            DemandFrac          = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ElemDemandFrac_Ag , DIM=1)         !Ratio of refuge demand to the total ag demand in the element
            rAgPump_Refuge(:,1) = RootZone%ElemSupply%Pumping_Ag   * DemandFrac
            rAgDeli_Refuge(:,1) = RootZone%ElemSupply%Diversion_Ag * DemandFrac
            IF (RootZone%Flags%lComputeETFromGW)  &
                rAgETGW_Refuge(:,1) = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ETFromGW_Actual , DIM=1)
            IF (lFlowBetweenElements) THEN
                DemandFrac                = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ElemDemandFrac ,DIM=1)   !Ratio of refuge demand to total element demand
                rAgSrfcInflow_Refuge(:,1) = RootZone%ElemSupply%UpstrmRunoff * DemandFrac                       !Surface inflow as runoff into rice areas 
            END IF
        END IF

        !Urban data
        IF (RootZone%Flags%lUrban_Defined) THEN
            rUrbArea(:,1) = RootZone%UrbanRootZone%UrbData%Area
            rUrbPump(:,1) = RootZone%ElemSupply%Pumping_Urb   
            rUrbDeli(:,1) = RootZone%ElemSupply%Diversion_Urb  
            IF (lFlowBetweenElements)  &
                rUrbSrfcInflow(:,1) = RootZone%ElemSupply%UpstrmRunoff * RootZone%UrbanRootZone%UrbData%ElemDemandFrac
        END IF
    END IF
    
    !Land and water use zone budget
    IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) CALL WriteLWUseFlowsToZoneBudRawFile(AppGrid,lFlowBetweenElements,rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgSrfcInflow_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgSrfcInflow_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgSrfcInflow_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli,rUrbSrfcInflow,RootZone)
      
    !Root zone zone budget
    IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) CALL WriteRootZoneFlowsToZoneBudRawFile(AppGrid,ETData,lFlowBetweenElements,rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgSrfcInflow_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgSrfcInflow_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgSrfcInflow_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli,rUrbSrfcInflow,RootZone)
          
  END SUBROUTINE RootZone_v411_PrintResults

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO LAND & WATER USE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToZoneBudRawFile(AppGrid,lFlowBetweenElements,rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgSrfcInflow_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgSrfcInflow_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgSrfcInflow_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli,rUrbSrfcInflow,RootZone)
    TYPE(AppGridType),INTENT(IN)                      :: AppGrid
    LOGICAL,INTENT(IN)                                :: lFlowBetweenElements
    REAL(8),DIMENSION(AppGrid%NElements,1),INTENT(IN) :: rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgSrfcInflow_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgSrfcInflow_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgSrfcInflow_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli,rUrbSrfcInflow
    TYPE(RootZone_v411_Type)                          :: RootZone
    
    !Local variables
    INTEGER,PARAMETER                      :: NLayers = 1 , &
                                              iLayer  = 1
    REAL(8),DIMENSION(AppGrid%NElements,1) :: rCUAW_NP,rAgSupReq_NP,rAgShort_NP,rETAW_NP,rETP_NP,rETOth_NP,                         &
                                              rCUAW_Rice,rAgSupReq_Rice,rAgShort_Rice,rETAW_Rice,rETP_Rice,rETOth_Rice,             &
                                              rCUAW_Refuge,rAgSupReq_Refuge,rAgShort_Refuge,rETAW_Refuge,rETP_Refuge,rETOth_Refuge, &
                                              rUrbSupReq,rUrbShort
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        rCUAW_NP(:,1)     = SUM(RootZone%NonPondedAgRootZone%Crops%DemandRaw , DIM=1)                           !Potential CUAW
        rAgSupReq_NP(:,1) = SUM(RootZone%NonPondedAgRootZone%Crops%Demand , DIM=1)                              !Ag supply requirement
        rETAW_NP(:,1)     = SUM(RootZone%NonPondedAgRootZone%Crops%ETAW , DIM=1)                                !ETAW
        rETP_NP(:,1)      = SUM(RootZone%NonPondedAgRootZone%Crops%ETP , DIM=1)                                 !Ag effective precipitation
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                     !Ag ET met from other sources
            rETOth_NP(:,1)    = SUM(RootZone%NonPondedAgRootZone%Crops%ETOth , DIM=1)                               
        rAgShort_NP(:,1)  = rAgSupReq_NP(:,1) - rAgPump_NP(:,1) - rAgDeli_NP(:,1)                               !Ag supply shortage
        IF (lFlowBetweenElements)  &                                                                            !If flow from upsteram elements, update shortage
            rAgShort_NP(:,1) = rAgShort_NP(:,1) - rAgSrfcInflow_NP(:,1) 
    END IF
    
    !Rice and refuge
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !Rice
        rCUAW_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%DemandRaw , DIM=1)                    !Potential CUAW
        rAgSupReq_Rice(:,1) = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%Demand , DIM=1)                       !Ag supply requirement
        rETAW_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ETAW , DIM=1)                         !ETAW
        rETP_Rice(:,1)      = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ETP , DIM=1)                          !Ag effective precipitation
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                    !Ag ET met from other sources
            rETOth_Rice(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ETOth , DIM=1)                        
        rAgShort_Rice(:,1)  = rAgSupReq_Rice(:,1) - rAgPump_Rice(:,1) - rAgDeli_Rice(:,1)                      !Ag supply shortage
        IF (lFlowBetweenElements)  &                                                                           !If flow from upsteram elements, update shortage
            rAgShort_Rice(:,1) = rAgShort_Rice(:,1) - rAgSrfcInflow_Rice(:,1) 

        !Refuge
        rCUAW_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%DemandRaw , DIM=1)                  !Potential CUAW
        rAgSupReq_Refuge(:,1) = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%Demand , DIM=1)                     !Ag supply requirement
        rETAW_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ETAW , DIM=1)                       !ETAW
        rETP_Refuge(:,1)      = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ETP , DIM=1)                        !Ag effective precipitation
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                    !Ag ET met from other sources
            rETOth_Refuge(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ETOth , DIM=1)                      
        rAgShort_Refuge(:,1)  = rAgSupReq_Refuge(:,1) - rAgPump_Refuge(:,1) - rAgDeli_Refuge(:,1)              !Ag supply shortage
        IF (lFlowBetweenElements)  &                                                                           !If flow from upsteram elements, update shortage
            rAgShort_Refuge(:,1) = rAgShort_Refuge(:,1) - rAgSrfcInflow_Refuge(:,1) 
    END IF
    
    !Urban data
    IF (RootZone%Flags%lUrban_Defined) THEN
        rUrbSupReq(:,1) = RootZone%UrbanRootZone%UrbData%Demand                                                !Urban supply requirement
        rUrbShort(:,1)  = rUrbSupReq(:,1) - rUrbPump(:,1) - rUrbDeli(:,1)                                      !Urban supply shortage
        IF (lFlowBetweenElements)  &                                                                           !If flow from upsteram elements, update shortage
            rUrbShort(:,1) = rUrbShort(:,1) - rUrbSrfcInflow(:,1) 
    END IF
    
    !Print non-ponded ag data
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN  
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,1,iLayer,rAgArea_NP)                  !Non-ponded ag area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,2,iLayer,rCUAW_NP)                    !Non-ponded potential CUAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,3,iLayer,rAgSupReq_NP)                !Non-ponded ag supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,4,iLayer,rAgPump_NP)                  !Non-ponded ag pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,5,iLayer,rAgDeli_NP)                  !Non-ponded ag deliveries
        IF (lFlowBetweenElements)  &                                                                          !Non-ponded ag inflow as surface runoff
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,6,iLayer,rAgSrfcInflow_NP)        
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,7,iLayer,rAgShort_NP)                 !Non-ponded ag shortage
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,8,iLayer,rETAW_NP)                    !Non-ponded ag ETAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,9,iLayer,rETP_NP)                     !Non-ponded ag effective precipitation
        IF (RootZone%Flags%lComputeETFromGW)  &                                                               !Non-ponded ag ET from groundwater
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,10,iLayer,rAgETGW_NP)             
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                   !Non-ponded ag ET met from other sources
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,11,iLayer,rETOth_NP)             
    END IF
    
    !Print rice and refuge data
    IF (RootZone%Flags%lPondedAg_Defined) THEN 
        !Rice
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,12,iLayer,rAgArea_Rice)               !Rice area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,13,iLayer,rCUAW_Rice)                 !Rice potential CUAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,14,iLayer,rAgSupReq_Rice)             !Rice supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,15,iLayer,rAgPump_Rice)               !Rice pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,16,iLayer,rAgDeli_Rice)               !Rice deliveries
        IF (lFlowBetweenElements)  &                                                                          !Rice inflow as surface runoff
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,17,iLayer,rAgSrfcInflow_Rice)     
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,18,iLayer,rAgShort_Rice)              !Rice shortage
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,19,iLayer,rETAW_Rice)                 !Rice ETAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,20,iLayer,rETP_Rice)                  !Rice effective precipitation
        IF (RootZone%Flags%lComputeETFromGW)  &                                                               !Rice ET from groundwater
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,21,iLayer,rAgETGW_Rice)           
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                   !Rice ET met from other sources
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,22,iLayer,rETOth_Rice)             

        !Refuge
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,23,iLayer,rAgArea_Refuge)             !Refuge area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,24,iLayer,rCUAW_Refuge)               !Refuge potential CUAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,25,iLayer,rAgSupReq_Refuge)           !Refuge supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,26,iLayer,rAgPump_Refuge)             !Refuge pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,27,iLayer,rAgDeli_Refuge)             !Refuge deliveries
        IF (lFlowBetweenElements)  &                                                                          !Refuge inflow as surface runoff
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,28,iLayer,rAgSrfcInflow_Refuge)   
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,29,iLayer,rAgShort_Refuge)            !Refuge shortage
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,30,iLayer,rETAW_Refuge)               !Refuge ETAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,31,iLayer,rETP_Refuge)                !Refuge effective precipitation
        IF (RootZone%Flags%lComputeETFromGW)  &                                                               !Refuge ET from groundwater
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,32,iLayer,rAgETGW_Refuge)   
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                   !Refuge ET met from other sources
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,33,iLayer,rETOth_Refuge)             
    END IF
    
    !Print urban data
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,34,iLayer,rUrbArea)                   !Urban area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,35,iLayer,rUrbSupReq)                 !Urban supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,36,iLayer,rUrbPump)                   !Urban pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,37,iLayer,rUrbDeli)                   !Urban deliveries
        IF (lFlowBetweenElements)  &                                                                          !Urban inflow as surface runoff
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,38,iLayer,rUrbSrfcInflow)               
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,iElemDataType,39,iLayer,rUrbShort)                  !Urban shortage
    END IF
        
  END SUBROUTINE WriteLWUseFlowsToZoneBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO ROOT ZONE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToZoneBudRawFile(AppGrid,ETData,lFlowBetweenElements,rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgSrfcInflow_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgSrfcInflow_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgSrfcInflow_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli,rUrbSrfcInflow,RootZone)
    TYPE(AppGridType),INTENT(IN)                      :: AppGrid
    TYPE(ETType),INTENT(IN)                           :: ETData
    LOGICAL,INTENT(IN)                                :: lFlowBetweenElements
    REAL(8),DIMENSION(AppGrid%NElements,1),INTENT(IN) :: rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgSrfcInflow_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgSrfcInflow_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgSrfcInflow_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli,rUrbSrfcInflow
    TYPE(RootZone_v411_Type)                          :: RootZone
    
    !Local variables
    INTEGER,PARAMETER                      :: NLayers = 1 , &
                                              iLayer  = 1
    REAL(8),DIMENSION(AppGrid%NElements,1) :: rAgPotET_NP,rAgPrecip_NP,rAgRunoff_NP,rAgAW_NP,rAgReuse_NP,rAgReturn_NP,rAgBeginStor_NP,rAgSoilMCh_NP,rAgInfilt_NP,rAgOthIn_NP,rAgETa_NP,rAgDP_NP,rAgEndStor_NP,rAgError_NP,                                                                            &
                                              rAgPotET_Rice,rAgPrecip_Rice,rAgRunoff_Rice,rAgAW_Rice,rAgReuse_Rice,rAgReturn_Rice,rAgBeginStor_Rice,rAgSoilMCh_Rice,rAgInfilt_Rice,rAgOthIn_Rice,rAgDrain_Rice,rAgETa_Rice,rAgDP_Rice,rAgEndStor_Rice,rAgError_Rice,                                  &
                                              rAgPotET_Refuge,rAgPrecip_Refuge,rAgRunoff_Refuge,rAgAW_Refuge,rAgReuse_Refuge,rAgReturn_Refuge,rAgBeginStor_Refuge,rAgSoilMCh_Refuge,rAgInfilt_Refuge,rAgOthIn_Refuge,rAgDrain_Refuge,rAgETa_Refuge,rAgDP_Refuge,rAgEndStor_Refuge,rAgError_Refuge,    &
                                              rUrbPotET,rUrbPrecip,rUrbRunoff,rUrbAW,rUrbReuse,rUrbReturn,rUrbBeginStor,rUrbSoilMCh,rUrbInfilt,rUrbETGW,rUrbOthIn,rUrbETa,rUrbDP,rUrbEndStor,rUrbError,                                                                                               &
                                              rNVRVArea,rNVRVPotET,rNVRVPrecip,rNVRVRunoff,rNVRVSrfcInflow,rNVRVBeginStor,rNVRVSoilMCh,rNVRVInfilt,rNVRVETGW,rNVRVOthIn,rNVRVStrmInflow,rNVRVETa,rNVRVDP,rNVRVEndStor,rNVRVError
    INTEGER                                :: indxElem,indxCrop
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        DO indxElem=1,AppGrid%NElements
            rAgPotET_NP(indxElem,1) = SUM(ETData%GetValues(RootZone%NonPondedAgRootZone%Crops(:,indxElem)%iColETc) * RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Area)       !Non-ponded ag potential ET
        END DO
        rAgPrecip_NP(:,1)    = RootZone%ElemPrecipData%Precip * rAgArea_NP(:,1)                                                                                                 !Non-ponded ag precip 
        rAgRunoff_NP(:,1)    = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff , DIM=1)                                                                                           !Non-ponded ag runoff
        rAgAW_NP             = rAgDeli_NP + rAgPump_NP                                                                                                                          !Non-ponded ag prime applied water
        rAgReuse_NP(:,1)     = SUM(RootZone%NonPondedAgRootZone%Crops%Reuse , DIM=1)                                                                                            !Non-ponded ag reuse
        rAgReturn_NP(:,1)    = SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow , DIM=1)                                                                                       !Non-ponded ag return
        rAgBeginStor_NP(:,1) = SUM((RootZone%NonPondedAgRootZone%Crops%SoilM_Precip_P_BeforeUpdate  &                                                                           !Non-ponded ag beginning storage
                                   +RootZone%NonPondedAgRootZone%Crops%SoilM_AW_P_BeforeUpdate      &                                                                      
                                   +RootZone%NonPondedAgRootZone%Crops%SoilM_Oth_P_BeforeUpdate     ) * RootZone%NonPondedAgRootZone%Crops%Area_P , DIM=1) 
        rAgSoilMCh_NP(:,1)   = SUM(RootZone%NonPondedAgRootZone%Crops%SoilMCh , DIM=1)                                                                                          !Non-ponded ag change in soil moisture due to land expansion
        rAgInfilt_NP(:,1)    = SUM(RootZone%NonPondedAgRootZone%Crops%PrecipInfilt + RootZone%NonPondedAgRootZone%Crops%IrigInfilt , DIM=1)                                     !Non-ponded ag infiltration
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                                                   !Non-ponded ag other inflow
            DO indxElem=1,AppGrid%NElements
                rAgOthIn_NP(indxElem,1) = 0.0
                DO indxCrop=1,RootZone%NonPondedAgRootZone%NCrops
                     rAgOthIn_NP(indxElem,1) = rAgOthIn_NP(indxElem,1) + (RootZone%GenericMoistureData%rGenericMoisture(1,indxElem) * RootZone%NonPondedAgRootZone%RootDepth(indxCrop) - RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%GMExcess) * RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%Area
                END DO
            END DO
        END IF
        rAgETa_NP(:,1)     = SUM(RootZone%NonPondedAgRootZone%Crops%ETa , DIM=1)                                                                                                !Non-ponded ag actual ET
        rAgDP_NP(:,1)      = SUM(RootZone%NonPondedAgRootZone%Crops%Perc + RootZone%NonPondedAgRootZone%Crops%PercCh , DIM=1)                                                   !Non-ponded ag perc                                                              
        rAgEndStor_NP(:,1) = SUM((RootZone%NonPondedAgRootZone%Crops%SoilM_Precip  &                                                                                            !Non-ponded ag ending storage
                                 +RootZone%NonPondedAgRootZone%Crops%SoilM_AW      &                                                                      
                                 +RootZone%NonPondedAgRootZone%Crops%SoilM_Oth     ) * RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
        rAgError_NP        = rAgBeginStor_NP + rAgSoilMCh_NP + rAgInfilt_NP + rAgETGW_NP - rAgETa_NP - rAgDP_NP - rAgEndStor_NP                                                 !Non-ponded ag error                                                              
        IF (RootZone%Flags%lGenericMoistureFile_Defined) rAgError_NP = rAgError_NP + rAgOthIn_NP
    END IF
    
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !Rice
        DO indxElem=1,AppGrid%NElements
            rAgPotET_Rice(indxElem,1) = rAgPotET_Rice(indxElem,1) + SUM(ETData%GetValues(RootZone%PondedAgRootZone%Crops(1:3,indxElem)%iColETc) * RootZone%PondedAgRootZone%Crops(1:3,indxElem)%Area)       !Rice potential ET
        END DO
        rAgPrecip_Rice(:,1)    = RootZone%ElemPrecipData%Precip * rAgArea_Rice(:,1)                                                                                                                         !Rice precip 
        rAgRunoff_Rice(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%Runoff , DIM=1)                                                                                                                 !Rice runoff
        rAgAW_Rice             = rAgDeli_Rice + rAgPump_Rice                                                                                                                                                !Rice prime applied water
        rAgReuse_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%Reuse , DIM=1)                                                                                                                  !Rice reuse
        rAgReturn_Rice(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ReturnFlow , DIM=1)                                                                                                             !Rice return
        rAgBeginStor_Rice(:,1) = SUM((RootZone%PondedAgRootZone%Crops(1:3,:)%SoilM_Precip_P_BeforeUpdate  &                                                                                                 !Rice beginning storage
                                     +RootZone%PondedAgRootZone%Crops(1:3,:)%SoilM_AW_P_BeforeUpdate      &                                                                                          
                                     +RootZone%PondedAgRootZone%Crops(1:3,:)%SoilM_Oth_P_BeforeUpdate     ) * RootZone%PondedAgRootZone%Crops(1:3,:)%Area_P , DIM=1)                                 
        rAgSoilMCh_Rice(:,1)   = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%SoilMCh , DIM=1)                                                                                                                !Rice change in soil moisture due to land expansion
        rAgInfilt_Rice(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%PrecipInfilt + RootZone%PondedAgRootZone%Crops(1:3,:)%IrigInfilt , DIM=1)                                                       !Rice infiltration
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                                                                               !Rice other inflow
            DO indxElem=1,AppGrid%NElements
                rAgOthIn_Rice(indxElem,1) = 0.0
                DO indxCrop=1,3
                     rAgOthIn_Rice(indxElem,1) = rAgOthIn_Rice(indxElem,1) + (RootZone%GenericMoistureData%rGenericMoisture(1,indxElem) * RootZone%PondedAgRootZone%RootDepth(indxCrop) - RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%GMExcess) * RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%Area
                END DO
            END DO
        END IF
        rAgDrain_Rice(:,1)   = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%Drain , DIM=1)                                                                                                                    !Rice pond drain
        rAgETa_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%ETa , DIM=1)                                                                                                                      !Rice actual ET
        rAgDP_Rice(:,1)      = SUM(RootZone%PondedAgRootZone%Crops(1:3,:)%Perc + RootZone%PondedAgRootZone%Crops(1:3,:)%PercCh , DIM=1)                                                                     !Rice perc                                                              
        rAgEndStor_Rice(:,1) = SUM((RootZone%PondedAgRootZone%Crops(1:3,:)%SoilM_Precip  &                                                                                                                  !Rice ending storage
                                   +RootZone%PondedAgRootZone%Crops(1:3,:)%SoilM_AW      &                                                                      
                                   +RootZone%PondedAgRootZone%Crops(1:3,:)%SoilM_Oth     ) * RootZone%PondedAgRootZone%Crops(1:3,:)%Area , DIM=1)
        rAgError_Rice        = rAgBeginStor_Rice + rAgSoilMCh_Rice + rAgInfilt_Rice + rAgETGW_Rice - rAgDrain_Rice - rAgETa_Rice - rAgDP_Rice - rAgEndStor_Rice                                             !Rice error                                                              
        IF (RootZone%Flags%lGenericMoistureFile_Defined) rAgError_Rice = rAgError_Rice + rAgOthIn_Rice

        !Refuge
        DO indxElem=1,AppGrid%NElements
            rAgPotET_Refuge(indxElem,1) = rAgPotET_Refuge(indxElem,1) + SUM(ETData%GetValues(RootZone%PondedAgRootZone%Crops(4:5,indxElem)%iColETc) * RootZone%PondedAgRootZone%Crops(4:5,indxElem)%Area)       !Refuge potential ET
        END DO
        rAgPrecip_Refuge(:,1)    = RootZone%ElemPrecipData%Precip * rAgArea_Refuge(:,1)                                                                                                                         !Refuge precip 
        rAgRunoff_Refuge(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%Runoff , DIM=1)                                                                                                                   !Refuge runoff
        rAgAW_Refuge             = rAgDeli_Refuge + rAgPump_Refuge                                                                                                                                              !Refuge prime applied water
        rAgReuse_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%Reuse , DIM=1)                                                                                                                    !Refuge reuse
        rAgReturn_Refuge(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ReturnFlow , DIM=1)                                                                                                               !Refuge return
        rAgBeginStor_Refuge(:,1) = SUM((RootZone%PondedAgRootZone%Crops(4:5,:)%SoilM_Precip_P_BeforeUpdate  &                                                                                                   !Refuge beginning storage
                                     +RootZone%PondedAgRootZone%Crops(4:5,:)%SoilM_AW_P_BeforeUpdate      &                                                                                          
                                     +RootZone%PondedAgRootZone%Crops(4:5,:)%SoilM_Oth_P_BeforeUpdate     ) * RootZone%PondedAgRootZone%Crops(4:5,:)%Area_P , DIM=1)                                 
        rAgSoilMCh_Refuge(:,1)   = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%SoilMCh , DIM=1)                                                                                                                  !Refuge change in soil moisture due to land expansion
        rAgInfilt_Refuge(:,1)    = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%PrecipInfilt + RootZone%PondedAgRootZone%Crops(4:5,:)%IrigInfilt , DIM=1)                                                         !Refuge infiltration
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                                                                                   !Refuge other inflow
            DO indxElem=1,AppGrid%NElements
                rAgOthIn_Refuge(indxElem,1) = 0.0
                DO indxCrop=4,5
                     rAgOthIn_Refuge(indxElem,1) = rAgOthIn_Refuge(indxElem,1) + (RootZone%GenericMoistureData%rGenericMoisture(1,indxElem) * RootZone%PondedAgRootZone%RootDepth(indxCrop) - RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%GMExcess) * RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%Area
                END DO
            END DO
        END IF
        rAgDrain_Refuge(:,1)   = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%Drain , DIM=1)                                                                                                                      !Refuge pond drain
        rAgETa_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%ETa , DIM=1)                                                                                                                        !Refuge actual ET
        rAgDP_Refuge(:,1)      = SUM(RootZone%PondedAgRootZone%Crops(4:5,:)%Perc + RootZone%PondedAgRootZone%Crops(4:5,:)%PercCh , DIM=1)                                                                       !Refuge perc                                                              
        rAgEndStor_Refuge(:,1) = SUM((RootZone%PondedAgRootZone%Crops(4:5,:)%SoilM_Precip  &                                                                                                                    !Refuge ending storage
                                   +RootZone%PondedAgRootZone%Crops(4:5,:)%SoilM_AW      &                                                                      
                                   +RootZone%PondedAgRootZone%Crops(4:5,:)%SoilM_Oth     ) * RootZone%PondedAgRootZone%Crops(4:5,:)%Area , DIM=1)
        rAgError_Refuge        = rAgBeginStor_Refuge + rAgSoilMCh_Refuge + rAgInfilt_Refuge + rAgETGW_Refuge - rAgDrain_Refuge - rAgETa_Refuge - rAgDP_Refuge - rAgEndStor_Refuge                               !Refuge error                                                              
        IF (RootZone%Flags%lGenericMoistureFile_Defined) rAgError_Refuge = rAgError_Refuge + rAgOthIn_Refuge    
    END IF

    !Urban data
    IF (RootZone%Flags%lUrban_Defined) THEN
        rUrbPotET(:,1)     = ETData%GetValues(RootZone%UrbanRootZone%UrbData%iColETc) * rUrbArea(:,1)                                          !Urban potential ET
        rUrbPrecip(:,1)    = RootZone%ElemPrecipData%Precip * rUrbArea(:,1)                                                                    !Urban precip
        rUrbRunoff(:,1)    = RootZone%UrbanRootZone%UrbData%Runoff                                                                             !Urban runoff
        rUrbAW             = rUrbDeli + rUrbPump                                                                                               !Urban prime appliaed water
        rUrbReuse(:,1)     = RootZone%UrbanRootZone%UrbData%Reuse                                                                              !Urban reuse
        rUrbReturn(:,1)    = RootZone%UrbanRootZone%UrbData%ReturnFlow                                                                         !Urban return
        rUrbBeginStor(:,1) = (RootZone%UrbanRootZone%UrbData%SoilM_Precip_P_BeforeUpdate  &                                                    !Urban beginning storage
                            + RootZone%UrbanRootZone%UrbData%SoilM_AW_P_BeforeUpdate      &
                            + RootZone%UrbanRootZone%UrbData%SoilM_Oth_P_BeforeUpdate     ) * RootZone%UrbanRootZone%UrbData%Area_P * RootZone%UrbanRootZone%UrbData%PerviousFrac                                  
        rUrbSoilMCh(:,1)   = RootZone%UrbanRootZone%UrbData%SoilMCh                                                                            !Urban change in soil moisture due to land expansion
        rUrbInfilt(:,1)    = RootZone%UrbanRootZone%UrbData%PrecipInfilt + RootZone%UrbanRootZone%UrbData%IrigInfilt                           !Urban infiltration
        rUrbETa(:,1)       = RootZone%UrbanRootZone%UrbData%ETa                                                                                !Urban actual ET
        rUrbDP(:,1)        = RootZone%UrbanRootZone%UrbData%Perc + RootZone%UrbanRootZone%UrbData%PercCh                                       !Urban perc                                                              
        rUrbEndStor(:,1)   = (RootZone%UrbanRootZone%UrbData%SoilM_Precip  &                                                                   !Urban ending storage
                            + RootZone%UrbanRootZone%UrbData%SoilM_AW      &
                            + RootZone%UrbanRootZone%UrbData%SoilM_Oth     ) * rUrbArea(:,1) * RootZone%UrbanRootZone%UrbData%PerviousFrac                                    
        rUrbError          = rUrbBeginStor + rUrbSoilMCh + rUrbInfilt - rUrbETa - rUrbDP - rUrbEndStor                                         !Urban error                                                             
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN
            rUrbOthIn(:,1) = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * RootZone%UrbanRootZone%RootDepth - RootZone%UrbanRootZone%UrbData%GMExcess) * rUrbArea(:,1) * RootZone%UrbanRootZone%UrbData%PerviousFrac   !Urban other inflow
            rUrbError      = rUrbError + rUrbOthIn                                                                                                           
        END IF
        IF (RootZone%Flags%lComputeETFromGW) THEN                                                                                              !Urban groundwater inflow for ET
            rUrbETGW(:,1) = RootZone%UrbanRootZone%UrbData%ETFromGW_Actual 
            rUrbError     = rUrbError + rUrbETGW
        END IF
    END IF

    !Native and riparian veg. data
    IF (RootZone%Flags%lNVRV_Defined) THEN
        rNVRVArea(:,1)      = RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%Area                             !Native and riparian area
        rNVRVPotET(:,1)     = ETData%GetValues(RootZone%NVRVRootZone%NativeVeg%iColETc) * RootZone%NVRVRootZone%NativeVeg%Area     &    !Native and riparian potential ET
                            + ETData%GetValues(RootZone%NVRVRootZone%RiparianVeg%iColETc) * RootZone%NVRVRootZone%RiparianVeg%Area
        rNVRVPrecip(:,1)    = RootZone%ElemPrecipData%Precip * rNVRVArea(:,1)                                                           !Native and riparian precip
        rNVRVRunoff(:,1)    = RootZone%NVRVRootZone%NativeVeg%Runoff    &                                                               !Native and riparian runoff
                            + RootZone%NVRVRootZone%RiparianVeg%Runoff                                                                                      
        rNVRVBeginStor(:,1) = (RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P_BeforeUpdate   &                                          !Native and riparian beginning storage
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P_BeforeUpdate        &
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P_BeforeUpdate       ) * RootZone%NVRVRootZone%NativeVeg%Area_P   &                                   
                            +(RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P_BeforeUpdate  &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P_BeforeUpdate      &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P_BeforeUpdate     ) * RootZone%NVRVRootZone%RiparianVeg%Area_P                                  
        rNVRVSoilMCh(:,1)   = RootZone%NVRVRootZone%NativeVeg%SoilMCh  &                                                                !Native and riparian change in soil moisture due to land expansion
                            + RootZone%NVRVRootZone%RiparianVeg%SoilMCh 
        rNVRVInfilt(:,1)    = RootZone%NVRVRootZone%NativeVeg%PrecipInfilt   &                                                          !Native and riparian infiltration
                            + RootZone%NVRVRootZone%RiparianVeg%PrecipInfilt                                                              
        CALL RootZone%GetActualRiparianET_AtElements(rNVRVStrmInflow(:,1))                                                              !Riparian ET from stream
        rNVRVETa(:,1)       = RootZone%NVRVRootZone%NativeVeg%ETa  &                                                                    !Native and riparian actual ET
                            + RootZone%NVRVRootZone%RiparianVeg%ETa  
        rNVRVDP(:,1)        = RootZone%NVRVRootZone%NativeVeg%Perc + RootZone%NVRVRootZone%NativeVeg%PercCh     &                       !Native and riparian perc
                            + RootZone%NVRVRootZone%RiparianVeg%Perc + RootZone%NVRVRootZone%RiparianVeg%PercCh 
        rNVRVEndStor(:,1)   = (RootZone%NVRVRootZone%NativeVeg%SoilM_Precip   &                                                         !Native and riparian ending storage
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_AW        &
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_Oth       ) * RootZone%NVRVRootZone%NativeVeg%Area &                                   
                            +(RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip  &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_AW      &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth     ) * RootZone%NVRVRootZone%RiparianVeg%Area                                    
        rNVRVError          = rNVRVBeginStor + rNVRVSoilMCh + rNVRVInfilt + RNVRVStrmInflow - rNVRVETa - rNVRVDP - rNVRVEndStor         !Native and riaprain error
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                           !Native and riparian other inflow
            rNVRVOthIn(:,1) = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * RootZone%NVRVRootZone%RootDepth_Native   - RootZone%NVRVRootZone%NativeVeg%GMExcess) * RootZone%NVRVRootZone%NativeVeg%Area     &
                            + (RootZone%GenericMoistureData%rGenericMoisture(1,:) * RootZone%NVRVRootZone%RootDepth_Riparian - RootZone%NVRVRootZone%RiparianVeg%GMExcess) * RootZone%NVRVRootZone%RiparianVeg%Area 
            rNVRVError      = rNVRVError + rNVRVOthIn 
        END IF
        IF (RootZone%Flags%lComputeETFromGW) THEN                                                                                       !Native and riparian groundwater inflow for ET
            rNVRVETGW(:,1)  = RootZone%NVRVRootZone%NativeVeg%ETFromGW_Actual   &                                                       
                            + RootZone%NVRVRootZone%RiparianVeg%ETFromGW_Actual
            rNVRVError      = rNVRVError + rNVRVETGW
        END IF
    END IF
    
    !Print non-ponded ag data
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,1,iLayer,rAgArea_NP)               !Non-ponded ag area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,2,iLayer,rAgPotET_NP)              !Non-ponded ag potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,3,iLayer,rAgPrecip_NP)             !Non-ponded ag precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,4,iLayer,rAgRunoff_NP)             !Non-ponded ag runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,5,iLayer,rAgAW_NP)                 !Non-ponded ag prime applied water
        IF (lFlowBetweenElements)  &                                                                            !Non-ponded ag surface inflow from upstream
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,6,iLayer,rAgSrfcInflow_NP)      
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,7,iLayer,rAgReuse_NP)              !Non-ponded ag reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,8,iLayer,rAgReturn_NP)             !Non-ponded ag return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,9,iLayer,rAgBeginStor_NP)          !Non-ponded ag beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,10,iLayer,rAgSoilMCh_NP)           !Non-ponded ag change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,11,iLayer,rAgInfilt_NP)            !Non-ponded ag infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                  !Non-ponded ag groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,12,iLayer,rAgETGW_NP)           
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                      !Non-ponded ag other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,13,iLayer,rAgOthIn_NP)           
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,14,iLayer,rAgETa_NP)               !Non-ponded ag actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,15,iLayer,rAgDP_NP)                !Non-ponded ag perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,16,iLayer,rAgEndStor_NP)           !Non-ponded ag end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,17,iLayer,rAgError_NP)             !Non-ponded ag error
    END IF                                                                                                   
                                                                                                             
    !Print ponded ag data
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !Rice
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,18,iLayer,rAgArea_Rice)               !Rice area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,19,iLayer,rAgPotET_Rice)              !Rice potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,20,iLayer,rAgPrecip_Rice)             !Rice precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,21,iLayer,rAgRunoff_Rice)             !Rice runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,22,iLayer,rAgAW_Rice)                 !Rice prime applied water
        IF (lFlowBetweenElements)  &                                                                               !Rice surface inflow from upstream
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,23,iLayer,rAgSrfcInflow_Rice)     
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,24,iLayer,rAgReuse_Rice)              !Rice reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,25,iLayer,rAgReturn_Rice)             !Rice return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,26,iLayer,rAgBeginStor_Rice)          !Rice beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,27,iLayer,rAgSoilMCh_Rice)            !Rice change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,28,iLayer,rAgInfilt_Rice)             !Rice infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                     !Rice groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,29,iLayer,rAgETGW_Rice)              
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                         !Rice other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,30,iLayer,rAgOthIn_Rice)              
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,31,iLayer,rAgDrain_Rice)              !Rice pond drain
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,32,iLayer,rAgETa_Rice)                !Rice actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,33,iLayer,rAgDP_Rice)                 !Rice perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,34,iLayer,rAgEndStor_Rice)            !Rice end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,35,iLayer,rAgError_Rice)              !Rice error
        
        !Refuge
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,36,iLayer,rAgArea_Refuge)               !Refuge area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,37,iLayer,rAgPotET_Refuge)              !Refuge potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,38,iLayer,rAgPrecip_Refuge)             !Refuge precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,39,iLayer,rAgRunoff_Refuge)             !Refuge runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,40,iLayer,rAgAW_Refuge)                 !Refuge prime applied water
        IF (lFlowBetweenElements)  &                                                                                 !Refuge surface inflow from upstream
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,41,iLayer,rAgSrfcInflow_Refuge)     
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,42,iLayer,rAgReuse_Refuge)              !Refuge reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,43,iLayer,rAgReturn_Refuge)             !Refuge return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,44,iLayer,rAgBeginStor_Refuge)          !Refuge beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,45,iLayer,rAgSoilMCh_Refuge)            !Refuge change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,46,iLayer,rAgInfilt_Refuge)             !Refuge infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                       !Refuge groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,47,iLayer,rAgETGW_Refuge)              
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                           !Refuge other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,48,iLayer,rAgOthIn_Refuge)              
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,49,iLayer,rAgDrain_Refuge)              !Refuge pond drain
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,50,iLayer,rAgETa_Refuge)                !Refuge actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,51,iLayer,rAgDP_Refuge)                 !Refuge perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,52,iLayer,rAgEndStor_Refuge)            !Refuge end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,53,iLayer,rAgError_Refuge)              !Refuge error
    END IF                                                                                                   
                                                                                                             
    !Print urban data
    IF (RootZone%Flags%lUrban_Defined) THEN                                                                  
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,54,iLayer,rUrbArea)             !Urban area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,55,iLayer,rUrbPotET)            !Urban potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,56,iLayer,rUrbPrecip)           !Urban precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,57,iLayer,rUrbRunoff)           !Urban runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,58,iLayer,rUrbAW)               !Urban prime applied water
        IF (lFlowBetweenElements)  &                                                                         !Urban surface inflow from upstream
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,59,iLayer,rUrbSrfcInflow)       
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,60,iLayer,rUrbReuse)            !Urban reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,61,iLayer,rUrbReturn)           !Urban return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,62,iLayer,rUrbBeginStor)        !Urban beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,63,iLayer,rUrbSoilMCh)          !Urban change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,64,iLayer,rUrbInfilt)           !Urban infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                               !Urban groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,65,iLayer,rUrbETGW)             
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                   !Urban other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,66,iLayer,rUrbOthIn)             
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,67,iLayer,rUrbETa)              !Urban actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,68,iLayer,rUrbDP)               !Urban perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,69,iLayer,rUrbEndStor)          !Urban end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,70,iLayer,rUrbError)            !Urban error
    END IF                                                                                                   
                                                                                                             
    !Print native and riparian veg data
    IF (RootZone%Flags%lNVRV_Defined) THEN                                                                   
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,71,iLayer,rNVRVArea)            !NVRV area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,72,iLayer,rNVRVPotET)           !NVRV potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,73,iLayer,rNVRVPrecip)          !NVRV precip
        IF (lFlowBetweenElements)  &                                                                         !NVRV surface inflow from upstream elements
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,74,iLayer,rNVRVSrfcInflow)      
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,75,iLayer,rNVRVRunoff)          !NVRV runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,76,iLayer,rNVRVBeginStor)       !NVRV beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,77,iLayer,rNVRVSoilMCh)         !NVRV change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,78,iLayer,rNVRVInfilt)          !NVRV infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                               !NVRV groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,79,iLayer,rNVRVETGW)             
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                   !NVRV other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,80,iLayer,rNVRVOthIn)             
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,81,iLayer,rNVRVStrmInflow)      !NVRV stream inflow for riparian ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,82,iLayer,rNVRVETa)             !NVRV actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,83,iLayer,rNVRVDP)              !NVRV perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,84,iLayer,rNVRVEndStor)         !NVRV end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,iElemDataType,85,iLayer,rNVRVError)           !NVRV error
    END IF
    
  END SUBROUTINE WriteRootZoneFlowsToZoneBudRawFile

  
END MODULE