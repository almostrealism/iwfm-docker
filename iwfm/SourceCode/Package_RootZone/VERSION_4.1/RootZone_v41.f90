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
MODULE RootZone_v41
  USE MessageLogger                  , ONLY: SetLastMessage          , &
                                             EchoProgress            , &
                                             MessageArray            , &
                                             iFatal
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE IOInterface
  USE Class_Version
  USE Package_Misc                   , ONLY: IntTSDataInFileType     , &
                                             RealTSDataInFileType    , &
                                             SolverDataType          , &
                                             ReadTSData              , &
                                             FlowDest_Outside        , &
                                             FlowDest_StrmNode       , &
                                             FlowDest_Element        , &
                                             FlowDest_Lake           , &
                                             FlowDest_Subregion      , &
                                             FlowDest_GWElement      , &
                                             Supply_Diversion_Ag     , &
                                             Supply_Diversion_Urb    , &
                                             Supply_Pumping_Ag       , &
                                             Supply_Pumping_Urb      , &
                                             Supply_UpstrmElemRunoff , &
                                             iLocationType_Subregion , &
                                             iAllLocationIDsListed   
  USE Package_Budget                 , ONLY: MaxLocationNameLen
  USE Package_ComponentConnectors    , ONLY: SupplyType
  USE Class_GenericLandUse  
  USE Class_RVETFromStrm             , ONLY: RVETFromStrm_GetActualET_AtElements
  USE Util_RootZone_v41              , ONLY: RootZoneSoil_v41_Type                  , &
                                             WaterSupplyType                        , &
                                             AddStringToStringList                  , &
                                             LWUseBudRawFile_New                    , &
                                             RootZoneBudRawFile_New                 , &
                                             NLWUseBudColumns                       , &
                                             NRootZoneBudColumns                    , & 
                                             NAgLWUseBudColumns                     , &
                                             NAgRootZoneBudColumns                  , &
                                             cLWUseBudgetColumnTitles               , &
                                             cRootZoneBudgetColumnTitles
  USE Class_GenericMoistureData
  USE Class_NonPondedAgLandUseGW
  USE Class_PondedAgLandUseGW
  USE Class_UrbanLandUseGW
  USE Class_NativeRiparianLandUseGW
  USE Class_AppGrid
  USE Package_PrecipitationET       , ONLY: ETType                                  , &
                                            PrecipitationType
  USE Package_UnsatZone
  USE Class_BaseRootZone            , ONLY: BaseRootZoneType                        , &
                                            FlagsType                               , &
                                            ElemSurfaceFlowToDestType               , &
                                            CompileElemSurfaceFlowToDestinationList , &
                                            ComputeRegionalETPot                    , &
                                            ElementLU_InterpolateExtrapolate        , &
                                            iMeasuredLUDataForSubregion             , &
                                            iMeasuredLUDataForModelDomain 
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
  PUBLIC :: RootZone_v41_Type   , &
            Flags_v41_Type      , &
            CheckTSDataPointers , &
            NGroupLandUse
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(FlagsType) :: Flags_v41_Type
    LOGICAL :: lComputeETFromGW = .FALSE. !Flag to see if root water uptake from groundwater will be simulated
  END TYPE Flags_v41_Type
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseRootZoneType) :: RootZone_v41_Type
    INTEGER                                   :: NLands                       = 0                       !Total number of land use types
    TYPE(Flags_v41_Type)                      :: Flags                                                  !Flags that affect the simulation of root zone
    TYPE(RootZoneSoil_v41_Type),ALLOCATABLE   :: ElemSoilsData(:)                                       !Soils data for each element
    REAL(8),ALLOCATABLE                       :: HydCondPonded(:)                                       !Saturated hydraulic conductivity to be used for ponded crops for each (element); overwrites the hydraulic conductivity listed in ElemSoils data
    TYPE(NonPondedAgDatabaseType)             :: NonPondedAgRootZone                                    !Non-ponded ag database
    TYPE(PondedAgDatabaseType)                :: PondedAgRootZone                                       !Rice/refuge database
    TYPE(UrbanDatabaseType)                   :: UrbanRootZone                                          !Urban database
    TYPE(NativeRiparianDatabaseType)          :: NVRVRootZone                                           !Native/riparian database
    TYPE(IntTSDataInFileType)                 :: IrigPeriodFile                                         !Irrigation period data file
    TYPE(GenericMoistureDataType)             :: GenericMoistureData                                    !Data to simulate generic moisture inflow (e.g. levee seepage, fog)
    TYPE(RealTSDataInFileType)                :: AgWaterDemandFile                                      !Agricultural water demand data file
    REAL(8)                                   :: AgWaterDemandFactor          = 1.0                     !Factor to convert ag water demand read from file
    REAL(8),ALLOCATABLE                       :: ElemDevelopedArea(:)                                   !Total ag and urban area in each element
    REAL(8),ALLOCATABLE                       :: Ratio_ElemSupplyToRegionSupply_Ag(:)                   !Ratio of element supply for ag to region supply for ag to be used to distribute regional ag water supplies to elements
    REAL(8),ALLOCATABLE                       :: Ratio_ElemSupplyToRegionSupply_Urb(:)                  !Ratio of element supply for urban to region supply for urban to be used to distribute regional urban water supplies to elements
    TYPE(WaterSupplyType),ALLOCATABLE         :: ElemSupply(:)                                          !Total water supply to element
    TYPE(SolverDataType)                      :: SolverData                                             !Data for iterative solution of soil moisture
  CONTAINS
    PROCEDURE,PASS :: New                                   => RootZone_v41_New
    PROCEDURE,PASS :: KillRZImplementation                  => RootZone_v41_Kill
    PROCEDURE,PASS :: IsLandUseUpdated                      => RootZone_v41_IsLandUseUpdated
    PROCEDURE,PASS :: GetNDataList_AtLocationType           => RootZone_v41_GetNDataList_AtLocationType
    PROCEDURE,PASS :: GetDataList_AtLocationType            => RootZone_v41_GetDataList_AtLocationType
    PROCEDURE,PASS :: GetLocationsWithData                  => RootZone_v41_GetLocationsWithData
    PROCEDURE,PASS :: GetSubDataList_AtLocation             => RootZone_v41_GetSubDataList_AtLocation 
    PROCEDURE,PASS :: GetModelData_AtLocation               => RootZone_v41_GetModelData_AtLocation 
    PROCEDURE,PASS :: GetNAgCrops                           => RootZone_v41_GetNAgCrops
    PROCEDURE,PASS :: GetNDemandLocations                   => RootZone_v41_GetNDemandLocations
    PROCEDURE,PASS :: GetElementPrecipInfilt                => RootZone_v41_GetElementPrecipInfilt
    PROCEDURE,PASS :: GetElementActualET                    => RootZone_v41_GetElementActualET
    PROCEDURE,PASS :: GetWaterDemand_Ag                     => RootZone_v41_GetElementWaterDemand_Ag 
    PROCEDURE,PASS :: GetWaterDemand_Urb                    => RootZone_v41_GetElementWaterDemand_Urb 
    PROCEDURE,PASS :: GetWaterSupply_Ag                     => RootZone_v41_GetWaterSupply_Ag 
    PROCEDURE,PASS :: GetWaterSupply_Urb                    => RootZone_v41_GetWaterSupply_Urb 
    PROCEDURE,PASS :: GetElementAgAreas                     => RootZone_v41_GetElementAgAreas
    PROCEDURE,PASS :: GetElementUrbanAreas                  => RootZone_v41_GetElementUrbanAreas
    PROCEDURE,PASS :: GetElementNativeVegAreas              => RootZone_v41_GetElementNativeVegAreas
    PROCEDURE,PASS :: GetElementRiparianVegAreas            => RootZone_v41_GetElementRiparianVegAreas
    PROCEDURE,PASS :: GetSubregionAgAreas                   => RootZone_v41_GetSubregionAgAreas
    PROCEDURE,PASS :: GetSubregionUrbanAreas                => RootZone_v41_GetSubregionUrbanAreas
    PROCEDURE,PASS :: GetSubregionNativeVegAreas            => RootZone_v41_GetSubregionNativeVegAreas
    PROCEDURE,PASS :: GetSubregionRiparianVegAreas          => RootZone_v41_GetSubregionRiparianVegAreas
    PROCEDURE,PASS :: GetDemandAgAreas                      => RootZone_v41_GetDemandAgAreas
    PROCEDURE,PASS :: GetDemandUrbanAreas                   => RootZone_v41_GetDemandUrbanAreas
    PROCEDURE,PASS :: GetElementSoilMVolume                 => RootZone_v41_GetElementSoilMVolume
    PROCEDURE,PASS :: GetPercAll                            => RootZone_v41_GetPercAll
    PROCEDURE,PASS :: GetPercElement                        => RootZone_v41_GetPercElement
    PROCEDURE,PASS :: GetFlowsToStreams                     => RootZone_v41_GetFlowsToStreams
    PROCEDURE,PASS :: GetFlowsToLakes                       => RootZone_v41_GetFlowsToLakes
    PROCEDURE,PASS :: GetActualETFromGW_AtElems             => RootZone_v41_GetActualETFromGW_AtElems             
    PROCEDURE,PASS :: GetActualRiparianET_AtStrmNodes       => RootZone_v41_GetActualRiparianET_AtStrmNodes       
    PROCEDURE,PASS :: GetActualRiparianET_AtElements        => RootZone_v41_GetActualRiparianET_AtElements       
    PROCEDURE,PASS :: GetRatio_DestSupplyToRegionSupply_Ag  => RootZone_v41_GetRatio_ElemSupplyToRegionSupply_Ag
    PROCEDURE,PASS :: GetRatio_DestSupplyToRegionSupply_Urb => RootZone_v41_GetRatio_ElemSupplyToRegionSupply_Urb
    PROCEDURE,PASS :: SetLakeElemFlag                       => RootZone_v41_SetLakeElemFlag
    PROCEDURE,PASS :: SetSupply                             => RootZone_v41_SetSupplyToElem 
    PROCEDURE,PASS :: SetActualRiparianET_AtStrmNodes       => RootZone_v41_SetActualRiparianET_AtStrmNodes       
    PROCEDURE,PASS :: ConvertTimeUnit                       => RootZone_v41_ConvertTimeUnit
    PROCEDURE,PASS :: ReadTSData                            => RootZone_v41_ReadTSData
    PROCEDURE,PASS :: ReadRestartData                       => RootZone_v41_ReadRestartData
    PROCEDURE,PASS :: AdvanceState                          => RootZone_v41_AdvanceState
    PROCEDURE,PASS :: ComputeWaterDemand                    => RootZone_v41_ComputeWaterDemand 
    PROCEDURE,PASS :: ComputeETFromGW_Max                   => RootZone_v41_ComputeETFromGW_Max
    PROCEDURE,PASS :: ZeroSupply                            => RootZone_v41_ZeroSupply
    PROCEDURE,PASS :: Simulate                              => RootZone_v41_Simulate
    PROCEDURE,PASS :: RegionalPerc                          => RootZone_v41_RegionalPerc
    PROCEDURE,PASS :: RegionalReturnFlow_Ag                 => RootZone_v41_RegionalReturnFlow_Ag
    PROCEDURE,PASS :: RegionalReturnFlow_Urb                => RootZone_v41_RegionalReturnFlow_Urb
    PROCEDURE,PASS :: PrintResults                          => RootZone_v41_PrintResults
    PROCEDURE,PASS :: PrintRestartData                      => RootZone_v41_PrintRestartData
    PROCEDURE,PASS :: GetVersion                            => RootZone_v41_GetVersion
  END TYPE RootZone_v41_Type


  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion          = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion             = '4.1.0000'
  INCLUDE 'RootZone_v41_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: nData_AtSubregion                        = 6 , &
                                 iLWU_AtSubregion                         = 1 , &
                                 iRootZone_AtSubregion                    = 2 , &
                                 iLWU_NonPondedCrop_AtSubregion           = 3 , &
                                 iRootZone_NonPondedCrop_AtSubregion      = 4 , &
                                 iLWU_PondedCrop_AtSubregion              = 5 , &
                                 iRootZone_PondedCrop_AtSubregion         = 6
  CHARACTER(LEN=50),PARAMETER :: cDataList_AtSubregion(nData_AtSubregion) = ['Land and water use budget'                          , &
                                                                             'Root zone budget'                                   , &
                                                                             'Non-ponded-crop specific land and water use budget' , &
                                                                             'Non-ponded-crop specific root zone budget'          , &
                                                                             'Ponded-crop specific land and water use budget'     , &
                                                                             'Ponded-crop specific root zone budget'              ]
    

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: NGroupLandUse           = 3 , &
                                         AgIndex                 = 1 , &
                                         UrbIndex                = 2 , &
                                         NVIndex                 = 3 , &
                                         iLandUse_NonPonded      = 1 , &
                                         iLandUse_Ponded         = 2 , &
                                         iLandUse_Urban          = 3 , &
                                         iLandUse_NVRV           = 4 
  INTEGER,PARAMETER                   :: ModNameLen = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'RootZone_v41::'
  
  
  
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
  SUBROUTINE RootZone_v41_New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
    CLASS(RootZone_v41_Type)           :: RootZone
    LOGICAL,INTENT(IN)                 :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    INTEGER,INTENT(IN)                 :: NStrmNodes,NLakes,NTIME
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16)                :: ThisProcedure = ModName // 'RootZone_v41_New'
    CHARACTER(LEN=1000)                         :: ALine,NonPondedCropFile,RiceRefugeFile,UrbanDataFile,NVRVFile,AgWaterDemandFile,GenericMoistureFile
    CHARACTER                                   :: cVersionLocal*20
    REAL(8)                                     :: FACTK,FACTCN,RegionArea(AppGrid%NSubregions+1),DummyFactor(1),FACTEXDTH,rDummy(14)
    INTEGER                                     :: NElements,NRegion,ErrorCode,indxElem,iColGenericMoisture(AppGrid%NElements),iFlagETFromGW,          &
                                                   SurfaceFlowDest(AppGrid%NElements),SurfaceFlowDestType(AppGrid%NElements),nDataCols
    TYPE(GenericFileType)                       :: RootZoneParamFile
    LOGICAL                                     :: TrackTime
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
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Read solution scheme controls
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(RootZone%SolverData%IterMax,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(FACTCN,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
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
      CALL SetLAstMessage(MessageArray(1:2),iFatal,ThisProcedure)
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

    !Land and water use budget binary output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LWUseBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,NRegion+1,RegionArea,RegionNames,'land and water use budget',cVersionLocal,RootZone%LWUseBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%LWUseBudRawFile_Defined = .TRUE.      
    END IF

    !Root zone budget binary output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,NRegion+1,RegionArea,RegionNames,'root zone budget',cVersionLocal,RootZone%RootZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%RootZoneBudRawFile_Defined = .TRUE.
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

  END SUBROUTINE RootZone_v41_New
  
  


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
  SUBROUTINE RootZone_v41_Kill(RootZone)
    CLASS(RootZone_v41_Type) :: RootZone
    
    !Local variables
    INTEGER              :: ErrorCode
    TYPE(SolverDataType) :: DummySolverData
    TYPE(Flags_v41_Type) :: DummyFlags
    
    !Deallocate arrays
    DEALLOCATE (RootZone%Flags%lLakeElems                   , &
                RootZone%ElemSoilsData                      , &
                RootZone%HydCondPonded                      , &
                RootZone%ElemDevelopedArea                  , &
                RootZone%Ratio_ElemSupplyToRegionSupply_Ag  , &
                RootZone%Ratio_ElemSupplyToRegionSupply_Urb , &
                RootZone%ElemSupply                         , &
                STAT = ErrorCode                            )
    
    !Kill components
    CALL NonPondedAgLandUse_Kill(RootZone%NonPondedAgRootZone)
    CALL PondedAgLandUse_Kill(RootZone%PondedAgRootZone)
    CALL UrbanLandUse_Kill(RootZone%UrbanRootZone)
    CALL NativeRiparianLandUse_Kill(RootZone%NVRVRootZone)
    CALL RootZone%GenericMoistureData%Kill()
    
    !Close files
    CALL RootZone%IrigPeriodFile%Close()
    CALL RootZone%AgWaterDemandFile%Close()
    
    !Default other components
    RootZone%NLands              = 0
    RootZone%Flags               = DummyFlags
    RootZone%AgWaterDemandFactor = 1.0
    RootZone%SolverData          = DummySolverData
    
  END SUBROUTINE RootZone_v41_Kill
  
  


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
  FUNCTION RootZone_v41_IsLandUseUpdated(RootZone) RESULT(lUpdated)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    LOGICAL                             :: lUpdated
    
    lUpdated = RootZone%NonPondedAgRootZone%LandUseDataFile%lUpdated       .OR.  &
               RootZone%PondedAgRootZone%LandUseDataFile%lUpdated          .OR.  &
               RootZone%UrbanRootZone%LandUseDataFile%lUpdated             .OR.  &
               RootZone%NVRVRootZone%LandUseDataFile%lUpdated
    
  END FUNCTION RootZone_v41_IsLandUseUpdated
  
  


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
  FUNCTION RootZone_v41_GetNDataList_AtLocationType(RootZone,iLocationType) RESULT(NData)
     CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                  :: iLocationType
     INTEGER                             :: NData
     
     !Initialize
     NData = 0
     
     IF (iLocationType .EQ. iLocationType_Subregion) THEN
         !Land and water use budget
         IF (RootZone%Flags%LWUseBudRawFile_Defined) THEN
             NData = 1
         END IF
         
         !Root zone budget
         IF (RootZone%Flags%RootZoneBudRawFile_Defined) THEN
             NData = NData + 1
         END IF
         !!Crop specific land and water use and root zone budgets for non-ponded crops
         !IF (RootZone%Flags%lNonPondedAg_Defined) THEN
         !    IF (RootZone%NonPondedAgRootZone%lLWUseBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iLWU_NonPondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount + 1
         !    END IF
         !    IF (RootZone%NonPondedAgRootZone%lRootZoneBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iRootZone_NonPondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount + 1
         !    END IF
         !END IF
         !!Crop specific land and water use and root zone budgets for ponded crops
         !IF (RootZone%Flags%lPondedAg_Defined) THEN
         !    IF (RootZone%PondedAgRootZone%lLWUseBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iLWU_PondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount +1
         !    END IF
         !    IF (RootZone%PondedAgRootZone%lRootZoneBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iRootZone_PondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount + 1
         !    END IF
         !END IF
    END IF
     
  END FUNCTION RootZone_v41_GetNDataList_AtLocationType


  ! -------------------------------------------------------------
  ! --- GET THE LIST OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetDataList_AtLocationType(RootZone,iLocationType,cDataList,cFileList,lBudgetType)
     CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
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
     
     IF (iLocationType .EQ. iLocationType_Subregion) THEN
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
         
         !!Crop specific land and water use and root zone budgets for non-ponded crops
         !IF (RootZone%Flags%lNonPondedAg_Defined) THEN
         !    IF (RootZone%NonPondedAgRootZone%lLWUseBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iLWU_NonPondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount + 1
         !    END IF
         !    IF (RootZone%NonPondedAgRootZone%lRootZoneBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iRootZone_NonPondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount + 1
         !    END IF
         !END IF
         !!Crop specific land and water use and root zone budgets for ponded crops
         !IF (RootZone%Flags%lPondedAg_Defined) THEN
         !    IF (RootZone%PondedAgRootZone%lLWUseBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iLWU_PondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount +1
         !    END IF
         !    IF (RootZone%PondedAgRootZone%lRootZoneBudRawFile_Defined) THEN
         !        CALL AddStringToStringList(cDataList_AtSubregion(iRootZone_PondedCrop_AtSubregion) , cDataList)
         !        iCount = iCount + 1
         !    END IF
         !END IF
    END IF
     
    !Store data in return variables
    ALLOCATE (cDataList(iCount) , cFileList(iCount) , lBudgetType(iCount))
    cDataList   = ''
    cDataList   = cDataList_Local(1:iCount)
    cFileList   = ''
    cFileList   = cFileList_Local(1:iCount)
    lBudgetType = lBudgetType_Local(1:iCount)
     
  END SUBROUTINE RootZone_v41_GetDataList_AtLocationType


  ! -------------------------------------------------------------
  ! --- GET THE LIST OF LOCATIONS THAT HAVE A DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetLocationsWithData(RootZone,iLocationType,cDataType,iLocations)
     CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)                  :: iLocationType
     CHARACTER(LEN=*),INTENT(IN)         :: cDataType    
     INTEGER,ALLOCATABLE,INTENT(OUT)     :: iLocations(:)
     
     IF (iLocationType .EQ. iLocationType_Subregion) THEN
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
    END IF
     
  END SUBROUTINE RootZone_v41_GetLocationsWithData


  ! -------------------------------------------------------------
  ! --- GET LIST OF SUB-DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetSubDataList_AtLocation(RootZone,iLocationType,cDataType,cSubDataList)
    CLASS(RootZone_v41_Type),INTENT(IN)      :: RootZone
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
     
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    !Sub-data exists only for root zone and land surface budget at subregion level
    IF (iLocationType .EQ. iLocationType_Subregion) THEN
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
    END IF
     
  END SUBROUTINE RootZone_v41_GetSubDataList_AtLocation


  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetModelData_AtLocation(RootZone,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(RootZone_v41_Type)    :: RootZone
    INTEGER,INTENT(IN)          :: iZExtent,iElems(:),iLayers(:),iZones(:),iZonesWithNames(:),iLocationType,iLocationID,iCol
    CHARACTER(LEN=*),INTENT(IN) :: cZoneNames(:),cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)          :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)         :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    !Proceed based on location type
    IF (iLocationType .EQ. iLocationType_Subregion) THEN
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
    END IF
      
  END SUBROUTINE RootZone_v41_GetModelData_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET RATIO OF ELEMENT SUPPLIES TO REGIONAL SUPLLIES FOR AG 
  ! -------------------------------------------------------------
  PURE SUBROUTINE RootZone_v41_GetRatio_ElemSupplyToRegionSupply_Ag(RootZone,Ratio)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Ratio(:)
    
    Ratio = RootZone%Ratio_ElemSupplyToRegionSupply_Ag
    
  END SUBROUTINE RootZone_v41_GetRatio_ElemSupplyToRegionSupply_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET RATIO OF ELEMENT SUPPLIES TO REGIONAL SUPPLIES FOR URBAN 
  ! -------------------------------------------------------------
  PURE SUBROUTINE RootZone_v41_GetRatio_ElemSupplyToRegionSupply_Urb(RootZone,Ratio)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Ratio(:)
    
    Ratio = RootZone%Ratio_ElemSupplyToRegionSupply_Urb
    
  END SUBROUTINE RootZone_v41_GetRatio_ElemSupplyToRegionSupply_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL RIPARIAN ET TAKEN OUT OF EACH STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetActualRiparianET_AtStrmNodes(RootZone,QRVET)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: QRVET(:)
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        CALL NativeRiparianLandUse_GetActualET_AtStrmNodes(RootZone%NVRVRootZone,QRVET)
    ELSE
        QRVET = 0.0
    END IF
    
  END SUBROUTINE RootZone_v41_GetActualRiparianET_AtStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL RIPARIAN ET ACCUMULATED TO EACH ELEMENT 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetActualRiparianET_AtElements(RootZone,RVETAtElems)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: RVETAtElems(:)
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        IF (RootZone%NVRVRootZone%lRVETFromStrm_Simulated) THEN
            CALL RVETFromStrm_GetActualET_AtElements(RootZone%NVRVRootZone%RVETFromStrm,RVETAtElems)
        ELSE
            RVETAtElems = 0.0
        END IF
    ELSE
        RVETAtElems = 0.0
    END IF
    
  END SUBROUTINE RootZone_v41_GetActualRiparianET_AtElements
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL ET FROM GW AT EACH ELEMENT 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetActualETFromGW_AtElems(RootZone,NElements,ETFromGW_Actual)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: NElements
    REAL(8),INTENT(OUT)                 :: ETFromGW_Actual(NElements)
    
    !Local variables
    INTEGER :: indxElem
    
    !Initialize
    ETFromGW_Actual = 0.0
    
    !Return if ET from GW is not simulated
    IF (.NOT. RootZone%Flags%lComputeETFromGW) RETURN
    
    !From non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        DO indxElem=1,NElements
            ETFromGW_Actual(indxElem) = SUM(RootZone%NonPondedAgRootZone%Crops(:,indxElem)%ETFromGW_Actual)
        END DO
    END IF
    
    !From ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        DO indxElem=1,NElements
            ETFromGW_Actual(indxElem) = ETFromGW_Actual(indxElem) + SUM(RootZone%PondedAgRootZone%Crops(:,indxElem)%ETFromGW_Actual)
        END DO
    END IF

    !From urban lands
    IF (RootZone%Flags%lUrban_Defined) THEN
        DO indxElem=1,NElements
            ETFromGW_Actual(indxElem) = ETFromGW_Actual(indxElem) + RootZone%UrbanRootZone%UrbData(indxElem)%ETFromGW_Actual
        END DO
    END IF

    !From native and riparian vegetation lands
    IF (RootZone%Flags%lNVRV_Defined) THEN
        DO indxElem=1,NElements
            ETFromGW_Actual(indxElem) = ETFromGW_Actual(indxElem) + RootZone%NVRVRootZone%NativeVeg(indxElem)%ETFromGW_Actual    &
                                                                  + RootZone%NVRVRootZone%RiparianVeg(indxElem)%ETFromGW_Actual
        END DO
    END IF
    
  END SUBROUTINE RootZone_v41_GetActualETFromGW_AtElems
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER 
  ! -------------------------------------------------------------
  FUNCTION RootZone_v41_GetVersion(RootZone) RESULT(cVrs)
    CLASS(RootZone_v41_Type) :: RootZone
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    IF (.NOT. RootZone%Version%IsDefined())   &
        RootZone%Version = RootZone%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = RootZone%Version%GetVersion()
    
  END FUNCTION RootZone_v41_GetVersion
  

  ! -------------------------------------------------------------
  ! --- GET TOTAL AG SUPPLIES TO ELEMENTS 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetWaterSupply_Ag(RootZone,AppGrid,Supply)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: Supply(:)
    
    !Local variables
    INTEGER             :: ErrorCode
    REAL(8),ALLOCATABLE :: NonPondedAgSupply(:,:),PondedAgSupply(:,:)
    
    !Supply as pumping and diversions
    Supply = RootZone%ElemSupply%Diversion_Ag + RootZone%ElemSupply%Pumping_Ag

    !Compute non-ponded ag supplies in terms of runoff from upstream elements
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
      CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,RootZone%ElemSupply%UpstrmRunoff,RootZone,iLandUse_NonPonded,NonPondedAgSupply)
      Supply = Supply + SUM(NonPondedAgSupply , DIM=1)
    END IF
    
    !Compute ponded ag supplies in terms of runoff from upstream elements
    IF (RootZone%Flags%lPondedAg_Defined) THEN
      CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,RootZone%ElemSupply%UpstrmRunoff,RootZone,iLandUse_Ponded,PondedAgSupply)
      Supply = Supply + SUM(PondedAgSupply , DIM=1)
    END IF
    
    !Clear memory
    DEALLOCATE (NonPondedAgSupply , PondedAgSupply , STAT=ErrorCode)

  END SUBROUTINE RootZone_v41_GetWaterSupply_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL URBAN SUPPLIES TO ELEMENTS 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetWaterSupply_Urb(RootZone,AppGrid,Supply)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: Supply(:)
    
    !Local variables
    INTEGER             :: ErrorCode
    REAL(8),ALLOCATABLE :: UrbSupply(:,:)
    
    !Supply as pumping and diversions
    Supply = RootZone%ElemSupply%Diversion_Urb + RootZone%ElemSupply%Pumping_Urb

    !Compute urban supplies in terms of runoff from upstream elements
    IF (RootZone%Flags%lUrban_Defined) THEN
      CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,RootZone%ElemSupply%UpstrmRunoff,RootZone,iLandUse_Urban,UrbSupply)
      Supply = Supply + UrbSupply(1,:)
    END IF
    
    !Clear memory
    DEALLOCATE (UrbSupply , STAT=ErrorCode)
    
  END SUBROUTINE RootZone_v41_GetWaterSupply_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DEMAND CALCULATION LOCATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION RootZone_v41_GetNDemandLocations(RootZone) RESULT(NLocations)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER                             :: NLocations
    
    NLocations = SIZE(RootZone%ElemSupply)
    
  END FUNCTION RootZone_v41_GetNDemandLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET PRECIP INFILTRATION AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementPrecipInfilt(RootZone,ElemRegion,PrecipInfilt)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: ElemRegion(:)   !Not used in this version
    REAL(8)                             :: PrecipInfilt(:)
    
    !From non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        PrecipInfilt = SUM(RootZone%NonPondedAgRootZone%Crops%PrecipInfilt * RootZone%NonPondedAgRootZone%Crops%Area, DIM=1)
    ELSE
        PrecipInfilt = 0.0
    END IF
    
    !From ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) &
        PrecipInfilt = PrecipInfilt + SUM(RootZone%PondedAgRootZone%Crops%PrecipInfilt * RootZone%PondedAgRootZone%Crops%Area, DIM=1)
    
    !From urban
    IF (RootZone%Flags%lUrban_Defined) &
        PrecipInfilt = PrecipInfilt + RootZone%UrbanRootZone%UrbData%PrecipInfilt *RootZone%UrbanRootZone%UrbData%Area * RootZone%UrbanRootZone%UrbData%PerviousFrac
    
    !From native and riparian veg
    IF (RootZone%Flags%lNVRV_Defined) &
        PrecipInfilt = PrecipInfilt + RootZone%NVRVRootZone%NativeVeg%PrecipInfilt   * RootZone%NVRVRootZone%NativeVeg%Area    &
                                    + RootZone%NVRVRootZone%RiparianVeg%PrecipInfilt * RootZone%NVRVRootZone%RiparianVeg%Area
    
  END SUBROUTINE RootZone_v41_GetElementPrecipInfilt
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL ET AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementActualET(RootZone,ElemRegion,ET)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: ElemRegion(:)   !Not used in this version
    REAL(8)                             :: ET(:)
    
    !From non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        ET = SUM(RootZone%NonPondedAgRootZone%Crops%ETa , DIM=1)
    ELSE
        ET = 0.0
    END IF
    
    !From ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) &
        ET = ET + SUM(RootZone%PondedAgRootZone%Crops%ETa , DIM=1)
    
    !From urban
    IF (RootZone%Flags%lUrban_Defined) &
        ET = ET + RootZone%UrbanRootZone%UrbData%ETa 
    
    !From native and riparian veg
    IF (RootZone%Flags%lNVRV_Defined) &
        ET = ET + RootZone%NVRVRootZone%NativeVeg%ETa + RootZone%NVRVRootZone%RiparianVeg%ETa 
    
  END SUBROUTINE RootZone_v41_GetElementActualET
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AG CROPS
  ! -------------------------------------------------------------
  PURE FUNCTION RootZone_v41_GetNAgCrops(RootZone) RESULT(NAgCrops)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER                             :: NAgCrops
    
    NAgCrops = RootZone%NonPondedAgRootZone%NCrops + RootZone%PondedAgRootZone%NCrops
    
  END FUNCTION RootZone_v41_GetNAgCrops
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL AG WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementWaterDemand_Ag(RootZone,AgDemand)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                             :: AgDemand(:)
    
    !Inform user
    CALL EchoProgress('Retrieving elemental agricultural water demand')
    
    !Initialize
    AgDemand = 0.0
    
    IF (RootZone%Flags%lNonPondedAg_Defined) AgDemand = SUM(RootZone%NonPondedAgRootZone%Crops%Demand , DIM=1)
    IF (RootZone%Flags%lPondedAg_Defined)    AgDemand = AgDemand + SUM(RootZone%PondedAgRootZone%Crops%Demand , DIM=1)

  END SUBROUTINE RootZone_v41_GetElementWaterDemand_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL URBAN WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementWaterDemand_Urb(RootZone,UrbDemand)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                             :: UrbDemand(:)
    
    !Inform user
    CALL EchoProgress('Retrieving elemental urban water demand')
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        UrbDemand = RootZone%UrbanRootZone%UrbData%Demand
    ELSE
        UrbDemand = 0.0
    END IF

  END SUBROUTINE RootZone_v41_GetElementWaterDemand_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL OF PONDED AND NON-PONDED AG AREAS AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementAgAreas(RootZone,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        Areas = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
    ELSE
        Areas = 0.0
    END IF
    
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) Areas = Areas + SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)

  END SUBROUTINE RootZone_v41_GetElementAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementUrbanAreas(RootZone,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
        
    !Urban areas
    IF (RootZone%Flags%lUrban_Defined) THEN
        Areas = RootZone%UrbanRootZone%UrbData%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v41_GetElementUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementNativeVegAreas(RootZone,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
        
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas = RootZone%NVRVRootZone%NativeVeg%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v41_GetElementNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL RIPARIAN VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementRiparianVegAreas(RootZone,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
        
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas = RootZone%NVRVRootZone%RiparianVeg%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v41_GetElementRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL OF PONDED AND NON-PONDED AG AREAS AT EACH SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetSubregionAgAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    Areas = RegionalLUArea(AppGrid,RootZone,AgIndex)

  END SUBROUTINE RootZone_v41_GetSubregionAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetSubregionUrbanAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
        
    Areas = RegionalLUArea(AppGrid,RootZone,UrbIndex)

  END SUBROUTINE RootZone_v41_GetSubregionUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetSubregionNativeVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (.NOT. RootZone%Flags%lNVRV_Defined) THEN
        Areas = 0.0
        RETURN
    ELSE
        Areas(1:AppGrid%NSubregions) = AppGrid%AccumElemValuesToSubregions(RootZone%NVRVRootZone%NativeVeg%Area)
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    END IF
    
  END SUBROUTINE RootZone_v41_GetSubregionNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetSubregionRiparianVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (.NOT. RootZone%Flags%lNVRV_Defined) THEN
        Areas = 0.0
        RETURN
    ELSE
        Areas(1:AppGrid%NSubregions) = AppGrid%AccumElemValuesToSubregions(RootZone%NVRVRootZone%RiparianVeg%Area)
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    END IF
    
  END SUBROUTINE RootZone_v41_GetSubregionRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL OF PONDED AND NON-PONDED AG AREAS AT DEMAND LOCATION (ELEMENT)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetDemandAgAreas(RootZone,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),ALLOCATABLE                 :: Areas(:)
    
    !Initialize
    IF (.NOT. ALLOCATED(Areas)) ALLOCATE(Areas(SIZE(RootZone%ElemSoilsData)))
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        Areas = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
    ELSE
        Areas = 0.0
    END IF
    
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) Areas = Areas + SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)

  END SUBROUTINE RootZone_v41_GetDemandAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN AREAS DEMAND LOCATION (ELEMENT)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetDemandUrbanAreas(RootZone,Areas)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),ALLOCATABLE                 :: Areas(:)
    
    !Initialize
    IF (.NOT. ALLOCATED(Areas)) ALLOCATE(Areas(SIZE(RootZone%ElemSoilsData)))
    
    !Urban areas
    IF (RootZone%Flags%lUrban_Defined) THEN
        Areas = RootZone%UrbanRootZone%UrbData%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v41_GetDemandUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! ---GET ELEMENTAL VOLUMETRIC SOIL MOISTURE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetElementSoilMVolume(RootZone,AppGrid,SoilM)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid   
    REAL(8),INTENT(OUT)                 :: SoilM(:)
    
    !Local variables
    INTEGER :: indxElem,indxCrop
    
    !Soil moisture from non-ponded lands
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        SoilM = 0.0
        DO indxElem=1,AppGrid%NElements
            DO indxCrop=1,RootZone%NonPondedAgRootZone%NCrops
                SoilM(indxElem) = SoilM(indxElem) + ( RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Precip  &
                                                    + RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_AW      &
                                                    + RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Oth   ) * RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%Area  
            END DO
        END DO
    ELSE
        SoilM = 0.0
    END IF
    
    !Soil moisture from ponded crops
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        DO indxElem=1,AppGrid%NElements
            DO indxCrop=1,RootZone%PondedAgRootZone%NCrops
                SoilM(indxElem) = SoilM(indxElem) + ( RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Precip  &
                                                    + RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_AW      &
                                                    + RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Oth   ) * RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%Area  
            END DO
        END DO
    END IF
    
    !Soil moisture from urban lands
    IF (RootZone%Flags%lUrban_Defined) THEN
        DO indxElem=1,AppGrid%NElements
            SoilM(indxElem) = SoilM(indxElem) + ( RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Precip  &
                                                + RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_AW      &
                                                + RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Oth   ) * RootZone%UrbanRootZone%UrbData(indxElem)%Area * RootZone%UrbanRootZone%UrbData(indxElem)%PerviousFrac
        END DO
    END IF

    !Soil moisture from native and riparian veg
    IF (RootZone%Flags%lNVRV_Defined) THEN
        DO indxElem=1,AppGrid%NElements
            SoilM(indxElem) = SoilM(indxElem) + ( RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Precip    &
                                                + RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_AW        &
                                                + RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Oth     ) * RootZone%NVRVRootZone%NativeVeg(indxElem)%Area  &
                                              + ( RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Precip  &
                                                + RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_AW      &
                                                + RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Oth   ) * RootZone%NVRVRootZone%RiparianVeg(indxElem)%Area
        END DO
    END IF

  END SUBROUTINE RootZone_v41_GetElementSoilMVolume
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLTAION AT ALL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION RootZone_v41_GetPercAll(RootZone,AppGrid) RESULT(Perc)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: Perc(AppGrid%NElements)

    !Initialize
    Perc = 0.0
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) Perc = SUM(RootZone%NonPondedAgRootZone%Crops%Perc , DIM=1) + SUM(RootZone%NonPondedAgRootZone%Crops%PercCh , DIM=1)

    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) Perc = Perc + SUM(RootZone%PondedAgRootZone%Crops%Perc , DIM=1) + SUM(RootZone%PondedAgRootZone%Crops%PercCh , DIM=1)
    
    !Urban
    IF (RootZone%Flags%lUrban_Defined) Perc = Perc + RootZone%UrbanRootZone%UrbData%Perc + RootZone%UrbanRootZone%UrbData%PercCh

    !Native and riparian vegetation areas
    IF (RootZone%Flags%lNVRV_Defined) Perc = Perc + RootZone%NVRVRootZone%NativeVeg%Perc   + RootZone%NVRVRootZone%RiparianVeg%Perc  &
                                                  + RootZone%NVRVRootZone%NativeVeg%PercCh + RootZone%NVRVRootZone%RiparianVeg%PercCh 
    
  END FUNCTION RootZone_v41_GetPercAll
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT AN INDIVIDUAL ELEMENT
  ! -------------------------------------------------------------
  FUNCTION RootZone_v41_GetPercElement(RootZone,iElem,AppGrid) RESULT(Perc)
    CLASS(RootZone_v41_Type),INTENT(IN)   :: RootZone
    INTEGER,INTENT(IN)                    :: iElem
    TYPE(AppGridType),OPTIONAL,INTENT(IN) :: AppGrid   !NOt used in this version of root zone component
    REAL(8)                               :: Perc
    
    !Initialize
    Perc = 0.0
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) Perc = SUM(RootZone%NonPondedAgRootZone%Crops(:,iElem)%Perc) + SUM(RootZone%NonPondedAgRootZone%Crops(:,iElem)%PercCh)

    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) Perc = Perc + SUM(RootZone%PondedAgRootZone%Crops(:,iElem)%Perc) + SUM(RootZone%PondedAgRootZone%Crops(:,iElem)%PercCh)
    
    !Urban
    IF (RootZone%Flags%lUrban_Defined) Perc = Perc + RootZone%UrbanRootZone%UrbData(iElem)%Perc + RootZone%UrbanRootZone%UrbData(iElem)%PercCh

    !Native and riparian vegetation areas
    IF (RootZone%Flags%lNVRV_Defined) Perc = Perc + RootZone%NVRVRootZone%NativeVeg(iElem)%Perc   + RootZone%NVRVRootZone%RiparianVeg(iElem)%Perc  &
                                                  + RootZone%NVRVRootZone%NativeVeg(iElem)%PercCh + RootZone%NVRVRootZone%RiparianVeg(iElem)%PercCh 
    
  END FUNCTION RootZone_v41_GetPercElement
  
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetFlowsToStreams(RootZone,AppGrid,DirectRunoff,ReturnFlow,RiparianET)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid                       !Not used in this version
    REAL(8),INTENT(OUT)                 :: DirectRunoff(:),ReturnFlow(:)
    REAL(8),INTENT(INOUT)               :: RiparianET(:)                 
    
    !Local variables
    INTEGER :: indx,iStrmNode,iElem
    LOGICAL :: lNonPondedAg_Defined,lPondedAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    DirectRunoff         = 0.0
    ReturnFlow           = 0.0
    lNonPondedAg_Defined = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg_Defined    = RootZone%Flags%lPondedAg_Defined
    lUrban_Defined       = RootZone%Flags%lUrban_Defined
    lNVRV_Defined        = RootZone%Flags%lNVRV_Defined
    
    
    !Flows into streams
    ASSOCIATE (pFlowData    => RootZone%ElemFlowToStreams         , &
               pCrops       => RootZone%NonPondedAgRootZone%Crops , &
               pPondedCrops => RootZone%PondedAgRootZone%Crops    , &
               pUrban       => RootZone%UrbanRootZone%UrbData     , &
               pNV          => RootZone%NVRVRootZone%NativeVeg    , &
               pRV          => RootZone%NVRVRootZone%RiparianVeg  )
    
      DO indx=1,SIZE(pFlowData)
        !Element ID
        iElem = pFlowData(indx)%iElement
          
        !Destination stream node
        iStrmNode = pFlowData(indx)%iDestID
        
        !Flows from non-ponded ag lands
        IF (lNonPondedAg_Defined) THEN
          DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + SUM(pCrops(:,iElem)%Runoff)
          ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + SUM(pCrops(:,iElem)%ReturnFlow)
        END IF
        
        !Flows from ponded lands
        IF (lPondedAg_Defined) THEN
          DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + SUM(pPondedCrops(:,iElem)%Runoff)
          ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + SUM(pPondedCrops(:,iElem)%ReturnFlow)
        END IF
        
        !Flows from urban lands
        IF (lUrban_Defined) THEN
          DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + pUrban(iElem)%Runoff
          ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + pUrban(iElem)%ReturnFlow
        END IF
        
        !Flows from native/riparian veg
        IF (lNVRV_Defined) &
          DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + pNV(iElem)%Runoff + pRV(iElem)%Runoff         

      END DO
      
    END ASSOCIATE
    
    !Flows out of streams
    IF (lNVRV_Defined) THEN
        IF (NativeRiparianLandUse_IsRVETFromStrmSimulated(RootZone%NVRVRootZone)) THEN
            CALL NativeRiparianLandUse_GetRequiredET_AtStrmNodes(RootZone%NVRVRootZone,RiparianET)
        END IF
    END IF
    
  END SUBROUTINE RootZone_v41_GetFlowsToStreams
    
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO LAKES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_GetFlowsToLakes(RootZone,AppGrid,DirectRunoff,ReturnFlow)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid                       !Not used in this version
    REAL(8),INTENT(OUT)                 :: DirectRunoff(:),ReturnFlow(:)
    
    !Local variables
    INTEGER :: indx,iLake,iElem
    LOGICAL :: lNonPondedAg_Defined,lPondedAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    DirectRunoff         = 0.0
    ReturnFlow           = 0.0
    lNonPondedAg_Defined = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg_Defined    = RootZone%Flags%lPondedAg_Defined
    lUrban_Defined       = RootZone%Flags%lUrban_Defined
    lNVRV_Defined        = RootZone%Flags%lNVRV_Defined
    
    
    !Flows into streams
    ASSOCIATE (pFlowData    => RootZone%ElemFlowToLakes           , &
               pCrops       => RootZone%NonPondedAgRootZone%Crops , &
               pPondedCrops => RootZone%PondedAgRootZone%Crops    , &
               pUrban       => RootZone%UrbanRootZone%UrbData     , &
               pNV          => RootZone%NVRVRootZone%NativeVeg    , &
               pRV          => RootZone%NVRVRootZone%RiparianVeg  )
    
      DO indx=1,SIZE(pFlowData)
        !Element ID
        iElem = pFlowData(indx)%iElement
        
        !Destination lake ID
        iLake = pFlowData(indx)%iDestID
        
        !Flows from non-ponded ag lands
        IF (lNonPondedAg_Defined) THEN
          DirectRunoff(iLake) = DirectRunoff(iLake) + SUM(pCrops(:,iElem)%Runoff)
          ReturnFlow(iLake)   = ReturnFlow(iLake)   + SUM(pCrops(:,iElem)%ReturnFlow)
        END IF
        
        !Flows from ponded lands
        IF (lPondedAg_Defined) THEN
          DirectRunoff(iLake) = DirectRunoff(iLake) + SUM(pPondedCrops(:,iElem)%Runoff)
          ReturnFlow(iLake)   = ReturnFlow(iLake)   + SUM(pPondedCrops(:,iElem)%ReturnFlow)
        END IF
        
        !Flows from urban lands
        IF (lUrban_Defined) THEN
          DirectRunoff(iLake) = DirectRunoff(iLake) + pUrban(iElem)%Runoff
          ReturnFlow(iLake)   = ReturnFlow(iLake)   + pUrban(iElem)%ReturnFlow
        END IF
        
        !Flows from native/riparian veg
        IF (lNVRV_Defined) &
          DirectRunoff(iLake) = DirectRunoff(iLake) + pNV(iElem)%Runoff + pRV(iElem)%Runoff         

      END DO
      
    END ASSOCIATE
    
  END SUBROUTINE RootZone_v41_GetFlowsToLakes

  
  
  
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
  ! --- SET ACTUAL RIPARIAN ET
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_SetActualRiparianET_AtStrmNodes(RootZone,RiparianETFrac)
    CLASS(RootZone_v41_Type) :: RootZone
    REAL(8),INTENT(IN)       :: RiparianETFrac(:)
    
    IF (RootZone%Flags%lNVRV_Defined) CALL NativeRiparianLandUse_SetActualRiparianET_AtStrmNodes(RiparianETFrac,RootZone%NVRVRootZone)
    
  END SUBROUTINE RootZone_v41_SetActualRiparianET_AtStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY TO ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_SetSupplyToElem(RootZone,Supply,SupplyType)
    CLASS(RootZone_v41_Type) :: RootZone
    REAL(8),INTENT(IN)       :: Supply(:)
    INTEGER,INTENT(IN)       :: SupplyType

    !Inform user
    CALL EchoProgress('Setting supply to elements ... ',lAdvance=.FALSE.)
    
    !Set supply
    SELECT CASE(SupplyType)
        CASE (Supply_Diversion_Ag)
            RootZone%ElemSupply%Diversion_Ag = Supply
        
        CASE (Supply_Diversion_Urb)
            RootZone%ElemSupply%Diversion_Urb = Supply

        CASE (Supply_Pumping_Ag)
            RootZone%ElemSupply%Pumping_Ag = Supply

        CASE (Supply_Pumping_Urb)
            RootZone%ElemSupply%Pumping_Urb = Supply

        CASE (Supply_UpstrmElemRunoff)
            RootZone%ElemSupply%UpstrmRunoff = Supply

    END SELECT
      
    CALL EchoProgress('DONE')
   
  END SUBROUTINE RootZone_v41_SetSupplyToElem
  
  
  ! -------------------------------------------------------------
  ! --- SET THE LAKE ELEMENT FLAG
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_SetLakeElemFlag(RootZone,iLakeElem)
    CLASS(RootZone_v41_Type) :: RootZone
    INTEGER,INTENT(IN)       :: iLakeElem(:)
    
    RootZone%Flags%lLakeElems(iLakeElem) = .TRUE.
    
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
      RootZone%NonPondedAgRootZone%Crops(:,iLakeElem)%SoilM_Precip   = 0.0
      RootZone%NonPondedAgRootZone%Crops(:,iLakeElem)%SoilM_AW       = 0.0
      RootZone%NonPondedAgRootZone%Crops(:,iLakeElem)%SoilM_Precip_P = 0.0
      RootZone%NonPondedAgRootZone%Crops(:,iLakeElem)%SoilM_AW_P     = 0.0
    END IF
    IF (RootZone%Flags%lPondedAg_Defined) THEN
      RootZone%PondedAgRootZone%Crops(:,iLakeElem)%SoilM_Precip   = 0.0
      RootZone%PondedAgRootZone%Crops(:,iLakeElem)%SoilM_AW       = 0.0
      RootZone%PondedAgRootZone%Crops(:,iLakeElem)%SoilM_Precip_P = 0.0
      RootZone%PondedAgRootZone%Crops(:,iLakeElem)%SoilM_AW_P     = 0.0
    END IF
    IF (RootZone%Flags%lUrban_Defined) THEN
      RootZone%UrbanRootZone%UrbData(iLakeElem)%SoilM_Precip   = 0.0
      RootZone%UrbanRootZone%UrbData(iLakeElem)%SoilM_AW       = 0.0
      RootZone%UrbanRootZone%UrbData(iLakeElem)%SoilM_Precip_P = 0.0
      RootZone%UrbanRootZone%UrbData(iLakeElem)%SoilM_AW_P     = 0.0
    END IF
    IF (RootZone%Flags%lNVRV_Defined) THEN
      RootZone%NVRVRootZone%NativeVeg(iLakeElem)%SoilM_Precip     = 0.0
      RootZone%NVRVRootZone%NativeVeg(iLakeElem)%SoilM_AW         = 0.0
      RootZone%NVRVRootZone%NativeVeg(iLakeElem)%SoilM_Precip_P   = 0.0
      RootZone%NVRVRootZone%NativeVeg(iLakeElem)%SoilM_AW_P       = 0.0
      RootZone%NVRVRootZone%RiparianVeg(iLakeElem)%SoilM_Precip   = 0.0
      RootZone%NVRVRootZone%RiparianVeg(iLakeElem)%SoilM_AW       = 0.0
      RootZone%NVRVRootZone%RiparianVeg(iLakeElem)%SoilM_Precip_P = 0.0
      RootZone%NVRVRootZone%RiparianVeg(iLakeElem)%SoilM_AW_P     = 0.0
    END IF
    
    RootZone%ElemPrecipData(iLakeElem)%PrecipFactor = 0.0
    
  END SUBROUTINE RootZone_v41_SetLakeElemFlag

  
  
  
  
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
  SUBROUTINE RootZone_v41_ReadRestartData(RootZone,InFile,iStat)
    CLASS(RootZone_v41_Type) :: RootZone
    TYPE(GenericFileType)    :: InFile
    INTEGER,INTENT(OUT)      :: iStat
    
    CALL InFile%ReadData(RootZone%RSoilM_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(RootZone%RSoilM,iStat)    ;  IF (iStat .EQ. -1) RETURN
    
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        CALL RootZone%NonPondedAgRootZone%ReadRestartData(InFile,iStat)  
        IF (iStat .EQ. -1) RETURN  
    END IF
    
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        CALL RootZone%PondedAgRootZone%ReadRestartData(InFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%UrbanRootZone%ReadRestartData(InFile,iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    IF (RootZone%Flags%lNVRV_Defined) CALL RootZone%NVRVRootZone%ReadRestartData(InFile,iStat)  
    
  END SUBROUTINE RootZone_v41_ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ ROOT ZONE RELATED TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_ReadTSData(RootZone,AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas)
    CLASS(RootZone_v41_Type),TARGET    :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ETData
    INTEGER,INTENT(OUT)                :: iStat
    REAL(8),OPTIONAL,INTENT(IN)        :: RegionLUAreas(:,:)  !In (region, land use) format
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23)     :: ThisProcedure = ModName // 'RootZone_v41_ReadTSData'
    INTEGER                          :: indxElem,indxLU,NElements,NLandUse,indxForNV
    REAL(8)                          :: Area,Demand,rCurrentDateAndTimeJulian,t1(AppGrid%NElements)
    LOGICAL                          :: lReturnFracUpdated,lReuseFracUpdated,lAgWaterDemandUpdated,lIrigPeriodUpdated
    TYPE(GenericLandUseType),POINTER :: pCrop
    CHARACTER(LEN=9),ALLOCATABLE     :: cLUCodes(:)
    REAL(8),ALLOCATABLE              :: ElemObsAreas1(:,:),ExIntAreas(:,:)
    
    !Initialize
    iStat     = 0
    NElements = AppGrid%NElements

    !Read return flow fractions
    CALL ReadReturnFlowFractions(TimeStep,RootZone%ReturnFracFile,lReturnFracUpdated,iStat)
    IF (iStat .EQ. -1) RETURN

    !Read re-use fractions
    CALL ReadReuseFractions(TimeStep,RootZone%ReuseFracFile,lReuseFracUpdated,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read irrigation period data
    CALL ReadIrigPeriodData(TimeStep,RootZone%IrigPeriodFile,lIrigPeriodUpdated,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read generic moisture data
    IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN
        CALL RootZone%GenericMoistureData%ReadTSData(TimeStep,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
      
    !Non-ponded crops related data
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        CALL NonPondedAgLandUse_ReadTSData(TimeStep,AppGrid,RootZone%IrigPeriodFile,RootZone%ElemSoilsData%WiltingPoint,RootZone%ElemSoilsData%FieldCapacity,RootZone%NonPondedAgRootZone,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !Ponded ag related data
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        CALL PondedAgLandUse_ReadTSData(TimeStep,AppGrid,ETData,RootZone%PondedAgRootZone,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Urban lands related data
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL UrbanLandUse_ReadTSData(TimeStep,AppGrid,RootZone%UrbanRootZone,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Native and riparian veg data
    IF (RootZone%Flags%lNVRV_Defined) THEN
        CALL NativeRiparianLandUse_ReadTSData(TimeStep,AppGrid,RootZone%NVRVRootZone,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !If subregional land-use areas are defined, update cell-level land-use areas
    IF (PRESENT(RegionLUAreas)) THEN
        !Allocate memory
        ALLOCATE (cLUCodes(SIZE(RegionLUAreas,DIM=2)) , ElemObsAreas1(SIZE(RegionLUAreas,DIM=2),NElements) , ExIntAreas(SIZE(RegionLUAreas,DIM=2),NElements))
        
        !Compile land use codes and cell-level land use areas
        NLandUse = 0
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
            NLandUse                    = RootZone%NonPondedAgRootZone%NCrops
            cLUCodes(1:NLandUse)        = RootZone%NonPondedAgRootZone%CropCodes
            ElemObsAreas1(1:NLandUse,:) = RootZone%NonPondedAgRootZone%Crops%Area
        END IF
        IF (RootZone%Flags%lPondedAg_Defined) THEN
            cLUCodes(NLandUse+1:NLandUse+RootZone%PondedAgRootZone%NCrops)        = RootZone%PondedAgRootZone%CropCodes
            ElemObsAreas1(NLandUse+1:NLandUse+RootZone%PondedAgRootZone%NCrops,:) = RootZone%PondedAgRootZone%Crops%Area
            NLandUse                                                              = NLandUse + RootZone%PondedAgRootZone%NCrops
        END IF
        IF (RootZone%Flags%lUrban_Defined) THEN
            cLUCodes(NLandUse+1)        = 'UR'
            ElemObsAreas1(NLandUse+1,:) = RootZone%UrbanRootZone%UrbData%Area
            NLandUse                    = NLandUse + 1
        END IF
        IF (RootZone%Flags%lNVRV_Defined) THEN
            cLUCodes(NLandUse+1:)       = ['NV','RV']
            ElemObsAreas1(NLandUse+1,:) = RootZone%NVRVRootZone%NativeVeg%Area
            ElemObsAreas1(NLandUse+2,:) = RootZone%NVRVRootZone%RiparianVeg%Area
        END IF
        
        !Index for native veg
        indxForNV = NLandUse + 1
        
        !Julian time for current date and time and interplotaion times for all elements
        rCurrentDateAndTimeJulian = TimeStampToJulian(TimeStep%CurrentDateAndTime)
        t1                        = rCurrentDateAndTimeJulian
                
        !Calculate cell level land use areas
        CALL ElementLU_InterpolateExtrapolate(AppGrid,cLUCodes,iMeasuredLUDataForSubregion,indxForNV,RootZone%Flags%lLakeElems,rCurrentDateAndTimeJulian,t1,t1,RegionLUAreas,ElemObsAreas1,ElemObsAreas1,ExIntAreas,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Update cell level land use areas
        NLandUse = 0
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
            NLandUse                                = RootZone%NonPondedAgRootZone%NCrops
            RootZone%NonPondedAgRootZone%Crops%Area = ExIntAreas(1:NLandUse,:) 
        END IF
        IF (RootZone%Flags%lPondedAg_Defined) THEN
            RootZone%PondedAgRootZone%Crops%Area = ExIntAreas(NLandUse+1:NLandUse+RootZone%PondedAgRootZone%NCrops,:) 
            NLandUse                             = NLandUse + RootZone%PondedAgRootZone%NCrops
        END IF
        IF (RootZone%Flags%lUrban_Defined) THEN
            RootZone%UrbanRootZone%UrbData%Area = ExIntAreas(NLandUse+1,:) 
            NLandUse                            = NLandUse + 1
        END IF
        IF (RootZone%Flags%lNVRV_Defined) THEN
            RootZone%NVRVRootZone%NativeVeg%Area   = ExIntAreas(NLandUse+1,:)
            RootZone%NVRVRootZone%RiparianVeg%Area = ExIntAreas(NLandUse+2,:)  
        END IF
    END IF

    !Process land use areas
    CALL ProcessLandUseAreas(AppGrid,TimeStep,RootZone,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Update urban element demend fractions w.r.t urban area
    CALL UrbanLandUse_ComputeWaterDemand(RootZone%UrbanRootZone)
                
    !Agricultural water demand data
    IF (RootZone%Flags%lReadNonPondedAgWaterDemand .OR. RootZone%Flags%lReadPondedAgWaterDemand) THEN
      CALL ReadAgWaterDemand(TimeStep,RootZone%AgWaterDemandFile,lAgWaterDemandUpdated,iStat)  ;  IF (iStat .EQ. -1) RETURN
      IF (lAgWaterDemandUpdated) RootZone%AgWaterDemandFile%rValues = RootZone%AgWaterDemandFile%rValues * RootZone%AgWaterDemandFactor
    END IF
    
    !Compute precipitation over each element
    IF (Precip%IsUpdated()) RootZone%ElemPrecipData%Precip = Precip%GetValues(RootZone%ElemPrecipData%iColPrecip) * RootZone%ElemPrecipData%PrecipFactor

    !Compute regional potential ET for each land use, if needed
    IF (ETData%IsUpdated()) THEN
        IF (RootZone%Flags%lNonPondedAg_Defined) CALL ComputeRegionalETPot(AppGrid,ETData,RootZone%NonPondedAgRootZone%Crops%iColETc,RootZone%NonPondedAgRootZone%Crops%Area,RootZone%NonPondedAgRootZone%RegionETPot)
        IF (RootZone%Flags%lPondedAg_Defined)    CALL ComputeRegionalETPot(AppGrid,ETData,RootZone%PondedAgRootZone%Crops%iColETc,RootZone%PondedAgRootZone%Crops%Area,RootZone%PondedAgRootZone%RegionETPot)
        IF (RootZone%Flags%lUrban_Defined)       CALL ComputeRegionalETPot(AppGrid,ETData,RootZone%UrbanRootZone%UrbData%iColETc,RootZone%UrbanRootZone%UrbData%Area,RootZone%UrbanRootZone%RegionETPot)
        IF (RootZone%Flags%lNVRV_Defined) THEN
            CALL ComputeRegionalETPot(AppGrid,ETData,RootZone%NVRVRootZone%NativeVeg%iColETc,RootZone%NVRVRootZone%NativeVeg%Area,RootZone%NVRVRootZone%RegionETPot_NV)
            CALL ComputeRegionalETPot(AppGrid,ETData,RootZone%NVRVRootZone%RiparianVeg%iColETc,RootZone%NVRVRootZone%RiparianVeg%Area,RootZone%NVRVRootZone%RegionETPot_RV)
        END IF
    END IF
    
    !Make sure that re-use fraction is not larger than return flow factor
    IF (lReturnFracUpdated .OR. lReuseFracUpdated) THEN
      ASSOCIATE (pReturnFrac  => RootZone%ReturnFracFile%rValues                  , &
                 pReuseFrac   => RootZone%ReuseFracFile%rValues                   , &
                 pCrops       => RootZone%NonPondedAgRootZone%Crops               , &
                 pCropCodes   => RootZone%NonPondedAgRootZone%CropCodes           , &
                 pUrban       => RootZone%UrbanRootZone%UrbData                   ) 
      
        !Non-ponded ag
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
          DO indxElem=1,NElements
            DO indxLU=1,RootZone%NonPondedAgRootZone%NCrops
              IF (pReturnFrac(pCrops(indxLU,indxElem)%iColReturnFrac) .LT. pReuseFrac(pCrops(indxLU,indxElem)%iColReuseFrac)) THEN
                MessageArray(1) = 'Agricultural re-use fraction for crop ' //TRIM(pCropCodes(indxLU))//' at element '//TRIM(IntToText(indxElem))//' is greater than return flow fraction!'
                WRITE (MessageArray(2),'(A,F5.3)') 'Re-use fraction      = ',pReuseFrac(pCrops(indxLU,indxElem)%iColReuseFrac)
                WRITE (MessageArray(3),'(A,F5.3)') 'Return flow fraction = ',pReturnFrac(pCrops(indxLU,indxElem)%iColReturnFrac)
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
              END IF
            END DO
          END DO
        END IF
          
        !Urban lands
        IF (RootZone%Flags%lUrban_Defined) THEN
          DO indxElem=1,NElements
            IF (pReturnFrac(pUrban(indxElem)%iColReturnFrac) .LT. pReuseFrac(pUrban(indxElem)%iColReuseFrac)) THEN
              MessageArray(1) = 'Urban re-use fraction at element '//TRIM(IntToText(indxElem))//' is greater than return flow fraction!'
              WRITE (MessageArray(2),'(A,F5.3)') 'Re-use fraction      = ',pReuseFrac(pUrban(indxElem)%iColReuseFrac)
              WRITE (MessageArray(3),'(A,F5.3)') 'Return flow fraction = ',pReturnFrac(pUrban(indxElem)%iColReturnFrac)
              CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
              iStat = -1
              RETURN
            END IF
          END DO
        END IF
        
      END ASSOCIATE
    END IF
    
    !Make sure that ag demand (if read from file) is zero when area is zero
    IF (RootZone%Flags%lReadNonPondedAgWaterDemand .OR. RootZone%Flags%lReadPondedAgWaterDemand) THEN
      IF (lAgWaterDemandUpdated                                  .OR. &
          RootZone%NonPondedAgRootZone%LandUseDataFile%lUpdated  .OR. &
          RootZone%PondedAgRootZone%LandUseDataFile%lUpdated          ) THEN
        DO indxElem=1,NElements
          Area = SUM(RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Area) + SUM(RootZone%PondedAgRootZone%Crops(:,indxElem)%Area)
          IF (Area .EQ. 0.0) THEN
            Demand = SUM(RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Demand) + SUM(RootZone%PondedAgRootZone%Crops(:,indxElem)%Demand)
            IF (Demand .GT. 0.0) THEN
              MessageArray(1) = 'Agricultural water demand at element '//TRIM(IntToText(indxElem))//' is greater '
              MessageArray(2) = 'than zero when agricultural area is zero.'
              MessageArray(3) = '(This may be due to the element being specified as a lake element)'
              CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
              iStat = -1
              RETURN
            END IF
          END IF
        END DO
      END IF
    END IF
    
    !Make sure that urban demand is zero when urban area is zero
    IF (RootZone%Flags%lUrban_Defined) THEN
      IF (RootZone%UrbanRootZone%LandUseDataFile%lUpdated        .OR.  &
          RootZone%UrbanRootZone%PopulationFile%lUpdated         .OR.  &
          RootZone%UrbanRootZone%PerCapitaWaterUseFile%lUpdated        ) THEN
        DO indxElem=1,NElements
          IF (RootZone%UrbanRootZone%UrbData(indxElem)%Area .EQ. 0.0) THEN
            IF (RootZone%UrbanRootZone%UrbData(indxElem)%Demand .GT. 0.0) THEN
              MessageArray(1) = 'Urban water demand at element '//TRIM(IntToText(indxElem))//' is greater'
              MessageArray(2) = 'than zero when urban area is zero.'
              MessageArray(3) = '(This may be due to the element being specified as a lake element)'
              CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
              iStat = -1
              RETURN
            END IF
          END IF
        END DO
      END IF
    END IF
    
    !Don't go beyond this point if it is not the first time step
    IF (TimeStep%CurrentTimeStep .GT. 1) RETURN
    
    !Computations for first time step only
    !Zero out initial soil moisture where area is zero
    DO indxElem=1,NElements
      !Non-ponded crops
      IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        DO indxLU=1,RootZone%NonPondedAgRootZone%NCrops
          pCrop => RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%GenericLandUseType
          IF (pCrop%Area .EQ. 0.0) THEN
            pCrop%SoilM_Precip_P = 0.0
            pCrop%SoilM_AW_P     = 0.0
            pCrop%SoilM_Precip   = 0.0
            pCrop%SoilM_AW       = 0.0
          END IF  
        END DO
      END IF
      !Ponded crops
      IF (RootZone%Flags%lPondedAg_Defined) THEN
        DO indxLU=1,RootZone%PondedAgRootZone%NCrops
          pCrop => RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%GenericLandUseType
          IF (pCrop%Area .EQ. 0.0) THEN
            pCrop%SoilM_Precip_P = 0.0
            pCrop%SoilM_AW_P     = 0.0
            pCrop%SoilM_Precip   = 0.0
            pCrop%SoilM_AW       = 0.0
          END IF  
        END DO
      END IF  
      !Urban lands
      IF (RootZone%Flags%lUrban_Defined) THEN
        pCrop => RootZone%UrbanRootZone%UrbData(indxElem)%GenericLandUseType
        IF (pCrop%Area .EQ. 0.0) THEN
          pCrop%SoilM_Precip_P = 0.0
          pCrop%SoilM_AW_P     = 0.0
          pCrop%SoilM_Precip   = 0.0
          pCrop%SoilM_AW       = 0.0
        END IF
      END IF  
      !Native and riparian vegetation
      IF (RootZone%Flags%lNVRV_Defined) THEN
        pCrop => RootZone%NVRVRootZone%NativeVeg(indxElem)%GenericLandUseType
        IF (pCrop%Area .EQ. 0.0) THEN
          pCrop%SoilM_Precip_P = 0.0
          pCrop%SoilM_Precip   = 0.0
        END IF  
        pCrop => RootZone%NVRVRootZone%RiparianVeg(indxElem)%GenericLandUseType
        IF (pCrop%Area .EQ. 0.0) THEN
          pCrop%SoilM_Precip_P = 0.0
          pCrop%SoilM_Precip   = 0.0
        END IF   
      END IF   
    END DO

    !Convert initial soil mositure contents to depths
    CALL NonPondedAgLandUse_SoilMContent_To_Depth(NElements,RootZone%ElemSoilsData%TotalPorosity,RootZone%NonPondedAgRootZone,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL PondedAgLandUse_SoilMContent_To_Depth(NElements,RootZone%ElemSoilsData%TotalPorosity,RootZone%PondedAgRootZone)
    CALL UrbanLandUse_SoilMContent_To_Depth(NElements,RootZone%ElemSoilsData%TotalPorosity,RootZone%UrbanRootZone,iStat)              ;  IF (iStat .EQ. -1) RETURN
    CALL NativeRiparianLandUse_SoilMContent_To_Depth(NElements,RootZone%ElemSoilsData%TotalPorosity,RootZone%NVRVRootZone,iStat)      ;  IF (iStat .EQ. -1) RETURN
        
    !Make sure that soil moisture at the end of previous timestep is saved, in case it is updated due to chnage in land use area
    RootZone%NonPondedAgRootZone%Crops%SoilM_Precip_P_BeforeUpdate = RootZone%NonPondedAgRootZone%Crops%SoilM_Precip_P
    RootZone%NonPondedAgRootZone%Crops%SoilM_AW_P_BeforeUpdate     = RootZone%NonPondedAgRootZone%Crops%SoilM_AW_P
    RootZone%NonPondedAgRootZone%Crops%SoilM_Oth_P_BeforeUpdate    = RootZone%NonPondedAgRootZone%Crops%SoilM_Oth_P
    RootZone%PondedAgRootZone%Crops%SoilM_Precip_P_BeforeUpdate    = RootZone%PondedAgRootZone%Crops%SoilM_Precip_P
    RootZone%PondedAgRootZone%Crops%SoilM_AW_P_BeforeUpdate        = RootZone%PondedAgRootZone%Crops%SoilM_AW_P
    RootZone%PondedAgRootZone%Crops%SoilM_Oth_P_BeforeUpdate       = RootZone%PondedAgRootZone%Crops%SoilM_Oth_P
    RootZone%UrbanRootZone%UrbData%SoilM_Precip_P_BeforeUpdate     = RootZone%UrbanRootZone%UrbData%SoilM_Precip_P
    RootZone%UrbanRootZone%UrbData%SoilM_AW_P_BeforeUpdate         = RootZone%UrbanRootZone%UrbData%SoilM_AW_P
    RootZone%UrbanRootZone%UrbData%SoilM_Oth_P_BeforeUpdate        = RootZone%UrbanRootZone%UrbData%SoilM_Oth_P
    RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P_BeforeUpdate    = RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P
    RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P_BeforeUpdate        = RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P
    RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P_BeforeUpdate       = RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P
    RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P_BeforeUpdate  = RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P
    RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P_BeforeUpdate      = RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P
    RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P_BeforeUpdate     = RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P
    
    !Initial regional moisture storage
    RootZone%RSoilM_P = RegionalMoistStorage(AppGrid,RootZone)

  END SUBROUTINE RootZone_v41_ReadTSData
  
  
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

    lReuseFracUpdated = .FALSE.
    IF (FileReadCode .EQ. 0) THEN
        lReuseFracUpdated = .TRUE.
    ELSE
        lReuseFracUpdated = .FALSE.
    END IF
    
  END SUBROUTINE ReadReuseFractions
  
  
  ! -------------------------------------------------------------
  ! --- READ AG WATER DEMAND DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadAgWaterDemand(TimeStep,AgWaterDemandFile,lAgWaterDemandUpdated,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(RealTSDataInFileType)    :: AgWaterDemandFile
    LOGICAL,INTENT(OUT)           :: lAgWaterDemandUpdated
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode

    !Read data
    CALL ReadTSData(TimeStep,'Agricultural water supply requirements data',AgWaterDemandFile,FileReadCode,iStat)

    IF (FileReadCode .EQ. 0) THEN
        lAgWaterDemandUpdated = .TRUE.
    ELSE
        lAgWaterDemandUpdated = .FALSE.
    END IF

  END SUBROUTINE ReadAgWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- READ IRRIGATION PERIOD DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadIrigPeriodData(TimeStep,IrigPeriodDataFile,lIrigPeriodUpdated,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(IntTSDataInFileType)     :: IrigPeriodDataFile
    LOGICAL,INTENT(OUT)           :: lIrigPeriodUpdated
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode

    !Read data
    CALL ReadTSData(TimeStep,'Irrigation periods data',IrigPeriodDataFile,FileReadCode,iStat)

    IF (FileReadCode .EQ. 0) THEN
        lIrigPeriodUpdated = .TRUE.
    ELSE
        lIrigPeriodUpdated = .FALSE.
    END IF 

  END SUBROUTINE ReadIrigPeriodData

  
  



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
  SUBROUTINE RootZone_v41_PrintRestartData(RootZone,OutFile)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(GenericFileType)               :: OutFile
    
    CALL OutFile%WriteData(RootZone%RSoilM_P)
    CALL OutFile%WriteData(RootZone%RSoilM)
    
    IF (RootZone%Flags%lNonPondedAg_Defined) CALL RootZone%NonPondedAgRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lPondedAg_Defined) CALL RootZone%PondedAgRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lUrban_Defined) CALL RootZone%UrbanRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lNVRV_Defined)  CALL RootZone%NVRVRootZone%PrintRestartData(OutFile)
    
  END SUBROUTINE RootZone_v41_PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE TO PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation)
    CLASS(RootZone_v41_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETTYpe),INTENT(IN)       :: ETData          !Not used in this version
    TYPE(TimeStepType),INTENT(IN) :: TimeStep        !Not used
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    !Local variables
    REAL(8),DIMENSION(AppGrid%NElements)     :: DemandFracAg,ElemPrecip
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RPump_Ag,RPump_Urb, &
                                                RDeli_Ag,RDeli_Urb, &
                                                RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb,RUpstrmElemRunoff_NV,      &
                                                RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_Ag,RGenericMoist_Urb, &
                                                RGenericMoist_NV
                                                
    !Echo progress
    CALL EchoProgress('Printing results of root zone simulation')
    
    !Initialize
    ElemPrecip = RootZone%ElemPrecipData%Precip
    
    ASSOCIATE (pFlags            => RootZone%Flags                                , &
               prGenericMoisture => RootZone%GenericMoistureData%rGenericMoisture )

      !Compute total agricultural element demand fractions
      DemandFracAg = 0.0
      IF (pFlags%lNonPondedAg_Defined) DemandFracAg = SUM(RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac ,DIM=1)
      IF (pFlags%lPondedAg_Defined)    DemandFracAg = DemandFracAg + SUM(RootZone%PondedAgRootZone%Crops%ElemDemandFrac ,DIM=1)
     
      !Compute variables necessary for both land&water use and root zone budget files
      IF (pFlags%LWUseBudRawFile_Defined .OR. pFlags%RootZoneBudRawFile_Defined) THEN
        RPump_Ag              = RegionalPumping(AppGrid,RootZone,AgIndex)
        RPump_Urb             = RegionalPumping(AppGrid,RootZone,UrbIndex)
        RDeli_Ag              = RegionalDeliveries(AppGrid,RootZone,AgIndex)
        RDeli_Urb             = RegionalDeliveries(AppGrid,RootZone,UrbIndex)
        RUpstrmElemRunoff_Ag  = RegionalUpStrmElemFlow(AppGrid,RootZone,DemandFracAg,AgIndex)
        RUpstrmElemRunoff_Urb = RegionalUpStrmElemFlow(AppGrid,RootZone,DemandFracAg,UrbIndex)
        RUpstrmElemRunoff_NV  = RegionalUpStrmElemFlow(AppGrid,RootZone,DemandFracAg,NVIndex)
        RLUArea_Ag            = RegionalLUArea(AppGrid,RootZone,AgIndex)
        RLUArea_Urb           = RegionalLUArea(AppGrid,RootZone,UrbIndex)
        RLUArea_NV            = RegionalLUArea(AppGrid,RootZone,NVIndex)
        IF (pFlags%lGenericMoistureFile_Defined) THEN
            RGenericMoist_Ag  = RegionalGenericMoistInflow(AppGrid,RootZone,AgIndex)
            RGenericMoist_Urb = RegionalGenericMoistInflow(AppGrid,RootZone,UrbIndex)
            RGenericMoist_NV  = RegionalGenericMoistInflow(AppGrid,RootZone,NVIndex)
        ELSE
            RGenericMoist_Ag  = 0.0
            RGenericMoist_Urb = 0.0
            RGenericMoist_NV  = 0.0
        END IF
      END IF
          
      !Land and water use budget file
      IF (pFlags%LWUseBudRawFile_Defined) CALL WriteLWUseFlowsToBudRawFile(AppGrid,RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb,RootZone)
      
      !Root zone budget file
      IF (pFlags%RootZoneBudRawFile_Defined) CALL WriteRootZoneFlowsToBudRawFile(AppGrid,RPump_Ag,RDeli_Ag,RGenericMoist_Ag,RPump_Urb,RDeli_Urb,RGenericMoist_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb,RUpstrmElemRunoff_NV,RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_NV,RootZone)
      
      !Non-ponded ag results print-out
      CALL NonPondedAgLandUse_PrintResults(AppGrid,RootZone%ElemSupply,ElemPrecip,prGenericMoisture,RootZone%NonPondedAgRootZone)

      !Ponded ag results print-out
      CALL PondedAgLandUse_PrintResults(AppGrid,RootZone%ElemSupply,ElemPrecip,prGenericMoisture,RootZone%PondedAgRootZone)

      !Final moisture print-out
      IF (lEndOfSimulation) THEN
        IF (pFlags%FinalMoistureOutFile_Defined) CALL WriteFinalMoistures(AppGrid%NElements,RootZone)
      END IF

    END ASSOCIATE
    
  END SUBROUTINE RootZone_v41_PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FINAL MOISTURES
  ! -------------------------------------------------------------
  SUBROUTINE WriteFinalMoistures(NElements,RootZone)
    INTEGER,INTENT(IN)      :: NElements
    TYPE(RootZone_v41_Type) :: RootZone
    
    !Local variables
    INTEGER                      :: NCrops,indxCrop,indxElem
    CHARACTER(LEN=16)            :: cArrayNP(RootZone%NonPondedAgRootZone%NCrops+1), &
                                    cArrayP(NPondedCrops+1)                        , &
                                    cArrayU(2)                                     , &
                                    cArrayNVRV(3)
    CHARACTER                    :: FormatSpec*50
    
    ASSOCIATE (pOutFile     => RootZone%FinalMoistureOutfile , &
               pFlags       => RootZone%Flags                , &
               pNonPondedAg => RootZone%NonPondedAgRootZone  , &
               pPondedAg    => RootZone%PondedAgRootZone     , &
               pUrban       => RootZone%UrbanRootZone        , &
               pNVRV        => RootZone%NVRVRootZone         )
    
      !Non-ponded ag moistures
      IF (pFlags%lNonPondedAg_Defined) THEN
       
        !Initialize
        NCrops     = pNonPondedAg%NCrops
        FormatSpec = ''
        FormatSpec = '(' // TRIM(IntToText(NCrops+1)) // '(A16,2X))' 
    
        !Titles
        cArrayNP    = ''
        cArrayNP(1) = 'C             IE'
        DO indxCrop=1,NCrops
          cArrayNP(indxCrop+1) = 'SOILM[' // pNonPondedAg%CropCodes(indxCrop) // ']'
          cArrayNP(indxCrop+1) = ADJUSTR(cArrayNP(indxCrop+1))
        END DO
        CALL pOutFile%WriteData('C*******************************************************************************')
        CALL pOutFile%WriteData('C                           Final Soil Moisture Contents')                       
        CALL pOutFile%WriteData('C                          For Non-Ponded Agricultural Lands')
        CALL pOutFile%WriteData('C')                                                                              
        CALL pOutFile%WriteData('C   IE   ;   Element ID')                                                         
        CALL pOutFile%WriteData('C   SOILM;   Final root zone moisture content; [L/L]')
        CALL pOutFile%WriteData('C') 
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
        CALL pOutFile%WriteData(cArrayNP,FormatSpec=FormatSpec)
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
      
        !Values
        DO indxElem=1,NElements
          cArrayNP    = ''
          cArrayNP(1) = ADJUSTR(TRIM(IntToText(indxElem)))
          DO indxCrop=1,NCrops
            WRITE (cArrayNP(indxCrop+1),'(F8.6)') (pNonPondedAg%Crops(indxCrop,indxElem)%SoilM_Precip + pNonPondedAg%Crops(indxCrop,indxElem)%SoilM_AW + pNonPondedAg%Crops(indxCrop,indxElem)%SoilM_Oth) / pNonPondedAg%RootDepth(indxCrop)
          END DO
          CALL pOutFile%WriteData(cArrayNP,FormatSpec=FormatSpec)
        END DO
      END IF
    
      !Ponded ag moistures
      IF (pFlags%lPondedAg_Defined) THEN
       
        !Initialize
        FormatSpec = ''
        FormatSpec = '(' // TRIM(IntToText(NCrops)) // '(A16,2X))' 
    
        !Titles
        cArrayP     = ''
        cArrayP(1)  = 'C             IE'
        DO indxCrop=1,NPondedCrops
          cArrayP(indxCrop+1) = 'SOILM[' // TRIM(pPondedAg%CropCodes(indxCrop)) // ']'
          cArrayP(indxCrop+1) = ADJUSTR(cArrayP(indxCrop+1))
        END DO
        CALL pOutFile%WriteData('C*******************************************************************************')
        CALL pOutFile%WriteData('C                           Final Soil Moisture Contents')                       
        CALL pOutFile%WriteData('C                          For Ponded Agricultural Lands')
        CALL pOutFile%WriteData('C')                                                                              
        CALL pOutFile%WriteData('C   IE   ;   Element ID')                                                         
        CALL pOutFile%WriteData('C   SOILM;   Final root zone moisture content; [L/L]')
        CALL pOutFile%WriteData('C') 
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
        CALL pOutFile%WriteData(cArrayP,FormatSpec=FormatSpec)
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
      
        !Values
        DO indxElem=1,NElements
          cArrayP    = ''
          cArrayP(1) = ADJUSTR(TRIM(IntToText(indxElem)))
          DO indxCrop=1,NPondedCrops
            WRITE (cArrayP(indxCrop+1),'(F8.6)') (pPondedAg%Crops(indxCrop,indxElem)%SoilM_Precip + pPondedAg%Crops(indxCrop,indxElem)%SoilM_AW + pPondedAg%Crops(indxCrop,indxElem)%SoilM_Oth) / pPondedAg%RootDepth(indxCrop)
          END DO
          CALL pOutFile%WriteData(cArrayP,FormatSpec=FormatSpec)
        END DO
      END IF
      
      !Urban moistures
      IF (pFlags%lUrban_Defined) THEN
       
        !Initialize
        FormatSpec = ''
        FormatSpec = '(2(A16,2X))' 
    
        !Titles
        CALL pOutFile%WriteData('C*******************************************************************************')
        CALL pOutFile%WriteData('C                           Final Soil Moisture Contents')                       
        CALL pOutFile%WriteData('C                                For Urban Lands')
        CALL pOutFile%WriteData('C')                                                                              
        CALL pOutFile%WriteData('C   IE   ;   Element ID')                                                         
        CALL pOutFile%WriteData('C   SOILM;   Final root zone moisture content; [L/L]')
        CALL pOutFile%WriteData('C') 
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
        CALL pOutFile%WriteData('C             IE      SOILM[URBAN]')
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
      
        !Values
        DO indxElem=1,SIZE(pUrban%UrbData)
          cArrayU    = ''
          cArrayU(1) = ADJUSTR(TRIM(IntToText(indxElem)))
          WRITE (cArrayU(2),'(F8.6)') (pUrban%UrbData(indxElem)%SoilM_Precip + pUrban%UrbData(indxElem)%SoilM_AW + pUrban%UrbData(indxElem)%SoilM_Oth) / pUrban%RootDepth
          CALL pOutFile%WriteData(cArrayU,FormatSpec=FormatSpec)
        END DO
      END IF
      
      !Native and riparian veg moistures
      IF (pFlags%lNVRV_Defined) THEN
       
        !Initialize
        NCrops     = 2
        FormatSpec = ''
        FormatSpec = '(3(A16,2X))' 
    
        !Titles
        cArrayNVRV     = ''
        cArrayNVRV(1)  = 'C             IE'
        cArrayNVRV(2)  = 'SOILM[NV]'
        cArrayNVRV(3)  = 'SOILM[RV]'
        cArrayNVRV     = ADJUSTR(cArrayNVRV)
        CALL pOutFile%WriteData('C*******************************************************************************')
        CALL pOutFile%WriteData('C                           Final Soil Moisture Contents')                       
        CALL pOutFile%WriteData('C                      For Native and Riparian Vegetation Lands')
        CALL pOutFile%WriteData('C')                                                                              
        CALL pOutFile%WriteData('C   IE   ;   Element ID')                                                         
        CALL pOutFile%WriteData('C   SOILM;   Final root zone moisture content; [L/L]')
        CALL pOutFile%WriteData('C') 
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
        CALL pOutFile%WriteData(cArrayNVRV,FormatSpec=FormatSpec)
        CALL pOutFile%WriteData('C-------------------------------------------------------------------------------')
      
        !Values
        DO indxElem=1,SIZE(pNVRV%NativeVeg)
          cArrayNVRV    = ''
          cArrayNVRV(1) = ADJUSTR(TRIM(IntToText(indxElem)))
          WRITE (cArrayNVRV(2),'(F8.6)') (pNVRV%NativeVeg(indxElem)%SoilM_Precip + pNVRV%NativeVeg(indxElem)%SoilM_AW + pNVRV%NativeVeg(indxElem)%SoilM_Oth) / pNVRV%RootDepth_Native
          WRITE (cArrayNVRV(3),'(F8.6)') (pNVRV%RiparianVeg(indxElem)%SoilM_Precip + pNVRV%RiparianVeg(indxElem)%SoilM_AW + pNVRV%RiparianVeg(indxElem)%SoilM_Oth) / pNVRV%RootDepth_Riparian
          CALL pOutFile%WriteData(cArrayNVRV,FormatSpec=FormatSpec)
        END DO
      END IF
      
    END ASSOCIATE
    
  END SUBROUTINE WriteFinalMoistures
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO ROOT ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToBudRawFile(AppGrid,RPump_Ag,RDeli_Ag,RGenericMoist_Ag,RPump_Urb,RDeli_Urb,RGenericMoist_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb,RUpstrmElemRunoff_NV,RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_NV,RootZone)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RPump_Ag,RDeli_Ag,RGenericMoist_Ag,RPump_Urb,RDeli_Urb,RGenericMoist_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb,RUpstrmElemRunoff_NV,RLUArea_Ag,RLUArea_Urb,RLUArea_NV,RGenericMoist_NV
    TYPE(RootZone_v41_Type)         :: RootZone
    
    !Local variables
    INTEGER                                  :: NRegions,NElements
    REAL(8)                                  :: DummyArray(NRootZoneBudColumns,(AppGrid%NSubregions+1)) 
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RRunoff_Ag,RRunoff_Urb,RRunoff_NV,                   &
                                                RPrecip_Ag,RPrecip_Urb,RPrecip_NV,                   &
                                                RReuse_Ag,RReuse_Urb,                                &
                                                RReturn_Ag,RReturn_Urb,                              &
                                                RDrain_Ag,                                           &
                                                RSoilMCh_Ag,RSoilMCh_Urb,RSoilMCh_NV,                &
                                                RInfilt_Ag,RInfilt_Urb,RInfilt_NV,                   &
                                                RGWInflow_Ag,RGWInflow_Urb,RGWInflow_NV,             &
                                                RRVETFromStrm_NV,                                    &
                                                RETPot_Ag,RETPot_Urb,RETPot_NV,                      &
                                                RETa_Ag,RETa_Urb,RETa_NV,                            &
                                                RPerc_Ag,RPerc_Urb,RPerc_NV,                         &
                                                Error_Ag,Error_Urb,Error_NV
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    NElements = AppGrid%NElements
    
    RETPot_Ag         = 0.0
    RPrecip_Ag        = 0.0
    RRunoff_Ag        = 0.0
    RReuse_Ag         = 0.0
    RReturn_Ag        = 0.0
    RDrain_Ag         = 0.0
    RSoilMCh_Ag       = 0.0
    RInfilt_Ag        = 0.0
    RETa_Ag           = 0.0
    RPerc_Ag          = 0.0
    Error_Ag          = 0.0
    RETPot_Urb        = 0.0
    RPrecip_Urb       = 0.0
    RRunoff_Urb       = 0.0
    RReuse_Urb        = 0.0
    RReturn_Urb       = 0.0
    RSoilMCh_Urb      = 0.0
    RInfilt_Urb       = 0.0
    RGWInflow_Ag      = 0.0    
    RGWInflow_Urb     = 0.0    
    RGWInflow_NV      = 0.0 
    RRVETFromStrm_NV  = 0.0
    RETa_Urb          = 0.0
    RPerc_Urb         = 0.0
    Error_Urb         = 0.0
    RETPot_NV         = 0.0
    RRunoff_NV        = 0.0
    RPrecip_NV        = 0.0
    RSoilMCh_NV       = 0.0
    RInfilt_NV        = 0.0
    RETa_NV           = 0.0
    RPerc_NV          = 0.0
    Error_NV          = 0.0
    
    !Regional moisture storages
    RootZone%RSoilM = RegionalMoistStorage(AppGrid,RootZone)
    
    ASSOCIATE (pFlags => RootZone%Flags)
               
      !Compute subregional values
      !---------------------------
      !Ponded and non-ponded ag
      IF (pFlags%lNonPondedAg_Defined .OR. pFlags%lPondedAg_Defined) THEN
          CALL RegionalETPot(AppGrid,RootZone,AgIndex,RETPot_Ag)
          RPrecip_Ag   = RegionalPrecip(AppGrid,RootZone,AgIndex)       
          RRunoff_Ag   = RegionalRunoff(AppGrid,RootZone,AgIndex)       
          RReuse_Ag    = RegionalReuse(AppGrid,RootZone,AgIndex)       
          RReturn_Ag   = RegionalReturn(AppGrid,RootZone,AgIndex)       
          RSoilMCh_Ag  = RegionalSoilMChange(AppGrid,RootZone,AgIndex) 
          RInfilt_Ag   = RegionalInfiltration(AppGrid,RootZone,AgIndex)
          RDrain_Ag    = RegionalDrain(AppGrid,RootZone,AgIndex)       
          RETa_Ag      = RegionalETa(AppGrid,RootZone,AgIndex)        
          RPerc_Ag     = RegionalPerc(AppGrid,RootZone,AgIndex)       
          RGWInflow_Ag = RegionalGWInflow(AppGrid,RootZone,AgIndex)       
          Error_Ag     = RootZone%RSoilM_P(:,AgIndex) + RSoilMCh_Ag + RInfilt_Ag + RGWInflow_Ag + RGenericMoist_Ag - RDrain_Ag - RETa_Ag - RPerc_Ag - RootZone%RSoilM(:,AgIndex)
      END IF
            
      !Urban
      IF (pFlags%lUrban_Defined) THEN
          CALL RegionalETPot(AppGrid,RootZone,UrbIndex,RETPot_Urb)
          RPrecip_Urb   = RegionalPrecip(AppGrid,RootZone,UrbIndex)       
          RRunoff_Urb   = RegionalRunoff(AppGrid,RootZone,UrbIndex)       
          RReuse_Urb    = RegionalReuse(AppGrid,RootZone,UrbIndex)       
          RReturn_Urb   = RegionalReturn(AppGrid,RootZone,UrbIndex)       
          RSoilMCh_Urb  = RegionalSoilMChange(AppGrid,RootZone,UrbIndex)        
          RInfilt_Urb   = RegionalInfiltration(AppGrid,RootZone,UrbIndex)       
          RETa_Urb      = RegionalETa(AppGrid,RootZone,UrbIndex)       
          RPerc_Urb     = RegionalPerc(AppGrid,RootZone,UrbIndex)       
          RGWInflow_Urb = RegionalGWInflow(AppGrid,RootZone,UrbIndex)       
          Error_Urb     = RootZone%RSoilM_P(:,UrbIndex) + RSoilMCh_Urb + RInfilt_Urb + RGWInflow_Urb + RGenericMoist_Urb - RETa_Urb - RPerc_Urb - RootZone%RSoilM(:,UrbIndex)
      END IF
      
      !Native and riparian veg
      IF (pFlags%lNVRV_Defined) THEN
          CALL RegionalETPot(AppGrid,RootZone,NVIndex,RETPot_NV)
          RPrecip_NV       = RegionalPrecip(AppGrid,RootZone,NVIndex)          
          RRunoff_NV       = RegionalRunoff(AppGrid,RootZone,NVIndex)
          RSoilMCh_NV      = RegionalSoilMChange(AppGrid,RootZone,NVIndex) 
          RInfilt_NV       = RegionalInfiltration(AppGrid,RootZone,NVIndex)
          CALL NativeRiparianLandUse_GetRegionalRVETFromStrm(AppGrid,RootZone%NVRVRootZone,RRVETFromStrm_NV)
          RETa_NV          = RegionalETa(AppGrid,RootZone,NVIndex)  
          RPerc_NV         = RegionalPerc(AppGrid,RootZone,NVIndex)
          RGWInflow_NV     = RegionalGWInflow(AppGrid,RootZone,NVIndex)
          Error_NV         = RootZone%RSoilM_P(:,NVIndex) + RSoilMCh_NV + RInfilt_NV + RGWInflow_NV + RGenericMoist_NV + RRVETFromStrm_NV - RETa_NV - RPerc_NV - RootZone%RSoilM(:,NVIndex)
      END IF
      
    END ASSOCIATE
    
    !Store in temporary array
    DummyArray(1,:)  = RLUArea_Ag                                                  !Agricultural area
    DummyArray(2,:)  = RETPot_Ag                                                   !Potential ET on ag lands
    DummyArray(3,:)  = RPrecip_Ag                                                  !Precipitation on ag lands
    DummyArray(4,:)  = RRunoff_Ag                                                  !Runoff from ag lands
    DummyArray(5,:)  = RDeli_Ag + RPump_Ag                                         !Prime applied water on ag lands prior to application of re-use water
    DummyArray(6,:)  = RUpstrmElemRunoff_Ag                                        !Surface runoff from upstream elements/subregions used on ag lands
    DummyArray(7,:)  = RReuse_Ag                                                   !Applied recycled water on ag lands 
    DummyArray(8,:)  = RReturn_Ag                                                  !Return flow from ag lands
    DummyArray(9,:)  = RootZone%RSoilM_P(:,AgIndex)                                !Storage at the beginning of the time interval
    DummyArray(10,:) = RSoilMCh_Ag                                                 !Soil moisture change due to expansion/contraction of ag lands
    DummyArray(11,:) = RInfilt_Ag                                                  !Infiltration on ag lands
    DummyArray(12,:) = RGWInflow_Ag                                                !Groundwater inflow on ag lands
    DummyArray(13,:) = RGenericMoist_Ag                                            !Generic moisture inflow to ag lands
    DummyArray(14,:) = RDrain_Ag                                                   !Rice/refuge pond drainage on ag lands
    DummyArray(15,:) = RETa_Ag                                                     !ET on ag lands
    DummyArray(16,:) = RPerc_Ag                                                    !Percolation on ag lands
    DummyArray(17,:) = RootZone%RSoilM(:,AgIndex)                                  !Storage at the end of the time interval
    DummyArray(18,:) = Error_Ag                                                    !Mass balance error for ag lands
    DummyArray(19,:) = RLUArea_Urb                                                 !Urban area
    DummyArray(20,:) = RETPot_Urb                                                  !Potential ET on urban lands
    DummyArray(21,:) = RPrecip_Urb                                                 !Precipitation on urban lands
    DummyArray(22,:) = RRunoff_Urb                                                 !Runoff from urban lands
    DummyArray(23,:) = RDeli_Urb + RPump_Urb                                       !Prime applied water on urban lands prior to re-used water
    DummyArray(24,:) = RUpstrmElemRunoff_Urb                                       !Surface runoff from upstream elements/subregions used on urban lands
    DummyArray(25,:) = RReuse_Urb                                                  !Applied recycled water on urban indoors and outdoors
    DummyArray(26,:) = RReturn_Urb                                                 !Return flow from urban lands
    DummyArray(27,:) = RootZone%RSoilM_P(:,UrbIndex)                               !Storage at the beginning of the time interval
    DummyArray(28,:) = RSoilMCh_Urb                                                !Soil moisture change due to expansion/contraction of urban lands
    DummyArray(29,:) = RInfilt_Urb                                                 !Infiltration on urban lands
    DummyArray(30,:) = RGWInflow_Urb                                               !Groundwater inflow on urban lands
    DummyArray(31,:) = RGenericMoist_Urb                                           !Generic moisture inflow to urban lands
    DummyArray(32,:) = RETa_Urb                                                    !ET on urban lands
    DummyArray(33,:) = RPerc_Urb                                                   !Percolation on urban lands
    DummyArray(34,:) = RootZone%RSoilM(:,UrbIndex)                                 !Storage at the end of the time interval     
    DummyArray(35,:) = Error_Urb                                                   !Mass balance error at urban lands
    DummyArray(36,:) = RLUArea_NV                                                  !Natural area
    DummyArray(37,:) = RETPot_NV                                                   !Potential ET on natural lands
    DummyArray(38,:) = RPrecip_NV                                                  !Precipitation on natural lands
    DummyArray(39,:) = RUpstrmElemRunoff_NV                                        !Runoff from upstream elements onto natural lands
    DummyArray(40,:) = RRunoff_NV                                                  !Total surface flow on natural lands
    DummyArray(41,:) = RootZone%RSoilM_P(:,NVIndex)                                !Storage at the beginning of the time interval
    DummyArray(42,:) = RSoilMCh_NV                                                 !Soil moisture change due to expansion/contraction of natural lands
    DummyArray(43,:) = RInfilt_NV                                                  !Infiltration on natural lands
    DummyArray(44,:) = RGWInflow_NV                                                !Groundwater inflow on natural lands
    DummyArray(45,:) = RGenericMoist_NV                                            !Generic moisture inflow to natural lands
    DummyArray(46,:) = RRVETFromStrm_NV                                            !Stream inflow to meet riparian ET
    DummyArray(47,:) = RETa_NV                                                     !ET on natural lands
    DummyArray(48,:) = RPerc_NV                                                    !Percolation on natural lands
    DummyArray(49,:) = RootZone%RSoilM(:,NVIndex)                                  !Storage at the end of the time interval          
    DummyArray(50,:) = Error_NV                                                    !Mass balance error at native and riparian lands

    !Print out values to binary file
    CALL RootZone%RootZoneBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteRootZoneFlowsToBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO LAND & WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToBudRawFile(AppGrid,RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb,RootZone)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb
    TYPE(RootZone_v41_Type)         :: RootZone
    
    !Local variables
    REAL(8)                                  :: DummyArray(NLWUseBudColumns,(AppGrid%NSubregions+1))
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RDemandRaw_Ag,RDemand_Ag,RDemand_Urb, &
                                                RDemandShort_Ag,RDemandShort_Urb,     &
                                                RETAW,RETP,RETOth,RETGW
    
    !Compute budget terms
    IF (RootZone%Flags%lNonPondedAg_Defined .OR. RootZone%Flags%lPondedAg_Defined) THEN
      RDemandRaw_Ag   = RegionalAgRawDemand(AppGrid,RootZone)
      RDemand_Ag      = RegionalDemand(AppGrid,RootZone,AgIndex)
      RDemandShort_Ag = RDemand_Ag - RPump_Ag - RDeli_Ag - RUpstrmElemRunoff_Ag
      RETAW           = RegionalETAW(AppGrid,RootZone)
      RETP            = RegionalETP(AppGrid,RootZone)
      RETOth          = RegionalETOth(AppGrid,RootZone)
      RETGW           = RegionalETGW(AppGrid,RootZone)
    ELSE
      RDemandRaw_Ag   = 0.0
      RDemand_Ag      = 0.0
      RDemandShort_Ag = 0.0
      RETAW           = 0.0
      RETP            = 0.0
      RETOth          = 0.0
      RETGW           = 0.0
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
      RDemand_Urb      = RegionalDemand(AppGrid,RootZone,UrbIndex)
      RDemandShort_Urb = RDemand_Urb - RPump_Urb - RDeli_Urb - RUpstrmElemRunoff_Urb
    ELSE
      RDemand_Urb      = 0.0
      RDEmandShort_Urb = 0.0
    END IF
    
    !Store in temporary array
    DummyArray(1,:)  = RLUArea_Ag
    DummyArray(2,:)  = RDemandRaw_Ag
    DummyArray(3,:)  = RDemand_Ag
    DummyArray(4,:)  = RPump_Ag
    DummyArray(5,:)  = RDeli_Ag
    DummyArray(6,:)  = RUpstrmElemRunoff_Ag
    DummyArray(7,:)  = RDemandShort_Ag
    DummyArray(8,:)  = RETAW
    DummyArray(9,:)  = RETP
    DummyArray(10,:) = RETGW
    DummyArray(11,:) = RETOth
    DummyArray(12,:) = RLUArea_Urb
    DummyArray(13,:) = RDemand_Urb
    DummyArray(14,:) = RPump_Urb
    DummyArray(15,:) = RDeli_Urb
    DummyArray(16,:) = RUpstrmElemRunoff_Urb
    DummyArray(17,:) = RDemandShort_Urb

    !Print out values to binary file
    CALL RootZone%LWUseBudRawFile%WriteData(DummyArray)

    
  END SUBROUTINE WriteLWUseFlowsToBudRawFile


  
  
  
  
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
  SUBROUTINE RootZone_v41_ComputeWaterDemand(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v41_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Initialize
    iStat = 0
    
    !Return if root zone is not simulated
    IF (RootZone%NLands .EQ. 0) RETURN
    
    !Riparian ET demand from streams
    IF (RootZone%Flags%lNVRV_Defined)  THEN
      CALL EchoProgress('Computing riparian ET demand from streams...')
      CALL NativeRiparianLandUse_ComputeWaterDemand(AppGrid%NElements                            , &
                                                    ETData                                       , &
                                                    TimeStep%DeltaT                              , &
                                                    RootZone%ElemPrecipData%Precip               , &
                                                    RootZone%GenericMoistureData%rGenericMoisture, &
                                                    RootZone%ElemSoilsData                       , &
                                                    RootZone%SolverData                          , &
                                                    RootZone%Flags%lLakeElems                    , &
                                                    RootZone%NVRVRootZone                        , &
                                                    iStat                                        )
      IF (iStat .EQ. -1) RETURN
    END IF
    
    !Return if no need to compute ag water demand (either due to no ag crops are specified or ag water demand is read in from file)
    IF (.NOT. RootZone%Flags%lComputeAgWaterDemand) THEN
      CALL ComputeDemandSupplyRelatedFracs(AppGrid,RootZone)
      RETURN
    END IF
  
    !Echo progress
    CALL EchoProgress('Computing agricultural water demand...')

    !Compute ag water demand (urban water demands are read in as input)
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
      CALL NonPondedAgLandUse_ComputeWaterDemand(AppGrid                                      , &
                                                 ETData                                       , &
                                                 TimeStep%DeltaT                              , &
                                                 RootZone%ElemPrecipData%Precip               , &
                                                 RootZone%GenericMoistureData%rGenericMoisture, &
                                                 RootZone%ElemSoilsData                       , &
                                                 RootZone%AgWaterDemandFile%rValues           , &
                                                 RootZone%ReuseFracFile%rValues               , &
                                                 RootZone%ReturnFracFile%rValues              , &
                                                 RootZone%IrigPeriodFile%iValues              , &
                                                 RootZone%SolverData                          , &
                                                 RootZone%Flags%lLakeElems                    , &
                                                 RootZone%Flags%lReadNonPondedAgWaterDemand   , &
                                                 RootZone%NonPondedAgRootZone                 , &
                                                 iStat                                        )
      IF (iStat .EQ. -1) RETURN
    END IF
                                               
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) &
      CALL PondedAgLandUse_ComputeWaterDemand(AppGrid                                      , &
                                              ETData                                       , &
                                              TimeStep%DeltaT                              , &
                                              RootZone%ElemPrecipData%Precip               , &
                                              RootZone%GenericMoistureData%rGenericMoisture, &
                                              RootZone%ElemSoilsData                       , &
                                              RootZone%HydCondPonded                       , &
                                              RootZone%AgWaterDemandFile%rValues           , &
                                              RootZone%IrigPeriodFile%iValues              , &
                                              RootZone%Flags%lLakeElems                    , &
                                              RootZone%Flags%lReadPondedAgWaterDemand      , &
                                              RootZone%PondedAgRootZone                    )
    
    !Compute demand and supply related fractions
    CALL ComputeDemandSupplyRelatedFracs(AppGrid,RootZone)

  END SUBROUTINE RootZone_v41_ComputeWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SOIL MOISTURE IN ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_Simulate(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v41_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'RootZone_v41_Simulate'
    INTEGER                      :: indxElem,indxIter,NElements,indxElem1
    REAL(8)                      :: AchievedConv,DeltaT,Area,Runoff,Runoff_P,ElemGenSupply(AppGrid%NElements),   &
                                    IrigSupply_Ag(AppGrid%NElements),IrigSupply_Urb(AppGrid%NElements),          &
                                    UpstrmElemRunoff_P(AppGrid%NElements),SurfaceFlow(AppGrid%NElements),        &                                
                                    ElemCropSupply(RootZone%NonPondedAgRootZone%NCrops,AppGrid%NElements),       &
                                    ElemPondSupply(NPondedCrops,AppGrid%NElements)                                                                                          
    REAL(8),ALLOCATABLE          :: InRunoff(:,:)
    
    !Initialize
    iStat = 0

    ASSOCIATE (pFlags             => RootZone%Flags                                , &
               pElemSupply        => RootZone%ElemSupply                           , &
               pSoilsData         => RootZone%ElemSoilsData                        , &
               pElemPrecip        => RootZone%ElemPrecipData%Precip                , &
               pElemsToGW         => RootZone%ElemFlowToGW                         , &
               prGenericMoisture  => RootZone%GenericMoistureData%rGenericMoisture , &
               pReuseFracs        => RootZone%ReuseFracFile%rValues                , &
               pReturnFracs       => RootZone%ReturnFracFile%rValues               , &
               pSolverData        => RootZone%SolverData                           , &
               pNonPondedAg       => RootZone%NonPondedAgRootZone                  , &
               pPondedAg          => RootZone%PondedAgRootZone                     , &
               pUrbanLand         => RootZone%UrbanRootZone                        , &
               pNVRV              => RootZone%NVRVRootZone                         )
               
      !Initialize
      DeltaT                   = TimeStep%DeltaT
      pElemSupply%UpstrmRunoff = 0.0
      NElements                = AppGrid%NElements
      IrigSupply_Ag            = pElemSupply%Diversion_Ag  + pElemSupply%Pumping_Ag
      IrigSupply_Urb           = pElemSupply%Diversion_Urb + pElemSupply%Pumping_Urb
    
      !Check water supply vs. irrigable lands
      DO indxElem=1,NElements
        !If this is a lake element, report that to the user
        IF (RootZone%Flags%lLakeElems(indxElem)) THEN
          IF (IrigSupply_Ag(indxElem)+IrigSupply_Urb(indxElem) .GT. 0.0) THEN 
            MessageArray(1) = 'Element '//TRIM(IntToText(indxElem))//' is a lake element.'
            MessageArray(2) = 'Water supply for lake elements must be zero!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
          END IF
        END IF
        !Check ag area vs. ag water supply
        IF (IrigSupply_Ag(indxElem) .GT. 0.0) THEN
          Area = 0.0
          IF (pFlags%lNonPondedAg_Defined) Area = Area + SUM(pNonPondedAg%Crops(:,indxElem)%Area)
          IF (pFlags%lPondedAg_Defined)    Area = Area + SUM(pPondedAg%Crops(:,indxElem)%Area)
          IF (Area .EQ. 0.0) THEN
            MessageArray(1) = 'Agricultural applied water at element '//TRIM(IntToText(indxElem))//' cannot be non-zero'
            MessageArray(2) = 'when agricultural area is zero!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
          END IF
        END IF
        !Check urban area vs. urban water supply
        IF (IrigSupply_Urb(indxElem) .GT. 0.0) THEN
          IF (pFlags%lUrban_Defined) THEN
            Area = pUrbanLand%UrbData(indxElem)%Area
          ELSE
            Area = 0.0
          END IF
          IF (Area .EQ. 0.0) THEN
            MessageArray(1) = 'Urban applied water at element '//TRIM(IntToText(indxElem))//' cannot be non-zero'
            MessageArray(2) = 'when urban area is zero!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
          END IF
        END IF
      END DO
    
      !Iterative solution
      DO indxIter=1,pSolverData%IterMax
        !Store UpstrmElemRunoff values in temporary stoarge and zero it out
        UpstrmElemRunoff_P       = pElemSupply%UpstrmRunoff
        pElemSupply%UpstrmRunoff = 0.0
        
        !Simulate non-ponded ag lands
        IF (pFlags%lNonPondedAg_Defined) THEN
          CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,iLandUse_NonPonded,InRunoff)
          FORALL (indxElem1=1:NElements) ElemCropSupply(:,indxElem1) = InRunoff(:,indxElem1) + IrigSupply_Ag(indxElem1) * pNonPondedAg%Crops(:,indxElem1)%ElemDemandFrac_Ag      
          CALL NonPondedAgLandUse_Simulate(AppGrid              , &
                                           ETData               , &
                                           DeltaT               , &
                                           pElemPrecip          , &
                                           prGenericMoisture    , &
                                           pSoilsData           , &
                                           ElemCropSupply       , &
                                           pReuseFracs          , &
                                           pReturnFracs         , &
                                           pElemsToGW           , &
                                           pSolverData          , &
                                           pFlags%lLakeElems    , &
                                           pNonPondedAg         , &
                                           iStat                )
          IF (iStat .EQ. -1) RETURN
          SurfaceFlow = SUM(pNonPondedAg%Crops%Runoff + pNonPondedAg%Crops%ReturnFlow,DIM=1)
          CALL FlowToElements(SurfaceFlow               , &
                              AppGrid                   , &
                              RootZone                  , &
                              NVIndex                   , &
                              pElemSupply%UpstrmRunoff  )
        END IF
    
        !Simulate ponded ag lands
        IF (pFlags%lPondedAg_Defined) THEN  
          CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,iLandUse_Ponded,InRunoff)
          FORALL (indxElem1=1:NElements) ElemPondSupply(:,indxElem1) = InRunoff(:,indxElem1) + IrigSupply_Ag(indxElem1) * pPondedAg%Crops(:,indxElem1)%ElemDemandFrac_Ag       
          CALL PondedAgLandUse_Simulate(AppGrid                , &
                                        ETData                 , &
                                        DeltaT                 , &
                                        pElemPrecip            , &
                                        prGenericMoisture      , &
                                        pSoilsData             , &
                                        RootZone%HydCondPonded , &
                                        ElemPondSupply         , &
                                        pElemsToGW             , &
                                        pSolverData            , &
                                        pFlags%lLakeElems      , &
                                        pPondedAg              , &
                                        iStat                  )
          IF (iStat .EQ. -1) RETURN
          SurfaceFlow = SUM(pPondedAg%Crops%Runoff + pPondedAg%Crops%ReturnFlow + pPondedAg%Crops%Drain , DIM=1)
          CALL FlowToElements(SurfaceFlow                 , &
                              AppGrid                     , &
                              RootZone                    , &
                              NVIndex                     , &
                              pElemSupply%UpstrmRunoff    )
        END IF
        
        !Simulate urban lands
        IF (pFlags%lUrban_Defined) THEN 
          CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,iLandUse_Urban,InRunoff)
          ElemGenSupply = InRunoff(1,:) + IrigSupply_Urb      
          CALL UrbanLandUse_Simulate(AppGrid            , &
                                     ETData             , &
                                     DeltaT             , &
                                     pElemPrecip        , &
                                     prGenericMoisture  , &
                                     pSoilsData         , &
                                     ElemGenSupply      , &
                                     pReuseFracs        , &
                                     pReturnFracs       , &
                                     pElemsToGW         , &
                                     pSolverData        , &
                                     pFlags%lLakeElems  , &
                                     pUrbanLand         , &
                                     iStat              )
          IF (iStat .EQ. -1) RETURN
          CALL FlowToElements(pUrbanLand%UrbData%Runoff + pUrbanLand%UrbData%ReturnFlow  , &
                              AppGrid                                                    , &
                              RootZone                                                   , &
                              NVIndex                                                    , &
                              pElemSupply%UpstrmRunoff                                   )
        END IF
        
        !Simulate native and riparian veg lands
        IF (pFlags%lNVRV_Defined) THEN
          CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,iLandUse_NVRV,InRunoff)
          CALL NativeRiparianLandUse_Simulate(AppGrid            , &
                                              ETData             , &
                                              DeltaT             , &
                                              pElemPrecip        , &
                                              prGenericMoisture  , &
                                              pSoilsData         , &
                                              InRunoff(1,:)      , &
                                              pElemsToGW         , &
                                              pSolverData        , &
                                              pFlags%lLakeElems  , &
                                              pNVRV              , &
                                              iStat              )
          IF (iStat .EQ. -1) RETURN
          CALL FlowToElements(pNVRV%NativeVeg%Runoff + pNVRV%RiparianVeg%Runoff , &
                              AppGrid                                           , &
                              RootZone                                          , &
                              NVIndex                                           , &
                              pElemSupply%UpstrmRunoff                          )
        END IF 

        !Check convergence
        AchievedConv = 0.0
        DO indxElem=1,NElements
          Runoff   = pElemSupply(indxElem)%UpstrmRunoff
          Runoff_P = UpstrmElemRunoff_P(indxElem)
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
    
  END SUBROUTINE RootZone_v41_Simulate




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
  ! --- COMPUTE REGIONAL GW INFLOW
  ! -------------------------------------------------------------
  FUNCTION RegionalGWInflow(AppGrid,RootZone,LUIndex) RESULT(RGWInflow)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RGWInflow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RGWInflow = 0.0
    
    !Return if ET from GW is not simulated
    IF (.NOT. RootZone%Flags%lComputeETFromGW) RETURN
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
          ElemValue = 0.0
          ASSOCIATE (pNonPondedCrops => RootZone%NonPondedAgRootZone%Crops  ,  &
                     pPondedCrops    => RootZone%PondedAgRootZone%Crops     )
              IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(pNonPondedCrops%ETFromGW_Actual , DIM=1)
              IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(pPondedCrops%ETFromGW_Actual , DIM=1)
          END ASSOCIATE
          
      !Urban
      CASE (UrbIndex)
          IF (RootZone%Flags%lUrban_Defined)   &
             ElemValue = RootZone%UrbanRootZone%UrbData%ETFromGW_Actual
      
      !Native and riparian vegetation
      CASE (NVIndex)
          IF (RootZone%Flags%lNVRV_Defined)  &
              ElemValue = RootZone%NVRVRootZone%NativeVeg%ETFromGW_Actual    &
                        + RootZone%NVRVRootZone%RiparianVeg%ETFromGW_Actual
      
    END SELECT
    
    !Regional GW inflow
    RGWInflow(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RGWInflow(NRegions+1) = SUM(RGWInflow(1:NRegions))

  END FUNCTION RegionalGWInflow
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL DEMAND
  ! -------------------------------------------------------------
  FUNCTION RegionalDemand(AppGrid,RootZone,LUIndex) RESULT(RDemand)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RDemand(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RDemand   = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Demand , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Demand , DIM=1)
   
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Demand
      
      !Otherwise
      CASE DEFAULT
        ElemValue = 0.0
        RETURN
      
    END SELECT
    
    !Regional demand
    RDemand(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RDemand(NRegions+1) = SUM(RDemand(1:NRegions))

  END FUNCTION RegionalDemand
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RAW (BEFORE RETURN FLOW IS ADDED) AGRICULTURAL DEMAND
  ! -------------------------------------------------------------
  FUNCTION RegionalAgRawDemand(AppGrid,RootZone) RESULT(RDemandRaw)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RDemandRaw(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions   = AppGrid%NSubregions
    RDemandRaw = 0.0
    ElemValue  = 0.0
    
    !Element values
    IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%DemandRaw , DIM=1)
    IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%DemandRaw , DIM=1)

    !Regional raw ag demand
    RDemandRaw(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RDemandRaw(NRegions+1) = SUM(RDemandRaw(1:NRegions))

  END FUNCTION RegionalAgRawDemand


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ETAW
  ! -------------------------------------------------------------
  FUNCTION RegionalETAW(AppGrid,RootZone) RESULT(RETAW)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RETAW(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions   = AppGrid%NSubregions
    RETAW      = 0.0
    ElemValue  = 0.0
    
    !Element values
    IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ETAW , DIM=1)
    IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ETAW , DIM=1)

    !Regional raw ag demand
    RETAW(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RETAW(NRegions+1) = SUM(RETAW(1:NRegions))

  END FUNCTION RegionalETAW


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ETP
  ! -------------------------------------------------------------
  FUNCTION RegionalETP(AppGrid,RootZone) RESULT(RETP)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RETP(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions   = AppGrid%NSubregions
    RETP       = 0.0
    ElemValue  = 0.0
    
    !Element values
    IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ETP , DIM=1)
    IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ETP , DIM=1)

    !Regional raw ag demand
    RETP(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RETP(NRegions+1) = SUM(RETP(1:NRegions))

  END FUNCTION RegionalETP


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ETGW
  ! -------------------------------------------------------------
  FUNCTION RegionalETGW(AppGrid,RootZone) RESULT(RETGW)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RETGW(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions   = AppGrid%NSubregions
    RETGW      = 0.0
    
    !Return if ET from GW is not simulated
    IF (.NOT. RootZone%Flags%lComputeETFromGW) RETURN
    
    
    !Element values
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ETFromGW_Actual , DIM=1)
    ELSE
        ElemValue = 0.0
    END IF
    IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ETFromGW_Actual , DIM=1)

    !Regional raw ag demand
    RETGW(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RETGW(NRegions+1) = SUM(RETGW(1:NRegions))

  END FUNCTION RegionalETGW


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ETOth
  ! -------------------------------------------------------------
  FUNCTION RegionalETOth(AppGrid,RootZone) RESULT(RETOth)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: RETOth(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions   = AppGrid%NSubregions
    RETOth     = 0.0
    ElemValue  = 0.0
    
    !Element values
    IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ETOth , DIM=1)
    IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ETOth , DIM=1)

    !Regional raw ag demand
    RETOth(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RETOth(NRegions+1) = SUM(RETOth(1:NRegions))

  END FUNCTION RegionalETOth


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RE-USE
  ! -------------------------------------------------------------
  FUNCTION RegionalReuse(AppGrid,RootZone,LUIndex) RESULT(RReuse)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RReuse(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions,indxElemRegion,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue,Supply
    
    !Initialize
    NRegions = AppGrid%NSubregions
    RReuse   = 0.0
    
    !Reuse internal in an element
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Reuse , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Reuse , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Reuse
        
      !Otherwise
      CASE DEFAULT
        ElemValue = 0.0
        RETURN
      
    END SELECT
    
    !Reuse due to element surface runoff being transferred to an element/subregion
    IF (LUIndex .EQ. AgIndex) THEN
      IF (RootZone%Flags%lNonPondedAg_Defined) THEN
          Supply = SUM(RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac, DIM=1)
      ELSE
          Supply = 0.0
      END IF
      IF (RootZone%Flags%lPondedAg_Defined) Supply = Supply + SUM(RootZone%PondedAgRootZone%Crops%ElemDemandFrac, DIM=1) 
      Supply = RootZone%ElemSupply%UpstrmRunoff * Supply
    ELSE
      Supply = RootZone%ElemSupply%UpstrmRunoff * RootZone%UrbanRootZone%UrbData%ElemDemandFrac
    END IF
    
    ASSOCIATE (pElemsToElements => RootZone%ElemFlowToElements     , &
               pElemsToSubregions => RootZone%ElemFlowToSubregions )
        !Process element flows to other elements
        DO indx=1,SIZE(pElemsToElements)
            iElem       = pElemsToElements(indx)%iElement
            iDest       = pElemsToElements(indx)%iDestID
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iRegion .EQ. iDestRegion) RReuse(iDestRegion) = RReuse(iDestRegion) + Supply(iDest)
        END DO
        
        !Process elements flows to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDestID
            IF (iDestRegion .EQ. iRegion) THEN
                DO indxElemRegion=1,AppGrid%AppSubregion(iDestRegion)%NRegionElements
                    iDest               = AppGrid%AppSubregion(iDestRegion)%RegionElements(indxElemRegion)
                    RReuse(iDestRegion) = RReuse(iDestRegion) + Supply(iDest)
                 END DO
            END IF 
        END DO
    END ASSOCIATE
               
    !Regional reuse
    RReuse(1:NRegions) = RReuse(1:NRegions) + AppGrid%AccumElemValuesToSubregions(ElemValue)
    RReuse(NRegions+1) = SUM(RReuse(1:NRegions))

  END FUNCTION RegionalReuse
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL PRECIP
  ! -------------------------------------------------------------
  FUNCTION RegionalPrecip(AppGrid,RootZone,LUIndex) RESULT(RPrecip)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RPrecip(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RPrecip   = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = RootZone%ElemPrecipData%Precip * SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + RootZone%ElemPrecipData%Precip * SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%ElemPrecipData%Precip * RootZone%UrbanRootZone%UrbData%Area
      
      !Native and riparian vegetation
      CASE (NVIndex)
        ElemValue = RootZone%ElemPrecipData%Precip * (RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%Area)
      
    END SELECT
    
    !Regional precip
    RPrecip(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RPrecip(NRegions+1) = SUM(RPrecip(1:NRegions))

  END FUNCTION RegionalPrecip
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL GENERIC MOISTURE INFLOW
  ! -------------------------------------------------------------
  FUNCTION RegionalGenericMoistInflow(AppGrid,RootZone,LUIndex) RESULT(RGenericMoist)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RGenericMoist(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions,indxElem,indxCrop,NElements,NNonPondedCrops,NPondedCrops
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    LOGICAL                              :: lNonPondedAg_Defined,lPondedAg_Defined
    
    !Initialize
    NElements     = AppGrid%NElements
    NRegions      = AppGrid%NSubregions
    RGenericMoist = 0.0
    ElemValue     = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        NNonPondedCrops      = RootZone%NonPondedAgRootZone%NCrops
        NPondedCrops         = RootZone%PondedAgRootZone%NCrops
        lNonPondedAg_Defined = RootZone%Flags%lNonPondedAg_Defined
        lPondedAg_Defined    = RootZone%Flags%lPondedAg_Defined
        ASSOCIATE (prGenericMoisture   => RootZone%GenericMoistureData%rGenericMoisture , &
                   pNonPondedRootDepth => RootZone%NonPondedAgRootZone%RootDepth        , &
                   pNonPondedCrops     => RootZone%NonPondedAgRootZone%Crops            , &
                   pNonPondedGMExcess  => RootZone%NonPondedAgRootZone%Crops%GMExcess   , &
                   pPondedRootDepth    => RootZone%PondedAgRootZone%RootDepth           , &
                   pPondedCrops        => RootZone%PondedAgRootZone%Crops               )
          DO indxElem=1,NElements
            IF (lNonPondedAg_Defined) THEN
              DO indxCrop=1,NNonPondedCrops
                ElemValue(indxElem) = ElemValue(indxElem) + (prGenericMoisture(1,indxElem) * pNonPondedRootDepth(indxCrop) - pNonPondedCrops(indxCrop,indxElem)%GMExcess) * pNonPondedCrops(indxCrop,indxElem)%Area
              END DO
            END IF
            IF (lPondedAg_Defined) THEN
              DO indxCrop=1,NPondedCrops
                ElemValue(indxElem) = ElemValue(indxElem) + (prGenericMoisture(1,indxElem) * pPondedRootDepth(indxCrop) - pPondedCrops(IndxCrop,indxElem)%GMExcess) * pPondedCrops(indxCrop,indxElem)%Area
              END DO
            END IF
          END DO
        END ASSOCIATE
        
      !Urban
      CASE (UrbIndex)
        IF (RootZone%Flags%lUrban_Defined) THEN
          ASSOCIATE (pUrbanRootZone => RootZone%UrbanRootZone)
            ElemValue = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * pUrbanRootZone%RootDepth - pUrbanRootZone%UrbData%GMExcess) * pUrbanRootZone%UrbData%Area * pUrbanRootZone%UrbData%PerviousFrac
          END ASSOCIATE
        END IF
        
      !Native and riparian vegetation
      CASE (NVIndex)
        IF (RootZone%Flags%lNVRV_Defined) THEN
          ASSOCIATE (pNVRVRootZone => RootZone%NVRVRootZone)
            ElemValue = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * pNVRVRootZone%RootDepth_Native   - pNVRVRootZone%NativeVeg%GMExcess)   * pNVRVRootZone%NativeVeg%Area    &
                      + (RootZone%GenericMoistureData%rGenericMoisture(1,:) * pNVRVRootZone%RootDepth_Riparian - pNVRVRootZone%RiparianVeg%GMExcess) * pNVRVRootZone%RiparianVeg%Area
          END ASSOCIATE
        END IF
        
    END SELECT
    
    !Regional generic moisture inflow
    RGenericMoist(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RGenericMoist(NRegions+1) = SUM(RGenericMoist(1:NRegions))

  END FUNCTION RegionalGenericMoistInflow
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL SOIL MISTURE CHANGE DUE TO LAND USE CHANGE
  ! -------------------------------------------------------------
  FUNCTION RegionalSoilMChange(AppGrid,RootZone,LUIndex) RESULT(RSoilMCh)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RSoilMCh(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RSoilMCh  = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%SoilMCh , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%SoilMCh , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%SoilMCh
      
      !Native and riparian vegetation
      CASE (NVIndex)
        ElemValue = RootZone%NVRVRootZone%NativeVeg%SoilMCh + RootZone%NVRVRootZone%RiparianVeg%SoilMCh
      
    END SELECT
    
    !Regional soil moisture change due to land use change
    RSoilMCh(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RSoilMCh(NRegions+1) = SUM(RSoilMCh(1:NRegions))

  END FUNCTION RegionalSoilMChange
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL ACTUAL ET
  ! -------------------------------------------------------------
  FUNCTION RegionalETa(AppGrid,RootZone,LUIndex) RESULT(RETa)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RETa(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RETa      = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ETa , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ETa , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%ETa
      
      !Native and riparian vegetation
      CASE (NVIndex)
        ElemValue = RootZone%NVRVRootZone%NativeVeg%ETa + RootZone%NVRVRootZone%RiparianVeg%ETa
      
    END SELECT
    
    !Regional actual ET
    RETa(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RETa(NRegions+1) = SUM(RETa(1:NRegions))

  END FUNCTION RegionalETa
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL PERCOLATION
  ! -------------------------------------------------------------
  FUNCTION RegionalPerc(AppGrid,RootZone,LUIndex) RESULT(RPerc)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RPerc(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RPerc = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Perc + RootZone%NonPondedAgRootZone%Crops%PercCh , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Perc + RootZone%PondedAgRootZone%Crops%PercCh , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Perc + RootZone%UrbanRootZone%UrbData%PercCh
      
      !Native and riparian vegetation
      CASE (NVIndex)
        ElemValue =  RootZone%NVRVRootZone%NativeVeg%Perc + RootZone%NVRVRootZone%NativeVeg%PercCh &
                   + RootZone%NVRVRootZone%RiparianVeg%Perc + RootZone%NVRVRootZone%RiparianVeg%PercCh
      
    END SELECT
    
    !Regional perc
    RPerc(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RPerc(NRegions+1) = SUM(RPerc(1:NRegions))

  END FUNCTION RegionalPerc
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL INFILTRATION
  ! -------------------------------------------------------------
  FUNCTION RegionalInfiltration(AppGrid,RootZone,LUIndex) RESULT(RInfilt)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RInfilt(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RInfilt   = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%PrecipInfilt + RootZone%NonPondedAgRootZone%Crops%IrigInfilt , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%PrecipInfilt + RootZone%PondedAgRootZone%Crops%IrigInfilt , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%PrecipInfilt + RootZone%UrbanRootZone%UrbData%IrigInfilt 
      
      !Native and riparian vegetation
      CASE (NVIndex)
        ElemValue =  RootZone%NVRVRootZone%NativeVeg%PrecipInfilt + RootZone%NVRVRootZone%RiparianVeg%PrecipInfilt 
      
    END SELECT
    
    !Regional soil moisture change due to land use change
    RInfilt(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RInfilt(NRegions+1) = SUM(RInfilt(1:NRegions))

  END FUNCTION RegionalInfiltration
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL LAND USE AREAS
  ! -------------------------------------------------------------
  FUNCTION RegionalLUArea(AppGrid,RootZone,LUIndex) RESULT(RLUArea)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RLUArea(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RLUArea   = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        IF (RootZone%Flags%lUrban_Defined)  &
            ElemValue = RootZone%UrbanRootZone%UrbData%Area
      
      !Native and riparian vegetation
      CASE (NVIndex)
        IF (RootZone%Flags%lNVRV_Defined)   &
            ElemValue = RootZone%NVRVRootZone%NativeVeg%Area + RootZone%NVRVRootZone%RiparianVeg%Area
      
    END SELECT
    
    !Regional land use areas
    RLUArea(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RLUArea(NRegions+1) = SUM(RLUArea(1:NRegions))

  END FUNCTION RegionalLUArea
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL LAND USE AREAS
  ! -------------------------------------------------------------
  FUNCTION RegionalLUArea_ForSingleRegion(iRegion,AppGrid,RootZone,LUIndex) RESULT(Area)
    INTEGER,INTENT(IN)                 :: iRegion,LUIndex
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: Area
    
    !Local variables
    INTEGER :: indxRegionElem,iElem
    
    !Initialize
    Area = 0.0
    
    ASSOCIATE (pAppSubregion => AppGrid%AppSubregion(iRegion))
    
      SELECT CASE (LUIndex)
        !Ponded and non-ponded ag
        CASE (AgIndex)
          ASSOCIATE (plNonPondedAg_Defined => RootZone%Flags%lNonPondedAg_Defined  , &
                     plPondedAg_Defined    => RootZone%Flags%lPondedAg_Defined     , &
                     pNonPondedCrops       => RootZone%NonPondedAgRootZone%Crops   , &
                     pPondedCrops          => RootZone%PondedAgRootZone%Crops      )
            DO indxRegionElem=1,pAppSubregion%NRegionElements
              iElem = pAppSubregion%RegionElements(indxRegionElem)
              IF (plNonPondedAg_Defined) Area = Area + SUM(pNonPondedCrops(:,iElem)%Area)
              IF (plPondedAg_Defined)    Area = Area + SUM(pPondedCrops(:,iElem)%Area)
            END DO
          END ASSOCIATE
    
        !Urban
        CASE (UrbIndex)
          IF (RootZone%Flags%lUrban_Defined) THEN
            ASSOCIATE (pUrban => RootZone%UrbanRootZone%UrbData)
              DO indxRegionElem=1,pAppSubregion%NRegionElements
                iElem = pAppSubregion%RegionElements(indxRegionElem)
                Area = Area + pUrban(iElem)%Area
              END DO
            END ASSOCIATE
          END IF
      
        !Native and riparian vegetation
        CASE (NVIndex)
          IF (RootZone%Flags%lNVRV_Defined) THEN
            ASSOCIATE (pNV => RootZone%NVRVRootZone%NativeVeg    , &
                       pRV => RootZone%NVRVRootZone%RiparianVeg  )
              DO indxRegionElem=1,pAppSubregion%NRegionElements
                iElem = pAppSubregion%RegionElements(indxRegionElem)
                Area = Area + pNV(iElem)%Area + pRV(iElem)%Area
              END DO
            END ASSOCIATE
          END IF
      
      END SELECT
    
    END ASSOCIATE
    
  END FUNCTION RegionalLUArea_ForSingleRegion
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL INFLOW DUE TO SURFACE FLOWS FROM UPSTREAM ELEMENTS/REGIONS 
  ! -------------------------------------------------------------
  FUNCTION RegionalUpStrmElemFlow(AppGrid,RootZone,DemandFracAg,LUIndex) RESULT(RInflow)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(IN)                 :: DemandFracAg(:)
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RInflow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RInflow   = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = RootZone%ElemSupply%UpstrmRunoff * DemandFracAg
    
      !Urban
      CASE (UrbIndex)
        IF (RootZone%Flags%lUrban_Defined)  &
            ElemValue = RootZone%ElemSupply%UpstrmRunoff * RootZone%UrbanRootZone%UrbData%ElemDemandFrac 
      
      !Native and riparian vegetation
      CASE (NVIndex)
        IF (RootZone%Flags%lNVRV_Defined) THEN 
            WHERE (RootZone%ElemDevelopedArea .EQ. 0.0) ElemValue = RootZone%ElemSupply%UpstrmRunoff 
        END IF
      
    END SELECT
      
    ASSOCIATE (pElemsToElements   => RootZone%ElemFlowToElements   , &
               pElemsToSubregions => RootZone%ElemFlowToSubregions )
        !Process element flow to other elements
        DO indx=1,SIZE(pElemsToElements)
            iElem       = pElemsToElements(indx)%iElement
            iDest       = pElemsToElements(indx)%iDestID
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RInflow(iDestRegion) = RInflow(iDestRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDestID
            IF (iDestRegion .NE. iRegion) RInflow(iDestRegion) = RInflow(iDestRegion) + ElemValue(iElem)
        END DO
    END ASSOCIATE
          
    !Accumulate to entire domain
    RInflow(NRegions+1) = SUM(RInflow(1:NRegions))
        
  END FUNCTION RegionalUpStrmElemFlow
  

  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RUNOFF 
  ! -------------------------------------------------------------
  FUNCTION RegionalRunoff(AppGrid,RootZone,LUIndex) RESULT(RRunoff)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RRunoff(AppGrid%NSubregions+1)

    !Local variables
    INTEGER                              :: NRegions,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RRunoff   = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue =ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Runoff , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Runoff 
      
      !Native and riparian vegetation
      CASE (NVIndex)
        ElemValue = RootZone%NVRVRootZone%NativeVeg%Runoff + RootZone%NVRVRootZone%RiparianVeg%Runoff
      
    END SELECT
      
    ASSOCIATE (pElemsToOutside    => RootZone%ElemFlowToOutside    , &
               pElemsToStreams    => RootZone%ElemFlowToStreams    , &
               pElemsToLakes      => RootZone%ElemFlowToLakes      , &
               pElemsToGW         => RootZone%ElemFlowToGW         , &
               pElemsToElements   => RootZone%ElemFlowToElements   , &
               pElemsToSubregions => RootZone%ElemFlowToSubregions )
        !Process element flows to outside
        DO indx=1,SIZE(pElemsToOutside)
            iElem            = pElemsToOutside(indx)
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RRunoff(iRegion) = RRunoff(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to streams
        DO indx=1,SIZE(pElemsToStreams)
            iElem            = pElemsToStreams(indx)%iElement
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RRunoff(iRegion) = RRunoff(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to lakes
        DO indx=1,SIZE(pElemsToLakes)
            iElem            = pElemsToLakes(indx)%iElement
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RRunoff(iRegion) = RRunoff(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to groundwater
        DO indx=1,SIZE(pElemsToGW)
            iElem            = pElemsToGW(indx)
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RRunoff(iRegion) = RRunoff(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to other elements
        DO indx=1,SIZE(pElemsToElements)
            iElem       = pElemsToElements(indx)%iElement
            iDest       = pElemsToElements(indx)%iDestID
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RRunoff(iRegion) = RRunoff(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDestID
            IF (iDestRegion .NE. iRegion) RRunoff(iRegion) = RRunoff(iRegion) + ElemValue(iElem)
        END DO
    END ASSOCIATE
    
    !Accumulate to entire domain
    RRunoff(NRegions+1) = SUM(RRunoff(1:NRegions))
    
  END FUNCTION RegionalRunoff


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL POND DRAIN FLOW 
  ! -------------------------------------------------------------
  FUNCTION RegionalDrain(AppGrid,RootZone,LUIndex) RESULT(RDrain)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RDrain(AppGrid%NSubregions+1)

    !Local variables
    INTEGER                              :: NRegions,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RDrain    = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded ag
      CASE (AgIndex)
        IF (RootZone%Flags%lPondedAg_Defined)   &
            ElemValue = SUM(RootZone%PondedAgRootZone%Crops%Drain , DIM=1)
    
      !Otherwise
      CASE DEFAULT
        RETURN
        
    END SELECT
      
    ASSOCIATE (pElemsToOutside    => RootZone%ElemFlowToOutside    , &
               pElemsToStreams    => RootZone%ElemFlowToStreams    , &
               pElemsToLakes      => RootZone%ElemFlowToLakes      , &
               pElemsToGW         => RootZone%ElemFlowToGW         , &
               pElemsToElements   => RootZone%ElemFlowToElements   , &
               pElemsToSubregions => RootZone%ElemFlowToSubregions )
        !Process element flows to outside
        DO indx=1,SIZE(pElemsToOutside)
            iElem            = pElemsToOutside(indx)
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RDrain(iRegion) = RDrain(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to streams
        DO indx=1,SIZE(pElemsToStreams)
            iElem            = pElemsToStreams(indx)%iElement
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RDrain(iRegion) = RDrain(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to lakes
        DO indx=1,SIZE(pElemsToLakes)
            iElem            = pElemsToLakes(indx)%iElement
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RDrain(iRegion) = RDrain(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to groundwater
        DO indx=1,SIZE(pElemsToGW)
            iElem            = pElemsToGW(indx)
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RDrain(iRegion) = RDrain(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to other elements
        DO indx=1,SIZE(pElemsToElements)
            iElem       = pElemsToElements(indx)%iElement
            iDest       = pElemsToElements(indx)%iDestID
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RDrain(iRegion) = RDrain(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDestID
            IF (iDestRegion .NE. iRegion) RDrain(iRegion) = RDrain(iRegion) + ElemValue(iElem)
        END DO
    END ASSOCIATE
    
    !Accumulate to entire domain
    RDrain(NRegions+1) = SUM(RDrain(1:NRegions))
    
  END FUNCTION RegionalDrain


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RETURN FLOW 
  ! -------------------------------------------------------------
  FUNCTION RegionalReturn(AppGrid,RootZone,LUIndex) RESULT(RReturn)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RReturn(AppGrid%NSubregions+1)

    !Local variables
    INTEGER                              :: NRegions,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RReturn   = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow , DIM=1)
    
      !Urban
      CASE (UrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%ReturnFlow 
      
      !Otherwise
      CASE DEFAULT
        ElemValue = 0.0
        RETURN
        
    END SELECT
      
    ASSOCIATE (pElemsToOutside    => RootZone%ElemFlowToOutside    , &
               pElemsToStreams    => RootZone%ElemFlowToStreams    , &
               pElemsToLakes      => RootZone%ElemFlowToLakes      , &
               pElemsToGW         => RootZone%ElemFlowToGW         , &
               pElemsToElements   => RootZone%ElemFlowToElements   , &
               pElemsToSubregions => RootZone%ElemFlowToSubregions )
        !Process element flows to outside
        DO indx=1,SIZE(pElemsToOutside)
            iElem            = pElemsToOutside(indx)
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RReturn(iRegion) = RReturn(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to streams
        DO indx=1,SIZE(pElemsToStreams)
            iElem            = pElemsToStreams(indx)%iElement
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RReturn(iRegion) = RReturn(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to lakes
        DO indx=1,SIZE(pElemsToLakes)
            iElem            = pElemsToLakes(indx)%iElement
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RReturn(iRegion) = RReturn(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flows to groundwater
        DO indx=1,SIZE(pElemsToGW)
            iElem            = pElemsToGW(indx)
            iRegion          = AppGrid%AppElement(iElem)%Subregion
            RReturn(iRegion) = RReturn(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to other elements
        DO indx=1,SIZE(pElemsToElements)
            iElem       = pElemsToElements(indx)%iElement
            iDest       = pElemsToElements(indx)%iDestID
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RReturn(iRegion) = RReturn(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDestID
            IF (iDestRegion .NE. iRegion) RReturn(iRegion) = RReturn(iRegion) + ElemValue(iElem)
        END DO
    END ASSOCIATE
    
    !Accumulate to entire domain
    RReturn(NRegions+1) = SUM(RReturn(1:NRegions))
    
  END FUNCTION RegionalReturn


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL PUMPING THAT IS DELIVERED TO MEET DEMAND
  ! -------------------------------------------------------------
  FUNCTION RegionalPumping(AppGrid,RootZone,LUIndex) RESULT(RPump)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RPump(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions = AppGrid%NSubregions
    RPump    = 0.0
    
    SELECT CASE (LUIndex)
      
      !Ponded and non-ponded ag
      CASE (AgIndex)
       ElemValue         = RootZone%ElemSupply%Pumping_Ag
       RPump(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      
      !Urban
      CASE (UrbIndex)
        ElemValue         = RootZone%ElemSupply%Pumping_Urb 
        RPump(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
        
      !Otherwise
      CASE DEFAULT
        RETURN
        
    END SELECT
    
    !Accumulate to entire domain
    RPump(NRegions+1) = SUM(RPump(1:NRegions))
    
  END FUNCTION RegionalPumping
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL DELIVERIES THAT IS DELIVERED TO MEET DEMAND 
  ! -------------------------------------------------------------
  FUNCTION RegionalDeliveries(AppGrid,RootZone,LUIndex) RESULT(RDeli)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RDeli(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions = AppGrid%NSubregions
    RDeli    = 0.0
    
    SELECT CASE (LUIndex)
      
      !Ponded and non-ponded ag
      CASE (AgIndex)
        ElemValue         = RootZone%ElemSupply%Diversion_Ag
        RDeli(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      
      !Urban
      CASE (UrbIndex)
        ElemValue         = RootZone%ElemSupply%Diversion_Urb
        RDeli(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
        
      !Otherwise
      CASE DEFAULT
        RETURN
        
    END SELECT
    
    !Accumulate to entire domain
    RDeli(NRegions+1) = SUM(RDeli(1:NRegions))
    
  END FUNCTION RegionalDeliveries


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL MOISTURE STORAGE
  ! -------------------------------------------------------------
  FUNCTION RegionalMoistStorage(AppGrid,RootZone) RESULT(RSoilM)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(RootZone_v41_Type)      :: RootZone
    REAL(8)                      :: RSoilM(AppGrid%NSubregions+1,NGroupLandUse)
    
    !Local variables
    INTEGER :: NRegions
    REAL(8) :: ElemValues(AppGrid%NElements)
    
    !Initialize
    ElemValues = 0.0
    RSoilM     = 0.0
    NRegions   = AppGrid%NSubregions
    
    ASSOCIATE (pFlags       => RootZone%Flags                     , &
               pCrops       => RootZone%NonPondedAgRootZone%Crops , &
               pPondedCrops => RootZone%PondedAgRootZone%Crops    , &
               pUrban       => RootZone%UrbanRootZone%UrbData     , &
               pNV          => RootZone%NVRVRootZone%NativeVeg    , &
               pRV          => RootZone%NVRVRootZone%RiparianVeg  ) 
               
      IF (pFlags%lNonPondedAg_Defined) THEN
        ElemValues                 = SUM((pCrops%SoilM_Precip + pCrops%SoilM_AW + pCrops%SoilM_Oth) * pCrops%Area , DIM=1)
        RSoilM(1:NRegions,AgIndex) = AppGrid%AccumElemValuesToSubregions(ElemValues)
      END IF
      
      IF (pFlags%lPondedAg_Defined) THEN
        ElemValues                 = SUM((pPondedCrops%SoilM_Precip + pPondedCrops%SoilM_AW + pPondedCrops%SoilM_Oth) * pPondedCrops%Area , DIM=1)
        RSoilM(1:NRegions,AgIndex) = RSoilM(1:NRegions,AgIndex) + AppGrid%AccumElemValuesToSubregions(ElemValues)
      END IF
               
      IF (pFlags%lUrban_Defined) THEN
        ElemValues                  = (pUrban%SoilM_Precip + pUrban%SoilM_AW + pUrban%SoilM_Oth) * pUrban%Area * pUrban%PerviousFrac
        RSoilM(1:NRegions,UrbIndex) = AppGrid%AccumElemValuesToSubregions(ElemValues)
      END IF
      
      IF (pFlags%lNVRV_Defined) THEN
        ElemValues                 = (pNV%SoilM_Precip + pNV%SoilM_AW + pNV%SoilM_Oth) * pNV%Area + (pRV%SoilM_Precip + pRV%SoilM_AW + pRV%SoilM_Oth) * pRV%Area
        RSoilM(1:NRegions,NVIndex) = AppGrid%AccumElemValuesToSubregions(ElemValues)
      END IF

    END ASSOCIATE 
    
    !Moisture storage for the entire domain
    RSoilM(NRegions+1,:) = SUM(RSoilM(1:NRegions,:) , DIM=1)

    
  END FUNCTION RegionalMoistStorage

  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL POTENTIAL ET
  ! -------------------------------------------------------------
  SUBROUTINE RegionalETPot(AppGrid,RootZone,LUIndex,RETp)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8),INTENT(OUT)                :: RETp(:)
    
    !Local variables
    INTEGER :: NRegions
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    SELECT CASE(LUIndex)
        !Agricultural lands
        CASE (AgIndex)
            RETp = 0.0
            IF (RootZone%Flags%lNonPondedAg_Defined) RETp(1:NRegions) = SUM(RootZone%NonPondedAgRootZone%RegionETPot , DIM=1)
            IF (RootZone%Flags%lPondedAg_Defined)    RETp(1:NRegions) = RETp(1:NRegions) + SUM(RootZone%PondedAgRootZone%RegionETPot , DIM=1)

        !Urban
        CASE (UrbIndex)
            IF (RootZone%Flags%lUrban_Defined) THEN
                RETp(1:NRegions) = RootZone%UrbanRootZone%RegionETPot
            ELSE
                RETp = 0.0
            END IF
            
        !Native and riparian
        CASE (NVIndex)
            IF (RootZone%Flags%lNVRV_Defined) THEN
                RETp(1:NRegions) = RootZone%NVRVRootZone%RegionETPot_NV + RootZone%NVRVRootZone%RegionETPot_RV
            ELSE
                RETp = 0.0
            END IF
    END SELECT
        
    !Potential ET for the entire model domain
    RETp(NRegions+1) = SUM(RETp(1:NRegions))   
    
  END SUBROUTINE RegionalETPot
  
    
  ! -------------------------------------------------------------
  ! --- COMPUTE DISTRIBUTION OF UPSTREAM ELEMENT RUNOFF TO LAND USE TYPES
  ! --- Note: This procedure assumes that existence of land use types have 
  ! ---       been already checked before a call to this procedure.
  ! -------------------------------------------------------------
  SUBROUTINE ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff,RootZone,iLandUseType,InRunoff)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    REAL(8),INTENT(IN)                 :: UpstrmElemRunoff(:)
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: iLandUseType
    REAL(8),ALLOCATABLE,INTENT(OUT)    :: InRunoff(:,:)
                                     
    !Local variables
    INTEGER :: indxElem,NElements,ErrorCode
    REAL(8) :: ElemArea(AppGrid%NElements)
    
    !Initialize
    NElements = AppGrid%NElements
    ElemArea  = AppGrid%AppElement%Area
    DEALLOCATE (InRunoff , STAT=ErrorCode)
    
    SELECT CASE (iLandUseType)
      !Flow from upstream runoff to non-ponded crops
      CASE (iLandUse_NonPonded)
        ALLOCATE (InRunoff(RootZone%NonPondedAgRootZone%NCrops,NElements))
        InRunoff = 0.0
        FORALL (indxElem=1:NElements) InRunoff(:,indxElem) = UpstrmElemRunoff(indxElem) * RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Area / ElemArea(indxElem)       
    
      !Flow from upstream runoff to ponded crops
      CASE (iLandUse_Ponded)
        ALLOCATE (InRunoff(RootZone%PondedAgRootZone%NCrops,NElements))
        InRunoff = 0.0
        FORALL (indxElem=1:NElements) InRunoff(:,indxElem) = UpstrmElemRunoff(indxElem) * RootZone%PondedAgRootZone%Crops(:,indxElem)%Area / ElemArea(indxElem)       

      !Flow from upstream runoff to urban lands
      CASE (iLandUse_Urban)
        ALLOCATE (InRunoff(1,NElements))
        InRunoff      = 0.0
        InRunoff(1,:) = UpstrmElemRunoff * RootZone%UrbanRootZone%UrbData%Area / ElemArea       
                                     
      !Flow from upstream runoff to native and riparian vegetation lands
      CASE (iLandUse_NVRV)
        ALLOCATE (InRunoff(1,NElements))
        InRunoff      = 0.0
        InRunoff(1,:) = UpstrmElemRunoff * (RootZone%NVRVRootZone%NativeVeg%Area+RootZone%NVRVRootZone%RiparianVeg%Area) / ElemArea       
        
    END SELECT
    
  END SUBROUTINE ComputeUpstrmElemRunoffToLandUse
                                      
  
  ! -------------------------------------------------------------
  ! --- COMPUTE DEMAND AND SUPPLY RELATED FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE ComputeDemandSupplyRelatedFracs(AppGrid,RootZone)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(RootZone_v41_Type)      :: RootZone
    
    !Local variables
    INTEGER :: indxElem,NElements,NCrops,indxRegion,iElem
    REAL(8) :: ElemDemand_Ag(AppGrid%NElements),ElemDemandFrac(RootZone%NLands-2), &
               RegionDemandArea,RegionArea,ElemDemand,RegionalDemand_Ag(AppGrid%NSubregions+1), &
               RegionalDemand_Urb(AppGrid%NSubregions+1),ElemDemandFrac_Ag
   
    !Initialize
    NElements     = AppGrid%NElements
    NCrops        = RootZone%NonPondedAgRootZone%NCrops
    ElemDemand_Ag = 0.0
    
    !Compute total regional and elemental demands and fractions
    ASSOCIATE (pRatioElemToRegion_Ag  => RootZone%Ratio_ElemSupplyToRegionSupply_Ag     , &
               pRatioElemToRegion_Urb => RootZone%Ratio_ElemSupplyToRegionSupply_Urb    , &
               pCrops                 => RootZone%NonPondedAgRootZone%Crops             , &
               pPondedCrops           => RootZone%PondedAgRootZone%Crops                , &
               pUrban                 => RootZone%UrbanRootZone%UrbData                 , &
               pFlags                 => RootZone%Flags                                 , &
               pAppElement            => AppGrid%AppElement                             )
               
      !Zero out the ratios
      pRatioElemToRegion_Ag  = 0.0
      pRatioElemToRegion_Urb = 0.0
 
      !Subregional total demand and ratio of demand of each irrigable land to total demand in each element
      DO indxElem=1,NElements
        ElemDemand = 0.0
        IF (pFlags%lNonPondedAg_Defined) ElemDemand_Ag(indxElem) = SUM(pCrops(:,indxElem)%Demand) 
        IF (pFlags%lPondedAg_Defined)    ElemDemand_Ag(indxElem) = ElemDemand_Ag(indxElem) + SUM(pPondedCrops(:,indxElem)%Demand)
        ElemDemand = ElemDemand_Ag(indxElem)
        IF (pFlags%lUrban_Defined)       ElemDemand              = ElemDemand + pUrban(indxElem)%Demand
      
        !Ratio of each land use demand to total element demand
        ElemDemandFrac = 0.0
        IF (ElemDemand .EQ. 0.0) THEN
          !Use area fractions if total element demand is zero
          IF (pFlags%lNonPondedAg_Defined) ElemDemandFrac(1:NCrops)                     = pCrops(:,indxElem)%Area
          IF (pFlags%lPondedAg_Defined)    ElemDemandFrac(NCrops+1:NCrops+NPondedCrops) = pPondedCrops(:,indxElem)%Area
          IF (pFlags%lUrban_Defined)       ElemDemandFrac(NCrops+NPondedCrops+1)        = pUrban(indxElem)%Area
        ELSE
          !Use demand fractions if total demand is greater than zero
          IF (pFlags%lNonPondedAg_Defined) ElemDemandFrac(1:NCrops)                     = pCrops(:,indxElem)%Demand
          IF (pFlags%lPondedAg_Defined)    ElemDemandFrac(NCrops+1:NCrops+NPondedCrops) = pPondedCrops(:,indxElem)%Demand
          IF (pFlags%lUrban_Defined)       ElemDemandFrac(NCrops+NPondedCrops+1)        = pUrban(indxElem)%Demand
        END IF
        CALL NormalizeArray(ElemDemandFrac)
        ElemDemandFrac_Ag = SUM(ElemDemandFrac(1:NCrops+NPondedCrops)) 
        IF (pFlags%lNonPondedAg_Defined) pCrops(:,indxElem)%ElemDemandFrac       = ElemDemandFrac(1:NCrops)
        IF (pFlags%lPondedAg_Defined)    pPondedCrops(:,indxElem)%ElemDemandFrac = ElemDemandFrac(NCrops+1:NCrops+NPondedCrops) 
        IF (pFlags%lUrban_Defined)       pUrban(indxElem)%ElemDemandFrac         = ElemDemandFrac(NCrops+NPondedCrops+1)
        IF (ElemDemandFrac_Ag .EQ. 0.0) THEN
          IF (pFlags%lNonPondedAg_Defined) pCrops(:,indxElem)%ElemDemandFrac       = 0.0
          IF (pFlags%lPondedAg_Defined)    pPondedCrops(:,indxElem)%ElemDemandFrac = 0.0
        ELSE
          IF (pFlags%lNonPondedAg_Defined) pCrops(:,indxElem)%ElemDemandFrac_Ag       = ElemDemandFrac(1:NCrops) / ElemDemandFrac_Ag
          IF (pFlags%lPondedAg_Defined)    pPondedCrops(:,indxElem)%ElemDemandFrac_Ag = ElemDemandFrac(NCrops+1:NCrops+NPondedCrops) / ElemDemandFrac_Ag
        END IF
      END DO
      
      !Compute fractions to distribute regional supply to elements
      IF (pFlags%lNonPondedAg_Defined  .OR. pFlags%lPondedAg_Defined) THEN
        RegionalDemand_Ag  = RegionalDemand(AppGrid,RootZone,AgIndex)
      ELSE
        RegionalDemand_Ag = 0.0
      END IF
      IF (pFlags%lUrban_Defined) THEN
        RegionalDemand_Urb = RegionalDemand(AppGrid,RootZone,UrbIndex)
      ELSE
        RegionalDemand_Urb = 0.0
      END IF  
      DO indxRegion=1,AppGrid%NSubregions
        !Ag related information
        !----------------------
        !No ag water demand in the region
        IF (RegionalDemand_Ag(indxRegion) .EQ. 0.0) THEN
          RegionDemandArea = RegionalLUArea_ForSingleRegion(indxRegion,AppGrid,RootZone,AgIndex)
              
          !Non-zero irrigable area
          IF (RegionDemandArea .GT. 0.0) THEN
            DO indxElem=1,AppGrid%AppSubregion(indxRegion)%NRegionElements
              iElem                        = AppGrid%AppSubregion(indxRegion)%RegionElements(indxElem)
              IF (pFlags%lNonPondedAg_Defined) pRatioElemToRegion_Ag(iElem) = pRatioElemToRegion_Ag(iElem) + SUM(pCrops(:,iElem)%Area) / RegionDemandArea 
              IF (pFlags%lPondedAg_Defined)    pRatioElemToRegion_Ag(iElem) = pRatioElemToRegion_Ag(iElem) + SUM(pPondedCrops(:,iElem)%Area) / RegionDemandArea 
            END DO
            
          !Zero irrigable area
          ELSE
            RegionArea = AppGrid%AppSubregion(indxRegion)%Area
            DO indxElem=1,AppGrid%AppSubregion(indxRegion)%NRegionElements
              iElem                        = AppGrid%AppSubregion(indxRegion)%RegionElements(indxElem)
              pRatioElemToRegion_Ag(iElem) = AppGrid%AppElement(iElem)%Area / RegionArea
            END DO
          END IF

        !Otherwise distribution is based on the ag demand at the element
        ELSE
          DO indxElem=1,AppGrid%AppSubregion(indxRegion)%NRegionElements
            iElem                        = AppGrid%AppSubregion(indxRegion)%RegionElements(indxElem)
            pRatioElemToRegion_Ag(iElem) = ElemDemand_Ag(iElem) / RegionalDemand_Ag(indxRegion)
          END DO
        END IF


        !Urban related information
        !----------------------
        IF (.NOT. pFlags%lUrban_Defined) CYCLE
        
        !No urban water demand in the region
        IF (RegionalDemand_Urb(indxRegion) .EQ. 0.0) THEN
          RegionDemandArea = RegionalLUArea_ForSingleRegion(indxRegion,AppGrid,RootZone,UrbIndex)
              
          !Non-zero urban area
          IF (RegionDemandArea .GT. 0.0) THEN
            DO indxElem=1,AppGrid%AppSubregion(indxRegion)%NRegionElements
              iElem                         = AppGrid%AppSubregion(indxRegion)%RegionElements(indxElem)
              pRatioElemToRegion_Urb(iElem) = pUrban(iElem)%Area / RegionDemandArea 
            END DO
            
          !Zero urban area
          ELSE
            RegionArea = AppGrid%AppSubregion(indxRegion)%Area
            DO indxElem=1,AppGrid%AppSubregion(indxRegion)%NRegionElements
              iElem                         = AppGrid%AppSubregion(indxRegion)%RegionElements(indxElem)
              pRatioElemToRegion_Urb(iElem) = AppGrid%AppElement(iElem)%Area / RegionArea
            END DO
          END IF

        !Otherwise distribution is based on the urban demand at the element
        ELSE
          DO indxElem=1,AppGrid%AppSubregion(indxRegion)%NRegionElements
            iElem                         = AppGrid%AppSubregion(indxRegion)%RegionElements(indxElem)
            pRatioElemToRegion_Urb(iElem) = pUrban(iElem)%Demand / RegionalDemand_Urb(indxRegion)
          END DO
        END IF
      END DO
      
    END ASSOCIATE
    
  END SUBROUTINE ComputeDemandSupplyRelatedFracs
  
  
  ! -------------------------------------------------------------
  ! --- ZERO OUT WATER SUPPLY
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_ZeroSupply(RootZone)
    CLASS(RootZone_v41_Type) :: RootZone
    
    !Inform user
    CALL EchoProgress('Resetting water supply to elements')
    
    ASSOCIATE (pElemSupply => RootZone%ElemSupply)
      pElemSupply%Diversion_Ag   = 0.0
      pElemSupply%Diversion_Urb  = 0.0
      pElemSupply%Pumping_Ag     = 0.0
      pElemSupply%Pumping_Urb    = 0.0
      pElemSupply%UpstrmRunoff   = 0.0
    END ASSOCIATE
    
  END SUBROUTINE RootZone_v41_ZeroSupply


  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF ROOT ZONE RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_ConvertTimeUnit(RootZone,NewUnit)
    CLASS(RootZone_v41_Type)    :: RootZone
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert
    Factor                         = TimeIntervalConversion(NewUnit,RootZone%VarTimeUnit)
    RootZone%VarTimeUnit           = NewUnit
    RootZone%ElemSoilsData%HydCond = RootZone%ElemSoilsData%HydCond * Factor
    RootZone%HydCondPonded         = RootZone%HydCondPonded * Factor
    
  END SUBROUTINE RootZone_v41_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- ZERO OUT SOIL OISTURE VARIABLES RELATED TO LAND USE AREA CHANGE
  ! -------------------------------------------------------------
  SUBROUTINE ZeroRedistributedMoist(RootZone)
    TYPE(RootZone_v41_Type) :: RootZone
    
    RootZone%NonPondedAgRootZone%Crops%SoilmCh = 0.0
    RootZone%PondedAgRootZone%Crops%SoilmCh    = 0.0
    RootZone%UrbanRootZone%UrbData%SoilmCh     = 0.0
    RootZone%NVRVRootZone%NativeVeg%SoilmCh    = 0.0
    RootZone%NVRVRootZone%RiparianVeg%SoilmCh  = 0.0
    
    RootZone%NonPondedAgRootZone%Crops%PercCh  = 0.0
    RootZone%PondedAgRootZone%Crops%PercCh     = 0.0
    RootZone%UrbanRootZone%UrbData%PercCh      = 0.0
    RootZone%NVRVRootZone%NativeVeg%PercCh     = 0.0
    RootZone%NVRVRootZone%RiparianVeg%PercCh   = 0.0
    
  END SUBROUTINE ZeroRedistributedMoist
  
  
  
  ! -------------------------------------------------------------
  ! --- REDISTRIBUTE MOISTURE BASED ON AREA CHANGES
  ! -------------------------------------------------------------
  SUBROUTINE RedistributeMoist(NElements,DeltaT,RootZone)
    INTEGER,INTENT(IN)      :: NElements
    REAL(8),INTENT(IN)      :: DeltaT
    TYPE(RootZone_v41_Type) :: RootZone

    !Local variables
    INTEGER                                        :: indxElem,indxLU,indxUrban_In,indxUrban_Out,indxNV,indxRV,NCrops,NLandsExt
    REAL(8)                                        :: ratio(3),SOILM_INT_Precip,SOILM_INT_AW,SOILM_INT_Oth,TotalReduc,                 &
                                                      Factor_Precip,Factor_AW,Factor_Oth,TotalPorosity,PerviousFrac,                   &
                                                      rValue,rTotalPorosityD
    REAL(8),DIMENSION(RootZone%NLands+1)           :: AreaExpand,AreaReduced,Area,Area_P,RootDepth,SM_Precip,SM_AW,SM_Oth
    REAL(8),DIMENSION(RootZone%NLands+1,NElements) :: SoilMCh_Precip,SoilMCh_AW,SoilMCh_Oth,SoilM_Precip,SoilM_AW,SoilM_Oth,PercCh
    LOGICAL                                        :: lNonPondedAg_Defined,lPondedAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    lNonPondedAg_Defined = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg_Defined    = RootZone%Flags%lPondedAg_Defined
    lUrban_Defined       = RootZone%Flags%lUrban_Defined
    lNVRV_Defined        = RootZone%Flags%lNVRV_Defined
    NCrops               = RootZone%NonPondedAgRootZone%NCrops
    NLandsExt            = RootZone%NLands + 1
    indxUrban_In         = NCrops + NPondedCrops + 1
    indxUrban_Out        = NCrops + NPondedCrops + 2
    indxNV               = NCrops + NPondedCrops + 3
    indxRV               = NCrops + NPondedCrops + 4
    Area                 = 0.0
    Area_P               = 0.0
    SM_Precip            = 0.0
    SM_AW                = 0.0
    SM_Oth               = 0.0
    AreaExpand           = 0.0
    AreaReduced          = 0.0
    RootDepth            = 0.0   ;   IF (lNonPondedAg_Defined) RootDepth(1:NCrops) = RootZone%NonPondedAgRootZone%RootDepth
                                     IF (lPondedAg_Defined) RootDepth(NCrops+1:NCrops+NpondedCrops) = RootZone%PondedAgRootZone%RootDepth
                                     IF (lUrban_Defined) THEN
                                       RootDepth(indxUrban_In)  = RootZone%UrbanRootZone%RootDepth
                                       RootDepth(indxUrban_Out) = RootZone%UrbanRootZone%RootDepth
                                     END IF
                                     IF (lNVRV_Defined) THEN
                                       RootDepth(indxNV) = RootZone%NVRVRootZone%RootDepth_Native
                                       RootDepth(indxRV) = RootZone%NVRVRootZone%RootDepth_Riparian
                                     END IF
    
    ASSOCIATE (pNonPondedCrops => RootZone%NonPondedAgRootZone%Crops  , &
               pPondedCrops    => RootZone%PondedAgRootZone%Crops     , &
               pUrban          => RootZone%UrbanRootZone%UrbData      , &
               pNV             => RootZone%NVRVRootZone%NativeVeg     , &
               pRV             => RootZone%NVRVRootZone%RiparianVeg   , &
               pSoilsData      => RootZone%ElemSoilsData              )
               
      !Compute the details of land use area expansion and contraction to compute new soil moisture contents
      DO indxElem=1,NElements
        
        !Initialize
        TotalPorosity = pSoilsData(indxElem)%TotalPorosity
        IF (lNonPondedAg_Defined) THEN
          Area(1:NCrops)      = pNonPondedCrops(:,indxElem)%Area
          Area_P(1:NCrops)    = pNonPondedCrops(:,indxElem)%Area_P
          SM_Precip(1:NCrops) = pNonPondedCrops(:,indxElem)%SoilM_Precip_P
          SM_AW(1:NCrops)     = pNonPondedCrops(:,indxElem)%SoilM_AW_P
          SM_Oth(1:NCrops)    = pNonPondedCrops(:,indxElem)%SoilM_Oth_P
        END IF 
        IF (lPondedAg_Defined) THEN
          Area(NCrops+1:NCrops+NPondedCrops)      = pPondedCrops(:,indxElem)%Area
          Area_P(NCrops+1:NCrops+NPondedCrops)    = pPondedCrops(:,indxElem)%Area_P
          SM_Precip(NCrops+1:NCrops+NPondedCrops) = pPondedCrops(:,indxElem)%SoilM_Precip_P
          SM_AW(NCrops+1:NCrops+NPondedCrops)     = pPondedCrops(:,indxElem)%SoilM_AW_P
          SM_Oth(NCrops+1:NCrops+NPondedCrops)    = pPondedCrops(:,indxElem)%SoilM_Oth_P
        END IF 
        IF (lUrban_Defined) THEN
          PerviousFrac             = pUrban(indxElem)%PerviousFrac
          Area_P(indxUrban_Out)    = pUrban(indxElem)%Area_P * PerviousFrac 
          Area_P(indxUrban_In)     = pUrban(indxElem)%Area_P - Area_P(indxUrban_Out) 
          Area(indxUrban_Out)      = pUrban(indxElem)%Area * PerviousFrac   
          Area(indxUrban_In)       = pUrban(indxElem)%Area - Area(indxUrban_Out)   
          SM_Precip(indxUrban_Out) = pUrban(indxElem)%SoilM_Precip_P
          SM_AW(indxUrban_Out)     = pUrban(indxElem)%SoilM_AW_P
          SM_Oth(indxUrban_Out)    = pUrban(indxElem)%SoilM_Oth_P
        END IF
        IF (lNVRV_Defined) THEN
          Area(indxNV)      = pNV(indxElem)%Area
          Area(indxRV)      = pRV(indxElem)%Area
          Area_P(indxNV)    = pNV(indxElem)%Area_P
          Area_P(indxRV)    = pRV(indxElem)%Area_P
          SM_Precip(indxNV) = pNV(indxElem)%SoilM_Precip_P
          SM_Precip(indxRV) = pRV(indxElem)%SoilM_Precip_P
          SM_AW(indxNV)     = pNV(indxElem)%SoilM_AW_P        !Although there is no irrigtaion for native and riparian veg, they
          SM_AW(indxRV)     = pRV(indxElem)%SoilM_AW_P        !  can inherit moisture due to irrigtaion when their area expands into ag and urban lands
          SM_Oth(indxNV)    = pNV(indxElem)%SoilM_Oth_P
          SM_Oth(indxRV)    = pRV(indxElem)%SoilM_Oth_P
        END IF 

        !Changes in element land use areas
        AreaExpand    = MAX(Area-Area_P,0.0)                        !Expansion in each land use area
        AreaReduced   = MAX(Area_P-Area,0.0)                        !Reduction in each land use area
        TotalReduc    = SUM(AreaReduced)                            !Total area reduction
        IF (TotalReduc .EQ. 0.0) THEN
          SoilM_Precip(:,indxElem)   = SM_Precip
          SoilM_AW(:,indxElem)       = SM_AW
          SoilM_Oth(:,indxElem)      = SM_Oth
          SoilMCh_Precip(:,indxElem) = 0.0
          SoilMCh_AW(:,indxElem)     = 0.0
          SoilMCh_Oth(:,indxElem)    = 0.0
          PercCh(:,indxElem)         = 0.0
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
            PercCh(indxLU,indxElem)         = PercCh(indxLU,indxElem) / DeltaT * Area(indxLU)  !Convert perc due to area chnage to volumetric rate
          ELSE
            SoilM_Precip(indxLU,indxElem) = 0.0
            SoilM_AW(indxLU,indxElem)     = 0.0
            SoilM_Oth(indxLU,indxElem)    = 0.0
            PercCh(indxLU,indxElem)       = 0.0
          END IF
                    
        END DO
      END DO
      
      !Store data in persisting arrays
      !Also, update SoilM_P to reflect changes in the moisture due to land expansion/shrinking
      !*Note: SoilM_P is now the moisture at the beginning of time step after redistribution.
      !       It is necessary to be careful when computing the reporting variables
      IF (lNonPondedAg_Defined) THEN
        pNonPondedCrops%SoilM_Precip = SoilM_Precip(1:NCrops,:) 
        pNonPondedCrops%SoilM_AW     = SoilM_AW(1:NCrops,:) 
        pNonPondedCrops%SoilM_Oth    = SoilM_Oth(1:NCrops,:) 
        pNonPondedCrops%SoilMCh      = SoilMCh_Precip(1:NCrops,:) + SoilMCh_AW(1:NCrops,:) + SoilMCh_Oth(1:NCrops,:)
        pNonPondedCrops%PercCh       = PercCh(1:NCrops,:)
      END IF
      IF (lPondedAg_Defined) THEN
        pPondedCrops%SoilM_Precip = SoilM_Precip(NCrops+1:NCrops+NPondedCrops,:) 
        pPondedCrops%SoilM_AW     = SoilM_AW(NCrops+1:NCrops+NPondedCrops,:)
        pPondedCrops%SoilM_Oth    = SoilM_Oth(NCrops+1:NCrops+NPondedCrops,:)
        pPondedCrops%SoilMCh      = SoilmCh_Precip(NCrops+1:NCrops+NPondedCrops,:) + SoilMCh_AW(NCrops+1:NCrops+NPondedCrops,:) + SoilMCh_Oth(NCrops+1:NCrops+NPondedCrops,:)
        pPondedCrops%PercCh       = PercCh(NCrops+1:NCrops+NPondedCrops,:)
      END IF
      IF (lUrban_Defined) THEN
        !Consolidate urban values to urban outdoors
        pUrban%SoilM_Precip = SoilM_Precip(indxUrban_Out,:) + SoilM_Precip(indxUrban_In,:) * (1d0/pUrban%PerviousFrac - 1d0) 
        pUrban%SoilM_AW     = SoilM_AW(indxUrban_Out,:) + SoilM_AW(indxUrban_In,:) * (1d0/pUrban%PerviousFrac - 1d0) 
        pUrban%SoilM_Oth    = SoilM_Oth(indxUrban_Out,:) + SoilM_Oth(indxUrban_In,:) * (1d0/pUrban%PerviousFrac - 1d0) 
        pUrban%SoilMCh      = SoilmCh_Precip(indxUrban_Out,:) + SoilmCh_Precip(indxUrban_In,:) + SoilmCh_AW(indxUrban_Out,:) + SoilmCh_AW(indxUrban_In,:) + SoilmCh_Oth(indxUrban_Out,:) + SoilmCh_Oth(indxUrban_In,:)
        pUrban%PercCh       = PercCh(indxUrban_Out,:) + PercCh(indxUrban_In,:)
        DO indxElem=1,NElements
          rTotalPorosityD = pSoilsData(indxElem)%TotalPorosity*RootDepth(indxUrban_Out)
          ASSOCIATE (pElemUrban => pUrban(indxElem))
            rValue = pElemUrban%SoilM_Precip + pElemUrban%SoilM_AW + pElemUrban%SoilM_Oth
            IF (rValue .LE. rTotalPorosityD) CYCLE
            pElemUrban%PercCh       = pElemUrban%PercCh + (rValue - rTotalPorosityD) / DeltaT * pElemUrban%Area * pElemUrban%PerviousFrac
            ratio                   = [pElemUrban%SoilM_Precip , pElemUrban%SoilM_AW , pElemUrban%SoilM_Oth]
            CALL NormalizeArray(ratio)
            pElemUrban%SoilM_Precip = rTotalPorosityD * ratio(1)
            pElemUrban%SoilM_AW     = rTotalPorosityD * ratio(2)
            pElemUrban%SoilM_Oth    = rTotalPorosityD * ratio(3)
          END ASSOCIATE
        END DO
      END IF
      IF (lNVRV_Defined) THEN
        pNV%SoilM_Precip = SoilM_Precip(indxNV,:)
        pNV%SoilM_AW     = SoilM_AW(indxNV,:)
        pNV%SoilM_Oth    = SoilM_Oth(indxNV,:)
        pNV%SoilMCh      = SoilmCh_Precip(indxNV,:) + SoilmCh_AW(indxNV,:) + SoilMCh_Oth(indxNV,:)
        pNV%PercCh       = PercCh(indxNV,:)
        pRV%SoilM_Precip = SoilM_Precip(indxRV,:)
        pRV%SoilM_AW     = SoilM_AW(indxRV,:)
        pRV%SoilM_Oth    = SoilM_Oth(indxRV,:)
        pRV%SoilMCh      = SoilmCh_Precip(indxRV,:) + SoilMCh_AW(indxRV,:) + SoilMCh_Oth(indxRV,:)
        pRV%PercCh       = PercCh(indxRV,:)
      END IF
      CALL AdvanceStateLocal(RootZone , lAdvanceArea=.FALSE.)  !Do not advance the area in time; we are only updating the soil moisture beacuse it changed
        
    END ASSOCIATE
    
  END SUBROUTINE RedistributeMoist
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF ROOT ZONE IN TIME WITH A CHOICE TO ADVANCE AREA AS WELL
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceStateLocal(RootZone,lAdvanceArea)
    TYPE(RootZone_v41_Type) :: RootZone
    LOGICAL,INTENT(IN)      :: lAdvanceArea
    
    !Local variables
    INTEGER :: NElements,indxElem,indxLU
    
    !Initialize
    NElements = SIZE(RootZone%ElemSoilsData)
    
    RootZone%RSoilM_P = RootZone%RSoilM
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        DO indxElem=1,NElements
            DO indxLU=1,RootZone%NonPondedAgRootZone%NCrops
                RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip_P = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip
                RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW_P     = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW
                RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth_P    = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth
                IF (lAdvanceArea)  &
                    RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%Area_P     = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%Area
            END DO
        END DO
    END IF
    
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        DO indxElem=1,NElements
            DO indxLU=1,NPondedCrops
                RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip_P    = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip
                RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW_P        = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW
                RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth_P       = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth
                IF (lAdvanceArea)  &
                    RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%Area_P        = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%Area
           END DO
        END DO
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        DO indxElem=1,NElements
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Precip_P     = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Precip
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_AW_P         = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_AW
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Oth_P        = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Oth
            IF (lAdvanceArea)  &
                 RootZone%UrbanRootZone%UrbData(indxElem)%Area_P        = RootZone%UrbanRootZone%UrbData(indxElem)%Area
       END DO
    END IF
    
    IF (RootZone%Flags%lNVRV_Defined) CALL NativeRiparianLandUse_AdvanceState(lAdvanceArea,RootZone%NVRVRootZone)
    
  END SUBROUTINE AdvanceStateLocal
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF ROOT ZONE IN TIME INCLUDING AREA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_AdvanceState(RootZone)
    CLASS(RootZone_v41_Type) :: RootZone
    
    !Store previous moisture stoarge in special arrays before they may be updated
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        RootZone%NonPondedAgRootZone%RootDepth_P                       = RootZone%NonPondedAgRootZone%RootDepth
        RootZone%NonPondedAgRootZone%Crops%SoilM_Precip_P_BeforeUpdate = RootZone%NonPondedAgRootZone%Crops%SoilM_Precip
        RootZone%NonPondedAgRootZone%Crops%SoilM_AW_P_BeforeUpdate     = RootZone%NonPondedAgRootZone%Crops%SoilM_AW
        RootZone%NonPondedAgRootZone%Crops%SoilM_Oth_P_BeforeUpdate    = RootZone%NonPondedAgRootZone%Crops%SoilM_Oth
    END IF
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        RootZone%PondedAgRootZone%Crops%SoilM_Precip_P_BeforeUpdate = RootZone%PondedAgRootZone%Crops%SoilM_Precip
        RootZone%PondedAgRootZone%Crops%SoilM_AW_P_BeforeUpdate     = RootZone%PondedAgRootZone%Crops%SoilM_AW
        RootZone%PondedAgRootZone%Crops%SoilM_Oth_P_BeforeUpdate    = RootZone%PondedAgRootZone%Crops%SoilM_Oth
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

    CALL AdvanceStateLocal(RootZone,lAdvanceArea=.TRUE.)
        
  END SUBROUTINE RootZone_v41_AdvanceState

  
  ! -------------------------------------------------------------
  ! --- DISTRIBUTE A FLOW DESTINED TO ELEMENT OR SUBREGION TO ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE FlowToElements(Flow,AppGrid,RootZone,SupplyToLUIndex,ToElements)
    REAL(8),INTENT(IN)                 :: Flow(:)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v41_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: SupplyToLUIndex
    REAL(8)                            :: ToElements(:)
    
    !Local variables
    INTEGER :: indx,iDest,indxElem,iElem,iElemRegion
    REAL(8) :: rFlow,rFraction
    
    !First process flows to elements
    DO indx=1,SIZE(RootZone%ElemFlowToElements)
        iElem             = RootZone%ElemFlowToElements(indx)%iElement
        iDest             = RootZone%ElemFlowToElements(indx)%iDestID
        ToElements(iDest) = ToElements(iDest) + Flow(iElem)
    END DO
    
    !Then process flows to subregions
    DO indx=1,SIZE(RootZone%ElemFlowToSubregions)
        iElem = RootZone%ElemFlowToSubregions(indx)%iElement
        rFlow = Flow(iElem)
        IF (rFlow .EQ. 0.0) CYCLE
        iDest = RootZone%ElemFlowToSubregions(indx)%iDestID
        DO indxElem=1,AppGrid%AppSubregion(iDest)%NRegionElements
            iElemRegion = AppGrid%AppSubregion(iDest)%RegionElements(indxElem)
            SELECT CASE (SupplyToLUIndex)
              !Ag supply in element
              CASE (AgIndex)
                ToElements(iElemRegion) = ToElements(iElemRegion) + RootZone%Ratio_ElemSupplyToRegionSupply_Ag(iElemRegion) * rFlow
              !Urban supply in element
              CASE (UrbIndex)
                ToElements(iElemRegion) = ToElements(iElemRegion) + RootZone%Ratio_ElemSupplyToRegionSupply_Urb(iElemRegion) * rFlow
              !Flow from upstream elements into element
              CASE DEFAULT
                rFraction               = (RootZone%Ratio_ElemSupplyToRegionSupply_Urb(iElemRegion) + RootZone%Ratio_ElemSupplyToRegionSupply_Ag(iElemRegion)) /2d0
                ToElements(iElemRegion) = ToElements(iElemRegion) + rFraction * rFlow
            END SELECT  
        END DO
    END DO
            
  END SUBROUTINE FlowToElements


  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL PERCOLATION
  ! -------------------------------------------------------------
  FUNCTION RootZone_v41_RegionalPerc(RootZone,AppGrid) RESULT(RPERC)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: RPERC(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegion
    REAL(8) :: Perc(AppGrid%NElements)

    !Initialize
    NRegion = AppGrid%NSubregions
    RPERC   = 0.0
    
    !Return if Root zone is not simulated
    IF (RootZone%NLands .EQ. 0) RETURN
        
    ASSOCIATE (pFlags => RootZone%Flags)
               
      !Initialize
      Perc = 0.0
      
      !Non-ponded ag
      IF (pFlags%lNonPondedAg_Defined) Perc = SUM(RootZone%NonPondedAgRootZone%Crops%Perc,DIM=1)    

      !Ponded ag
      IF (pFlags%lPondedAg_Defined) Perc = Perc + SUM(RootZone%PondedAgRootZone%Crops%Perc,DIM=1)    
      
      !Urban
      IF (pFlags%lUrban_Defined) Perc = Perc + RootZone%UrbanRootZone%UrbData%Perc    
      
      !Native and riparian veg
      IF (pFlags%lNVRV_Defined) Perc = Perc + RootZone%NVRVRootZone%NativeVeg%Perc + RootZone%NVRVRootZone%RiparianVeg%Perc   
    
      !Compute sub-regional perc
      RPERC(1:NRegion) = AppGrid%AccumElemValuesToSubregions(Perc)
      
      !Compute perc for the entire model area
      RPERC(NRegion+1) = SUM(RPERC(1:NRegion))
      
    END ASSOCIATE

  END FUNCTION RootZone_v41_RegionalPerc
  

  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_RegionalReturnFlow_Ag(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: RReturnFlow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegion
    REAL(8) :: ReturnFlow(AppGrid%NElements)

    !Initialize
    NRegion = AppGrid%NSubregions
    
    !Return if Root zone is not simulated
    IF (RootZone%NLands .EQ. 0) THEN
        RReturnFlow = 0.0
        RETURN
    END IF
                       
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        ReturnFlow = SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow,DIM=1) 
    ELSE
        ReturnFlow = 0.0
    END IF

    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) ReturnFlow = ReturnFlow + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow,DIM=1)    
    
    !Compute sub-regional return flow
    RReturnFlow(1:NRegion) = AppGrid%AccumElemValuesToSubregions(ReturnFlow)
    
    !Compute return flow for the entire model area
    RReturnFlow(NRegion+1) = SUM(RReturnFlow(1:NRegion))
      
  END SUBROUTINE RootZone_v41_RegionalReturnFlow_Ag
  

  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_RegionalReturnFlow_Urb(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: RReturnFlow(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: NRegion

    !Initialize
    NRegion = AppGrid%NSubregions
    
    !Compute sub-regional return flow
    IF (RootZone%Flags%lUrban_Defined) THEN
        RReturnFlow(1:NRegion) = AppGrid%AccumElemValuesToSubregions(RootZone%UrbanRootZone%UrbData%ReturnFlow)
        RReturnFlow(NRegion+1) = SUM(RReturnFlow(1:NRegion))
    ELSE
        RReturnFlow = 0.0
    END IF
      
  END SUBROUTINE RootZone_v41_RegionalReturnFlow_Urb
  

  ! -------------------------------------------------------------
  ! --- PROCESS LAND USE AREA
  ! -------------------------------------------------------------
  SUBROUTINE ProcessLandUseAreas(AppGrid,TimeStep,RootZone,iStat)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(RootZone_v41_Type)       :: RootZone
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'ProcessLandUseAreas'
    INTEGER                      :: indxElem,NNonPondCrops,NAllCrops,NElements
    REAL(8)                      :: LUArea(RootZone%NLands,AppGrid%NElements)
    
    !Initialize
    iStat         = 0
    NElements     = AppGrid%NElements
    NNonPondCrops = RootZone%NonPondedAgRootZone%NCrops
    NAllCrops     = NNonPondCrops + NPondedCrops
    
    !Zero out the variables that hold information regarding soil moisture change due to land area change
    CALL ZeroRedistributedMoist(RootZone) 
    
    !Return if new data is not read
    IF (.NOT. RootZone_v41_IsLandUseUpdated(RootZone)) RETURN
        
    !Check for errors and process data
    LUArea = 0.0
    DO indxElem=1,NElements

      !If lake element, zero out areas and cycle
      IF (RootZone%Flags%lLakeElems(indxElem)) THEN
        LUArea(:,indxElem) = 0.0
        CYCLE
      END IF
        
      !Store data in the work array
      IF (RootZone%Flags%lNonPondedAg_Defined) LUArea(1:NNonPondCrops,indxElem)           = RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Area
      IF (RootZone%Flags%lPondedAg_Defined)    LUArea(NNonPondCrops+1:NAllCrops,indxElem) = RootZone%PondedAgRootZone%Crops(:,indxElem)%Area
      IF (RootZone%Flags%lUrban_Defined)       LUArea(NAllCrops+1,indxElem)               = RootZone%UrbanRootZone%UrbData(indxElem)%Area
      IF (RootZone%Flags%lNVRV_Defined) THEN
        LUArea(NAllCrops+2,indxElem) = RootZone%NVRVRootZone%NativeVeg(indxElem)%Area
        LUArea(NAllCrops+3,indxElem) = RootZone%NVRVRootZone%RiparianVeg(indxElem)%Area
      END IF
      
      !Check for zero area
      IF (ALL(LUArea(:,indxElem) .LE. 0.0)) THEN
          CALL SetLastMessage('Total land use area is zero at element ' // TRIM(IntToText(indxElem)) // '!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Normalize the land use areas
      CALL NormalizeArray(LUArea(:,indxElem))

      !Compute final land use areas 
      LUArea(:,indxElem) = LUArea(:,indxElem) * AppGrid%AppElement(indxElem)%Area
      
    END DO
      
    ASSOCIATE (pNonPondedAg => RootZone%NonPondedAgRootZone  , &
               pPondedAg    => RootZone%PondedAgRootZone     , &
               pUrban       => RootZone%UrbanRootZone        , &
               pNVRV        => RootZone%NVRVRootZone         )
               
      !Store the areas in persistent objects
      CALL NonPondedAgLandUse_SetAreas(LUArea(1:NNonPondCrops,:),pNonPondedAg)
      CALL PondedAgLandUse_SetAreas(LUArea(NNonPondCrops+1:NAllCrops,:),pPondedAg) 
      CALL UrbanLandUse_SetAreas(LUArea(NAllCrops+1,:),pUrban) 
      CALL NativeRiparianLandUse_SetAreas(LUArea(NAllCrops+2:NAllCrops+3,:),pNVRV)
    
      !If first time step, do the advancement of land use areas in time again since previous one had no effect
      IF (TimeStep%CurrentTimeStep .EQ. 1) THEN
        CALL NonPondedAgLandUse_AdvanceAreas(pNonPondedAg)
        CALL PondedAgLandUse_AdvanceAreas(pPondedAg) 
        CALL UrbanLandUse_AdvanceAreas(pUrban) 
        CALL NativeRiparianLandUse_AdvanceAreas(pNVRV) 
 
      !Otherwise redistribute soil moisture based on the decreased/increased land use area
      ELSE
        CALL RedistributeMoist(NElements,TimeStep%DeltaT,RootZone)
      END IF
      
      !Compute developed areas in each element
      IF (RootZone%Flags%lNonPondedAg_Defined) RootZone%ElemDevelopedArea = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
      IF (RootZone%Flags%lPondedAg_Defined)    RootZone%ElemDevelopedArea = RootZone%ElemDevelopedArea + SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)
      IF (RootZone%Flags%lUrban_Defined)       RootZone%ElemDevelopedArea = RootZone%ElemDevelopedArea + SUM(RootZone%UrbanRootZone%UrbData%Area)
      
    END ASSOCIATE
    
  END SUBROUTINE ProcessLandUseAreas
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF PROPER TIME-SERIES DATA COLUMNS ARE POINTED TO
  ! -------------------------------------------------------------
  SUBROUTINE CheckTSDataPointers(RootZone,Precip,ET,iStat)
    CLASS(RootZone_v41_Type),INTENT(IN) :: RootZone
    TYPE(PrecipitationType),INTENT(IN)  :: Precip
    TYPE(ETType),INTENT(IN)             :: ET
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'CheckTSDataPointers'
    INTEGER                      :: iElem(1),iETColMax,indxElem,iCrop(1),iReturnFlowCol(1),iReuseCol(1)
    LOGICAL                      :: lNonPondedAg_Defined,lPondedAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    iStat                = 0
    lNonPondedAg_Defined = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg_Defined    = RootZone%Flags%lPondedAg_Defined
    lUrban_Defined       = RootZone%Flags%lUrban_Defined
    lNVRV_Defined        = RootZone%Flags%lNVRV_Defined
    
    ASSOCIATE (pElemPrecipData => RootZone%ElemPrecipData      , &
               pNonPondedAg    => RootZone%NonPondedAgRootZone , &
               pPondedAg       => RootZone%PondedAgRootZone    , &
               pUrban          => RootZone%UrbanRootZone       , &
               pNVRV           => RootZone%NVRVRootZone        )
    
        !Check precipitation
        IF (Precip%GetNDataColumns() .LT. MAXVAL(pElemPrecipData%iColPrecip)) THEN
            iElem = MAXLOC(pElemPrecipData%iColPrecip)
            MessageArray(1) = 'Precipitation data column for element '//TRIM(IntToText(iElem(1)))//' in the root zone component'
            MessageArray(2) = 'is greater than the available data columns in the Precipitation Data file!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Check ET and other data
        iETColMax = ET%GetNDataColumns()
        DO indxElem=1,SIZE(pElemPrecipData)
          !Non-ponded crops
          IF (lNonPondedAg_Defined) THEN
              IF (iETColMax .LT. MAXVAL(pNonPondedAg%Crops(:,indxElem)%iColETc)) THEN
                  iCrop = MAXLOC(pNonPondedAg%Crops(:,indxElem)%iColETc)
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(indxElem))//' and non-ponded crop '//TRIM(pNonPondedAg%CropCodes(iCrop(1)))
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
              CALL RootZone%ReturnFracFile%CheckColNum('Return flow fractions data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(indxElem))//')',pNonPondedAg%Crops(:,indxElem)%iColReturnFrac,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%ReuseFracFile%CheckColNum('Re-use fractions data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(indxElem))//')',pNonPondedAg%Crops(:,indxElem)%iColReuseFrac,.TRUE.,iStat)         ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%IrigPeriodFile%CheckColNum('Irrigation periods data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(indxElem))//')',pNonPondedAg%Crops(:,indxElem)%iColIrigPeriod,.TRUE.,iStat)     ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%AgWaterDemandFile%CheckColNum('Agricultural demand data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(indxElem))//')',pNonPondedAg%iColAgDemand(:,indxElem),.FALSE.,iStat)        ;  IF (iStat .EQ. -1) RETURN
          END IF
          
          !Ponded crops
          IF (lPondedAg_Defined) THEN
              IF (iETColMax .LT. MAXVAL(pPondedAg%Crops(:,indxElem)%iColETc)) THEN
                  iCrop = MAXLOC(pPondedAg%Crops(:,indxElem)%iColETc)
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(indxElem))//' and ponded crop '//TRIM(pPondedAg%CropCodes(iCrop(1)))
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
              CALL RootZone%IrigPeriodFile%CheckColNum('Irrigation periods data file (referenced by ponded crop data file for element '//TRIM(IntToText(indxElem))//')',pPondedAg%Crops(:,indxElem)%iColIrigPeriod,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%AgWaterDemandFile%CheckColNum('Agricultural demand data file (referenced by ponded crop data file for element '//TRIM(IntToText(indxElem))//')',pPondedAg%iColAgDemand(:,indxElem),.FALSE.,iStat)     ;  IF (iStat .EQ. -1) RETURN
          END IF
          
          !Urban
          IF (lUrban_Defined) THEN
              IF (iETColMax .LT. pUrban%UrbData(indxElem)%iColETc) THEN
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(indxElem))//' at urban lands '
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF 
              iReturnFlowCol(1) = pUrban%UrbData(indxElem)%iColReturnFrac
              iReuseCol(1)      = pUrban%UrbData(indxElem)%iColReuseFrac
              CALL RootZone%ReturnFracFile%CheckColNum('Return flow fractions data file (referenced by urban data file for element '//TRIM(IntToText(indxElem))//')',iReturnFlowCol,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%ReuseFracFile%CheckColNum('Re-use fractions data file (referenced by urban data file for element '//TRIM(IntToText(indxElem))//')',iReuseCol,.TRUE.,iStat)             ;  IF (iStat .EQ. -1) RETURN
          END IF
          
          !Native 
          IF (lNVRV_Defined) THEN
              IF (iETColMax .LT. pNVRV%NativeVeg(indxElem)%iColETc) THEN
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(indxElem))//' at native vegetation lands '
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF 
              
          !Riparian 
              IF (iETColMax .LT. pNVRV%RiparianVeg(indxElem)%iColETc) THEN
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(indxElem))//' at riparian vegetation lands '
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
          END IF   
        END DO
        
    END ASSOCIATE
    
  END SUBROUTINE CheckTSDataPointers
  

  ! -------------------------------------------------------------
  ! --- COMPUTE MAXIMUM POSSIBLE ET FROM GW 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v41_ComputeETFromGW_Max(RootZone,DepthToGW,Sy)
    CLASS(RootZone_v41_Type) :: RootZone
    REAL(8),INTENT(IN)       :: DepthToGW(:),Sy(:)
    
    REAL(8) :: CapillaryRise(SIZE(RootZone%ElemSoilsData))
    
    !Compute gw inflow if necessary
    ASSOCIATE (pFlags => RootZone%Flags)
        IF (.NOT. pFlags%lComputeETFromGW) RETURN
        CapillaryRise = RootZone%ElemSoilsData%CapillaryRise
        IF (pFlags%lNonPondedAg_Defined) CALL NonPondedAgLandUse_ComputeETFromGW_Max(DepthToGW,Sy,CapillaryRise,RootZone%NonPondedAgRootZone)
        IF (pFlags%lPondedAg_Defined)    CALL PondedAgLandUse_ComputeETFromGW_Max(DepthToGW,Sy,CapillaryRise,RootZone%PondedAgRootZone)
        IF (pFlags%lUrban_Defined)       CALL UrbanLandUse_ComputeETFromGW_Max(DepthToGW,Sy,CapillaryRise,RootZone%UrbanRootZone)
        IF (pFlags%lNVRV_Defined)        CALL NativeRiparianLandUse_ComputeETFromGW_Max(DepthToGW,Sy,CapillaryRise,RootZone%NVRVRootZone)
    END ASSOCIATE
   
  END SUBROUTINE RootZone_v41_ComputeETFromGW_Max
  
END MODULE