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
MODULE RootZone_v40
  !$ USE OMP_LIB
  USE Package_Misc                 , ONLY: RealTSDataInFileType                        , &
                                           IntTSDataInFileType                         , &
                                           SolverDataType                              , &
                                           ReadTSData                                  , &
                                           f_iFlowDest_Outside                         , &
                                           f_iFlowDest_StrmNode                        , &
                                           f_iFlowDest_Element                         , &
                                           f_iFlowDest_Lake                            , &
                                           f_iFlowDest_Subregion                       , &
                                           f_iFlowDest_GWElement                       , &
                                           f_iSupply_Diversion                         , &
                                           f_iSupply_Pumping                           , &
                                           f_iSupply_UpstrmElemRunoff                  , &
                                           f_iLocationType_Subregion                   , &
                                           f_iAllLocationIDsListed                     , &
                                           f_iAg                                       , &
                                           f_iUrb                                      , &
                                           f_iNonPondedAg                              , &
                                           f_iRice                                     , &
                                           f_iRefuge                                   , &
                                           f_iNVRV                                     
  USE TimeSeriesUtilities          , ONLY: TimeStepType                                , &
                                           TimeIntervalConversion                      , &
                                           IncrementTimeStamp                          , &
                                           NPeriods                                    , &
                                           CTimeStep_To_RTimeStep                      , &
                                           TimeStampToJulian                           , &
                                           OPERATOR(.TSGE.)
  USE GeneralUtilities             , ONLY: StripTextUntilCharacter                     , &
                                           IntToText                                   , &
                                           LocateInList                                , &
                                           CleanSpecialCharacters                      , &
                                           NormalizeArray                              , &
                                           ConvertID_To_Index                          , &
                                           EstablishAbsolutePathFileName               , &
                                           GetFileDirectory                            , &
                                           UpperCase                                   
  USE IOInterface                  , ONLY: GenericFileType                             , &
                                           f_iUNKNOWN                                      
  USE MessageLogger                , ONLY: SetLastMessage                              , &
                                           EchoProgress                                , &
                                           MessageArray                                , &
                                           f_iFatal                                      
  USE Package_ComponentConnectors  , ONLY: SupplyType                                  
  USE Class_GenericLandUse         , ONLY: GenericLandUseType 
  USE Util_Package_RootZone        , ONLY: WaterSupplyType                             , &
                                           AddStringToStringList                       , &
                                           f_iBudgetType_LWU                           , &
                                           f_iBudgetType_RootZone                      , &
                                           f_iBudgetType_NonPondedCrop_LWU             , &
                                           f_iBudgetType_NonPondedCrop_RZ              , & 
                                           f_iBudgetType_PondedCrop_LWU                , &
                                           f_iBudgetType_PondedCrop_RZ                 , & 
                                           f_cDescription_NPCrop_LWUBudget             , &
                                           f_cDescription_NPCrop_RootZoneBudget        , &
                                           f_cDescription_PCrop_LWUBudget              , &
                                           f_cDescription_PCrop_RootZoneBudget  
  USE Util_RootZone_v40            , ONLY: LWUseBudRawFile_New                         , &
                                           RootZoneBudRawFile_New                      , &
                                           f_iNLWUseBudColumns                         , &
                                           f_iNRootZoneBudColumns                      , & 
                                           f_iNAgLWUseBudColumns                       , &
                                           f_iNAgRootZoneBudColumns                    , &
                                           f_cLWUseBudgetColumnTitles                  , &
                                           f_cRootZoneBudgetColumnTitles               
  USE Class_GenericMoistureData    , ONLY: GenericMoistureDataType                     
  USE Class_NonPondedAgLandUse     , ONLY: NonPondedAgDatabaseType                     
  USE Class_PondedAgLandUse        , ONLY: PondedAgDatabaseType                        , &
                                           f_iNPondedCrops
  USE Class_UrbanLandUse           , ONLY: UrbanDatabaseType 
  USE Class_NativeRiparianLandUse  , ONLY: NativeRiparianDatabaseType                  
  USE Package_Discretization       , ONLY: AppGridType
  USE Package_PrecipitationET      , ONLY: PrecipitationType                           , &
                                           ETType                                      
  USE Package_UnsatZone            , ONLY: RootZoneSoilType                            , &
                                           f_iKUnsatMethodList                         
  USE Class_BaseRootZone           , ONLY: BaseRootZoneType                            , &
                                           FlagsType                                   , &
                                           ElemSurfaceFlowToDestType                   , &
                                           CompileElemSurfaceFlowToDestinationList     , &
                                           ComputeRegionalETPot                        , &
                                           ElementLU_InterpolateExtrapolate            , &
                                           iMeasuredLUDataForSubregion                 , &
                                           iMeasuredLUDataForModelDomain 
  USE Package_Budget               , ONLY: BudgetType                                  , &
                                           f_iMaxLocationNameLen                       , &
                                           f_iColumnHeaderLen
  USE Package_ZBudget              , ONLY: ZBudgetType                                 , &
                                           ZoneListType
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
  PUBLIC :: RootZone_v40_Type    , &
            CheckTSDataPointers  , &
            f_iNGroupLandUse
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseRootZoneType) :: RootZone_v40_Type
    INTEGER                                   :: NLands                       = 0                       !Total number of land use types
    TYPE(RootZoneSoilType),ALLOCATABLE        :: ElemSoilsData(:)                                       !Soils data for each element
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
    PROCEDURE,PASS   :: New                                   => RootZone_v40_New
    PROCEDURE,PASS   :: KillRZImplementation                  => RootZone_v40_Kill
    PROCEDURE,PASS   :: IsLandUseUpdated                      => RootZone_v40_IsLandUseUpdated
    PROCEDURE,PASS   :: GetMaxAndMinNetReturnFlowFrac         => RootZone_v40_GetMaxAndMinNetReturnFlowFrac
    PROCEDURE,PASS   :: GetBudget_List_RZImplementation       => RootZone_v40_GetBudget_List
    PROCEDURE,PASS   :: GetBudget_NColumns                    => RootZone_v40_GetBudget_NColumns
    PROCEDURE,PASS   :: GetBudget_ColumnTitles                => RootZone_v40_GetBudget_ColumnTitles
    PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenRootZone  => RootZone_v40_GetBudget_MonthlyFlows_GivenRootZone
    PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile      => RootZone_v40_GetBudget_MonthlyFlows_GivenFile
    PROCEDURE,PASS   :: GetBudget_TSData_RZImplementation     => RootZone_v40_GetBudget_TSData
    PROCEDURE,PASS   :: GetZBudget_NColumns                   => RootZone_v40_GetZBudget_NColumns
    PROCEDURE,PASS   :: GetZBudget_ColumnTitles               => RootZone_v40_GetZBudget_ColumnTitles
    PROCEDURE,PASS   :: GetNAgCrops                           => RootZone_v40_GetNAgCrops
    PROCEDURE,PASS   :: GetNDemandLocations                   => RootZone_v40_GetNDemandLocations
    PROCEDURE,PASS   :: GetElementPrecipInfilt                => RootZone_v40_GetElementPrecipInfilt
    PROCEDURE,PASS   :: GetElementActualET                    => RootZone_v40_GetElementActualET
    PROCEDURE,PASS   :: GetWaterDemandAll                     => RootZone_v40_GetElementWaterDemand 
    PROCEDURE,PASS   :: GetWaterDemandAtLocations             => RootZone_v40_GetWaterDemandAtLocations 
    PROCEDURE,PASS   :: GetWaterSupply                        => RootZone_v40_GetWaterSupply 
    PROCEDURE,PASS   :: GetElementAgAreas                     => RootZone_v40_GetElementAgAreas
    PROCEDURE,PASS   :: GetElementUrbanAreas                  => RootZone_v40_GetElementUrbanAreas
    PROCEDURE,PASS   :: GetElementNativeVegAreas              => RootZone_v40_GetElementNativeVegAreas
    PROCEDURE,PASS   :: GetElementRiparianVegAreas            => RootZone_v40_GetElementRiparianVegAreas
    PROCEDURE,PASS   :: GetSubregionAgAreas                   => RootZone_v40_GetSubregionAgAreas
    PROCEDURE,PASS   :: GetSubregionUrbanAreas                => RootZone_v40_GetSubregionUrbanAreas
    PROCEDURE,PASS   :: GetSubregionNativeVegAreas            => RootZone_v40_GetSubregionNativeVegAreas
    PROCEDURE,PASS   :: GetSubregionRiparianVegAreas          => RootZone_v40_GetSubregionRiparianVegAreas
    PROCEDURE,PASS   :: GetDemandAgAreas                      => RootZone_v40_GetDemandAgAreas
    PROCEDURE,PASS   :: GetDemandUrbanAreas                   => RootZone_v40_GetDemandUrbanAreas
    PROCEDURE,PASS   :: GetElementSoilMVolume                 => RootZone_v40_GetElementSoilMVolume
    PROCEDURE,PASS   :: GetPercAll                            => RootZone_v40_GetPercAll
    PROCEDURE,PASS   :: GetPercElement                        => RootZone_v40_GetPercElement
    PROCEDURE,PASS   :: GetFlowsToStreams                     => RootZone_v40_GetFlowsToStreams
    PROCEDURE,PASS   :: GetFlowsToLakes                       => RootZone_v40_GetFlowsToLakes
    PROCEDURE,PASS   :: GetRatio_DestSupplyToRegionSupply_Ag  => RootZone_v40_GetRatio_ElemSupplyToRegionSupply_Ag
    PROCEDURE,PASS   :: GetRatio_DestSupplyToRegionSupply_Urb => RootZone_v40_GetRatio_ElemSupplyToRegionSupply_Urb
    PROCEDURE,PASS   :: SetLakeElemFlag                       => RootZone_v40_SetLakeElemFlag
    PROCEDURE,PASS   :: SetSupply                             => RootZone_v40_SetSupplyToElem        
    PROCEDURE,PASS   :: ConvertTimeUnit                       => RootZone_v40_ConvertTimeUnit
    PROCEDURE,PASS   :: ReadTSData                            => RootZone_v40_ReadTSData
    PROCEDURE,PASS   :: ReadRestartData                       => RootZone_v40_ReadRestartData
    PROCEDURE,PASS   :: AdvanceState                          => RootZone_v40_AdvanceState
    PROCEDURE,PASS   :: ComputeWaterDemand                    => RootZone_v40_ComputeWaterDemand 
    PROCEDURE,PASS   :: ComputeFutureWaterDemand              => RootZone_v40_ComputeFutureWaterDemand 
    PROCEDURE,PASS   :: ZeroSupply                            => RootZone_v40_ZeroSupply
    PROCEDURE,PASS   :: ZeroSurfaceFlows                      => RootZone_v40_ZeroSurfaceFlows
    PROCEDURE,PASS   :: Simulate                              => RootZone_v40_Simulate
    PROCEDURE,PASS   :: RegionalPerc                          => RootZone_v40_RegionalPerc
    PROCEDURE,PASS   :: RegionalReturnFlow_Ag                 => RootZone_v40_RegionalReturnFlow_Ag
    PROCEDURE,PASS   :: RegionalReturnFlow_Urb                => RootZone_v40_RegionalReturnFlow_Urb
    PROCEDURE,PASS   :: PrintResults                          => RootZone_v40_PrintResults
    PROCEDURE,PASS   :: PrintRestartData                      => RootZone_v40_PrintRestartData
    PROCEDURE,PASS   :: GetVersion                            => RootZone_v40_GetVersion
  END TYPE RootZone_v40_Type 


  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion          = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion             = '4.0.0000'
  INCLUDE 'RootZone_v40_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: f_iNGroupLandUse           = 3 , &
                                         f_iAgIndex                 = 1 , &
                                         f_iUrbIndex                = 2 , &
                                         f_iNVIndex                 = 3 , &
                                         f_iLandUse_NonPonded       = 1 , &
                                         f_iLandUse_Ponded          = 2 , &
                                         f_iLandUse_Urban           = 3 , &
                                         f_iLandUse_NVRV            = 4 
  INTEGER,PARAMETER                   :: ModNameLen = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'RootZone_v40::'
  
  
  
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
  SUBROUTINE RootZone_v40_New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iStrmNodeIDs,iLakeIDs)
    CLASS(RootZone_v40_Type)           :: RootZone
    LOGICAL,INTENT(IN)                 :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    INTEGER,INTENT(IN)                 :: NTIME
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    INTEGER,OPTIONAL,INTENT(IN)        :: iStrmNodeIDs(:),iLakeIDs(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16)                :: ThisProcedure = ModName // 'RootZone_v40_New'
    CHARACTER(LEN=1000)                         :: ALine,NonPondedCropFile,RiceRefugeFile,UrbanDataFile,NVRVFile,AgWaterDemandFile,GenericMoistureFile
    CHARACTER                                   :: cVersionLocal*20
    REAL(8)                                     :: FACTK,FACTCN,RegionArea(AppGrid%NSubregions+1),DummyFactor(1),rDummy(13)
    INTEGER                                     :: NElements,NRegion,ErrorCode,indxElem,iColGenericMoisture(AppGrid%NElements),SurfaceFlowDest(AppGrid%NElements), &
                                                   SurfaceFlowDestType(AppGrid%NElements),nDataCols,iElemIDs(AppGrid%NElements),iElemID,iElem,iFeatureIndex,       &
                                                   iSubregionIDs(AppGrid%NSubregions)
    TYPE(GenericFileType)                       :: RootZoneParamFile
    LOGICAL                                     :: TrackTime,lProcessed(AppGrid%NElements)
    CHARACTER(LEN=f_iMaxLocationNameLen)        :: RegionNames(AppGrid%NSubregions+1)
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
    iElemIDs               = AppGrid%AppElement%ID
    iSubregionIDs          = AppGrid%AppSubregion%ID
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
              RootZone%RSoilM_P(NRegion+1,f_iNGroupLandUse)          , &
              RootZone%RSoilM(NRegion+1,f_iNGroupLandUse)            , &
              RootZone%Flags%lLakeElems(NElements)                   , &
              STAT=ErrorCode                                         )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for root zone soils data!',f_iFatal,ThisProcedure)
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

    !Initialize related files
    !-------------------------
    
    !Non-ponded crops data file
    CALL RootZoneParamFile%ReadData(NonPondedCropFile,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    NonPondedCropFile = StripTextUntilCharacter(NonPondedCropFile,'/') 
    CALL CleanSpecialCharacters(NonPondedCropFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(NonPondedCropFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%NonPondedAgRootZone%New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,FactCN,AppGrid,iElemIDs,TimeStep,NTIME,cVersionLocal,iStat)
    IF (iStat .EQ. -1) RETURN
       
    !Rice/refuge data file
    CALL RootZoneParamFile%ReadData(RiceRefugeFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    RiceRefugeFile = StripTextUntilCharacter(RiceRefugeFile,'/') 
    CALL CleanSpecialCharacters(RiceRefugeFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(RiceRefugeFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%PondedAgRootZone%New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,FactCN,AppGrid,iElemIDs,TimeStep,NTIME,cVersionLocal,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Urban data file
    CALL RootZoneParamFile%ReadData(UrbanDataFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    UrbanDataFile = StripTextUntilCharacter(UrbanDataFile,'/') 
    CALL CleanSpecialCharacters(UrbanDataFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(UrbanDataFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%UrbanRootZone%New(cAbsPathFileName,cWorkingDirectory,FactCN,NElements,NRegion,iElemIDs,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Native/riparian veg. data file
    CALL RootZoneParamFile%ReadData(NVRVFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    NVRVFile = StripTextUntilCharacter(NVRVFile,'/') 
    CALL CleanSpecialCharacters(NVRVFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(NVRVFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%NVRVRootZone%New(cAbsPathFileName,cWorkingDirectory,FactCN,NElements,NRegion,iElemIDs,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Check if at least one type of land use is specified
    IF ( NonPondedCropFile .EQ. ''   .AND.   &
         RiceRefugeFile    .EQ. ''   .AND.   &
         UrbanDataFile     .EQ. ''   .AND.   &
         NVRVFile          .EQ. ''           )  THEN
        MessageArray(1) = 'At least one type of land use and related data should '
        MessageArray(2) = 'be specified for the simulation of root zone processes!' 
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
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
    RootZone%NLands = RootZone%NonPondedAgRootZone%NCrops + f_iNPondedCrops + 3
    
    !Return flow data file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
            CALL SetLastMessage('Missing return flow fractions data file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%ReturnFracFile%Init(cAbsPathFileName,cWorkingDirectory,'Return flow fractions data file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
        
    !Re-use data file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined  .OR.  RootZone%Flags%lUrban_Defined) THEN
            CALL SetLastMessage('Missing irrigation water re-use factors data file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%ReuseFracFile%Init(cAbsPathFileName,cWorkingDirectory,'Irrigation water re-use factors file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Irrigation period data file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        IF (RootZone%Flags%lNonpondedAg_Defined  .OR.  RootZone%Flags%lPondedAg_Defined) THEN
            CALL SetLastMessage('Missing irrigation period data file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%IrigPeriodFile%Init(cAbsPathFileName,cWorkingDirectory,'Irrigation period data file',TrackTime,1,iStat=iStat)
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
    
    !Agricultural water demand file
    CALL RootZoneParamFile%ReadData(AgWaterDemandFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    AgWaterDemandFile = StripTextUntilCharacter(AgWaterDemandFile,'/') 
    CALL CleanSpecialCharacters(AgWaterDemandFile)
    IF (AgWaterDemandFile .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(AgWaterDemandFile)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%AgWaterDemandFile%Init(cAbsPathFileName,cWorkingDirectory,'Agricultural water supply requirement file',TrackTime,1,.TRUE.,DummyFactor,(/.TRUE./),iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
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
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)
    RootZone%VarTimeUnit = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Backward compatibility: Check if the user entered KPonded values at all
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)  ;  ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/')) 
    READ (ALine,*,IOSTAT=ErrorCode) rDummy
    IF (ErrorCode .EQ. 0) THEN
        ALLOCATE (DummyRealArray(NElements,13))
        nDataCols = 13
    ELSE
        ALLOCATE (DummyRealArray(NElements,12))
        nDataCols = 12
    END IF
    CALL RootZoneParamFile%BackspaceFile()
    
    CALL RootZoneParamFile%ReadData(DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ASSOCIATE (pSoilsData  => RootZone%ElemSoilsData   , &
               pPrecipData => RootZone%ElemPrecipData  )
        lProcessed = .FALSE.
        DO indxElem=1,NElements
            iElemID = INT(DummyRealArray(indxElem,1))
            
            !Check if element is in the model
            CALL ConvertID_To_Index(iElemID,iElemIDs,iElem)
            IF (iElem .EQ. 0) THEN
                CALL SetLastMessage('Element '//TRIM(IntToText(iElemID))//' listed for root zone parameter definitions is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Check if it was defined before
            IF (lProcessed(iElem)) THEN
                CALL SetLastMessage('Element '//TRIM(IntToText(iElemID))//' is listed more than once for root zone parameter definitions!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Process data
            lProcessed(iElem)               = .TRUE.
            pSoilsData(iElem)%WiltingPoint  =     DummyRealArray(indxElem,2)      
            pSoilsData(iElem)%FieldCapacity =     DummyRealArray(indxElem,3)
            pSoilsData(iElem)%TotalPorosity =     DummyRealArray(indxElem,4)
            pSoilsData(iElem)%Lambda        =     DummyRealArray(indxElem,5)
            pSoilsData(iElem)%HydCond       =     DummyRealArray(indxElem,6) * FACTK * TimeStep%DeltaT
            pSoilsData(iElem)%KunsatMethod  = INT(DummyRealArray(indxElem,7))
            pPrecipData(iElem)%iColPrecip   = INT(DummyRealArray(indxElem,8))
            pPrecipData(iElem)%PrecipFactor =     DummyRealArray(indxElem,9)
            iColGenericMoisture(iElem)      = INT(DummyRealArray(indxElem,10))
            SurfaceFlowDestType(iElem)      = INT(DummyRealArray(indxElem,11))
            SurfaceFlowDest(iElem)          = INT(DummyRealArray(indxElem,12))
            IF (nDataCols .EQ. 12) THEN
                RootZone%HydCondPonded(iElem) = pSoilsData(iElem)%HydCond
            ELSE
                IF (DummyRealArray(indxElem,13) .EQ. -1.0) THEN
                    RootZone%HydCondPonded(iElem) = pSoilsData(iElem)%HydCond
                ELSE
                    RootZone%HydCondPonded(iElem) = DummyRealArray(indxElem,13) * FACTK * TimeStep%DeltaT
                END IF
            END IF
            
            !Check for destination errors
            ASSOCIATE (pDestType => SurfaceFlowDestType(iElem))
                !Make sure that destination types are recognized
                IF (pDestType .NE. f_iFlowDest_Outside    .AND.   &
                    pDestType .NE. f_iFlowDest_StrmNode   .AND.   &
                    pDestType .NE. f_iFlowDest_Lake       .AND.   &
                    pDestType .NE. f_iFlowDest_GWElement       )  THEN
                    CALL SetLastMessage ('Surface flow destination type for element ' // TRIM(IntToText(iElemID)) // ' is not recognized!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
              
                !Make sure destination locations are modeled
                SELECT CASE (pDestType)
                    CASE (f_iFlowDest_StrmNode)
                        IF (PRESENT(iStrmNodeIDs)) THEN
                            CALL ConvertID_To_Index(SurfaceFlowDest(iElem),iStrmNodeIDs,iFeatureIndex)
                            IF (iFeatureIndex .EQ. 0) THEN
                                CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(iElemID))//' flows into a stream node ('//TRIM(IntToText(SurfaceFlowDest(iElem)))//') that is not in the model!',f_iFatal,ThisProcedure)
                                iStat = -1
                                RETURN
                            END IF
                            SurfaceFlowDest(iElem) = iFeatureIndex
                        END IF
                    
                    CASE (f_iFlowDest_Element)
                        CALL ConvertID_To_Index(SurfaceFlowDest(iElem),iElemIDs,iFeatureIndex)
                        IF (iFeatureIndex .EQ. 0) THEN
                            CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(iElemID))//' goes to an element ('//TRIM(IntToText(SurfaceFlowDest(iElem)))//') that is not in the model!',f_iFatal,ThisProcedure)
                            iStat = -1
                            RETURN
                        END IF
                        SurfaceFlowDest(iElem) = iFeatureIndex
                    
                    CASE (f_iFlowDest_Lake)
                        IF (PRESENT(iLakeIDs)) THEN
                            CALL ConvertID_To_Index(SurfaceFlowDest(iElem),iLakeIDs,iFeatureIndex)
                            IF (iFeatureIndex .EQ. 0) THEN
                                CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(iElemID))//' flows into a lake ('//TRIM(IntToText(SurfaceFlowDest(iElem)))//') that is not in the model!',f_iFatal,ThisProcedure)
                                iStat = -1
                                RETURN
                            END IF
                            SurfaceFlowDest(iElem) = iFeatureIndex
                        END IF
                        
                    CASE (f_iFlowDest_Subregion)
                        CALL ConvertID_To_Index(SurfaceFlowDest(iElem),iSubregionIDs,iFeatureIndex)
                        IF (iFeatureIndex .EQ. 0) THEN
                            CALL SetLastMessage('Surface flow from element '//TRIM(IntToText(iElemID))//' goes to a subregion ('//TRIM(IntToText(SurfaceFlowDest(iElem)))//') that is not in the model!',f_iFatal,ThisProcedure)
                            iStat = -1
                            RETURN
                        END IF
                        SurfaceFlowDest(iElem) = iFeatureIndex
                    
                    CASE (f_iFlowDest_GWElement)
                        SurfaceFlowDest(iElem) = iElem
                        
                    CASE (f_iFlowDest_Outside)
                        SurfaceFlowDest(iElem) = 0                        
                END SELECT
            END ASSOCIATE
            
            !Method to compute Kunsat must be recognized
            IF (LocateInList(pSoilsData(iElem)%KunsatMethod,f_iKunsatMethodList) .LT. 1) THEN
                CALL SetLastMessage('Method to compute unsaturated hydraulic conductivity at element '//TRIM(IntToText(iElemID))//' is not recognized!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Wilting point should be less than field capacity
            IF (pSoilsData(iElem)%WiltingPoint .GE. pSoilsData(iElem)%FieldCapacity) THEN
                CALL SetLastMessage('At element ' // TRIM(IntToText(iElemID)) // ' wilting point is greater than or equal to field capacity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Field capacity should be less than or equal to total porosity
            IF (pSoilsData(iElem)%FieldCapacity .GT. pSoilsData(iElem)%TotalPorosity) THEN
                CALL SetLastMessage('At element ' // TRIM(IntToText(iElemID)) // ' field capacity is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Instantiate generic moisture data
        CALL RootZone%GenericMoistureData%New(GenericMoistureFile,cWorkingDirectory,1,NElements,iColGenericMoisture,TrackTime,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Compile element-flow-to-outside connection list
        CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_Outside,SurfaceFlowDest,SurfaceFlowDestType,ElemFlowToOutside,iStat)  ;  IF (iStat .EQ. -1) RETURN
        ALLOCATE (RootZone%ElemFlowToOutside(SIZE(ElemFlowToOutside)))
        RootZone%ElemFlowToOutside = ElemFlowToOutside%iElement
        
        !Compile element-flow-to-stream-node connection list
        CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_StrmNode,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToStreams,iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Compile element-flow-to-lake connection list
        CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_Lake,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToLakes,iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Compile element-flow-to-subregion connection list
        CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_Subregion,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToSubregions,iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Compile element-flow-to-groundwater connection list
        CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_GWElement,SurfaceFlowDest,SurfaceFlowDestType,ElemFlowToGW,iStat)  ;  IF (iStat .EQ. -1) RETURN
        ALLOCATE (RootZone%ElemFlowToGW(SIZE(ElemFlowToGW)))
        RootZone%ElemFlowToGW = ElemFlowToGW%iElement
        
        !Compile element-flow-to-another-element connection list
        CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_Element,SurfaceFlowDest,SurfaceFlowDestType,RootZone%ElemFlowToElements,iStat)  
        IF (iStat .EQ. -1) RETURN
      
    END ASSOCIATE
    
    !Flag to see if ag water demand will be read or not, check for inconsistencies as well
    IF (RootZone%NonPondedAgRootZone%NCrops.GT.0  .OR.  RiceRefugeFile.NE.'') THEN
      !Check with non-ponded crops
      IF (ALLOCATED(RootZone%NonPondedAgRootZone%iColAgDemand)) THEN
          IF (ANY(RootZone%NonPondedAgRootZone%iColAgDemand.GT.0)) RootZone%Flags%lReadNonPondedAgWaterDemand = .TRUE.
      END IF
      
      !Then, check with ponded crops
      IF (ALLOCATED(RootZone%PondedAgRootZone%iColAgDemand)) THEN
          IF (ANY(RootZone%PondedAgRootZone%iColAgDemand.GT.0)) RootZone%Flags%lReadPondedAgWaterDemand = .TRUE.
      END IF
      
      !Are pointers defined without a defined ag water demand file?
      IF (AgWaterDemandFile .EQ. '' ) THEN
          IF (RootZone%Flags%lReadNonPondedAgWaterDemand  .OR. RootZone%Flags%lReadPondedAgWaterDemand) THEN 
              CALL SetLastMessage('Data columns from agricultural water supply requirement file is referenced but this file is not specified!',f_iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
      END IF
      
    END IF
    
    !Check if return flow, re-use and irrigation column pointers are referring to existing data columns
    CALL CheckTSDataPointers(RootZone,iElemIDs,Precip,ET,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL RootZoneParamFile%Kill()
    
    !Clear memory
    DEALLOCATE (ElemFlowToGW , ElemFlowToOutside , cAbsPathFileName , DummyRealArray , STAT=ErrorCode)

  END SUBROUTINE RootZone_v40_New
  
  


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
  SUBROUTINE RootZone_v40_Kill(RootZone)
    CLASS(RootZone_v40_Type) :: RootZone
    
    !Local variables
    INTEGER              :: ErrorCode
    TYPE(SolverDataType) :: DummySolverData
    TYPE(FlagsType)      :: DummyFlags
    
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
    CALL RootZone%NonPondedAgRootZone%Kill()
    CALL RootZone%PondedAgRootZone%Kill()
    CALL RootZone%UrbanRootZone%Kill()
    CALL RootZone%NVRVRootZone%Kill()
    CALL RootZone%GenericMoistureData%Kill()
    
    !Close files
    CALL RootZone%IrigPeriodFile%Close()
    CALL RootZone%AgWaterDemandFile%Close()
    
    !Default other components
    RootZone%NLands              = 0
    RootZone%Flags               = DummyFlags
    RootZone%AgWaterDemandFactor = 1.0
    RootZone%SolverData          = DummySolverData
    
  END SUBROUTINE RootZone_v40_Kill
  
  


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
  FUNCTION RootZone_v40_IsLandUseUpdated(RootZone) RESULT(lUpdated)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    LOGICAL                             :: lUpdated
    
    lUpdated = RootZone%NonPondedAgRootZone%LandUseDataFile%lUpdated       .OR.  &
               RootZone%PondedAgRootZone%LandUseDataFile%lUpdated          .OR.  &
               RootZone%UrbanRootZone%LandUseDataFile%lUpdated             .OR.  &
               RootZone%NVRVRootZone%LandUseDataFile%lUpdated
    
  END FUNCTION RootZone_v40_IsLandUseUpdated
  
  


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
  ! --- GET BUDGET LIST
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetBudget_List(RootZone,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(RootZone_v40_Type),INTENT(IN)      :: RootZone
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    !Local variables
    INTEGER                  :: iCount,iErrorCode,iTypeList(4),iLocationList(4)
    CHARACTER(LEN=500)       :: cFiles(4)
    CHARACTER(LEN=100)       :: cDescription(4)
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    iCount = 0
    DEALLOCATE (iBudgetTypeList , iBudgetLocationTypeList , cBudgetDescriptions , cBudgetFiles , STAT=iErrorCode)
         
    !Non-ponded ag land and water use budget (ignore for now)
    !IF (RootZone%NonPondedAgRootZone%lLWUseBudRawFile_Defined) THEN
    !    CALL RootZone%NonPondedAgRootZone%LWUseBudRawFile%GetFileName(cFileName)
    !    cFiles(iCount+1)        = cFileName
    !    iTypeList(iCount+1)     = f_iBudgetType_NonPondedCrop_LWU
    !    iLocationList(iCount+1) = f_iLocationType_Subregion
    !    cDescription(iCount+1)  = f_cDescription_NPCrop_LWUBudget
    !    iCount                  = iCount + 1 
    !END IF
    
    !Non-ponded ag root zone budget (ignore for now)
    !IF (RootZone%NonPondedAgRootZone%lRootZoneBudRawFile_Defined) THEN
    !    CALL RootZone%NonPondedAgRootZone%RootZoneBudRawFile%GetFileName(cFileName)
    !    cFiles(iCount+1)        = cFileName
    !    iTypeList(iCount+1)     = f_iBudgetType_NonPondedCrop_RZ
    !    iLocationList(iCount+1) = f_iLocationType_Subregion
    !    cDescription(iCount+1)  = f_cDescription_NPCrop_RootZoneBudget
    !    iCount                  = iCount + 1 
    !END IF
    
    !Ponded ag land and water use budget (ignore for now)
    !IF (RootZone%PondedAgRootZone%lLWUseBudRawFile_Defined) THEN
    !    CALL RootZone%PondedAgRootZone%LWUseBudRawFile%GetFileName(cFileName)
    !    cFiles(iCount+1)        = cFileName
    !    iTypeList(iCount+1)     = f_iBudgetType_PondedCrop_LWU
    !    iLocationList(iCount+1) = f_iLocationType_Subregion
    !    cDescription(iCount+1)  = f_cDescription_PCrop_LWUBudget
    !    iCount                  = iCount + 1 
    !END IF
    
    !Ponded ag root zone budget (ignore for now)
    !IF (RootZone%PondedAgRootZone%lRootZoneBudRawFile_Defined) THEN
    !    CALL RootZone%PondedAgRootZone%RootZoneBudRawFile%GetFileName(cFileName)
    !    cFiles(iCount+1)        = cFileName
    !    iTypeList(iCount+1)     = f_iBudgetType_PondedCrop_RZ
    !    iLocationList(iCount+1) = f_iLocationType_Subregion
    !    cDescription(iCount+1)  = f_cDescription_PCrop_RootZoneBudget
    !    iCount                  = iCount + 1 
    !END IF
    
    !Copy info to return arguments
    ALLOCATE (iBudgetTypeList(iCount) , iBudgetLocationTypeList(iCount) , cBudgetDescriptions(iCount) , cBudgetFiles(iCount))
    iBudgetTypeList         = iTypeList(1:iCount)
    iBudgetLocationTypeList = iLocationList(1:iCount)
    cBudgetDescriptions     = cDescription(1:iCount)
    cBudgetFiles            = cFiles(1:iCount)

  END SUBROUTINE RootZone_v40_GetBudget_List
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BUDGET FILE COLUMNS (EXCLUDING Time COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetBudget_NColumns(RootZone,iBudgetType,iLocationIndex,iNCols,iStat)
    CLASS(RootZone_v40_Type),TARGET,INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                         :: iBudgetType,iLocationIndex
    INTEGER,INTENT(OUT)                        :: iNCols,iStat
    
    !LOcal variables
    TYPE(BudgetType),POINTER :: pBudget
    
    !Initialize
    iNCols  =  0
    iStat   =  0
    pBudget => NULL()
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_LWU)
            IF (RootZone%Flags%LWUseBudRawFile_Defined) pBudget => RootZone%LWUseBudRawFile
            
        CASE (f_iBudgetType_RootZone)
            IF (RootZone%Flags%RootZoneBudRawFile_Defined) pBudget => RootZone%RootZoneBudRawFile
            
        CASE (f_iBudgetType_NonPondedCrop_LWU)  
            IF (RootZone%NonPondedAgRootZone%lLWUseBudRawFile_Defined) pBudget => RootZone%NonPondedAgRootZone%LWUseBudRawFile
            
        CASE (f_iBudgetType_PondedCrop_LWU)
            IF (RootZone%PondedAgRootZone%lLWUseBudRawFile_Defined) pBudget => RootZone%PondedAgRootZone%LWUseBudRawFile

        CASE (f_iBudgetType_NonPondedCrop_RZ) 
            IF (RootZone%NonPondedAgRootZone%lRootZoneBudRawFile_Defined) pBudget => RootZone%NonPondedAgRootZone%RootZoneBudRawFile
            
        CASE (f_iBudgetType_PondedCrop_RZ)
            IF (RootZone%PondedAgRootZone%lRootZoneBudRawFile_Defined) pBudget => RootZone%PondedAgRootZone%RootZoneBudRawFile
    END SELECT
        
    !Get the number of columns (includes Time column)
    IF (ASSOCIATED(pBudget)) THEN
        CALL pBudget%GetNDataColumns(iLocationIndex,iNCols,iStat) 
        !Exclude Time column
        iNCols = iNCols - 1
    END IF
    
    !Clear memory
    NULLIFY (pBudget)
    
  END SUBROUTINE RootZone_v40_GetBudget_NColumns
     
     
  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN TITLES (EXCLUDING Time COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetBudget_ColumnTitles(RootZone,iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(RootZone_v40_Type),TARGET,INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                         :: iBudgetType,iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)                :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)   :: cColTitles(:)
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local variables
    INTEGER                                       :: iNCols,iErrorCode
    TYPE(BudgetType),POINTER                      :: pBudget
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    !Initailize
    iStat   =  0
    pBudget => NULL()
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_LWU)
            IF (RootZone%Flags%LWUseBudRawFile_Defined) pBudget => RootZone%LWUseBudRawFile 
            
        CASE (f_iBudgetType_RootZone)
            IF (RootZone%Flags%RootZoneBudRawFile_Defined) pBudget => RootZone%RootZoneBudRawFile
            
        CASE (f_iBudgetType_NonPondedCrop_LWU)  
            IF (RootZone%NonPondedAgRootZone%lLWUseBudRawFile_Defined) pBudget => RootZone%NonPondedAgRootZone%LWUseBudRawFile
            
        CASE (f_iBudgetType_PondedCrop_LWU)
            IF (RootZone%PondedAgRootZone%lLWUseBudRawFile_Defined) pBudget => RootZone%PondedAgRootZone%LWUseBudRawFile

        CASE (f_iBudgetType_NonPondedCrop_RZ) 
            IF (RootZone%NonPondedAgRootZone%lRootZoneBudRawFile_Defined) pBudget => RootZone%NonPondedAgRootZone%RootZoneBudRawFile
            
        CASE (f_iBudgetType_PondedCrop_RZ)
            IF (RootZone%PondedAgRootZone%lRootZoneBudRawFile_Defined) pBudget => RootZone%PondedAgRootZone%RootZoneBudRawFile
    
    END SELECT
        
    !Return if no Budget file
    IF (.NOT. ASSOCIATED(pBudget)) THEN
        ALLOCATE (cColTitles(0))
        RETURN
    END IF
    
    !Number of columns (includes Time column)
    CALL pBudget%GetNDataColumns(iLocationIndex,iNCols,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get column titles (includes Time column)
    ALLOCATE (cColTitles_Local(iNCols))
    cColTitles_Local = pBudget%GetFullColumnHeaders(iLocationIndex,iNCols)
    
    !Insert units
    CALL pBudget%ModifyFullColumnHeaders(cUnitLT,cUnitAR,cUnitVL,cColTitles_Local)
    
    !Remove Time column
    iNCols = iNCols - 1
    ALLOCATE (cColTitles(iNCols))
    cColTitles = ADJUSTL(cColTitles_Local(2:))
    
    !Clear memory
    DEALLOCATE (cColTitles_Local , STAT=iErrorCode)
    NULLIFY(pBudget)
               
  END SUBROUTINE RootZone_v40_GetBudget_ColumnTitles
     
     
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM RootZOne OBJECT
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetBudget_MonthlyFlows_GivenRootZone(RootZone,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(RootZone_v40_Type),TARGET,INTENT(IN) :: RootZone
    CHARACTER(LEN=*),INTENT(IN)                :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                         :: iBudgetType,iLUType,iSubregionID  
    REAL(8),INTENT(IN)                         :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)            :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)   :: cFlowNames(:)
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local variables
    TYPE(BudgetType),POINTER :: pBudget
    
    !Initailize
    iStat   =  0
    pBudget => NULL()
    
    !Select the Budget file
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_LWU)
            IF (RootZone%Flags%LWUseBudRawFile_Defined) pBudget => RootZone%LWUseBudRawFile
        CASE (f_iBudgetType_RootZone)
            IF (RootZone%Flags%RootZoneBudRawFile_Defined) pBudget => RootZone%RootZoneBudRawFile
    END SELECT
        
    !Return if there is no budget file
    IF (.NOT.ASSOCIATED(pBudget)) THEN
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
        RETURN
    END IF
  
    !Get the values
    CALL RootZone_v40_GetBudget_MonthlyFlows_GivenFile(pBudget,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat) 
    
  END SUBROUTINE RootZone_v40_GetBudget_MonthlyFlows_GivenRootZone

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLUType,iSubregionID  
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+45) :: ThisProcedure = ModName // 'RootZone_v40_GetBudget_MonthlyFlows_GivenFile'
    INTEGER,TARGET               :: iDimActual,iNTimeSteps,                         &
                                    iReadCols_LWU_Ag(5) = [3,4,5,6,7],              &
                                    iReadCols_LWU_Urb(5) = [12,13,14,15,16],        &
                                    iReadCols_RZ_Ag(8) = [9,10,11,12,13,14,15,16],  &
                                    iReadCols_RZ_Urb(7) = [26,27,28,29,30,31,32],   &
                                    iReadCols_RZ_NVRV(7) = [39,40,41,42,43,44,45]
    REAL(8),ALLOCATABLE          :: rValues(:,:)
    INTEGER,POINTER              :: piReadCols(:)
    
    !Number of time steps
    iNTimeSteps = Budget%GetNTimeSteps()

    !Land&Water Use Budget
    IF (iBudgetType .EQ. f_iBudgetType_LWU) THEN
        !Allocate arrays
        ALLOCATE (rValues(6,iNTimeSteps) , cFlowNames(5))  !Adding 1 to the first dimension for Time column; it will be removed later
        
        !Flow names
        cFlowNames     = ''
        cFlowNames(1)  = 'Supply Requirement'      
        cFlowNames(2)  = 'Pumping'                 
        cFlowNames(3)  = 'Deliveries'              
        cFlowNames(4)  = 'Inflow as Surface Runoff'
        cFlowNames(5)  = 'Shortage'  
        
        !Columns to read based on land use type
        SELECT CASE (iLUType)
            CASE (f_iAg)
                piReadCols => iReadCols_LWU_Ag
            CASE (f_iUrb)
                piReadCols => iReadCols_LWU_Urb
            CASE (f_iNonPondedAg) 
                CALL SetLastMessage('Non-ponded-crop-specific Land & Water Use Budget cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iRice)
                CALL SetLastMessage('Land & Water Use Budget for rice cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iRefuge)
                CALL SetLastMessage('Land & Water Use Budget for refuges cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iNVRV)
                CALL SetLastMessage('Land & Water Use Budget does not exist for native and riparian vegetation!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
        END SELECT
            
        !Read data
        CALL Budget%ReadData(iSubregionID,piReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Store values in return argument
        ALLOCATE (rFlows(5,iDimActual))
        rFlows(1,:)  = -rValues(2,1:iDimActual)       !Supply Requirement              
        rFlows(2,:)  = rValues(3,1:iDimActual)        !Pumping                          
        rFlows(3,:)  = rValues(4,1:iDimActual)        !Deliveries                       
        rFlows(4,:)  = rValues(5,1:iDimActual)        !Inflow as Surface Runoff         
        rFlows(5,:)  = rValues(6,1:iDimActual)        !Shortage                         

    !Root Zone Budget 
    ELSEIF (iBudgetType .EQ. f_iBudgetType_RootZone) THEN
        !Columns to read based on land use type
        SELECT CASE (iLUType)
            CASE (f_iAg)
                piReadCols => iReadCols_RZ_Ag
                !Allocate arrays
                ALLOCATE (rValues(9,iNTimeSteps) , cFlowNames(7))  !Adding 1 to the first dimension for Time column; it will be removed later
                !Flow names
                cFlowNames     = ''
                cFlowNames(1)  = 'Change in Storage'              
                cFlowNames(2)  = 'Net Gain from Land Expansion'   
                cFlowNames(3)  = 'Infiltration'                   
                cFlowNames(4)  = 'Other Inflow'                   
                cFlowNames(5)  = 'Pond Drain'                     
                cFlowNames(6)  = 'Actual ET'                      
                cFlowNames(7)  = 'Percolation'                    
                !Read data
                CALL Budget%ReadData(iSubregionID,piReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                !Store values in return argument
                ALLOCATE (rFlows(7,iDimActual))
                rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(9,1:iDimActual)       !Change in Storage              
                rFlows(2,:)  = rValues(3,1:iDimActual)                                 !Net Gain from Land Expansion          
                rFlows(3,:)  = rValues(4,1:iDimActual)                                 !Infiltration                          
                rFlows(4,:)  = rValues(5,1:iDimActual)                                 !Other Inflow                          
                rFlows(5,:)  = -rValues(6,1:iDimActual)                                !Pond Drain                    
                rFlows(6,:)  = -rValues(7,1:iDimActual)                                !Actual ET                     
                rFlows(7,:)  = -rValues(8,1:iDimActual)                                !Percolation                   

            CASE (f_iUrb)      
                piReadCols => iReadCols_RZ_Urb
                !Allocate arrays
                ALLOCATE (rValues(8,iNTimeSteps) , cFlowNames(6))  !Adding 1 to the first dimension for Time column; it will be removed later
                !Flow names
                cFlowNames     = ''
                cFlowNames(1)  = 'Change in Storage'              
                cFlowNames(2)  = 'Net Gain from Land Expansion'   
                cFlowNames(3)  = 'Infiltration'                   
                cFlowNames(4)  = 'Other Inflow'                   
                cFlowNames(5)  = 'Actual ET'                      
                cFlowNames(6)  = 'Percolation'                    
                !Read data
                CALL Budget%ReadData(iSubregionID,piReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                !Store values in return argument
                ALLOCATE (rFlows(6,iDimActual))
                rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(8,1:iDimActual)       !Change in Storage              
                rFlows(2,:)  = rValues(3,1:iDimActual)                                 !Net Gain from Land Expansion          
                rFlows(3,:)  = rValues(4,1:iDimActual)                                 !Infiltration                          
                rFlows(4,:)  = rValues(5,1:iDimActual)                                 !Other Inflow                          
                rFlows(5,:)  = -rValues(6,1:iDimActual)                                !Actual ET                     
                rFlows(6,:)  = -rValues(7,1:iDimActual)                                !Percolation                   

            CASE (f_iNVRV)
                piReadCols => iReadCols_RZ_NVRV
                !Allocate arrays
                ALLOCATE (rValues(8,iNTimeSteps) , cFlowNames(6))  !Adding 1 to the first dimension for Time column; it will be removed later
                !Flow names
                cFlowNames     = ''
                cFlowNames(1)  = 'Change in Storage'              
                cFlowNames(2)  = 'Net Gain from Land Expansion'   
                cFlowNames(3)  = 'Infiltration'                   
                cFlowNames(4)  = 'Other Inflow'                   
                cFlowNames(5)  = 'Actual ET'                      
                cFlowNames(6)  = 'Percolation'                    
                !Read data
                CALL Budget%ReadData(iSubregionID,piReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                !Store values in return argument
                ALLOCATE (rFlows(6,iDimActual))
                rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(8,1:iDimActual)       !Change in Storage              
                rFlows(2,:)  = rValues(3,1:iDimActual)                                 !Net Gain from Land Expansion          
                rFlows(3,:)  = rValues(4,1:iDimActual)                                 !Infiltration                          
                rFlows(4,:)  = rValues(5,1:iDimActual)                                 !Other Inflow                          
                rFlows(5,:)  = -rValues(6,1:iDimActual)                                !Actual ET                     
                rFlows(6,:)  = -rValues(7,1:iDimActual)                                !Percolation                   

            CASE (f_iNonPondedAg) 
                CALL SetLastMessage('Non-ponded-crop-specific Root Zone Budget cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iRice)
                CALL SetLastMessage('Root Zone Budget for rice cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iRefuge)
                CALL SetLastMessage('Root Zone Budget for refuges cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
        END SELECT

    END IF
        
  END SUBROUTINE RootZone_v40_GetBudget_MonthlyFlows_GivenFile

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetBudget_TSData(RootZone,iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: iBudgetType,iSubregionID,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)         :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                  :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                 :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                 :: iDataTypes(:),inActualOutput,iStat
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_NonPondedCrop_LWU , f_iBudgetType_NonPondedCrop_RZ)
            CALL RootZone%NonPondedAgRootZone%GetBudget_TSData(iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
            
        CASE (f_iBudgetType_PondedCrop_LWU , f_iBudgetType_PondedCrop_RZ)
            CALL RootZone%PondedAgRootZone%GetBudget_TSData(iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
            
    END SELECT
    
  END SUBROUTINE RootZone_v40_GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET MAX AND MIN NET RETURN FLOW FRACTIONS FOR THE ENTIRE SIMULATION PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetMaxAndMinNetReturnFlowFrac(RootZone,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    CLASS(RootZone_v40_Type)      :: RootZone
    TYPE(TimeStepType),INTENT(IN) :: FirstTimeStep
    REAL(8),INTENT(OUT)           :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    REAL(8)                  :: rDummyFactor(1),rMaxFracUrb,rMinFracUrb
    CHARACTER(:),ALLOCATABLE :: cReturnFracFileName,cWorkDirReturn,cReuseFracFileName,cWorkDirReuse
    
    !Initialize
    rMaxFrac = 1.0
    rMinFrac = 0.0
    
    !Filenames and working directories
    CALL RootZone%ReturnFracFile%GetFileName(cReturnFracFileName)
    CALL GetFileDirectory(cReturnFracFileName,cWorkDirReturn)
    CALL RootZone%ReuseFracFile%GetFileName(cReuseFracFileName)
    CALL GetFileDirectory(cReuseFracFileName,cWorkDirReuse)
    
    !Get the min and max return flow fractions from non-ponded ag component
    IF (RootZone%Flags%lNonPondedAg_Defined) &
        CALL RootZone%NonPondedAgRootZone%GetMaxAndMinNetReturnFlowFrac(RootZone%ReturnFracFile,RootZone%ReuseFracFile,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    
    !Close/open return and reuse fraction files to initialize them
    CALL RootZone%ReturnFracFile%Close()
    CALL RootZone%ReturnFracFile%Init(cReturnFracFileName,cWorkDirReturn,'Return flow fractions data file',FirstTimeStep%TrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
    CALL RootZone%ReuseFracFile%Close()
    CALL RootZone%ReuseFracFile%Init(cReuseFracFileName,cWorkDirReuse,'Irrigation water re-use factors file',FirstTimeStep%TrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
    
    !Get the min and max return flow fractions from urban component
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%UrbanRootZone%GetMaxAndMinNetReturnFlowFrac(RootZone%ReturnFracFile,RootZone%ReuseFracFile,FirstTimeStep,rMaxFracUrb,rMinFracUrb,iStat)
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
            rMaxFrac = MAX(rMaxFrac , rMaxFracUrb)
            rMinFrac = MIN(rMinFrac , rMinFracUrb)
        END IF
    END IF
    
    !Close/open return and reuse fraction files to initialize them
    CALL RootZone%ReturnFracFile%Close()
    CALL RootZone%ReturnFracFile%Init(cReturnFracFileName,cWorkDirReturn,'Return flow fractions data file',FirstTimeStep%TrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
    CALL RootZone%ReuseFracFile%Close()
    CALL RootZone%ReuseFracFile%Init(cReuseFracFileName,cWorkDirReuse,'Irrigation water re-use factors file',FirstTimeStep%TrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
    
  END SUBROUTINE RootZone_v40_GetMaxAndMinNetReturnFlowFrac
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS FOR A ZONE BUDGET
  ! -------------------------------------------------------------
  FUNCTION RootZone_v40_GetZBudget_NColumns(RootZone,iZBudgetType) RESULT(iNCols)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: iZBudgetType
    INTEGER                             :: iNCols
    
    iNCols = 0
    
  END FUNCTION RootZone_v40_GetZBudget_NColumns
     
     
  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES FOR A ZONE BUDGET
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetZBudget_ColumnTitles(RootZone,iZBudgetType,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(RootZone_v40_Type),INTENT(IN)      :: RootZone
    INTEGER,INTENT(IN)                       :: iZBudgetType
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    iStat = 0
    ALLOCATE(cColTitles(0))
        
  END SUBROUTINE RootZone_v40_GetZBudget_ColumnTitles
     
     
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER 
  ! -------------------------------------------------------------
  FUNCTION RootZone_v40_GetVersion(RootZone) RESULT(cVrs)
    CLASS(RootZone_v40_Type) :: RootZone
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    IF (.NOT. RootZone%Version%IsDefined())   &
        RootZone%Version = RootZone%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = RootZone%Version%GetVersion()
    
  END FUNCTION RootZone_v40_GetVersion
  

  ! -------------------------------------------------------------
  ! --- GET TOTAL AG OR URBAN WATER SUPPLIES TO ELEMENTS 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetWaterSupply(RootZone,AppGrid,iSupplyFor,rSupply)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    INTEGER,INTENT(IN)                  :: iSupplyFor
    REAL(8)                             :: rSupply(:)
    
    !Local variables
    REAL(8) :: NonPondedAgSupply(RootZone%NonPondedAgRootZone%NCrops,AppGrid%NElements), &
               PondedAgSupply(RootZone%PondedAgRootZone%NCrops,AppGrid%NElements),       &
               UrbSupply(1,AppGrid%NElements)
    
    !Ag water supply
    IF (iSupplyFor .EQ. f_iAg) THEN
        !Supply as pumping and diversions
        rSupply = RootZone%ElemSupply%Diversion_Ag + RootZone%ElemSupply%Pumping_Ag

        !Compute non-ponded ag supplies in terms of runoff from upstream elements
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
          CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,RootZone%ElemSupply%UpstrmRunoff,RootZone,f_iLandUse_NonPonded,NonPondedAgSupply)
          rSupply = rSupply + SUM(NonPondedAgSupply , DIM=1)
        END IF
        
        !Compute ponded ag supplies in terms of runoff from upstream elements
        IF (RootZone%Flags%lPondedAg_Defined) THEN
          CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,RootZone%ElemSupply%UpstrmRunoff,RootZone,f_iLandUse_Ponded,PondedAgSupply)
          rSupply = rSupply + SUM(PondedAgSupply , DIM=1)
        END IF
    
    
    !Urban water supply
    ELSE
        !Supply as pumping and diversions
        rSupply = RootZone%ElemSupply%Diversion_Urb + RootZone%ElemSupply%Pumping_Urb
        
        !Compute urban supplies in terms of runoff from upstream elements
        IF (RootZone%Flags%lUrban_Defined) THEN
          CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,RootZone%ElemSupply%UpstrmRunoff,RootZone,f_iLandUse_Urban,UrbSupply)
          rSupply = rSupply + UrbSupply(1,:)
        END IF
    END IF
    
  END SUBROUTINE RootZone_v40_GetWaterSupply
  
  
  ! -------------------------------------------------------------
  ! --- GET RATIO OF ELEMENT SUPPLIES TO REGIONAL SUPLLIES FOR AG 
  ! -------------------------------------------------------------
  PURE SUBROUTINE RootZone_v40_GetRatio_ElemSupplyToRegionSupply_Ag(RootZone,Ratio)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Ratio(:)
    
    Ratio = RootZone%Ratio_ElemSupplyToRegionSupply_Ag
    
  END SUBROUTINE RootZone_v40_GetRatio_ElemSupplyToRegionSupply_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET RATIO OF ELEMENT SUPPLIES TO REGIONAL SUPPLIES FOR URBAN 
  ! -------------------------------------------------------------
  PURE SUBROUTINE RootZone_v40_GetRatio_ElemSupplyToRegionSupply_Urb(RootZone,Ratio)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Ratio(:)
    
    Ratio = RootZone%Ratio_ElemSupplyToRegionSupply_Urb
    
  END SUBROUTINE RootZone_v40_GetRatio_ElemSupplyToRegionSupply_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DEMAND CALUCLATION LOCATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION RootZone_v40_GetNDemandLocations(RootZone) RESULT(NLocations)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER                             :: NLocations
    
    NLocations = SIZE(RootZone%ElemSupply)
    
  END FUNCTION RootZone_v40_GetNDemandLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET PRECIP INFILTRATION AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementPrecipInfilt(RootZone,ElemRegion,PrecipInfilt)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: ElemRegion(:)    !Not used in this version
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
        PrecipInfilt = PrecipInfilt + RootZone%UrbanRootZone%UrbData%PrecipInfilt * RootZone%UrbanRootZone%UrbData%Area * RootZone%UrbanRootZone%UrbData%PerviousFrac
    
    !From native and riparian veg
    IF (RootZone%Flags%lNVRV_Defined) &
        PrecipInfilt = PrecipInfilt + RootZone%NVRVRootZone%NativeVeg%PrecipInfilt   * RootZone%NVRVRootZone%NativeVeg%Area    &
                                    + RootZone%NVRVRootZone%RiparianVeg%PrecipInfilt * RootZone%NVRVRootZone%RiparianVeg%Area
    
  END SUBROUTINE RootZone_v40_GetElementPrecipInfilt
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL ET AT ALL ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementActualET(RootZone,ElemRegion,ET)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
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
    
  END SUBROUTINE RootZone_v40_GetElementActualET
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AG CROPS
  ! -------------------------------------------------------------
  PURE FUNCTION RootZone_v40_GetNAgCrops(RootZone) RESULT(NAgCrops)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER                             :: NAgCrops
    
    NAgCrops = RootZone%NonPondedAgRootZone%NCrops + RootZone%PondedAgRootZone%NCrops
    
  END FUNCTION RootZone_v40_GetNAgCrops
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementWaterDemand(RootZone,iDemandFor,rDemand)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: iDemandFor
    REAL(8)                             :: rDemand(:)
    
    !Inform user
    CALL EchoProgress('Retrieving elemental water demand')
    
    IF (iDemandFor .EQ. f_iAg) THEN
        rDemand = 0.0
        IF (RootZone%Flags%lNonPondedAg_Defined) rDemand = SUM(RootZone%NonPondedAgRootZone%Crops%Demand , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    rDemand = rDemand + SUM(RootZone%PondedAgRootZone%Crops%Demand , DIM=1)
    ELSE
        IF (RootZone%Flags%lUrban_Defined) THEN
            rDemand = RootZone%UrbanRootZone%UrbData%Demand
        ELSE
            rDemand = 0.0
        END IF
    END IF
    
  END SUBROUTINE RootZone_v40_GetElementWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- GET WATER DEMAND AT SPECIFIED LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetWaterDemandAtLocations(RootZone,AppGrid,iLocationTypeID,iLocationIDList,iDemandFor,rDemand,iStat)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    INTEGER,INTENT(IN)                  :: iLocationTypeID,iLocationIDList(:),iDemandFor
    REAL(8)                             :: rDemand(:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38),PARAMETER :: ThisProcedure = ModName // 'RootZone_v40_GetWaterDemandAtLocations'
    INTEGER                                :: indxElem,indxRegion,iElem,iRegion,iLocationList(SIZE(iLocationIDList))
    
    !Initialize
    iStat = 0
    
    
    !Ag demand
    IF (iDemandFor .EQ. f_iAg) THEN
        !Inform user
        CALL EchoProgress('Retrieving agricultural water demand at specified locations...')
        !Initialize
        rDemand = 0.0
        !Make sure location type is element or subregion, if so compile information
        SELECT CASE (iLocationTypeID)
            CASE (f_iFlowDest_Element)
                CALL ConvertID_To_Index(iLocationIDList,AppGrid%AppElement%ID,iLocationList) 
                DO indxElem=1,SIZE(iLocationList)
                    iElem = iLocationList(indxElem)
                    IF (RootZone%Flags%lNonPondedAg_Defined) rDemand(indxElem) = rDemand(indxElem)+ SUM(RootZone%NonPondedAgRootZone%Crops(:,iElem)%Demand , DIM=1)
                    IF (RootZone%Flags%lPondedAg_Defined)    rDemand(indxElem) = rDemand(indxElem) + SUM(RootZone%PondedAgRootZone%Crops(:,iElem)%Demand , DIM=1)
                END DO
            CASE (f_iFlowDest_Subregion)
                CALL ConvertID_To_Index(iLocationIDList,AppGrid%AppSubregion%ID,iLocationList)
                DO indxRegion=1,SIZE(iLocationList)
                    iRegion = iLocationList(indxRegion)
                    DO indxElem=1,AppGrid%AppSubregion(iRegion)%NRegionElements
                        iElem = AppGrid%AppSubregion(iRegion)%RegionElements(indxElem)
                        IF (RootZone%Flags%lNonPondedAg_Defined) rDemand(indxRegion) = rDemand(indxRegion) + SUM(RootZone%NonPondedAgRootZone%Crops(:,iElem)%Demand , DIM=1)
                        IF (RootZone%Flags%lPondedAg_Defined)    rDemand(indxRegion) = rDemand(indxRegion) + SUM(RootZone%PondedAgRootZone%Crops(:,iElem)%Demand , DIM=1)
                    END DO
                END DO
            CASE DEFAULT
                CALL SetLastMessage('Agricultural water demand cannot be retrieved at the specified location type.',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
        END SELECT
            
    !Urban demand
    ELSE
        !Inform user
        CALL EchoProgress('Retrieving urban water demand at specified locations...')
        !Return if no urban area is simulated
        IF (.NOT. RootZone%Flags%lUrban_Defined) THEN
            rDemand = 0.0
            RETURN
        END IF
        !Make sure location type is element or subregion, if so compile information
        SELECT CASE (iLocationTypeID)
            CASE (f_iFlowDest_Element)
                CALL ConvertID_To_Index(iLocationIDList,AppGrid%AppElement%ID,iLocationList) 
                rDemand = RootZone%UrbanRootZone%UrbData(iLocationList)%Demand
            CASE (f_iFlowDest_Subregion)
                rDemand = 0.0
                CALL ConvertID_To_Index(iLocationIDList,AppGrid%AppSubregion%ID,iLocationList)
                DO indxRegion=1,SIZE(iLocationList)
                    iRegion               = iLocationList(indxRegion)
                    rDemand(indxRegion) = SUM(RootZone%UrbanRootZone%UrbData(AppGrid%AppSubregion(iRegion)%RegionElements)%Demand)
                END DO
            CASE DEFAULT
                CALL SetLastMessage('Urban water demand cannot be retrieved at the specified location type.',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
        END SELECT
    END IF

  END SUBROUTINE RootZone_v40_GetWaterDemandAtLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL OF PONDED AND NON-PONDED AG AREAS AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementAgAreas(RootZone,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        Areas = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
    ELSE
        Areas = 0.0
    END IF
    
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) Areas = Areas + SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)

  END SUBROUTINE RootZone_v40_GetElementAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementUrbanAreas(RootZone,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    !Urban areas
    IF (RootZone%Flags%lUrban_Defined) THEN
        Areas = RootZone%UrbanRootZone%UrbData%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v40_GetElementUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementNativeVegAreas(RootZone,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas = RootZone%NVRVRootZone%NativeVeg%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v40_GetElementNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL RIPARIAN VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementRiparianVegAreas(RootZone,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)                 :: Areas(:)
        
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Areas = RootZone%NVRVRootZone%RiparianVeg%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v40_GetElementRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL OF PONDED AND NON-PONDED AG AREAS AT EACH SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetSubregionAgAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    Areas = RegionalLUARea(AppGrid,RootZone,f_iAgIndex)

  END SUBROUTINE RootZone_v40_GetSubregionAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetSubregionUrbanAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    Areas = RegionalLUARea(AppGrid,RootZone,f_iUrbIndex)

  END SUBROUTINE RootZone_v40_GetSubregionUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetSubregionNativeVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (.NOT. RootZone%Flags%lNVRV_Defined) THEN
        Areas = 0.0
        RETURN
    ELSE
        Areas(1:AppGrid%NSubregions) = AppGrid%AccumElemValuesToSubregions(RootZone%NVRVRootZone%NativeVeg%Area)
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    END IF
    
  END SUBROUTINE RootZone_v40_GetSubregionNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetSubregionRiparianVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8),INTENT(OUT)                 :: Areas(:)
    
    IF (.NOT. RootZone%Flags%lNVRV_Defined) THEN
        Areas = 0.0
        RETURN
    ELSE
        Areas(1:AppGrid%NSubregions) = AppGrid%AccumElemValuesToSubregions(RootZone%NVRVRootZone%RiparianVeg%Area)
        Areas(AppGrid%NSubregions+1) = SUM(Areas(1:AppGrid%NSubregions))
    END IF
    
  END SUBROUTINE RootZone_v40_GetSubregionRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL OF PONDED AND NON-PONDED AG AREAS AT DEMAND LOCATION (ELEMENT)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetDemandAgAreas(RootZone,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
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

  END SUBROUTINE RootZone_v40_GetDemandAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN AREAS DEMAND LOCATION (ELEMENT)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetDemandUrbanAreas(RootZone,Areas)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8),ALLOCATABLE                 :: Areas(:)
    
    !Initialize
    IF (.NOT. ALLOCATED(Areas)) ALLOCATE(Areas(SIZE(RootZone%ElemSoilsData)))
    
    !Urban areas
    IF (RootZone%Flags%lUrban_Defined) THEN
        Areas = RootZone%UrbanRootZone%UrbData%Area
    ELSE
        Areas = 0.0
    END IF

  END SUBROUTINE RootZone_v40_GetDemandUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! ---GET ELEMENTAL VOLUMETRIC SOIL MOISTURE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetElementSoilMVolume(RootZone,AppGrid,SoilM)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
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

  END SUBROUTINE RootZone_v40_GetElementSoilMVolume
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT ALL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION RootZone_v40_GetPercAll(RootZone,AppGrid) RESULT(Perc)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
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
    
  END FUNCTION RootZone_v40_GetPercAll
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT AN INDIVIDUAL ELEMENT
  ! -------------------------------------------------------------
  FUNCTION RootZone_v40_GetPercElement(RootZone,iElem,AppGrid) RESULT(Perc)
    CLASS(RootZone_v40_Type),INTENT(IN)   :: RootZone
    INTEGER,INTENT(IN)                    :: iElem
    TYPE(AppGridType),OPTIONAL,INTENT(IN) :: AppGrid   !Not used in this version of the root zone component
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
    
  END FUNCTION RootZone_v40_GetPercElement
  
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetFlowsToStreams(RootZone,AppGrid,DirectRunoff,ReturnFlow,RiparianET)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid                       !Not used in this version
    REAL(8),INTENT(OUT)                 :: DirectRunoff(:),ReturnFlow(:)
    REAL(8),INTENT(INOUT)               :: RiparianET(:)                 !Included only to be consistent with the BaseRootZone template! This process is not simulated in this version.
    
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
        iStrmNode = pFlowData(indx)%iDest
        
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
    
  END SUBROUTINE RootZone_v40_GetFlowsToStreams
    
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO LAKES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_GetFlowsToLakes(RootZone,AppGrid,DirectRunoff,ReturnFlow)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid                       !Not used in this version
    REAL(8),INTENT(OUT)                 :: DirectRunoff(:),ReturnFlow(:)
    
    !Local variables
    INTEGER :: iElem,iLake,indx
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
        iLake = pFlowData(indx)%iDest
        
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
    
  END SUBROUTINE RootZone_v40_GetFlowsToLakes

  
  
  
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
  ! --- SET SUPPLY TO ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_SetSupplyToElem(RootZone,rSupply,iSupplyType,iSupplyFor)
    CLASS(RootZone_v40_Type) :: RootZone
    REAL(8),INTENT(IN)       :: rSupply(:)
    INTEGER,INTENT(IN)       :: iSupplyType,iSupplyFor

    !Inform user
    CALL EchoProgress('Setting supply to elements ... ', lAdvance=.FALSE.)
    
    !Set supply
    SELECT CASE(iSupplyType)
        CASE (f_iSupply_Diversion)
            IF (iSupplyFor .EQ. f_iAg) THEN
                RootZone%ElemSupply%Diversion_Ag = rSupply
            ELSE
                RootZone%ElemSupply%Diversion_Urb = rSupply
            END IF

        CASE (f_iSupply_Pumping)
            IF (iSupplyFor .EQ. f_iAg) THEN
                RootZone%ElemSupply%Pumping_Ag = rSupply
            ELSE
                RootZone%ElemSupply%Pumping_Urb = rSupply
            END IF
            
        CASE (f_iSupply_UpstrmElemRunoff)
            RootZone%ElemSupply%UpstrmRunoff = rSupply

    END SELECT

    CALL EchoProgress('DONE')

  END SUBROUTINE RootZone_v40_SetSupplyToElem
  
  
  ! -------------------------------------------------------------
  ! --- SET THE LAKE ELEMENT FLAG
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_SetLakeElemFlag(RootZone,iLakeElem)
    CLASS(RootZone_v40_Type) :: RootZone
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
    
  END SUBROUTINE RootZone_v40_SetLakeElemFlag

  
  
  
  
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
  SUBROUTINE RootZone_v40_ReadRestartData(RootZone,InFile,iStat)
    CLASS(RootZone_v40_Type) :: RootZone
    TYPE(GenericFileType)    :: InFile
    INTEGER,INTENT(OUT)      :: iStat
    
    CALL InFile%ReadData(RootZone%RSoilM_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(RootZone%RSoilM,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
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
    
  END SUBROUTINE RootZone_v40_ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ ROOT ZONE RELATED TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_ReadTSData(RootZone,AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas)
    CLASS(RootZone_v40_Type),TARGET    :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ETData
    INTEGER,INTENT(OUT)                :: iStat
    REAL(8),OPTIONAL,INTENT(IN)        :: RegionLUAreas(:,:)   !In (region, land use) format
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23)     :: ThisProcedure = ModName // 'RootZone_v40_ReadTSData'
    INTEGER                          :: indxElem,indxLU,NElements,NLandUse,indxForNV,iElemIDs(AppGrid%NElements)
    REAL(8)                          :: Area,Demand,rCurrentDateAndTimeJulian,t1(AppGrid%NElements),rElemAreas(AppGrid%NElements)
    LOGICAL                          :: lReturnFracUpdated,lReuseFracUpdated,lAgWaterDemandUpdated,lIrigPeriodUpdated
    TYPE(GenericLandUseType),POINTER :: pCrop
    CHARACTER(LEN=9),ALLOCATABLE     :: cLUCodes(:)
    REAL(8),ALLOCATABLE              :: ElemObsAreas1(:,:),ExIntAreas(:,:)
    
    !Initialize
    iStat      = 0
    NElements  = AppGrid%NElements
    iElemIDs   = AppGrid%AppElement%ID
    rElemAreas = AppGrid%AppElement%Area

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
        CALL RootZone%NonPondedAgRootZone%ReadTSData(TimeStep,AppGrid,RootZone%IrigPeriodFile,iElemIDs,rElemAreas,RootZone%ElemSoilsData%WiltingPoint,RootZone%ElemSoilsData%FieldCapacity,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Ponded ag related data
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        CALL RootZone%PondedAgRootZone%ReadTSData(TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Urban lands related data
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%UrbanRootZone%ReadTSData(TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Native and riparian veg data
    IF (RootZone%Flags%lNVRV_Defined) THEN
        CALL RootZone%NVRVRootZone%ReadTSData(TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
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
    
    !Update urban element demand 
    CALL RootZone%UrbanRootZone%ComputeWaterDemand()
        
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
                MessageArray(1) = 'Agricultural re-use fraction for crop ' //TRIM(pCropCodes(indxLU))//' at element '//TRIM(IntToText(iElemIDs(indxElem)))//' is greater than return flow fraction!'
                WRITE (MessageArray(2),'(A,F5.3)') 'Re-use fraction      = ',pReuseFrac(pCrops(indxLU,indxElem)%iColReuseFrac)
                WRITE (MessageArray(3),'(A,F5.3)') 'Return flow fraction = ',pReturnFrac(pCrops(indxLU,indxElem)%iColReturnFrac)
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
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
                MessageArray(1) = 'Urban re-use fraction at element '//TRIM(IntToText(iElemIDs(indxElem)))//' is greater than return flow fraction!'
                WRITE (MessageArray(2),'(A,F5.3)') 'Re-use fraction      = ',pReuseFrac(pUrban(indxElem)%iColReuseFrac)
                WRITE (MessageArray(3),'(A,F5.3)') 'Return flow fraction = ',pReturnFrac(pUrban(indxElem)%iColReturnFrac)
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
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
            !Check with non-ponded crops
            IF (RootZone%Flags%lNonPondedAg_Defined) THEN
                CALL RootZone%NonPondedAgRootZone%CheckSpecifiedDemandAndArea(iElemIDs,RootZone%AgWaterDemandFile%rValues,iStat)
                IF (iStat .NE. 0) RETURN
            END IF
            !Check with ponded crops
            IF (RootZone%Flags%lPondedAg_Defined) THEN
                CALL RootZone%PondedAgRootZone%CheckSpecifiedDemandAndArea(iElemIDs,RootZone%AgWaterDemandFile%rValues,iStat)
                IF (iStat .NE. 0) RETURN
            END IF
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
                MessageArray(1) = 'Urban water demand at element '//TRIM(IntToText(iElemIDs(indxElem)))//' is greater'
                MessageArray(2) = 'than zero when urban area is zero.'
                MessageArray(3) = '(This may be due to the element being specified as a lake element)'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
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
    IF (.NOT. RootZone%Flags%lMoistureContentToDepth) THEN
        CALL RootZone%NonPondedAgRootZone%SoilMContent_To_Depth(NElements,iElemIDs,RootZone%ElemSoilsData%TotalPorosity,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL RootZone%PondedAgRootZone%SoilMContent_To_Depth(NElements,RootZone%ElemSoilsData%TotalPorosity)
        CALL RootZone%UrbanRootZone%SoilMContent_To_Depth(NElements,iElemIDs,RootZone%ElemSoilsData%TotalPorosity,iStat)        ;  IF (iStat .EQ. -1) RETURN 
        CALL RootZone%NVRVRootZone%SoilMContent_To_Depth(NElements,iElemIDs,RootZone%ElemSoilsData%TotalPorosity,iStat)         ;  IF (iStat .EQ. -1) RETURN
        RootZone%Flags%lMoistureContentToDepth = .TRUE.
    END IF
    
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

  END SUBROUTINE RootZone_v40_ReadTSData
  
  
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
    IF (iStat .EQ. -1) RETURN

    lReturnFracUpdated = .FALSE.
    IF (FileReadCode .EQ. 0) lReturnFracUpdated = .TRUE.

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
  SUBROUTINE RootZone_v40_PrintRestartData(RootZone,OutFile)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    TYPE(GenericFileType)               :: OutFile
    
    CALL OutFile%WriteData(RootZone%RSoilM_P)
    CALL OutFile%WriteData(RootZone%RSoilM)
    
    IF (RootZone%Flags%lNonPondedAg_Defined) CALL RootZone%NonPondedAgRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lPondedAg_Defined) CALL RootZone%PondedAgRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lUrban_Defined) CALL RootZone%UrbanRootZone%PrintRestartData(OutFile)
    IF (RootZone%Flags%lNVRV_Defined)  CALL RootZone%NVRVRootZone%PrintRestartData(OutFile)
    
  END SUBROUTINE RootZone_v40_PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE TO PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation)
    CLASS(RootZone_v40_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETTYpe),INTENT(IN)       :: ETData          !Not used in this version
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
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
        RPump_Ag              = RegionalPumping(AppGrid,RootZone,f_iAgIndex)
        RPump_Urb             = RegionalPumping(AppGrid,RootZone,f_iUrbIndex)
        RDeli_Ag              = RegionalDeliveries(AppGrid,RootZone,f_iAgIndex)
        RDeli_Urb             = RegionalDeliveries(AppGrid,RootZone,f_iUrbIndex)
        RUpstrmElemRunoff_Ag  = RegionalUpStrmElemFlow(AppGrid,RootZone,DemandFracAg,f_iAgIndex)
        RUpstrmElemRunoff_Urb = RegionalUpStrmElemFlow(AppGrid,RootZone,DemandFracAg,f_iUrbIndex)
        RUpstrmElemRunoff_NV  = RegionalUpStrmElemFlow(AppGrid,RootZone,DemandFracAg,f_iNVIndex)
        RLUArea_Ag            = RegionalLUArea(AppGrid,RootZone,f_iAgIndex)
        RLUArea_Urb           = RegionalLUArea(AppGrid,RootZone,f_iUrbIndex)
        RLUArea_NV            = RegionalLUArea(AppGrid,RootZone,f_iNVIndex)
        IF (pFlags%lGenericMoistureFile_Defined) THEN
            RGenericMoist_Ag  = RegionalGenericMoistInflow(AppGrid,RootZone,f_iAgIndex)
            RGenericMoist_Urb = RegionalGenericMoistInflow(AppGrid,RootZone,f_iUrbIndex)
            RGenericMoist_NV  = RegionalGenericMoistInflow(AppGrid,RootZone,f_iNVIndex)
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
      CALL RootZone%NonPondedAgRootZone%PrintResults(AppGrid,RootZone%ElemSupply,ElemPrecip,prGenericMoisture)

      !Ponded ag results print-out
      CALL RootZone%PondedAgRootZone%PrintResults(AppGrid,RootZone%ElemSupply,ElemPrecip,prGenericMoisture)

      !Final moisture print-out
      IF (lEndOfSimulation) THEN
        IF (pFlags%FinalMoistureOutFile_Defined) CALL WriteFinalMoistures(AppGrid%NElements,AppGrid%AppElement%ID,RootZone)
      END IF

    END ASSOCIATE
    
  END SUBROUTINE RootZone_v40_PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FINAL MOISTURES
  ! -------------------------------------------------------------
  SUBROUTINE WriteFinalMoistures(NElements,iElemIDs,RootZone)
    INTEGER,INTENT(IN)      :: NElements,iElemIDs(NElements)
    TYPE(RootZone_v40_Type) :: RootZone
    
    !Local variables
    INTEGER                      :: NCrops,indxCrop,indxElem
    CHARACTER(LEN=16)            :: cArrayNP(RootZone%NonPondedAgRootZone%NCrops+1), &
                                    cArrayP(f_iNPondedCrops+1)                     , &
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
          cArrayNP(1) = ADJUSTR(TRIM(IntToText(iElemIDs(indxElem))))
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
        DO indxCrop=1,f_iNPondedCrops
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
          cArrayP(1) = ADJUSTR(TRIM(IntToText(iElemIDs(indxElem))))
          DO indxCrop=1,f_iNPondedCrops
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
          cArrayU(1) = ADJUSTR(TRIM(IntToText(iElemIDs(indxElem))))
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
          cArrayNVRV(1) = ADJUSTR(TRIM(IntToText(iElemIDs(indxElem))))
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
    TYPE(RootZone_v40_Type)         :: RootZone
    
    !Local variables
    INTEGER                                  :: NRegions,NElements
    REAL(8)                                  :: DummyArray(f_iNRootZoneBudColumns,(AppGrid%NSubregions+1)) 
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RRunoff_Ag,RRunoff_Urb,RRunoff_NV,                         &
                                                RPrecip_Ag,RPrecip_Urb,RPrecip_NV,                         &
                                                RReuse_Ag,RReuse_Urb,                                      &
                                                RReturn_Ag,RReturn_Urb,                                    &
                                                RDrain_Ag,                                                 &
                                                RSoilMCh_Ag,RSoilMCh_Urb,RSoilMCh_NV,                      &
                                                RInfilt_Ag,RInfilt_Urb,RInfilt_NV,                         &
                                                RETPot_Ag,RETPot_Urb,RETPot_NV,                            &
                                                RETa_Ag,RETa_Urb,RETa_NV,                                  &
                                                RPerc_Ag,RPerc_Urb,RPerc_NV,                               &
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
          CALL RegionalETPot(AppGrid,RootZone,f_iAgIndex,RETPot_Ag)
          RPrecip_Ag   = RegionalPrecip(AppGrid,RootZone,f_iAgIndex)
          RRunoff_Ag   = RegionalRunoff(AppGrid,RootZone,f_iAgIndex)
          RReuse_Ag    = RegionalReuse(AppGrid,RootZone,f_iAgIndex)
          RReturn_Ag   = RegionalReturn(AppGrid,RootZone,f_iAgIndex)
          RSoilMCh_Ag  = RegionalSoilMChange(AppGrid,RootZone,f_iAgIndex) 
          RInfilt_Ag   = RegionalInfiltration(AppGrid,RootZone,f_iAgIndex)
          RDrain_Ag    = RegionalDrain(AppGrid,RootZone,f_iAgIndex)
          RETa_Ag      = RegionalETa(AppGrid,RootZone,f_iAgIndex) 
          RPerc_Ag     = RegionalPerc(AppGrid,RootZone,f_iAgIndex)
          Error_Ag     = RootZone%RSoilM_P(:,f_iAgIndex) + RSoilMCh_Ag + RInfilt_Ag + RGenericMoist_Ag - RDrain_Ag - RETa_Ag - RPerc_Ag - RootZone%RSoilM(:,f_iAgIndex)
      END IF
            
      !Urban
      IF (pFlags%lUrban_Defined) THEN
          CALL RegionalETPot(AppGrid,RootZone,f_iUrbIndex,RETPot_Urb)
          RPrecip_Urb   = RegionalPrecip(AppGrid,RootZone,f_iUrbIndex)
          RRunoff_Urb   = RegionalRunoff(AppGrid,RootZone,f_iUrbIndex)
          RReuse_Urb    = RegionalReuse(AppGrid,RootZone,f_iUrbIndex)
          RReturn_Urb   = RegionalReturn(AppGrid,RootZone,f_iUrbIndex)
          RSoilMCh_Urb  = RegionalSoilMChange(AppGrid,RootZone,f_iUrbIndex) 
          RInfilt_Urb   = RegionalInfiltration(AppGrid,RootZone,f_iUrbIndex)
          RETa_Urb      = RegionalETa(AppGrid,RootZone,f_iUrbIndex)
          RPerc_Urb     = RegionalPerc(AppGrid,RootZone,f_iUrbIndex)
          Error_Urb     = RootZone%RSoilM_P(:,f_iUrbIndex) + RSoilMCh_Urb + RInfilt_Urb + RGenericMoist_Urb - RETa_Urb - RPerc_Urb - RootZone%RSoilM(:,f_iUrbIndex)
      END IF
      
      !Native and riparian veg
      IF (pFlags%lNVRV_Defined) THEN
          CALL RegionalETPot(AppGrid,RootZone,f_iNVIndex,RETPot_NV)
          RPrecip_NV   = RegionalPrecip(AppGrid,RootZone,f_iNVIndex)       
          RRunoff_NV   = RegionalRunoff(AppGrid,RootZone,f_iNVIndex)
          RSoilMCh_NV  = RegionalSoilMChange(AppGrid,RootZone,f_iNVIndex) 
          RInfilt_NV   = RegionalInfiltration(AppGrid,RootZone,f_iNVIndex)
          RETa_NV      = RegionalETa(AppGrid,RootZone,f_iNVIndex)
          RPerc_NV     = RegionalPerc(AppGrid,RootZone,f_iNVIndex)
          Error_NV     = RootZone%RSoilM_P(:,f_iNVIndex) + RSoilMCh_NV + RInfilt_NV + RGenericMoist_NV - RETa_NV - RPerc_NV - RootZone%RSoilM(:,f_iNVIndex)
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
    DummyArray(9,:)  = RootZone%RSoilM_P(:,f_iAgIndex)                             !Storage at the beginning of the time interval
    DummyArray(10,:) = RSoilMCh_Ag                                                 !Soil moisture change due to expansion/contraction of ag lands
    DummyArray(11,:) = RInfilt_Ag                                                  !Infiltration on ag lands
    DummyArray(12,:) = RGenericMoist_Ag                                            !Generic moisture inflow to ag lands
    DummyArray(13,:) = RDrain_Ag                                                   !Rice/refuge pond drainage on ag lands
    DummyArray(14,:) = RETa_Ag                                                     !ET on ag lands
    DummyArray(15,:) = RPerc_Ag                                                    !Percolation on ag lands
    DummyArray(16,:) = RootZone%RSoilM(:,f_iAgIndex)                               !Storage at the end of the time interval
    DummyArray(17,:) = Error_Ag                                                    !Mass balance error for ag lands
    DummyArray(18,:) = RLUArea_Urb                                                 !Urban area
    DummyArray(19,:) = RETPot_Urb                                                  !Potential ET on urban lands
    DummyArray(20,:) = RPrecip_Urb                                                 !Precipitation on urban lands
    DummyArray(21,:) = RRunoff_Urb                                                 !Runoff from urban lands
    DummyArray(22,:) = RDeli_Urb + RPump_Urb                                       !Prime applied water on urban lands prior to re-used water
    DummyArray(23,:) = RUpstrmElemRunoff_Urb                                       !Surface runoff from upstream elements/subregions used on urban lands
    DummyArray(24,:) = RReuse_Urb                                                  !Applied recycled water on urban indoors and outdoors
    DummyArray(25,:) = RReturn_Urb                                                 !Return flow from urban lands
    DummyArray(26,:) = RootZone%RSoilM_P(:,f_iUrbIndex)                            !Storage at the beginning of the time interval
    DummyArray(27,:) = RSoilMCh_Urb                                                !Soil moisture change due to expansion/contraction of urban lands
    DummyArray(28,:) = RInfilt_Urb                                                 !Infiltration on urban lands
    DummyArray(29,:) = RGenericMoist_Urb                                           !Generic moisture inflow to urban lands
    DummyArray(30,:) = RETa_Urb                                                    !ET on urban lands
    DummyArray(31,:) = RPerc_Urb                                                   !Percolation on urban lands
    DummyArray(32,:) = RootZone%RSoilM(:,f_iUrbIndex)                              !Storage at the end of the time interval     
    DummyArray(33,:) = Error_Urb                                                   !Mass balance error at urban lands
    DummyArray(34,:) = RLUArea_NV                                                  !Natural area
    DummyArray(35,:) = RETPot_NV                                                   !Potential ET on natural lands
    DummyArray(36,:) = RPrecip_NV                                                  !Precipitation on natural lands
    DummyArray(37,:) = RUpstrmElemRunoff_NV                                        !Runoff from upstream elements onto natural lands
    DummyArray(38,:) = RRunoff_NV                                                  !Total surface flow on natural lands
    DummyArray(39,:) = RootZone%RSoilM_P(:,f_iNVIndex)                             !Storage at the beginning of the time interval
    DummyArray(40,:) = RSoilMCh_NV                                                 !Soil moisture change due to expansion/contraction of natural lands
    DummyArray(41,:) = RInfilt_NV                                                  !Infiltration on natural lands
    DummyArray(42,:) = RGenericMoist_NV                                            !Generic moisture inflow to natural lands
    DummyArray(43,:) = RETa_NV                                                     !ET on natural lands
    DummyArray(44,:) = RPerc_NV                                                    !Percolation on natural lands
    DummyArray(45,:) = RootZone%RSoilM(:,f_iNVIndex)                               !Storage at the end of the time interval          
    DummyArray(46,:) = Error_NV                                                    !Mass balance error at native and riparian lands

    !Print out values to binary file
    CALL RootZone%RootZoneBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteRootZoneFlowsToBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO LAND & WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToBudRawFile(AppGrid,RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb,RootZone)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RUpstrmElemRunoff_Ag,RUpstrmElemRunoff_Urb
    TYPE(RootZone_v40_Type)         :: RootZone
    
    !Local variables
    REAL(8)                                  :: DummyArray(f_iNLWUseBudColumns,(AppGrid%NSubregions+1))
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RDemandRaw_Ag,RDemand_Ag,RDemand_Urb, &
                                                RDemandShort_Ag,RDemandShort_Urb,     &
                                                RETAW,RETP,RETOth
    
    !Compute budget terms
    IF (RootZone%Flags%lNonPondedAg_Defined .OR. RootZone%Flags%lPondedAg_Defined) THEN
      RDemandRaw_Ag   = RegionalAgRawDemand(AppGrid,RootZone)
      RDemand_Ag      = RegionalDemand(AppGrid,RootZone,f_iAgIndex)
      RDemandShort_Ag = RDemand_Ag - RPump_Ag - RDeli_Ag - RUpstrmElemRunoff_Ag
      RETAW           = RegionalETAW(AppGrid,RootZone)
      RETP            = RegionalETP(AppGrid,RootZone)
      RETOth          = RegionalETOth(AppGrid,RootZone)
    ELSE
      RDemandRaw_Ag   = 0.0
      RDemand_Ag      = 0.0
      RDemandShort_Ag = 0.0
      RETAW           = 0.0
      RETP            = 0.0
      RETOth          = 0.0
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
      RDemand_Urb      = RegionalDemand(AppGrid,RootZone,f_iUrbIndex)
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
    DummyArray(10,:) = RETOth
    DummyArray(11,:) = RLUArea_Urb
    DummyArray(12,:) = RDemand_Urb
    DummyArray(13,:) = RPump_Urb
    DummyArray(14,:) = RDeli_Urb
    DummyArray(15,:) = RUpstrmElemRunoff_Urb
    DummyArray(16,:) = RDemandShort_Urb

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
  SUBROUTINE RootZone_v40_ComputeWaterDemand(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v40_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Initialize
    iStat = 0

    !Return if root zone is not simulated
    IF (RootZone%NLands .EQ. 0) RETURN
    
    !Echo progress
    CALL EchoProgress('Computing agricultural water demand')

    !Compute ag water demand (urban water demands are read in as input)
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
      CALL RootZone%NonPondedAgRootZone%ComputeWaterDemand(AppGrid                                      , &
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
                                                           iStat                                        )
      IF (iStat .EQ. -1) RETURN
    END IF
                                               
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) &
      CALL RootZone%PondedAgRootZone%ComputeWaterDemand(AppGrid                                      , &
                                                        ETData                                       , &
                                                        TimeStep%DeltaT                              , &
                                                        RootZone%ElemPrecipData%Precip               , &
                                                        RootZone%GenericMoistureData%rGenericMoisture, &
                                                        RootZone%ElemSoilsData                       , &
                                                        RootZone%HydCondPonded                       , &
                                                        RootZone%AgWaterDemandFile%rValues           , &
                                                        RootZone%IrigPeriodFile%iValues              , &
                                                        RootZone%Flags%lLakeElems                    , &
                                                        RootZone%Flags%lReadPondedAgWaterDemand      )
    
    !Compute demand and supply related fractions
    CALL ComputeDemandSupplyRelatedFracs(AppGrid,RootZone)

  END SUBROUTINE RootZone_v40_ComputeWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENTAL FUTURE WATER DEMAND UNTIL A SPECIFIED DATE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_ComputeFutureWaterDemand(RootZone,AppGrid,TimeStep,Precip,ET,cEndComputeDate,iStat)
    CLASS(RootZone_v40_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(PrecipitationType)       :: Precip
    TYPE(ETType)                  :: ET
    CHARACTER(LEN=*),INTENT(IN)   :: cEndComputeDate
    INTEGER,INTENT(OUT)           :: iStat 
    
    !Local variables
    INTEGER                 :: iNPeriods,iErrorCode,indxTime,iDim
    REAL(8)                 :: rDemand_Ag(AppGrid%NElements),rDemand_Urb(AppGrid%NElements)
    TYPE(RootZone_v40_Type) :: RootZone_Work
    TYPE(TimeStepType)      :: TimeStep_Work
    
    !Initialize
    iStat = 0

    !Return if root zone is not simulated
    IF (RootZone%NLands .EQ. 0) RETURN
    
    !Return if future demands are already computed until the date
    IF (ALLOCATED(RootZone%cFutureDemandDates)) THEN
        iDim = SIZE(RootZone%cFutureDemandDates)
        IF (RootZone%cFutureDemandDates(iDim) .TSGE. cEndComputeDate) RETURN
    END IF
    
    !Echo progress
    CALL EchoProgress('Computing future water demands')
    
    !Initialize the working RootZone and TimeStep objects
    RootZone_Work = RootZone
    TimeStep_Work = TimeStep
    
    !Calculate the number of time steps until cEndComputeDate
    iNPeriods = NPeriods(TimeStep%DELTAT_InMinutes,TimeStep%CurrentDateAndTime,cEndComputeDate) + 1
    
    !Allocate memory
    DEALLOCATE (RootZone%cFutureDemandDates , RootZone%rFutureAgElemDemand , RootZone%rFutureUrbElemDemand , STAT=iErrorCode)
    ALLOCATE (RootZone%cFutureDemandDates(iNPeriods) , RootZone%rFutureAgElemDemand(AppGrid%NElements,iNPeriods) , RootZone%rFutureUrbElemDemand(AppGrid%NElements,iNPeriods))

    !Loop through timesteps to simulate future demand
    DO indxTime=1,iNPeriods
        !Demand computation date
        RootZone%cFutureDemandDates(indxTime) = TimeStep_Work%CurrentDateAndTime
        
        !Read time series data
        CALL Precip%ReadTSData(TimeStep_Work,iStat)                                 ;  IF (iStat .NE. 0) RETURN
        CALL ET%ReadTSData(TimeStep_Work,iStat)                                     ;  IF (iStat .NE. 0) RETURN
        CALL RootZone_Work%ReadTSData(AppGrid,TimeStep_Work,Precip,ET,iStat=iStat)  ;  IF (iStat .NE. 0) RETURN

        !Compute water demand
        CALL RootZone_Work%ComputeWaterDemand(AppGrid,TimeStep_Work,ET,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Retrieve water demands
        CALL RootZone_Work%GetWaterDemandAll(f_iAg,rDemand_Ag)    ;  RootZone%rFutureAgElemDemand(:,indxTime)  = rDemand_Ag
        CALL RootZone_Work%GetWaterDemandAll(f_iUrb,rDemand_urb)  ;  RootZone%rFutureUrbElemDemand(:,indxTime) = rDemand_Urb

        !Set the water supply to be equal to water demand and from diversions
        CALL RootZone_Work%ZeroSupply()
        CALL RootZone_Work%SetSupply(rDemand_Ag,f_iSupply_Diversion,f_iAg)
        CALL RootZone_Work%SetSupply(rDemand_Urb,f_iSupply_Diversion,f_iUrb)
        
        !Simulate flows
        CALL RootZone_Work%Simulate(AppGrid,TimeStep_Work,ET,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Advance state
        CALL RootZone_Work%AdvanceState()
        
        !Advance time
        TimeStep_Work%CurrentDateAndTime = IncrementTimeStamp(TimeStep_Work%CurrentDateAndTime,TimeStep_Work%DELTAT_InMinutes)
        TimeStep_Work%CurrentTimeStep    = TimeStep_Work%CurrentTimeStep + 1
        
    END DO
    
    !Rewind timeseries input files
    CALL Precip%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL Precip%ReadTSData(TimeStep,iStat)                   ;  IF (iStat .NE. 0) RETURN
    CALL ET%File%RewindFile_To_BeginningOfTSData(iStat)      ;  IF (iStat .NE. 0) RETURN
    CALL ET%ReadTSData(TimeStep,iStat)                       ;  IF (iStat .NE. 0) RETURN
    CALL RewindTSInputFilesToTimeStamp(RootZone,AppGrid%AppElement%ID,AppGrid%AppElement%Area,TimeStep,iStat)             

  END SUBROUTINE RootZone_v40_ComputeFutureWaterDemand
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SOIL MOISTURE IN ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_Simulate(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v40_Type)      :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'RootZone_v40_Simulate'
    INTEGER                      :: indxElem,indxIter,NElements,iElemID
    REAL(8)                      :: AchievedConv,DeltaT,Area,                                                        &    
                                    IrigSupply_Ag(AppGrid%NElements),IrigSupply_Urb(AppGrid%NElements),              &
                                    UpstrmElemRunoff_P(AppGrid%NElements),SurfaceFlow(AppGrid%NElements),            &                                
                                    ElemCropSupply(RootZone%NonPondedAgRootZone%NCrops,AppGrid%NElements),           &
                                    ElemPondSupply(f_iNPondedCrops,AppGrid%NElements),                               &
                                    ElemGenSupply(AppGrid%NElements),Runoff,Runoff_P,                                &
                                    InRunoffNP(RootZone%NonPondedAgRootZone%NCrops,AppGrid%NElements),               &
                                    InRunoffP(RootZone%PondedAgRootZone%NCrops,AppGrid%NElements),                   &
                                    InRunoffUrb(1,AppGrid%NElements),InRunoffNVRV(1,AppGrid%NElements)
    
    !Initialize
    iStat = 0

    ASSOCIATE (pElemSupply          => RootZone%ElemSupply                           , &
               pSoilsData           => RootZone%ElemSoilsData                        , &
               pElemsToGW           => RootZone%ElemFlowToGW                         , &
               pElemPrecip          => RootZone%ElemPrecipData%Precip                , &
               prGenericMoisture    => RootZone%GenericMoistureData%rGenericMoisture , &
               pReuseFracs          => RootZone%ReuseFracFile%rValues                , &
               pReturnFracs         => RootZone%ReturnFracFile%rValues               , &
               pSolverData          => RootZone%SolverData                           , &
               pNVRV                => RootZone%NVRVRootZone                         )
               
      !Initialize
      DeltaT                   = TimeStep%DeltaT
      pElemSupply%UpstrmRunoff = 0.0
      NElements                = AppGrid%NElements
      IrigSupply_Ag            = pElemSupply%Diversion_Ag  + pElemSupply%Pumping_Ag
      IrigSupply_Urb           = pElemSupply%Diversion_Urb + pElemSupply%Pumping_Urb
    
      !Check water supply vs. irrigable lands
      !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem,iElemID,Area) NUM_THREADS(OMP_GET_NUM_PROCS()-1) SCHEDULE(DYNAMIC,500)
      DO indxElem=1,NElements
          !If this is a lake element, report that to the user
          IF (RootZone%Flags%lLakeElems(indxElem)) THEN
              IF (IrigSupply_Ag(indxElem)+IrigSupply_Urb(indxElem) .GT. 0.0) THEN 
                  iElemID         = AppGrid%AppElement(indxElem)%ID
                  MessageArray(1) = 'Element '//TRIM(IntToText(iElemID))//' is a lake element.'
                  MessageArray(2) = 'Water supply for lake elements must be zero!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  CYCLE
              END IF
          END IF
          !Check ag area vs. ag water supply
          IF (IrigSupply_Ag(indxElem) .GT. 0.0) THEN
              Area = 0.0
              IF (RootZone%Flags%lNonPondedAg_Defined) Area = Area + SUM(RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Area)
              IF (RootZone%Flags%lPondedAg_Defined)    Area = Area + SUM(RootZone%PondedAgRootZone%Crops(:,indxElem)%Area)
              IF (Area .EQ. 0.0) THEN
                  iElemID         = AppGrid%AppElement(indxElem)%ID
                  MessageArray(1) = 'Agricultural applied water at element '//TRIM(IntToText(iElemID))//' cannot be non-zero'
                  MessageArray(2) = 'when agricultural area is zero!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  CYCLE
              END IF
          END IF
          !Check urban area vs. urban water supply
          IF (IrigSupply_Urb(indxElem) .GT. 0.0) THEN
              IF (RootZone%Flags%lUrban_Defined) THEN
                  Area = RootZone%UrbanRootZone%UrbData(indxElem)%Area
              ELSE
                  Area = 0.0
              END IF
              IF (Area .EQ. 0.0) THEN
                  iElemID         = AppGrid%AppElement(indxElem)%ID
                  MessageArray(1) = 'Urban applied water at element '//TRIM(IntToText(iElemID))//' cannot be non-zero'
                  MessageArray(2) = 'when urban area is zero!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  CYCLE
              END IF
          END IF
      END DO
      !$OMP END PARALLEL DO
    
      !Return if there was an error
      IF (iStat .EQ. -1) RETURN
        
      !Iterative solution
      DO indxIter=1,pSolverData%IterMax
          !Store UpstrmElemRunoff values in temporary stoarge and zero it out
          UpstrmElemRunoff_P       = pElemSupply%UpstrmRunoff
          pElemSupply%UpstrmRunoff = 0.0
          
          !Simulate non-ponded ag lands
          IF (RootZone%Flags%lNonPondedAg_Defined) THEN
              CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,f_iLandUse_NonPonded,InRunoffNP)
              !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) NUM_THREADS(OMP_GET_NUM_PROCS()-1) SCHEDULE(STATIC,100)
              DO indxElem=1,NElements
                  ElemCropSupply(:,indxElem) = InRunoffNP(:,indxElem) + IrigSupply_Ag(indxElem) * RootZone%NonPondedAgRootZone%Crops(:,indxElem)%ElemDemandFrac_Ag
              END DO
              !$OMP END PARALLEL DO
              CALL RootZone%NonPondedAgRootZone%Simulate(AppGrid                   , &
                                                         ETData                    , &
                                                         DeltaT                    , &
                                                         pElemPrecip               , &
                                                         prGenericMoisture         , &
                                                         pSoilsData                , &
                                                         ElemCropSupply            , &
                                                         pReuseFracs               , &
                                                         pReturnFracs              , &
                                                         pElemsToGW                , &
                                                         pSolverData               , &
                                                         RootZone%Flags%lLakeElems , &
                                                         iStat                     )
              IF (iStat .EQ. -1) RETURN  
              !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) NUM_THREADS(OMP_GET_NUM_PROCS()-1) SCHEDULE(STATIC,100)
              DO indxElem=1,NElements
                  SurfaceFlow(indxElem) = SUM(RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Runoff + RootZone%NonPondedAgRootZone%Crops(:,indxElem)%ReturnFlow , DIM=1)
              END DO
              !$OMP END PARALLEL DO
              CALL FlowToElements(SurfaceFlow                , &
                                  AppGrid                    , &
                                  RootZone                   , &
                                  f_iNVIndex                 , &
                                  pElemSupply%UpstrmRunoff   )
          END IF
          
          !Simulate ponded ag lands
          IF (RootZone%Flags%lPondedAg_Defined) THEN  
              CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,f_iLandUse_Ponded,InRunoffP)
              !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) NUM_THREADS(OMP_GET_NUM_PROCS()-1) SCHEDULE(STATIC,100)
              DO indxElem=1,NElements
                  ElemPondSupply(:,indxElem) = InRunoffP(:,indxElem) + IrigSupply_Ag(indxElem) * RootZone%PondedAgRootZone%Crops(:,indxElem)%ElemDemandFrac_Ag
              END DO
              !$OMP END PARALLEL DO
              CALL RootZone%PondedAgRootZone%Simulate(AppGrid                   , &
                                                      ETData                    , &
                                                      DeltaT                    , &
                                                      pElemPrecip               , &
                                                      prGenericMoisture         , &
                                                      pSoilsData                , &
                                                      RootZone%HydCondPonded    , &
                                                      ElemPondSupply            , &
                                                      pElemsToGW                , &
                                                      pSolverData               , &
                                                      RootZone%Flags%lLakeElems , &
                                                      iStat                     )
              IF (iStat .EQ. -1) RETURN
              !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) NUM_THREADS(OMP_GET_NUM_PROCS()-1) SCHEDULE(STATIC,100)
              DO indxElem=1,NElements
                  SurfaceFlow(indxElem) = SUM(RootZone%PondedAgRootZone%Crops(:,indxElem)%Runoff + RootZone%PondedAgRootZone%Crops(:,indxElem)%ReturnFlow + RootZone%PondedAgRootZone%Crops(:,indxElem)%Drain , DIM=1)
              END DO
              !$OMP END PARALLEL DO
              CALL FlowToElements(SurfaceFlow              , &
                                  AppGrid                  , &
                                  RootZone                 , &
                                  f_iNVIndex               , &
                                  pElemSupply%UpstrmRunoff )
          END IF
          
          !Simulate urban lands
          IF (RootZone%Flags%lUrban_Defined) THEN 
              CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,f_iLandUse_Urban,InRunoffUrb)
              !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) NUM_THREADS(OMP_GET_NUM_PROCS()-1) SCHEDULE(STATIC,100)
              DO indxElem=1,NElements
                  ElemGenSupply(indxElem) = InRunoffUrb(1,indxElem) + IrigSupply_Urb(indxElem)
              END DO
              !$OMP END PARALLEL DO
              CALL RootZone%UrbanRootZone%Simulate(AppGrid                   , &
                                                   ETData                    , &
                                                   DeltaT                    , &
                                                   pElemPrecip               , &
                                                   prGenericMoisture         , &
                                                   pSoilsData                , &
                                                   ElemGenSupply             , &
                                                   pReuseFracs               , &
                                                   pReturnFracs              , &
                                                   pElemsToGW                , &
                                                   pSolverData               , &
                                                   RootZone%Flags%lLakeElems , &
                                                   iStat                     )
              IF (iStat .EQ. -1) RETURN
              SurfaceFlow = RootZone%UrbanRootZone%UrbData%Runoff + RootZone%UrbanRootZone%UrbData%ReturnFlow
              CALL FlowToElements(SurfaceFlow              , &
                                  AppGrid                  , &
                                  RootZone                 , &
                                  f_iNVIndex               , &
                                  pElemSupply%UpstrmRunoff )
          END IF
          
          !Simulate native and riparian veg lands
          IF (RootZone%Flags%lNVRV_Defined) THEN
              CALL ComputeUpstrmElemRunoffToLandUse(AppGrid,UpstrmElemRunoff_P,RootZone,f_iLandUse_NVRV,InRunoffNVRV)
              CALL pNVRV%Simulate(AppGrid                   , &
                                  ETData                    , &
                                  DeltaT                    , &
                                  pElemPrecip               , &
                                  prGenericMoisture         , &
                                  pSoilsData                , &
                                  InRunoffNVRV(1,:)         , &
                                  pElemsToGW                , &
                                  pSolverData               , &
                                  RootZone%Flags%lLakeElems , &
                                  iStat                     )
              IF (iStat .EQ. -1) RETURN
              SurfaceFlow = pNVRV%NativeVeg%Runoff + pNVRV%RiparianVeg%Runoff
              CALL FlowToElements(SurfaceFlow              , &
                                  AppGrid                  , &
                                  RootZone                 , &
                                  f_iNVIndex               , &
                                  pElemSupply%UpstrmRunoff )
          END IF 
          
          !Check convergence
          AchievedConv = 0.0
          IF (ANY(pElemSupply%UpstrmRunoff .NE. 0.0)) THEN  !Needed to add this check to avoid a signaling IEEE_INVALID flag
              DO indxElem=1,NElements
                  Runoff   = pElemSupply(indxElem)%UpstrmRunoff
                  Runoff_P = UpstrmElemRunoff_P(indxElem)
                  IF (Runoff .EQ. 0.0) THEN
                      IF (Runoff_P .EQ. 0.0) THEN
                          CYCLE
                      ELSE
                          AchievedConv = MAX(AchievedConv , 1D0)  ! 1D0 = ABS((Runoff - Runoff_P) / Runoff_P))
                      END IF
                  ELSE
                      AchievedConv = MAX(AchievedConv,ABS((Runoff - Runoff_P) / Runoff))
                  END IF
              END DO
          END IF
          IF (AchievedConv .LT. RootZone%SolverData%Tolerance) THEN
               AchievedConv = 0.0
               EXIT
          END IF
      END DO
      
    END ASSOCIATE
    
  END SUBROUTINE RootZone_v40_Simulate




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
  FUNCTION RegionalDemand(AppGrid,RootZone,LUIndex) RESULT(RDemand)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RDemand(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RDemand   = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Demand , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Demand , DIM=1)
   
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Demand
      
      !Otherwise
      CASE DEFAULT
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
  ! --- COMPUTE REGIONAL ETOth
  ! -------------------------------------------------------------
  FUNCTION RegionalETOth(AppGrid,RootZone) RESULT(RETOth)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RReuse(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions,indxElemRegion,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue,Supply
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RReuse    = 0.0
    ElemValue = 0.0
    
    !Reuse internal in an element
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Reuse , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Reuse , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Reuse
        
      !Otherwise
      CASE DEFAULT
        RETURN
      
    END SELECT
    
    !Reuse due to element surface runoff being transferred to an element/subregion
    IF (LUIndex .EQ. f_iAgIndex) THEN
      IF (RootZone%Flags%lNonPondedAg_Defined) Supply = SUM(RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac, DIM=1)
      IF (RootZone%Flags%lPondedAg_Defined)    Supply = Supply + SUM(RootZone%PondedAgRootZone%Crops%ElemDemandFrac, DIM=1) 
      Supply = RootZone%ElemSupply%UpstrmRunoff * Supply
    ELSE
      Supply = RootZone%ElemSupply%UpstrmRunoff * RootZone%UrbanRootZone%UrbData%ElemDemandFrac
    END IF
    
    ASSOCIATE (pElemsToElements => RootZone%ElemFlowToElements     , &
               pElemsToSubregions => RootZone%ElemFlowToSubregions )
        !Process element flows to other elements
        DO indx=1,SIZE(pElemsToElements)
            iElem       = pElemsToElements(indx)%iElement
            iDest       = pElemsToElements(indx)%iDest
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iRegion .EQ. iDestRegion) RReuse(iDestRegion) = RReuse(iDestRegion) + Supply(iDest)
        END DO
        
        !Process elements flows to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDest
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RPrecip(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RPrecip   = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = RootZone%ElemPrecipData%Precip * SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + RootZone%ElemPrecipData%Precip * SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%ElemPrecipData%Precip * RootZone%UrbanRootZone%UrbData%Area
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RGenericMoist(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions,indxElem,indxCrop,NElements,NNonPondedCrops,NPondedCrops
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    LOGICAL                              :: lNonPondedAg_Defined,lPondedAg_Defined
    
    !Initialize
    NElements            = AppGrid%NElements
    NRegions             = AppGrid%NSubregions
    RGenericMoist        = 0.0
    ElemValue            = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
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
      CASE (f_iUrbIndex)
        IF (RootZone%Flags%lUrban_Defined) THEN
          ASSOCIATE (pUrbanRootZone => RootZone%UrbanRootZone)
            ElemValue = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * pUrbanRootZone%RootDepth - pUrbanRootZone%UrbData%GMExcess) * pUrbanRootZone%UrbData%Area * pUrbanRootZone%UrbData%PerviousFrac
          END ASSOCIATE
        END IF
        
      !Native and riparian vegetation
      CASE (f_iNVIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RSoilMCh(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RSoilMCh  = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%SoilMCh , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%SoilMCh , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%SoilMCh
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RETa(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RETa      = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ETa , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ETa , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%ETa
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RPerc(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RPerc     = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Perc + RootZone%NonPondedAgRootZone%Crops%PercCh , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Perc + RootZone%PondedAgRootZone%Crops%PercCh , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Perc + RootZone%UrbanRootZone%UrbData%PercCh
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
        ElemValue =  RootZone%NVRVRootZone%NativeVeg%Perc + RootZone%NVRVRootZone%NativeVeg%PercCh &
                   + RootZone%NVRVRootZone%RiparianVeg%Perc + RootZone%NVRVRootZone%RiparianVeg%PercCh
      
    END SELECT
    
    !Regional percolation
    RPerc(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
    RPerc(NRegions+1) = SUM(RPerc(1:NRegions))

  END FUNCTION RegionalPerc
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL INFILTRATION
  ! -------------------------------------------------------------
  FUNCTION RegionalInfiltration(AppGrid,RootZone,LUIndex) RESULT(RInfilt)
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RInfilt(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER                              :: NRegions
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RInfilt   = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%PrecipInfilt + RootZone%NonPondedAgRootZone%Crops%IrigInfilt , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%PrecipInfilt + RootZone%PondedAgRootZone%Crops%IrigInfilt , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%PrecipInfilt + RootZone%UrbanRootZone%UrbData%IrigInfilt 
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Area , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        IF (.NOT. RootZone%Flags%lUrban_Defined) RETURN
        ElemValue = RootZone%UrbanRootZone%UrbData%Area
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
        IF (.NOT. RootZone%Flags%lNVRV_Defined) RETURN
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    REAL(8)                            :: Area
    
    !Local variables
    INTEGER :: indxRegionElem,iElem
    
    !Initialize
    Area = 0.0
    
    ASSOCIATE (pAppSubregion => AppGrid%AppSubregion(iRegion))
    
      SELECT CASE (LUIndex)
        !Ponded and non-ponded ag
        CASE (f_iAgIndex)
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
        CASE (f_iUrbIndex)
          IF (RootZone%Flags%lUrban_Defined) THEN
            ASSOCIATE (pUrban => RootZone%UrbanRootZone%UrbData)
              DO indxRegionElem=1,pAppSubregion%NRegionElements
                iElem = pAppSubregion%RegionElements(indxRegionElem)
                Area = Area + pUrban(iElem)%Area
              END DO
            END ASSOCIATE
          END IF
      
        !Native and riparian vegetation
        CASE (f_iNVIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
      CASE (f_iAgIndex)
        ElemValue = RootZone%ElemSupply%UpstrmRunoff * DemandFracAg
    
      !Urban
      CASE (f_iUrbIndex)
        IF (.NOT. RootZone%Flags%lUrban_Defined) RETURN
        ElemValue = RootZone%ElemSupply%UpstrmRunoff * RootZone%UrbanRootZone%UrbData%ElemDemandFrac 
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
        IF (.NOT. RootZone%Flags%lNVRV_Defined) RETURN 
        WHERE (RootZone%ElemDevelopedArea .EQ. 0.0) ElemValue = RootZone%ElemSupply%UpstrmRunoff 
      
    END SELECT
      
    ASSOCIATE (pElemsToElements   => RootZone%ElemFlowToElements   , &
               pElemsToSubregions => RootZone%ElemFlowToSubregions )
        !Process element flow to other elements
        DO indx=1,SIZE(pElemsToElements)
            iElem       = pElemsToElements(indx)%iElement
            iDest       = pElemsToElements(indx)%iDest
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RInflow(iDestRegion) = RInflow(iDestRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDest
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RRunoff(AppGrid%NSubregions+1)

    !Local variables
    INTEGER                              :: NRegions,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RRunoff   = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue =ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Runoff , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%Runoff 
      
      !Native and riparian vegetation
      CASE (f_iNVIndex)
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
            iDest       = pElemsToElements(indx)%iDest
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RRunoff(iRegion) = RRunoff(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDest
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%Drain , DIM=1)
    
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
            iDest       = pElemsToElements(indx)%iDest
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RDrain(iRegion) = RDrain(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDest
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8)                            :: RReturn(AppGrid%NSubregions+1)

    !Local variables
    INTEGER                              :: NRegions,iRegion,iDest,iDestRegion,indx,iElem
    REAL(8),DIMENSION(AppGrid%NElements) :: ElemValue
    
    !Initialize
    NRegions  = AppGrid%NSubregions
    RReturn   = 0.0
    ElemValue = 0.0
    
    SELECT CASE (LUIndex)
    
      !Ponded and non-ponded ag
      CASE (f_iAgIndex)
        IF (RootZone%Flags%lNonPondedAg_Defined) ElemValue = SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow , DIM=1)
        IF (RootZone%Flags%lPondedAg_Defined)    ElemValue = ElemValue + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow , DIM=1)
    
      !Urban
      CASE (f_iUrbIndex)
        ElemValue = RootZone%UrbanRootZone%UrbData%ReturnFlow 
      
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
            iDest       = pElemsToElements(indx)%iDest
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = AppGrid%AppElement(iDest)%Subregion
            IF (iDestRegion .NE. iRegion) RReturn(iRegion) = RReturn(iRegion) + ElemValue(iElem)
        END DO
        
        !Process element flow to subregions
        DO indx=1,SIZE(pElemsToSubregions)
            iElem       = pElemsToSubregions(indx)%iElement
            iRegion     = AppGrid%AppElement(iElem)%Subregion
            iDestRegion = pElemsToSubregions(indx)%iDest
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
      CASE (f_iAgIndex)
       ElemValue         = RootZone%ElemSupply%Pumping_Ag
       RPump(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      
      !Urban
      CASE (f_iUrbIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
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
      CASE (f_iAgIndex)
        ElemValue         = RootZone%ElemSupply%Diversion_Ag
        RDeli(1:NRegions) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      
      !Urban
      CASE (f_iUrbIndex)
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
    TYPE(RootZone_v40_Type)      :: RootZone
    REAL(8)                      :: RSoilM(AppGrid%NSubregions+1,f_iNGroupLandUse)
    
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
        ElemValues                    = SUM((pCrops%SoilM_Precip + pCrops%SoilM_AW + pCrops%SoilM_Oth) * pCrops%Area , DIM=1)
        RSoilM(1:NRegions,f_iAgIndex) = AppGrid%AccumElemValuesToSubregions(ElemValues)
      END IF
      
      IF (pFlags%lPondedAg_Defined) THEN
        ElemValues                    = SUM((pPondedCrops%SoilM_Precip + pPondedCrops%SoilM_AW + pPondedCrops%SoilM_Oth) * pPondedCrops%Area , DIM=1)
        RSoilM(1:NRegions,f_iAgIndex) = RSoilM(1:NRegions,f_iAgIndex) + AppGrid%AccumElemValuesToSubregions(ElemValues)
      END IF
               
      IF (pFlags%lUrban_Defined) THEN
        ElemValues                     = (pUrban%SoilM_Precip + pUrban%SoilM_AW + pUrban%SoilM_Oth) * pUrban%Area * pUrban%PerviousFrac
        RSoilM(1:NRegions,f_iUrbIndex) = AppGrid%AccumElemValuesToSubregions(ElemValues)
      END IF
      
      IF (pFlags%lNVRV_Defined) THEN
        ElemValues                    = (pNV%SoilM_Precip + pNV%SoilM_AW + pNV%SoilM_Oth) * pNV%Area + (pRV%SoilM_Precip + pRV%SoilM_AW + pRV%SoilM_Oth)*pRV%Area
        RSoilM(1:NRegions,f_iNVIndex) = AppGrid%AccumElemValuesToSubregions(ElemValues)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: LUIndex
    REAL(8),INTENT(OUT)                :: RETp(:)
    
    !Local variables
    INTEGER :: NRegions
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    SELECT CASE(LUIndex)
        !Agricultural lands
        CASE (f_iAgIndex)
            RETp = 0.0
            IF (RootZone%Flags%lNonPondedAg_Defined) RETp(1:NRegions) = SUM(RootZone%NonPondedAgRootZone%RegionETPot , DIM=1)
            IF (RootZone%Flags%lPondedAg_Defined)    RETp(1:NRegions) = RETp(1:NRegions) + SUM(RootZone%PondedAgRootZone%RegionETPot , DIM=1)

        !Urban
        CASE (f_iUrbIndex)
            IF (RootZone%Flags%lUrban_Defined) THEN
                RETp(1:NRegions) = RootZone%UrbanRootZone%RegionETPot
            ELSE
                RETp = 0.0
            END IF
            
        !Native and riparian
        CASE (f_iNVIndex)
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
    TYPE(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: iLandUseType
    REAL(8),INTENT(OUT)                :: InRunoff(:,:)
                                     
    !Local variables
    INTEGER :: indxElem,NElements
    REAL(8) :: ElemArea(AppGrid%NElements)
    
    !Initialize
    NElements = AppGrid%NElements
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(indxElem) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,100)
    DO indxElem=1,NElements
        ElemArea(indxElem) = AppGrid%AppElement(indxElem)%Area
    END DO
    !$OMP END DO
    
    SELECT CASE (iLandUseType)
        !Flow from upstream runoff to non-ponded crops
        CASE (f_iLandUse_NonPonded)
            !$OMP DO SCHEDULE(STATIC,100)
            DO indxElem=1,NElements
                InRunoff(:,indxElem) = UpstrmElemRunoff(indxElem) * RootZone%NonPondedAgRootZone%Crops(:,indxElem)%Area / ElemArea(indxElem)  
            END DO
            !$OMP END DO
        
        !Flow from upstream runoff to ponded crops
        CASE (f_iLandUse_Ponded)
            !$OMP DO SCHEDULE(STATIC,100)
            DO indxElem=1,NElements
                InRunoff(:,indxElem) = UpstrmElemRunoff(indxElem) * RootZone%PondedAgRootZone%Crops(:,indxElem)%Area / ElemArea(indxElem)       
            END DO
            !$OMP END DO
        
        !Flow from upstream runoff to urban lands
        CASE (f_iLandUse_Urban)
            !$OMP DO SCHEDULE(STATIC,100)
            DO indxElem=1,NElements
                InRunoff(1,indxElem) = UpstrmElemRunoff(indxElem) * RootZone%UrbanRootZone%UrbData(indxElem)%Area / ElemArea(indxElem) 
            END DO
            !$OMP END DO
                                       
        !Flow from upstream runoff to native and riparian vegetation lands
        CASE (f_iLandUse_NVRV)
            !$OMP DO SCHEDULE(STATIC,100)
            DO indxElem=1,NElements
                InRunoff(1,indxElem) = UpstrmElemRunoff(indxElem) * (RootZone%NVRVRootZone%NativeVeg(indxElem)%Area+RootZone%NVRVRootZone%RiparianVeg(indxElem)%Area) / ElemArea(indxElem) 
            END DO
            !$OMP END DO
          
    END SELECT
    !$OMP END PARALLEL
    
  END SUBROUTINE ComputeUpstrmElemRunoffToLandUse
                                      
  
  ! -------------------------------------------------------------
  ! --- COMPUTE DEMAND AND SUPPLY RELATED FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE ComputeDemandSupplyRelatedFracs(AppGrid,RootZone)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(RootZone_v40_Type)      :: RootZone
    
    !Local variables
    INTEGER :: indxElem,NElements,NCrops,indxRegion,iElem
    REAL(8) :: ElemDemand_Ag(AppGrid%NElements),ElemDemandFrac(RootZone%NLands-2),              &
               RegionDemandArea,RegionArea,ElemDemand,RegionalDemand_Ag(AppGrid%NSubregions+1), &
               RegionalDemand_Urb(AppGrid%NSubregions+1),rElemAgArea(RootZone%NLands-3),        &
               ElemDemandFrac_Ag
   
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
          IF (pFlags%lNonPondedAg_Defined) ElemDemandFrac(1:NCrops)                        = pCrops(:,indxElem)%Area
          IF (pFlags%lPondedAg_Defined)    ElemDemandFrac(NCrops+1:NCrops+f_iNPondedCrops) = pPondedCrops(:,indxElem)%Area
          IF (pFlags%lUrban_Defined)       ElemDemandFrac(NCrops+f_iNPondedCrops+1)        = pUrban(indxElem)%Area
        ELSE
          !Use demand fractions if total demand is greater than zero
          IF (pFlags%lNonPondedAg_Defined) ElemDemandFrac(1:NCrops)                        = pCrops(:,indxElem)%Demand
          IF (pFlags%lPondedAg_Defined)    ElemDemandFrac(NCrops+1:NCrops+f_iNPondedCrops) = pPondedCrops(:,indxElem)%Demand
          IF (pFlags%lUrban_Defined)       ElemDemandFrac(NCrops+f_iNPondedCrops+1)        = pUrban(indxElem)%Demand
        END IF
        CALL NormalizeArray(ElemDemandFrac)
        ElemDemandFrac_Ag = SUM(ElemDemandFrac(1:NCrops+f_iNPondedCrops)) 
        IF (pFlags%lNonPondedAg_Defined) pCrops(:,indxElem)%ElemDemandFrac       = ElemDemandFrac(1:NCrops)
        IF (pFlags%lPondedAg_Defined)    pPondedCrops(:,indxElem)%ElemDemandFrac = ElemDemandFrac(NCrops+1:NCrops+f_iNPondedCrops) 
        IF (pFlags%lUrban_Defined)       pUrban(indxElem)%ElemDemandFrac         = ElemDemandFrac(NCrops+f_iNPondedCrops+1)
        IF (ElemDemandFrac_Ag .EQ. 0.0) THEN
            rElemAgArea = 0.0
            IF (pFlags%lNonPondedAg_Defined) rElemAgArea(1:NCrops)                     = pCrops(:,indxElem)%Area
            IF (pFlags%lPondedAg_Defined) rElemAgArea(NCrops+1:NCrops+f_iNPondedCrops) = pPondedCrops(:,indxElem)%Area
            IF (SUM(rElemAgArea) .EQ. 0.0) THEN
                IF (pFlags%lNonPondedAg_Defined) pCrops(:,indxElem)%ElemDemandFrac_Ag       = 0.0
                IF (pFlags%lPondedAg_Defined)    pPondedCrops(:,indxElem)%ElemDemandFrac_Ag = 0.0
            ELSE
                CALL NormalizeArray(rElemAgArea)
                IF (pFlags%lNonPondedAg_Defined) pCrops(:,indxElem)%ElemDemandFrac_Ag       = rElemAgArea(1:NCrops)
                IF (pFlags%lPondedAg_Defined)    pPondedCrops(:,indxElem)%ElemDemandFrac_Ag = rElemAgArea(NCrops+1:NCrops+f_iNPondedCrops)
            END IF 
        ELSE
            IF (pFlags%lNonPondedAg_Defined) pCrops(:,indxElem)%ElemDemandFrac_Ag       = ElemDemandFrac(1:NCrops) / ElemDemandFrac_Ag
            IF (pFlags%lPondedAg_Defined)    pPondedCrops(:,indxElem)%ElemDemandFrac_Ag = ElemDemandFrac(NCrops+1:NCrops+f_iNPondedCrops) / ElemDemandFrac_Ag
        END IF
      END DO
      
      !Compute fractions to distribute regional supply to elements
      IF (pFlags%lNonPondedAg_Defined  .OR. pFlags%lPondedAg_Defined) THEN
        RegionalDemand_Ag  = RegionalDemand(AppGrid,RootZone,f_iAgIndex)
      ELSE
        RegionalDemand_Ag = 0.0
      END IF
      IF (pFlags%lUrban_Defined) THEN
        RegionalDemand_Urb = RegionalDemand(AppGrid,RootZone,f_iUrbIndex)
      ELSE
        RegionalDemand_Urb = 0.0
      END IF  
      DO indxRegion=1,AppGrid%NSubregions
        !Ag related information
        !----------------------
        !No ag water demand in the region
        IF (RegionalDemand_Ag(indxRegion) .EQ. 0.0) THEN
          RegionDemandArea = RegionalLUArea_ForSingleRegion(indxRegion,AppGrid,RootZone,f_iAgIndex)
              
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
          RegionDemandArea = RegionalLUArea_ForSingleRegion(indxRegion,AppGrid,RootZone,f_iUrbIndex)
              
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
  SUBROUTINE RootZone_v40_ZeroSupply(RootZone)
    CLASS(RootZone_v40_Type) :: RootZone
    
    !Inform user
    CALL EchoProgress('Resetting water supply to elements')
    
    ASSOCIATE (pElemSupply => RootZone%ElemSupply)
      pElemSupply%Diversion_Ag  = 0.0
      pElemSupply%Diversion_Urb = 0.0
      pElemSupply%Pumping_Ag    = 0.0
      pElemSupply%Pumping_Urb   = 0.0
      pElemSupply%UpstrmRunoff  = 0.0
    END ASSOCIATE
    
  END SUBROUTINE RootZone_v40_ZeroSupply


  ! -------------------------------------------------------------
  ! --- ZERO OUT SURFACE FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_ZeroSurfaceFlows(RootZone)
    CLASS(RootZone_v40_Type) :: RootZone
    
    !Inform user
    CALL EchoProgress('Resetting rainfall runoff and return flow from elements')
    
    !Zero out surface flows from non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        RootZone%NonPondedAgRootZone%Crops%Runoff     = 0.0
        RootZone%NonPondedAgRootZone%Crops%ReturnFlow = 0.0
    END IF
    
    !Zero out surface flows from ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        RootZone%PondedAgRootZone%Crops%Runoff     = 0.0
        RootZone%PondedAgRootZone%Crops%ReturnFlow = 0.0
    END IF

    !Zero out surface flows from urban lands
    IF (RootZone%Flags%lUrban_Defined) THEN
        RootZone%UrbanRootZone%UrbData%Runoff     = 0.0
        RootZone%UrbanRootZone%UrbData%ReturnFlow = 0.0
    END IF

    !Zero out surface flows from native and riparian veg lands
    IF (RootZone%Flags%lNVRV_Defined) THEN
        RootZone%NVRVRootZone%NativeVeg%Runoff   = 0.0
        RootZone%NVRVRootZone%RiparianVeg%Runoff = 0.0
    END IF
  
  END SUBROUTINE RootZone_v40_ZeroSurfaceFlows


  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF ROOT ZONE RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_ConvertTimeUnit(RootZone,NewUnit)
    CLASS(RootZone_v40_Type)    :: RootZone
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
    
  END SUBROUTINE RootZone_v40_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- ZERO OUT SOIL OISTURE VARIABLES RELATED TO LAND USE AREA CHANGE
  ! -------------------------------------------------------------
  SUBROUTINE ZeroRedistributedMoist(RootZone)
    TYPE(RootZone_v40_Type) :: RootZone
    
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
    TYPE(RootZone_v40_Type) :: RootZone

    !Local variables
    INTEGER                                        :: indxElem,indxLU,indxUrban_In,indxUrban_Out,indxNV, &
                                                      indxRV,NCrops,NLandsExt
    REAL(8)                                        :: ratio(3),SOILM_INT_Precip,SOILM_INT_AW,SOILM_INT_Oth,TotalReduc,  &
                                                      Factor_Precip,Factor_AW,Factor_Oth,TotalPorosity,PerviousFrac,    &
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
    indxUrban_In         = NCrops + f_iNPondedCrops + 1
    indxUrban_Out        = NCrops + f_iNPondedCrops + 2
    indxNV               = NCrops + f_iNPondedCrops + 3
    indxRV               = NCrops + f_iNPondedCrops + 4
    Area                 = 0.0
    Area_P               = 0.0
    SM_Precip            = 0.0
    SM_AW                = 0.0
    SM_Oth               = 0.0
    AreaExpand           = 0.0
    AreaReduced          = 0.0
    RootDepth            = 0.0   ;   IF (lNonPondedAg_Defined) RootDepth(1:NCrops) = RootZone%NonPondedAgRootZone%RootDepth
                                     IF (lPondedAg_Defined) RootDepth(NCrops+1:NCrops+f_iNPondedCrops) = RootZone%PondedAgRootZone%RootDepth
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
          !Cycle if lake element
          IF (RootZone%Flags%lLakeElems(indxElem)) THEN
              SoilM_Precip(:,indxElem)   = 0.0
              SoilM_AW(:,indxElem)       = 0.0
              SoilM_Oth(:,indxElem)      = 0.0
              SoilMCh_Precip(:,indxElem) = 0.0
              SoilMCh_AW(:,indxElem)     = 0.0
              SoilMCh_Oth(:,indxElem)    = 0.0
              PercCh(:,indxElem)         = 0.0
              CYCLE
          END IF  
        
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
            Area(NCrops+1:NCrops+f_iNPondedCrops)      = pPondedCrops(:,indxElem)%Area
            Area_P(NCrops+1:NCrops+f_iNPondedCrops)    = pPondedCrops(:,indxElem)%Area_P
            SM_Precip(NCrops+1:NCrops+f_iNPondedCrops) = pPondedCrops(:,indxElem)%SoilM_Precip_P
            SM_AW(NCrops+1:NCrops+f_iNPondedCrops)     = pPondedCrops(:,indxElem)%SoilM_AW_P
            SM_Oth(NCrops+1:NCrops+f_iNPondedCrops)    = pPondedCrops(:,indxElem)%SoilM_Oth_P
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
            SoilMCH_Oth(:,indxElem)    = 0.0
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
              PercCh(indxLU,indxElem)   = PercCh(indxLU,indxElem) / DeltaT * Area(indxLU)  !Convert perc due to area chnage to volumetric rate
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
        pPondedCrops%SoilM_Precip = SoilM_Precip(NCrops+1:NCrops+f_iNPondedCrops,:) 
        pPondedCrops%SoilM_AW     = SoilM_AW(NCrops+1:NCrops+f_iNPondedCrops,:)
        pPondedCrops%SoilM_Oth    = SoilM_Oth(NCrops+1:NCrops+f_iNPondedCrops,:)
        pPondedCrops%SoilMCh      = SoilmCh_Precip(NCrops+1:NCrops+f_iNPondedCrops,:) + SoilMCh_AW(NCrops+1:NCrops+f_iNPondedCrops,:) + SoilMCh_Oth(NCrops+1:NCrops+f_iNPondedCrops,:)
        pPondedCrops%PercCh       = PercCh(NCrops+1:NCrops+f_iNPondedCrops,:)
      END IF
      IF (lUrban_Defined) THEN
          !Consolidate urban values to urban outdoors
          WHERE (pUrban%PerviousFrac .GT. 0.0) 
              pUrban%SoilM_Precip = SoilM_Precip(indxUrban_Out,:) + SoilM_Precip(indxUrban_In,:) * (1d0/pUrban%PerviousFrac - 1d0) 
              pUrban%SoilM_AW     = SoilM_AW(indxUrban_Out,:) + SoilM_AW(indxUrban_In,:) * (1d0/pUrban%PerviousFrac - 1d0) 
              pUrban%SoilM_Oth    = SoilM_Oth(indxUrban_Out,:) + SoilM_Oth(indxUrban_In,:) * (1d0/pUrban%PerviousFrac - 1d0) 
          END WHERE 
          pUrban%SoilMCh = SoilmCh_Precip(indxUrban_Out,:) + SoilmCh_Precip(indxUrban_In,:) + SoilmCh_AW(indxUrban_Out,:) + SoilmCh_AW(indxUrban_In,:) + SoilmCh_Oth(indxUrban_Out,:) + SoilmCh_Oth(indxUrban_In,:) 
          pUrban%PercCh  = PercCh(indxUrban_Out,:) + PercCh(indxUrban_In,:)
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
    TYPE(RootZone_v40_Type) :: RootZone
    LOGICAL,INTENT(IN)      :: lAdvanceArea
    
    !Local variables
    INTEGER :: NElements,indxElem,indxLU
    
    !Initialize
    NElements = SIZE(RootZone%ElemSoilsData)
    
    RootZone%RSoilM_P = RootZone%RSoilM
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NElements,RootZone,lAdvanceArea) NUM_THREADS(OMP_GET_MAX_THREADS()-1) 
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            DO indxLU=1,RootZone%NonPondedAgRootZone%NCrops
                RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip_P = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip
                RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW_P     = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW
                RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth_P    = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth
                IF (lAdvanceArea)  &
                    RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%Area_P     = RootZone%NonPondedAgRootZone%Crops(indxLU,indxElem)%Area
            END DO
        END DO
        !$OMP END DO NOWAIT
    END IF
    
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            DO indxLU=1,f_iNPondedCrops
                RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip_P    = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Precip
                RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW_P        = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_AW
                RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth_P       = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%SoilM_Oth
                IF (lAdvanceArea)  &
                    RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%Area_P        = RootZone%PondedAgRootZone%Crops(indxLU,indxElem)%Area
           END DO
        END DO
        !$OMP END DO NOWAIT
    END IF
    
    IF (RootZone%Flags%lUrban_Defined) THEN
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Precip_P     = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Precip
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_AW_P         = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_AW
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Oth_P        = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Oth
            IF (lAdvanceArea)  &
                 RootZone%UrbanRootZone%UrbData(indxElem)%Area_P        = RootZone%UrbanRootZone%UrbData(indxElem)%Area
       END DO
       !$OMP END DO NOWAIT
    END IF
    
    IF (RootZone%Flags%lNVRV_Defined) THEN
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Precip_P    = RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Precip
            RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_AW_P        = RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_AW
            RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Oth_P       = RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Oth
            RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Precip_P  = RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Precip
            RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_AW_P      = RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_AW
            RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Oth_P     = RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Oth
            IF (lAdvanceArea) THEN
                RootZone%NVRVRootZone%NativeVeg(indxElem)%Area_P        = RootZone%NVRVRootZone%NativeVeg(indxElem)%Area
                RootZone%NVRVRootZone%RiparianVeg(indxElem)%Area_P      = RootZone%NVRVRootZone%RiparianVeg(indxElem)%Area
            END IF
        END DO
       !$OMP END DO NOWAIT
    END IF
    !$OMP END PARALLEL

  END SUBROUTINE AdvanceStateLocal
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF ROOT ZONE IN TIME INCLUDING AREA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_AdvanceState(RootZone)
    CLASS(RootZone_v40_Type) :: RootZone
    
    !Local variables
    INTEGER :: indxElem,indxCrop,NElements
    
    !Initialize
    NElements = SIZE(RootZone%ElemSoilsData)
    
    !Store previous moisture stoarge in special arrays before they may be updated
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(NElements,RootZone) NUM_THREADS(OMP_GET_MAX_THREADS()-1)
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        !$OMP SINGLE
        RootZone%NonPondedAgRootZone%RootDepth_P = RootZone%NonPondedAgRootZone%RootDepth
        !$OMP END SINGLE
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            DO indxCrop=1,RootZone%NonPondedAgRootZone%NCrops
                RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Precip_P_BeforeUpdate = RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Precip
                RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_AW_P_BeforeUpdate     = RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_AW
                RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Oth_P_BeforeUpdate    = RootZone%NonPondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Oth
            END DO
        END DO
        !$OMP END DO NOWAIT
    END IF
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            DO indxCrop=1,RootZone%PondedAgRootZone%NCrops
                RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Precip_P_BeforeUpdate = RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Precip
                RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_AW_P_BeforeUpdate     = RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_AW
                RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Oth_P_BeforeUpdate    = RootZone%PondedAgRootZone%Crops(indxCrop,indxElem)%SoilM_Oth
            END DO
        END DO
        !$OMP END DO NOWAIT
    END IF
    IF (RootZone%Flags%lUrban_Defined) THEN
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Precip_P_BeforeUpdate = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Precip
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_AW_P_BeforeUpdate     = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_AW
            RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Oth_P_BeforeUpdate    = RootZone%UrbanRootZone%UrbData(indxElem)%SoilM_Oth
        END DO
        !$OMP END DO NOWAIT
    END IF
    IF (RootZone%Flags%lNVRV_Defined) THEN
        !$OMP DO SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Precip_P_BeforeUpdate   = RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Precip
            RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_AW_P_BeforeUpdate       = RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_AW
            RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Oth_P_BeforeUpdate      = RootZone%NVRVRootZone%NativeVeg(indxElem)%SoilM_Oth
            RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Precip_P_BeforeUpdate = RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Precip
            RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_AW_P_BeforeUpdate     = RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_AW
            RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Oth_P_BeforeUpdate    = RootZone%NVRVRootZone%RiparianVeg(indxElem)%SoilM_Oth
        END DO
        !$OMP END DO NOWAIT
    END IF
    !$OMP END PARALLEL

    CALL AdvanceStateLocal(RootZone,lAdvanceArea=.TRUE.)
    
  END SUBROUTINE RootZone_v40_AdvanceState

  
  ! -------------------------------------------------------------
  ! --- DISTRIBUTE A FLOW DESTINED TO ELEMENT OR SUBREGION TO ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE FlowToElements(Flow,AppGrid,RootZone,SupplyToLUIndex,ToElements)
    REAL(8),INTENT(IN)                         :: Flow(:)
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    TYPE(RootZone_v40_Type),INTENT(IN)         :: RootZone
    INTEGER,INTENT(IN)                         :: SupplyToLUIndex
    REAL(8)                                    :: ToElements(:)
    
    !Local variables
    INTEGER :: indx,iDest,indxElem,iElem,iElemRegion
    REAL(8) :: rFlow,rFraction
    
    !First process flows to elements
    DO indx=1,SIZE(RootZone%ElemFlowToElements)
        iElem             = RootZone%ElemFlowToElements(indx)%iElement
        iDest             = RootZone%ElemFlowToElements(indx)%iDest
        ToElements(iDest) = ToElements(iDest) + Flow(iElem)
    END DO
    
    !Then process flows to subregions
    DO indx=1,SIZE(RootZone%ElemFlowToSubregions)
        iElem = RootZone%ElemFlowToSubregions(indx)%iElement
        rFlow = Flow(iElem)
        IF (rFlow .EQ. 0.0) CYCLE
        iDest = RootZone%ElemFlowToSubregions(indx)%iDest
        DO indxElem=1,AppGrid%AppSubregion(iDest)%NRegionElements
            iElemRegion = AppGrid%AppSubregion(iDest)%RegionElements(indxElem)
            SELECT CASE (SupplyToLUIndex)
              !Ag supply in element
              CASE (f_iAgIndex)
                ToElements(iElemRegion) = ToElements(iElemRegion) + RootZone%Ratio_ElemSupplyToRegionSupply_Ag(iElemRegion) * rFlow
              !Urban supply in element
              CASE (f_iUrbIndex)
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
  FUNCTION RootZone_v40_RegionalPerc(RootZone,AppGrid) RESULT(RPERC)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
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

  END FUNCTION RootZone_v40_RegionalPerc
  

  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_RegionalReturnFlow_Ag(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
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
      
  END SUBROUTINE RootZone_v40_RegionalReturnFlow_Ag
  

  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v40_RegionalReturnFlow_Urb(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
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
      
  END SUBROUTINE RootZone_v40_RegionalReturnFlow_Urb
  

  ! -------------------------------------------------------------
  ! --- PROCESS LAND USE AREA
  ! -------------------------------------------------------------
  SUBROUTINE ProcessLandUseAreas(AppGrid,TimeStep,RootZone,iStat)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(RootZone_v40_Type)       :: RootZone
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'ProcessLandUseAreas'
    INTEGER                      :: indxElem,NNonPondCrops,NAllCrops,NElements,iElemID
    REAL(8)                      :: LUArea(RootZone%NLands,AppGrid%NElements)
    
    !Initialize
    iStat         = 0
    NElements     = AppGrid%NElements
    NNonPondCrops = RootZone%NonPondedAgRootZone%NCrops
    NAllCrops     = NNonPondCrops + f_iNPondedCrops
    
    !Zero out the variables that hold information regarding soil moisture change due to land area change
    CALL ZeroRedistributedMoist(RootZone) 
    
    !Return if new data is not read
    IF (.NOT. RootZone_v40_IsLandUseUpdated(RootZone)) RETURN
        
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
          iElemID = AppGrid%AppElement(indxElem)%ID
          CALL SetLastMessage('Total land use area is zero at element ' // TRIM(IntToText(iElemID)) // '!',f_iFatal,ThisProcedure)
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
      CALL pNonPondedAg%SetAreas(LUArea(1:NNonPondCrops,:))
      CALL pPondedAg%SetAreas(LUArea(NNonPondCrops+1:NAllCrops,:)) 
      CALL pUrban%SetAreas(LUArea(NAllCrops+1,:)) 
      CALL pNVRV%SetAreas(LUArea(NAllCrops+2:NAllCrops+3,:))
    
      !If first time step, do the advancement of land use areas in time again since previous one had no effect
      IF (TimeStep%CurrentTimeStep .EQ. 1) THEN
        CALL pNonPondedAg%AdvanceAreas()
        CALL pPondedAg%AdvanceAreas() 
        CALL pUrban%AdvanceAreas() 
        CALL pNVRV%AdvanceAreas() 
 
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
  SUBROUTINE CheckTSDataPointers(RootZone,iElemIDs,Precip,ET,iStat)
    CLASS(RootZone_v40_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: iElemIDs(:)
    TYPE(PrecipitationType),INTENT(IN)  :: Precip
    TYPE(ETType),INTENT(IN)             :: ET
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'CheckTSDataPointers'
    INTEGER                      :: iElem(1),iETColMax,indxElem,iCrop(1),iReturnFlowCol(1),iReuseCol(1),iElemID
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
          iElem   = MAXLOC(pElemPrecipData%iColPrecip)
          iElemID = iElemIDs(iElem(1))
          MessageArray(1) = 'Precipitation data column for element '//TRIM(IntToText(iElemID))//' in the root zone component'
          MessageArray(2) = 'is greater than the available data columns in the Precipitation Data file!'
          CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
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
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(iElemIDs(indxElem)))//' and non-ponded crop '//TRIM(pNonPondedAg%CropCodes(iCrop(1)))
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
              CALL RootZone%ReturnFracFile%CheckColNum('Return flow fractions data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',pNonPondedAg%Crops(:,indxElem)%iColReturnFrac,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%ReuseFracFile%CheckColNum('Re-use fractions data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',pNonPondedAg%Crops(:,indxElem)%iColReuseFrac,.TRUE.,iStat)         ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%IrigPeriodFile%CheckColNum('Irrigation periods data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',pNonPondedAg%Crops(:,indxElem)%iColIrigPeriod,.TRUE.,iStat)     ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%AgWaterDemandFile%CheckColNum('Agricultural demand data file (referenced by non-ponded crop data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',pNonPondedAg%iColAgDemand(:,indxElem),.FALSE.,iStat)        ;  IF (iStat .EQ. -1) RETURN
          END IF
          
          !Ponded crops
          IF (lPondedAg_Defined) THEN
              IF (iETColMax .LT. MAXVAL(pPondedAg%Crops(:,indxElem)%iColETc)) THEN
                  iCrop = MAXLOC(pPondedAg%Crops(:,indxElem)%iColETc)
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(iElemIDs(indxElem)))//' and ponded crop '//TRIM(pPondedAg%CropCodes(iCrop(1)))
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
              CALL RootZone%IrigPeriodFile%CheckColNum('Irrigation periods data file (referenced by ponded crop data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',pPondedAg%Crops(:,indxElem)%iColIrigPeriod,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%AgwaterDemandFile%CheckColNum('Agricultural demand data file (referenced by ponded crop data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',pPondedAg%iColAgDemand(:,indxElem),.FALSE.,iStat)     ;  IF (iStat .EQ. -1) RETURN
          END IF
          
          !Urban
          IF (lUrban_Defined) THEN
              IF (iETColMax .LT. pUrban%UrbData(indxElem)%iColETc) THEN
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(iElemIDs(indxElem)))//' at urban lands '
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF 
              iReturnFlowCol(1) = pUrban%UrbData(indxElem)%iColReturnFrac
              iReuseCol(1)      = pUrban%UrbData(indxElem)%iColReuseFrac
              CALL RootZone%ReturnFracFile%CheckColNum('Return flow fractions data file (referenced by urban data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',iReturnFlowCol,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
              CALL RootZone%ReuseFracFile%CheckColNum('Re-use fractions data file (referenced by urban data file for element '//TRIM(IntToText(iElemIDs(indxElem)))//')',iReuseCol,.TRUE.,iStat)             ;  IF (iStat .EQ. -1) RETURN
          END IF
          
          !Native 
          IF (lNVRV_Defined) THEN
              IF (iETColMax .LT. pNVRV%NativeVeg(indxElem)%iColETc) THEN
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(iElemIDs(indxElem)))//' at native vegetation lands '
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF 
              
          !Riparian 
              IF (iETColMax .LT. pNVRV%RiparianVeg(indxElem)%iColETc) THEN
                  MessageArray(1) = 'Evapotranspiration data column for element '//TRIM(IntToText(iElemIDs(indxElem)))//' at riparian vegetation lands '
                  MessageArray(2) = 'is greater than the available data columns in the Evapotranspiration Data file!'
                  CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
          END IF   
        END DO
                
    END ASSOCIATE
    
  END SUBROUTINE CheckTSDataPointers

  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO  A TIMESTAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(RootZone,iElemIDs,rElemAreas,TimeStep,iStat)
    TYPE(RootZone_v40_Type)       :: RootZone
    INTEGER,INTENT(IN)            :: iElemIDs(:)
    REAL(8),INTENT(IN)            :: rElemAreas(:) 
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    LOGICAL :: lUpdated
    
    !Rewind return flow fraction file
    IF (RootZone%ReturnFracFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL RootZone%ReturnFracFile%File%RewindFile_To_BeginningOfTSData(iStat)       ;  IF (iStat .NE. 0) RETURN  
        CALL ReadReturnFlowFractions(TimeStep,RootZone%ReturnFracFile,lUpdated,iStat)  ;  IF (iStat .NE. 0) RETURN        
    END IF

    !Rewind reuse fraction file
    IF (RootZone%ReuseFracFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL RootZone%ReuseFracFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL ReadReuseFractions(TimeStep,RootZone%ReuseFracFile,lUpdated,iStat)  ;  IF (iStat .NE. 0) RETURN
    END IF

    !Rewind irrigation period file
    IF (RootZone%IrigPeriodFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL RootZone%IrigPeriodFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL ReadIrigPeriodData(TimeStep,RootZone%IrigPeriodFile,lUpdated,iStat)  ;  IF (iStat .NE. 0) RETURN
    END IF
    
    !Rewind generic moisture file
    IF (RootZone%GenericMoistureData%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL RootZone%GenericMoistureData%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL RootZone%GenericMoistureData%ReadTSData(TimeStep,iStat)                   ;  IF (iStat .NE. 0) RETURN
    END IF

    !Rewind ag water demand file
    IF (RootZone%AgWaterDemandFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL RootZone%AgWaterDemandFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL ReadAgWaterDemand(TimeStep,RootZone%AgWaterDemandFile,lUpdated,iStat)   ;  IF (iStat .NE. 0) RETURN
        IF (lUpdated) RootZone%AgWaterDemandFile%rValues = RootZone%AgWaterDemandFile%rValues * RootZone%AgWaterDemandFactor
    END IF

    !Rewind for non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        CALL RootZone%NonPondedAgRootZone%RewindTSInputFilesToTimeStamp(iElemIDs,rElemAreas,TimeStep,iStat)
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Rewind for ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        CALL RootZone%PondedAgRootZone%RewindTSInputFilesToTimeStamp(iElemIDs,rElemAreas,TimeStep,iStat)
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Rewind for urban
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%UrbanRootZone%RewindTSInputFilesToTimeStamp(iElemIDs,rElemAreas,TimeStep,iStat)
        IF (iStat .NE. 0) RETURN
    END IF
    
    !Rewind for NVRV
    IF (RootZone%Flags%lNVRV_Defined) THEN
        CALL RootZone%NVRVRootZone%RewindTSInputFilesToTimeStamp(iElemIDs,rElemAreas,TimeStep,iStat)
        IF (iStat .NE. 0) RETURN
    END IF   
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp
    
END MODULE