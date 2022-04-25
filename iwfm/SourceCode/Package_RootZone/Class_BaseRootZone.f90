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
MODULE Class_BaseRootZone
  !$ USE OMP_LIB
  USE Class_Version               , ONLY: VersionType
  USE MessageLogger               , ONLY: SetLastMessage                       , &
                                          f_iFatal                               
  USE IOInterface                 , ONLY: GenericFileType                      
  USE TimeSeriesUtilities         , ONLY: TimeStepType                         , &
                                          TimeStampToJulian                    , &
                                          OPERATOR(.TSGE.)                     , &  
                                          f_iTimeStampLength
  USE GeneralUtilities            , ONLY: NormalizeArray                       , &
                                          LocateInList                         , &
                                          IntToText                            
  USE GenericLinkedList           , ONLY: GenericLinkedListType                
  USE Package_Misc                , ONLY: RealTSDataInFileType                 , &
                                          f_iRootZoneComp                      , &
                                          f_iLocationType_Subregion            , &
                                          f_iFlowDest_Outside                  , &
                                          f_iFlowDest_StrmNode                 , &
                                          f_iFlowDest_Element                  , &
                                          f_iFlowDest_Subregion                , &
                                          f_iFlowDest_Lake                     , &
                                          f_iFlowDest_GWElement                , &
                                          f_iAg                                , &
                                          f_iUrb                               
  USE Package_Budget              , ONLY: BudgetType                           
  USE Package_Discretization      , ONLY: AppGridType                          
  USE Package_PrecipitationET     , ONLY: PrecipitationType                    , &
                                          ETType                               
  USE Package_ComponentConnectors , ONLY: SupplyDestinationConnectorType       
  USE Package_ZBudget             , ONLY: ZBudgetType                          , &
                                          ZoneListType                         
  USE Util_Package_RootZone       , ONLY: f_iBudgetType_LWU                    , &
                                          f_iBudgetType_RootZone               , &
                                          f_cDescription_LWUBudget             , &
                                          f_cDescription_RootZoneBudget        
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: BaseRootZoneType                        , &
            FlagsType                               , &
            ElemSurfaceFlowToDestType               , &
            TrackMoistureDueToSource                , &
            CompileElemSurfaceFlowToDestinationList , &
            ComputeRegionalETPot                    , &
            CalculateUrbanFracDemand                , &
            ElementLU_InterpolateExtrapolate        , &
            iMeasuredLUDataForSubregion             , &
            iMeasuredLUDataForModelDomain           
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS USED FOR ELEMENT LAND USE INTERPLOATION
  ! -------------------------------------------------------------
   INTEGER,PARAMETER :: iMeasuredLUDataForSubregion   = 0 , &
                        iMeasuredLUDataForModelDomain = 1
   
   
  ! -------------------------------------------------------------
  ! --- FLAGS DATA TYPE
  ! -------------------------------------------------------------
  TYPE FlagsType
    LOGICAL             :: lNonPondedAg_Defined           = .FALSE. !Flag to show if non-ponded ag is simulated
    LOGICAL             :: lPondedAg_Defined              = .FALSE. !Flag to show if rice and refuge is simulated
    LOGICAL             :: lUrban_Defined                 = .FALSE. !Flag to show if urban lands are simulated
    LOGICAL             :: lNVRV_Defined                  = .FALSE. !Flag to show if native and riparian veg are simulated
    LOGICAL             :: lGenericMoistureFile_Defined   = .FALSE. !Flag that shows if generic source of moisture file is defined
    LOGICAL             :: lReadNonPondedAgWaterDemand    = .FALSE. !Flag that shows if non-ponded ag water demand will be read from file
    LOGICAL             :: lReadPondedAgWaterDemand       = .FALSE. !Flag that shows if ponded ag water demand will be read from file
    LOGICAL             :: LWUseBudRawFile_Defined        = .FALSE. !Flag to see if raw land and water use budget output file is defined
    LOGICAL             :: RootZoneBudRawFile_Defined     = .FALSE. !Flag to see if raw root zone budget output file is defined
    LOGICAL             :: LWUseZoneBudRawFile_Defined    = .FALSE. !Flag to see if raw land and water use zone budget output file is defined
    LOGICAL             :: RootZoneZoneBudRawFile_Defined = .FALSE. !Flag to see if raw root zone zone budget output file is defined
    LOGICAL             :: FinalMoistureOutFile_Defined   = .FALSE. !Flag to see if final moisture output file is defined
    LOGICAL             :: lMoistureContentToDepth        = .FALSE. !Flag to check if initial soil moisture content is already converted into depth (used when REadTSData method is called more than once for the same timestep; e.g. when iterated with another software) 
    LOGICAL             :: lComputeETFromGW               = .FALSE. !Flag to see if root water uptake from groundwater will be simulated (special to v41)
    LOGICAL             :: lAg_Defined                    = .FALSE. !Flag to show if ag lands are simulated (special to v50)
    LOGICAL,ALLOCATABLE :: lLakeElems(:)                            !Flag that specifies if an element is lake element
  END TYPE FlagsType
  
  
  ! -------------------------------------------------------------
  ! --- ELEMENTAL PRECIPITATION DATA TYPE
  ! -------------------------------------------------------------
  TYPE ElemPrecipDataType
    INTEGER :: iColPrecip          =  0      !Column number in the precipitation data file
    REAL(8) :: PrecipFactor        =  1.0    !Factor to multiply the precip in column iColPrecip to end up with the precip for an element
    REAL(8) :: Precip              =  0.0    !Elemental precipitation  
  END TYPE ElemPrecipDataType  
  
  
  ! -------------------------------------------------------------
  ! --- ELEMENT SURFACE FLOW TO DESTINATION CONNECTION TYPE
  ! -------------------------------------------------------------
  TYPE ElemSurfaceFlowToDestType
      INTEGER :: iElement = 0    !Element index from which surface flow originates
      INTEGER :: iDest    = 0    !Destination index to which surface flow contributes
  END TYPE ElemSurfaceFlowToDestType
  

  ! -------------------------------------------------------------
  ! --- OVERLOADED METHOD(S)
  ! -------------------------------------------------------------
  INTERFACE ComputeRegionalETPot
    MODULE PROCEDURE ComputeRegionalETPot_MultipleLUPerElem
    MODULE PROCEDURE ComputeRegionalETPot_SingleLUPerElem
  END INTERFACE ComputeRegionalETPot
  

  ! -------------------------------------------------------------
  ! --- ABSTRACT BASE ROOT ZONE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: BaseRootZoneType
      TYPE(VersionType)                             :: Version                            !Root zone component version number
      CHARACTER(LEN=6)                              :: VarTimeUnit            = ''        !Time unit of rate-type variables
      TYPE(FlagsType)                               :: Flags                              !Flags that affect the simulation of root zone
      TYPE(ElemPrecipDataType),ALLOCATABLE          :: ElemPrecipData(:)                  !Precipitation data at each element
      INTEGER,ALLOCATABLE                           :: ElemFlowToOutside(:)               !Element surface runoff to outside of model domain connection data set
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE   :: ElemFlowToStreams(:)               !Element surface runoff to streams connection data set
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE   :: ElemFlowToLakes(:)                 !Element surface runoff to lakes connection data set
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE   :: ElemFlowToSubregions(:)            !Element surface runoff to subregions connection data set
      INTEGER,ALLOCATABLE                           :: ElemFlowToGW(:)                    !Element surface runoff to groundwater (at the same element) connection data set
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE   :: ElemFlowToElements(:)              !Element surface runoff to another element connection data set
      TYPE(RealTSDataInFileType)                    :: ReturnFracFile                     !Return flow fractions data file
      TYPE(RealTSDataInFileType)                    :: ReuseFracFile                      !Reuse fractions data file
      REAL(8),ALLOCATABLE                           :: RSoilM_P(:,:)                      !Regional soil moisture storage as volume at the beginning of time step
      REAL(8),ALLOCATABLE                           :: RSoilM(:,:)                        !Regional soil moisture storage as volume at the end of time step
      CHARACTER(LEN=f_iTimeStampLength),ALLOCATABLE :: cFutureDemandDates(:)              !Dates for which future demand is already computed for          
      REAL(8),ALLOCATABLE                           :: rFutureAgElemDemand(:,:)           !Future agricultural demand at each (element,timestep) combination
      REAL(8),ALLOCATABLE                           :: rFutureUrbElemDemand(:,:)          !Future urban demand at each (element,timestep) combination
      TYPE(BudgetType)                              :: LWUseBudRawFile                    !Raw land and water use budget output file
      TYPE(BudgetType)                              :: RootZoneBudRawFile                 !Raw root zone budget output file
      TYPE(GenericFileType)                         :: FinalMoistureOutFile               !Output file for the soil moisture at the end of simulation
  CONTAINS
      PROCEDURE(Abstract_New),PASS,DEFERRED                                   :: New
      PROCEDURE(Abstract_Kill),PASS,DEFERRED                                  :: KillRZImplementation
      PROCEDURE,PASS                                                          :: Kill
      PROCEDURE(Abstract_IsLandUseUpdated),PASS,DEFERRED                      :: IsLandUseUpdated
      PROCEDURE,PASS                                                          :: GetBudget_List
      PROCEDURE(Abstract_GetBudget_List),PASS,DEFERRED                        :: GetBudget_List_RZImplementation
      PROCEDURE(Abstract_GetBudget_NColumns),PASS,DEFERRED                    :: GetBudget_NColumns
      PROCEDURE(Abstract_GetBudget_ColumnTitles),PASS,DEFERRED                :: GetBudget_ColumnTitles
      PROCEDURE(Abstract_GetBudget_MonthlyFlows_GivenFile),NOPASS,DEFERRED    :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE(Abstract_GetBudget_MonthlyFlows_GivenRootZone),PASS,DEFERRED  :: GetBudget_MonthlyFlows_GivenRootZone
      PROCEDURE,PASS                                                          :: GetBudget_TSData
      PROCEDURE(Abstract_GetBudget_TSData_RZImplementation),PASS,DEFERRED     :: GetBudget_TSData_RZImplementation
      PROCEDURE,PASS                                                          :: GetZBudget_List
      PROCEDURE(Abstract_GetZBudget_NColumns),PASS,DEFERRED                   :: GetZBudget_NColumns
      PROCEDURE(Abstract_GetZBudget_ColumnTitles),PASS,DEFERRED               :: GetZBudget_ColumnTitles
      PROCEDURE,PASS                                                          :: GetZBudget_MonthlyFlows_GivenRootZone
      PROCEDURE,NOPASS                                                        :: GetZBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS                                                          :: GetZBudget_TSData
      PROCEDURE(Abstract_GetNAgCrops),PASS,DEFERRED                           :: GetNAgCrops
      PROCEDURE(Abstract_GetNDemandLocations),PASS,DEFERRED                   :: GetNDemandLocations
      PROCEDURE,PASS                                                          :: GetElementPrecip
      PROCEDURE(Abstract_GetElementPrecipInfilt),PASS,DEFERRED                :: GetElementPrecipInfilt
      PROCEDURE(Abstract_GetElementActualET),PASS,DEFERRED                    :: GetElementActualET
      PROCEDURE(Abstract_GetWaterDemandAll),PASS,DEFERRED                     :: GetWaterDemandAll
      PROCEDURE(Abstract_GetWaterDemandAtLocations),PASS,DEFERRED             :: GetWaterDemandAtLocations
      PROCEDURE(Abstract_GetWaterSupply),PASS,DEFERRED                        :: GetWaterSupply
      PROCEDURE(Abstract_GetElementAgAreas),PASS,DEFERRED                     :: GetElementAgAreas
      PROCEDURE(Abstract_GetElementUrbanAreas),PASS,DEFERRED                  :: GetElementUrbanAreas
      PROCEDURE(Abstract_GetElementNativeVegAreas),PASS,DEFERRED              :: GetElementNativeVegAreas
      PROCEDURE(Abstract_GetElementRiparianVegAreas),PASS,DEFERRED            :: GetElementRiparianVegAreas
      PROCEDURE(Abstract_GetSubregionAgAreas),PASS,DEFERRED                   :: GetSubregionAgAreas
      PROCEDURE(Abstract_GetSubregionUrbanAreas),PASS,DEFERRED                :: GetSubregionUrbanAreas
      PROCEDURE(Abstract_GetSubregionNativeVegAreas),PASS,DEFERRED            :: GetSubregionNativeVegAreas
      PROCEDURE(Abstract_GetSubregionRiparianVegAreas),PASS,DEFERRED          :: GetSubregionRiparianVegAreas
      PROCEDURE(Abstract_GetDemandAgAreas),PASS,DEFERRED                      :: GetDemandAgAreas
      PROCEDURE(Abstract_GetDemandUrbanAreas),PASS,DEFERRED                   :: GetDemandUrbanAreas
      PROCEDURE(Abstract_GetElementSoilMVolume),PASS,DEFERRED                 :: GetElementSoilMVolume
      PROCEDURE(Abstract_GetPercAll),PASS,DEFERRED                            :: GetPercAll
      PROCEDURE(Abstract_GetPercElement),PASS,DEFERRED                        :: GetPercElement
      PROCEDURE(Abstract_GetFlowsToStreams),PASS,DEFERRED                     :: GetFlowsToStreams
      PROCEDURE(Abstract_GetFlowsToLakes),PASS,DEFERRED                       :: GetFlowsToLakes
      PROCEDURE(Abstract_GetRatio_DestSupplyToRegionSupply_Ag),PASS,DEFERRED  :: GetRatio_DestSupplyToRegionSupply_Ag
      PROCEDURE(Abstract_GetRatio_DestSupplyToRegionSupply_Urb),PASS,DEFERRED :: GetRatio_DestSupplyToRegionSupply_Urb
      PROCEDURE(Abstract_GetVersion),PASS,DEFERRED                            :: GetVersion
      PROCEDURE(Abstract_GetMaxAndMinNetReturnFlowFrac),PASS,DEFERRED         :: GetMaxAndMinNetReturnFlowFrac
      PROCEDURE,PASS                                                          :: GetSurfaceFlowDestinations
      PROCEDURE,PASS                                                          :: GetSurfaceFlowDestinationTypes
      PROCEDURE,PASS                                                          :: GetSupplyShortAtDestination_ForSomeSupplies
      PROCEDURE,PASS                                                          :: GetFutureDemands
      PROCEDURE(Abstract_SetLakeElemFlag),PASS,DEFERRED                       :: SetLakeElemFlag
      PROCEDURE(Abstract_SetSupply),PASS,DEFERRED                             :: SetSupply
      PROCEDURE(Abstract_ConvertTimeUnit),PASS,DEFERRED                       :: ConvertTimeUnit
      PROCEDURE(Abstract_ReadTSData),PASS,DEFERRED                            :: ReadTSData
      PROCEDURE(Abstract_ReadRestartData),PASS,DEFERRED                       :: ReadRestartData
      PROCEDURE(Abstract_AdvanceState),PASS,DEFERRED                          :: AdvanceState
      PROCEDURE(Abstract_ComputeWaterDemand),PASS,DEFERRED                    :: ComputeWaterDemand
      PROCEDURE(Abstract_ComputeFutureWaterDemand),PASS,DEFERRED              :: ComputeFutureWaterDemand
      PROCEDURE(Abstract_ZeroSupply),PASS,DEFERRED                            :: ZeroSupply
      PROCEDURE(Abstract_ZeroSurfaceFlows),PASS,DEFERRED                      :: ZeroSurfaceFlows
      PROCEDURE(Abstract_Simulate),PASS,DEFERRED                              :: Simulate
      PROCEDURE(Abstract_RegionalPerc),PASS,DEFERRED                          :: RegionalPerc
      PROCEDURE(Abstract_RegionalReturnFlow_Ag),PASS,DEFERRED                 :: RegionalReturnFlow_Ag
      PROCEDURE(Abstract_RegionalReturnFlow_Urb),PASS,DEFERRED                :: RegionalReturnFlow_Urb
      PROCEDURE(Abstract_PrintResults),PASS,DEFERRED                          :: PrintResults
      PROCEDURE(Abstract_PrintRestartData),PASS,DEFERRED                      :: PrintRestartData
  END TYPE BaseRootZoneType
  
  
  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE
  
     SUBROUTINE Abstract_New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iStrmNodeIDs,iLakeIDs) 
        IMPORT                             :: AppGridType,TimeStepType,BaseRootZoneType,ETType,PrecipitationType
        CLASS(BaseRootZoneType)            :: RootZone
        LOGICAL,INTENT(IN)                 :: IsForInquiry
        CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cWorkingDirectory
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        TYPE(TimeStepType),INTENT(IN)      :: TimeStep
        INTEGER,INTENT(IN)                 :: NTIME
        TYPE(ETType),INTENT(IN)            :: ET
        TYPE(PrecipitationType),INTENT(IN) :: Precip
        INTEGER,INTENT(OUT)                :: iStat
        INTEGER,OPTIONAL,INTENT(IN)        :: iStrmNodeIDs(:),iLakeIDs(:)
     END SUBROUTINE Abstract_New
     
     
     SUBROUTINE Abstract_Kill(RootZone)
        IMPORT                  :: BaseRootZoneType
        CLASS(BaseRootZoneType) :: RootZone
     END SUBROUTINE Abstract_Kill
     
     
     FUNCTION Abstract_IsLandUseUpdated(RootZone) RESULT(lUpdated)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        LOGICAL                            :: lUpdated
     END FUNCTION Abstract_IsLandUseUpdated
     
     
    SUBROUTINE Abstract_GetMaxAndMinNetReturnFlowFrac(RootZone,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
        IMPORT                        :: BaseRootZoneType,TimeStepType
        CLASS(BaseRootZoneType)       :: RootZone
        TYPE(TimeStepType),INTENT(IN) :: FirstTimeStep
        REAL(8),INTENT(OUT)           :: rMaxFrac,rMinFrac
        INTEGER,INTENT(OUT)           :: iStat
    END SUBROUTINE Abstract_GetMaxAndMinNetReturnFlowFrac
  
  
     SUBROUTINE Abstract_GetBudget_List(RootZone,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
       IMPORT                                   :: BaseRootZoneType
       CLASS(BaseRootZoneType),INTENT(IN)       :: RootZone
       INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
       CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
     END SUBROUTINE Abstract_GetBudget_List

    
     SUBROUTINE Abstract_GetBudget_NColumns(RootZone,iBudgetType,iLocationIndex,iNCols,iStat) 
       IMPORT                                    :: BaseRootZoneType
       CLASS(BaseRootZoneType),TARGET,INTENT(IN) :: RootZone
       INTEGER,INTENT(IN)                        :: iBudgetType,iLocationIndex
       INTEGER,INTENT(OUT)                       :: iNCols,iStat
     END SUBROUTINE Abstract_GetBudget_NColumns
     
     
     SUBROUTINE Abstract_GetBudget_ColumnTitles(RootZone,iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
       IMPORT                                    :: BaseRootZoneType
       CLASS(BaseRootZoneType),TARGET,INTENT(IN) :: RootZone
       INTEGER,INTENT(IN)                        :: iBudgetType,iLocationIndex
       CHARACTER(LEN=*),INTENT(IN)               :: cUnitLT,cUnitAR,cUnitVL
       CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)  :: cColTitles(:)
       INTEGER,INTENT(OUT)                       :: iStat
     END SUBROUTINE Abstract_GetBudget_ColumnTitles
     
     
     SUBROUTINE Abstract_GetBudget_MonthlyFlows_GivenRootZone(RootZone,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
       IMPORT                                    :: BaseRootZoneType
       CLASS(BaseRootZoneType),TARGET,INTENT(IN) :: RootZone
       CHARACTER(LEN=*),INTENT(IN)               :: cBeginDate,cEndDate
       INTEGER,INTENT(IN)                        :: iBudgetType,iLUType,iSubregionID  
       REAL(8),INTENT(IN)                        :: rFactVL
       REAL(8),ALLOCATABLE,INTENT(OUT)           :: rFlows(:,:)  
       CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)  :: cFlowNames(:)
       INTEGER,INTENT(OUT)                       :: iStat
     END SUBROUTINE Abstract_GetBudget_MonthlyFlows_GivenRootZone
     
     
     SUBROUTINE Abstract_GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
       IMPORT                                   :: BudgetType
       TYPE(BudgetType),INTENT(IN)              :: Budget      
       CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
       INTEGER,INTENT(IN)                       :: iBudgetType,iLUType,iSubregionID  
       REAL(8),INTENT(IN)                       :: rFactVL
       REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  
       CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
       INTEGER,INTENT(OUT)                      :: iStat
     END SUBROUTINE Abstract_GetBudget_MonthlyFlows_GivenFile
     
     
     SUBROUTINE Abstract_GetBudget_TSData_RZImplementation(RootZone,iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        INTEGER,INTENT(IN)                 :: iBudgetType,iSubregionID,iCols(:)
        CHARACTER(LEN=*),INTENT(IN)        :: cBeginDate,cEndDate,cInterval
        REAL(8),INTENT(IN)                 :: rFactLT,rFactAR,rFactVL
        REAL(8),INTENT(OUT)                :: rOutputDates(:),rOutputValues(:,:)    
        INTEGER,INTENT(OUT)                :: iDataTypes(:),inActualOutput,iStat
     END SUBROUTINE Abstract_GetBudget_TSData_RZImplementation
     
     
     FUNCTION Abstract_GetZBudget_NColumns(RootZone,iZBudgetType) RESULT(iNCols)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        INTEGER,INTENT(IN)                 :: iZBudgetType
        INTEGER                            :: iNCols
     END FUNCTION Abstract_GetZBudget_NColumns


     SUBROUTINE Abstract_GetZBudget_ColumnTitles(RootZone,iZBudgetType,cUnitAR,cUnitVL,cColTitles,iStat)
        IMPORT                                   :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN)       :: RootZone
        INTEGER,INTENT(IN)                       :: iZBudgetType
        CHARACTER(LEN=*),INTENT(IN)              :: cUnitAR,cUnitVL
        CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
        INTEGER,INTENT(OUT)                      :: iStat
     END SUBROUTINE Abstract_GetZBudget_ColumnTitles


     PURE FUNCTION Abstract_GetNAgCrops(RootZone) RESULT(NAgCrops)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        INTEGER                            :: NAgCrops
     END FUNCTION Abstract_GetNAgCrops


     PURE FUNCTION Abstract_GetNDemandLocations(RootZone) RESULT(NLocations)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        INTEGER                            :: NLocations
     END FUNCTION Abstract_GetNDemandLocations


     SUBROUTINE Abstract_GetElementPrecipInfilt(RootZone,ElemRegion,PrecipInfilt)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        INTEGER,INTENT(IN)                 :: ElemRegion(:)
        REAL(8)                            :: PrecipInfilt(:)
     END SUBROUTINE Abstract_GetElementPrecipInfilt


     SUBROUTINE Abstract_GetElementActualET(RootZone,ElemRegion,ET)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        INTEGER,INTENT(IN)                 :: ElemRegion(:)
        REAL(8)                            :: ET(:)
     END SUBROUTINE Abstract_GetElementActualET


     SUBROUTINE Abstract_GetWaterDemandAll(RootZone,iDemandFor,rDemand)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        INTEGER,INTENT(IN)                 :: iDemandFor 
        REAL(8)                            :: rDemand(:)
     END SUBROUTINE Abstract_GetWaterDemandAll


     SUBROUTINE Abstract_GetWaterDemandAtLocations(RootZone,AppGrid,iLocationTypeID,iLocationIDList,iDemandFor,rDemand,iStat)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid 
        INTEGER,INTENT(IN)                 :: iLocationTypeID,iLocationIDList(:),iDemandFor  
        REAL(8)                            :: rDemand(:)
        INTEGER,INTENT(OUT)                :: iStat
     END SUBROUTINE Abstract_GetWaterDemandAtLocations


     SUBROUTINE Abstract_GetWaterSupply(RootZone,AppGrid,iSupplyFor,rSupply)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        INTEGER,INTENT(IN)                 :: iSupplyFor
        REAL(8)                            :: rSupply(:)
     END SUBROUTINE Abstract_GetWaterSupply
     
     
    SUBROUTINE Abstract_GetElementAgAreas(RootZone,Areas)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetElementAgAreas
     
     
     SUBROUTINE Abstract_GetElementUrbanAreas(RootZone,Areas)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetElementUrbanAreas
     
     
     SUBROUTINE Abstract_GetElementNativeVegAreas(RootZone,Areas)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetElementNativeVegAreas
     
     
     SUBROUTINE Abstract_GetElementRiparianVegAreas(RootZone,Areas)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetElementRiparianVegAreas
     
     
    SUBROUTINE Abstract_GetSubregionAgAreas(RootZone,AppGrid,Areas)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetSubregionAgAreas
     
     
     SUBROUTINE Abstract_GetSubregionUrbanAreas(RootZone,AppGrid,Areas)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetSubregionUrbanAreas
     
     
    SUBROUTINE Abstract_GetSubregionNativeVegAreas(RootZone,AppGrid,Areas)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetSubregionNativeVegAreas
     
     
    SUBROUTINE Abstract_GetSubregionRiparianVegAreas(RootZone,AppGrid,Areas)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: Areas(:)
     END SUBROUTINE Abstract_GetSubregionRiparianVegAreas
     
     
     SUBROUTINE Abstract_GetDemandAgAreas(RootZone,Areas)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),ALLOCATABLE                :: Areas(:)
     END SUBROUTINE Abstract_GetDemandAgAreas
     
     
     SUBROUTINE Abstract_GetDemandUrbanAreas(RootZone,Areas)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),ALLOCATABLE                :: Areas(:)
     END SUBROUTINE Abstract_GetDemandUrbanAreas
     
     
     SUBROUTINE Abstract_GetElementSoilMVolume(RootZone,AppGrid,SoilM)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: SoilM(:)
     END SUBROUTINE Abstract_GetElementSoilMVolume
     
     
     FUNCTION Abstract_GetPercAll(RootZone,AppGrid) RESULT(Perc)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGRidType),INTENT(IN)       :: AppGrid
        REAL(8)                            :: Perc(AppGrid%NElements)
     END FUNCTION Abstract_GetPercAll
     
     
     FUNCTION Abstract_GetPercElement(RootZone,iElem,AppGrid) RESULT(Perc)
        IMPORT                                :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN)    :: RootZone
        INTEGER,INTENT(IN)                    :: iElem
        TYPE(AppGridType),OPTIONAL,INTENT(IN) :: AppGrid
        REAL(8)                               :: Perc
     END FUNCTION Abstract_GetPercElement
     
     
     SUBROUTINE Abstract_GetFlowsToStreams(RootZone,AppGrid,DirectRunoff,ReturnFlow,RiparianET)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: DirectRunoff(:),ReturnFlow(:)
        REAL(8),INTENT(INOUT)              :: RiparianET(:)
     END SUBROUTINE Abstract_GetFlowsToStreams
     
     
     SUBROUTINE Abstract_GetFlowsToLakes(RootZone,AppGrid,DirectRunoff,ReturnFlow)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: DirectRunoff(:),ReturnFlow(:)
     END SUBROUTINE Abstract_GetFlowsToLakes
     
     
     PURE SUBROUTINE Abstract_GetRatio_DestSupplyToRegionSupply_Ag(RootZone,Ratio)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),INTENT(OUT)                :: Ratio(:)
     END SUBROUTINE Abstract_GetRatio_DestSupplyToRegionSupply_Ag
     
     
     PURE SUBROUTINE Abstract_GetRatio_DestSupplyToRegionSupply_Urb(RootZone,Ratio)
        IMPORT                             :: BaseRootZoneType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        REAL(8),INTENT(OUT)                :: Ratio(:)
     END SUBROUTINE Abstract_GetRatio_DestSupplyToRegionSupply_Urb
     
     
     FUNCTION Abstract_GetVersion(RootZone) RESULT(cVrs)
        IMPORT                   :: BaseRootZoneType
        CLASS(BaseRootZoneType)  :: RootZone
        CHARACTER(:),ALLOCATABLE :: cVrs
     END FUNCTION Abstract_GetVersion

    
     SUBROUTINE Abstract_SetLakeElemFlag(RootZone,iLakeElem)
        IMPORT                  :: BaseRootZoneType
        CLASS(BaseRootZoneType) :: RootZone
        INTEGER,INTENT(IN)      :: iLakeElem(:)
     END SUBROUTINE Abstract_SetLakeElemFlag
     
     
     SUBROUTINE Abstract_SetSupply(RootZone,rSupply,iSupplyType,iSupplyFor)
        IMPORT                  :: BaseRootZoneType
        CLASS(BaseRootZoneType) :: RootZone
        REAL(8),INTENT(IN)      :: rSupply(:)
        INTEGER,INTENT(IN)      :: iSupplyType,iSupplyFor
     END SUBROUTINE Abstract_SetSupply
     
     
     SUBROUTINE Abstract_ConvertTimeUnit(RootZone,NewUnit)
        IMPORT                      :: BaseRootZoneType
        CLASS(BaseRootZoneType)     :: RootZone
        CHARACTER(LEN=*),INTENT(IN) :: NewUnit
     END SUBROUTINE Abstract_ConvertTimeUnit
     
     
     SUBROUTINE Abstract_ReadTSData(RootZone,AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas)
        IMPORT                             :: BaseRootZoneType,AppGridType,TimeStepType,PrecipitationType,ETType
        CLASS(BaseRootZoneType),TARGET     :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        TYPE(TimeStepType),INTENT(IN)      :: TimeStep
        TYPE(PrecipitationType),INTENT(IN) :: Precip
        TYPE(ETType),INTENT(IN)            :: ETData
        INTEGER,INTENT(OUT)                :: iStat
        REAL(8),OPTIONAL,INTENT(IN)        :: RegionLUAreas(:,:)
     END SUBROUTINE Abstract_ReadTSData
     
     
     SUBROUTINE Abstract_ReadRestartData(RootZone,InFile,iStat)
        IMPORT                  :: BaseRootZoneType,GenericFileType
        CLASS(BaseRootZoneType) :: RootZone
        TYPE(GenericFileType)   :: InFile
        INTEGER,INTENT(OUT)     :: iStat
     END SUBROUTINE Abstract_ReadRestartData
     
     
     SUBROUTINE Abstract_AdvanceState(RootZone)
        IMPORT                  :: BaseRootZoneType
        CLASS(BaseRootZoneType) :: RootZone
     END SUBROUTINE Abstract_AdvanceState
     
     
     SUBROUTINE Abstract_ComputeWaterDemand(RootZone,AppGrid,TimeStep,ETData,iStat)
        IMPORT                        :: BaseRootZoneType,AppGridType,TimeStepType,ETType
        CLASS(BaseRootZoneType)       :: RootZone
        TYPE(AppGridType),INTENT(IN)  :: AppGrid
        TYPE(TimeStepType),INTENT(IN) :: TimeStep
        TYPE(ETType),INTENT(IN)       :: ETData
        INTEGER,INTENT(OUT)           :: iStat
     END SUBROUTINE Abstract_ComputeWaterDemand
     
     
     SUBROUTINE Abstract_ComputeFutureWaterDemand(RootZone,AppGrid,TimeStep,Precip,ET,cEndComputeDate,iStat)
        IMPORT                        :: BaseRootZoneType,AppGridType,TimeStepType,PrecipitationType,ETType
        CLASS(BaseRootZoneType)       :: RootZone
        TYPE(AppGridType),INTENT(IN)  :: AppGrid
        TYPE(TimeStepType),INTENT(IN) :: TimeStep
        TYPE(PrecipitationType)       :: Precip
        TYPE(ETType)                  :: ET
        CHARACTER(LEN=*),INTENT(IN)   :: cEndComputeDate 
        INTEGER,INTENT(OUT)           :: iStat
     END SUBROUTINE Abstract_ComputeFutureWaterDemand
     
     
     SUBROUTINE Abstract_ZeroSupply(RootZone)
        IMPORT                  :: BaseRootZoneType
        CLASS(BaseRootZoneType) :: RootZone
     END SUBROUTINE Abstract_ZeroSupply


     SUBROUTINE Abstract_ZeroSurfaceFlows(RootZone)
        IMPORT                  :: BaseRootZoneType
        CLASS(BaseRootZoneType) :: RootZone
     END SUBROUTINE Abstract_ZeroSurfaceFlows


     SUBROUTINE Abstract_Simulate(RootZone,AppGrid,TimeStep,ETData,iStat)
        IMPORT                        :: BaseRootZoneType,AppGridType,TimeStepType,ETType
        CLASS(BaseRootZoneType)       :: RootZone
        TYPE(AppGridType),INTENT(IN)  :: AppGrid
        TYPE(TimeStepType),INTENT(IN) :: TimeStep
        TYPE(ETType),INTENT(IN)       :: ETData
        INTEGER,INTENT(OUT)           :: iStat
     END SUBROUTINE Abstract_Simulate
     
     
     FUNCTION Abstract_RegionalPerc(RootZone,AppGrid) RESULT(RPERC)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8)                            :: RPERC(AppGrid%NSubregions+1)
     END FUNCTION Abstract_RegionalPerc
     
     
     SUBROUTINE Abstract_RegionalReturnFlow_Ag(RootZone,AppGrid,RReturnFlow)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: RReturnFlow(AppGrid%NSubregions+1)
     END SUBROUTINE Abstract_RegionalReturnFlow_Ag
     
     
     SUBROUTINE Abstract_RegionalReturnFlow_Urb(RootZone,AppGrid,RReturnFlow)
        IMPORT                             :: BaseRootZoneType,AppGridType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(AppGridType),INTENT(IN)       :: AppGrid
        REAL(8),INTENT(OUT)                :: RReturnFlow(AppGrid%NSubregions+1)
     END SUBROUTINE Abstract_RegionalReturnFlow_Urb
     
     
     SUBROUTINE Abstract_PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation)
        IMPORT                        :: BaseRootZoneType,AppGridType,ETType,TimeStepType
        CLASS(BaseRootZoneType)       :: RootZone
        TYPE(AppGridType),INTENT(IN)  :: AppGrid
        TYPE(ETType),INTENT(IN)       :: ETData
        TYPE(TimeStepType),INTENT(IN) :: TimeStep
        LOGICAL,INTENT(IN)            :: lEndOfSimulation
     END SUBROUTINE Abstract_PrintResults

 
     SUBROUTINE Abstract_PrintRestartData(RootZone,OutFile)
        IMPORT                             :: BaseRootZoneType,GenericFileType
        CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
        TYPE(GenericFileType)              :: OutFile
     END SUBROUTINE Abstract_PrintRestartData
     
     
  END INTERFACE
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen    = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName       = 'Class_BaseRootZone::'
  
  
  
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
  ! --- CLEAR MEMORY
  ! -------------------------------------------------------------
  SUBROUTINE Kill(RootZone)
    CLASS(BaseRootZoneType) :: RootZone
    
    !Local variables
    INTEGER         :: ErrorCode
    TYPE(FlagsType) :: DummyFlags
    
    !Deallocate arrays
    DEALLOCATE(RootZone%ElemPrecipData            , &
               RootZone%ElemFlowToOutside         , &
               RootZone%ElemFlowToStreams         , &
               RootZone%ElemFlowToLakes           , &
               RootZone%ElemFlowToSubregions      , &
               RootZone%ElemFlowToGW              , &
               RootZone%ElemFlowToElements        , &
               RootZone%RSoilM_P                  , &
               RootZone%RSoilM                    , &
               RootZone%rFutureAgElemDemand       , &
               RootZone%rFutureUrbElemDemand      , &
               RootZone%Flags%lLakeElems          , &
               STAT=ErrorCode                     )
    
    !Close files
    CALL RootZone%ReturnFracFile%Close()
    CALL RootZone%ReuseFracFile%Close()
    CALL RootZone%LWUseBudRawFile%Kill()
    CALL RootZone%RootZoneBudRawFile%Kill()
    CALL RootZone%FinalMoistureOutFile%Kill()
    
    !Reset flags
    RootZone%Flags = DummyFlags
    
    CALL RootZone%KillRZImplementation()
    
    CALL RootZone%Version%Kill()
    RootZone%VarTimeUnit = ''
    
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
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(RootZone,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(BaseRootZoneType),INTENT(IN)       :: RootZone
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    !Local variables
    INTEGER                        :: iCount,iErrorCode,iTypeList(10),iLocationList(10),iDim
    CHARACTER(LEN=500)             :: cFiles(10)
    CHARACTER(LEN=100)             :: cDescription(10)
    CHARACTER(:),ALLOCATABLE       :: cFileName
    INTEGER,ALLOCATABLE            :: iBudgetTypeList_Local(:),iBudgetLocationTypeList_Local(:)
    CHARACTER(LEN=500),ALLOCATABLE :: cBudgetFiles_Local(:)
    CHARACTER(LEN=100),ALLOCATABLE :: cBudgetDescriptions_Local(:)
    
    !Initialize
    iCount = 0
    DEALLOCATE (iBudgetTypeList , iBudgetLocationTypeList , cBudgetDescriptions , cBudgetFiles , STAT=iErrorCode)
         
    !Land and water use budget
    IF (RootZone%Flags%LWUseBudRawFile_Defined) THEN
        CALL RootZone%LWUseBudRawFile%GetFileName(cFileName)
        cFiles(iCount+1)        = cFileName
        iTypeList(iCount+1)     = f_iBudgetType_LWU
        iLocationList(iCount+1) = f_iLocationType_Subregion
        cDescription(iCount+1)  = f_cDescription_LWUBudget
        iCount                  = iCount + 1 
    END IF
    
    !Root zone budget
    IF (RootZone%Flags%RootZoneBudRawFile_Defined) THEN
        CALL RootZone%RootZoneBudRawFile%GetFileName(cFileName)
        cFiles(iCount+1)        = cFileName
        iTypeList(iCount+1)     = f_iBudgetType_RootZone
        iLocationList(iCount+1) = f_iLocationType_Subregion
        cDescription(iCount+1)  = f_cDescription_RootZoneBudget
        iCount                  = iCount + 1 
    END IF
     
    !Retrieve any implementation specific budgets
    CALL RootZone%GetBudget_List_RZImplementation(iBudgetTypeList_Local,iBudgetLocationTypeList_Local,cBudgetDescriptions_Local,cBudgetFiles_Local) 
    iDim = SIZE(iBudgetTypeList_Local)
    IF (iDim .GT. 0) THEN
        cFiles(iCount+1:iCount+iDim)        = cBudgetFiles_Local
        iTypeList(iCount+1:iCount+iDim)     = iBudgetTypeList_Local
        iLocationList(iCount+1:iCount+iDim) = iBudgetLocationTypeList_Local
        cDescription(iCount+1:iCount+iDim)  = cBudgetDescriptions_Local
        iCount                              = iCount + iDim 
    END IF
    
    !Copy info to return arguments
    ALLOCATE (iBudgetTypeList(iCount) , iBudgetLocationTypeList(iCount) , cBudgetDescriptions(iCount) , cBudgetFiles(iCount))
    iBudgetTypeList         = iTypeList(1:iCount)
    iBudgetLocationTypeList = iLocationList(1:iCount)
    cBudgetDescriptions     = cDescription(1:iCount)
    cBudgetFiles            = cFiles(1:iCount)
    
  END SUBROUTINE GetBudget_List


  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(RootZone,iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(BaseRootZoneType),TARGET,INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                        :: iBudgetType,iSubregionID,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)               :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                        :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                       :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                       :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER                  :: indx
    TYPE(BudgetType),POINTER :: pBudget
    
    !Initialize
    pBudget => NULL()
    
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_LWU)
            IF (RootZone%Flags%LWUseBudRawFile_Defined) pBudget => RootZone%LWUseBudRawFile
            
        CASE (f_iBudgetType_RootZone)
            IF (RootZone%Flags%RootZoneBudRawFile_Defined) pBudget => RootZone%RootZoneBudRawFile
            
        CASE DEFAULT
            CALL RootZone%GetBudget_TSData_RZImplementation(iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
            
    END SELECT
    
    !Return if no budget file
    IF (.NOT. ASSOCIATED(pBudget)) THEN
        iStat          = 0
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
        RETURN
    END IF
        
    !Read data
    DO indx=1,SIZE(iCols)
        CALL pBudget%ReadData(iSubregionID,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
    END DO
    
    !Clear memory
    NULLIFY(pBudget)
    
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET Z-BUDGET LIST (THIS WILL BE OVERWRITTEN WITH VERSIONS THAT HAVE ZBUDGET OUTPUT)
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_List(RootZone,iZBudgetTypeList,cZBudgetDescriptions,cZBudgetFiles)
     CLASS(BaseRootZoneType),INTENT(IN)       :: RootZone
     INTEGER,ALLOCATABLE,INTENT(OUT)          :: iZBudgetTypeList(:)          
     CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cZBudgetDescriptions(:),cZBudgetFiles(:)
     
     ALLOCATE (iZBudgetTypeList(0) , cZBudgetDescriptions(0) , cZBudgetFiles(0))
     
  END SUBROUTINE GetZBudget_List


  ! -------------------------------------------------------------
  ! --- GET MONTHLY FLOWS FROM RootZone OBJECT (THIS WILL BE OVERWRITTEN WITH VERSIONS THAT HAVE ZBUDGET OUTPUT)
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_MonthlyFlows_GivenRootZone(RootZone,iZBudgetType,iLUType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
     CLASS(BaseRootZoneType),TARGET,INTENT(IN) :: RootZone              
     INTEGER,INTENT(IN)                        :: iZBudgetType,iZoneID,iLUType,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
     CHARACTER(LEN=*),INTENT(IN)               :: cBeginDate,cEndDate  
     REAL(8),INTENT(IN)                        :: rFactVL
     REAL(8),ALLOCATABLE,INTENT(OUT)           :: rFlows(:,:)          
     CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)  :: cFlowNames(:)
     INTEGER,INTENT(OUT)                       :: iStat
     
     iStat = 0
     ALLOCATE (rFlows(0,0) , cFlowNames(0))
     
  END SUBROUTINE GetZBudget_MonthlyFlows_GivenRootZone


  ! -------------------------------------------------------------
  ! --- GET MONTHLY FLOWS FROM ZBUGDET OUTPUT (THIS WILL BE OVERWRITTEN WITH VERSIONS THAT HAVE ZBUDGET OUTPUT)
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_MonthlyFlows_GivenFile(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
     TYPE(ZBudgetType),INTENT(IN)             :: ZBudget              
     TYPE(ZoneListType),INTENT(IN)            :: ZoneList
     INTEGER,INTENT(IN)                       :: iZBudgetType,iZoneID,iLUType
     CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate  
     REAL(8),INTENT(IN)                       :: rFactVL
     REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)          
     CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
     INTEGER,INTENT(OUT)                      :: iStat
     
     iStat = 0
     ALLOCATE (rFlows(0,0) , cFlowNames(0))
     
  END SUBROUTINE GetZBudget_MonthlyFlows_GivenFile


  ! -------------------------------------------------------------
  ! --- GET TIME SERIES DATA FROM ZBUDGET FILE FOR A SELECTED ZONE AND SELECTED COLUMNS (THIS WILL BE OVERWRITTEN WITH VERSIONS THAT HAVE ZBUDGET OUTPUT)
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_TSData(RootZone,iZBudgetType,iZoneID,iCols,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,cInterval,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(BaseRootZoneType),TARGET,INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                        :: iZBudgetType,iZoneID,iCols(:),iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)               :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                        :: rFactAR,rFactVL
    REAL(8),INTENT(OUT)                       :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                       :: iDataTypes(:),inActualOutput,iStat
    
    iStat         = 0
    rOutputDates  = 0.0
    rOutputValues = 0.0
    iDataTypes    = -1
    inActualOutput = 0
    
  END SUBROUTINE GetZBudget_TSData

  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT PRECIPITATION
  ! -------------------------------------------------------------
  SUBROUTINE GetElementPrecip(RootZone,rElemArea,rPrecip)
     CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
     REAL(8),INTENT(IN)                 :: rElemArea(:)
     REAL(8)                            :: rPrecip(:)
     
     rPrecip = RootZone%ElemPrecipData%Precip * rElemArea
     
  END SUBROUTINE GetElementPrecip


  ! -------------------------------------------------------------
  ! --- GET SUPPLY SHORTAGE AT DESTINATIONS FOR LISTED SUPPLIES
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyShortAtDestination_ForSomeSupplies(RootZone,AppGrid,iSupplyList,iSupplyFor,SupplyDestConnector,rSupplyShortAtDest)
    CLASS(BaseRootZoneType),INTENT(IN)              :: RootZone
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    INTEGER,INTENT(IN)                              :: iSupplyList(:),iSupplyFor
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: SupplyDestConnector
    REAL(8),INTENT(OUT)                             :: rSupplyShortAtDest(:) 
  
    !Local variables
    INTEGER                                             :: indxDest,indxSupply,indxSupply1,iSupply,iSupply1,iDest,iCount
    REAL(8),DIMENSION(SupplyDestConnector%NDestination) :: rSupplyReq,rSupply,rSupplyShort
    
    !Initialize
    rSupplyShortAtDest = 0.0
    
    !Get demands and water shortage to meet demand at each demand location
    IF (iSupplyFor .EQ. f_iAg) THEN
        CALL RootZone%GetWaterDemandAll(f_iAg,rSupplyReq)
        CALL RootZone%GetWaterSupply(AppGrid,f_iAg,rSupply)
    ELSE
        CALL RootZone%GetWaterDemandAll(f_iUrb,rSupplyReq)
        CALL RootZone%GetWaterSupply(AppGrid,f_iUrb,rSupply)
    END IF
    
    !Supply short
    rSupplyShort = rSupplyReq - rSupply
    
    !Loop over listed supplies and compile supply short
    ASSOCIATE (pDestToSupply => SupplyDestConnector%DestinationToSupply , &
               pSupplyToDest => SupplyDestConnector%SupplyToDestination )
        DO indxSupply=1,SIZE(iSupplyList)
            iSupply = iSupplyList(indxSupply)
            IF (.NOT. (pSupplyToDest(iSupply)%iDestType.EQ.f_iFlowDest_Element .OR. pSupplyToDest(iSupply)%iDestType.EQ.f_iFlowDest_Subregion)) CYCLE
            DO indxDest=1,pSupplyToDest(iSupply)%nDest
                iDest = pSupplyToDest(iSupply)%iDests(indxDest)
                
                !Skip if there is no supply shortage/surplus
                IF (rSupplyShort(iDest) .EQ. 0.0) CYCLE
                
                !Count the adjusted supplies serving this destination
                iCount = 1
                DO indxSupply1=1,SIZE(iSupplyList)
                    iSupply1 = iSupplyList(indxSupply1)
                    IF (iSupply1 .EQ. iSupply) CYCLE
                    IF (.NOT. (pSupplyToDest(iSupply1)%iDestType.EQ.f_iFlowDest_Element .OR. pSupplyToDest(iSupply1)%iDestType.EQ.f_iFlowDest_Subregion)) CYCLE
                    IF (LocateInList(iDest,pSupplyToDest(iSupply1)%iDests) .GT. 0) iCount = iCount + 1
                END DO
                
                !Assume supply shortage is served equally by all listed supplies serving the destination
                rSupplyShortAtDest(indxSupply) = rSupplyShortAtDest(indxSupply) + rSupplyShort(iDest) / REAL(iCount,8)
            END DO
        END DO
    END ASSOCIATE
    
  END SUBROUTINE GetSupplyShortAtDestination_ForSomeSupplies

  
  ! -------------------------------------------------------------
  ! --- GET SURFACE FLOW DESTINATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION GetSurfaceFlowDestinations(RootZone,NElements) RESULT(Dest)
    CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NElements
    INTEGER                            :: Dest(NElements)
    
    !Local variables
    INTEGER :: indx,iElem
    
    !To outside
    Dest(RootZone%ElemFlowToOutside) = 0
    
    !To stream nodes
    DO indx=1,SIZE(RootZone%ElemFlowToStreams)
        iElem       = RootZone%ElemFlowToStreams(indx)%iElement        
        Dest(iElem) = RootZone%ElemFlowToStreams(indx)%iDest
    END DO
    
    !To lakes
    DO indx=1,SIZE(RootZone%ElemFlowToLakes)
        iElem       = RootZone%ElemFlowToLakes(indx)%iElement        
        Dest(iElem) = RootZone%ElemFlowToLakes(indx)%iDest
    END DO
    
    !To subregions
    DO indx=1,SIZE(RootZone%ElemFlowToSubregions)
        iElem       = RootZone%ElemFlowToSubregions(indx)%iElement        
        Dest(iElem) = RootZone%ElemFlowToSubregions(indx)%iDest
    END DO
    
    !To groundwater
    Dest(RootZone%ElemFlowToGW) = RootZone%ElemFlowToGW

  END FUNCTION GetSurfaceFlowDestinations
  
  
  ! -------------------------------------------------------------
  ! --- GET SURFACE FLOW DESTINATION TYPES
  ! -------------------------------------------------------------
  PURE FUNCTION GetSurfaceFlowDestinationTypes(RootZone,NElements) RESULT(DestTypes)
    CLASS(BaseRootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                 :: NElements
    INTEGER                            :: DestTypes(NElements)
    
    !Local variables
    INTEGER :: indx,iElem
    
    !To outside
    DestTypes(RootZone%ElemFlowToOutside) = f_iFlowDest_Outside
    
    !To stream nodes
    DO indx=1,SIZE(RootZone%ElemFlowToStreams)
        iElem            = RootZone%ElemFlowToStreams(indx)%iElement        
        DestTypes(iElem) = f_iFlowDest_StrmNode
    END DO
    
    !To lakes
    DO indx=1,SIZE(RootZone%ElemFlowToLakes)
        iElem            = RootZone%ElemFlowToLakes(indx)%iElement        
        DestTypes(iElem) = f_iFlowDest_Lake
    END DO
    
    !To subregions
    DO indx=1,SIZE(RootZone%ElemFlowToSubregions)
        iElem            = RootZone%ElemFlowToSubregions(indx)%iElement        
        DestTypes(iElem) = f_iFlowDest_Subregion
    END DO
    
    !To groundwater
    DestTypes(RootZone%ElemFlowToGW) = f_iFlowDest_GWElement

  END FUNCTION GetSurfaceFlowDestinationTypes
  
  
  ! -------------------------------------------------------------
  ! --- GET FUTURE DEMANDS AT A SPECIFIED DATE
  ! -------------------------------------------------------------
  SUBROUTINE GetFutureDemands(RootZone,AppGrid,TimeStep,Precip,ET,cFutureDemandDate,rElemAgDemand,rElemUrbDemand,iStat)
    CLASS(BaseRootZoneType)       :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(PrecipitationType)       :: Precip
    TYPE(ETType)                  :: ET
    CHARACTER(LEN=*),INTENT(IN)   :: cFutureDemandDate
    REAL(8),INTENT(OUT)           :: rElemAgDemand(:),rElemUrbDemand(:)  !Return data for each (element) 
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'GetFutureDemands'
    INTEGER                                :: iDimElem,iDimTime,iCount,indxTime,indxRetrieve
    
    !If future demands are already calculated until the specified date...
    IF (ALLOCATED(RootZone%cFutureDemandDates)) THEN
        iDimTime = SIZE(RootZone%cFutureDemandDates)
        IF (RootZone%cFutureDemandDates(iDimTime) .TSGE. cFutureDemandDate) THEN
            iDimElem     = SIZE(RootZone%rFutureAgElemDemand , DIM=1)
            indxRetrieve = 0
            DO indxTime=1,iDimTime
                IF (RootZone%cFutureDemandDates(indxTime) .EQ. cFutureDemandDate) THEN
                    indxRetrieve = indxTime
                    EXIT
                END IF
            END DO
            IF (indxRetrieve .GT. 0) THEN
                rElemAgDemand  = RootZone%rFutureAgElemDemand(:,indxRetrieve)
                rElemUrbDemand = RootZone%rFutureUrbElemDemand(:,indxRetrieve)
                iStat          = 0
            ELSE
                rElemAgDemand  = -1.0
                rElemUrbDemand = -1.0
                iStat          = -1
                CALL SetLastMessage('Future demands for '//TRIM(cFutureDemandDate)//' have not been computed!',f_iFatal,ThisProcedure)
            END IF
            RETURN
        END IF
    END IF
    
    !Compute future demands if they are not yet computed until specified date
    CALL RootZone%ComputeFutureWaterDemand(AppGrid,TimeStep,Precip,ET,cFutureDemandDate,iStat)  ;  IF (iStat .NE. 0) RETURN
    iDimElem = SIZE(RootZone%rFutureAgElemDemand , DIM=1)
    iDimTime = SIZE(RootZone%rFutureAgElemDemand , DIM=2)
    rElemAgDemand  = RootZone%rFutureAgElemDemand(:,iDimTime)
    rElemUrbDemand = RootZone%rFutureUrbElemDemand(:,iDimTime)

  END SUBROUTINE GetFutureDemands
  
  
  
  
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
  ! --- SUBROUTINE TO TRACK MOISTURE DUE TO PRECIP AND IRRIGATION
  ! -------------------------------------------------------------
  SUBROUTINE TrackMoistureDueToSource(SoilM_P,Infilt,Perc,ETa,Drain,SoilM,ETPartition)
    REAL(8),INTENT(IN)  :: SoilM_P(:),Infilt(:),Perc,ETa,Drain
    REAL(8),INTENT(OUT) :: SoilM(:),ETPartition(:)
    
    !Local variables
    INTEGER                          :: iDim
    REAL(8)                          :: divisor
    REAL(8),DIMENSION(SIZE(SoilM_P)) :: ratio,PercPartition,DrainPartition
    
    !Initialize
    iDim = SIZE(SoilM_P)
    
    !Compute
    divisor = SUM(SoilM_P) + SUM(Infilt)
    IF (divisor .EQ. 0.0) THEN
      ratio = 0.0
    ELSE
      ratio = SoilM_P + Infilt
      CALL NormalizeArray(ratio)
    END IF
    ETPartition    = ETa * ratio
    PercPartition  = Perc * ratio
    DrainPartition = Drain * ratio
    SoilM          = SoilM_P + Infilt - PercPartition - ETPartition - DrainPartition
    
  END SUBROUTINE TrackMoistureDueToSource
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE ELEMENT TO SURFACE FLOW DESTINATION LIST
  ! -------------------------------------------------------------
  SUBROUTINE CompileElemSurfaceFlowToDestinationList(iCompileDestType,SurfaceFlowDest,SurfaceFlowDestType,ElemFlowToDestList,iStat)
    INTEGER,INTENT(IN)                          :: iCompileDestType
    INTEGER,INTENT(IN)                          :: SurfaceFlowDest(:),SurfaceFlowDestType(:)
    TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToDestList(:)
    INTEGER,INTENT(OUT)                         :: iStat
    
    !Local data type
    TYPE,EXTENDS(GenericLinkedListType) :: ElemFlowToDestListType
    END TYPE ElemFlowToDestListType
    
    !Local variables
    INTEGER                         :: indxElem,NData,indx
    TYPE(ElemSurfaceFlowToDestType) :: ElemToDest
    TYPE(ElemFlowToDestListType)    :: TempElemFlowToDestList
    CLASS(*),POINTER                :: pCurrent
    
    !Initialize
    iStat = 0
    
    !Compile the list
    DO indxElem=1,SIZE(SurfaceFlowDest)
        IF (SurfaceFlowDestType(indxElem) .EQ. iCompileDestType) THEN
            ElemToDest%iElement = indxElem
            ElemToDest%iDest    = SurfaceFlowDest(indxElem)
            CALL TempElemFlowToDestList%AddNode(ElemToDest,iStat)
            IF (iStat .EQ. -1) RETURN
        END IF        
    END DO
    
    !Transfer list to subroutine return array
    NData = TempElemFlowToDestList%GetNNodes()
    ALLOCATE (ElemFlowToDestList(NData))
    CALL TempElemFlowToDestList%Reset()
    DO indx=1,NData
        pCurrent => TempElemFlowToDestList%GetCurrentValue()
        SELECT TYPE (pCurrent)
           TYPE IS (ElemSurfaceFlowToDestType)
              ElemFlowToDestList(indx) = pCurrent
        END SELECT
        CALL TempElemFlowToDestList%Next()
    END DO
    
    !Clear memory from temporary list
    CALL TempElemFlowToDestList%Delete()
    
  END SUBROUTINE CompileElemSurfaceFlowToDestinationList
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL POTENTIAL ET WHEN MULTIPLE LAND USES FOR EACH ELEMENT ARE GIVEN
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRegionalETPot_MultipleLUPerElem(AppGrid,ETData,iColETc,Area,ETPot)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(ETType),INTENT(IN)      :: ETData
    INTEGER,INTENT(IN)           :: iColETc(:,:)
    REAL(8),INTENT(IN)           :: Area(:,:)
    REAL(8),INTENT(OUT)          :: ETPot(:,:)
    
    !Local variables
    INTEGER :: indxElem,iRegion
    REAL(8) :: ETc(SIZE(iColETc,DIM=1))
    
    !Initialize
    ETPot = 0.0
    
    !Compile
    DO indxElem=1,AppGrid%NElements
        iRegion          = AppGrid%AppElement(indxElem)%Subregion
        ETc              = ETData%GetValues(iColETc(:,indxElem))
        ETPot(:,iRegion) = ETPot(:,iRegion) + ETc * Area(:,indxElem)
    END DO
    
  END SUBROUTINE ComputeRegionalETPot_MultipleLUPerElem

  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL POTENTIAL ET WHEN MULTIPLE LAND USES FOR EACH ELEMENT ARE GIVEN
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRegionalETPot_SingleLUPerElem(AppGrid,ETData,iColETc,Area,ETPot)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(ETType),INTENT(IN)      :: ETData
    INTEGER,INTENT(IN)           :: iColETc(:)
    REAL(8),INTENT(IN)           :: Area(:)
    REAL(8),INTENT(OUT)          :: ETPot(:)
    
    !Local variables
    INTEGER :: indxElem,iRegion
    REAL(8) :: ETc(SIZE(iColETc))
    
    !Initialize
    ETPot = 0.0
    ETc   = ETData%GetValues(iColETc)
    
    !Compile
    DO indxElem=1,AppGrid%NElements
        iRegion        = AppGrid%AppElement(indxElem)%Subregion
        ETPot(iRegion) = ETPot(iRegion) + ETc(indxElem) * Area(indxElem)
    END DO
    
  END SUBROUTINE ComputeRegionalETPot_SingleLUPerElem
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE ELEMENT URBAN DEMAND FRACTIONS FROM TOTAL URBAN DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE CalculateUrbanFracDemand(NElements,iColPopulation,iColPerCapitaWaterUse,FracDemand)
    INTEGER,INTENT(IN)  :: NElements
    INTEGER,INTENT(IN)  :: iColPopulation(NElements),iColPerCapitaWaterUse(NElements)
    REAL(8)             :: FracDemand(NElements)
    
    !Local variables
    INTEGER :: indxElem
    REAL(8) :: W(MAXVAL(iColPopulation),MAXVAL(iColPerCapitaWaterUse))
    
    !Initialize
    W = 0.0
    
    !Calculate total fraction for each (iColPopulation,iColPerCapitaWaterUse) combination
    DO indxElem=1,NElements
        W(iColPopulation(indxElem),iColPerCapitaWaterUse(indxElem)) = W(iColPopulation(indxElem),iColPerCapitaWaterUse(indxElem)) + FracDemand(indxElem)
    END DO
    
    !Normalize
    DO indxElem=1,NElements
        IF (W(iColPopulation(indxElem),iColPerCapitaWaterUse(indxElem)) .EQ. 0.0) THEN
            FracDemand(indxElem) = 0.0
        ELSE
            FracDemand(indxElem) = FracDemand(indxElem) / W(iColPopulation(indxElem),iColPerCapitaWaterUse(indxElem))
        END IF
    END DO

  END SUBROUTINE CalculateUrbanFracDemand
  
  
  ! -------------------------------------------------------------
  ! --- INTERPOLATE/EXTRAPOLATE ELEMENT-LEVEL LAND USE BASED ON SUBREGIONAL DATA
  ! -------------------------------------------------------------
  SUBROUTINE ElementLU_InterpolateExtrapolate(AppGrid,cLUCodes,iMeasuredLUDataFlag,indxForNV,lLakeElems,rCurrentDateAndTimeJulian,t1,t2,MeasuredRegionalLUArea,ElemObsAreas1,ElemObsAreas2,ExIntAreas,iStat)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    CHARACTER(LEN=*),INTENT(IN)  :: cLUCodes(:)
    INTEGER,INTENT(IN)           :: iMeasuredLUDataFlag,indxForNV
    LOGICAL,INTENT(IN)           :: lLakeElems(:)
    REAL(8),INTENT(IN)           :: rCurrentDateAndTimeJulian,MeasuredRegionalLUArea(:,:),t1(:),t2(:),ElemObsAreas1(:,:),ElemObsAreas2(:,:)
    REAL(8)                      :: ExIntAreas(:,:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    INTEGER         :: indxElem,indxLU,iDim,indxRegion,NLandUse
    REAL(8)         :: Dt,Area1,Area2,SubregionalAreasFromElemAreas(SIZE(ExIntAreas,DIM=1),AppGrid%NSubregions)
    REAL(8),TARGET  :: DiffArray(SIZE(ExIntAreas,DIM=1),AppGrid%NSubregions)
    REAL(8),POINTER :: pDiff
    
    !Initialize
    iStat = 0
    IF (iMeasuredLUDataFlag .EQ. iMeasuredLUDataForSubregion) THEN
      iDim = AppGrid%NSubregions
    ELSE
      iDim = 1
    END IF
    NLandUse = SIZE(ExIntAreas,DIM=1)
    
    !Initial estimate for element areas
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElems,t1,t2,ElemObsAreas1,ElemObsAreas2,ExIntAreas,indxForNV,rCurrentDateAndTimeJulian,NLandUse) NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    !$OMP DO SCHEDULE(STATIC,500)
    DO indxElem=1,AppGrid%NElements
        !Time interval between observation Julian dates
        Dt = t1(indxElem) - t2(indxElem)
        !Initial estimate of interpolated areas
        IF (Dt .EQ. 0.0) THEN
            ExIntAreas(:,indxElem) = ElemObsAreas1(:,indxElem)
        ELSE
            DO indxLU=1,NLandUse
                Area1                       = ElemObsAreas1(indxLU,indxElem)
                Area2                       = ElemObsAreas2(indxLU,indxElem)
                ExIntAreas(indxLU,indxElem) = MAX((( rCurrentDateAndTimeJulian-t1(indxElem)) * (Area1-Area2)) / Dt + Area1  ,  0.0)     
            END DO
        END IF
        !If lake element; assume all native vegetation
        IF (lLakeElems(indxElem)) THEN
            ExIntAreas(:,indxElem)         = 0.0
            ExIntAreas(indxForNV,indxElem) = 1.0
        END IF
        !Normalize land use area fractions just in case to avoid round-off errors
        CALL NormalizeArray(ExIntAreas(:,indxElem))
        !Compute actual areas (they are so far in fraction form)
        ExIntAreas(:,indxElem) = ExIntAreas(:,indxElem) * AppGrid%AppElement(indxElem)%Area 
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    !Compute subregional or model-domain-wide areas using interpolated/extrapolated element areas
    CALL ComputeSubregionalLandUse_FromElemAreas(AppGrid,iMeasuredLUDataFlag,ExIntAreas,SubregionalAreasFromElemAreas)
    
    !First decrease the element land use when necessary
    CALL ComputeDiscrepancy(iMeasuredLUDataFlag,MeasuredRegionalLUArea,SubregionalAreasFromElemAreas,DiffArray)
    DO indxRegion=1,iDim
      DO indxLU=1,NLandUse
        pDiff => DiffArray(indxLU,indxRegion)
        
        !Cycle if we are not adjusting
        IF (pDiff .GE. 0.0) CYCLE
    
        !Distribute discrepancy to elements
        CALL AdjustElemLandUseAreas(AppGrid,iMeasuredLUDataFlag,indxRegion,indxLU,SubregionalAreasFromElemAreas(indxLU,indxRegion),pDiff,cLUCodes(indxLU),ExIntAreas,iStat)
        IF (iStat .EQ. -1) RETURN
      END DO     
    END DO
    
    !Recompute subregional areas from element values and discrepancy
    CALL ComputeSubregionalLandUse_FromElemAreas(AppGrid,iMeasuredLUDataFlag,ExIntAreas,SubregionalAreasFromElemAreas)
    CALL ComputeDiscrepancy(iMeasuredLUDataFlag,MeasuredRegionalLUArea,SubregionalAreasFromElemAreas,DiffArray)
    
    !Now increase the element areas until subregional observed areas match sum of land use at elements
    DO
      !Exit if no more adjustment is necessary
      IF (ALL(DiffArray .LE. 0.0)) EXIT
      
      !Adjust for all regions and land use types
      DO indxRegion=1,iDim
        DO indxLU=1,NLandUse
          pDiff => DiffArray(indxLU,indxRegion)
          
          !Cycle if we are not adjusting
          IF (pDiff .LE. 0.0) CYCLE
          
          !Distribute discrepancy to elements
          CALL AdjustElemLandUseAreas(AppGrid,iMeasuredLUDataFlag,indxRegion,indxLU,SubregionalAreasFromElemAreas(indxLU,indxRegion),pDiff,cLUCodes(indxLU),ExIntAreas,iStat)
          IF (iStat .EQ. -1) RETURN
        END DO
      END DO
      
      !Recompute subregional areas from element values and discrepancy
      CALL ComputeSubregionalLandUse_FromElemAreas(AppGrid,iMeasuredLUDataFlag,ExIntAreas,SubregionalAreasFromElemAreas)
      CALL ComputeDiscrepancy(iMeasuredLUDataFlag,MeasuredRegionalLUArea,SubregionalAreasFromElemAreas,DiffArray)
    
    END DO
     
  END SUBROUTINE ElementLU_InterpolateExtrapolate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL LAND USE AREAS FROM ELEMENT VALUES
  ! -------------------------------------------------------------
  SUBROUTINE ComputeSubregionalLandUse_FromElemAreas(AppGrid,iMeasuredLUDataFlag,ElemAreas,SubregionalLandUse)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: iMeasuredLUDataFlag
    REAL(8),INTENT(IN)           :: ElemAreas(:,:)
    REAL(8),INTENT(OUT)          :: SubregionalLandUse(:,:)
   
    !Local variables
    INTEGER :: indxLU,NLandUse
    
    !Initialize
    NLandUse = SIZE(SubregionalLandUse,DIM=1)
   
    IF (iMeasuredLUDataFlag .EQ. iMeasuredLUDataForSubregion) THEN
      DO indxLU=1,NLandUse
        SubregionalLandUse(indxLU,:) = AppGrid%AccumElemValuesToSubregions(ElemAreas(indxLU,:))
      END DO
    ELSE
      DO indxLU=1,NLandUse
        SubregionalLandUse(indxLU,1) = SUM(ElemAreas(indxLU,:))
      END DO
    END IF
  
  END SUBROUTINE ComputeSubregionalLandUse_FromElemAreas

 
  ! -------------------------------------------------------------
  ! --- COMPUTE AREA DISCREPANCY
  ! -------------------------------------------------------------
  SUBROUTINE ComputeDiscrepancy(iMeasuredLUDataFlag,MeasuredLandUseArea,ComputedLandUse,DiffArray) 
    INTEGER,INTENT(IN)  :: iMeasuredLUDataFlag
    REAL(8),INTENT(IN)  :: MeasuredLandUseArea(:,:),ComputedLandUse(:,:)
    REAL(8),INTENT(OUT) :: DiffArray(:,:)
   
    !Local variables
    INTEGER :: indxRegion,indxLU,NRegions,NLandUse
    
    !Initailize
    NLandUse = SIZE(ComputedLandUse,DIM=1)
    NRegions = SIZE(ComputedLandUse,DIM=2)
   
    IF (iMeasuredLUDataFlag .EQ. iMeasuredLUDataForSubregion) THEN
      DO indxRegion=1,NRegions
        DO indxLU=1,NLandUse
          ASSOCIATE (pDiff            => DiffArray(indxLU,indxRegion)       , &
                     pComputedLandUse => ComputedLandUse(indxLU,indxRegion) )
            pDiff = MeasuredLandUSeArea(indxRegion,indxLU) - pComputedLandUse
            IF (pComputedLandUse .GT. 0.0) THEN
              IF (ABS(pDiff)/pComputedLandUse .LT. 1D-7) pDiff = 0.0
            END IF
          END ASSOCIATE 
        END DO
      END DO
      
    ELSE
      DO indxLU=1,NLandUse
        ASSOCIATE (pDiff            => DiffArray(indxLU,1)       , &
                   pComputedLandUse => ComputedLandUse(indxLU,1) )
          pDiff = MeasuredLandUSeArea(1,indxLU) - pComputedLandUse
          IF (pComputedLandUse .GT. 0.0) THEN
            IF (ABS(pDiff)/pComputedLandUse .LT. 1D-7) pDiff = 0.0
          END IF
        END ASSOCIATE
      END DO
    END IF
   
  END SUBROUTINE ComputeDiscrepancy
 
 
  ! -------------------------------------------------------------
  ! --- ADJUST ELEMENT LAND USE AREAS
  ! -------------------------------------------------------------
  SUBROUTINE AdjustElemLandUseAreas(AppGrid,iMeasuredLUDataFlag,iRegion,iLandUse,SubregionalLandUseArea,Diff,cLUCode,ElemAreas,iStat)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: iMeasuredLUDataFlag,iRegion,iLandUse
    REAL(8),INTENT(IN)           :: SubregionalLandUseArea,Diff
    CHARACTER(LEN=*),INTENT(IN)  :: cLUCode
    REAL(8)                      :: ElemAreas(:,:)
    INTEGER,INTENT(OUT)          :: iStat
    
    IF (Diff .LT. 0.0)  &
      CALL AdjustElemLandUseAreasForDecrease(AppGrid,iMeasuredLUDataFlag,iRegion,iLandUse,SubregionalLandUseArea,Diff,ElemAreas,iStat)
    
    IF (Diff .GT. 0.0)  &
      CALL AdjustElemLandUseAreasForIncrease(AppGrid,iMeasuredLUDataFlag,iRegion,iLandUse,Diff,cLUCode,ElemAreas,iStat)
  
  END SUBROUTINE AdjustElemLandUseAreas
 

  ! -------------------------------------------------------------
  ! --- ADJUST ELEMENT LAND USE AREAS FOR INCREASE
  ! -------------------------------------------------------------
  SUBROUTINE AdjustElemLandUseAreasForIncrease(AppGrid,iMeasuredLUDataFlag,iRegion,iLandUse,Diff,cLUCode,ElemAreas,iStat)
    TYPE(AppGridType),TARGET,INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)                  :: iMeasuredLUDataFlag,iRegion,iLandUse
    REAL(8),INTENT(IN)                  :: Diff
    CHARACTER(LEN=*),INTENT(IN)         :: cLUCode
    REAL(8)                             :: ElemAreas(:,:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33),PARAMETER :: ThisProcedure = ModName // 'AdjustElemLandUseAreasForIncrease'
    INTEGER                                :: indxElem,iElem,iElemWork(AppGrid%NElements),iDim,iSubregionID
    REAL(8)                                :: TotalArea,rAvailableArea,rAdjust
    INTEGER,POINTER                        :: pElems(:)
    INTEGER,ALLOCATABLE,TARGET,SAVE        :: iModelElements(:)
    
    !Initialize
    iStat = 0
    IF (iMeasuredLUDataFlag .EQ. iMeasuredLUDataForSubregion) THEN
        pElems => AppGrid%AppSubregion(iRegion)%RegionElements
    ELSE
        IF (.NOT. ALLOCATED(iModelElements)) THEN
            ALLOCATE (iModelElements(AppGrid%NElements))
            iModelElements = [(indxElem,indxElem=1,AppGrid%NElements)]
        END IF
        pElems => iModelElements
    END IF
    iDim      = SIZE(pElems)
    iElemWork = 0
    
    !Find elements where land-use area can be increased
    DO indxElem=1,iDim
      iElem     =  pElems(indxElem)
      IF (SPACING(AppGrid%AppElement(iElem)%Area) .LT. ABS(AppGrid%AppElement(iElem)%Area-SUM(ElemAreas(:,iElem)))) iElemWork(iElem) = 1
    END DO
    
    !Make sure that there is at least one element for land-use adjustment
    IF (LocateInList(1,iElemWork) .EQ. 0) THEN
        iSubregionID = AppGrid%AppSubregion(iRegion)%ID
        CALL SetLastMessage('There are no elements where land use can be adjusted for subregion '//TRIM(IntToText(iSubregionID))//  &
                        ' and land use type '//TRIM(cLUCode)//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
                      
    !Find total area of land use for these elements
    TotalArea = 0.0
    DO indxElem=1,iDim
      iElem = pElems(indxElem)
      IF (iElemWork(iElem) .EQ. 0) CYCLE
      TotalArea = TotalArea + ElemAreas(iLandUse,iElem)
    END DO
    
    !Distribute the discrepancy to elements that can take it up to the total element area
    IF (TotalArea .GT. 0.0) THEN
      DO indxElem=1,iDim
        iElem                     = pElems(indxElem)
        IF (iElemWork(iElem) .EQ. 0) CYCLE
        rAvailableArea            = AppGrid%AppElement(iElem)%Area - SUM(ElemAreas(:,iElem))
        rAdjust                   = MIN(Diff * ElemAreas(iLandUse,iElem) / TotalArea  , rAvailableArea)
        ElemAreas(iLandUse,iElem) = ElemAreas(iLandUse,iElem) + rAdjust 
      END DO
      
    ELSEIF (TotalArea .EQ. 0.0) THEN
      !Find the total area of empty lands
      DO indxElem=1,iDim
        iElem = pElems(indxElem)
        IF (iElemWork(iElem) .EQ. 0) CYCLE
        TotalArea = TotalArea + AppGrid%AppElement(iElem)%Area - SUM(ElemAreas(:,iElem))
      END DO
      !Now distribute
      DO indxElem=1,iDim
        iElem                     = pElems(indxElem)
        IF (iElemWork(iElem) .EQ. 0) CYCLE
        rAvailableArea            = AppGrid%AppElement(iElem)%Area - SUM(ElemAreas(:,iElem))
        rAdjust                   = MIN(Diff * rAvailableArea / TotalArea  , rAvailableArea)
        ElemAreas(iLandUse,iElem) = ElemAreas(iLandUse,iElem) + rAdjust 
      END DO
      
    ELSE
        iSubregionID = AppGrid%AppSubregion(iRegion)%ID
        CALL SetLastMessage('Total land use area for land use type '//TRIM(cLUCode)//  &
                            ' at subregion '//TRIM(IntToText(iSubregionID))//' is less than zero!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
  END SUBROUTINE AdjustElemLandUseAreasForIncrease
 
 
  ! -------------------------------------------------------------
  ! --- ADJUST ELEMENT LAND USE AREAS FOR DECREASE
  ! -------------------------------------------------------------
  SUBROUTINE AdjustElemLandUseAreasForDecrease(AppGrid,iMeasuredLUDataFlag,iRegion,iLandUse,SubregionalLandUseArea,Diff,ElemAreas,iStat)
    TYPE (AppGridType),TARGET,INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)                   :: iMeasuredLUDataFlag,iRegion,iLandUse
    REAL(8),INTENT(IN)                   :: SubregionalLandUseArea,Diff
    REAL(8)                              :: ElemAreas(:,:)
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    INTEGER                         :: indxElem,iElem,iDim
    REAL(8)                         :: NewArea,rAdjust,rFractions(AppGrid%NElements),DiffWork
    INTEGER,POINTER                 :: pElems(:)
    INTEGER,ALLOCATABLE,TARGET,SAVE :: iModelElements(:)
    
    !Initialize
    iStat = 0
    IF (iMeasuredLUDataFlag .EQ. iMeasuredLUDataForSubregion) THEN
        pElems => AppGrid%AppSubregion(iRegion)%RegionElements
    ELSE
        IF (.NOT. ALLOCATED(iModelElements)) THEN
            ALLOCATE (iModelElements(AppGrid%NElements))
            iModelElements = [(indxElem,indxElem=1,AppGrid%NElements)]
        END IF
        pElems => iModelElements
    END IF
    iDim      = SIZE(pElems)
    DiffWork  = Diff
    
    !First decrease areas on elements that don't have only this land use (assuming the interpoltaion says this land use stays constant)
    DO
        !Calculate reduction fractions for each element
        DO indxElem=1,iDim
            iElem = pElems(indxElem)
            
            !If the only land use in the element is this land use, do not adjust
            IF (AppGrid%AppElement(iElem)%Area .EQ. ElemAreas(iLandUse,iElem)) THEN
                rFractions(indxElem) = 0.0
            ELSE
                rFractions(indxElem) = ElemAreas(iLandUse,iElem)
            END IF
        END DO
        
        !If all fractions are zero, nothing can be done; exit loop
        IF (SUM(rFractions(1:iDim)) .EQ. 0.0) EXIT
        
        !Normalize fractions
        CALL NormalizeArray(rFractions(1:iDim))
 
        !Distribute discrepancy to elements
        DO indxElem=1,iDim
          iElem = pElems(indxElem)
          
          !Compute adjustment area
          rAdjust = DiffWork * rFractions(indxElem)
          
          !Adjust element land use area only if this land use is not the only land use in the element
          ElemAreas(iLandUse,iElem) = MAX(ElemAreas(iLandUse,iElem) + rAdjust  , 0.0)
        END DO
        
        !New land use area
        NewArea = SUM(ElemAreas(iLandUse,pElems))
        
        !Check if more adjustment is possible
        IF (NewArea-SubregionalLandUseArea .EQ. Diff) RETURN
 
        !If it is possible, calculate new difference and adjust again
        DiffWork = SubregionalLandUseArea + Diff - NewArea
        IF (DiffWork .GT. 0.0) RETURN
        
    END DO
    
    !If made it to this point, it is necessary to decrease land use in elements with only this land use
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(iDim,pElems,AppGrid,DiffWork,ElemAreas,NewArea,iLandUse) 
    !$OMP DO SCHEDULE(STATIC,500)
    DO indxElem=1,iDim
        iElem                     = pElems(indxElem)
        rAdjust                   = DiffWork * ElemAreas(iLandUse,iElem) / NewArea
        ElemAreas(iLandUse,iElem) = MAX(ElemAreas(iLandUse,iElem) + rAdjust  , 0.0)
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
     
  END SUBROUTINE AdjustElemLandUseAreasForDecrease
  
  
END MODULE