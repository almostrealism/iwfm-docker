!***********************************************************************                     
!  Integrated Water Flow Model (IWFM)                                                        
!  Copyright (C) 2005-2022                                                                   
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
MODULE RootZone_v412                                                                         
  !$ USE OMP_LIB                                                                             
  USE MessageLogger              , ONLY: SetLastMessage                                         , &
                                         EchoProgress                                           , &
                                         MessageArray                                           , &
                                         f_iFatal                                                 
  USE GeneralUtilities           , ONLY: StripTextUntilCharacter                                , &
                                         IntToText                                              , &
                                         ArrangeText                                            , &
                                         UpperCase                                              , & 
                                         ConvertID_To_Index                                     , &
                                         CleanSpecialCharacters                                 , &
                                         EstablishAbsolutePathFileName                          , &
                                         LocateInList                                           , &
                                         NormalizeArray                                         
  USE TimeSeriesUtilities        , ONLY: TimeStepType                                           , &
                                         IncrementTimeStamp                                     
  USE IOInterface                , ONLY: GenericFileType                                        , &
                                         IntPairTSDataInFileType                                
  USE Package_Misc               , ONLY: f_iFlowDest_Outside                                    , &
                                         f_iFlowDest_StrmNode                                   , &
                                         f_iFlowDest_Lake                                       , &
                                         f_iFlowDest_GWElement                                  , &
                                         f_iFlowDest_Element                                    , &
                                         f_iFlowDest_Subregion                                  , &
                                         f_iLandUse_Ag                                          , &
                                         f_iLandUse_NonPondedAg                                 , &
                                         f_iLandUse_PondedAg                                    , &
                                         f_iLandUse_Urb                                         , &
                                         f_iLandUse_UrbIn                                       , &
                                         f_iLandUse_UrbOut                                      , &
                                         f_iLandUse_NVRV                                        , &
                                         f_iLandUse_Rice                                        , &
                                         f_iLandUse_Refuge                                     
  USE Package_Discretization     , ONLY: AppGridType                                            
  USE Package_PrecipitationET    , ONLY: PrecipitationType                                      , &
                                         ETType                                                 
  USE Class_GenericLandUseGW     , ONLY: GenericLandUseGWType             
  USE Class_BaseRootZone         , ONLY: FlagsType                                              , &
                                         ElemSurfaceFlowToDestType                              
  USE Util_Package_RootZone      , ONLY: WaterSupplyType                                        , &
                                         f_iBudgetType_LWU                                      , &
                                         f_iBudgetType_RootZone                                 , &
                                         f_iZBudgetType_RootZone                                , &
                                         f_iZBudgetType_LWU                       
  USE Class_PondedAgLandUseGW    , ONLY: PondedAgDatabaseType                                   , &
                                         f_iNPondedCrops                                        
  USE Class_NonPondedAgLandUseGW , ONLY: NonPondedAgDatabaseType
  USE RootZone_v411              , ONLY: RootZone_v411_Type                                     
  USE Package_UnsatZone          , ONLY: f_iKUnsatMethodList                                    
  USE Package_Budget             , ONLY: BudgetType                                             , &
                                         BudgetHeaderType                                       , &
                                         f_iMaxLocationNameLen_Budget  => f_iMaxLocationNameLen , &                  
                                         f_cVolumeUnitMarker_Budget    => f_cVolumeUnitMarker   , &
                                         f_cAreaUnitMarker_Budget      => f_cAreaUnitMarker     , &
                                         f_cLocationNameMarker_Budget  => f_cLocationNameMarker , &
                                         f_cAreaMarker_Budget          => f_cAreaMarker         , &
                                         f_iAR_Budget                  => f_iAR                 , &
                                         f_iVR_Budget                  => f_iVR                 , &
                                         f_iVLB_Budget                 => f_iVLB                , &
                                         f_iVLE_Budget                 => f_iVLE                , &
                                         f_iVR_lwu_PotCUAW                                      , &
                                         f_iVR_lwu_AgSupplyReq                                  , &
                                         f_iVR_lwu_AgPump                                       , &
                                         f_iVR_lwu_AgDiv                                        , &
                                         f_iVR_lwu_AgShort                                      , &
                                         f_iPER_AVER                                            , &
                                         f_iPER_CUM                                             
  USE Package_ZBudget            , ONLY: ZBudgetType                                            , &
                                         SystemDataType                                         , &
                                         ZBudgetHeaderType                                      , &
                                         ZoneListType                                           , &
                                         f_iElemDataType                                        , &
                                         f_cMarkerChar_ZBudget         => f_cMarkerChar         , &
                                         f_cAreaUnitMarker_ZBudget     => f_cAreaUnitMarker     , &
                                         f_iAR_ZBudget                 => f_iAR                 , &
                                         f_iVR_ZBudget                 => f_iVR                 , &
                                         f_iVLB_ZBudget                => f_iVLB                , &
                                         f_iVLE_ZBudget                => f_iVLE                
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
  PUBLIC :: RootZone_v412_Type 
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RootZone_v411_Type) :: RootZone_v412_Type
      PRIVATE
      LOGICAL                                     :: lStrmNodeIDs_Provided = .TRUE.             !Flag to be used to decide if stream node IDs will be converted to indices (mainly for IDC runs)
      LOGICAL                                     :: lLakeIDs_Provided     = .TRUE.             !Flag to be used to decide if lake IDs will be converted to indices (mainly for IDC runs)
      INTEGER,ALLOCATABLE                         :: iStrmNodeIDs(:)                            !To be used to convert stream node IDs (as surface flow destinations) to indices
      INTEGER,ALLOCATABLE                         :: iLakeIDs(:)                                !To be used to convert lake IDs (as surfacce flow destinations) to indices
      INTEGER,ALLOCATABLE                         :: iColSurfaceFlowDestination_Ag(:)           !For each (element)
      INTEGER,ALLOCATABLE                         :: iColSurfaceFlowDestination_UrbIndoors(:)   !For each (element)
      INTEGER,ALLOCATABLE                         :: iColSurfaceFlowDestination_UrbOutdoors(:)  !For each (element)
      INTEGER,ALLOCATABLE                         :: iColSurfaceFlowDestination_NVRV(:)         !For each (element)
      TYPE(IntPairTSDataInFileType)               :: SurfaceFlowDestinationFile                 !File that stores timeseries destination data
      INTEGER,ALLOCATABLE                         :: ElemFlowToOutside_Ag(:)                    !List of elements from which agricultural surface flows go to outside the model area
      INTEGER,ALLOCATABLE                         :: ElemFlowToOutside_UrbIn(:)                 !List of elements from which urban indoors return flows go to outside the model area
      INTEGER,ALLOCATABLE                         :: ElemFlowToOutside_UrbOut(:)                !List of elements from which urban outdoors flows go to outside the model area
      INTEGER,ALLOCATABLE                         :: ElemFlowToOutside_NVRV(:)                  !List of elements from which native&riparain veg surface flows go to outside the model area
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToStreams_Ag(:)                    !List of elements from which agricultural surface flows go to stream nodes and the stream node ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToStreams_UrbIn(:)                 !List of elements from which urban indoors return flows go to stream nodes and the stream node ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToStreams_UrbOut(:)                !List of elements from which urban outdoors surface flows go to stream nodes and the stream node ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToStreams_NVRV(:)                  !List of elements from which native&riparian veg surface flows go to stream nodes and the stream node ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToLakes_Ag(:)                      !List of elements from which agricultural surface flows go to lakes and the lake ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToLakes_UrbIn(:)                   !List of elements from which urban indoors return flows go to lakes and the lake ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToLakes_UrbOut(:)                  !List of elements from which urban outdoors surface flows go to lakes and the lake ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToLakes_NVRV(:)                    !List of elements from which native&riparian veg surface flows go to lakes and the lake ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToGW_Ag(:)                         !List of elements from which agricultural surface flows go to gw and the element ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToGW_UrbIn(:)                      !List of elements from which urban indoors return flows go to gw and the element ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToGW_UrbOut(:)                     !List of elements from which urban outdoors surface flows go to gw and the element ID
      TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemFlowToGW_NVRV(:)                       !List of elements from which native&riparian veg surface flows go to gw and the element ID
      REAL(8),ALLOCATABLE                         :: rAW_UrbanIndoors(:)                        !Urban indoors applied water at each (element) 
  CONTAINS
      PROCEDURE,PASS   :: New                                   => RootZone_v412_New
      PROCEDURE,PASS   :: KillRZImplementation                  => RootZone_v412_Kill
      PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile      => RootZone_v412_GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenRootZone  => RootZone_v412_GetBudget_MonthlyFlows_GivenRootZone
      PROCEDURE,PASS   :: GetZBudget_NColumns                   => RootZone_v412_GetZBudget_NColumns
      PROCEDURE,NOPASS :: GetZBudget_MonthlyFlows_GivenFile     => RootZone_v412_GetZBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetZBudget_MonthlyFlows_GivenRootZone => RootZone_v412_GetZBudget_MonthlyFlows_GivenRootZone
      PROCEDURE,PASS   :: GetFlowsToStreams                     => RootZone_v412_GetFlowsToStreams
      PROCEDURE,PASS   :: GetFlowsToLakes                       => RootZone_v412_GetFlowsToLakes
      PROCEDURE,PASS   :: GetPercAll                            => RootZone_v412_GetPercAll
      PROCEDURE,PASS   :: GetPercElement                        => RootZone_v412_GetPercElement
      PROCEDURE,PASS   :: GetSurfaceFlowDestinations            => RootZone_v412_GetSurfaceFlowDestinations
      PROCEDURE,PASS   :: GetSurfaceFlowDestinationTypes        => RootZone_v412_GetSurfaceFlowDestinationTypes
      PROCEDURE,PASS   :: GetVersion                            => RootZone_v412_GetVersion
      PROCEDURE,PASS   :: RegionalReturnFlow_Ag                 => RootZone_v412_RegionalReturnFlow_Ag
      PROCEDURE,PASS   :: RegionalReturnFlow_Urb                => RootZone_v412_RegionalReturnFlow_Urb
      PROCEDURE,PASS   :: RegionalPerc                          => RootZone_v412_RegionalPerc
      PROCEDURE,PASS   :: PrintResults                          => RootZone_v412_PrintResults
      PROCEDURE,PASS   :: ReadTSData                            => RootZone_v412_ReadTSData
      PROCEDURE,PASS   :: Simulate                              => RootZone_v412_Simulate
  END TYPE RootZone_v412_Type

  
  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 9
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '4.12.0000'
  INCLUDE 'RootZone_v412_Revision.fi'
  
  
  ! -------------------------------------------------------------
  ! --- Z-BUDGET AND BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iNLWUseBudColumns                                   = 15  , &
                                 f_iNAgLWUseBudColumns                                 = 10  , &
                                 f_iNLWUseZBudColumns                                  = 35  , &
                                 f_iNRootZoneBudColumns                                = 59  , &
                                 f_iNAgRootZoneBudColumns                              = 21  , &
                                 f_iNRootZoneZBudColumns                               = 100  
  CHARACTER(LEN=30),PARAMETER :: f_cLWUseBudgetColumnTitles(f_iNLWUseBudColumns)       = ['Ag. Area'                           , &
                                                                                          'Potential CUAW'                     , &
                                                                                          'Ag. Supply Requirement'             , &
                                                                                          'Ag. Pumping'                        , &
                                                                                          'Ag. Deliveries'                     , &
                                                                                          'Ag. Shortage'                       , &
                                                                                          'Ag. ETAW'                           , &
                                                                                          'Ag. Effective Precipitation'        , &
                                                                                          'Ag. ET from Groundwater'            , &
                                                                                          'Ag. ET from Other Sources'          , &
                                                                                          'Urban Area'                         , &
                                                                                          'Urban Supply Requirement'           , &
                                                                                          'Urban Pumping'                      , &
                                                                                          'Urban Deliveries'                   , &
                                                                                          'Urban Shortage'                     ]
  CHARACTER(LEN=40),PARAMETER :: f_cLWUseZBudgetColumnTitles(f_iNLWUseZBudColumns)     = ['Non-ponded Ag. Area'                           , &
                                                                                          'Non-ponded Potential CUAW'                     , &
                                                                                          'Non-ponded Ag. Supply Requirement'             , &
                                                                                          'Non-ponded Ag. Pumping'                        , &
                                                                                          'Non-ponded Ag. Deliveries'                     , &
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
                                                                                          'Refuge Shortage'                               , &
                                                                                          'Refuge ETAW'                                   , &
                                                                                          'Refuge Effective Precipitation'                , &
                                                                                          'Refuge ET from Groundwater'                    , &
                                                                                          'Refuge ET from Other Sources'                  , &
                                                                                          'Urban Area'                                    , &
                                                                                          'Urban Supply Requirement'                      , &
                                                                                          'Urban Pumping'                                 , &
                                                                                          'Urban Deliveries'                              , &
                                                                                          'Urban Shortage'                                ]
  CHARACTER(LEN=53),PARAMETER :: f_cRootZoneBudgetColumnTitles(f_iNRootZoneBudColumns) = ['Ag. Area'                                               , &
                                                                                          'Ag. Potential ET'                                       , &
                                                                                          'Ag. Precipitation'                                      , &
                                                                                          'Ag. Runoff'                                             , &
                                                                                          'Ag. Prime Applied Water'                                , &
                                                                                          'Ag. Reused Water'                                       , &
                                                                                          'Ag. Net Return Flow'                                    , &
                                                                                          'Ag. Surface Flow to GW (Ag)'                            , &
                                                                                          'Ag. Surface Flow to GW (Urban Indrs)'                   , &
                                                                                          'Ag. Surface Flow to GW (Urban Outdrs)'                  , &
                                                                                          'Ag. Surface Flow to GW (Native Veg)'                    , &
                                                                                          'Ag. Beginning Storage (+)'                              , &
                                                                                          'Ag. Net Gain from Land Expansion (+)'                   , &
                                                                                          'Ag. Infiltration (+)'                                   , &
                                                                                          'Ag. Groundwater Inflow (+)'                             , &
                                                                                          'Ag. Other Inflow (+)'                                   , &
                                                                                          'Ag. Pond Drain (-)'                                     , &
                                                                                          'Ag. Actual ET (-)'                                      , &
                                                                                          'Ag. Percolation (-)'                                    , &
                                                                                          'Ag. Ending Storage (-)'                                 , &
                                                                                          'Ag. Discrepancy (=)'                                    , &
                                                                                          'Urban Area'                                             , &
                                                                                          'Urban Potential ET'                                     , &
                                                                                          'Urban Precipitation'                                    , &
                                                                                          'Urban Runoff'                                           , &
                                                                                          'Urban Prime Applied Water'                              , &
                                                                                          'Urban Reused Water'                                     , &
                                                                                          'Urban Net Return Flow'                                  , &
                                                                                          'Urban Surface Flow to GW (Ag)'                          , &
                                                                                          'Urban Surface Flow to GW (Urban Indrs)'                 , &
                                                                                          'Urban Surface Flow to GW (Urban Outdrs)'                , &
                                                                                          'Urban Surface Flow to GW (Native Veg)'                  , &
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
                                                                                          'Native&Riparian Veg. Runoff'                            , &
                                                                                          'Native&Riparian Surface Flow to GW (Ag)'                , &
                                                                                          'Native&Riparian Surface Flow to GW (Urban Indrs)'       , &
                                                                                          'Native&Riparian Surface Flow to GW (Urban Outdrs)'      , &
                                                                                          'Native&Riparian Surface Flow to GW (Native Veg)'        , &
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
  CHARACTER(LEN=54),PARAMETER :: f_cRootZoneZBudgetColumnTitles(f_iNRootZoneZBudColumns) = ['Non-ponded Ag. Area'                                    , &
                                                                                            'Non-ponded Ag. Potential ET'                            , &
                                                                                            'Non-ponded Ag. Precipitation'                           , &
                                                                                            'Non-ponded Ag. Runoff'                                  , &
                                                                                            'Non-ponded Ag. Prime Applied Water'                     , &
                                                                                            'Non-ponded Ag. Reused Water'                            , &
                                                                                            'Non-ponded Ag. Net Return Flow'                         , &
                                                                                            'Non-ponded Ag. Surface Flow to GW (Ag)'                 , &
                                                                                            'Non-ponded Ag. Surface Flow to GW (Urban Indrs)'        , &
                                                                                            'Non-ponded Ag. Surface Flow to GW (Urban Outdrs)'       , &
                                                                                            'Non-ponded Ag. Surface Flow to GW (Native Veg)'         , &
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
                                                                                            'Rice Reused Water'                                      , &
                                                                                            'Rice Net Return Flow'                                   , &
                                                                                            'Rice Surface Flow to GW (Ag)'                           , &
                                                                                            'Rice Surface Flow to GW (Urban Indrs)'                  , &
                                                                                            'Rice Surface Flow to GW (Urban Outdrs)'                 , &
                                                                                            'Rice Surface Flow to GW (Native Veg)'                   , &
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
                                                                                            'Refuge Reused Water'                                    , &
                                                                                            'Refuge Net Return Flow'                                 , &
                                                                                            'Refuge Surface Flow to GW (Ag)'                         , &
                                                                                            'Refuge Surface Flow to GW (Urban Indrs)'                , &
                                                                                            'Refuge Surface Flow to GW (Urban Outdrs)'               , &
                                                                                            'Refuge Surface Flow to GW (Native Veg)'                 , &
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
                                                                                            'Urban Reused Water'                                     , &
                                                                                            'Urban Net Return Flow'                                  , &
                                                                                            'Urban Surface Flow to GW (Ag)'                          , &
                                                                                            'Urban Surface Flow to GW (Urban Indrs)'                 , &
                                                                                            'Urban Surface Flow to GW (Urban Outdrs)'                , &
                                                                                            'Urban Surface Flow to GW (Native Veg)'                  , &
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
                                                                                            'Native&Riparian Veg. Runoff'                            , &
                                                                                            'Native&Riparian Veg. Surface Flow to GW (Ag)'           , &
                                                                                            'Native&Riparian Veg. Surface Flow to GW (Urban Indrs)'  , &
                                                                                            'Native&Riparian Veg. Surface Flow to GW (Urban Outdrs)' , &
                                                                                            'Native&Riparian Veg. Surface Flow to GW (Native Veg)'   , &
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
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'RootZone_v412::'
  

  
  
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
  SUBROUTINE RootZone_v412_New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iStrmNodeIDs,iLakeIDs)
    CLASS(RootZone_v412_Type)          :: RootZone
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
    CHARACTER(LEN=ModNameLen+17)                :: ThisProcedure = ModName // 'RootZone_v412_New'
    CHARACTER(LEN=1000)                         :: ALine,NonPondedCropFile,RiceRefugeFile,UrbanDataFile,NVRVFile,AgWaterDemandFile,GenericMoistureFile
    CHARACTER                                   :: cVersionLocal*20
    REAL(8)                                     :: FACTK,FACTCN,RegionArea(AppGrid%NSubregions+1),rDummyFactor(1),rDummyArray16(AppGrid%NElements,16), &
                                                   FACTEXDTH                                                                                           
    INTEGER                                     :: NElements,NRegion,ErrorCode,indxElem,iColGenericMoisture(AppGrid%NElements),iElemID,iElem,          &
                                                   iElemIDs(AppGrid%NElements),iSubregionIDs(AppGrid%NSubregions),iFlagETFromGW
    TYPE(GenericFileType)                       :: RootZoneParamFile
    LOGICAL                                     :: TrackTime,lProcessed(AppGrid%NElements)
    CHARACTER(LEN=f_iMaxLocationNameLen_Budget) :: RegionNames(AppGrid%NSubregions+1)
    CHARACTER(:),ALLOCATABLE                    :: cAbsPathFileName
    PROCEDURE(AgLWUseBudRawFile_New),POINTER    :: pAgLWUseBudRawFile_New
    PROCEDURE(AgRootZoneBudRawFile_New),POINTER :: pAgRootZoneBudRawFile_New
    
    !Initialize
    iStat = 0
    
    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN
    
    !Print progress
    CALL EchoProgress('Instantiating root zone')

    !Initialize
    RootZone%Version          = RootZone%Version%New(iLenVersion,cVersion,cRevision)
    cVersionLocal             = ADJUSTL('v' // TRIM(RootZone%Version%GetVersion()))
    NElements                 = AppGrid%NElements
    NRegion                   = AppGrid%NSubregions
    iElemIDs                  = AppGrid%AppElement%ID
    iSubregionIDs             = AppGrid%AppSubregion%ID
    TrackTime                 = TimeStep%TrackTime
    RegionArea(1:NRegion)     = AppGrid%GetSubregionAreaForAll()
    RegionArea(NRegion+1)     = SUM(RegionArea(1:NRegion))
    RegionNames               = ''  ;  RegionNames(1:NRegion) = AppGrid%GetSubregionNames()
    RegionNames(NRegion+1)    = 'ENTIRE MODEL AREA'
    pAgLWUseBudRawFile_New    => AgLWUseBudRawFile_New
    pAgRootZoneBudRawFile_New => AgRootZoneBudRawFile_New

    !Allocate memory
    ALLOCATE (RootZone%ElemSoilsData(NElements)                          , &
              RootZone%HydCondPonded(NElements)                          , &
              RootZone%ElemPrecipData(NElements)                         , &
              RootZone%ElemSupply(NElements)                             , &
              RootZone%ElemDevelopedArea(NElements)                      , &
              RootZone%Ratio_ElemSupplyToRegionSupply_Ag(NElements)      , &
              RootZone%Ratio_ElemSupplyToRegionSupply_Urb(NElements)     , &
              RootZone%RSoilM_P(NRegion+1,3)                             , &  !2nd Dim: 1 = Ag; 2 = Urban; 3 = Native & Riparain Veg
              RootZone%RSoilM(NRegion+1,3)                               , &  !2nd Dim: 1 = Ag; 2 = Urban; 3 = Native & Riparain Veg
              RootZone%Flags%lLakeElems(NElements)                       , &
              RootZone%iColSurfaceFlowDestination_Ag(NElements)          , &
              RootZone%iColSurfaceFlowDestination_UrbIndoors(NElements)  , &
              RootZone%iColSurfaceFlowDestination_UrbOutdoors(NElements) , &
              RootZone%iColSurfaceFlowDestination_NVRV(NElements)        , &
              RootZone%rAW_UrbanIndoors(NElements)                       , &
              STAT=ErrorCode                                             )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for root zone soils data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Stream node and lake IDs
    IF (PRESENT(iStrmNodeIDs)) THEN
        ALLOCATE(RootZone%iStrmNodeIDs(SIZE(iStrmNodeIDs)))
        RootZone%iStrmNodeIDs = iStrmNodeIDs
        RootZone%lStrmNodeIDs_Provided = .TRUE.
    ELSE
        ALLOCATE(RootZone%iStrmNodeIDs(0))
        RootZone%lStrmNodeIDs_Provided = .FALSE.
    END IF
    IF (PRESENT(iLakeIDs)) THEN
        ALLOCATE(RootZone%iLakeIDs(SIZE(iLakeIDs)))
        RootZone%iLakeIDs = iLakeIDs
        RootZone%lLakeIDs_Provided = .TRUE.
    ELSE
        ALLOCATE(RootZone%iLakeIDs(0))
        RootZone%lLakeIDs_Provided = .FALSE.
    END IF
    
    !Initialize lake element flag and urban indoors applied water
    RootZone%Flags%lLakeElems = .FALSE.
    RootZone%rAW_UrbanIndoors = 0.0
    
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
            CALL SetLastMessage('Flag to simulate root water uptake from groundwater is not recognized!',f_iFatal,ThisProcedure)
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
    CALL RootZone%NonPondedAgRootZone%New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,FactCN,AppGrid,iElemIDs,TimeStep,NTIME,cVersionLocal,iStat,pAgLWUseBudRawFile_New,pAgRootZoneBudRawFile_New)
    IF (iStat .EQ. -1) RETURN
       
    !Rice/refuge data file
    CALL RootZoneParamFile%ReadData(RiceRefugeFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    RiceRefugeFile = StripTextUntilCharacter(RiceRefugeFile,'/') 
    CALL CleanSpecialCharacters(RiceRefugeFile)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(RiceRefugeFile)),cWorkingDirectory,cAbsPathFileName)
    CALL RootZone%PondedAgRootZone%New(IsForInquiry,cAbsPathFileName,cWorkingDirectory,FactCN,AppGrid,iElemIDs,TimeStep,NTIME,cVersionLocal,iStat,pAgLWUseBudRawFile_New,pAgRootZoneBudRawFile_New)
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
    IF (PRESENT(iStrmNodeIDs)) THEN
        CALL RootZone%NVRVRootZone%New(cAbsPathFileName,cWorkingDirectory,FactCN,NElements,NRegion,iElemIDs,TrackTime,iStat,iStrmNodeIDs)
    ELSE
        CALL RootZone%NVRVRootZone%New(cAbsPathFileName,cWorkingDirectory,FactCN,NElements,NRegion,iElemIDs,TrackTime,iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    
    !Check if at least one type of land use is specified
    IF (NonPondedCropFile .EQ. '' ) THEN
        IF (RiceRefugeFile .EQ. '' ) THEN
            IF (UrbanDataFile .EQ. '') THEN
                IF (NVRVFile .EQ. '')  THEN
                    MessageArray(1) = 'At least one type of land use and related data should '
                    MessageArray(2) = 'be specified for the simulation of root zone processes!' 
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END IF
        END IF
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
        CALL RootZone%ReturnFracFile%Init(cAbsPathFileName,cWorkingDirectory,'Return flow fractions data file',TrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
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
        CALL RootZone%ReuseFracFile%Init(cAbsPathFileName,cWorkingDirectory,'Irrigation water re-use factors file',TrackTime,1,.FALSE.,rDummyFactor,iStat=iStat)  
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
        GenericMoistureFile = GenericMoistureFile
        RootZone%Flags%lGenericMoistureFile_Defined = .TRUE.
    END IF
    
    !Agricultural water demand file
    CALL RootZoneParamFile%ReadData(AgWaterDemandFile,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    AgWaterDemandFile = StripTextUntilCharacter(AgWaterDemandFile,'/') 
    CALL CleanSpecialCharacters(AgWaterDemandFile)
    IF (AgWaterDemandFile .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(AgWaterDemandFile)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%AgWaterDemandFile%Init(cAbsPathFileName,cWorkingDirectory,'Agricultural water supply requirement file',TrackTime,1,.TRUE.,rDummyFactor,[.TRUE.],iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        RootZone%AgWaterDemandFactor = rDummyFactor(1)
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
       
    !Land and water use zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LWUseZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,cVersionLocal,RootZone%Flags,AppGrid,RootZone%LWUZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%LWUseZoneBudRawFile_Defined = .TRUE.      
    END IF

    !Root zone zone budget HDF5 output file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZoneZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,cVersionLocal,RootZone%Flags,AppGrid,RootZone%RootZoneZoneBudRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
        RootZone%Flags%RootZoneZoneBudRawFile_Defined = .TRUE.
    END IF
       
    !Conversion factors soil parameters
    CALL RootZoneParamFile%ReadData(FACTK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(FACTEXDTH,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)
    RootZone%VarTimeUnit = ADJUSTL(StripTextUntilCharacter(ALine,'/'))

    !Surface flow destinations file
    CALL RootZoneParamFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        CALL SetLastMessage('Surface flow destinations data file is missing for the Root Zone component!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    ELSE
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL RootZone%SurfaceFlowDestinationFile%Init(cAbsPathFileName,cWorkingDirectory,'Surface flow destinations data file',.TRUE.,1,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Read soil parameters and surface flow destinations
    CALL RootZoneParamFile%ReadData(rDummyArray16,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ASSOCIATE (pSoilsData  => RootZone%ElemSoilsData  , &
               pPrecipData => RootZone%ElemPrecipData )
        lProcessed = .FALSE.
        DO indxElem=1,NElements
            iElemID = INT(rDummyArray16(indxElem,1))
            
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
            lProcessed(iElem)                                      = .TRUE.
            pSoilsData(iElem)%WiltingPoint                         =     rDummyArray16(indxElem,2)      
            pSoilsData(iElem)%FieldCapacity                        =     rDummyArray16(indxElem,3)
            pSoilsData(iElem)%TotalPorosity                        =     rDummyArray16(indxElem,4)
            pSoilsData(iElem)%Lambda                               =     rDummyArray16(indxElem,5)
            pSoilsData(iElem)%HydCond                              =     rDummyArray16(indxElem,6) * FACTK * TimeStep%DeltaT
            RootZone%HydCondPonded(iElem)                          =     rDummyArray16(indxElem,7)
            pSoilsData(iElem)%KunsatMethod                         = INT(rDummyArray16(indxElem,8))
            pSoilsData(iElem)%CapillaryRise                        =     rDummyArray16(indxElem,9) * FACTEXDTH
            pPrecipData(iElem)%iColPrecip                          = INT(rDummyArray16(indxElem,10))
            pPrecipData(iElem)%PrecipFactor                        =     rDummyArray16(indxElem,11)
            iColGenericMoisture(iElem)                             = INT(rDummyArray16(indxElem,12))
            RootZone%iColSurfaceFlowDestination_Ag(iElem)          = INT(rDummyArray16(indxElem,13))
            RootZone%iColSurfaceFlowDestination_UrbIndoors(iElem)  = INT(rDummyArray16(indxElem,14))
            RootZone%iColSurfaceFlowDestination_UrbOutdoors(iElem) = INT(rDummyArray16(indxElem,15))
            RootZone%iColSurfaceFlowDestination_NVRV(iElem)        = INT(rDummyArray16(indxElem,16))
           
            !Process ponded Ksat
            IF (RootZone%HydCondPonded(iElem) .EQ. -1.0) THEN
                RootZone%HydCondPonded(iElem) = pSoilsData(iElem)%HydCond
            ELSE
                RootZone%HydCondPonded(iElem) = RootZone%HydCondPonded(iElem) * FACTK * TimeStep%DeltaT
            END IF

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
    
    !Check if time series data column pointers are referring to existing data columns
    CALL CheckTSDataPointers(RootZone,iElemIDs,Precip,ET,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL RootZoneParamFile%Kill()
    
  END SUBROUTINE RootZone_v412_New

  
  ! -------------------------------------------------------------
  ! --- NEW BINARY LAND AND WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE LWUseBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 6   , &
                              f_iTitleLen           = 215 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNLWUseBudColumns) = ['AREA'   , &
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
                                                             'VOLUME' ]
    CHARACTER(LEN=13)      :: FParts(f_iNLWUseBudColumns) = ['AG_AREA'         ,&
                                                             'AG_POTNL_CUAW'   ,&
                                                             'AG_SUP_REQ'      ,&    
                                                             'AG_PUMPING'      ,&
                                                             'AG_DELIVERY'     ,&
                                                             'AG_SHORTAGE'     ,&
                                                             'AG_ETAW'         ,&
                                                             'AG_EFF_PRECIP'   ,&
                                                             'AG_ET_GW'        ,&
                                                             'AG_ET_OTH'       ,&
                                                             'URB_AREA'        ,&
                                                             'URB_SUP_REQ'     ,&       
                                                             'URB_PUMPING'     ,&
                                                             'URB_DELIVERY'    ,&
                                                             'URB_SHORTAGE'    ]
    
    !Initialize
    iStat = 0
    
    !Instantiate the land and water use raw file for when it is openned for inquiry
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
      pASCIIOutput%TitleLen           = f_iTitleLen
      pASCIIOutput%NTitles            = f_iNTitles
      ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
      pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//f_cVolumeUnitMarker_Budget//' FOR '//f_cLocationNameMarker_Budget , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker_Budget//' '//f_cAreaUnitMarker_Budget , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(5)         = REPEAT(' ',73)//'Agricultural Area'//REPEAT(' ',87)//'Urban Area'
      pASCIIOutput%cTitles(6)         = REPEAT(' ',18)//REPEAT('-',128)//REPEAT(' ',4)//REPEAT('-',65)
      pASCIIOutput%lTitlePersist(1:3) = .TRUE.
      pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,10(F12.1,1X),3X,5(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                             , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNLWUseBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNLWUseBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNLWUseBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNLWUseBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)             )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns           = f_iNLWUseBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                               
      pLocation%cFullColumnHeaders(2:) = f_cLWUseBudgetColumnTitles                               
      pLocation%cFullColumnHeaders(2)  = TRIM(pLocation%cFullColumnHeaders(2))  // ' ('//f_cAreaUnitMarker_Budget//')'    
      pLocation%cFullColumnHeaders(13) = TRIM(pLocation%cFullColumnHeaders(13)) // ' ('//f_cAreaUnitMarker_Budget//')'    
      pLocation%iDataColumnTypes       = [f_iAR_Budget          ,&  !Ag area
                                          f_iVR_lwu_PotCUAW     ,&  !Potential CUAW
                                          f_iVR_lwu_AgSupplyReq ,&  !Ag supply req.
                                          f_iVR_lwu_AgPump      ,&  !Pumping for ag
                                          f_iVR_lwu_AgDiv       ,&  !Deliveries for ag
                                          f_iVR_lwu_AgShort     ,&  !Ag supply shortage
                                          f_iVR_Budget          ,&  !ETAW
                                          f_iVR_Budget          ,&  !ETP
                                          f_iVR_Budget          ,&  !ETGW
                                          f_iVR_Budget          ,&  !ETOth
                                          f_iAR_Budget          ,&  !Urban area
                                          f_iVR_Budget          ,&  !Urban supply req.
                                          f_iVR_Budget          ,&  !Pumping for urban
                                          f_iVR_Budget          ,&  !Deliveries for urban
                                          f_iVR_Budget          ]   !Urban supply shortage
      pLocation%iColWidth              = [17,12,14,(13,indxCol=1,8),12,14,(13,indxCol=1,3)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker_Budget)//')'
        pColumnHeaders(:,1) = ['                 ','            ','    Potential ',' Agricultural','             ','             ','             ','             ','             ','      ET     ','      ET     ','            ','     Urban    ','             ','             ','             ']
        pColumnHeaders(:,2) = ['      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping','  Deliveries ','     Shortage','             ','   Effective ','     from    ','  from Other ','        Area','     Supply   ','      Pumping','  Deliveries ','     Shortage']
        pColumnHeaders(:,3) = [               Text,         Text1,'              ','  Requirement','        (-)  ','      (-)    ','       (=)   ','       ETAW  ','    Precip   ','  Groundwater','   Sources   ',         Text1,'   Requirement','        (-)  ','      (-)    ','       (=)   ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,8A13,3X,A12,A14,3A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,8A13,3X,A12,A14,3A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,8A13,3X,A12,A14,3A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',f_iTitleLen)//'",'//TRIM(IntToText(f_iNLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,9),f_iPER_AVER,(f_iPER_CUM,indxCol=1,4)]
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE LWUseBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW HDF5 LAND AND WATER USE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE LWUseZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,cVersion,Flags,AppGrid,ZBudFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(FlagsType),INTENT(IN)    :: Flags
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ZBudgetType)             :: ZBudFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'LWUseZoneBudRawFile_New'
    CHARACTER(LEN=13),PARAMETER            :: cArea = f_cMarkerChar_ZBudget // '        (' // f_cAreaUnitMarker_ZBudget // ')' // f_cMarkerChar_ZBudget
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
    ALLOCATE (SystemData%iElementIDs(AppGrid%NElements)                   , &
              SystemData%iElementNNodes(AppGrid%NElements)                , &
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
    SystemData%iElementNNodes = AppGrid%NVertex
    DO indxElem=1,AppGrid%NElements
        SystemData%iElementIDs(indxElem)     = AppGrid%AppElement(indxElem)%ID
        SystemData%iElementNodes(:,indxElem) = AppGrid%Vertex(:,indxElem)
        DO indxVertex=1,AppGrid%NVertex(indxElem)
            SystemData%rElementNodeAreas(indxVertex,indxElem)         = AppGrid%AppElement(indxElem)%VertexArea(indxVertex)
            SystemData%rElementNodeAreaFractions(indxVertex,indxElem) = AppGrid%AppElement(indxElem)%VertexAreaFraction(indxVertex)
        END DO
        IF (AppGrid%NVertex(indxElem) .EQ. 3) THEN
            SystemData%rElementNodeAreas(4,indxElem)         = 0.0
            SystemData%rElementNodeAreaFractions(4,indxElem) = 0.0
        END IF
    END DO
    DO indxFace=1,AppGrid%NFaces
        SystemData%iFaceElems(:,indxFace) = AppGrid%AppFace%Element(:,indxFace)
    END DO
    SystemData%lBoundaryFace = AppGrid%AppFace%BoundaryFace
    SystemData%lActiveNode   = .TRUE.
    
    !Compile Header data
    Header%cSoftwareVersion   = 'IWFM ROOT ZONE PACKAGE (' // TRIM(cVersion) // ')'
    Header%cDescriptor        = 'Land and water use zone budget'
    Header%lFaceFlows_Defined = .FALSE.
    Header%lStorages_Defined  = .FALSE.
    Header%lComputeError      = .FALSE.
    Header%iNData             = f_iNLWUseZBudColumns
    ALLOCATE (Header%iDataTypes(f_iNLWUseZBudColumns)                           , &
              Header%cFullDataNames(f_iNLWUseZBudColumns)                       , &
              Header%cDataHDFPaths(f_iNLWUseZBudColumns)                        , &
              Header%iNDataElems(f_iNLWUseZBudColumns,1)                        , &
              Header%iElemDataColumns(AppGrid%NElements,f_iNLWUseZBudColumns,1) , &
              !Header%iErrorInCols()                                            , &  ! Since mass balance error is not calcuated no need
              !Header%iErrorOutCols()                                           , &  !  to allocate these arrays
              Header%cDSSFParts(f_iNLWUseZBudColumns)                           , &
              Header%ASCIIOutput%cColumnTitles(5)                               , &
              STAT = ErrorCode                                                  )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for land and water use Z-Budget file!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Header%iDataTypes = [f_iAR_ZBudget         ,&  !Non-ponded ag area                 1
                         f_iVR_lwu_PotCUAW     ,&  !Non-ponded potential CUAW          2
                         f_iVR_lwu_AgSupplyReq ,&  !Non-ponded ag supply req.          3
                         f_iVR_lwu_AgPump      ,&  !Non-ponded ag pumping              4
                         f_iVR_lwu_AgDiv       ,&  !Non-ponded ag deliveries           5
                         f_iVR_lwu_AgShort     ,&  !Non-ponded ag supply shortage      6
                         f_iVR_ZBudget         ,&  !Non-ponded ETAW                    7
                         f_iVR_ZBudget         ,&  !Non-ponded ETP                     8
                         f_iVR_ZBudget         ,&  !Non-ponded ETGW                    9
                         f_iVR_ZBudget         ,&  !Non-ponded ETOth                   10
                         f_iAR_ZBudget         ,&  !Rice area                          11
                         f_iVR_lwu_PotCUAW     ,&  !Rice potential CUAW                12
                         f_iVR_lwu_AgSupplyReq ,&  !Rice supply req.                   13
                         f_iVR_lwu_AgPump      ,&  !Rice pumping                       14
                         f_iVR_lwu_AgDiv       ,&  !Rice deliveries                    15
                         f_iVR_lwu_AgShort     ,&  !Rice supply shortage               16
                         f_iVR_ZBudget         ,&  !Rice ETAW                          17
                         f_iVR_ZBudget         ,&  !Rice ETP                           18
                         f_iVR_ZBudget         ,&  !Rice ETGW                          19
                         f_iVR_ZBudget         ,&  !Rice ETOth                         20
                         f_iAR_ZBudget         ,&  !Refuge area                        21
                         f_iVR_lwu_PotCUAW     ,&  !Refuge potential CUAW              22
                         f_iVR_lwu_AgSupplyReq ,&  !Refuge supply req.                 23
                         f_iVR_lwu_AgPump      ,&  !Refuge pumping                     24
                         f_iVR_lwu_AgDiv       ,&  !Refuge deliveries                  25
                         f_iVR_lwu_AgShort     ,&  !Refuge supply shortage             26
                         f_iVR_ZBudget         ,&  !Refuge ETAW                        27
                         f_iVR_ZBudget         ,&  !Refuge ETP                         28
                         f_iVR_ZBudget         ,&  !Refuge ETGW                        29
                         f_iVR_ZBudget         ,&  !Refuge ETOth                       30
                         f_iAR_ZBudget         ,&  !Urban area                         31
                         f_iVR_ZBudget         ,&  !Urban supply req.                  32
                         f_iVR_ZBudget         ,&  !Pumping for urban                  33
                         f_iVR_ZBudget         ,&  !Deliveries for urban               34
                         f_iVR_ZBudget         ]   !Urban supply shortage              35
    Header%cFullDataNames     = f_cLWUseZBudgetColumnTitles
    Header%cFullDataNames(1)  = TRIM(Header%cFullDataNames(1))  // cArea 
    Header%cFullDataNames(11) = TRIM(Header%cFullDataNames(11)) // cArea  
    Header%cFullDataNames(21) = TRIM(Header%cFullDataNames(21)) // cArea  
    Header%cFullDataNames(31) = TRIM(Header%cFullDataNames(31)) // cArea  
    Header%cDataHDFPaths      = f_cLWUseZBudgetColumnTitles
    
    !Non-ponded ag data
    IF (Flags%lNonPondedAg_Defined) THEN
        Header%iNDataElems(1:11,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,1:10,:) = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(9,:)        = 0
            Header%iElemDataColumns(:,9,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(10,:)        = 0
            Header%iElemDataColumns(:,10,:) = 0
        END IF
    ELSE
        Header%iNDataElems(1:10,:)        = 0
        Header%iElemDataColumns(:,1:10,:) = 0
    END IF
    
    !Rice and refuge data
    IF (Flags%lPondedAg_Defined) THEN
        !Rice
        Header%iNDataElems(11:20,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,11:20,:) = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(19,:)        = 0
            Header%iElemDataColumns(:,19,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(20,:)        = 0
            Header%iElemDataColumns(:,20,:) = 0
        END IF
        
        !Refuge
        Header%iNDataElems(21:30,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,21:30,:) = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(29,:)        = 0
            Header%iElemDataColumns(:,29,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(30,:)        = 0
            Header%iElemDataColumns(:,30,:) = 0
        END IF
    ELSE
        Header%iNDataElems(11:30,:)        = 0
        Header%iElemDataColumns(:,11:30,:) = 0
    END IF
    
    !Urban data    
    IF (Flags%lUrban_Defined) THEN
        Header%iNDataElems(31:,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,31:,:) = indxElem
        END DO
    ELSE
        Header%iNDataElems(31:,:)        = 0
        Header%iElemDataColumns(:,31:,:) = 0
    END IF
    
    !ASCII output titles
    Header%ASCIIOutput%iNTitles         = 5
    Header%ASCIIOutput%iLenColumnTitles = 480
    Header%ASCIIOutput%cColumnTitles(1) = '                                                                    Non-Ponded Agricultural Area                                                                                                                 Rice Area                                                                                                                           Refuge Area                                                                                          Urban Area                            '
    Header%ASCIIOutput%cColumnTitles(2) = '                 ---------------------------------------------------------------------------------------------------------------------------------   ----------------------------------------------------------------------------------------------------------------------------------   ----------------------------------------------------------------------------------------------------------------------------------   -----------------------------------------------------------------'
    Header%ASCIIOutput%cColumnTitles(3) = '                                            Agricultural                                                                      ET            ET                                   Agricultural                                                                      ET            ET                                      Water                                                                          ET            ET                          Urban                                         '
    Header%ASCIIOutput%cColumnTitles(4) = '      Time               Area    Potential     Supply         Pumping  Deliveries      Shortage                Effective     from       from Other            Area    Potential     Supply         Pumping  Deliveries      Shortage                Effective     from       from Other            Area    Potential     Supply         Pumping  Deliveries      Shortage                Effective     from       from Other            Area      Supply        Pumping  Deliveries     Shortage'
    Header%ASCIIOutput%cColumnTitles(5) = '                 '  //cArea//'     CUAW      Requirement        (-)        (-)            (=)          ETAW      Precip   Groundwater     Sources     '  //cArea//'     CUAW      Requirement        (-)        (-)            (=)          ETAW      Precip   Groundwater     Sources     '  //cArea//'     CUAW      Requirement        (-)        (-)            (=)          ETAW      Precip   Groundwater     Sources     '  //cArea//'   Requirement       (-)        (-)           (=)  '
    Header%ASCIIOutput%cNumberFormat    = '(A16,10(2X,F11.1),3X,10(2X,F11.1),3X,10(2X,F11.1),3X,5(2X,F11.1))'
    
    !DSS output pathnames
    Header%cDSSFParts = ['NP_AG_AREA'           ,&
                         'NP_AG_POTNL_CUAW'     ,&
                         'NP_AG_SUP_REQ'        ,&    
                         'NP_AG_PUMPING'        ,&
                         'NP_AG_DELIVERY'       ,&
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
                         'REFUGE_SHORTAGE'      ,&
                         'REFUGE_ETAW'          ,&
                         'REFUGE_EFF_PRECIP'    ,&
                         'REFUGE_ET_GW'         ,&
                         'REFUGE_ET_OTH'        ,&
                         'URB_AREA'             ,&
                         'URB_SUP_REQ'          ,&       
                         'URB_PUMPING'          ,&
                         'URB_DELIVERY'         ,&
                         'URB_SHORTAGE'         ]
                             
    !Instantiate Z-Budget file
    CALL ZBudFile%New(cFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE LWUseZoneBudRawFile_New

    
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
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 6   , &
                              f_iTitleLen           = 908 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNRootZoneBudColumns) = ['AREA'   , &   !Ag. Area                                                
                                                                'VOLUME' , &   !Ag. Potential ET                                        
                                                                'VOLUME' , &   !Ag. Precipitation                                       
                                                                'VOLUME' , &   !Ag. Runoff                                              
                                                                'VOLUME' , &   !Ag. Prime Applied Water                                 
                                                                'VOLUME' , &   !Ag. Reused Water                                        
                                                                'VOLUME' , &   !Ag. Net Return Flow                                     
                                                                'VOLUME' , &   !Ag. Surface Flow for GW Recharge (Ag)                            
                                                                'VOLUME' , &   !Ag. Surface Flow for GW Recharge (UrbIn)                            
                                                                'VOLUME' , &   !Ag. Surface Flow for GW Recharge (UrbOut)                            
                                                                'VOLUME' , &   !Ag. Surface Flow for GW Recharge (NRV)                            
                                                                'VOLUME' , &   !Ag. Beginning Storage (+)                               
                                                                'VOLUME' , &   !Ag. Net Gain from Land Expansion (+)                    
                                                                'VOLUME' , &   !Ag. Infiltration (+)                                    
                                                                'VOLUME' , &   !Ag. Groundwater Inflow (+)                              
                                                                'VOLUME' , &   !Ag. Other Inflow (+)                                    
                                                                'VOLUME' , &   !Ag. Pond Drain (-)                                      
                                                                'VOLUME' , &   !Ag. Actual ET (-)                                       
                                                                'VOLUME' , &   !Ag. Percolation (-)                                     
                                                                'VOLUME' , &   !Ag. Ending Storage (-)                                  
                                                                'VOLUME' , &   !Ag. Discrepancy (=)                                     
                                                                'AREA'   , &   !Urban Area                                              
                                                                'VOLUME' , &   !Urban Potential ET                                      
                                                                'VOLUME' , &   !Urban Precipitation                                     
                                                                'VOLUME' , &   !Urban Runoff                                            
                                                                'VOLUME' , &   !Urban Prime Applied Water                               
                                                                'VOLUME' , &   !Urban Reused Water                                      
                                                                'VOLUME' , &   !Urban Net Return Flow                                   
                                                                'VOLUME' , &   !Urban Surface Flow for GW Recharge (Ag)                            
                                                                'VOLUME' , &   !Urban Surface Flow for GW Recharge (UrbIn)                            
                                                                'VOLUME' , &   !Urban Surface Flow for GW Recharge (UrbOut)                            
                                                                'VOLUME' , &   !Urban Surface Flow for GW Recharge (NRV)                            
                                                                'VOLUME' , &   !Urban Beginning Storage (+)                             
                                                                'VOLUME' , &   !Urban Net Gain from Land Expansion (+)                  
                                                                'VOLUME' , &   !Urban Infiltration (+)                                  
                                                                'VOLUME' , &   !Urban Groundwater Inflow (+)                            
                                                                'VOLUME' , &   !Urban Other Inflow (+)                                  
                                                                'VOLUME' , &   !Urban Actual ET (-)                                     
                                                                'VOLUME' , &   !Urban Percolation (-)                                   
                                                                'VOLUME' , &   !Urban Ending Storage (-)                                
                                                                'VOLUME' , &   !Urban Discrepancy (=)                                   
                                                                'AREA'   , &   !Native&Riparian Veg. Area                               
                                                                'VOLUME' , &   !Native&Riparian Veg. Potential ET                       
                                                                'VOLUME' , &   !Native&Riparian Veg. Precipitation                      
                                                                'VOLUME' , &   !Native&Riparian Veg. Runoff                             
                                                                'VOLUME' , &   !Native&Riparian Surface Flow for GW Recharge (Ag)                            
                                                                'VOLUME' , &   !Native&Riparian Surface Flow for GW Recharge (UrbIn)                            
                                                                'VOLUME' , &   !Native&Riparian Surface Flow for GW Recharge (UrbOut)                            
                                                                'VOLUME' , &   !Native&Riparian Surface Flow for GW Recharge (NRV)                            
                                                                'VOLUME' , &   !Native&Riparian Veg. Beginning Storage (+)              
                                                                'VOLUME' , &   !Native&Riparian Veg. Net Gain from Land Expansion (+)   
                                                                'VOLUME' , &   !Native&Riparian Veg. Infiltration (+)                   
                                                                'VOLUME' , &   !Native&Riparian Veg. Groundwater Inflow (+)             
                                                                'VOLUME' , &   !Native&Riparian Veg. Other Inflow (+)                   
                                                                'VOLUME' , &   !Native&Riparian Veg. Stream Inflow for ET (+)           
                                                                'VOLUME' , &   !Native&Riparian Veg. Actual ET (-)                      
                                                                'VOLUME' , &   !Native&Riparian Veg. Percolation (-)                    
                                                                'VOLUME' , &   !Native&Riparian Veg. Ending Storage (-)                 
                                                                'VOLUME' ]     !Native&Riparian Veg. Discrepancy (=) 
    CHARACTER(LEN=15)      :: FParts(f_iNRootZoneBudColumns) = ['AG_AREA'            ,&      !Ag. Area                                                                            
                                                                'AG_POT_ET'          ,&      !Ag. Potential ET                                        
                                                                'AG_PRECIP'          ,&      !Ag. Precipitation                                       
                                                                'AG_RUNOFF'          ,&      !Ag. Runoff                                              
                                                                'AG_PRM_H2O'         ,&      !Ag. Prime Applied Water                                 
                                                                'AG_RE-USE'          ,&      !Ag. Reused Water                                        
                                                                'AG_NT_RTRN_FLW'     ,&      !Ag. Net Return Flow                                     
                                                                'AG_FLW_GW_AG'       ,&      !Ag. Surface Flow for GW Recharge (Ag)                            
                                                                'AG_FLW_GW_URIN'     ,&      !Ag. Surface Flow for GW Recharge (UrbIn)                            
                                                                'AG_FLW_GW_UROT'     ,&      !Ag. Surface Flow for GW Recharge (UrbOut)                            
                                                                'AG_FLW_GW_NRV'      ,&      !Ag. Surface Flow for GW Recharge (NrV)                            
                                                                'AG_BEGIN_STOR'      ,&      !Ag. Beginning Storage (+)                               
                                                                'AG_GAIN_EXP'        ,&      !Ag. Net Gain from Land Expansion (+)                    
                                                                'AG_INFILTR'         ,&      !Ag. Infiltration (+)                                    
                                                                'AG_GW_INFLW'        ,&      !Ag. Groundwater Inflow (+)                              
                                                                'AG_OTHER_INFLW'     ,&      !Ag. Other Inflow (+)                                    
                                                                'AG_DRAIN'           ,&      !Ag. Pond Drain (-)                                      
                                                                'AG_ET'              ,&      !Ag. Actual ET (-)                                       
                                                                'AG_PERC'            ,&      !Ag. Percolation (-)                                     
                                                                'AG_END_STOR'        ,&      !Ag. Ending Storage (-)                                  
                                                                'AG_DISCREPANCY'     ,&      !Ag. Discrepancy (=)                                     
                                                                'URB_AREA'           ,&      !Urban Area                                              
                                                                'URB_POT_ET'         ,&      !Urban Potential ET                                      
                                                                'URB_PRECIP'         ,&      !Urban Precipitation                                     
                                                                'URB_RUNOFF'         ,&      !Urban Runoff                                            
                                                                'URB_PRM_H2O'        ,&      !Urban Prime Applied Water                               
                                                                'URB_RE-USE'         ,&      !Urban Reused Water                                      
                                                                'URB_NT_RTRN_FLW'    ,&      !Urban Net Return Flow                                   
                                                                'URB_FLW_GW_AG'      ,&      !Urban Surface Flow for GW Recharge (Ag)                        
                                                                'URB_FLW_GW_URIN'    ,&      !Urban Surface Flow for GW Recharge (UrbIn)                        
                                                                'URB_FLW_GW_UROT'    ,&      !Urban Surface Flow for GW Recharge (UrbOut)                        
                                                                'URB_FLW_GW_NRV'     ,&      !Urban Surface Flow for GW Recharge (NRV)                        
                                                                'URB_BEGIN_STOR'     ,&      !Urban Beginning Storage (+)                             
                                                                'URB_GAIN_EXP'       ,&      !Urban Net Gain from Land Expansion (+)                  
                                                                'URB_INFILTR'        ,&      !Urban Infiltration (+)                                  
                                                                'URB_GW_INFLW'       ,&      !Urban Groundwater Inflow (+)                            
                                                                'URB_OTHER_INFLW'    ,&      !Urban Other Inflow (+)                                  
                                                                'URB_ET'             ,&      !Urban Actual ET (-)                                     
                                                                'URB_PERC'           ,&      !Urban Percolation (-)                                   
                                                                'URB_END_STOR'       ,&      !Urban Ending Storage (-)                                
                                                                'URB_DISCREPANCY'    ,&      !Urban Discrepancy (=)                                   
                                                                'NRV_AREA'           ,&      !Native&Riparian Veg. Area                               
                                                                'NRV_POT_ET'         ,&      !Native&Riparian Veg. Potential ET                       
                                                                'NRV_PRECIP'         ,&      !Native&Riparian Veg. Precipitation                      
                                                                'NRV_RUNOFF'         ,&      !Native&Riparian Veg. Runoff                             
                                                                'NRV_FLW_GW_AG'      ,&      !Native&Riparian Veg. Surface Flow for GW Recharge (Ag)          
                                                                'NRV_FLW_GW_URIN'    ,&      !Native&Riparian Veg. Surface Flow for GW Recharge (UrbIn)     
                                                                'NRV_FLW_GW_UROT'    ,&      !Native&Riparian Veg. Surface Flow for GW Recharge (UrbOut)         
                                                                'NRV_FLW_GW_NRV'     ,&      !Native&Riparian Veg. Surface Flow for GW Recharge (NRV)         
                                                                'NRV_BEGIN_STOR'     ,&      !Native&Riparian Veg. Beginning Storage (+)              
                                                                'NRV_GAIN_EXP'       ,&      !Native&Riparian Veg. Net Gain from Land Expansion (+)   
                                                                'NRV_INFILTR'        ,&      !Native&Riparian Veg. Infiltration (+)                   
                                                                'NRV_GW_INFLW'       ,&      !Native&Riparian Veg. Groundwater Inflow (+)             
                                                                'NRV_OTHER_INFLW'    ,&      !Native&Riparian Veg. Other Inflow (+)                   
                                                                'NRV_STRM_IN_ET'     ,&      !Native&Riparian Veg. Stream Inflow for ET (+)           
                                                                'NRV_ET'             ,&      !Native&Riparian Veg. Actual ET (-)                      
                                                                'NRV_PERC'           ,&      !Native&Riparian Veg. Percolation (-)                    
                                                                'NRV_END_STOR'       ,&      !Native&Riparian Veg. Ending Storage (-)                 
                                                                'NRV_DISCREPANCY'    ]       !Native&Riparian Veg. Discrepancy (=) 
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
        pASCIIOutput%TitleLen = f_iTitleLen
        pASCIIOutput%NTitles  = f_iNTitles
          ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
          pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//f_cVolumeUnitMarker_Budget//' FOR '//f_cLocationNameMarker_Budget , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker_Budget//' '//f_cAreaUnitMarker_Budget , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(5)         = REPEAT(' ',166)//'Agricultural Area'//REPEAT(' ',297)//'Urban Area'//REPEAT(' ',266)//'Native & Riparian Vegetation Area'
          pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',315)//REPEAT(' ',3)//REPEAT('-',300)//REPEAT(' ',3)//REPEAT('-',270)
          pASCIIOutput%lTitlePersist(1:3) = .TRUE.
          pASCIIOutput%lTitlePersist(4:6) = .FALSE.
        pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,21(F14.1,1X),3X,20(F14.1,1X),3X,18(F14.1,1X))')
        pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                                , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNRootZoneBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNRootZoneBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNRootZoneBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNRootZoneBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)                )
    ASSOCIATE (pLocation => OutputData%Locations(1))
        pLocation%NDataColumns           = f_iNRootZoneBudColumns
        pLocation%cFullColumnHeaders(1)  = 'Time' 
        pLocation%cFullColumnHeaders(2:) =  f_cRootZoneBudgetColumnTitles
        pLocation%cFullColumnHeaders(2)  =  'Ag. Area ('//f_cAreaUnitMarker_Budget//')'
        pLocation%cFullColumnHeaders(23) =  'Urban Area ('//f_cAreaUnitMarker_Budget//')'
        pLocation%cFullColumnHeaders(43) =  'Native&Riparian Veg. Area ('//f_cAreaUnitMarker_Budget//')'
        pLocation%iDataColumnTypes  =  [f_iAR_Budget ,&  !Ag area
                                        f_iVR_Budget ,&  !Ag potential ET
                                        f_iVR_Budget ,&  !Ag precipitation
                                        f_iVR_Budget ,&  !Ag runoff
                                        f_iVR_Budget ,&  !Ag prime applied water
                                        f_iVR_Budget ,&  !Ag re-used water
                                        f_iVR_Budget ,&  !Ag return flow
                                        f_iVR_Budget ,&  !Ag surface flow for gw recharge (Ag)
                                        f_iVR_Budget ,&  !Ag surface flow for gw recharge (UrbIn)
                                        f_iVR_Budget ,&  !Ag surface flow for gw recharge (UrbOut)
                                        f_iVR_Budget ,&  !Ag surface flow for gw recharge (NRV)
                                        f_iVLB_Budget,&  !Ag beginning storage
                                        f_iVR_Budget ,&  !Ag net gain from land expansion
                                        f_iVR_Budget ,&  !Ag infiltration
                                        f_iVR_Budget ,&  !Ag groundwater inflow
                                        f_iVR_Budget ,&  !Ag generic inflow
                                        f_iVR_Budget ,&  !Ag pond drain
                                        f_iVR_Budget ,&  !Ag actual ET
                                        f_iVR_Budget ,&  !Ag perc
                                        f_iVLE_Budget,&  !Ag ending storage
                                        f_iVR_Budget ,&  !Ag discrepancy
                                        f_iAR_Budget ,&  !Urban area
                                        f_iVR_Budget ,&  !Urban potential ET
                                        f_iVR_Budget ,&  !Urban precipitation
                                        f_iVR_Budget ,&  !Urban runoff
                                        f_iVR_Budget ,&  !Urban prime applied water
                                        f_iVR_Budget ,&  !Urban re-used water
                                        f_iVR_Budget ,&  !Urban return flow
                                        f_iVR_Budget ,&  !Urban surface flow for gw recharge (Ag)
                                        f_iVR_Budget ,&  !Urban surface flow for gw recharge (UrbIn)
                                        f_iVR_Budget ,&  !Urban surface flow for gw recharge (UrbOut)
                                        f_iVR_Budget ,&  !Urban surface flow for gw recharge (NRV)
                                        f_iVLB_Budget,&  !Urban beginning storage
                                        f_iVR_Budget ,&  !Urban net gain from land expansion
                                        f_iVR_Budget ,&  !Urban infiltration
                                        f_iVR_Budget ,&  !Urban gw inflow
                                        f_iVR_Budget ,&  !Urban generic inflow
                                        f_iVR_Budget ,&  !Urban actual ET
                                        f_iVR_Budget ,&  !Urban perc
                                        f_iVLE_Budget,&  !Urban ending storage
                                        f_iVR_Budget ,&  !Urban discrepancy
                                        f_iAR_Budget ,&  !NV&RV area
                                        f_iVR_Budget ,&  !NV&RV potential ET
                                        f_iVR_Budget ,&  !NV&RV precipitation
                                        f_iVR_Budget ,&  !NV&RV runoff
                                        f_iVR_Budget ,&  !NV&RV surface flow for gw recharge (Ag)
                                        f_iVR_Budget ,&  !NV&RV surface flow for gw recharge (UrbIn)
                                        f_iVR_Budget ,&  !NV&RV surface flow for gw recharge (UrbOut)
                                        f_iVR_Budget ,&  !NV&RV surface flow for gw recharge (NRV)
                                        f_iVLB_Budget,&  !NV&RV beginning storage
                                        f_iVR_Budget ,&  !NV&RV net gain from land expansion
                                        f_iVR_Budget ,&  !NV&RV infiltration
                                        f_iVR_Budget ,&  !NV&RV gw inflow
                                        f_iVR_Budget ,&  !NV&RV generic inflow
                                        f_iVR_Budget ,&  !NV&RV stream inflow for ET 
                                        f_iVR_Budget ,&  !NV&RV actual ET
                                        f_iVR_Budget ,&  !NV&RV perc
                                        f_iVLE_Budget,&  !NV&RV ending storage
                                        f_iVR_Budget ]   !NV&RV discrepancy
        pLocation%iColWidth       = [17,14,15,16,(15,indxCol=1,18),14,15,16,(15,indxCol=1,17),14,15,16,(15,indxCol=1,15)]
        ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                   pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
            Text                = ArrangeText(TRIM(UnitT),17)
            Text1               = '('//TRIM(f_cAreaUnitMarker_Budget)//')'
            pColumnHeaders(:,1) = ['                 ','              ','               ','                ','               ','       Prime   ','         Reused','          Net  ','  Surface Flow ','  Surface Flow ','  Surface Flow ','  Surface Flow ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','          Pond ','         Actual','               ','        Ending ','               ','              ','               ','                ','               ','       Prime   ','         Reused','          Net  ','  Surface Flow ','  Surface Flow ','  Surface Flow ','  Surface Flow ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','         Actual','               ','        Ending ','               ','              ','               ','                ','               ','  Surface Flow ','  Surface Flow ','  Surface Flow ','  Surface Flow ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','  Stream Inflow','         Actual','               ','        Ending ','               ']
            pColumnHeaders(:,2) = ['      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ','         Water ','         Return','     to GW     ','     to GW     ','     to GW     ','     to GW     ','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','   Precipitation','         Runoff','      Applied  ','         Water ','         Return','     to GW     ','     to GW     ','     to GW     ','     to GW     ','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','  Precipitation ','        Runoff ','     to GW     ','     to GW     ','     to GW     ','     to GW     ','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','     for ET    ','           ET  ','    Percolation','        Storage','    Discrepancy']
            pColumnHeaders(:,3) = [               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','          Flow ','     (Ag)      ',' (Urban Indrs) ',' (Urban Outdrs)',' (Native Veg.) ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','       Water   ','               ','          Flow ','     (Ag)      ',' (Urban Indrs) ',' (Urban Outdrs)',' (Native Veg.) ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','     (Ag)      ',' (Urban Indrs) ',' (Urban Outdrs)',' (Native Veg.) ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','      (+)      ','           (-) ','       (-)     ','          (-)  ','        (=)    ']
            pColumnHeaders(:,4) = ''
            pFormatSpecs(1)     = '(A17,A14,A15,A16,18A15,3X,A14,A15,A16,17A15,3X,A14,A15,A16,15A15)'
            pFormatSpecs(2)     = '(A17,A14,A15,A16,18A15,3X,A14,A15,A16,17A15,3X,A14,A15,A16,15A15)'
            pFormatSpecs(3)     = '(A17,A14,A15,A16,18A15,3X,A14,A15,A16,17A15,3X,A14,A15,A16,15A15)'
            pFormatSpecs(4)     = '('//TRIM(IntToText(f_iTitleLen))//'(1H-),'//TRIM(IntToText(f_iNRootZoneBudColumns+1))//'A0)'
        END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
        ALLOCATE (pDSSOutput%cPathNames(f_iNRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNRootZoneBudColumns))
        iCount = 1
        DO indxLocation=1,NRegion
            DO indxCol=1,f_iNRootZoneBudColumns
                pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                                TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                                TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                                '/'                                                            //  &  !D part
                                                 TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                                TRIM(FParts(indxCol))//'/'                                            !F part
                iCount = iCount+1
            END DO
        END DO
        pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,20),f_iPER_AVER,(f_iPER_CUM,indxCol=1,19),f_iPER_AVER,(f_iPER_CUM,indxCol=1,17)]
    END ASSOCIATE
                                             
    !Instantiate the root zone budget file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
    !Free memory
    CALL OutputData%Kill()
    
  END SUBROUTINE RootZoneBudRawFile_New

  
  ! -------------------------------------------------------------
  ! --- NEW HDF5 ROOT ZONE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZoneZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,cVersion,Flags,AppGrid,ZBudFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(FlagsType),INTENT(IN)    :: Flags
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ZBudgetType)             :: ZBudFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26),PARAMETER :: ThisProcedure = ModName // 'RootZoneZoneBudRawFile_New'
    CHARACTER(LEN=15),PARAMETER            :: cArea = f_cMarkerChar_ZBudget // '          (' // f_cAreaUnitMarker_ZBudget // ')' // f_cMarkerChar_ZBudget
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
    ALLOCATE (SystemData%iElementIDs(AppGrid%NElements)                   , &
              SystemData%iElementNNodes(AppGrid%NElements)                , &
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
    SystemData%iElementNNodes = AppGrid%NVertex
    DO indxElem=1,AppGrid%NElements
        SystemData%iElementIDs(indxElem)     = AppGrid%AppElement(indxElem)%ID
        SystemData%iElementNodes(:,indxElem) = AppGrid%Vertex(:,indxElem)
        DO indxVertex=1,AppGrid%NVertex(indxElem)
            SystemData%rElementNodeAreas(indxVertex,indxElem)         = AppGrid%AppElement(indxElem)%VertexArea(indxVertex)
            SystemData%rElementNodeAreaFractions(indxVertex,indxElem) = AppGrid%AppElement(indxElem)%VertexAreaFraction(indxVertex)
        END DO
        IF (AppGrid%NVertex(indxElem) .EQ. 3) THEN
            SystemData%rElementNodeAreas(4,indxElem)         = 0.0
            SystemData%rElementNodeAreaFractions(4,indxElem) = 0.0
        END IF
    END DO
    DO indxFace=1,AppGrid%NFaces
        SystemData%iFaceElems(:,indxFace) = AppGrid%AppFace%Element(:,indxFace)
    END DO
    SystemData%lBoundaryFace = AppGrid%AppFace%BoundaryFace
    SystemData%lActiveNode   = .TRUE.
    
    !Compile Header data
    Header%cSoftwareVersion   = 'IWFM ROOT ZONE PACKAGE (' // TRIM(cVersion) // ')'
    Header%cDescriptor        = 'Root zone zone budget'
    Header%lFaceFlows_Defined = .FALSE.
    Header%lStorages_Defined  = .FALSE.
    Header%lComputeError      = .FALSE.
    Header%iNData             = f_iNRootZoneZBudColumns
    ALLOCATE (Header%iDataTypes(f_iNRootZoneZBudColumns)                           , &
              Header%cFullDataNames(f_iNRootZoneZBudColumns)                       , &
              Header%cDataHDFPaths(f_iNRootZoneZBudColumns)                        , &
              Header%iNDataElems(f_iNRootZoneZBudColumns,1)                        , &
              Header%iElemDataColumns(AppGrid%NElements,f_iNRootZoneZBudColumns,1) , &
              !Header%iErrorInCols()                                               , &  ! Since mass balance error is not calcuated no need
              !Header%iErrorOutCols()                                              , &  !  to allocate these arrays
              Header%cDSSFParts(f_iNRootZoneZBudColumns)                           , &
              Header%ASCIIOutput%cColumnTitles(5)                                  , &
              STAT = ErrorCode                                                     )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for root zone Z-Budget file!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Header%iDataTypes = [f_iAR_ZBudget ,&  !Non-ponded ag area                                          1
                         f_iVR_ZBudget ,&  !Non-ponded ag potential ET                                  2
                         f_iVR_ZBudget ,&  !Non-ponded ag precipitation                                 3
                         f_iVR_ZBudget ,&  !Non-ponded ag runoff                                        4
                         f_iVR_ZBudget ,&  !Non-ponded ag prime applied water                           5
                         f_iVR_ZBudget ,&  !Non-ponded ag re-used water                                 6
                         f_iVR_ZBudget ,&  !Non-ponded ag return flow                                   7
                         f_iVR_ZBudget ,&  !Non-ponded ag surface flow for gw recharge (Ag)             8
                         f_iVR_ZBudget ,&  !Non-ponded ag surface flow for gw recharge (UrbIn)          9
                         f_iVR_ZBudget ,&  !Non-ponded ag surface flow for gw recharge (UrbOut)         10
                         f_iVR_ZBudget ,&  !Non-ponded ag surface flow for gw recharge (NVRV)           11
                         f_iVLB_ZBudget,&  !Non-ponded ag beginning storage                             12
                         f_iVR_ZBudget ,&  !Non-ponded ag net gain from land expansion                  13
                         f_iVR_ZBudget ,&  !Non-ponded ag infiltration                                  14
                         f_iVR_ZBudget ,&  !Non-ponded ag groundwater inflow                            15
                         f_iVR_ZBudget ,&  !Non-ponded ag generic inflow                                16
                         f_iVR_ZBudget ,&  !Non-ponded ag actual ET                                     17
                         f_iVR_ZBudget ,&  !Non-ponded ag perc                                          18
                         f_iVLE_ZBudget,&  !Non-ponded ag ending storage                                19
                         f_iVR_ZBudget ,&  !Non-ponded ag discrepancy                                   20
                         f_iAR_ZBudget ,&  !Rice area                                                   21
                         f_iVR_ZBudget ,&  !Rice potential ET                                           22
                         f_iVR_ZBudget ,&  !Rice precipitation                                          23
                         f_iVR_ZBudget ,&  !Rice runoff                                                 24
                         f_iVR_ZBudget ,&  !Rice prime applied water                                    25
                         f_iVR_ZBudget ,&  !Rice re-used water                                          26 
                         f_iVR_ZBudget ,&  !Rice return flow                                            27
                         f_iVR_ZBudget ,&  !Rice surface flow for gw recharge (Ag)                      28
                         f_iVR_ZBudget ,&  !Rice surface flow for gw recharge (UrbIn)                   29
                         f_iVR_ZBudget ,&  !Rice surface flow for gw recharge (UrbOut)                  30
                         f_iVR_ZBudget ,&  !Rice surface flow for gw recharge (NVRV)                    31
                         f_iVLB_ZBudget,&  !Rice beginning storage                                      32
                         f_iVR_ZBudget ,&  !Rice net gain from land expansion                           33
                         f_iVR_ZBudget ,&  !Rice infiltration                                           34
                         f_iVR_ZBudget ,&  !Rice groundwater inflow                                     35
                         f_iVR_ZBudget ,&  !Rice generic inflow                                         36
                         f_iVR_ZBudget ,&  !Rice pond drain                                             37
                         f_iVR_ZBudget ,&  !Rice actual ET                                              38
                         f_iVR_ZBudget ,&  !Rice perc                                                   39
                         f_iVLE_ZBudget,&  !Rice ending storage                                         40
                         f_iVR_ZBudget ,&  !Rice discrepancy                                            41
                         f_iAR_ZBudget ,&  !Refuge area                                                 42
                         f_iVR_ZBudget ,&  !Refuge potential ET                                         43
                         f_iVR_ZBudget ,&  !Refuge precipitation                                        44
                         f_iVR_ZBudget ,&  !Refuge runoff                                               45
                         f_iVR_ZBudget ,&  !Refuge prime applied water                                  46
                         f_iVR_ZBudget ,&  !Refuge re-used water                                        47
                         f_iVR_ZBudget ,&  !Refuge return flow                                          48
                         f_iVR_ZBudget ,&  !Refuge surface flow for gw recharge (Ag)                    49
                         f_iVR_ZBudget ,&  !Refuge surface flow for gw recharge (UrbIn)                 50
                         f_iVR_ZBudget ,&  !Refuge surface flow for gw recharge (UrbOut)                51
                         f_iVR_ZBudget ,&  !Refuge surface flow for gw recharge (NVRV)                  52
                         f_iVLB_ZBudget,&  !Refuge beginning storage                                    53
                         f_iVR_ZBudget ,&  !Refuge net gain from land expansion                         54
                         f_iVR_ZBudget ,&  !Refuge infiltration                                         55
                         f_iVR_ZBudget ,&  !Refuge groundwater inflow                                   56
                         f_iVR_ZBudget ,&  !Refuge generic inflow                                       57
                         f_iVR_ZBudget ,&  !Refuge pond drain                                           58
                         f_iVR_ZBudget ,&  !Refuge actual ET                                            59
                         f_iVR_ZBudget ,&  !Refuge perc                                                 60
                         f_iVLE_ZBudget,&  !Refuge ending storage                                       61
                         f_iVR_ZBudget ,&  !Refuge discrepancy                                          62
                         f_iAR_ZBudget ,&  !Urban area                                                  63
                         f_iVR_ZBudget ,&  !Urban potential ET                                          64
                         f_iVR_ZBudget ,&  !Urban precipitation                                         65
                         f_iVR_ZBudget ,&  !Urban runoff                                                66
                         f_iVR_ZBudget ,&  !Urban prime applied water                                   67
                         f_iVR_ZBudget ,&  !Urban re-used water                                         68
                         f_iVR_ZBudget ,&  !Urban return flow                                           69
                         f_iVR_ZBudget ,&  !Urban surface flow for gw recharge (Ag)                     70
                         f_iVR_ZBudget ,&  !Urban surface flow for gw recharge (UrbIn)                  71
                         f_iVR_ZBudget ,&  !Urban surface flow for gw recharge (UrbOut)                 72
                         f_iVR_ZBudget ,&  !Urban surface flow for gw recharge (NVRV)                   73
                         f_iVLB_ZBudget,&  !Urban beginning storage                                     74
                         f_iVR_ZBudget ,&  !Urban net gain from land expansion                          75
                         f_iVR_ZBudget ,&  !Urban infiltration                                          76
                         f_iVR_ZBudget ,&  !Urban groundwater inflow                                    77
                         f_iVR_ZBudget ,&  !Urban generic inflow                                        78
                         f_iVR_ZBudget ,&  !Urban actual ET                                             79
                         f_iVR_ZBudget ,&  !Urban perc                                                  80
                         f_iVLE_ZBudget,&  !Urban ending storage                                        81
                         f_iVR_ZBudget ,&  !Urban discrepancy                                           82
                         f_iAR_ZBudget ,&  !NV&RV area                                                  83
                         f_iVR_ZBudget ,&  !NV&RV potential ET                                          84
                         f_iVR_ZBudget ,&  !NV&RV precipitation                                         85
                         f_iVR_ZBudget ,&  !NV&RV runoff                                                86
                         f_iVR_ZBudget ,&  !NV&RV surface flow for gw recharge (Ag)                     87
                         f_iVR_ZBudget ,&  !NV&RV surface flow for gw recharge (UrbIn)                  88
                         f_iVR_ZBudget ,&  !NV&RV surface flow for gw recharge (UrbOut)                 89
                         f_iVR_ZBudget ,&  !NV&RV surface flow for gw recharge (NVRV)                   90
                         f_iVLB_ZBudget,&  !NV&RV beginning storage                                     91
                         f_iVR_ZBudget ,&  !NV&RV net gain from land expansion                          92
                         f_iVR_ZBudget ,&  !NV&RV infiltration                                          93
                         f_iVR_ZBudget ,&  !NV&RV groundwater inflow                                    94
                         f_iVR_ZBudget ,&  !NV&RV generic inflow                                        95
                         f_iVR_ZBudget ,&  !NV&RV stream inflow for RV ET                               96
                         f_iVR_ZBudget ,&  !NV&RV actual ET                                             97
                         f_iVR_ZBudget ,&  !NV&RV perc                                                  98
                         f_iVLE_ZBudget,&  !NV&RV ending storage                                        99
                         f_iVR_ZBudget ]   !NV&RV discrepancy                                           100
    Header%cFullDataNames     = f_cRootZoneZBudgetColumnTitles
    Header%cFullDataNames(1)  = TRIM(Header%cFullDataNames(1)) // cArea
    Header%cFullDataNames(21) = TRIM(Header%cFullDataNames(21)) // cArea
    Header%cFullDataNames(42) = TRIM(Header%cFullDataNames(42)) // cArea
    Header%cFullDataNames(63) = TRIM(Header%cFullDataNames(63)) // cArea
    Header%cFullDataNames(83) = TRIM(Header%cFullDataNames(83)) // cArea
    Header%cDataHDFPaths      = f_cRootZoneZBudgetColumnTitles
    
    !Non-ponded ag data
    IF (Flags%lNonPondedAg_Defined) THEN
        Header%iNDataElems(1:20,:)  = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,1:20,:)  = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(15,:)        = 0
            Header%iElemDataColumns(:,15,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(16,:)        = 0
            Header%iElemDataColumns(:,16,:) = 0
        END IF
    ELSE
        Header%iNDataElems(1:20,:)        = 0
        Header%iElemDataColumns(:,1:20,:) = 0
    END IF
    
    !Ponded ag
    IF (Flags%lPondedAg_Defined) THEN
        !Rice
        Header%iNDataElems(21:41,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,21:41,:) = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(35,:)        = 0
            Header%iElemDataColumns(:,35,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(36,:)        = 0
            Header%iElemDataColumns(:,36,:) = 0
        END IF
        
        !Refuge
        Header%iNDataElems(42:62,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,42:62,:) = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(56,:)        = 0
            Header%iElemDataColumns(:,56,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(57,:)        = 0
            Header%iElemDataColumns(:,57,:) = 0
        END IF
    ELSE
        Header%iNDataElems(21:62,:)        = 0
        Header%iElemDataColumns(:,21:62,:) = 0
    END IF
    
    !Urban
    IF (Flags%lUrban_Defined) THEN
        Header%iNDataElems(63:82,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,63:82,:) = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(77,:)        = 0
            Header%iElemDataColumns(:,77,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(78,:)        = 0
            Header%iElemDataColumns(:,78,:) = 0
        END IF
    ELSE
        Header%iNDataElems(63:82,:)        = 0
        Header%iElemDataColumns(:,63:82,:) = 0
    END IF
    
    !Native and riparian veg.
    IF (Flags%lNVRV_Defined) THEN
        Header%iNDataElems(83:,:) = AppGrid%NElements
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,83:,:) = indxElem
        END DO
        IF (.NOT. Flags%lComputeETFromGW) THEN
            Header%iNDataElems(94,:)        = 0
            Header%iElemDataColumns(:,94,:) = 0
        END IF
        IF (.NOT. Flags%lGenericMoistureFile_Defined) THEN
            Header%iNDataElems(95,:)        = 0
            Header%iElemDataColumns(:,95,:) = 0
        END IF
    ELSE
        Header%iNDataElems(83:,:)        = 0
        Header%iElemDataColumns(:,83:,:) = 0
    END IF

    !ASCII output titles
    Header%ASCIIOutput%iNTitles         = 5
    Header%ASCIIOutput%iLenColumnTitles = 1529
    Header%ASCIIOutput%cColumnTitles(1) = '                                                                                                                                                         Non-Ponded Agricultural Area                                                                                                                                                                                                                                                                                                    Rice Area                                                                                                                                                                                                                                                                                                                    Refuge Area                                                                                                                                                                                                                                                                                                            Urban Area                                                                                                                                                                                                                                                                          Native & Riparian Vegetation Area                                                                                                                       ' 
    Header%ASCIIOutput%cColumnTitles(2) = '                 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
    Header%ASCIIOutput%cColumnTitles(3) = '                                                                                    Prime            Reused          Net     Surface Flow   Surface Flow   Surface Flow   Surface Flow     Beginning  Net Gain from                    Groundwater        Other           Actual                       Ending                                                                                      Prime            Reused          Net     Surface Flow   Surface Flow   Surface Flow   Surface Flow     Beginning  Net Gain from                    Groundwater        Other           Pond           Actual                       Ending                                                                                      Prime            Reused          Net     Surface Flow   Surface Flow   Surface Flow   Surface Flow     Beginning  Net Gain from                    Groundwater        Other           Pond           Actual                       Ending                                                                                      Prime            Reused          Net     Surface Flow   Surface Flow   Surface Flow   Surface Flow     Beginning  Net Gain from                    Groundwater        Other           Actual                       Ending                                                                                  Surface Flow   Surface Flow   Surface Flow   Surface Flow     Beginning  Net Gain from                    Groundwater        Other   Stream Inflow          Actual                       Ending                '
    Header%ASCIIOutput%cColumnTitles(4) = '      Time                 Area      Potential   Precipitation         Runoff      Applied           Water          Return      to GW          to GW          to GW          to GW          Storage   Land Expansion   Infiltration       Inflow          Inflow            ET      Percolation        Storage    Discrepancy             Area      Potential   Precipitation         Runoff      Applied           Water          Return      to GW          to GW          to GW          to GW          Storage   Land Expansion   Infiltration       Inflow          Inflow          Drain            ET      Percolation        Storage    Discrepancy             Area      Potential   Precipitation         Runoff      Applied           Water          Return      to GW          to GW          to GW          to GW          Storage   Land Expansion   Infiltration       Inflow          Inflow          Drain            ET      Percolation        Storage    Discrepancy             Area      Potential   Precipitation         Runoff      Applied           Water          Return      to GW          to GW          to GW          to GW          Storage   Land Expansion   Infiltration       Inflow          Inflow            ET      Percolation        Storage    Discrepancy             Area      Potential  Precipitation         Runoff       to GW          to GW          to GW          to GW          Storage   Land Expansion   Infiltration       Inflow          Inflow     for ET                ET      Percolation        Storage    Discrepancy'
    Header%ASCIIOutput%cColumnTitles(5) = '                 '    //cArea//'        ET                                          Water                            Flow        (Ag)      (Urban Indrs)  (Urban Outdrs)  (Native Veg)        (+)           (+)             (+)             (+)             (+)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                          Water                            Flow        (Ag)      (Urban Indrs)  (Urban Outdrs)  (Native Veg)        (+)           (+)             (+)             (+)             (+)            (-)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                          Water                            Flow        (Ag)      (Urban Indrs)  (Urban Outdrs)  (Native Veg)        (+)           (+)             (+)             (+)             (+)            (-)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                          Water                            Flow        (Ag)      (Urban Indrs)  (Urban Outdrs)  (Native Veg)        (+)           (+)             (+)             (+)             (+)             (-)        (-)               (-)          (=)       '    //cArea//'        ET                                         (Ag)       (Urban Indrs)  (Urban Outdrs)  (Native Veg)        (+)           (+)             (+)             (+)             (+)       (+)                  (-)        (-)               (-)          (=)    '
    Header%ASCIIOutput%cNumberFormat    = '(A16,20(2X,F13.1),3X,21(2X,F13.1),3X,21(2X,F13.1),3X,20(2X,F13.1),3X,18(2X,F13.1))'
    
    !DSS pathanmes
    Header%cDSSFParts = ['NP_AG_AREA'               ,&
                         'NP_AG_POT_ET'             ,&
                         'NP_AG_PRECIP'             ,&   
                         'NP_AG_RUNOFF'             ,&   
                         'NP_AG_PRM_H2O'            ,&
                         'NP_AG_RE-USE'             ,&   
                         'NP_AG_NT_RTRN_FLW'        ,&   
                         'NP_AG_FLW_GW_AG'          ,&
                         'NP_AG_FLW_GW_URIN'        ,&
                         'NP_AG_FLW_GW_UROT'        ,&
                         'NP_AG_FLW_GW_NRV'         ,&
                         'NP_AG_BEGIN_STOR'         ,&   
                         'NP_AG_GAIN_EXP'           ,&   
                         'NP_AG_INFILTR'            ,&
                         'NP_AG_GW_INFLW'           ,&
                         'NP_AG_OTHER_INFLW'        ,&
                         'NP_AG_ET'                 ,&   
                         'NP_AG_PERC'               ,&   
                         'NP_AG_END_STOR'           ,&  
                         'NP_AG_DISCREPANCY'        ,& 
                         'RICE_AREA'                ,&
                         'RICE_POT_ET'              ,&
                         'RICE_PRECIP'              ,&   
                         'RICE_RUNOFF'              ,&   
                         'RICE_PRM_H2O'             ,&
                         'RICE_RE-USE'              ,&   
                         'RICE_NT_RTRN_FLW'         ,&   
                         'RICE_FLW_GW_AG'           ,&
                         'RICE_FLW_GW_URIN'         ,&
                         'RICE_FLW_GW_UROT'         ,&
                         'RICE_FLW_GW_NRV'          ,&
                         'RICE_BEGIN_STOR'          ,&   
                         'RICE_GAIN_EXP'            ,&   
                         'RICE_INFILTR'             ,&
                         'RICE_GW_INFLW'            ,&
                         'RICE_OTHER_INFLW'         ,&
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
                         'REFUGE_RE-USE'            ,&   
                         'REFUGE_NT_RTRN_FLW'       ,&   
                         'REFUGE_FLW_GW_AG'         ,&
                         'REFUGE_FLW_GW_URIN'       ,&
                         'REFUGE_FLW_GW_UROT'       ,&
                         'REFUGE_FLW_GW_NRV'        ,&
                         'REFUGE_BEGIN_STOR'        ,&   
                         'REFUGE_GAIN_EXP'          ,&   
                         'REFUGE_INFILTR'           ,&
                         'REFUGE_GW_INFLW'          ,&
                         'REFUGE_OTHER_INFLW'       ,&
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
                         'URB_RE-USE'               ,&     
                         'URB_NT_RTRN_FLW'          ,&     
                         'URB_FLW_GW_AG'            ,&
                         'URB_FLW_GW_URIN'          ,&
                         'URB_FLW_GW_UROT'          ,&
                         'URB_FLW_GW_NRV'           ,&
                         'URB_BEGIN_STOR'           ,&     
                         'URB_GAIN_EXP'             ,&     
                         'URB_INFILTR'              ,&     
                         'URB_GW_INFLW'             ,&
                         'URB_OTHER_INFLW'          ,&
                         'URB_ET'                   ,&     
                         'URB_PERC'                 ,&     
                         'URB_END_STOR'             ,& 
                         'URB_DISCREPANCY'          ,&    
                         'NRV_AREA'                 ,&
                         'NRV_POT_ET'               ,&
                         'NRV_PRECIP'               ,&
                         'NRV_RUNOFF'               ,&  
                         'NRV_FLW_GW_AG'            ,&
                         'NRV_FLW_GW_URIN'          ,&
                         'NRV_FLW_GW_UROT'          ,&
                         'NRV_FLW_GW_NRV'           ,&
                         'NRV_BEGIN_STOR'           ,&     
                         'NRV_GAIN_EXP'             ,&     
                         'NRV_INFILTR'              ,&     
                         'NRV_GW_INFLW'             ,&
                         'NRV_OTHER_INFLW'          ,&
                         'NRV_STRM_ET'              ,&
                         'NRV_ET'                   ,&     
                         'NRV_PERC'                 ,&     
                         'NRV_END_STOR'             ,&
                         'NRV_DISCREPANCY'          ] 
                             
    !Instantiate Z-Budget file
    CALL ZBudFile%New(cFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE RootZoneZoneBudRawFile_New
  

  ! -------------------------------------------------------------
  ! --- NEW BINARY CROP-SPECIFIC LAND AND WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE AgLWUseBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 4   , &
                              f_iTitleLen           = 149 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNAgLWUseBudColumns) = ['AREA'   , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' ]
    CHARACTER(LEN=10)      :: FParts(f_iNAgLWUseBudColumns) = ['AREA'         ,&
                                                               'POTNL_CUAW'   ,&
                                                               'SUP_REQ'      ,&    
                                                               'PUMPING'      ,&
                                                               'DELIVERY'     ,&
                                                               'SHORTAGE'     ,&
                                                               'ETAW'         ,&
                                                               'EFF_PRECIP'   ,&
                                                               'ET_GW'        ,&
                                                               'ET_OTHER'     ]
    
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
        pASCIIOutput%TitleLen           = f_iTitleLen
        pASCIIOutput%NTitles            = f_iNTitles
          ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
          pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//f_cVolumeUnitMarker_Budget//' FOR '//f_cLocationNameMarker_Budget , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker_Budget//' '//f_cAreaUnitMarker_Budget , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
          pASCIIOutput%lTitlePersist(1:3) = .TRUE.
          pASCIIOutput%lTItlePersist(4)   = .FALSE.
        pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,10(F12.1,1X))')
        pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                               , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNAgLWUseBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNAgLWUseBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNAgLWUseBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNAgLWUseBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)               )
    ASSOCIATE (pLocation => OutputData%Locations(1))
        pLocation%NDataColumns       = f_iNAgLWUseBudColumns
        pLocation%cFullColumnHeaders =  ['Time'                                  , &
                                         'Area ('//f_cAreaUnitMarker_Budget//')' , &
                                         'Potential CUAW'                        , &
                                         'Supply Requirement'                    , &
                                         'Pumping'                               , &
                                         'Deliveries'                            , &
                                         'Shortage'                              , &
                                         'ETAW'                                  , &
                                         'Effective Precipitation'               , &
                                         'ET from Groundwater'                   , &
                                         'ET from Other Sources'                 ]
        pLocation%iDataColumnTypes  =  [f_iAR_Budget          ,&      !Ag area
                                        f_iVR_lwu_PotCUAW     ,&      !Potential CUAW
                                        f_iVR_lwu_AgSupplyReq ,&      !Ag supply req.
                                        f_iVR_lwu_AgPump      ,&      !Pumping for ag
                                        f_iVR_lwu_AgDiv       ,&      !Deliveries for ag
                                        f_iVR_lwu_AgShort     ,&      !Ag supply shortage
                                        f_iVR_Budget          ,&      !ETAW
                                        f_iVR_Budget          ,&      !ETP
                                        f_iVR_Budget          ,&      !ETGW
                                        f_iVR_Budget          ]       !ETOth
        pLocation%iColWidth       = [17,12,14,(13,indxCol=1,7)]
        ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                   pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
            Text                = ArrangeText(TRIM(UnitT),17)
            Text1               = '('//TRIM(f_cAreaUnitMarker_Budget)//')'
            pColumnHeaders(:,1) = ['                 ','            ','    Potential ',' Agricultural','             ','             ','             ','             ','             ','      ET     ','      ET     ']
            pColumnHeaders(:,2) = ['      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping','   Deliveries','     Shortage','             ','   Effective ','     from    ','  from Other ']
            pColumnHeaders(:,3) = [               Text,         Text1,'              ','  Requirement','        (-)  ','       (-)   ','       (=)   ','        ETAW ','    Precip   ','  Groundwater','    Sources  ']
            pColumnHeaders(:,4) = ''
            pFormatSpecs(1)     = '(A17,A12,A14,8A13)'
            pFormatSpecs(2)     = '(A17,A12,A14,8A13)'
            pFormatSpecs(3)     = '(A17,A12,A14,8A13)'
            pFormatSpecs(4)     = '("'//REPEAT('-',f_iTitleLen)//'",'//TRIM(IntToText(f_iNAgLWUseBudColumns+1))//'A0)'
        END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
        ALLOCATE (pDSSOutput%cPathNames(f_iNAgLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNAgLWUseBudColumns))
        iCount = 1
        DO indxLocation=1,NRegion
            DO indxCol=1,f_iNAgLWUseBudColumns
                pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                                TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                                TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                                '/'                                                            //  &  !D part
                                                 TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                                TRIM(FParts(indxCol))//'/'                                            !F part
                iCount = iCount+1
            END DO
        END DO
        pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,9)]
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
        
  END SUBROUTINE AgLWUseBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW BINARY ROOT ZONE BUDGET FILE FOR POST-PROCESSING OF AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE AgRootZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: f_iNTitles            = 4   , &
                              f_iTitleLen           = 333 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNAgRootZoneBudColumns) = ['AREA'   , &
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
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' , &
                                                                  'VOLUME' ]
    CHARACTER(LEN=11)      :: FParts(f_iNAgRootZoneBudColumns) = ['AREA'           ,&
                                                                  'POT_ET'         ,&   
                                                                  'PRECIP'         ,&   
                                                                  'RUNOFF'         ,&   
                                                                  'PRM_H2O'        ,&
                                                                  'RE-USE'         ,&   
                                                                  'NET_RTRN_FLW'   ,&   
                                                                  'FLW_GW_AG'      ,&                             
                                                                  'FLW_GW_URIN'    ,&                                
                                                                  'FLW_GW_UROT'    ,&                                 
                                                                  'FLW_GW_NRV'     ,&                              
                                                                  'BEGIN_STOR'     ,&   
                                                                  'GAIN_EXP'       ,&   
                                                                  'INFILTR'        ,&
                                                                  'GW_INFLW'       ,&
                                                                  'OTHER_INFLW'    ,&
                                                                  'DRAIN'          ,&  
                                                                  'ET'             ,&   
                                                                  'PERC'           ,&   
                                                                  'END_STOR'       ,&  
                                                                  'DISCREPANCY'    ]
    
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
        pASCIIOutput%TitleLen = f_iTitleLen
        pASCIIOutput%NTitles  = f_iNTitles
          ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
          pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//f_cVolumeUnitMarker_Budget//' FOR '//f_cLocationNameMarker_Budget , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker_Budget//' '//f_cAreaUnitMarker_Budget , pASCIIOutput%TitleLen)
          pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
          pASCIIOutput%lTitlePersist(1:3) = .TRUE.
          pASCIIOutput%lTitlePersist(4)   = .FALSE.
        pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,21(F14.1,1X))')
        pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                            , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNAgRootZoneBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNAgRootZoneBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(f_iNAgRootZoneBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(f_iNAgRootZoneBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)               )
    ASSOCIATE (pLocation => OutputData%Locations(1))
        pLocation%NDataColumns       = f_iNAgRootZoneBudColumns
        pLocation%cFullColumnHeaders =  ['Time'                                               , &
                                         'Area ('//f_cAreaUnitMarker_Budget//')'              , &
                                         'Potential ET'                                       , &
                                         'Precipitation'                                      , &
                                         'Runoff'                                             , &
                                         'Prime Applied Water'                                , &
                                         'Reused Water'                                       , &
                                         'Net Return Flow'                                    , &
                                         'Surface Flow to GW (Ag)'                            , &
                                         'Surface Flow to GW (Urban Indrs)'                   , &
                                         'Surface Flow to GW (Urban Outdrs)'                  , &
                                         'Surface Flow to GW (Native Veg)'                    , &
                                         'Beginning Storage (+)'                              , &
                                         'Net Gain from Land Expansion (+)'                   , &
                                         'Infiltration (+)'                                   , &
                                         'Groundwater Inflow (+)'                             , &
                                         'Other Inflow (+)'                                   , &
                                         'Pond Drain (-)'                                     , &
                                         'Actual ET (-)'                                      , &
                                         'Percolation (-)'                                    , &
                                         'Ending Storage (-)'                                 , &
                                         'Discrepancy (=)'                                    ]
        pLocation%iDataColumnTypes  =  [f_iAR_Budget ,&  !Ag area
                                        f_iVR_Budget ,&  !Ag potential ET
                                        f_iVR_Budget ,&  !Ag precipitation
                                        f_iVR_Budget ,&  !Ag runoff
                                        f_iVR_Budget ,&  !Ag prime applied water
                                        f_iVR_Budget ,&  !Ag re-used water
                                        f_iVR_Budget ,&  !Ag return flow
                                        f_iVR_Budget ,&  !Ag surface inflow from other ag lands for gw recharge
                                        f_iVR_Budget ,&  !Ag surface inflow from urban indoors for gw recharge
                                        f_iVR_Budget ,&  !Ag surface inflow from urban outdoors for gw recharge
                                        f_iVR_Budget ,&  !Ag surface inflow from native&riparian veg for gw recharge
                                        f_iVLB_Budget,&  !Ag beginning storage
                                        f_iVR_Budget ,&  !Ag net gain from land expansion
                                        f_iVR_Budget ,&  !Ag infiltration
                                        f_iVR_Budget ,&  !Ag groundwater inflow
                                        f_iVR_Budget ,&  !Ag generic inflow
                                        f_iVR_Budget ,&  !Ag pond drain
                                        f_iVR_Budget ,&  !Ag actual ET
                                        f_iVR_Budget ,&  !Ag perc
                                        f_iVLE_Budget,&  !Ag ending storage
                                        f_iVR_Budget ]   !Ag discrepancy
        pLocation%iColWidth       = [17,14,15,16,(15,indxCol=1,18)]
        ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                   pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
            Text                = ArrangeText(TRIM(UnitT),17)
            Text1               = '('//TRIM(f_cAreaUnitMarker_Budget)//')'
            pColumnHeaders(:,1) = ['                 ','              ','               ','                ','               ','       Prime   ','         Reused','          Net  ','  Surface Flow ','  Surface Flow ','  Surface Flow ','  Surface Flow ','     Beginning ',' Net Gain from ','               ','    Groundwater','        Other  ','          Pond ','         Actual','               ','        Ending ','               ']
            pColumnHeaders(:,2) = ['      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ','         Water ','         Return','     to GW     ','     to GW     ','     to GW     ','     to GW     ','      Storage  ',' Land Expansion','   Infiltration','       Inflow  ','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy']
            pColumnHeaders(:,3) = [               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','          Flow ','     (Ag)      ',' (Urban Indrs) ',' (Urban Outdrs)',' (Native Veg.) ','        (+)    ','       (+)     ','        (+)    ','         (+)   ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    ']
            pColumnHeaders(:,4) = ''
            pFormatSpecs(1)     = '(A17,A14,A15,A16,18A15)'
            pFormatSpecs(2)     = '(A17,A14,A15,A16,18A15)'
            pFormatSpecs(3)     = '(A17,A14,A15,A16,18A15)'
            pFormatSpecs(4)     = '('//TRIM(IntToText(f_iTitleLen))//'(1H-),'//TRIM(IntToText(f_iNAgRootZoneBudColumns+1))//'A0)'
        END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
        ALLOCATE (pDSSOutput%cPathNames(f_iNAgRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNAgRootZoneBudColumns))
        iCount = 1
        DO indxLocation=1,NRegion
            DO indxCol=1,f_iNAgRootZoneBudColumns
                pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                                TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                                TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                                '/'                                                            //  &  !D part
                                                 TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                                TRIM(FParts(indxCol))//'/'                                            !F part
                iCount = iCount+1
            END DO
        END DO
        pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,20)]
    END ASSOCIATE
                                             
    !Instantiate the root zone budget raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE AgRootZoneBudRawFile_New

  
  
  
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
  ! ---KILL ROOT ZONE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_Kill(RootZone)
    CLASS(RootZone_v412_Type) :: RootZone
    
    !Local variable
    INTEGER                  :: iErrorCode
    TYPE(RootZone_v412_Type) :: Dummy
    
    CALL RootZone%RootZone_v411_Type%KillRZImplementation()
    DEALLOCATE (RootZone%iStrmNodeIDs                           , &
                RootZone%iLakeIDs                               , &
                RootZone%iColSurfaceFlowDestination_Ag          , &
                RootZone%iColSurfaceFlowDestination_UrbIndoors  , &
                RootZone%iColSurfaceFlowDestination_UrbOutdoors , &
                RootZone%iColSurfaceFlowDestination_NVRV        , &
                RootZone%ElemFlowToOutside_Ag                   , &
                RootZone%ElemFlowToOutside_UrbIn                , & 
                RootZone%ElemFlowToOutside_UrbOut               , & 
                RootZone%ElemFlowToOutside_NVRV                 , & 
                RootZone%ElemFlowToStreams_Ag                   , & 
                RootZone%ElemFlowToStreams_UrbIn                , & 
                RootZone%ElemFlowToStreams_UrbOut               , &
                RootZone%ElemFlowToStreams_NVRV                 , &
                RootZone%ElemFlowToLakes_Ag                     , &
                RootZone%ElemFlowToLakes_UrbIn                  , & 
                RootZone%ElemFlowToLakes_UrbOut                 , &
                RootZone%ElemFlowToLakes_NVRV                   , & 
                RootZone%ElemFlowToGW_Ag                        , & 
                RootZone%ElemFlowToGW_UrbIn                     , &
                RootZone%ElemFlowToGW_UrbOut                    , & 
                RootZone%ElemFlowToGW_NVRV                      , & 
                RootZone%rAW_UrbanIndoors                       , &
                STAT=iErrorCode                                 )
    CALL RootZone%SurfaceFlowDestinationFile%Close()
    
    SELECT TYPE (RootZone)
        TYPE IS (RootZone_v412_Type)
            RootZone = Dummy
    END SELECT 

  END SUBROUTINE RootZone_v412_Kill

  
  
  
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
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLUType,iSubregionID
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+46) :: ThisProcedure = ModName // 'RootZone_v412_GetBudget_MonthlyFlows_GivenFile'
    INTEGER,TARGET               :: iDimActual,iNTimeSteps,                             &
                                    iReadCols_LWU_Ag(4) = [3,4,5,6],                    &
                                    iReadCols_LWU_Urb(4) = [12,13,14,15],               &
                                    iReadCols_RZ_Ag(9) = [12,13,14,15,16,17,18,19,20],  &
                                    iReadCols_RZ_Urb(8) = [33,34,35,36,37,38,39,40],    &
                                    iReadCols_RZ_NVRV(9) = [50,51,52,53,54,55,56,57,58]
    REAL(8),ALLOCATABLE          :: rValues(:,:)
    INTEGER,POINTER              :: piReadCols(:)
    
    !Number of time steps
    iNTimeSteps = Budget%GetNTimeSteps()

    !Land&Water Use Budget
    IF (iBudgetType .EQ. f_iBudgetType_LWU) THEN
        !Allocate arrays
        ALLOCATE (rValues(5,iNTimeSteps) , cFlowNames(4))  !Adding 1 to the first dimension for Time column; it will be removed later
        
        !Flow names
        cFlowNames     = ''
        cFlowNames(1)  = 'Supply Requirement'      
        cFlowNames(2)  = 'Pumping'                 
        cFlowNames(3)  = 'Deliveries'              
        cFlowNames(4)  = 'Shortage'  
        
        !Columns to read based on land use type
        SELECT CASE (iLUType)
            CASE (f_iLandUse_Ag)
                piReadCols => iReadCols_LWU_Ag
            CASE (f_iLandUse_Urb)
                piReadCols => iReadCols_LWU_Urb
            CASE (f_iLandUse_NonPondedAg) 
                CALL SetLastMessage('Non-ponded-crop-specific Land & Water Use Budget cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iLandUse_Rice)
                CALL SetLastMessage('Land & Water Use Budget for rice cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iLandUse_Refuge)
                CALL SetLastMessage('Land & Water Use Budget for refuges cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iLandUse_NVRV)
                CALL SetLastMessage('Land & Water Use Budget does not exist for native and riparian vegetation!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
        END SELECT
            
        !Read data
        CALL Budget%ReadData(iSubregionID,piReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Store values in return argument
        ALLOCATE (rFlows(4,iDimActual))
        rFlows(1,:)  = -rValues(2,1:iDimActual)       !Supply Requirement              
        rFlows(2,:)  = rValues(3,1:iDimActual)        !Pumping                          
        rFlows(3,:)  = rValues(4,1:iDimActual)        !Deliveries                       
        rFlows(4,:)  = rValues(5,1:iDimActual)        !Shortage                         

    !Root Zone Budget 
    ELSEIF (iBudgetType .EQ. f_iBudgetType_RootZone) THEN
        !Columns to read based on land use type
        SELECT CASE (iLUType)
            CASE (f_iLandUse_Ag)
                piReadCols => iReadCols_RZ_Ag
                !Allocate arrays
                ALLOCATE (rValues(10,iNTimeSteps) , cFlowNames(8))  !Adding 1 to the first dimension for Time column; it will be removed later
                !Flow names
                cFlowNames     = ''
                cFlowNames(1)  = 'Change in Storage'              
                cFlowNames(2)  = 'Net Gain from Land Expansion'   
                cFlowNames(3)  = 'Infiltration' 
                cFlowNames(4)  = 'GW Inflow'
                cFlowNames(5)  = 'Other Inflow'                   
                cFlowNames(6)  = 'Pond Drain'                     
                cFlowNames(7)  = 'Actual ET'                      
                cFlowNames(8)  = 'Percolation'                    
                !Read data
                CALL Budget%ReadData(iSubregionID,piReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                !Store values in return argument
                ALLOCATE (rFlows(8,iDimActual))
                rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(10,1:iDimActual)      !Change in Storage              
                rFlows(2,:)  = rValues(3,1:iDimActual)                                 !Net Gain from Land Expansion          
                rFlows(3,:)  = rValues(4,1:iDimActual)                                 !Infiltration 
                rFlows(4,:)  = rValues(5,1:iDimActual)                                 !GW Inflow                          
                rFlows(5,:)  = rValues(6,1:iDimActual)                                 !Other Inflow                          
                rFlows(6,:)  = -rValues(7,1:iDimActual)                                !Pond Drain                    
                rFlows(7,:)  = -rValues(8,1:iDimActual)                                !Actual ET                     
                rFlows(8,:)  = -rValues(9,1:iDimActual)                                !Percolation                   

            CASE (f_iLandUse_Urb)      
                piReadCols => iReadCols_RZ_Urb
                !Allocate arrays
                ALLOCATE (rValues(9,iNTimeSteps) , cFlowNames(7))  !Adding 1 to the first dimension for Time column; it will be removed later
                !Flow names
                cFlowNames     = ''
                cFlowNames(1)  = 'Change in Storage'              
                cFlowNames(2)  = 'Net Gain from Land Expansion'   
                cFlowNames(3)  = 'Infiltration'                   
                cFlowNames(4)  = 'GW Inflow'
                cFlowNames(5)  = 'Other Inflow'                   
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
                rFlows(4,:)  = rValues(5,1:iDimActual)                                 !GW Inflow                          
                rFlows(5,:)  = rValues(6,1:iDimActual)                                 !Other Inflow                          
                rFlows(6,:)  = -rValues(7,1:iDimActual)                                !Actual ET                     
                rFlows(7,:)  = -rValues(8,1:iDimActual)                                !Percolation                   

            CASE (f_iLandUse_NVRV)
                piReadCols => iReadCols_RZ_NVRV
                !Allocate arrays
                ALLOCATE (rValues(10,iNTimeSteps) , cFlowNames(8))  !Adding 1 to the first dimension for Time column; it will be removed later
                !Flow names
                cFlowNames     = ''
                cFlowNames(1)  = 'Change in Storage'              
                cFlowNames(2)  = 'Net Gain from Land Expansion'   
                cFlowNames(3)  = 'Infiltration'                   
                cFlowNames(4)  = 'GW Inflow'                   
                cFlowNames(5)  = 'Other Inflow'                   
                cFlowNames(6)  = 'Stream Inflow for ET'                   
                cFlowNames(7)  = 'Actual ET'                      
                cFlowNames(8)  = 'Percolation'                    
                !Read data
                CALL Budget%ReadData(iSubregionID,piReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                !Store values in return argument
                ALLOCATE (rFlows(8,iDimActual))
                rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(10,1:iDimActual)      !Change in Storage              
                rFlows(2,:)  = rValues(3,1:iDimActual)                                 !Net Gain from Land Expansion          
                rFlows(3,:)  = rValues(4,1:iDimActual)                                 !Infiltration                          
                rFlows(4,:)  = rValues(5,1:iDimActual)                                 !GW Inflow                          
                rFlows(5,:)  = rValues(6,1:iDimActual)                                 !Other Inflow                          
                rFlows(6,:)  = rValues(7,1:iDimActual)                                 !Stream Inflow for ET                          
                rFlows(7,:)  = -rValues(8,1:iDimActual)                                !Actual ET                     
                rFlows(8,:)  = -rValues(9,1:iDimActual)                                !Percolation                   

            CASE (f_iLandUse_NonPondedAg) 
                CALL SetLastMessage('Non-ponded-crop-specific Root Zone Budget cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iLandUse_Rice)
                CALL SetLastMessage('Root Zone Budget for rice cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            CASE (f_iLandUse_Refuge)
                CALL SetLastMessage('Root Zone Budget for refuges cannot be retrived from the specified budget file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
        END SELECT

    END IF
        
  END SUBROUTINE RootZone_v412_GetBudget_MonthlyFlows_GivenFile

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM RootZone OBJECT
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetBudget_MonthlyFlows_GivenRootZone(RootZone,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(RootZone_v412_Type),TARGET,INTENT(IN) :: RootZone
    CHARACTER(LEN=*),INTENT(IN)                 :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                          :: iBudgetType,iLUType,iSubregionID
    REAL(8),INTENT(IN)                          :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)             :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)    :: cFlowNames(:)
    INTEGER,INTENT(OUT)                         :: iStat
    
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
    CALL RootZone_v412_GetBudget_MonthlyFlows_GivenFile(pBudget,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat) 
    
  END SUBROUTINE RootZone_v412_GetBudget_MonthlyFlows_GivenRootZone

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS FOR A ZONE BUDGET (EXCLUDING TIME COLUMN)
  ! -------------------------------------------------------------
  FUNCTION RootZone_v412_GetZBudget_NColumns(RootZone,iZBudgetType) RESULT(iNCols)
    CLASS(RootZone_v412_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                   :: iZBudgetType
    INTEGER                              :: iNCols
    
    !Initialize
    iNCols = 0
    
    !Land and water use z-budget
    IF (iZBudgetType .EQ. f_iZBudgetType_LWU) THEN
        IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) THEN
            iNCols = f_iNLWUseZBudColumns
        END IF
        
    !Root zone budget
    ELSEIF (iZBudgetType .EQ. f_iZBudgetType_RootZone) THEN
        IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN
            iNCols = f_iNRootZoneZBudColumns
        END IF
    END IF    
        
  END FUNCTION RootZone_v412_GetZBudget_NColumns
     
     
  ! -------------------------------------------------------------
  ! --- GET MONTHLY ZBUDGET FLOWS 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetZBudget_MonthlyFlows_GivenFile(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
     TYPE(ZBudgetType),INTENT(IN)             :: ZBudget              
     TYPE(ZoneListType),INTENT(IN)            :: ZoneList
     INTEGER,INTENT(IN)                       :: iZBudgetType,iZoneID,iLUType
     CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate  
     REAL(8),INTENT(IN)                       :: rFactVL
     REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)          
     CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
     INTEGER,INTENT(OUT)                      :: iStat
     
     !Local variables
     CHARACTER(LEN=ModNameLen+47),PARAMETER :: ThisProcedure = ModName // 'RootZone_v412_GetZBudget_MonthlyFlows_GivenFile'
     INTEGER                                :: iNTimeSteps,indx,ErrorCode,iNPopulatedValues,indxTime
     TYPE(TimeStepType)                     :: TimeStep
     INTEGER,ALLOCATABLE                    :: iColList(:),iDataUnitTypes(:)
     REAL(8),ALLOCATABLE                    :: rValues(:,:)
     
    !Get number of time steps stored in the ZBudget file
    CALL ZBudget%GetTimeStepRelatedData(iNTimeSteps,TimeStep)
    
    !Land and water use z-budget
    IF (iZBudgetType .EQ. f_iZBudgetType_LWU) THEN
        ALLOCATE (iColList(4) , iDataUnitTypes(4) , cFlowNames(4) , rValues(5,iNTimeSteps))
        cFlowNames = ['Supply Requirement' , 'Pumping' , 'Deliveries' , 'Shortage']
        SELECT CASE (iLUType)
            CASE (f_iLandUse_NonPondedAg)
                iColList = [(indx,indx=3,6)]
                
            CASE (f_iLandUse_Rice)
                iColList = [(indx,indx=13,16)]
                
            CASE (f_iLandUse_Refuge)
                iColList = [(indx,indx=23,26)]
                
            CASE (f_iLandUse_Urb)
                iColList = [(indx,indx=32,35)]
                
            CASE DEFAULT
                CALL SetLastMessage('Land&Water Use ZBudget is not available for the selected land use type!',f_iFatal,ThisProcedure)
                DEALLOCATE (rFlows , cFlowNames , STAT=ErrorCode)
                ALLOCATE (rFlows(0,0) , cFlowNames(0))
                iStat = -1
                RETURN
        END SELECT
        
        !Read data for the interval
        CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,'1MON',cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iNPopulatedValues,rValues,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Calculate monthly averages
        ALLOCATE (rFlows(5,iNPopulatedValues))
        DO indxTime=1,iNPopulatedValues
            rFlows(1,indxTime) = -rValues(2,indxTime)
            rFlows(2,indxTime) =  rValues(3,indxTime) 
            rFlows(3,indxTime) =  rValues(4,indxTime)
            rFlows(4,indxTime) =  rValues(5,indxTime)
        END DO
    
    
    !Root zone budget
    ELSEIF (iZBudgetType .EQ. f_iZBudgetType_RootZone) THEN
        SELECT CASE (iLUType)
            CASE (f_iLandUse_NonPondedAg)
                ALLOCATE (iColList(8) , iDataUnitTypes(8) , cFlowNames(7) , rValues(9,iNTimeSteps))
                cFlowNames = ['Change in Storage' , 'Gain from Land Expansion' , 'Infiltration' , 'GW Inflow' , 'Other Inflow' , 'ET' , 'Percolation']
                iColList   = [(indx,indx=12,19)]
                    
                !Read data for the interval
                CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,'1MON',cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iNPopulatedValues,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                
                !Calculate monthly averages
                ALLOCATE (rFlows(7,iNPopulatedValues))
                DO indxTime=1,iNPopulatedValues
                    rFlows(1,indxTime) =  rValues(2,indxTime) - rValues(9,indxTime)  !Change in storage
                    rFlows(2,indxTime) =  rValues(3,indxTime)                        !Gain from land expansion
                    rFlows(3,indxTime) =  rValues(4,indxTime)                        !Infiltration
                    rFlows(4,indxTime) =  rValues(5,indxTime)                        !GW Inflow
                    rFlows(5,indxTime) =  rValues(6,indxTime)                        !Other Inflow
                    rFlows(6,indxTime) = -rValues(7,indxTime)                        !ET
                    rFlows(7,indxTime) = -rValues(8,indxTime)                        !Percolation
                END DO
                
            CASE (f_iLandUse_Rice)
                ALLOCATE (iColList(9) , iDataUnitTypes(9) , cFlowNames(8) , rValues(10,iNTimeSteps))
                cFlowNames = ['Change in Storage' , 'Gain from Land Expansion' , 'Infiltration' , 'GW Inflow' , 'Other Inflow' , 'Pond Drain' , 'ET' , 'Percolation']
                iColList   = [(indx,indx=32,40)]
                
                !Read data for the interval
                CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,'1MON',cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iNPopulatedValues,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                
                !Calculate monthly averages
                ALLOCATE (rFlows(8,iNPopulatedValues))
                DO indxTime=1,iNPopulatedValues
                    rFlows(1,indxTime) =  rValues(2,indxTime) - rValues(10,indxTime) !Change in storage
                    rFlows(2,indxTime) =  rValues(3,indxTime)                        !Gain from land expansion
                    rFlows(3,indxTime) =  rValues(4,indxTime)                        !Infiltration
                    rFlows(4,indxTime) =  rValues(5,indxTime)                        !GW Inflow
                    rFlows(5,indxTime) =  rValues(6,indxTime)                        !Other Inflow
                    rFlows(6,indxTime) = -rValues(7,indxTime)                        !Pond drain
                    rFlows(7,indxTime) = -rValues(8,indxTime)                        !ET
                    rFlows(8,indxTime) = -rValues(9,indxTime)                        !Percolation
                END DO
                
            CASE (f_iLandUse_Refuge)
                ALLOCATE (iColList(9) , iDataUnitTypes(9) , cFlowNames(8) , rValues(10,iNTimeSteps))
                cFlowNames = ['Change in Storage' , 'Gain from Land Expansion' , 'Infiltration' , 'GW Inflow' , 'Other Inflow' , 'Pond Drain' , 'ET' , 'Percolation']
                iColList   = [(indx,indx=53,61)]
                
                !Read data for the interval
                CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,'1MON',cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iNPopulatedValues,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                
                !Calculate monthly averages
                ALLOCATE (rFlows(8,iNPopulatedValues))
                DO indxTime=1,iNPopulatedValues
                    rFlows(1,indxTime) =   rValues(2,indxTime) - rValues(10,indxTime) !Change in storage
                    rFlows(2,indxTime) =   rValues(3,indxTime)                        !Gain from land expansion
                    rFlows(3,indxTime) =   rValues(4,indxTime)                        !Infiltration
                    rFlows(4,indxTime) =   rValues(5,indxTime)                        !GW Inflow
                    rFlows(5,indxTime) =   rValues(6,indxTime)                        !Other Inflow
                    rFlows(6,indxTime) =  -rValues(7,indxTime)                        !Pond drain
                    rFlows(7,indxTime) =  -rValues(8,indxTime)                        !ET
                    rFlows(8,indxTime) =  -rValues(9,indxTime)                        !Percolation
                END DO
                
            CASE (f_iLandUse_Urb)
                ALLOCATE (iColList(8) , iDataUnitTypes(8) , cFlowNames(7) , rValues(9,iNTimeSteps))
                cFlowNames = ['Change in Storage' , 'Gain from Land Expansion' , 'Infiltration' , 'GW Inflow' , 'Other Inflow' , 'ET' , 'Percolation']
                iColList   = [(indx,indx=74,81)]
                
                !Read data for the interval
                CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,'1MON',cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iNPopulatedValues,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                
                !Calculate monthly averages
                ALLOCATE (rFlows(7,iNPopulatedValues))
                DO indxTime=1,iNPopulatedValues
                    rFlows(1,indxTime) =  rValues(2,indxTime) - rValues(9,indxTime)  !Change in storage
                    rFlows(2,indxTime) =  rValues(3,indxTime)                        !Gain from land expansion
                    rFlows(3,indxTime) =  rValues(4,indxTime)                        !Infiltration
                    rFlows(4,indxTime) =  rValues(5,indxTime)                        !GW Inflow
                    rFlows(5,indxTime) =  rValues(6,indxTime)                        !Other Inflow
                    rFlows(6,indxTime) = -rValues(7,indxTime)                        !ET
                    rFlows(7,indxTime) = -rValues(8,indxTime)                        !Percolation
                END DO
                
            CASE (f_iLandUse_NVRV)
                ALLOCATE (iColList(9) , iDataUnitTypes(9) , cFlowNames(8) , rValues(10,iNTimeSteps))
                cFlowNames = ['Change in Storage' , 'Gain from Land Expansion' , 'Infiltration' , 'GW Inflow' , 'Other Inflow' , 'Stream Inflow for ET' , 'ET' , 'Percolation']
                iColList   = [(indx,indx=91,99)]
                
                !Read data for the interval
                CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,'1MON',cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iNPopulatedValues,rValues,iStat)
                IF (iStat .NE. 0) RETURN
                
                !Calculate monthly averages
                ALLOCATE (rFlows(8,iNPopulatedValues))
                DO indxTime=1,iNPopulatedValues
                    rFlows(1,indxTime) =  rValues(2,indxTime) - rValues(10,indxTime) !Change in storage
                    rFlows(2,indxTime) =  rValues(3,indxTime)                        !Gain from land expansion
                    rFlows(3,indxTime) =  rValues(4,indxTime)                        !Infiltration
                    rFlows(4,indxTime) =  rValues(5,indxTime)                        !GW Inflow
                    rFlows(5,indxTime) =  rValues(6,indxTime)                        !Other Inflow
                    rFlows(6,indxTime) =  rValues(7,indxTime)                        !Stream Inflow for ET
                    rFlows(7,indxTime) = -rValues(8,indxTime)                        !ET
                    rFlows(8,indxTime) = -rValues(9,indxTime)                        !Percolation
                END DO
                
            CASE DEFAULT
                CALL SetLastMessage('Root Zone ZBudget is not available for the selected land use type!',f_iFatal,ThisProcedure)
                DEALLOCATE (rFlows , cFlowNames , STAT=ErrorCode)
                ALLOCATE (rFlows(0,0) , cFlowNames(0))
                iStat = -1
                RETURN
        END SELECT        
    END IF  
    
  END SUBROUTINE RootZone_v412_GetZBudget_MonthlyFlows_GivenFile
    
  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY ZBUDGET FLOWS FROM RootZone OBJECT 
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetZBudget_MonthlyFlows_GivenRootZone(RootZone,iZBudgetType,iLUType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(RootZone_v412_Type),TARGET,INTENT(IN) :: RootZone              
    INTEGER,INTENT(IN)                          :: iZBudgetType,iZoneID,iLUType,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)                 :: cBeginDate,cEndDate  
    REAL(8),INTENT(IN)                          :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)             :: rFlows(:,:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT)    :: cFlowNames(:)
    INTEGER,INTENT(OUT)                         :: iStat
    
    !Local variables
    INTEGER                   :: iZonesWithNames(0)
    CHARACTER                 :: cZoneNames(0)*1 
    TYPE(ZBudgetType),POINTER :: pZBudget
    TYPE(ZoneListType)        :: ZoneList
    
    !Initialize
    NULLIFY(pZBudget)
    
    !Get a pointer to ZBudget file
    SELECT CASE (iZBudgetType)
        CASE (f_iZBudgetType_LWU)
            IF (RootZone%Flags%LWUseZoneBudRawFile_Defined) pZBudget => RootZone%LWUZoneBudRawFile
        CASE (f_iZBudgetType_RootZone)
            IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined) pZBudget => RootZone%RootZoneZoneBudRawFile
    END SELECT
    
    !Return if ZBudget file does not exist
    IF (.NOT.ASSOCIATED(pZBudget)) THEN
        iStat = 0
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
        RETURN
    END IF
    
    !Generate zone list
    CALL ZoneList%New(pZBudget%Header%iNData,pZBudget%Header%lFaceFlows_Defined,pZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Retrieve data
    CALL RootZone_v412_GetZBudget_MonthlyFlows_GivenFile(pZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)

    !Clear memory
    CALL ZoneList%Kill()
    NULLIFY(pZBudget)
    
  END SUBROUTINE RootZone_v412_GetZBudget_MonthlyFlows_GivenRootZone
     
     
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO LAKES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetFlowsToLakes(RootZone,AppGrid,DirectRunoff,ReturnFlow,PondDrain)
    CLASS(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)         :: AppGrid                                    !Not used in this version
    REAL(8),INTENT(OUT)                  :: DirectRunoff(:),ReturnFlow(:),PondDrain(:)
    
    !Local variables
    INTEGER :: iElem,iLake,indx
    LOGICAL :: lNonPondedAg_Defined,lPondedAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    DirectRunoff         = 0.0
    ReturnFlow           = 0.0
    PondDrain            = 0.0
    lNonPondedAg_Defined = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg_Defined    = RootZone%Flags%lPondedAg_Defined
    lUrban_Defined       = RootZone%Flags%lUrban_Defined
    lNVRV_Defined        = RootZone%Flags%lNVRV_Defined
    
    !Flows into lakes
    ASSOCIATE (pFlowData_Ag     => RootZone%ElemFlowToLakes_Ag        , &
               pFlowData_UrbIn  => RootZone%ElemFlowToLakes_UrbIn     , &
               pFlowData_UrbOut => RootZone%ElemFlowToLakes_UrbOut    , &
               pFlowData_NVRV   => RootZone%ElemFlowToLakes_NVRV      , &
               pCrops           => RootZone%NonPondedAgRootZone%Crops , &
               pPondedCrops     => RootZone%PondedAgRootZone%Crops    , &
               pUrban           => RootZone%UrbanRootZone%UrbData     , &
               pNV              => RootZone%NVRVRootZone%NativeVeg    , &
               pRV              => RootZone%NVRVRootZone%RiparianVeg  )
        !Process ag lands
        IF (lNonPondedAg_Defined  .OR.  lPondedAg_Defined) THEN
            DO indx=1,SIZE(pFlowData_Ag)
                !Element ID
                iElem = pFlowData_Ag(indx)%iElement
                
                !Destination lake 
                iLake = pFlowData_Ag(indx)%iDest
                
                !Flows from non-ponded ag lands
                IF (lNonPondedAg_Defined) THEN
                    DirectRunoff(iLake) = DirectRunoff(iLake) + SUM(pCrops%Runoff(:,iElem))
                    ReturnFlow(iLake)   = ReturnFlow(iLake)   + SUM(pCrops%ReturnFlow(:,iElem))
                END IF
                
                !Flows from ponded lands
                IF (lPondedAg_Defined) THEN
                    DirectRunoff(iLake) = DirectRunoff(iLake) + SUM(pPondedCrops%Runoff(:,iElem))
                    ReturnFlow(iLake)   = ReturnFlow(iLake)   + SUM(pPondedCrops%ReturnFlow(:,iElem))
                    PondDrain(iLake)    = PondDrain(iLake)    + SUM(pPondedCrops%Drain(:,iElem))
                END IF
            END DO
        END IF
        
        !Flows from urban lands
        IF (lUrban_Defined) THEN
            !Urban indoors
            DO indx=1,SIZE(pFlowData_UrbIn)
                !Element ID
                iElem = pFlowData_UrbIn(indx)%iElement
                
                !Destination lake 
                iLake = pFlowData_UrbIn(indx)%iDest
                
                ReturnFlow(iLake) = ReturnFlow(iLake) + RootZone%rAW_UrbanIndoors(iElem)
            END DO

            !Urban outdoors
            DO indx=1,SIZE(pFlowData_UrbOut)
                !Element ID
                iElem = pFlowData_UrbOut(indx)%iElement
                
                !Destination lake
                iLake = pFlowData_UrbOut(indx)%iDest
                
                DirectRunoff(iLake) = DirectRunoff(iLake) + pUrban%Runoff(iElem,1)
                ReturnFlow(iLake)   = ReturnFlow(iLake)   + pUrban%ReturnFlow(iElem,1) 
            END DO
        END IF
          
        !Process native&riparian veg
        IF (lNVRV_Defined) THEN
            DO indx=1,SIZE(pFlowData_NVRV)
                !Element ID
                iElem = pFlowData_NVRV(indx)%iElement
                
                !Destination lake
                iLake = pFlowData_NVRV(indx)%iDest
                
                DirectRunoff(iLake) = DirectRunoff(iLake) + pNV%Runoff(iElem,1) + pRV%Runoff(iElem,1)
            END DO
        END IF
    END ASSOCIATE
    
  END SUBROUTINE RootZone_v412_GetFlowsToLakes

  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetFlowsToStreams(RootZone,AppGrid,DirectRunoff,ReturnFlow,PondDrain,RiparianET)
    CLASS(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)         :: AppGrid                                     !Not used in this version
    REAL(8),INTENT(OUT)                  :: DirectRunoff(:),ReturnFlow(:),PondDrain(:)
    REAL(8),INTENT(INOUT)                :: RiparianET(:)                 
    
    !Local variables
    INTEGER :: indx,iStrmNode,iElem
    LOGICAL :: lNonPondedAg_Defined,lPondedAg_Defined,lUrban_Defined,lNVRV_Defined
    
    !Initialize
    DirectRunoff         = 0.0
    ReturnFlow           = 0.0
    PondDrain            = 0.0
    lNonPondedAg_Defined = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg_Defined    = RootZone%Flags%lPondedAg_Defined
    lUrban_Defined       = RootZone%Flags%lUrban_Defined
    lNVRV_Defined        = RootZone%Flags%lNVRV_Defined
    
    !Flows into streams
    ASSOCIATE (pFlowData_Ag     => RootZone%ElemFlowToStreams_Ag      , &
               pFlowData_UrbIn  => RootZone%ElemFlowToStreams_UrbIn   , &
               pFlowData_UrbOut => RootZone%ElemFlowToStreams_UrbOut  , &
               pFlowData_NVRV   => RootZone%ElemFlowToStreams_NVRV    , &
               pCrops           => RootZone%NonPondedAgRootZone%Crops , &
               pPondedCrops     => RootZone%PondedAgRootZone%Crops    , &
               pUrban           => RootZone%UrbanRootZone%UrbData     , &
               pNV              => RootZone%NVRVRootZone%NativeVeg    , &
               pRV              => RootZone%NVRVRootZone%RiparianVeg  )
        !Process ag lands
        IF (lNonPondedAg_Defined  .OR.  lPondedAg_Defined) THEN
            DO indx=1,SIZE(pFlowData_Ag)
                !Element ID
                iElem = pFlowData_Ag(indx)%iElement
                
                !Destination stream node
                iStrmNode = pFlowData_Ag(indx)%iDest
                
                !Flows from non-ponded ag lands
                IF (lNonPondedAg_Defined) THEN
                    DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + SUM(pCrops%Runoff(:,iElem))
                    ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + SUM(pCrops%ReturnFlow(:,iElem))
                END IF
                
                !Flows from ponded lands
                IF (lPondedAg_Defined) THEN
                    DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + SUM(pPondedCrops%Runoff(:,iElem))
                    ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + SUM(pPondedCrops%ReturnFlow(:,iElem))
                    PondDrain(iStrmNode)    = PondDrain(iStrmNode)    + SUM(pPondedCrops%Drain(:,iElem))
                END IF
            END DO
        END IF
        
        !Process urban lands
        IF (lUrban_Defined) THEN
            !Urban indoors
            DO indx=1,SIZE(pFlowData_UrbIn)
                !Element ID
                iElem = pFlowData_UrbIn(indx)%iElement
                
                !Destination stream node
                iStrmNode = pFlowData_UrbIn(indx)%iDest
                
                ReturnFlow(iStrmNode) = ReturnFlow(iStrmNode) + RootZone%rAW_UrbanIndoors(iElem)
            END DO

            !Urban outdoors
            DO indx=1,SIZE(pFlowData_UrbOut)
                !Element ID
                iElem = pFlowData_UrbOut(indx)%iElement
                
                !Destination stream node
                iStrmNode = pFlowData_UrbOut(indx)%iDest
                
                DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + pUrban%Runoff(iElem,1)
                ReturnFlow(iStrmNode)   = ReturnFlow(iStrmNode)   + pUrban%ReturnFlow(iElem,1) 
            END DO
        END IF
    
        !Process native&riparian veg
        IF (lNVRV_Defined) THEN
            DO indx=1,SIZE(pFlowData_NVRV)
                !Element ID
                iElem = pFlowData_NVRV(indx)%iElement
                
                !Destination stream node
                iStrmNode = pFlowData_NVRV(indx)%iDest
                
                DirectRunoff(iStrmNode) = DirectRunoff(iStrmNode) + pNV%Runoff(iElem,1) + pRV%Runoff(iElem,1)
            END DO
        END IF
    END ASSOCIATE
               
    !Flows out of streams
    IF (lNVRV_Defined) THEN
        IF (RootZone%NVRVRootZone%IsRVETFromStrmSimulated()) THEN
            CALL RootZone%NVRVRootZone%GetRequiredET_AtStrmNodes(RiparianET)
        END IF
    END IF
    
  END SUBROUTINE RootZone_v412_GetFlowsToStreams
    
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT ALL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION RootZone_v412_GetPercAll(RootZone,AppGrid) RESULT(Perc)
    CLASS(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    REAL(8)                              :: Perc(AppGrid%NElements)

    !Local variables 
    INTEGER :: indx,iElem,iDestElem
    
    !Initialize
    Perc = 0.0
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        Perc = SUM(RootZone%NonPondedAgRootZone%Crops%Perc , DIM=1) + SUM(RootZone%NonPondedAgRootZone%Crops%PercCh , DIM=1)
        
        !Process surface flows that go to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
            iElem           = RootZone%ElemFlowToGW_Ag(indx)%iElement
            iDestElem       = RootZone%ElemFlowToGW_Ag(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            Perc(iDestElem) = Perc(iDestElem) + SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow(:,iElem)) + SUM(RootZone%NonPondedAgRootZone%Crops%Runoff(:,iElem))
        END DO
    END IF
    
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        Perc = Perc + SUM(RootZone%PondedAgRootZone%Crops%Perc , DIM=1) + SUM(RootZone%PondedAgRootZone%Crops%PercCh , DIM=1)
        
        !Process surface flows that go to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
            iElem           = RootZone%ElemFlowToGW_Ag(indx)%iElement
            iDestElem       = RootZone%ElemFlowToGW_Ag(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            Perc(iDestElem) = Perc(iDestElem) + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(:,iElem)) &
                                              + SUM(RootZone%PondedAgRootZone%Crops%Runoff(:,iElem))     &
                                              + SUM(RootZone%PondedAgRootZone%Crops%Drain(:,iElem))
        END DO
    END IF
    
    !Urban
    IF (RootZone%Flags%lUrban_Defined) THEN
        Perc = Perc + RootZone%UrbanRootZone%UrbData%Perc(:,1) + RootZone%UrbanRootZone%UrbData%PercCh(:,1)
      
        !Process urban indoors return flow that go to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbIn)
            iElem           = RootZone%ElemFlowToGW_UrbIn(indx)%iElement
            iDestElem       = RootZone%ElemFlowToGW_UrbIn(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            Perc(iDestElem) = Perc(iDestElem) + RootZone%rAW_UrbanIndoors(iElem)
        END DO
      
        !Process urban outdoors surface flow that go to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbOut)
            iElem           = RootZone%ElemFlowToGW_UrbOut(indx)%iElement
            iDestElem       = RootZone%ElemFlowToGW_UrbOut(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            Perc(iDestElem) = Perc(iDestElem) + RootZone%UrbanRootZone%UrbData%ReturnFlow(iElem,1) &
                                              + RootZone%UrbanRootZone%UrbData%Runoff(iElem,1)
        END DO
    END IF
    
    !Native and riparian vegetation areas
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Perc = Perc + RootZone%NVRVRootZone%NativeVeg%Perc(:,1)   + RootZone%NVRVRootZone%RiparianVeg%Perc(:,1)  &
                    + RootZone%NVRVRootZone%NativeVeg%PercCh(:,1) + RootZone%NVRVRootZone%RiparianVeg%PercCh(:,1) 
        
        !Process surface flows that go to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_NVRV)
            iElem           = RootZone%ElemFlowToGW_NVRV(indx)%iElement
            iDestElem       = RootZone%ElemFlowToGW_NVRV(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            Perc(iDestElem) = Perc(iDestElem) + RootZone%NVRVRootZone%NativeVeg%Runoff(iElem,1)   &
                                              + RootZone%NVRVRootZone%RiparianVeg%Runoff(iElem,1)
        END DO
    END IF
               
  END FUNCTION RootZone_v412_GetPercAll
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT AN INDIVIDUAL ELEMENT
  ! -------------------------------------------------------------
  FUNCTION RootZone_v412_GetPercElement(RootZone,iElem,AppGrid) RESULT(Perc)
    CLASS(RootZone_v412_Type),INTENT(IN)  :: RootZone
    INTEGER,INTENT(IN)                    :: iElem
    TYPE(AppGridType),OPTIONAL,INTENT(IN) :: AppGrid   !Not used in this version of the root zone component
    REAL(8)                               :: Perc
    
    !Local variables
    INTEGER :: indx,iDestElem,iElemOrigin
    
    !Initialize
    Perc = 0.0
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        Perc = SUM(RootZone%NonPondedAgRootZone%Crops%Perc(:,iElem)) + SUM(RootZone%NonPondedAgRootZone%Crops%PercCh(:,iElem))
    
        !Process surface flows going to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
            iElemOrigin = RootZone%ElemFlowToGW_Ag(indx)%iElement
            iDestElem   = RootZone%ElemFlowToGW_Ag(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElemOrigin
            IF (iDestElem .EQ. iElem) THEN
                Perc = Perc + SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow(:,iElemOrigin)) &
                            + SUM(RootZone%NonPondedAgRootZone%Crops%Runoff(:,iElemOrigin))
            END IF
        END DO
    END IF
    
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        Perc = Perc + SUM(RootZone%PondedAgRootZone%Crops%Perc(:,iElem)) + SUM(RootZone%PondedAgRootZone%Crops%PercCh(:,iElem))
    
        !Process surface flows going to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
            iElemOrigin = RootZone%ElemFlowToGW_Ag(indx)%iElement
            iDestElem   = RootZone%ElemFlowToGW_Ag(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElemOrigin
            IF (iDestElem .EQ. iElem) THEN
                Perc = Perc + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(:,iElemOrigin)) &
                            + SUM(RootZone%PondedAgRootZone%Crops%Runoff(:,iElemOrigin))     &
                            + SUM(RootZone%PondedAgRootZone%Crops%Drain(:,iElemOrigin))
            END IF
        END DO
    END IF
    
    !Urban
    IF (RootZone%Flags%lUrban_Defined) THEN
        Perc = Perc + RootZone%UrbanRootZone%UrbData%Perc(iElem,1) + RootZone%UrbanRootZone%UrbData%PercCh(iElem,1)
    
        !Process urban indoors return flows going to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbIn)
            iElemOrigin = RootZone%ElemFlowToGW_UrbIn(indx)%iElement
            iDestElem   = RootZone%ElemFlowToGW_UrbIn(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElemOrigin
            IF (iDestElem .EQ. iElem) Perc = Perc + RootZone%rAW_UrbanIndoors(iElemOrigin)
        END DO
    
        !Process urban outdoors surface flows going to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbOut)
            iElemOrigin = RootZone%ElemFlowToGW_UrbOut(indx)%iElement
            iDestElem   = RootZone%ElemFlowToGW_UrbOut(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElemOrigin
            IF (iDestElem .EQ. iElem) THEN
                Perc = Perc + RootZone%UrbanRootZone%UrbData%ReturnFlow(iElemOrigin,1)  &
                            + RootZone%UrbanRootZone%UrbData%Runoff(iElemOrigin,1)
            END IF
        END DO
    END IF
    
    !Native and riparian vegetation areas
    IF (RootZone%Flags%lNVRV_Defined) THEN
        Perc = Perc + RootZone%NVRVRootZone%NativeVeg%Perc(iElem,1)   + RootZone%NVRVRootZone%RiparianVeg%Perc(iElem,1)  &
                    + RootZone%NVRVRootZone%NativeVeg%PercCh(iElem,1) + RootZone%NVRVRootZone%RiparianVeg%PercCh(iElem,1) 
      
        !Process surface flows going to gw
        DO indx=1,SIZE(RootZone%ElemFlowToGW_NVRV)
            iElemOrigin = RootZone%ElemFlowToGW_NVRV(indx)%iElement
            iDestElem   = RootZone%ElemFlowToGW_NVRV(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElemOrigin
            IF (iDestElem .EQ. iElem) THEN
                Perc = Perc + RootZone%NVRVRootZone%NativeVeg%Runoff(iElemOrigin,1)   &
                            + RootZone%NVRVRootZone%RiparianVeg%Runoff(iElemOrigin,1)
            END IF
        END DO
    END IF
    
  END FUNCTION RootZone_v412_GetPercElement
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL TOTAL PERCOLATION FROM ALL LAND USES
  ! -------------------------------------------------------------
  FUNCTION RootZone_v412_RegionalPerc(RootZone,AppGrid) RESULT(RPERC)
    CLASS(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    REAL(8)                              :: RPERC(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: indx,iElem,iDestElem,iNSubregions 
    REAL(8) :: rElemValues(AppGrid%NElements),rSurfaceFlow
    LOGICAL :: lNonPondedAg,lPondedAg,lUrban,lNVRV
    
    !Initialize
    lNonPondedAg = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg    = RootZone%Flags%lPondedAg_Defined
    lUrban       = RootZone%Flags%lUrban_Defined
    lNVRV        = RootZone%Flags%lNVRV_Defined
    rElemValues  = 0.0
    
    !Non-ponded ag lands
    IF (lNonPondedAg)  rElemValues = SUM(RootZone%NonPondedAgRootZone%Crops%Perc , DIM=1)  &
                                   + SUM(RootZone%NonPondedAgRootZone%Crops%PercCh , DIM=1)
    
    !Ponded ag lands
    IF (lPondedAg)  rElemValues = rElemValues                                         &
                                + SUM(RootZone%PondedAgRootZone%Crops%Perc , DIM=1)   &
                                + SUM(RootZone%PondedAgRootZone%Crops%PercCh , DIM=1)
    
    !Urban lands
    IF (lUrban)  rElemValues = rElemValues                                        &
                             + SUM(RootZone%UrbanRootZone%UrbData%Perc , DIM=1)   &
                             + SUM(RootZone%UrbanRootZone%UrbData%PercCh , DIM=1)
    
    !Native and riparian veg lands
    IF (lNVRV)  rElemValues = rElemValues                                           &
                            + SUM(RootZone%NVRVRootZone%NativeVeg%Perc , DIM=1)     &
                            + SUM(RootZone%NVRVRootZone%NativeVeg%PercCh , DIM=1)   &
                            + SUM(RootZone%NVRVRootZone%RiparianVeg%Perc , DIM=1)   &
                            + SUM(RootZone%NVRVRootZone%RiparianVeg%PercCh , DIM=1)
    
    !Ag surface flows going to gw
    IF (lNonPondedAg  .OR.  lPondedAg) THEN
        DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
            iElem     = RootZone%ElemFlowToGW_Ag(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_Ag(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            rSurfaceFlow = 0.0
            IF (lNonPondedAg) rSurfaceFlow = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff(:,iElem))     &
                                           + SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow(:,iElem)) 
            IF (lPondedAg)    rSurfaceFlow = rSurfaceFlow                                                &
                                           + SUM(RootZone%PondedAgRootZone%Crops%Runoff(:,iElem))        &
                                           + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(:,iElem))    &
                                           + SUM(RootZone%PondedAgRootZone%Crops%Drain(:,iElem))  
            rElemValues(iDestElem) = rElemValues(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !Urban surface flows going to gw
    IF (lUrban) THEN
        !Urban indoors
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbIn)
            iElem     = RootZone%ElemFlowToGW_UrbIn(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbIn(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            rSurfaceFlow           = RootZone%rAW_UrbanIndoors(iElem)
            rElemValues(iDestElem) = rElemValues(iDestElem) + rSurfaceFlow
        END DO
        
        !Urban outdoors
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbOut)
            iElem     = RootZone%ElemFlowToGW_UrbOut(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbOut(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            rSurfaceFlow           = RootZone%UrbanRootZone%UrbData%Runoff(iElem,1)    &
                                   + RootZone%UrbanRootZone%UrbData%ReturnFlow(iElem,1)
            rElemValues(iDestElem) = rElemValues(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !NVRV surface flows going to gw
    IF (lNVRV) THEN
        DO indx=1,SIZE(RootZone%ElemFlowToGW_NVRV)
            iElem     = RootZone%ElemFlowToGW_NVRV(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_NVRV(indx)%iDest
            IF (iDestElem .EQ. -1) iDestElem = iElem
            rSurfaceFlow = RootZone%NVRVRootZone%NativeVeg%Runoff(iElem,1)     &
                         + RootZone%NVRVRootZone%RiparianVeg%Runoff(iElem,1)  
            rElemValues(iDestElem) = rElemValues(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !Accumulate to subregions
    iNSubregions          = AppGrid%NSubregions
    RPERC(1:iNSubregions) = AppGrid%AccumElemValuesToSubregions(rElemValues)
    RPERC(iNSubregions+1) = SUM(RPERC(1:iNSubregions))
    
  END FUNCTION RootZone_v412_RegionalPerc
  

  ! -------------------------------------------------------------
  ! --- GET SURFACE FLOW DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetSurfaceFlowDestinations(RootZone,iLUType,NElements,Dest)
    CLASS(RootZone_v412_Type),TARGET,INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                          :: iLUType,NElements
    INTEGER,INTENT(OUT)                         :: Dest(NElements)
    
    !Local variables
    INTEGER                                 :: indx,iElem
    INTEGER,POINTER                         :: pFlowDestOutside(:)  
    TYPE(ElemSurfaceFlowToDestType),POINTER :: pFlowDestStreams(:),pFlowDestLakes(:),pFlowDestGW(:)
    
    !Select the destination data based on land use type (iLUTYpe for this version of RootZone must be present)
    SELECT CASE (iLUType)
        CASE (f_iLandUse_Ag)
            pFlowDestOutside => RootZone%ElemFlowToOutside_Ag
            pFlowDestStreams => RootZone%ElemFlowToStreams_Ag
            pFlowDestLakes   => RootZone%ElemFlowToLakes_Ag
            pFlowDestGW      => RootZone%ElemFlowToGW_Ag
        CASE (f_iLandUse_UrbIn)
            pFlowDestOutside => RootZone%ElemFlowToOutside_UrbIn
            pFlowDestStreams => RootZone%ElemFlowToStreams_UrbIn
            pFlowDestLakes   => RootZone%ElemFlowToLakes_UrbIn
            pFlowDestGW      => RootZone%ElemFlowToGW_UrbIn
        CASE (f_iLandUse_UrbOut)
            pFlowDestOutside => RootZone%ElemFlowToOutside_UrbOut
            pFlowDestStreams => RootZone%ElemFlowToStreams_UrbOut
            pFlowDestLakes   => RootZone%ElemFlowToLakes_UrbOut
            pFlowDestGW      => RootZone%ElemFlowToGW_UrbOut
        CASE (f_iLandUse_NVRV)
            pFlowDestOutside => RootZone%ElemFlowToOutside_NVRV
            pFlowDestStreams => RootZone%ElemFlowToStreams_NVRV
            pFlowDestLakes   => RootZone%ElemFlowToLakes_NVRV
            pFlowDestGW      => RootZone%ElemFlowToGW_NVRV
    END SELECT
        
    !To outside
    Dest(pFlowDestOutside) = 0
    
    !To stream nodes
    DO indx=1,SIZE(pFlowDestStreams)
        iElem       = pFlowDestStreams(indx)%iElement        
        Dest(iElem) = pFlowDestStreams(indx)%iDest
    END DO
    
    !To lakes
    DO indx=1,SIZE(pFlowDestLakes)
        iElem       = pFlowDestLakes(indx)%iElement        
        Dest(iElem) = pFlowDestLakes(indx)%iDest
    END DO
    
    !To groundwater
    DO indx=1,SIZE(pFlowDestGW)
        iElem       = pFlowDestGW(indx)%iElement        
        Dest(iElem) = pFlowDestGW(indx)%iDest
        IF (Dest(iElem) .EQ. -1) Dest(iElem) = iElem
    END DO

  END SUBROUTINE RootZone_v412_GetSurfaceFlowDestinations
  
  
  ! -------------------------------------------------------------
  ! --- GET SURFACE FLOW DESTINATION TYPES
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_GetSurfaceFlowDestinationTypes(RootZone,iLUType,NElements,DestTypes)
    CLASS(RootZone_v412_Type),TARGET,INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                          :: iLUType,NElements
    INTEGER,INTENT(OUT)                         :: DestTypes(NElements)
    
    !Local variables
    INTEGER,POINTER                         :: pFlowDestOutside(:)  
    TYPE(ElemSurfaceFlowToDestType),POINTER :: pFlowDestStreams(:),pFlowDestLakes(:),pFlowDestGW(:)
    
    !Select the destination data based on land use type (iLUTYpe for this version of RootZone must be present)
    SELECT CASE (iLUType)
        CASE (f_iLandUse_Ag)
            pFlowDestOutside => RootZone%ElemFlowToOutside_Ag
            pFlowDestStreams => RootZone%ElemFlowToStreams_Ag
            pFlowDestLakes   => RootZone%ElemFlowToLakes_Ag
            pFlowDestGW      => RootZone%ElemFlowToGW_Ag
        CASE (f_iLandUse_UrbIn)
            pFlowDestOutside => RootZone%ElemFlowToOutside_UrbIn
            pFlowDestStreams => RootZone%ElemFlowToStreams_UrbIn
            pFlowDestLakes   => RootZone%ElemFlowToLakes_UrbIn
            pFlowDestGW      => RootZone%ElemFlowToGW_UrbIn
        CASE (f_iLandUse_UrbOut)
            pFlowDestOutside => RootZone%ElemFlowToOutside_UrbOut
            pFlowDestStreams => RootZone%ElemFlowToStreams_UrbOut
            pFlowDestLakes   => RootZone%ElemFlowToLakes_UrbOut
            pFlowDestGW      => RootZone%ElemFlowToGW_UrbOut
        CASE (f_iLandUse_NVRV)
            pFlowDestOutside => RootZone%ElemFlowToOutside_NVRV
            pFlowDestStreams => RootZone%ElemFlowToStreams_NVRV
            pFlowDestLakes   => RootZone%ElemFlowToLakes_NVRV
            pFlowDestGW      => RootZone%ElemFlowToGW_NVRV
    END SELECT
        
    !To outside
    DestTypes(pFlowDestOutside) = f_iFlowDest_Outside
    
    !To stream nodes
    DestTypes(pFlowDestStreams%iElement) = f_iFlowDest_StrmNode
    
    !To lakes
    DestTypes(pFlowDestLakes%iElement) = f_iFlowDest_Lake
    
    !To groundwater
    DestTypes(pFlowDestGW%iElement) = f_iFlowDest_GWElement

  END SUBROUTINE RootZone_v412_GetSurfaceFlowDestinationTypes
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER 
  ! -------------------------------------------------------------
  FUNCTION RootZone_v412_GetVersion(RootZone) RESULT(cVrs)
    CLASS(RootZone_v412_Type) :: RootZone
    CHARACTER(:),ALLOCATABLE  :: cVrs
    
    IF (.NOT. RootZone%Version%IsDefined())   &
        RootZone%Version = RootZone%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = RootZone%Version%GetVersion()
    
  END FUNCTION RootZone_v412_GetVersion
  
  
  
  
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
  ! --- READ ROOT ZONE RELATED TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_ReadTSData(RootZone,AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas)
    CLASS(RootZone_v412_Type)          :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ETData
    INTEGER,INTENT(OUT)                :: iStat
    REAL(8),OPTIONAL,INTENT(IN)        :: RegionLUAreas(:,:)   !In (region, land use) format
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24)                                 :: ThisProcedure = ModName // 'RootZone_v412_ReadTSData'
    INTEGER                                                      :: iFileReadError,indx,ID,iCol,indxElem,iErrorCode,                                                      &  
                                                                    iNElemFlowToOutside_Ag,iNElemFlowToOutside_UrbIn,iNElemFlowToOutside_UrbOut,iNElemFlowToOutside_NVRV, &
                                                                    iNElemFlowToStreams_Ag,iNElemFlowToStreams_UrbIn,iNElemFlowToStreams_UrbOut,iNElemFlowToStreams_NVRV, &
                                                                    iNElemFlowToLakes_Ag,iNElemFlowToLakes_UrbIn,iNElemFlowToLakes_UrbOut,iNElemFlowToLakes_NVRV,         &
                                                                    iNElemFlowToGW_Ag,iNElemFlowToGW_UrbIn,iNElemFlowToGW_UrbOut,iNElemFlowToGW_NVRV 
    INTEGER,DIMENSION(AppGrid%NElements)                         :: ElemFlowToOutside_Ag,ElemFlowToOutside_UrbIn,ElemFlowToOutside_UrbOut,ElemFlowToOutside_NVRV, &
                                                                    iElemIDs                                                   
    TYPE(ElemSurfaceFlowToDestType),DIMENSION(AppGrid%NElements) :: ElemFlowToStreams_Ag,ElemFlowToStreams_UrbIn,ElemFlowToStreams_UrbOut,ElemFlowToStreams_NVRV, &
                                                                    ElemFlowToLakes_Ag,ElemFlowToLakes_UrbIn,ElemFlowToLakes_UrbOut,ElemFlowToLakes_NVRV,         &
                                                                    ElemFlowToGW_Ag,ElemFlowToGW_UrbIn,ElemFlowToGW_UrbOut,ElemFlowToGW_NVRV 

    !First read the timeseries data for the parent class
    IF (PRESENT(RegionLUAreas)) THEN
        CALL RootZone%RootZone_v411_Type%ReadTSData(AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas)
    ELSE 
        CALL RootZone%RootZone_v411_Type%ReadTSData(AppGrid,TimeStep,Precip,ETData,iStat)
    END IF 
    IF (iStat .NE. 0) RETURN
    
    !Read surface flow destination data
    CALL RootZone%SurfaceFlowDestinationFile%ReadTSData(TimeStep,'Surface flow destination data',iFileReadError,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Process surface flow destination data
    IF (RootZone%SurfaceFlowDestinationFile%IsUpdated()) THEN
        iElemIDs = AppGrid%AppElement%ID
        ASSOCIATE (pDestTypes => RootZone%SurfaceFlowDestinationFile%iValues1 , &
                   pDests     => RootZone%SurfaceFlowDestinationFile%iValues2 )
            !Convert destination IDs to indices
            DO indx=1,RootZone%SurfaceFlowDestinationFile%GetNDataColumns()
                SELECT CASE (pDestTypes(indx))
                    CASE (f_iFlowDest_Outside)
                        !Do nothing
                    
                    CASE (f_iFlowDest_StrmNode)
                        IF (RootZone%lStrmNodeIDs_Provided) THEN
                            ID           = pDests(indx)
                            pDests(indx) = LocateInList(ID,RootZone%iStrmNodeIDs)
                            IF (pDests(indx) .EQ. 0) THEN
                                CALL SetLastMessage('Stream node ID '//TRIM(IntToText(ID))//' listed in the Surface Flow Destination file in the Root Zone component is not in the model!',f_iFatal,ThisProcedure)
                                iStat = -1
                                RETURN
                            END IF
                        END IF
                        
                    CASE (f_iFlowDest_Lake)
                        IF (RootZone%lLakeIDs_Provided) THEN
                            ID           = pDests(indx)
                            pDests(indx) = LocateInList(ID,RootZone%iLakeIDs)
                            IF (pDests(indx) .EQ. 0) THEN
                                CALL SetLastMessage('Lake ID '//TRIM(IntToText(ID))//' listed in the Surface Flow Destination file in the Root Zone component is not in the model!',f_iFatal,ThisProcedure)
                                iStat = -1
                                RETURN
                            END IF
                        END IF
                        
                    CASE (f_iFlowDest_GWElement)
                        ID = pDests(indx)
                        IF (ID .NE. -1) THEN
                            pDests(indx) = LocateInList(ID,iElemIDs)
                            IF (pDests(indx) .EQ. 0) THEN
                                CALL SetLastMessage('Element ID '//TRIM(IntToText(ID))//' listed the Surface Flow Destination file in the Root Zone component is not in the model!',f_iFatal,ThisProcedure)
                                iStat = -1
                                RETURN
                            END IF
                        END IF
                        
                    CASE DEFAULT
                        MessageArray(1) = "Surface flow destination type '"//TRIM(IntToText(pDestTypes(indx)))//"' listed in the Surface "
                        MessageArray(2) = " Flow Destination file in the Root Zone component is not recognized!"
                        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                END SELECT
            END DO 
            
            !Compile which element-land use flows to which destination type and ID
            iNElemFlowToOutside_Ag     = 0
            iNElemFlowToStreams_Ag     = 0
            iNElemFlowToLakes_Ag       = 0
            iNElemFlowToGW_Ag          = 0
            iNElemFlowToOutside_UrbIn  = 0
            iNElemFlowToStreams_UrbIn  = 0
            iNElemFlowToLakes_UrbIn    = 0
            iNElemFlowToGW_UrbIn       = 0
            iNElemFlowToOutside_UrbOut = 0
            iNElemFlowToStreams_UrbOut = 0
            iNElemFlowToLakes_UrbOut   = 0
            iNElemFlowToGW_UrbOut      = 0
            iNElemFlowToOutside_NVRV   = 0
            iNElemFlowToStreams_NVRV   = 0
            iNElemFlowToLakes_NVRV     = 0
            iNElemFlowToGW_NVRV        = 0
            DO indxElem=1,AppGrid%NElements
                !Process ag surface flow destinations
                iCol = RootZone%iColSurfaceFlowDestination_Ag(indxElem)
                CALL CompileElemFlowToDest(pDestTypes(iCol),pDests(iCol),indxElem,ElemFlowToOutside_Ag,ElemFlowToStreams_Ag,ElemFlowToLakes_Ag,ElemFlowToGW_Ag,iNElemFlowToOutside_Ag,iNElemFlowToStreams_Ag,iNElemFlowToLakes_Ag,iNElemFlowToGW_Ag)
                
                !Process urban indoors
                iCol = RootZone%iColSurfaceFlowDestination_UrbIndoors(indxElem)
                CALL CompileElemFlowToDest(pDestTypes(iCol),pDests(iCol),indxElem,ElemFlowToOutside_UrbIn,ElemFlowToStreams_UrbIn,ElemFlowToLakes_UrbIn,ElemFlowToGW_UrbIn,iNElemFlowToOutside_UrbIn,iNElemFlowToStreams_UrbIn,iNElemFlowToLakes_UrbIn,iNElemFlowToGW_UrbIn)
                
                !Process urban outdoors
                iCol = RootZone%iColSurfaceFlowDestination_UrbOutdoors(indxElem)
                CALL CompileElemFlowToDest(pDestTypes(iCol),pDests(iCol),indxElem,ElemFlowToOutside_UrbOut,ElemFlowToStreams_UrbOut,ElemFlowToLakes_UrbOut,ElemFlowToGW_UrbOut,iNElemFlowToOutside_UrbOut,iNElemFlowToStreams_UrbOut,iNElemFlowToLakes_UrbOut,iNElemFlowToGW_UrbOut)
                
                !Process native&riparian veg
                iCol = RootZone%iColSurfaceFlowDestination_NVRV(indxElem)
                CALL CompileElemFlowToDest(pDestTypes(iCol),pDests(iCol),indxElem,ElemFlowToOutside_NVRV,ElemFlowToStreams_NVRV,ElemFlowToLakes_NVRV,ElemFlowToGW_NVRV,iNElemFlowToOutside_NVRV,iNElemFlowToStreams_NVRV,iNElemFlowToLakes_NVRV,iNElemFlowToGW_NVRV)
            END DO
            
            DEALLOCATE (RootZone%ElemFlowToOutside_Ag , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToOutside_Ag(iNElemFlowToOutside_Ag))
            RootZone%ElemFlowToOutside_Ag = ElemFlowToOutside_Ag(1:iNElemFlowToOutside_Ag)
            
            DEALLOCATE (RootZone%ElemFlowToStreams_Ag , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToStreams_Ag(iNElemFlowToStreams_Ag))
            RootZone%ElemFlowToStreams_Ag = ElemFlowToStreams_Ag(1:iNElemFlowToStreams_Ag)
            
            DEALLOCATE (RootZone%ElemFlowToLakes_Ag , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToLakes_Ag(iNElemFlowToLakes_Ag))
            RootZone%ElemFlowToLakes_Ag = ElemFlowToLakes_Ag(1:iNElemFlowToLakes_Ag)
            
            DEALLOCATE (RootZone%ElemFlowToGW_Ag , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToGW_Ag(iNElemFlowToGW_Ag))
            RootZone%ElemFlowToGW_Ag = ElemFlowToGW_Ag(1:iNElemFlowToGW_Ag)
            
            DEALLOCATE (RootZone%ElemFlowToOutside_UrbIn , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToOutside_UrbIn(iNElemFlowToOutside_UrbIn))
            RootZone%ElemFlowToOutside_UrbIn = ElemFlowToOutside_UrbIn(1:iNElemFlowToOutside_UrbIn)
            
            DEALLOCATE (RootZone%ElemFlowToStreams_UrbIn , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToStreams_UrbIn(iNElemFlowToStreams_UrbIn))
            RootZone%ElemFlowToStreams_UrbIn = ElemFlowToStreams_UrbIn(1:iNElemFlowToStreams_UrbIn)
            
            DEALLOCATE (RootZone%ElemFlowToLakes_UrbIn , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToLakes_UrbIn(iNElemFlowToLakes_UrbIn))
            RootZone%ElemFlowToLakes_UrbIn = ElemFlowToLakes_UrbIn(1:iNElemFlowToLakes_UrbIn)
            
            DEALLOCATE (RootZone%ElemFlowToGW_UrbIn , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToGW_UrbIn(iNElemFlowToGW_UrbIn))
            RootZone%ElemFlowToGW_UrbIn = ElemFlowToGW_UrbIn(1:iNElemFlowToGW_UrbIn)
            
            DEALLOCATE (RootZone%ElemFlowToOutside_UrbOut , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToOutside_UrbOut(iNElemFlowToOutside_UrbOut))
            RootZone%ElemFlowToOutside_UrbOut = ElemFlowToOutside_UrbOut(1:iNElemFlowToOutside_UrbOut)
            
            DEALLOCATE (RootZone%ElemFlowToStreams_UrbOut , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToStreams_UrbOut(iNElemFlowToStreams_UrbOut))
            RootZone%ElemFlowToStreams_UrbOut = ElemFlowToStreams_UrbOut(1:iNElemFlowToStreams_UrbOut)
            
            DEALLOCATE (RootZone%ElemFlowToLakes_UrbOut , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToLakes_UrbOut(iNElemFlowToLakes_UrbOut))
            RootZone%ElemFlowToLakes_UrbOut = ElemFlowToLakes_UrbOut(1:iNElemFlowToLakes_UrbOut)
            
            DEALLOCATE (RootZone%ElemFlowToGW_UrbOut , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToGW_UrbOut(iNElemFlowToGW_UrbOut))
            RootZone%ElemFlowToGW_UrbOut = ElemFlowToGW_UrbOut(1:iNElemFlowToGW_UrbOut)
            
            DEALLOCATE (RootZone%ElemFlowToOutside_NVRV , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToOutside_NVRV(iNElemFlowToOutside_NVRV))
            RootZone%ElemFlowToOutside_NVRV = ElemFlowToOutside_NVRV(1:iNElemFlowToOutside_NVRV)
            
            DEALLOCATE (RootZone%ElemFlowToStreams_NVRV , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToStreams_NVRV(iNElemFlowToStreams_NVRV))
            RootZone%ElemFlowToStreams_NVRV = ElemFlowToStreams_NVRV(1:iNElemFlowToStreams_NVRV)
            
            DEALLOCATE (RootZone%ElemFlowToLakes_NVRV , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToLakes_NVRV(iNElemFlowToLakes_NVRV))
            RootZone%ElemFlowToLakes_NVRV = ElemFlowToLakes_NVRV(1:iNElemFlowToLakes_NVRV)
            
            DEALLOCATE (RootZone%ElemFlowToGW_NVRV , STAT=iErrorCode)
            ALLOCATE (RootZone%ElemFlowToGW_NVRV(iNElemFlowToGW_NVRV))
            RootZone%ElemFlowToGW_NVRV = ElemFlowToGW_NVRV(1:iNElemFlowToGW_NVRV)
        END ASSOCIATE
    END IF
  
    
  CONTAINS
  
  
    ! #############################################################
    ! ### COMPILE ELEMENT TO DESTINATION LIST
    ! #############################################################
    SUBROUTINE CompileElemFlowToDest(iDestType,iDest,iElem,ElemFlowToOutside,ElemFlowToStreams,ElemFlowToLakes,ElemFlowToGW,iCountOutside,iCountStream,iCountLake,iCountGW)
      INTEGER,INTENT(IN)              :: iDestType,iDest,iElem
      TYPE(ElemSurfaceFlowToDestType) :: ElemFlowToStreams(:),ElemFlowToLakes(:),ElemFlowToGW(:)
      INTEGER                         :: ElemFlowToOutside(:),iCountOutside,iCountStream,iCountLake,iCountGW
      
      SELECT CASE (iDestType)
          CASE (f_iFlowDest_Outside)
             iCountOutside                    = iCountOutside + 1
             ElemFlowToOutside(iCountOutside) = iElem
             
          CASE (f_iFlowDest_StrmNode)
              iCountStream                             = iCountStream + 1
              ElemFlowToStreams(iCountStream)%iElement = iElem
              ElemFlowToStreams(iCountStream)%iDest    = iDest
             
          CASE (f_iFlowDest_Lake)
              iCountLake                           = iCountLake + 1
              ElemFlowToLakes(iCountLake)%iElement = iElem
              ElemFlowToLakes(iCountLake)%iDest    = iDest
             
          CASE (f_iFlowDest_GWElement)
              iCountGW                        = iCountGW + 1
              ElemFlowToGW(iCountGW)%iElement = iElem
              ElemFlowToGW(iCountGW)%iDest    = iDest
      END SELECT
      
    END SUBROUTINE CompileElemFlowToDest
    
  END SUBROUTINE RootZone_v412_ReadTSData
  
  


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
  SUBROUTINE RootZone_v412_PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation)
    CLASS(RootZone_v412_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETType),INTENT(IN)       :: ETData
    TYPE(TimeStepType),INTENT(IN) :: TimeStep           !Not used in this version
    LOGICAL,INTENT(IN)            :: lEndOfSimulation   !Not used in this version
    
    !Local variables
    REAL(8),DIMENSION(AppGrid%NElements)     :: rElemPrecip,rDemandFrac
    REAL(8),DIMENSION(AppGrid%NElements,1)   :: rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgETGW_NP,                 &
                                                rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgETGW_Rice,         &
                                                rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgETGW_Refuge, &
                                                rUrbArea,rUrbPump,rUrbDeli                            
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb,RLUArea_Ag,RLUArea_Urb
    
    !Initialize
    rElemPrecip = RootZone%ElemPrecipData%Precip
    
    !Print non-ponded ag results
    IF (RootZone%Flags%lNonPondedAg_Defined) CALL PrintNonPondedAgResults(RootZone,AppGrid)
    
    !Print ponded ag results
    IF (RootZone%Flags%lPondedAg_Defined) CALL PrintPondedAgResults(RootZone,AppGrid)
    
    !Compute variables necessary for both land&water use and root zone budget files
    IF (RootZone%Flags%LWUseBudRawFile_Defined .OR. RootZone%Flags%RootZoneBudRawFile_Defined) THEN
        RPump_Ag    = RootZone%RootZone_v411_Type%RegionalPumping(AppGrid,f_iLandUse_Ag)
        RPump_Urb   = RootZone%RootZone_v411_Type%RegionalPumping(AppGrid,f_iLandUse_Urb)
        RDeli_Ag    = RootZone%RootZone_v411_Type%RegionalDeliveries(AppGrid,f_iLandUse_Ag)
        RDeli_Urb   = RootZone%RootZone_v411_Type%RegionalDeliveries(AppGrid,f_iLandUse_Urb)
        RLUArea_Ag  = RootZone%RootZone_v411_Type%RegionalLUArea(AppGrid,f_iLandUse_Ag)
        RLUArea_Urb = RootZone%RootZone_v411_Type%RegionalLUArea(AppGrid,f_iLandUse_Urb)
    END IF
    
    !Print out land and water use budget 
    IF (RootZone%Flags%LWUseBudRawFile_Defined)  &
        CALL WriteLWUseFlowsToBudRawFile(RootZone,AppGrid,RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb)
    
    !Print out root zone budget 
    IF (RootZone%Flags%RootZoneBudRawFile_Defined)  &
        CALL WriteRootZoneFlowsToBudRawFile(RootZone,AppGrid,RPump_Ag,RDeli_Ag,RPump_Urb,RDeli_Urb,RLUArea_Ag,RLUArea_Urb)
    
    !Compile data that will be used for both Z-Budget output
    IF (RootZone%Flags%LWUseZoneBudRawFile_Defined  .OR.  RootZone%Flags%RootZoneZoneBudRawFile_Defined) THEN   
        !Non-ponded ag data
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
            rAgArea_NP(:,1) = SUM(RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
            rDemandFrac     = SUM(RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac_Ag , DIM=1)     !Ratio of non-ponded crop demand to the total ag demand in the element
            rAgPump_NP(:,1) = RootZone%ElemSupply%Pumping_Ag   * rDemandFrac
            rAgDeli_NP(:,1) = RootZone%ElemSupply%Diversion_Ag * rDemandFrac
            IF (RootZone%Flags%lComputeETFromGW)  &
                rAgETGW_NP(:,1) = SUM(RootZone%NonPondedAgRootZone%Crops%ETFromGW_Actual , DIM=1)
        END IF
        
        !Rice and refuge areas
        IF (RootZone%Flags%lPondedAg_Defined)  THEN
            !Rice
            rAgArea_Rice(:,1) = SUM(RootZone%PondedAgRootZone%Crops%Area(1:3,:) , DIM=1)
            rDemandFrac       = SUM(RootZone%PondedAgRootZone%Crops%ElemDemandFrac_Ag(1:3,:), DIM=1)          !Ratio of rice demand to the total ag demand in the element
            rAgPump_Rice(:,1) = RootZone%ElemSupply%Pumping_Ag   * rDemandFrac
            rAgDeli_Rice(:,1) = RootZone%ElemSupply%Diversion_Ag * rDemandFrac
            IF (RootZone%Flags%lComputeETFromGW)  &
                rAgETGW_Rice(:,1) = SUM(RootZone%PondedAgRootZone%Crops%ETFromGW_Actual(1:3,:) , DIM=1)

            !Refuge
            rAgArea_Refuge(:,1) = SUM(RootZone%PondedAgRootZone%Crops%Area(4:5,:) , DIM=1)
            rDemandFrac         = SUM(RootZone%PondedAgRootZone%Crops%ElemDemandFrac_Ag(4:5,:) , DIM=1)         !Ratio of refuge demand to the total ag demand in the element
            rAgPump_Refuge(:,1) = RootZone%ElemSupply%Pumping_Ag   * rDemandFrac
            rAgDeli_Refuge(:,1) = RootZone%ElemSupply%Diversion_Ag * rDemandFrac
            IF (RootZone%Flags%lComputeETFromGW)  &
                rAgETGW_Refuge(:,1) = SUM(RootZone%PondedAgRootZone%Crops%ETFromGW_Actual(4:5,:) , DIM=1)
        END IF
        
        !Urban data
        IF (RootZone%Flags%lUrban_Defined) THEN
            rUrbArea(:,1) = RootZone%UrbanRootZone%UrbData%Area(:,1)
            rUrbPump(:,1) = RootZone%ElemSupply%Pumping_Urb   
            rUrbDeli(:,1) = RootZone%ElemSupply%Diversion_Urb  
        END IF
    END IF
    
    !Print out land and water use z-budget (from RootZone_v411)
    IF (RootZone%Flags%LWUseZoneBudRawFile_Defined)  &
        CALL WriteLWUseFlowsToZoneBudRawFile(RootZone,AppGrid,                                            &
                                             rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgETGW_NP,                 &
                                             rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgETGW_Rice,         &
                                             rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgETGW_Refuge, &
                                             rUrbArea,rUrbPump,rUrbDeli                                   )
    
    !Root zone zone budget
    IF (RootZone%Flags%RootZoneZoneBudRawFile_Defined)   &
        CALL WriteRootZoneFlowsToZoneBudRawFile(RootZone,AppGrid,ETData,                                     &
                                                rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgETGW_NP,                 &
                                                rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgETGW_Rice,         &
                                                rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgETGW_Refuge, &
                                                rUrbArea,rUrbPump,rUrbDeli                                   )
          
  END SUBROUTINE RootZone_v412_PrintResults

  
  ! -------------------------------------------------------------
  ! --- PRINT NON-PONDED AG RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintNonPondedAgResults(RootZone,AppGrid)
    TYPE(RootZone_v412_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    
    !Local variables
    INTEGER                                                                              :: iNBudgetCrops,indxLast,indxCrop,indxElem,iCrop
    REAL(8),DIMENSION(AppGrid%NElements)                                                 :: rElemValues
    REAL(8),DIMENSION(AppGrid%NElements,RootZone%NonPondedAgRootZone%NBudgetCrops)       :: rDemandFrac,rArea
    REAL(8),DIMENSION(AppGrid%NSubregions+1)                                             :: RSrfcInForGW_Ag_Local,RSrfcInForGW_UrbIn_Local,         &
                                                                                            RSrfcInForGW_UrbOut_Local,RSrfcInForGW_NVRV_Local,      &
                                                                                            RInfilt_Local,RPerc_Local
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*RootZone%NonPondedAgRootZone%NBudgetCrops) :: RPump,RDeli,RRunoff,RLUArea,RGenMoistInflow,RReturn,    &
                                                                                            RInfilt,RPerc,RSrfcInForGW_Ag,RSrfcInForGW_UrbIn,       &
                                                                                            RSrfcInForGW_UrbOut,RSrfcInForGW_NVRV,RZeroDrain
    
    !Return if no output is desired
    IF (RootZone%NonPondedAgRootZone%NBudgetCrops .EQ. 0) RETURN
    
    ASSOCIATE (pNonPondedAg => RootZone%NonPondedAgRootZone)
        !Initialize
        iNBudgetCrops = pNonPondedAg%NBudgetCrops
        indxLast      = iNBudgetCrops * AppGrid%NSubregions
        
        !Compute crop specific flows
        IF (pNonPondedAg%lRootZoneBudRawFile_Defined) THEN
            DO indxCrop=1,iNBudgetCrops
                iCrop = pNonPondedAg%iBudgetCrops(indxCrop)
                CALL RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,.TRUE.,f_iLandUse_NonPondedAg,iCrop,RSrfcInForGW_Ag_Local,RSrfcInForGW_UrbIn_Local,RSrfcInForGW_UrbOut_Local,RSrfcInForGW_NVRV_Local,RInfilt_Local,RPerc_Local)
                RSrfcInForGW_Ag(indxCrop::iNBudgetCrops)     = RSrfcInForGW_Ag_Local
                RSrfcInForGW_UrbIn(indxCrop::iNBudgetCrops)  = RSrfcInForGW_UrbIn_Local
                RSrfcInForGW_UrbOut(indxCrop::iNBudgetCrops) = RSrfcInForGW_UrbOut_Local
                RSrfcInForGW_NVRV(indxCrop::iNBudgetCrops)   = RSrfcInForGW_NVRV_Local
                RInfilt(indxCrop::iNBudgetCrops)             = RInfilt_Local
                RPerc(indxCrop::iNBudgetCrops)               = RPerc_Local
                rElemValues                                  = pNonPondedAg%Crops%Runoff(iCrop,:)
                RRunoff(indxCrop:indxLast:iNBudgetCrops)     = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RRunoff(indxLast+indxCrop)                   = SUM(RRunoff(indxCrop:indxLast:iNBudgetCrops))
                rElemValues                                  = pNonPondedAg%Crops%ReturnFlow(iCrop,:)
                RReturn(indxCrop:indxLast:iNBudgetCrops)     = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RReturn(indxLast+indxCrop)                   = SUM(RReturn(indxCrop:indxLast:iNBudgetCrops))
            END DO
        END IF
            
        !Compute variables necessary for both land&water use and root zone budget files
        IF (pNonPondedAg%lLWUseBudRawFile_Defined .OR. pNonPondedAg%lRootZoneBudRawFile_Defined) THEN
            DO indxElem=1,AppGrid%NElements
                DO indxCrop=1,iNBudgetCrops 
                    rDemandFrac(indxElem,indxCrop) = pNonPondedAg%Crops%ElemDemandFrac_Ag(pNonPondedAg%iBudgetCrops(indxCrop),indxElem)
                    rArea(indxElem,indxCrop)       = pNonPondedAg%Crops%Area(pNonPondedAg%iBudgetCrops(indxCrop),indxElem)
                END DO
            END DO
            DO indxCrop=1,iNBudgetCrops
                iCrop                                            = pNonPondedAg%iBudgetCrops(indxCrop)
                rElemValues                                      = RootZone%ElemSupply%Pumping_Ag * rDemandFrac(:,indxCrop)
                RPump(indxCrop:indxLast:iNBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RPump(indxLast+indxCrop)                         = SUM(RPump(indxCrop:indxLast:iNBudgetCrops))
                rElemValues                                      = RootZone%ElemSupply%Diversion_Ag * rDemandFrac(:,indxCrop)
                RDeli(indxCrop:indxLast:iNBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RDeli(indxLast+indxCrop)                         = SUM(RDeli(indxCrop:indxLast:iNBudgetCrops))
                rElemValues                                      = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * pNonPondedAg%RootDepth(iCrop) - pNonPondedAg%Crops%GMExcess(indxCrop,:)) * rArea(:,indxCrop)
                RGenMoistInflow(indxCrop:indxLast:iNBudgetCrops) = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RGenMoistInflow(indxLast+indxCrop)               = SUM(RGenMoistInflow(indxCrop:indxLast:iNBudgetCrops))
                RLUArea(indxCrop:indxLast:iNBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(rArea(:,indxCrop))
                RLUArea(indxLast+indxCrop)                       = SUM(RLUArea(indxCrop:indxLast:iNBudgetCrops))
            END DO
        END IF
        
        IF (pNonPondedAg%lLWUseBudRawFile_Defined)  &
            CALL WriteAgLWUseFlowsToBudRawFile(pNonPondedAg%LWUseBudRawFile,       &
                                               AppGrid,                            &
                                               pNonPondedAg%iBudgetCrops,          &
                                               RLUArea,RPump,RDeli,                &
                                               pNonPondedAg%Crops%DemandRaw,       &
                                               pNonPondedAg%Crops%Demand,          &
                                               pNonPondedAg%Crops%ETAW,            &
                                               pNonPondedAg%Crops%ETP,             &
                                               pNonPondedAg%Crops%ETFromGW_Actual, &
                                               pNonPondedAg%Crops%ETOth            )
        
        IF (pNonPondedAg%lRootZoneBudRawFile_Defined) THEN
            rZeroDrain = 0.0
            CALL WriteAgRootZoneFlowsToBudRawFile(pNonPondedAg%RootZoneBudRawFile         , &
                                                  AppGrid                                 , &
                                                  pNonPondedAg%iBudgetCrops               , &
                                                  pNonPondedAg%Crops%GenericLandUseGWType , &
                                                  RLUArea                                 , &
                                                  RootZone%ElemPrecipData%Precip          , &
                                                  RRunoff,RPump,RDeli                     , &
                                                  pNonPondedAg%Crops%Reuse                , &
                                                  RReturn                                 , &
                                                  RSrfcInForGW_Ag                         , &
                                                  RSrfcInForGW_UrbIn                      , &
                                                  RSrfcInForGW_UrbOut                     , &
                                                  RSrfcInForGW_NVRV                       , &
                                                  RGenMoistInflow                         , &
                                                  RInfilt                                 , &
                                                  RPerc                                   , &
                                                  RZeroDrain                              , &
                                                  pNonPondedAg%RegionETPot                )
        END IF
    END ASSOCIATE
    
  END SUBROUTINE PrintNonPondedAgResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT PONDED AG RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintPondedAgResults(RootZone,AppGrid)
    TYPE(RootZone_v412_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    
    !Local variables
    INTEGER                                                                           :: iNBudgetCrops,indxLast,indxCrop,indxElem,iCrop
    REAL(8),DIMENSION(AppGrid%NElements)                                              :: rElemValues
    REAL(8),DIMENSION(AppGrid%NElements,RootZone%PondedAgRootZone%NBudgetCrops)       :: rDemandFrac,rArea
    REAL(8),DIMENSION(AppGrid%NSubregions+1)                                          :: RSrfcInForGW_Ag_Local,RSrfcInForGW_UrbIn_Local,         &
                                                                                         RSrfcInForGW_UrbOut_Local,RSrfcInForGW_NVRV_Local,      &
                                                                                         RInfilt_Local,RPerc_Local
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*RootZone%PondedAgRootZone%NBudgetCrops) :: RPump,RDeli,RRunoff,RLUArea,RGenMoistInflow,RReturn,    &
                                                                                         RInfilt,RPerc,RSrfcInForGW_Ag,RSrfcInForGW_UrbIn,       &
                                                                                         RSrfcInForGW_UrbOut,RSrfcInForGW_NVRV,RDrain    
    !Return if no output is desired
    IF (RootZone%PondedAgRootZone%NBudgetCrops .EQ. 0) RETURN
    
    ASSOCIATE (pPondedAg => RootZone%PondedAgRootZone)
        !Initialize
        iNBudgetCrops = pPondedAg%NBudgetCrops
        indxLast      = iNBudgetCrops * AppGrid%NSubregions
        
        !Compute crop specific flows
        IF (pPondedAg%lRootZoneBudRawFile_Defined) THEN
            DO indxCrop=1,iNBudgetCrops
                iCrop = pPondedAg%iBudgetCrops(indxCrop)
                CALL RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,.TRUE.,f_iLandUse_PondedAg,iCrop,RSrfcInForGW_Ag_Local,RSrfcInForGW_UrbIn_Local,RSrfcInForGW_UrbOut_Local,RSrfcInForGW_NVRV_Local,RInfilt_Local,RPerc_Local)
                RSrfcInForGW_Ag(indxCrop::iNBudgetCrops)     = RSrfcInForGW_Ag_Local
                RSrfcInForGW_UrbIn(indxCrop::iNBudgetCrops)  = RSrfcInForGW_UrbIn_Local
                RSrfcInForGW_UrbOut(indxCrop::iNBudgetCrops) = RSrfcInForGW_UrbOut_Local
                RSrfcInForGW_NVRV(indxCrop::iNBudgetCrops)   = RSrfcInForGW_NVRV_Local
                RInfilt(indxCrop::iNBudgetCrops)             = RInfilt_Local
                RPerc(indxCrop::iNBudgetCrops)               = RPerc_Local
                rElemValues                                  = pPondedAg%Crops%Runoff(iCrop,:)
                RRunoff(indxCrop:indxLast:iNBudgetCrops)     = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RRunoff(indxLast+indxCrop)                   = SUM(RRunoff(indxCrop:indxLast:iNBudgetCrops))
                rElemValues                                  = pPondedAg%Crops%ReturnFlow(iCrop,:)
                RReturn(indxCrop:indxLast:iNBudgetCrops)     = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RReturn(indxLast+indxCrop)                   = SUM(RReturn(indxCrop:indxLast:iNBudgetCrops))
            END DO
        END IF
            
        !Compute variables necessary for both land&water use and root zone budget files
        IF (pPondedAg%lLWUseBudRawFile_Defined .OR. pPondedAg%lRootZoneBudRawFile_Defined) THEN
            DO indxElem=1,AppGrid%NElements
                DO indxCrop=1,iNBudgetCrops 
                    rDemandFrac(indxElem,indxCrop) = pPondedAg%Crops%ElemDemandFrac_Ag(pPondedAg%iBudgetCrops(indxCrop),indxElem)
                    rArea(indxElem,indxCrop)       = pPondedAg%Crops%Area(pPondedAg%iBudgetCrops(indxCrop),indxElem)
                END DO
            END DO
            DO indxCrop=1,iNBudgetCrops
                iCrop                                            = pPondedAg%iBudgetCrops(indxCrop)
                rElemValues                                      = RootZone%ElemSupply%Pumping_Ag * rDemandFrac(:,indxCrop)
                RPump(indxCrop:indxLast:iNBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RPump(indxLast+indxCrop)                         = SUM(RPump(indxCrop:indxLast:iNBudgetCrops))
                rElemValues                                      = RootZone%ElemSupply%Diversion_Ag * rDemandFrac(:,indxCrop)
                RDeli(indxCrop:indxLast:iNBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RDeli(indxLast+indxCrop)                         = SUM(RDeli(indxCrop:indxLast:iNBudgetCrops))
                rElemValues                                      = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * pPondedAg%RootDepth(iCrop) - pPondedAg%Crops%GMExcess(indxCrop,:)) * rArea(:,indxCrop)
                RGenMoistInflow(indxCrop:indxLast:iNBudgetCrops) = AppGrid%AccumElemValuesToSubregions(rElemValues)
                RGenMoistInflow(indxLast+indxCrop)               = SUM(RGenMoistInflow(indxCrop:indxLast:iNBudgetCrops))
                RLUArea(indxCrop:indxLast:iNBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(rArea(:,indxCrop))
                RLUArea(indxLast+indxCrop)                       = SUM(RLUArea(indxCrop:indxLast:iNBudgetCrops))
                RDrain(indxCrop:indxLast:iNBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(pPondedAg%Crops%Drain(iCrop,:))
                RDrain(indxLast+indxCrop)                        = SUM(RDrain(indxCrop:indxLast:iNBudgetCrops))
            END DO
        END IF
        
        IF (pPondedAg%lLWUseBudRawFile_Defined)  &
            CALL WriteAgLWUseFlowsToBudRawFile(pPondedAg%LWUseBudRawFile,       &
                                               AppGrid,                         &
                                               pPondedAg%iBudgetCrops,          &
                                               RLUArea,RPump,RDeli,             &
                                               pPondedAg%Crops%DemandRaw,       &
                                               pPondedAg%Crops%Demand,          &
                                               pPondedAg%Crops%ETAW,            &
                                               pPondedAg%Crops%ETP,             &
                                               pPondedAg%Crops%ETFromGW_Actual, &
                                               pPondedAg%Crops%ETOth            )
        
        IF (pPondedAg%lRootZoneBudRawFile_Defined) THEN
            CALL WriteAgRootZoneFlowsToBudRawFile(pPondedAg%RootZoneBudRawFile         , &
                                                  AppGrid                              , &
                                                  pPondedAg%iBudgetCrops               , &
                                                  pPondedAg%Crops%GenericLandUseGWType , &
                                                  RLUArea                              , &
                                                  RootZone%ElemPrecipData%Precip       , &
                                                  RRunoff,RPump,RDeli                  , &
                                                  pPondedAg%Crops%Reuse                , &
                                                  RReturn                              , &
                                                  RSrfcInForGW_Ag                      , &
                                                  RSrfcInForGW_UrbIn                   , &
                                                  RSrfcInForGW_UrbOut                  , &
                                                  RSrfcInForGW_NVRV                    , &
                                                  RGenMoistInflow                      , &
                                                  RInfilt                              , &
                                                  RPerc                                , &
                                                  RDrain                               , &
                                                  pPondedAg%RegionETPot                )
        END IF
    END ASSOCIATE
    
  END SUBROUTINE PrintPondedAgResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO LAND & WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToBudRawFile(RootZone,AppGrid,RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb)
    CLASS(RootZone_v412_Type)       :: RootZone
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RLUArea_Ag,RLUArea_Urb,RPump_Ag,RPump_Urb,RDeli_Ag,RDeli_Urb
    
    !Local variables
    REAL(8)                                  :: rDummyArray(f_iNLWUseBudColumns,(AppGrid%NSubregions+1))
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RDemandRaw_Ag,RDemand_Ag,RDemand_Urb, &
                                                RDemandShort_Ag,RDemandShort_Urb,     &
                                                RETAW,RETP,RETOth,RETGW
    
    !Compute budget terms
    IF (RootZone%Flags%lNonPondedAg_Defined .OR. RootZone%Flags%lPondedAg_Defined) THEN
        RDemandRaw_Ag   = RootZone%RegionalAgRawDemand(AppGrid)
        RDemand_Ag      = RootZone%RegionalDemand(AppGrid,f_iLandUse_Ag)
        RDemandShort_Ag = RDemand_Ag - RPump_Ag - RDeli_Ag
        RETAW           = RootZone%RegionalETAW(AppGrid)
        RETP            = RootZone%RegionalETP(AppGrid)
        RETOth          = RootZone%RegionalETOth(AppGrid)
        RETGW           = RootZone%RegionalETGW(AppGrid)
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
        RDemand_Urb      = RootZone%RegionalDemand(AppGrid,f_iLandUse_Urb)
        RDemandShort_Urb = RDemand_Urb - RPump_Urb - RDeli_Urb 
    ELSE
        RDemand_Urb      = 0.0
        RDEmandShort_Urb = 0.0
    END IF
    
    !Store in temporary array
    rDummyArray(1,:)  = RLUArea_Ag
    rDummyArray(2,:)  = RDemandRaw_Ag
    rDummyArray(3,:)  = RDemand_Ag
    rDummyArray(4,:)  = RPump_Ag
    rDummyArray(5,:)  = RDeli_Ag
    rDummyArray(6,:)  = RDemandShort_Ag
    rDummyArray(7,:)  = RETAW
    rDummyArray(8,:)  = RETP
    rDummyArray(9,:)  = RETGW
    rDummyArray(10,:) = RETOth
    rDummyArray(11,:) = RLUArea_Urb
    rDummyArray(12,:) = RDemand_Urb
    rDummyArray(13,:) = RPump_Urb
    rDummyArray(14,:) = RDeli_Urb
    rDummyArray(15,:) = RDemandShort_Urb

    !Print out values to binary file
    CALL RootZone%LWUseBudRawFile%WriteData(rDummyArray)
  
  END SUBROUTINE WriteLWUseFlowsToBudRawFile

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO ROOT ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToBudRawFile(RootZone,AppGrid,RPump_Ag,RDeli_Ag,RPump_Urb,RDeli_Urb,RLUArea_Ag,RLUArea_Urb)
    TYPE(RootZone_v412_Type)        :: RootZone
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RPump_Ag,RDeli_Ag,RPump_Urb,RDeli_Urb,RLUArea_Ag,RLUArea_Urb
    
    !Local variables
    REAL(8)                                  :: rDummyArray(f_iNRootZoneBudColumns,(AppGrid%NSubregions+1))
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RLUArea_NV,                                                                               &
                                                RRunoff_Ag,RRunoff_Urb,RRunoff_NV,                                                        &
                                                RPrecip_Ag,RPrecip_Urb,RPrecip_NV,                                                        &
                                                RReuse_Ag,RReuse_Urb,                                                                     &
                                                RReturn_Ag,RReturn_Urb,                                                                   &
                                                RDrain_Ag,                                                                                &
                                                RSoilMCh_Ag,RSoilMCh_Urb,RSoilMCh_NV,                                                     &
                                                RInfilt_Ag,RInfilt_Urb,RInfilt_NV,                                                        &
                                                RGenericMoist_Ag,RGenericMoist_Urb,RGenericMoist_NV,                                      &
                                                RETPot_Ag,RETPot_Urb,RETPot_NV,                                                           &
                                                RETa_Ag,RETa_Urb,RETa_NV,                                                                 &
                                                RPerc_Ag,RPerc_Urb,RPerc_NV,                                                              &
                                                RSrfcInForGW_Ag_Ag,RSrfcInForGW_Ag_UrbIn,RSrfcInForGW_Ag_UrbOut,RSrfcInForGW_Ag_NVRV,     &
                                                RSrfcInForGW_Urb_Ag,RSrfcInForGW_Urb_UrbIn,RSrfcInForGW_Urb_UrbOut,RSrfcInForGW_Urb_NVRV, &
                                                RSrfcInForGW_NV_Ag,RSrfcInForGW_NV_UrbIn,RSrfcInForGW_NV_UrbOut,RSrfcInForGW_NV_NVRV,     &
                                                RGWInflow_Ag,RGWInflow_Urb,RGWInflow_NV,                                                  &
                                                RRVETFromStrm_NV,                                                                         &  
                                                Error_Ag,Error_Urb,Error_NV
    
    !Initialize
    RETPot_Ag               = 0.0
    RPrecip_Ag              = 0.0
    RRunoff_Ag              = 0.0
    RReuse_Ag               = 0.0
    RReturn_Ag              = 0.0
    RSrfcInForGW_Ag_Ag      = 0.0
    RSrfcInForGW_Ag_UrbIn   = 0.0
    RSrfcInForGW_Ag_UrbOut  = 0.0
    RSrfcInForGW_Ag_NVRV    = 0.0
    RDrain_Ag               = 0.0
    RSoilMCh_Ag             = 0.0
    RInfilt_Ag              = 0.0
    RGWInflow_Ag            = 0.0    
    RETa_Ag                 = 0.0
    RPerc_Ag                = 0.0
    Error_Ag                = 0.0
    RETPot_Urb              = 0.0
    RPrecip_Urb             = 0.0
    RRunoff_Urb             = 0.0
    RReuse_Urb              = 0.0
    RReturn_Urb             = 0.0
    RSrfcInForGW_Urb_Ag     = 0.0
    RSrfcInForGW_Urb_UrbIn  = 0.0
    RSrfcInForGW_Urb_UrbOut = 0.0
    RSrfcInForGW_Urb_NVRV   = 0.0
    RSoilMCh_Urb            = 0.0
    RInfilt_Urb             = 0.0
    RGWInflow_Urb           = 0.0    
    RETa_Urb                = 0.0
    RPerc_Urb               = 0.0
    Error_Urb               = 0.0
    RLUArea_NV              = 0.0 
    RETPot_NV               = 0.0
    RPrecip_NV              = 0.0
    RRunoff_NV              = 0.0
    RSrfcInForGW_NV_Ag      = 0.0
    RSrfcInForGW_NV_UrbIn   = 0.0
    RSrfcInForGW_NV_UrbOut  = 0.0
    RSrfcInForGW_NV_NVRV    = 0.0
    RSoilMCh_NV             = 0.0
    RInfilt_NV              = 0.0
    RGWInflow_NV            = 0.0 
    RRVETFromStrm_NV        = 0.0
    RETa_NV                 = 0.0
    RPerc_NV                = 0.0
    Error_NV                = 0.0
    
    !Regional moisture storages
    RootZone%RSoilM = RootZone%RegionalMoistStorage(AppGrid)
    
    !Generic moisture
    IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN
        RGenericMoist_Ag  = RootZone%RegionalGenericMoistInflow(AppGrid,f_iLandUse_Ag)
        RGenericMoist_Urb = RootZone%RegionalGenericMoistInflow(AppGrid,f_iLandUse_Urb)
        RGenericMoist_NV  = RootZone%RegionalGenericMoistInflow(AppGrid,f_iLandUse_NVRV)
    ELSE
        RGenericMoist_Ag  = 0.0
        RGenericMoist_Urb = 0.0
        RGenericMoist_NV  = 0.0
    END IF

    !Compute subregional flows for ag lands
    IF (RootZone%Flags%lNonPondedAg_Defined .OR. RootZone%Flags%lPondedAg_Defined) THEN
        CALL RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,.FALSE.,f_iLandUse_Ag,0,RSrfcInForGW_Ag_Ag,RSrfcInForGW_Ag_UrbIn,RSrfcInForGW_Ag_UrbOut,RSrfcInForGW_Ag_NVRV,RInfilt_Ag,RPerc_Ag)
        CALL RegionalReturn(RootZone,AppGrid,f_iLandUse_Ag,RReturn_Ag)
        CALL RegionalRunoff(RootZone,AppGrid,f_iLandUse_Ag,RRunoff_Ag)
        CALL RegionalDrain(RootZone,AppGrid,RDrain_Ag)
        CALL RootZone%RegionalETPot(AppGrid,f_iLandUse_Ag,RETPot_Ag)
        RPrecip_Ag   = RootZone%RegionalPrecip(AppGrid,f_iLandUse_Ag)
        RReuse_Ag    = RootZone%RegionalReuse(AppGrid,f_iLandUse_Ag)
        RSoilMCh_Ag  = RootZone%RegionalSoilMChange(AppGrid,f_iLandUse_Ag) 
        RETa_Ag      = RootZone%RegionalETa(AppGrid,f_iLandUse_Ag) 
        RGWInflow_Ag = RootZone%RegionalGWInflow(AppGrid,f_iLandUse_Ag)       
        Error_Ag     = RootZone%RSoilM_P(:,1) + RSoilMCh_Ag + RInfilt_Ag + RGWInflow_Ag + RGenericMoist_Ag - RDrain_Ag - RETa_Ag - RPerc_Ag - RootZone%RSoilM(:,1)
    END IF
    
    !Compute subregional flows for urban lands
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,.FALSE.,f_iLandUse_Urb,0,RSrfcInForGW_Urb_Ag,RSrfcInForGW_Urb_UrbIn,RSrfcInForGW_Urb_UrbOut,RSrfcInForGW_Urb_NVRV,RInfilt_Urb,RPerc_Urb)
        CALL RegionalReturn(RootZone,AppGrid,f_iLandUse_Urb,RReturn_Urb)
        CALL RegionalRunoff(RootZone,AppGrid,f_iLandUse_Urb,RRunoff_Urb)
        CALL RootZone%RegionalETPot(AppGrid,f_iLandUse_Urb,RETPot_Urb)
        RPrecip_Urb   = RootZone%RegionalPrecip(AppGrid,f_iLandUse_Urb)
        RReuse_Urb    = RootZone%RegionalReuse(AppGrid,f_iLandUse_Urb)
        RSoilMCh_Urb  = RootZone%RegionalSoilMChange(AppGrid,f_iLandUse_Urb) 
        RETa_Urb      = RootZone%RegionalETa(AppGrid,f_iLandUse_Urb)
        RGWInflow_Urb = RootZone%RegionalGWInflow(AppGrid,f_iLandUse_Urb)       
        Error_Urb     = RootZone%RSoilM_P(:,2) + RSoilMCh_Urb + RInfilt_Urb + RGWInflow_Urb + RGenericMoist_Urb - RETa_Urb - RPerc_Urb - RootZone%RSoilM(:,2)
    END IF

    !Compute subregional flows for native&riparian veg
    IF (RootZone%Flags%lNVRV_Defined) THEN
        CALL RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,.FALSE.,f_iLandUse_NVRV,0,RSrfcInForGW_NV_Ag,RSrfcInForGW_NV_UrbIn,RSrfcInForGW_NV_UrbOut,RSrfcInForGW_NV_NVRV,RInfilt_NV,RPerc_NV)
        CALL RegionalRunoff(RootZone,AppGrid,f_iLandUse_NVRV,RRunoff_NV)
        CALL RootZone%RegionalETPot(AppGrid,f_iLandUse_NVRV,RETPot_NV)
        CALL RootZone%NVRVRootZone%GetRegionalRVETFromStrm(AppGrid,RRVETFromStrm_NV)
        RLUArea_NV   = RootZone%RegionalLUArea(AppGrid,f_iLandUse_NVRV)
        RPrecip_NV   = RootZone%RegionalPrecip(AppGrid,f_iLandUse_NVRV)       
        RSoilMCh_NV  = RootZone%RegionalSoilMChange(AppGrid,f_iLandUse_NVRV) 
        RETa_NV      = RootZone%RegionalETa(AppGrid,f_iLandUse_NVRV)
        RGWInflow_NV = RootZone%RegionalGWInflow(AppGrid,f_iLandUse_NVRV)
        Error_NV     = RootZone%RSoilM_P(:,3) + RSoilMCh_NV + RInfilt_NV + RGWInflow_NV + RGenericMoist_NV + RRVETFromStrm_NV - RETa_NV - RPerc_NV - RootZone%RSoilM(:,3)
    END IF
        
    !Store in temporary array
    rDummyArray(1,:)  = RLUArea_Ag                                                  !Agricultural area
    rDummyArray(2,:)  = RETPot_Ag                                                   !Potential ET on ag lands
    rDummyArray(3,:)  = RPrecip_Ag                                                  !Precipitation on ag lands
    rDummyArray(4,:)  = RRunoff_Ag                                                  !Runoff from ag lands
    rDummyArray(5,:)  = RDeli_Ag + RPump_Ag                                         !Prime applied water on ag lands prior to application of re-use water
    rDummyArray(6,:)  = RReuse_Ag                                                   !Reused water on ag lands 
    rDummyArray(7,:)  = RReturn_Ag                                                  !Return flow from ag lands
    rDummyArray(8,:)  = RSrfcInForGW_Ag_Ag                                          !Surface flow from ag lands for gw recharge on ag lands
    rDummyArray(9,:)  = RSrfcInForGW_Ag_UrbIn                                       !Surface flow from urban indoors for gw recharge on ag lands
    rDummyArray(10,:) = RSrfcInForGW_Ag_UrbOut                                      !Surface flow from urban outdoors for gw recharge on ag lands
    rDummyArray(11,:) = RSrfcInForGW_Ag_NVRV                                        !Surface flow from NVRV for gw recharge on ag lands
    rDummyArray(12,:) = RootZone%RSoilM_P(:,1)                                      !Storage at the beginning of the time interval
    rDummyArray(13,:) = RSoilMCh_Ag                                                 !Soil moisture change due to expansion/contraction of ag lands
    rDummyArray(14,:) = RInfilt_Ag                                                  !Infiltration on ag lands
    rDummyArray(15,:) = RGWInflow_Ag                                                !Groundwater inflow on ag lands
    rDummyArray(16,:) = RGenericMoist_Ag                                            !Generic moisture inflow to ag lands
    rDummyArray(17,:) = RDrain_Ag                                                   !Rice/refuge pond drainage on ag lands
    rDummyArray(18,:) = RETa_Ag                                                     !ET on ag lands
    rDummyArray(19,:) = RPerc_Ag                                                    !Percolation on ag lands
    rDummyArray(20,:) = RootZone%RSoilM(:,1)                                        !Storage at the end of the time interval
    rDummyArray(21,:) = Error_Ag                                                    !Mass balance error for ag lands
    rDummyArray(22,:) = RLUArea_Urb                                                 !Urban area
    rDummyArray(23,:) = RETPot_Urb                                                  !Potential ET on urban lands
    rDummyArray(24,:) = RPrecip_Urb                                                 !Precipitation on urban lands
    rDummyArray(25,:) = RRunoff_Urb                                                 !Runoff from urban lands
    rDummyArray(26,:) = RDeli_Urb + RPump_Urb                                       !Prime applied water on urban lands prior to re-used water
    rDummyArray(27,:) = RReuse_Urb                                                  !Reused water on urban indoors and outdoors
    rDummyArray(28,:) = RReturn_Urb                                                 !Return flow from urban lands
    rDummyArray(29,:) = RSrfcInForGW_Urb_Ag                                         !Surface flow from ag lands for gw recharge on urban lands
    rDummyArray(30,:) = RSrfcInForGW_Urb_UrbIn                                      !Surface flow from urban indoors for gw recharge on urban lands
    rDummyArray(31,:) = RSrfcInForGW_Urb_UrbOut                                     !Surface flow from urban outdoors for gw recharge on urban lands
    rDummyArray(32,:) = RSrfcInForGW_Urb_NVRV                                       !Surface flow from NVRV for gw recharge on urban lands
    rDummyArray(33,:) = RootZone%RSoilM_P(:,2)                                      !Storage at the beginning of the time interval
    rDummyArray(34,:) = RSoilMCh_Urb                                                !Soil moisture change due to expansion/contraction of urban lands
    rDummyArray(35,:) = RInfilt_Urb                                                 !Infiltration on urban lands
    rDummyArray(36,:) = RGWInflow_Urb                                               !Groundwater inflow on urban lands
    rDummyArray(37,:) = RGenericMoist_Urb                                           !Generic moisture inflow to urban lands
    rDummyArray(38,:) = RETa_Urb                                                    !ET on urban lands
    rDummyArray(39,:) = RPerc_Urb                                                   !Percolation on urban lands
    rDummyArray(40,:) = RootZone%RSoilM(:,2)                                        !Storage at the end of the time interval     
    rDummyArray(41,:) = Error_Urb                                                   !Mass balance error at urban lands
    rDummyArray(42,:) = RLUArea_NV                                                  !Natural area
    rDummyArray(43,:) = RETPot_NV                                                   !Potential ET on natural lands
    rDummyArray(44,:) = RPrecip_NV                                                  !Precipitation on natural lands
    rDummyArray(45,:) = RRunoff_NV                                                  !Total rainfall runoff on natural lands
    rDummyArray(46,:) = RSrfcInForGW_NV_Ag                                          !Surface flow from ag lands for gw recharge on natural lands
    rDummyArray(47,:) = RSrfcInForGW_NV_UrbIn                                       !Surface flow from urban indoorsfor gw recharge on natural lands
    rDummyArray(48,:) = RSrfcInForGW_NV_UrbOut                                      !Surface flow from urban outdoors for gw recharge on natural lands
    rDummyArray(49,:) = RSrfcInForGW_NV_NVRV                                        !Surface flow from NVRV for gw recharge on natural lands
    rDummyArray(50,:) = RootZone%RSoilM_P(:,3)                                      !Storage at the beginning of the time interval
    rDummyArray(51,:) = RSoilMCh_NV                                                 !Soil moisture change due to expansion/contraction of natural lands
    rDummyArray(52,:) = RInfilt_NV                                                  !Infiltration on natural lands
    rDummyArray(53,:) = RGWInflow_NV                                                !Groundwater inflow on natural lands
    rDummyArray(54,:) = RGenericMoist_NV                                            !Generic moisture inflow to natural lands
    rDummyArray(55,:) = RRVETFromStrm_NV                                            !Stream inflow to meet riparian ET
    rDummyArray(56,:) = RETa_NV                                                     !ET on natural lands
    rDummyArray(57,:) = RPerc_NV                                                    !Percolation on natural lands
    rDummyArray(58,:) = RootZone%RSoilM(:,3)                                        !Storage at the end of the time interval          
    rDummyArray(59,:) = Error_NV                                                    !Mass balance error at native and riparian lands

    !Print out values to binary file
    CALL RootZone%RootZoneBudRawFile%WriteData(rDummyArray)
    
  END SUBROUTINE WriteRootZoneFlowsToBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT LAND AND WATER USE BUDGET RAW DATA FOR SPECIFIED AG CROPS
  ! -------------------------------------------------------------
  SUBROUTINE WriteAgLWUseFlowsToBudRawFile(LWUseBudRawFile,AppGrid,iBudgetCrops,RLUArea,RPump,RDeli,rCropsDemandRaw,rCropsDemand,rCropsETAW,rCropsETP,rCropsETGW,rCropsETOth)
    TYPE(BudgetType)             :: LWUseBudRawFile
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: iBudgetCrops(:)
    REAL(8),INTENT(IN)           :: RLUArea(:),RPump(:),RDeli(:),rCropsDemandRaw(:,:),rCropsDemand(:,:),rCropsETAW(:,:),rCropsETP(:,:),rCropsETGW(:,:),rCropsETOth(:,:)
    
    !Local variables
    INTEGER                                                       :: indxCrop,indxLast,iNBudgetCrops
    REAL(8),DIMENSION(AppGrid%NElements)                          :: rElemValues
    REAL(8)                                                       :: rDummyArray(f_iNAgLWUseBudColumns,(AppGrid%NSubregions+1)*SIZE(iBudgetCrops))
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*SIZE(iBudgetCrops)) :: RDemandRaw,RDemand,RDemandShort,RETAW,RETP,RETOth,RETGW

    !Initialize
    iNBudgetCrops = SIZE(iBudgetCrops)
    indxLast      = iNBudgetCrops * AppGrid%NSubregions
    
    !Compute budget terms
    DO indxCrop=1,iNBudgetCrops
        rElemValues                                 = rCropsDemandRaw(iBudgetCrops(indxCrop),:)
        RDemandRaw(indxCrop:indxLast:iNBudgetCrops) = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RDemandRaw(indxLast+indxCrop)               = SUM(RDemandRaw(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                 = rCropsDemand(iBudgetCrops(indxCrop),:)
        RDemand(indxCrop:indxLast:iNBudgetCrops)    = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RDemand(indxLast+indxCrop)                  = SUM(RDemand(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                 = rCropsETAW(iBudgetCrops(indxCrop),:)
        RETAW(indxCrop:indxLast:iNBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RETAW(indxLast+indxCrop)                    = SUM(RETAW(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                 = rCropsETP(iBudgetCrops(indxCrop),:)
        RETP(indxCrop:indxLast:iNBudgetCrops)       = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RETP(indxLast+indxCrop)                     = SUM(RETP(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                 = rCropsETGW(iBudgetCrops(indxCrop),:)
        RETGW(indxCrop:indxLast:iNBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RETGW(indxLast+indxCrop)                    = SUM(RETGW(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                 = rCropsETOth(iBudgetCrops(indxCrop),:)
        RETOth(indxCrop:indxLast:iNBudgetCrops)     = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RETOth(indxLast+indxCrop)                   = SUM(RETOth(indxCrop:indxLast:iNBudgetCrops))
    END DO
    RDemandShort = RDemand - RPump - RDeli
    
    !Store in temporary array
    rDummyArray(1,:)  = RLUArea
    rDummyArray(2,:)  = RDemandRaw
    rDummyArray(3,:)  = RDemand
    rDummyArray(4,:)  = RPump
    rDummyArray(5,:)  = RDeli
    rDummyArray(6,:)  = RDemandShort
    rDummyArray(7,:)  = RETAW
    rDummyArray(8,:)  = RETP
    rDummyArray(9,:)  = RETGW
    rDummyArray(10,:) = RETOth

    !Print out values to binary file
    CALL LWUseBudRawFile%WriteData(rDummyArray)

  END SUBROUTINE WriteAgLWUseFlowsToBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT CROP-SPECIFIC ROOT ZONE BUDGET RAW DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteAgRootZoneFlowsToBudRawFile(RootZoneBudRawFile,AppGrid,iBudgetCrops,Crops,RLUArea,rElemPrecip,RRunoff,RPump,RDeli,rCropsReuse,RReturn,RSrfcInForGW_Ag,RSrfcInForGW_UrbIn,RSrfcInForGW_UrbOut,RSrfcInForGW_NVRV,RGenMoistInflow,RInfilt,RPerc,RDrain,RegionETPot)
    TYPE(BudgetType)                      :: RootZoneBudRawFile
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    INTEGER,INTENT(IN)                    :: iBudgetCrops(:)
    TYPE(GenericLandUseGWType),INTENT(IN) :: Crops
    REAL(8),INTENT(IN)                    :: RPump(:),RDeli(:),RGenMoistInflow(:),RRunoff(:),RReturn(:),RInfilt(:),RPerc(:),      &
                                             RLUArea(:),RDrain(:),rElemPrecip(:),RegionETPot(:,:),rCropsReuse(:,:),               &
                                             RSrfcInForGW_Ag(:),RSrfcInForGW_UrbIn(:),RSrfcInForGW_UrbOut(:),RSrfcInForGW_NVRV(:) 
    
    !Local variables
    INTEGER                                                       :: iNBudgetCrops,indxCrop,indxLast,iCrop
    REAL(8)                                                       :: rDummyArray(f_iNAgRootZoneBudColumns,(AppGrid%NSubregions+1)*SIZE(iBudgetCrops)), &
                                                                     rArea(SIZE(Crops%Area,DIM=2),SIZE(Crops%Area,DIM=1))
    REAL(8),DIMENSION(AppGrid%NElements)                          :: rElemValues
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*SIZE(iBudgetCrops)) :: RPrecip,RReuse,RSoilMCh,RETa,RSoilM,RSoilM_P,RGWInflow,RETPot,Error
    
    !Initialize
    iNBudgetCrops = SIZE(iBudgetCrops)
    indxLast      = iNBudgetCrops * AppGrid%NSubregions
    rArea         = TRANSPOSE(Crops%Area)
    
    !Compute subregional values
    DO indxCrop=1,iNBudgetCrops
        iCrop                                      = iBudgetCrops(indxCrop)
        !Moisture at the beginning of the time step must be corrected for chnages due to acreage changes
        rElemValues                                = (Crops%SoilM_Precip_P(iCrop,:) + Crops%SoilM_AW_P(iCrop,:) + Crops%SoilM_Oth_P(iCrop,:)) * rArea(:,iCrop) &
                                                    - Crops%SoilMCh(iCrop,:)                                                                                                                                               &
                                                    + Crops%PercCh(iCrop,:)
        RSoilM_P(indxCrop:indxLast:iNBudgetCrops)  = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RSoilM_P(indxLast+indxCrop)                = SUM(RSoilM_P(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                = (Crops%SoilM_Precip(iCrop,:) + Crops%SoilM_AW(iCrop,:) + Crops%SoilM_Oth(iCrop,:)) * rArea(:,iCrop)
        RSoilM(indxCrop:indxLast:iNBudgetCrops)    = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RSoilM(indxLast+indxCrop)                  = SUM(RSoilM(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                = rElemPrecip * rArea(:,iCrop)
        RPrecip(indxCrop:indxLast:iNBudgetCrops)   = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RPrecip(indxLast+indxCrop)                 = SUM(RPrecip(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                = rCropsReuse(iCrop,:)
        RReuse(indxCrop:indxLast:iNBudgetCrops)    = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RReuse(indxLast+indxCrop)                  = SUM(RReuse(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                = Crops%SoilMCh(iCrop,:)
        RSoilMCh(indxCrop:indxLast:iNBudgetCrops)  = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RSoilMCh(indxLast+indxCrop)                = SUM(RSoilMCh(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                = Crops%ETFromGW_Actual(iCrop,:) 
        RGWInflow(indxCrop:indxLast:iNBudgetCrops) = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RGWInflow(indxLast+indxCrop)               = SUM(RGWInflow(indxCrop:indxLast:iNBudgetCrops))
        rElemValues                                = Crops%ETa(iCrop,:)
        RETa(indxCrop:indxLast:iNBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RETa(indxLast+indxCrop)                    = SUM(RETa(indxCrop:indxLast:iNBudgetCrops))
        RETPot(indxCrop:indxLast:iNBudgetCrops)    = RegionETPot(iCrop,:)
        RETPot(indxLast+indxCrop)                  = SUM(RETPot(indxCrop:indxLast:iNBudgetCrops))
    END DO
    Error = RSoilM_P + RSoilMCh + RInfilt + RGWInflow + RGenMoistInflow - RDrain - RETa - RPerc - RSoilM  
    
    !Store in temporary array
    rDummyArray(1,:)  = RLUArea                                         !Agricultural area
    rDummyArray(2,:)  = RETPot                                          !Potential ET
    rDummyArray(3,:)  = RPrecip                                         !Precipitation on ag lands
    rDummyArray(4,:)  = RRunoff                                         !Runoff from ag lands
    rDummyArray(5,:)  = RDeli + RPump                                   !Prime applied water on ag lands prior to application of re-use water
    rDummyArray(6,:)  = RReuse                                          !Applied recycled water on ag lands 
    rDummyArray(7,:)  = RReturn                                         !Return flow from ag lands
    rDummyArray(8,:)  = RSrfcInForGW_Ag                                 !Surface inflow from other ag lands for gw recharge 
    rDummyArray(9,:)  = RSrfcInForGW_UrbIn                              !Surface inflow from urban indoors for gw recharge 
    rDummyArray(10,:) = RSrfcInForGW_UrbOut                             !Surface inflow from urban outdoors for gw recharge 
    rDummyArray(11,:) = RSrfcInForGW_NVRV                               !Surface inflow from native&riparian vegitation for gw recharge 
    rDummyArray(12,:) = RSoilM_P                                        !Storage at the beginning of the time interval
    rDummyArray(13,:) = RSoilMCh                                        !Soil moisture chnage due to expansion/contraction of ag lands
    rDummyArray(14,:) = RInfilt                                         !Infiltration on ag lands
    rDummyArray(15,:) = RGWInflow                                       !GW inflow to non-ponded ag lands
    rDummyArray(16,:) = RGenMoistInflow                                 !Generic moisture inflow to non-ponded ag lands
    rDummyArray(17,:) = RDrain                                          !Rice/refuge pond drainage on ag lands
    rDummyArray(18,:) = RETa                                            !ET on ag lands
    rDummyArray(19,:) = RPerc                                           !Percolation on ag lands
    rDummyArray(20,:) = RSoilM                                          !Storage at the end of the time interval
    rDummyArray(21,:) = Error                                           !Mass balance error for ag lands

    !Print out values to binary file
    CALL RootZoneBudRawFile%WriteData(rDummyArray)

  END SUBROUTINE WriteAgRootZoneFlowsToBudRawFile

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO LAND & WATER USE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToZoneBudRawFile(RootZone,AppGrid,rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli)
    TYPE(RootZone_v412_Type)                          :: RootZone
    TYPE(AppGridType),INTENT(IN)                      :: AppGrid
    REAL(8),DIMENSION(AppGrid%NElements,1),INTENT(IN) :: rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli
    
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
            rETOth_NP(:,1) = SUM(RootZone%NonPondedAgRootZone%Crops%ETOth , DIM=1)                               
        rAgShort_NP(:,1)  = rAgSupReq_NP(:,1) - rAgPump_NP(:,1) - rAgDeli_NP(:,1)                               !Ag supply shortage
    END IF
    
    !Rice and refuge
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !Rice
        rCUAW_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%DemandRaw(1:3,:) , DIM=1)                    !Potential CUAW
        rAgSupReq_Rice(:,1) = SUM(RootZone%PondedAgRootZone%Crops%Demand(1:3,:) , DIM=1)                       !Ag supply requirement
        rETAW_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%ETAW(1:3,:) , DIM=1)                         !ETAW
        rETP_Rice(:,1)      = SUM(RootZone%PondedAgRootZone%Crops%ETP(1:3,:) , DIM=1)                          !Ag effective precipitation
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                    !Ag ET met from other sources
            rETOth_Rice(:,1) = SUM(RootZone%PondedAgRootZone%Crops%ETOth(1:3,:) , DIM=1)                        
        rAgShort_Rice(:,1)  = rAgSupReq_Rice(:,1) - rAgPump_Rice(:,1) - rAgDeli_Rice(:,1)                      !Ag supply shortage

        !Refuge
        rCUAW_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%DemandRaw(4:5,:) , DIM=1)                  !Potential CUAW
        rAgSupReq_Refuge(:,1) = SUM(RootZone%PondedAgRootZone%Crops%Demand(4:5,:) , DIM=1)                     !Ag supply requirement
        rETAW_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%ETAW(4:5,:) , DIM=1)                       !ETAW
        rETP_Refuge(:,1)      = SUM(RootZone%PondedAgRootZone%Crops%ETP(4:5,:) , DIM=1)                        !Ag effective precipitation
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                    !Ag ET met from other sources
            rETOth_Refuge(:,1) = SUM(RootZone%PondedAgRootZone%Crops%ETOth(4:5,:) , DIM=1)                      
        rAgShort_Refuge(:,1)  = rAgSupReq_Refuge(:,1) - rAgPump_Refuge(:,1) - rAgDeli_Refuge(:,1)              !Ag supply shortage
    END IF
    
    !Urban data
    IF (RootZone%Flags%lUrban_Defined) THEN
        rUrbSupReq(:,1) = RootZone%UrbanRootZone%UrbData%Demand(:,1)                                           !Urban supply requirement
        rUrbShort(:,1)  = rUrbSupReq(:,1) - rUrbPump(:,1) - rUrbDeli(:,1)                                      !Urban supply shortage
    END IF
    
    !Print non-ponded ag data
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN  
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,1,iLayer,rAgArea_NP)                  !Non-ponded ag area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,2,iLayer,rCUAW_NP)                    !Non-ponded potential CUAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,3,iLayer,rAgSupReq_NP)                !Non-ponded ag supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,4,iLayer,rAgPump_NP)                  !Non-ponded ag pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,5,iLayer,rAgDeli_NP)                  !Non-ponded ag deliveries
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,6,iLayer,rAgShort_NP)                 !Non-ponded ag shortage
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,7,iLayer,rETAW_NP)                    !Non-ponded ag ETAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,8,iLayer,rETP_NP)                     !Non-ponded ag effective precipitation
        IF (RootZone%Flags%lComputeETFromGW)  &                                                                 !Non-ponded ag ET from groundwater
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,9,iLayer,rAgETGW_NP)             
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                     !Non-ponded ag ET met from other sources
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,10,iLayer,rETOth_NP)             
    END IF
    
    !Print rice and refuge data
    IF (RootZone%Flags%lPondedAg_Defined) THEN 
        !Rice
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,11,iLayer,rAgArea_Rice)               !Rice area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,12,iLayer,rCUAW_Rice)                 !Rice potential CUAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,13,iLayer,rAgSupReq_Rice)             !Rice supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,14,iLayer,rAgPump_Rice)               !Rice pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,15,iLayer,rAgDeli_Rice)               !Rice deliveries
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,16,iLayer,rAgShort_Rice)              !Rice shortage
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,17,iLayer,rETAW_Rice)                 !Rice ETAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,18,iLayer,rETP_Rice)                  !Rice effective precipitation
        IF (RootZone%Flags%lComputeETFromGW)  &                                                                 !Rice ET from groundwater
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,19,iLayer,rAgETGW_Rice)           
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                     !Rice ET met from other sources
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,20,iLayer,rETOth_Rice)             

        !Refuge
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,21,iLayer,rAgArea_Refuge)             !Refuge area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,22,iLayer,rCUAW_Refuge)               !Refuge potential CUAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,23,iLayer,rAgSupReq_Refuge)           !Refuge supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,24,iLayer,rAgPump_Refuge)             !Refuge pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,25,iLayer,rAgDeli_Refuge)             !Refuge deliveries
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,26,iLayer,rAgShort_Refuge)            !Refuge shortage
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,27,iLayer,rETAW_Refuge)               !Refuge ETAW
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,28,iLayer,rETP_Refuge)                !Refuge effective precipitation
        IF (RootZone%Flags%lComputeETFromGW)  &                                                                 !Refuge ET from groundwater
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,29,iLayer,rAgETGW_Refuge)   
        IF (RootZone%Flags%lGenericMoistureFile_Defined)  &                                                     !Refuge ET met from other sources
            CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,30,iLayer,rETOth_Refuge)             
    END IF
    
    !Print urban data
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,31,iLayer,rUrbArea)                   !Urban area
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,32,iLayer,rUrbSupReq)                 !Urban supply requirement
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,33,iLayer,rUrbPump)                   !Urban pumping
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,34,iLayer,rUrbDeli)                   !Urban deliveries
        CALL RootZone%LWUZoneBudRawFile%WriteData(NLayers,f_iElemDataType,35,iLayer,rUrbShort)                  !Urban shortage
    END IF
        
  END SUBROUTINE WriteLWUseFlowsToZoneBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT FLOWS TO ROOT ZONE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToZoneBudRawFile(RootZone,AppGrid,ETData,rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli)
    TYPE(RootZone_v412_Type)                          :: RootZone
    TYPE(AppGridType),INTENT(IN)                      :: AppGrid
    TYPE(ETType),INTENT(IN)                           :: ETData
    REAL(8),DIMENSION(AppGrid%NElements,1),INTENT(IN) :: rAgArea_NP,rAgPump_NP,rAgDeli_NP,rAgETGW_NP,rAgArea_Rice,rAgPump_Rice,rAgDeli_Rice,rAgETGW_Rice,rAgArea_Refuge,rAgPump_Refuge,rAgDeli_Refuge,rAgETGW_Refuge,rUrbArea,rUrbPump,rUrbDeli
    
    !Local variables
    INTEGER,PARAMETER                      :: NLayers = 1 , &
                                              iLayer  = 1
    REAL(8),DIMENSION(AppGrid%NElements,1) :: rAgPotET_NP,rAgPrecip_NP,rAgRunoff_NP,rAgAW_NP,rAgReuse_NP,rAgReturn_NP,rAgBeginStor_NP,rAgSoilMCh_NP,rAgInfilt_NP,rAgOthIn_NP,rAgETa_NP,rAgPerc_NP,rAgEndStor_NP,rAgError_NP,                                                                          &
                                              rAgSrfcInForGW_Ag_NP,rAgSrfcInForGW_UrbIn_NP,rAgSrfcInForGW_UrbOut_NP,rAgSrfcInForGW_NVRV_NP,                                                                                                                                                           &    
                                              rAgPotET_Rice,rAgPrecip_Rice,rAgRunoff_Rice,rAgAW_Rice,rAgReuse_Rice,rAgReturn_Rice,rAgBeginStor_Rice,rAgSoilMCh_Rice,rAgInfilt_Rice,rAgOthIn_Rice,rAgDrain_Rice,rAgETa_Rice,rAgPerc_Rice,rAgEndStor_Rice,rAgError_Rice,                                &
                                              rAgSrfcInForGW_Ag_Rice,rAgSrfcInForGW_UrbIn_Rice,rAgSrfcInForGW_UrbOut_Rice,rAgSrfcInForGW_NVRV_Rice,                                                                                                                                                   &    
                                              rAgPotET_Refuge,rAgPrecip_Refuge,rAgRunoff_Refuge,rAgAW_Refuge,rAgReuse_Refuge,rAgReturn_Refuge,rAgBeginStor_Refuge,rAgSoilMCh_Refuge,rAgInfilt_Refuge,rAgOthIn_Refuge,rAgDrain_Refuge,rAgETa_Refuge,rAgPerc_Refuge,rAgEndStor_Refuge,rAgError_Refuge,  &
                                              rAgSrfcInForGW_Ag_Refuge,rAgSrfcInForGW_UrbIn_Refuge,rAgSrfcInForGW_UrbOut_Refuge,rAgSrfcInForGW_NVRV_Refuge,                                                                                                                                           &    
                                              rUrbPotET,rUrbPrecip,rUrbRunoff,rUrbAW,rUrbReuse,rUrbReturn,rUrbBeginStor,rUrbSoilMCh,rUrbInfilt,rUrbETGW,rUrbOthIn,rUrbETa,rUrbPerc,rUrbEndStor,rUrbError,                                                                                             &
                                              rUrbSrfcInForGW_Ag,rUrbSrfcInForGW_UrbIn,rUrbSrfcInForGW_UrbOut,rUrbSrfcInForGW_NVRV,                                                                                                                                                                   &    
                                              rNVRVArea,rNVRVPotET,rNVRVPrecip,rNVRVRunoff,rNVRVBeginStor,rNVRVSoilMCh,rNVRVInfilt,rNVRVETGW,rNVRVOthIn,rNVRVStrmInflow,rNVRVETa,rNVRVPerc,rNVRVEndStor,rNVRVError,                                                                                   &
                                              rNVRVSrfcInForGW_Ag,rNVRVSrfcInForGW_UrbIn,rNVRVSrfcInForGW_UrbOut,rNVRVSrfcInForGW_NVRV                                                                                                                                                       
    REAL(8),DIMENSION(AppGrid%NElements)   :: rSrfcInForGW_Ag_Work,rSrfcInForGW_UrbIn_Work,rSrfcInForGW_UrbOut_Work,rSrfcInForGW_NVRV_Work,rInfilt_Work,rPerc_Work
    INTEGER                                :: indxElem,indxCrop
    
    !Non-ponded ag
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        rAgSrfcInForGW_Ag_NP     = 0.0
        rAgSrfcInForGW_UrbIn_NP  = 0.0
        rAgSrfcInForGW_UrbOut_NP = 0.0
        rAgSrfcInForGW_NVRV_NP   = 0.0
        rAgInfilt_NP             = 0.0
        rAgPerc_NP               = 0.0
        DO indxCrop=1,RootZone%NonPondedAgRootZone%NCrops
            CALL ElementalNonPondedCropFlows_ForACrop(RootZone,AppGrid,indxCrop,rSrfcInForGW_Ag_Work,rSrfcInForGW_UrbIn_Work,rSrfcInForGW_UrbOut_Work,rSrfcInForGW_NVRV_Work,rInfilt_Work,rPerc_Work)
            rAgSrfcInForGW_Ag_NP(:,1)     = rAgSrfcInForGW_Ag_NP(:,1) + rSrfcInForGW_Ag_Work    
            rAgSrfcInForGW_UrbIn_NP(:,1)  = rAgSrfcInForGW_UrbIn_NP(:,1) + rSrfcInForGW_UrbIn_Work
            rAgSrfcInForGW_UrbOut_NP(:,1) = rAgSrfcInForGW_UrbOut_NP(:,1) + rSrfcInForGW_UrbOut_Work
            rAgSrfcInForGW_NVRV_NP(:,1)   = rAgSrfcInForGW_NVRV_NP(:,1) + rSrfcInForGW_NVRV_Work   
            rAgInfilt_NP(:,1)             = rAgInfilt_NP(:,1) + rInfilt_Work             
            rAgPerc_NP(:,1)               = rAgPerc_NP(:,1) + rPerc_Work              
        END DO
        DO indxElem=1,AppGrid%NElements
            rAgPotET_NP(indxElem,1) = SUM(ETData%GetValues(RootZone%NonPondedAgRootZone%Crops%iColETc(:,indxElem)) * RootZone%NonPondedAgRootZone%Crops%Area(:,indxElem))       !Non-ponded ag potential ET
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
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                                                   !Non-ponded ag other inflow
            DO indxElem=1,AppGrid%NElements
                rAgOthIn_NP(indxElem,1) = 0.0
                DO indxCrop=1,RootZone%NonPondedAgRootZone%NCrops
                     rAgOthIn_NP(indxElem,1) = rAgOthIn_NP(indxElem,1) + (RootZone%GenericMoistureData%rGenericMoisture(1,indxElem) * RootZone%NonPondedAgRootZone%RootDepth(indxCrop) - RootZone%NonPondedAgRootZone%Crops%GMExcess(indxCrop,indxElem)) * RootZone%NonPondedAgRootZone%Crops%Area(indxCrop,indxElem)
                END DO
            END DO
        END IF
        rAgETa_NP(:,1)     = SUM(RootZone%NonPondedAgRootZone%Crops%ETa , DIM=1)                                                                                                !Non-ponded ag actual ET
        rAgEndStor_NP(:,1) = SUM((RootZone%NonPondedAgRootZone%Crops%SoilM_Precip  &                                                                                            !Non-ponded ag ending storage
                                 +RootZone%NonPondedAgRootZone%Crops%SoilM_AW      &                                                                      
                                 +RootZone%NonPondedAgRootZone%Crops%SoilM_Oth     ) * RootZone%NonPondedAgRootZone%Crops%Area , DIM=1)
        rAgError_NP        = rAgBeginStor_NP + rAgSoilMCh_NP + rAgInfilt_NP - rAgETa_NP - rAgPerc_NP - rAgEndStor_NP                                                              !Non-ponded ag error                                                              
        IF (RootZone%Flags%lComputeETFromGW)             rAgError_NP = rAgError_NP + rAgETGW_NP
        IF (RootZone%Flags%lGenericMoistureFile_Defined) rAgError_NP = rAgError_NP + rAgOthIn_NP
    END IF
    
    !Ponded ag
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !Rice
        rAgSrfcInForGW_Ag_Rice     = 0.0
        rAgSrfcInForGW_UrbIn_Rice  = 0.0
        rAgSrfcInForGW_UrbOut_Rice = 0.0
        rAgSrfcInForGW_NVRV_Rice   = 0.0
        rAgInfilt_Rice             = 0.0
        rAgPerc_Rice               = 0.0
        DO indxCrop=1,3
            CALL ElementalPondedCropFlows_ForACrop(RootZone,AppGrid,indxCrop,rSrfcInForGW_Ag_Work,rSrfcInForGW_UrbIn_Work,rSrfcInForGW_UrbOut_Work,rSrfcInForGW_NVRV_Work,rInfilt_Work,rPerc_Work)
            rAgSrfcInForGW_Ag_Rice(:,1)     = rAgSrfcInForGW_Ag_Rice(:,1) + rSrfcInForGW_Ag_Work    
            rAgSrfcInForGW_UrbIn_Rice(:,1)  = rAgSrfcInForGW_UrbIn_Rice(:,1) + rSrfcInForGW_UrbIn_Work
            rAgSrfcInForGW_UrbOut_Rice(:,1) = rAgSrfcInForGW_UrbOut_Rice(:,1) + rSrfcInForGW_UrbOut_Work
            rAgSrfcInForGW_NVRV_Rice(:,1)   = rAgSrfcInForGW_NVRV_Rice(:,1) + rSrfcInForGW_NVRV_Work   
            rAgInfilt_Rice(:,1)             = rAgInfilt_Rice(:,1) + rInfilt_Work             
            rAgPerc_Rice(:,1)               = rAgPerc_Rice(:,1) + rPerc_Work              
        END DO
        DO indxElem=1,AppGrid%NElements
            rAgPotET_Rice(indxElem,1) = SUM(ETData%GetValues(RootZone%PondedAgRootZone%Crops%iColETc(1:3,indxElem)) * RootZone%PondedAgRootZone%Crops%Area(1:3,indxElem))       !Rice potential ET
        END DO
        rAgPrecip_Rice(:,1)    = RootZone%ElemPrecipData%Precip * rAgArea_Rice(:,1)                                                                                                                         !Rice precip 
        rAgRunoff_Rice(:,1)    = SUM(RootZone%PondedAgRootZone%Crops%Runoff(1:3,:) , DIM=1)                                                                                                                 !Rice runoff
        rAgAW_Rice             = rAgDeli_Rice + rAgPump_Rice                                                                                                                                                !Rice prime applied water
        rAgReuse_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%Reuse(1:3,:) , DIM=1)                                                                                                                  !Rice reuse
        rAgReturn_Rice(:,1)    = SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(1:3,:) , DIM=1)                                                                                                             !Rice return
        rAgBeginStor_Rice(:,1) = SUM((RootZone%PondedAgRootZone%Crops%SoilM_Precip_P_BeforeUpdate(1:3,:)  &                                                                                                 !Rice beginning storage
                                     +RootZone%PondedAgRootZone%Crops%SoilM_AW_P_BeforeUpdate(1:3,:)      &                                                                                          
                                     +RootZone%PondedAgRootZone%Crops%SoilM_Oth_P_BeforeUpdate(1:3,:)     ) * RootZone%PondedAgRootZone%Crops%Area_P(1:3,:) , DIM=1)                                 
        rAgSoilMCh_Rice(:,1)   = SUM(RootZone%PondedAgRootZone%Crops%SoilMCh(1:3,:) , DIM=1)                                                                                                                !Rice change in soil moisture due to land expansion
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                                                                               !Rice other inflow
            DO indxElem=1,AppGrid%NElements
                rAgOthIn_Rice(indxElem,1) = 0.0
                DO indxCrop=1,3
                     rAgOthIn_Rice(indxElem,1) = rAgOthIn_Rice(indxElem,1) + (RootZone%GenericMoistureData%rGenericMoisture(1,indxElem) * RootZone%PondedAgRootZone%RootDepth(indxCrop) - RootZone%PondedAgRootZone%Crops%GMExcess(indxCrop,indxElem)) * RootZone%PondedAgRootZone%Crops%Area(indxCrop,indxElem)
                END DO
            END DO
        END IF
        rAgDrain_Rice(:,1)   = SUM(RootZone%PondedAgRootZone%Crops%Drain(1:3,:) , DIM=1)                                                                                                                    !Rice pond drain
        rAgETa_Rice(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%ETa(1:3,:) , DIM=1)                                                                                                                      !Rice actual ET
        rAgEndStor_Rice(:,1) = SUM((RootZone%PondedAgRootZone%Crops%SoilM_Precip(1:3,:)  &                                                                                                                  !Rice ending storage
                                   +RootZone%PondedAgRootZone%Crops%SoilM_AW(1:3,:)      &                                                                      
                                   +RootZone%PondedAgRootZone%Crops%SoilM_Oth(1:3,:)     ) * RootZone%PondedAgRootZone%Crops%Area(1:3,:) , DIM=1)
        rAgError_Rice        = rAgBeginStor_Rice + rAgSoilMCh_Rice + rAgInfilt_Rice - rAgDrain_Rice - rAgETa_Rice - rAgPerc_Rice - rAgEndStor_Rice                                                            !Rice error                                                              
        IF (RootZone%Flags%lComputeETFromGW)             rAgError_Rice = rAgError_Rice + rAgETGW_Rice
        IF (RootZone%Flags%lGenericMoistureFile_Defined) rAgError_Rice = rAgError_Rice + rAgOthIn_Rice

        !Refuge
        rAgSrfcInForGW_Ag_Refuge     = 0.0
        rAgSrfcInForGW_UrbIn_Refuge  = 0.0
        rAgSrfcInForGW_UrbOut_Refuge = 0.0
        rAgSrfcInForGW_NVRV_Refuge   = 0.0
        rAgInfilt_Refuge             = 0.0
        rAgPerc_Refuge               = 0.0
        DO indxCrop=4,5
            CALL ElementalPondedCropFlows_ForACrop(RootZone,AppGrid,indxCrop,rSrfcInForGW_Ag_Work,rSrfcInForGW_UrbIn_Work,rSrfcInForGW_UrbOut_Work,rSrfcInForGW_NVRV_Work,rInfilt_Work,rPerc_Work)
            rAgSrfcInForGW_Ag_Refuge(:,1)     = rAgSrfcInForGW_Ag_Refuge(:,1) + rSrfcInForGW_Ag_Work    
            rAgSrfcInForGW_UrbIn_Refuge(:,1)  = rAgSrfcInForGW_UrbIn_Refuge(:,1) + rSrfcInForGW_UrbIn_Work
            rAgSrfcInForGW_UrbOut_Refuge(:,1) = rAgSrfcInForGW_UrbOut_Refuge(:,1) + rSrfcInForGW_UrbOut_Work
            rAgSrfcInForGW_NVRV_Refuge(:,1)   = rAgSrfcInForGW_NVRV_Refuge(:,1) + rSrfcInForGW_NVRV_Work   
            rAgInfilt_Refuge(:,1)             = rAgInfilt_Refuge(:,1) + rInfilt_Work             
            rAgPerc_Refuge(:,1)               = rAgPerc_Refuge(:,1) + rPerc_Work              
        END DO
        DO indxElem=1,AppGrid%NElements
            rAgPotET_Refuge(indxElem,1) = SUM(ETData%GetValues(RootZone%PondedAgRootZone%Crops%iColETc(4:5,indxElem)) * RootZone%PondedAgRootZone%Crops%Area(4:5,indxElem))       !Refuge potential ET
        END DO
        rAgPrecip_Refuge(:,1)    = RootZone%ElemPrecipData%Precip * rAgArea_Refuge(:,1)                                                                                                                         !Refuge precip 
        rAgRunoff_Refuge(:,1)    = SUM(RootZone%PondedAgRootZone%Crops%Runoff(4:5,:) , DIM=1)                                                                                                                   !Refuge runoff
        rAgAW_Refuge             = rAgDeli_Refuge + rAgPump_Refuge                                                                                                                                              !Refuge prime applied water
        rAgReuse_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%Reuse(4:5,:) , DIM=1)                                                                                                                    !Refuge reuse
        rAgReturn_Refuge(:,1)    = SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(4:5,:) , DIM=1)                                                                                                               !Refuge return
        rAgBeginStor_Refuge(:,1) = SUM((RootZone%PondedAgRootZone%Crops%SoilM_Precip_P_BeforeUpdate(4:5,:)  &                                                                                                   !Refuge beginning storage
                                     +RootZone%PondedAgRootZone%Crops%SoilM_AW_P_BeforeUpdate(4:5,:)        &                                                                                          
                                     +RootZone%PondedAgRootZone%Crops%SoilM_Oth_P_BeforeUpdate(4:5,:)       ) * RootZone%PondedAgRootZone%Crops%Area_P(4:5,:) , DIM=1)                                 
        rAgSoilMCh_Refuge(:,1)   = SUM(RootZone%PondedAgRootZone%Crops%SoilMCh(4:5,:) , DIM=1)                                                                                                                  !Refuge change in soil moisture due to land expansion
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                                                                                   !Refuge other inflow
            DO indxElem=1,AppGrid%NElements
                rAgOthIn_Refuge(indxElem,1) = 0.0
                DO indxCrop=4,5
                     rAgOthIn_Refuge(indxElem,1) = rAgOthIn_Refuge(indxElem,1) + (RootZone%GenericMoistureData%rGenericMoisture(1,indxElem) * RootZone%PondedAgRootZone%RootDepth(indxCrop) - RootZone%PondedAgRootZone%Crops%GMExcess(indxCrop,indxElem)) * RootZone%PondedAgRootZone%Crops%Area(indxCrop,indxElem)
                END DO
            END DO
        END IF
        rAgDrain_Refuge(:,1)   = SUM(RootZone%PondedAgRootZone%Crops%Drain(4:5,:) , DIM=1)                                                                                                                      !Refuge pond drain
        rAgETa_Refuge(:,1)     = SUM(RootZone%PondedAgRootZone%Crops%ETa (4:5,:), DIM=1)                                                                                                                        !Refuge actual ET
        rAgEndStor_Refuge(:,1) = SUM((RootZone%PondedAgRootZone%Crops%SoilM_Precip(4:5,:)  &                                                                                                                    !Refuge ending storage
                                   +RootZone%PondedAgRootZone%Crops%SoilM_AW(4:5,:)        &                                                                      
                                   +RootZone%PondedAgRootZone%Crops%SoilM_Oth(4:5,:)       ) * RootZone%PondedAgRootZone%Crops%Area(4:5,:) , DIM=1)
        rAgError_Refuge        = rAgBeginStor_Refuge + rAgSoilMCh_Refuge + rAgInfilt_Refuge - rAgDrain_Refuge - rAgETa_Refuge - rAgPerc_Refuge - rAgEndStor_Refuge                               !Refuge error                                                              
        IF (RootZone%Flags%lComputeETFromGW)             rAgError_Refuge = rAgError_Refuge + rAgETGW_Refuge
        IF (RootZone%Flags%lGenericMoistureFile_Defined) rAgError_Refuge = rAgError_Refuge + rAgOthIn_Refuge    
    END IF

    !Urban data
    IF (RootZone%Flags%lUrban_Defined) THEN
        CALL ElementalUrbanFlows(RootZone,AppGrid,rUrbSrfcInForGW_Ag(:,1),rUrbSrfcInForGW_UrbIn(:,1),rUrbSrfcInForGW_UrbOut(:,1),rUrbSrfcInForGW_NVRV(:,1),rUrbInfilt(:,1),rUrbPerc(:,1))
        rUrbPotET(:,1)     = ETData%GetValues(RootZone%UrbanRootZone%UrbData%iColETc(:,1)) * rUrbArea(:,1)                                          !Urban potential ET
        rUrbPrecip(:,1)    = RootZone%ElemPrecipData%Precip * rUrbArea(:,1)                                                                         !Urban precip
        rUrbRunoff(:,1)    = RootZone%UrbanRootZone%UrbData%Runoff(:,1)                                                                             !Urban runoff
        rUrbAW             = rUrbDeli + rUrbPump                                                                                                    !Urban prime appliaed water
        rUrbReuse(:,1)     = RootZone%UrbanRootZone%UrbData%Reuse(:,1)                                                                              !Urban reuse
        rUrbReturn(:,1)    = RootZone%UrbanRootZone%UrbData%ReturnFlow(:,1) + RootZone%rAW_UrbanIndoors                                                                        !Urban return
        rUrbBeginStor(:,1) = (RootZone%UrbanRootZone%UrbData%SoilM_Precip_P_BeforeUpdate(:,1)  &                                                    !Urban beginning storage
                            + RootZone%UrbanRootZone%UrbData%SoilM_AW_P_BeforeUpdate(:,1)      &
                            + RootZone%UrbanRootZone%UrbData%SoilM_Oth_P_BeforeUpdate(:,1)     ) * RootZone%UrbanRootZone%UrbData%Area_P(:,1) * RootZone%UrbanRootZone%UrbData%PerviousFrac(:,1)                                  
        rUrbSoilMCh(:,1)   = RootZone%UrbanRootZone%UrbData%SoilMCh(:,1)                                                                            !Urban change in soil moisture due to land expansion
        rUrbETa(:,1)       = RootZone%UrbanRootZone%UrbData%ETa(:,1)                                                                                !Urban actual ET
        rUrbEndStor(:,1)   = (RootZone%UrbanRootZone%UrbData%SoilM_Precip(:,1)  &                                                                   !Urban ending storage
                            + RootZone%UrbanRootZone%UrbData%SoilM_AW(:,1)      &
                            + RootZone%UrbanRootZone%UrbData%SoilM_Oth(:,1)     ) * rUrbArea(:,1) * RootZone%UrbanRootZone%UrbData%PerviousFrac(:,1)                                    
        rUrbError          = rUrbBeginStor + rUrbSoilMCh + rUrbInfilt - rUrbETa - rUrbPerc - rUrbEndStor                                              !Urban error                                                             
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN
            rUrbOthIn(:,1) = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * RootZone%UrbanRootZone%RootDepth - RootZone%UrbanRootZone%UrbData%GMExcess(:,1)) * rUrbArea(:,1) * RootZone%UrbanRootZone%UrbData%PerviousFrac(:,1)   !Urban other inflow
            rUrbError      = rUrbError + rUrbOthIn                                                                                                           
        END IF
        IF (RootZone%Flags%lComputeETFromGW) THEN                                                                                              !Urban groundwater inflow for ET
            rUrbETGW(:,1) = RootZone%UrbanRootZone%UrbData%ETFromGW_Actual(1,:) 
            rUrbError     = rUrbError + rUrbETGW
        END IF
    END IF

    !Native and riparian veg. data
    IF (RootZone%Flags%lNVRV_Defined) THEN
        CALL ElementalNVRVFlows(RootZone,AppGrid,rNVRVSrfcInForGW_Ag(:,1),rNVRVSrfcInForGW_UrbIn(:,1),rNVRVSrfcInForGW_UrbOut(:,1),rNVRVSrfcInForGW_NVRV(:,1),rNVRVInfilt(:,1),rNVRVPerc(:,1))
        rNVRVArea(:,1)      = RootZone%NVRVRootZone%NativeVeg%Area(:,1) + RootZone%NVRVRootZone%RiparianVeg%Area(:,1)                             !Native and riparian area
        rNVRVPotET(:,1)     = ETData%GetValues(RootZone%NVRVRootZone%NativeVeg%iColETc(:,1)) * RootZone%NVRVRootZone%NativeVeg%Area(:,1)     &    !Native and riparian potential ET
                            + ETData%GetValues(RootZone%NVRVRootZone%RiparianVeg%iColETc(:,1)) * RootZone%NVRVRootZone%RiparianVeg%Area(:,1)
        rNVRVPrecip(:,1)    = RootZone%ElemPrecipData%Precip * rNVRVArea(:,1)                                                                     !Native and riparian precip
        rNVRVRunoff(:,1)    = RootZone%NVRVRootZone%NativeVeg%Runoff(:,1)    &                                                                    !Native and riparian runoff
                            + RootZone%NVRVRootZone%RiparianVeg%Runoff(:,1)                                                                                           
        rNVRVBeginStor(:,1) = (RootZone%NVRVRootZone%NativeVeg%SoilM_Precip_P_BeforeUpdate(:,1)   &                                               !Native and riparian beginning storage
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_AW_P_BeforeUpdate(:,1)        &
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_Oth_P_BeforeUpdate(:,1)       ) * RootZone%NVRVRootZone%NativeVeg%Area_P(:,1)   &                                   
                            +(RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip_P_BeforeUpdate(:,1)  &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_AW_P_BeforeUpdate(:,1)      &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth_P_BeforeUpdate(:,1)     ) * RootZone%NVRVRootZone%RiparianVeg%Area_P(:,1)                                  
        rNVRVSoilMCh(:,1)   = RootZone%NVRVRootZone%NativeVeg%SoilMCh(:,1)  &                                                                !Native and riparian change in soil moisture due to land expansion
                            + RootZone%NVRVRootZone%RiparianVeg%SoilMCh(:,1) 
        CALL RootZone%GetActualRiparianET_AtElements(rNVRVStrmInflow(:,1))                                                                   !Riparian ET from stream
        rNVRVETa(:,1)       = RootZone%NVRVRootZone%NativeVeg%ETa(:,1)  &                                                                    !Native and riparian actual ET
                            + RootZone%NVRVRootZone%RiparianVeg%ETa(:,1)  
        rNVRVEndStor(:,1)   = (RootZone%NVRVRootZone%NativeVeg%SoilM_Precip(:,1)   &                                                         !Native and riparian ending storage
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_AW(:,1)        &
                            + RootZone%NVRVRootZone%NativeVeg%SoilM_Oth(:,1)       ) * RootZone%NVRVRootZone%NativeVeg%Area(:,1) &                                   
                            +(RootZone%NVRVRootZone%RiparianVeg%SoilM_Precip(:,1)  &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_AW(:,1)      &
                            + RootZone%NVRVRootZone%RiparianVeg%SoilM_Oth(:,1)     ) * RootZone%NVRVRootZone%RiparianVeg%Area(:,1)                                    
        rNVRVError          = rNVRVBeginStor + rNVRVSoilMCh + rNVRVInfilt + RNVRVStrmInflow - rNVRVETa - rNVRVPerc - rNVRVEndStor            !Native and riaprain error
        IF (RootZone%Flags%lGenericMoistureFile_Defined) THEN                                                                                !Native and riparian other inflow
            rNVRVOthIn(:,1) = (RootZone%GenericMoistureData%rGenericMoisture(1,:) * RootZone%NVRVRootZone%RootDepth_Native   - RootZone%NVRVRootZone%NativeVeg%GMExcess(:,1)) * RootZone%NVRVRootZone%NativeVeg%Area(:,1)     &
                            + (RootZone%GenericMoistureData%rGenericMoisture(:,1) * RootZone%NVRVRootZone%RootDepth_Riparian - RootZone%NVRVRootZone%RiparianVeg%GMExcess(:,1)) * RootZone%NVRVRootZone%RiparianVeg%Area(:,1) 
            rNVRVError      = rNVRVError + rNVRVOthIn 
        END IF
        IF (RootZone%Flags%lComputeETFromGW) THEN                                                                                            !Native and riparian groundwater inflow for ET
            rNVRVETGW(:,1)  = RootZone%NVRVRootZone%NativeVeg%ETFromGW_Actual(:,1)   &                                                       
                            + RootZone%NVRVRootZone%RiparianVeg%ETFromGW_Actual(:,1)
            rNVRVError      = rNVRVError + rNVRVETGW
        END IF
    END IF
    
    !Print non-ponded ag data
    IF (RootZone%Flags%lNonPondedAg_Defined) THEN
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,1,iLayer,rAgArea_NP)                       !Non-ponded ag area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,2,iLayer,rAgPotET_NP)                      !Non-ponded ag potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,3,iLayer,rAgPrecip_NP)                     !Non-ponded ag precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,4,iLayer,rAgRunoff_NP)                     !Non-ponded ag runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,5,iLayer,rAgAW_NP)                         !Non-ponded ag prime applied water
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,6,iLayer,rAgReuse_NP)                      !Non-ponded ag reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,7,iLayer,rAgReturn_NP)                     !Non-ponded ag return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,8,iLayer,rAgSrfcInForGW_Ag_NP)             !Non-ponded ag surface inflow from ag lands to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,9,iLayer,rAgSrfcInForGW_UrbIn_NP)          !Non-ponded ag surface inflow from urban indoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,10,iLayer,rAgSrfcInForGW_UrbOut_NP)        !Non-ponded ag surface inflow from urban outdoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,11,iLayer,rAgSrfcInForGW_NVRV_NP)          !Non-ponded ag surface inflow from NVRV to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,12,iLayer,rAgBeginStor_NP)                 !Non-ponded ag beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,13,iLayer,rAgSoilMCh_NP)                   !Non-ponded ag change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,14,iLayer,rAgInfilt_NP)                    !Non-ponded ag infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                            !Non-ponded ag groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,15,iLayer,rAgETGW_NP)           
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                                !Non-ponded ag other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,16,iLayer,rAgOthIn_NP)           
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,17,iLayer,rAgETa_NP)                       !Non-ponded ag actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,18,iLayer,rAgPerc_NP)                      !Non-ponded ag perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,19,iLayer,rAgEndStor_NP)                   !Non-ponded ag end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,20,iLayer,rAgError_NP)                     !Non-ponded ag error
    END IF                                                                                                   
                                                                                                             
    !Print ponded ag data
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        !Rice
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,21,iLayer,rAgArea_Rice)               !Rice area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,22,iLayer,rAgPotET_Rice)              !Rice potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,23,iLayer,rAgPrecip_Rice)             !Rice precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,24,iLayer,rAgRunoff_Rice)             !Rice runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,25,iLayer,rAgAW_Rice)                 !Rice prime applied water
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,26,iLayer,rAgReuse_Rice)              !Rice reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,27,iLayer,rAgReturn_Rice)             !Rice return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,28,iLayer,rAgSrfcInForGW_Ag_Rice)     !Rice surface inflow from ag lands to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,29,iLayer,rAgSrfcInForGW_UrbIn_Rice)  !Rice surface inflow from urban indoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,30,iLayer,rAgSrfcInForGW_UrbOut_Rice) !Rice surface inflow from urban outdoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,31,iLayer,rAgSrfcInForGW_NVRV_Rice)   !Rice surface inflow from NVRV to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,32,iLayer,rAgBeginStor_Rice)          !Rice beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,33,iLayer,rAgSoilMCh_Rice)            !Rice change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,34,iLayer,rAgInfilt_Rice)             !Rice infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                       !Rice groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,35,iLayer,rAgETGW_Rice)              
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                           !Rice other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,36,iLayer,rAgOthIn_Rice)              
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,37,iLayer,rAgDrain_Rice)              !Rice pond drain
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,38,iLayer,rAgETa_Rice)                !Rice actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,39,iLayer,rAgPerc_Rice)               !Rice perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,40,iLayer,rAgEndStor_Rice)            !Rice end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,41,iLayer,rAgError_Rice)              !Rice error
        
        !Refuge
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,42,iLayer,rAgArea_Refuge)               !Refuge area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,43,iLayer,rAgPotET_Refuge)              !Refuge potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,44,iLayer,rAgPrecip_Refuge)             !Refuge precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,45,iLayer,rAgRunoff_Refuge)             !Refuge runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,46,iLayer,rAgAW_Refuge)                 !Refuge prime applied water
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,47,iLayer,rAgReuse_Refuge)              !Refuge reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,48,iLayer,rAgReturn_Refuge)             !Refuge return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,49,iLayer,rAgSrfcInForGW_Ag_Refuge)     !Refuge surface inflow from ag lands to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,50,iLayer,rAgSrfcInForGW_UrbIn_Refuge)  !Refuge surface inflow from urban indoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,51,iLayer,rAgSrfcInForGW_UrbOut_Refuge) !Refuge surface inflow from urban outdoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,52,iLayer,rAgSrfcInForGW_NVRV_Refuge)   !Refuge surface inflow from NVRV to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,53,iLayer,rAgBeginStor_Refuge)          !Refuge beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,54,iLayer,rAgSoilMCh_Refuge)            !Refuge change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,55,iLayer,rAgInfilt_Refuge)             !Refuge infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                         !Refuge groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,56,iLayer,rAgETGW_Refuge)              
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                             !Refuge other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,57,iLayer,rAgOthIn_Refuge)              
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,58,iLayer,rAgDrain_Refuge)              !Refuge pond drain
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,59,iLayer,rAgETa_Refuge)                !Refuge actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,60,iLayer,rAgPerc_Refuge)               !Refuge perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,61,iLayer,rAgEndStor_Refuge)            !Refuge end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,62,iLayer,rAgError_Refuge)              !Refuge error
    END IF                                                                                                   
                                                                                                             
    !Print urban data
    IF (RootZone%Flags%lUrban_Defined) THEN                                                                  
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,63,iLayer,rUrbArea)               !Urban area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,64,iLayer,rUrbPotET)              !Urban potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,65,iLayer,rUrbPrecip)             !Urban precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,66,iLayer,rUrbRunoff)             !Urban runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,67,iLayer,rUrbAW)                 !Urban prime applied water
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,68,iLayer,rUrbReuse)              !Urban reuse
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,69,iLayer,rUrbReturn)             !Urban return
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,70,iLayer,rUrbSrfcInForGW_Ag)     !Urban surface inflow from ag lands to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,71,iLayer,rUrbSrfcInForGW_UrbIn)  !Urban surface inflow from urban indoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,72,iLayer,rUrbSrfcInForGW_UrbOut) !Urban surface inflow from urban outdoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,73,iLayer,rUrbSrfcInForGW_NVRV)   !Urban surface inflow from NVRV to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,74,iLayer,rUrbBeginStor)          !Urban beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,75,iLayer,rUrbSoilMCh)            !Urban change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,76,iLayer,rUrbInfilt)             !Urban infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                   !Urban groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,77,iLayer,rUrbETGW)               
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                       !Urban other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,78,iLayer,rUrbOthIn)               
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,79,iLayer,rUrbETa)                !Urban actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,80,iLayer,rUrbPerc)               !Urban perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,81,iLayer,rUrbEndStor)            !Urban end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,82,iLayer,rUrbError)              !Urban error
    END IF                                                                                                   
                                                                                                             
    !Print native and riparian veg data
    IF (RootZone%Flags%lNVRV_Defined) THEN                                                                   
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,83,iLayer,rNVRVArea)               !NVRV area
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,84,iLayer,rNVRVPotET)              !NVRV potential ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,85,iLayer,rNVRVPrecip)             !NVRV precip
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,86,iLayer,rNVRVRunoff)             !NVRV runoff
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,87,iLayer,rNVRVSrfcInForGW_Ag)     !NVRV surface inflow from ag lands to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,88,iLayer,rNVRVSrfcInForGW_UrbIn)  !NVRV surface inflow from urban indoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,89,iLayer,rNVRVSrfcInForGW_UrbOut) !NVRV surface inflow from urban outdoors to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,90,iLayer,rNVRVSrfcInForGW_NVRV)   !NVRV surface inflow from NVRV to recharge gw
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,91,iLayer,rNVRVBeginStor)          !NVRV beginning storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,92,iLayer,rNVRVSoilMCh)            !NVRV change in soil storage from land expansion
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,93,iLayer,rNVRVInfilt)             !NVRV infiltration
        IF (RootZone%Flags%lComputeETFromGW) &                                                                    !NVRV groundwater inflow for ET
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,94,iLayer,rNVRVETGW)                
        IF (RootZone%Flags%lGenericMoistureFile_Defined) &                                                        !NVRV other inflow
            CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,95,iLayer,rNVRVOthIn)                
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,96,iLayer,rNVRVStrmInflow)         !NVRV stream inflow for riparian ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,97,iLayer,rNVRVETa)                !NVRV actual ET
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,98,iLayer,rNVRVPerc)               !NVRV perc
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,99,iLayer,rNVRVEndStor)            !NVRV end storage
        CALL RootZone%RootZoneZoneBudRawFile%WriteData(NLayers,f_iElemDataType,100,iLayer,rNVRVError)             !NVRV error
    END IF
    
  END SUBROUTINE WriteRootZoneFlowsToZoneBudRawFile

  
  
    
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
  ! --- SIMULATE SOIL MOISTURE IN ROOT ZONE
  ! --- Note1: In this version, we assume surface flow from an element cannot 
  ! ---        be used as water supply in another element, so we are not
  ! ---        calculating cross-element surface flows and not iterating
  ! --- Note2: Urban return flow is only the outdoors return flow for easy 
  ! ---        budgeting later; rAW_UrbIndoors variable is the urban indoors 
  ! ---        return flow since 100% of urban indoors water is assumed to 
  ! ---        return
  ! --- Note3: Any surface flow from any land use going to gw is not dealt 
  ! ---        with during simulation; it is dealt with later during 
  ! ---        post-processing and data retrieval
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_Simulate(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZone_v412_Type)     :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'RootZone_v412_Simulate'
    INTEGER                      :: indxElem,NElements,iElemID,iNoElemsToGW(0)
    REAL(8)                      :: DeltaT,Area,rZeroFlow(AppGrid%NElements),                            &    
                                    IrigSupply_Ag(AppGrid%NElements),IrigSupply_Urb(AppGrid%NElements),    &
                                    ElemCropSupply(RootZone%NonPondedAgRootZone%NCrops,AppGrid%NElements), &
                                    ElemPondSupply(f_iNPondedCrops,AppGrid%NElements)
                                    
    !Initialize
    iStat     = 0
    rZeroFlow = 0.0

    ASSOCIATE (pElemSupply       => RootZone%ElemSupply                           , &
               pSoilsData        => RootZone%ElemSoilsData                        , &
               pElemPrecip       => RootZone%ElemPrecipData%Precip                , &
               prGenericMoisture => RootZone%GenericMoistureData%rGenericMoisture , &
               pReuseFracs       => RootZone%ReuseFracFile%rValues                , &
               pReturnFracs      => RootZone%ReturnFracFile%rValues               , &
               pSolverData       => RootZone%SolverData                           )
               
        !Initialize
        DeltaT         = TimeStep%DeltaT
        NElements      = AppGrid%NElements
        IrigSupply_Ag  = pElemSupply%Diversion_Ag  + pElemSupply%Pumping_Ag
        IrigSupply_Urb = pElemSupply%Diversion_Urb + pElemSupply%Pumping_Urb
        
        !Check water supply vs. irrigable lands
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem,iElemID,Area) SCHEDULE(DYNAMIC,500)
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
                IF (RootZone%Flags%lNonPondedAg_Defined) Area = Area + SUM(RootZone%NonPondedAgRootZone%Crops%Area(:,indxElem))
                IF (RootZone%Flags%lPondedAg_Defined)    Area = Area + SUM(RootZone%PondedAgRootZone%Crops%Area(:,indxElem))
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
                    Area = RootZone%UrbanRootZone%UrbData%Area(indxElem,1)
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
          
        !Simulate non-ponded ag lands
        IF (RootZone%Flags%lNonPondedAg_Defined) THEN
            !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem)
            DO indxElem=1,NElements
                ElemCropSupply(:,indxElem) = IrigSupply_Ag(indxElem) * RootZone%NonPondedAgRootZone%Crops%ElemDemandFrac_Ag(:,indxElem)
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
                                                       iNoElemsToGW              , &
                                                       pSolverData               , &
                                                       RootZone%Flags%lLakeElems , &
                                                       iStat                     )
            IF (iStat .EQ. -1) RETURN
        END IF
        
        !Simulate ponded ag lands
        IF (RootZone%Flags%lPondedAg_Defined) THEN  
            !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) SCHEDULE(STATIC,160)
            DO indxElem=1,NElements
                ElemPondSupply(:,indxElem) = IrigSupply_Ag(indxElem) * RootZone%PondedAgRootZone%Crops%ElemDemandFrac_Ag(:,indxElem)
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
                                                    iNoElemsToGW              , &
                                                    pSolverData               , &
                                                    RootZone%Flags%lLakeElems , &
                                                    iStat                     )
            IF (iStat .EQ. -1) RETURN
        END IF
        
        !Simulate urban lands
        IF (RootZone%Flags%lUrban_Defined) THEN 
            CALL RootZone%UrbanRootZone%Simulate(AppGrid                   , &
                                                 ETData                    , &
                                                 DeltaT                    , &
                                                 pElemPrecip               , &
                                                 prGenericMoisture         , &
                                                 pSoilsData                , &
                                                 IrigSupply_Urb            , &
                                                 pReuseFracs               , &
                                                 pReturnFracs              , &
                                                 iNoElemsToGW              , &
                                                 pSolverData               , &
                                                 RootZone%Flags%lLakeElems , &
                                                 iStat                     , &
                                                 RootZone%rAW_UrbanIndoors )
            IF (iStat .EQ. -1) RETURN
            RootZone%UrbanRootZone%UrbData%ReturnFlow(:,1) = RootZone%UrbanRootZone%UrbData%ReturnFlow(:,1) - RootZone%rAW_UrbanIndoors
        END IF
        
        !Simulate native and riparian veg lands
        IF (RootZone%Flags%lNVRV_Defined) THEN
            CALL RootZone%NVRVRootZone%Simulate(AppGrid                        , &
                                                ETData                         , &
                                                DeltaT                         , &
                                                pElemPrecip                    , &
                                                prGenericMoisture              , &
                                                pSoilsData                     , &
                                                rZeroFlow                      , &
                                                iNoElemsToGW                   , &
                                                pSolverData                    , &
                                                RootZone%Flags%lLakeElems      , &
                                                iStat                          )
            IF (iStat .EQ. -1) RETURN
        END IF 
    END ASSOCIATE
    
  END SUBROUTINE RootZone_v412_Simulate

  
  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_RegionalReturnFlow_Ag(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    REAL(8),INTENT(OUT)                  :: RReturnFlow(AppGrid%NSubregions+1)
    
    CALL RegionalReturn(RootZone,AppGrid,f_iLandUse_Ag,RReturnFlow)
                       
  END SUBROUTINE RootZone_v412_RegionalReturnFlow_Ag
  

  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL RETURN FLOW FROM URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE RootZone_v412_RegionalReturnFlow_Urb(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    REAL(8),INTENT(OUT)                  :: RReturnFlow(AppGrid%NSubregions+1)
    
    CALL RegionalReturn(RootZone,AppGrid,f_iLandUse_Urb,RReturnFlow)
                       
  END SUBROUTINE RootZone_v412_RegionalReturnFlow_Urb
  

  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL SURFACE FLOWS TO GW, INFILTRATION AND PERCOLATION
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,lIsForACrop,iLUType,iCrop,RSrfcInForGW_Ag,RSrfcInForGW_UrbIn,RSrfcInForGW_UrbOut,RSrfcInForGW_NVRV,RInfilt,RPerc)
    TYPE(RootZone_v412_Type),INTENT(IN)                  :: RootZone
    TYPE(AppGridType),INTENT(IN)                         :: AppGrid
    LOGICAL,INTENT(IN)                                   :: lIsForACrop 
    INTEGER,INTENT(IN)                                   :: iLUType,iCrop
    REAL(8),DIMENSION(AppGrid%NSubregions+1),INTENT(OUT) :: RSrfcInForGW_Ag,RSrfcInForGW_UrbIn,RSrfcInForGW_UrbOut,RSrfcInForGW_NVRV,RInfilt,RPerc
    
    !Local variables
    INTEGER                                  :: indxCrop,iNSubregions
    REAL(8),DIMENSION(AppGrid%NElements)     :: rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc,  &
                                                rElemSrfcInForGW_Ag_Work,rElemSrfcInForGW_UrbIn_Work,rElemSrfcInForGW_UrbOut_Work,rElemSrfcInForGW_NVRV_Work,    &
                                                rElemInfilt_Work,rElemPerc_Work
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RSrfcInForGW_Ag_Work,RSrfcInForGW_UrbIn_Work,RSrfcInForGW_UrbOut_Work,RSrfcInForGW_NVRV_Work,                    &
                                                RInfilt_Work,RPerc_Work
    
    !Initialize
    iNSubregions = AppGrid%NSubregions
    
    SELECT CASE (iLUType)
        CASE (f_iLandUse_Ag)
            CALL RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,.FALSE.,f_iLandUse_NonPondedAg,0,RSrfcInForGW_Ag,RSrfcInForGW_UrbIn,RSrfcInForGW_UrbOut,RSrfcInForGW_NVRV,RInfilt,RPerc)
            CALL RegionalSrfcInForGW_Infiltration_Perc(RootZone,AppGrid,.FALSE.,f_iLandUse_PondedAg,0,RSrfcInForGW_Ag_Work,RSrfcInForGW_UrbIn_Work,RSrfcInForGW_UrbOut_Work,RSrfcInForGW_NVRV_Work,RInfilt_Work,RPerc_Work)
            RSrfcInForGW_Ag     = RSrfcInForGW_Ag + RSrfcInForGW_Ag_Work
            RSrfcInForGW_UrbIn  = RSrfcInForGW_UrbIn + RSrfcInForGW_UrbIn_Work
            RSrfcInForGW_UrbOut = RSrfcInForGW_UrbOut + RSrfcInForGW_UrbOut_Work
            RSrfcInForGW_NVRV   = RSrfcInForGW_NVRV + RSrfcInForGW_NVRV_Work
            RInfilt             = RInfilt + RInfilt_Work
            RPerc               = RPerc + RPerc_Work
            RETURN
            
        CASE (f_iLandUse_NonPondedAg)
            IF (lIsForACrop) THEN
                CALL ElementalNonPondedCropFlows_ForACrop(RootZone,AppGrid,iCrop,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
            ELSE
                rElemSrfcInForGW_Ag     = 0.0
                rElemSrfcInForGW_UrbIn  = 0.0
                rElemSrfcInForGW_UrbOut = 0.0
                rElemSrfcInForGW_NVRV   = 0.0 
                rElemInfilt             = 0.0
                rElemPerc               = 0.0
                DO indxCrop=1,RootZone%NonPondedAgRootZone%NCrops
                    CALL ElementalNonPondedCropFlows_ForACrop(RootZone,AppGrid,indxCrop,rElemSrfcInForGW_Ag_Work,rElemSrfcInForGW_UrbIn_Work,rElemSrfcInForGW_UrbOut_Work,rElemSrfcInForGW_NVRV_Work,rElemInfilt_Work,rElemPerc_Work)
                    rElemSrfcInForGW_Ag     = rElemSrfcInForGW_Ag     + rElemSrfcInForGW_Ag_Work    
                    rElemSrfcInForGW_UrbIn  = rElemSrfcInForGW_UrbIn  + rElemSrfcInForGW_UrbIn_Work 
                    rElemSrfcInForGW_UrbOut = rElemSrfcInForGW_UrbOut + rElemSrfcInForGW_UrbOut_Work
                    rElemSrfcInForGW_NVRV   = rElemSrfcInForGW_NVRV   + rElemSrfcInForGW_NVRV_Work   
                    rElemInfilt             = rElemInfilt             + rElemInfilt_Work            
                    rElemPerc               = rElemPerc               + rElemPerc_Work              
                END DO
            END IF
            
        CASE (f_iLandUse_PondedAg)
            IF (lIsForACrop) THEN
                CALL ElementalPondedCropFlows_ForACrop(RootZone,AppGrid,iCrop,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
            ELSE
                rElemSrfcInForGW_Ag     = 0.0
                rElemSrfcInForGW_UrbIn  = 0.0
                rElemSrfcInForGW_UrbOut = 0.0
                rElemSrfcInForGW_NVRV   = 0.0 
                rElemInfilt             = 0.0
                rElemPerc               = 0.0
                DO indxCrop=1,RootZone%PondedAgRootZone%NCrops
                    CALL ElementalPondedCropFlows_ForACrop(RootZone,AppGrid,indxCrop,rElemSrfcInForGW_Ag_Work,rElemSrfcInForGW_UrbIn_Work,rElemSrfcInForGW_UrbOut_Work,rElemSrfcInForGW_NVRV_Work,rElemInfilt_Work,rElemPerc_Work)
                    rElemSrfcInForGW_Ag     = rElemSrfcInForGW_Ag     + rElemSrfcInForGW_Ag_Work    
                    rElemSrfcInForGW_UrbIn  = rElemSrfcInForGW_UrbIn  + rElemSrfcInForGW_UrbIn_Work 
                    rElemSrfcInForGW_UrbOut = rElemSrfcInForGW_UrbOut + rElemSrfcInForGW_UrbOut_Work
                    rElemSrfcInForGW_NVRV   = rElemSrfcInForGW_NVRV   + rElemSrfcInForGW_NVRV_Work   
                    rElemInfilt             = rElemInfilt             + rElemInfilt_Work            
                    rElemPerc               = rElemPerc               + rElemPerc_Work              
                END DO
            END IF
            
        CASE (f_iLandUse_Urb)
            CALL ElementalUrbanFlows(RootZone,AppGrid,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
            
        CASE (f_iLandUse_NVRV)
            CALL ElementalNVRVFlows(RootZone,AppGrid,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
    END SELECT
        
    !Accumulate element values to subregions
    RSrfcInForGW_Ag(1:iNSubregions)     = AppGrid%AccumElemValuesToSubregions(rElemSrfcInForGW_Ag) 
    RSrfcInForGW_Ag(iNSubregions+1)     = SUM(RSrfcInForGW_Ag(1:iNSubregions))
    RSrfcInForGW_UrbIn(1:iNSubregions)  = AppGrid%AccumElemValuesToSubregions(rElemSrfcInForGW_UrbIn) 
    RSrfcInForGW_UrbIn(iNSubregions+1)  = SUM(RSrfcInForGW_UrbIn(1:iNSubregions))
    RSrfcInForGW_UrbOut(1:iNSubregions) = AppGrid%AccumElemValuesToSubregions(rElemSrfcInForGW_UrbOut) 
    RSrfcInForGW_UrbOut(iNSubregions+1) = SUM(RSrfcInForGW_UrbOut(1:iNSubregions))
    RSrfcInForGW_NVRV(1:iNSubregions)   = AppGrid%AccumElemValuesToSubregions(rElemSrfcInForGW_NVRV) 
    RSrfcInForGW_NVRV(iNSubregions+1)   = SUM(RSrfcInForGW_NVRV(1:iNSubregions))
    RInfilt(1:iNSubregions)             = AppGrid%AccumElemValuesToSubregions(rElemInfilt) 
    RInfilt(iNSubregions+1)             = SUM(RInfilt(1:iNSubregions))
    RPerc(1:iNSubregions)               = AppGrid%AccumElemValuesToSubregions(rElemPerc) 
    RPerc(iNSubregions+1)               = SUM(RPerc(1:iNSubregions))

  END SUBROUTINE RegionalSrfcInForGW_Infiltration_Perc
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RETURN FLOWS FROM A SPECIFIED LAND USE
  ! --- Note1: Accumulate return flow to subregion and entire model domain 
  ! ---        if it is not going to gw
  ! --- Note2: Handle return flow going to gw through the "Surface Inflow 
  ! ---        to GW" terms
  ! -------------------------------------------------------------
  SUBROUTINE RegionalReturn(RootZone,AppGrid,iLUType,RReturn)
    TYPE(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    INTEGER,INTENT(IN)                  :: iLUType
    REAL(8),INTENT(OUT)                 :: RReturn(AppGrid%NSubregions+1)

    !Local variables
    INTEGER :: iNRegions
    REAL(8) :: rElemValues(AppGrid%NElements)
    
    !Initialize
    RReturn = 0.0
    
    SELECT CASE (iLUType)
        !Ponded and non-ponded ag
        CASE (f_iLandUse_Ag)           
            !Return if no ag lands are defined
            IF (.NOT. RootZone%Flags%lNonPondedAg_Defined) THEN
                IF (.NOT. RootZone%Flags%lPondedAg_Defined) RETURN
            END IF
            
            !Return flows at element level
            IF (RootZone%Flags%lNonPondedAg_Defined) THEN
                rElemValues = SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow , DIM=1)
            ELSE
                rElemValues = 0.0
            END IF
            IF (RootZone%Flags%lPondedAg_Defined) rElemValues = rElemValues + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow , DIM=1)
            
        !General urban 
        CASE (f_iLandUse_Urb)
            !Return if no urban lands are defined
            IF (.NOT. RootZone%Flags%lUrban_Defined) RETURN
            
            !Return flows at element level
            rElemValues = RootZone%UrbanRootZone%UrbData%ReturnFlow(:,1) + RootZone%rAW_UrbanIndoors
            
        !Urban indoors
        CASE (f_iLandUse_UrbIn)           
            !Return if no urban lands are defined
            IF (.NOT. RootZone%Flags%lUrban_Defined) RETURN
            
            !Return flows at element level
            rElemValues =RootZone%rAW_UrbanIndoors
            
        !Urban outdoors
        CASE (f_iLandUse_UrbOut)
            !Return if no urban lands are defined
            IF (.NOT. RootZone%Flags%lUrban_Defined) RETURN
            
            !Return flows at element level
            rElemValues = RootZone%UrbanRootZone%UrbData%ReturnFlow(:,1)

        !Otherwise
        CASE DEFAULT
            RETURN
    END SELECT
    
    !Aggregate element values to subregions
    iNRegions            = AppGrid%NSubregions
    RReturn(1:iNRegions) = AppGrid%AccumElemValuesToSubregions(rElemValues) 
    RReturn(iNRegions+1) = SUM(RReturn(1:iNRegions))

  END SUBROUTINE RegionalReturn


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL RUNOFF FROM A SPECIFIED LAND USE 
  ! --- Note1: Accumulate runoff to subregion and entire model domain 
  ! ---        as if it is not going to gw
  ! --- Note2: Handle runoff going to gw through the "Surface Inflow 
  ! ---        to GW" terms
  ! -------------------------------------------------------------
  SUBROUTINE RegionalRunoff(RootZone,AppGrid,iLUType,RRunoff)
    TYPE(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    INTEGER,INTENT(IN)                  :: iLUType
    REAL(8),INTENT(OUT)                 :: RRunoff(AppGrid%NSubregions+1)

    !Local variables
    INTEGER :: iNRegions
    REAL(8) :: rElemValues(AppGrid%NElements)
    
    !Initialize
    RRunoff = 0.0
    
    SELECT CASE (iLUType)
        !Ponded and non-ponded ag
        CASE (f_iLandUse_Ag)           
            !Return if no ag lands are defined
            IF (.NOT. RootZone%Flags%lNonPondedAg_Defined) THEN
                IF (.NOT. RootZone%Flags%lPondedAg_Defined) RETURN
            END IF
            
            !Runoff at element level
            IF (RootZone%Flags%lNonPondedAg_Defined) THEN
                rElemValues = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff , DIM=1)
            ELSE
                rElemValues = 0.0
            END IF
            IF (RootZone%Flags%lPondedAg_Defined) rElemValues = rElemValues + SUM(RootZone%PondedAgRootZone%Crops%Runoff , DIM=1)
            
        !General urban 
        CASE (f_iLandUse_Urb)
            !Return if no urban lands are defined
            IF (.NOT. RootZone%Flags%lUrban_Defined) RETURN
            
            !Runoff at element level
            rElemValues = RootZone%UrbanRootZone%UrbData%Runoff(:,1)

        !Native and riparian veg
        CASE (f_iLandUse_NVRV)
            !Return if native and riparain veg are not defined
            IF (.NOT. RootZone%Flags%lNVRV_Defined) RETURN
            
            !Runoff at element level
            rElemValues = RootZone%NVRVRootZone%NativeVeg%Runoff(:,1)   &
                        + RootZone%NVRVRootZone%RiparianVeg%Runoff(:,1)

        !Otherwise
        CASE DEFAULT
            RETURN
    END SELECT
    
    !Accumulate element values to subregions
    iNRegions            = AppGrid%NSubregions
    RRunoff(1:iNRegions) = AppGrid%AccumElemValuesToSubregions(rElemValues) 
    RRunoff(iNRegions+1) = SUM(RRunoff(1:iNRegions))
      
  END SUBROUTINE RegionalRunoff


  ! -------------------------------------------------------------
  ! --- COMPUTE REGIONAL POND DRAIN FLOW 
  ! -------------------------------------------------------------
  SUBROUTINE RegionalDrain(RootZone,AppGrid,RDrain)
    TYPE(RootZone_v412_Type),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    REAL(8)                             :: RDrain(AppGrid%NSubregions+1)

    !Local variables
    INTEGER :: iNRegions
    REAL(8) :: rElemValues(AppGrid%NElements)
    
    !Compute element values 
    IF (RootZone%Flags%lPondedAg_Defined) THEN
        rElemValues = SUM(RootZone%PondedAgRootZone%Crops%Drain , DIM=1)
        iNRegions = AppGrid%NSubregions
        RDrain(1:iNRegions) = AppGrid%AccumElemValuesToSubregions(rElemValues)
        RDrain(iNRegions+1) = SUM(RDrain(1:iNRegions))
    ELSE
        RDrain = 0.0
    END IF
      
  END SUBROUTINE RegionalDrain


  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT NON-PONDED CROP SURFACE INFLOWS FOR GW RECHARGE,INFILTRATION  AND PERCOLATION
  ! -------------------------------------------------------------
  SUBROUTINE ElementalNonPondedCropFlows_ForACrop(RootZone,AppGrid,iCrop,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
    TYPE(RootZone_v412_Type),INTENT(IN)              :: RootZone
    TYPE(AppGridType),INTENT(IN)                     :: AppGrid
    INTEGER,INTENT(IN)                               :: iCrop   
    REAL(8),DIMENSION(AppGrid%NElements),INTENT(OUT) :: rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc

    !Local variables
    INTEGER :: indx,iElem,iDestElem
    REAL(8) :: rSurfaceFlow,rAreaFrac
    LOGICAL :: lPondedAg 
    
    !Initialize
    lPondedAg               = RootZone%Flags%lPondedAg_Defined
    rElemInfilt             = RootZone%NonPondedAgRootZone%Crops%IrigInfilt(iCrop,:)     &
                            + RootZone%NonPondedAgRootZone%Crops%PrecipInfilt(iCrop,:)
    rElemPerc               = RootZone%NonPondedAgRootZone%Crops%Perc(iCrop,:)    &
                            + RootZone%NonPondedAgRootZone%Crops%PercCh(iCrop,:) 
    rElemSrfcInForGW_Ag     = 0.0
    rElemSrfcInForGW_UrbIn  = 0.0
    rElemSrfcInForGW_UrbOut = 0.0
    rElemSrfcInForGW_NVRV   = 0.0
    
    !Process ag flows going to gw at non-ponded crop
    DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
        iElem     = RootZone%ElemFlowToGW_Ag(indx)%iElement
        iDestElem = RootZone%ElemFlowToGW_Ag(indx)%iDest
        IF (iDestElem .EQ. -1) iDestElem = iElem
        
        !Runoff, return flow and pond drain
        IF (iElem .EQ. iDestElem) THEN
            rSurfaceFlow = RootZone%NonPondedAgRootZone%Crops%Runoff(iCrop,iElem)    &
                         + RootZone%NonPondedAgRootZone%Crops%ReturnFlow(iCrop,iElem)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            rElemSrfcInForGW_Ag(iDestElem) = rElemSrfcInForGW_Ag(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)         = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)           = rElemPerc(iDestElem) + rSurfaceFlow
        ELSE
            rSurfaceFlow = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff(:,iElem))    &
                         + SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow(:,iElem))
            IF (lPondedAg) rSurfaceFlow = rSurfaceFlow                                             &
                                        + SUM(RootZone%PondedAgRootZone%Crops%Drain(:,iElem))      &
                                        + SUM(RootZone%PondedAgRootZone%Crops%Runoff(:,iElem))     & 
                                        + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(:,iElem))
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            rAreaFrac = RootZone%NonPondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            IF (rAreaFrac .GT. 0.0) THEN
                rSurfaceFlow                   = rSurfaceFlow * rAreaFrac
                rElemSrfcInForGW_Ag(iDestElem) = rElemSrfcInForGW_Ag(iDestElem) + rSurfaceFlow
                rElemInfilt(iDestElem)         = rElemInfilt(iDestElem) + rSurfaceFlow
                rElemPerc(iDestElem)           = rElemPerc(iDestElem) + rSurfaceFlow
            ELSE
            END IF
        END IF
    END DO
    
    !Process urban surface flows going to gw at non-ponded ag crop
    IF (RootZone%Flags%lUrban_Defined) THEN
        !Urban indoors return flows
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbIn)
            iElem     = RootZone%ElemFlowToGW_UrbIn(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbIn(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%rAW_UrbanIndoors(iElem)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                         = RootZone%NonPondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                      = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbIn(iDestElem) = rElemSrfcInForGW_UrbIn(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)            = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)              = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
        
        !Urban outdoors surface flows
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbOut)
            iElem     = RootZone%ElemFlowToGW_UrbOut(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbOut(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%UrbanRootZone%UrbData%Runoff(iElem,1) + RootZone%UrbanRootZone%UrbData%ReturnFlow(iElem,1)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                          = RootZone%NonPondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                       = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbOut(iDestElem) = rElemSrfcInForGW_UrbOut(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !Process native and riparian veg surface flows going to gw at non-ponded ag crop
    IF (RootZone%Flags%lNVRV_Defined) THEN
        DO indx=1,SIZE(RootZone%ElemFlowToGW_NVRV)
            iElem     = RootZone%ElemFlowToGW_NVRV(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_NVRV(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%NVRVRootZone%NativeVeg%Runoff(iElem,1) + RootZone%NVRVRootZone%RiparianVeg%Runoff(iElem,1)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                        = RootZone%NonPondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                     = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_NVRV(iDestElem) = rElemSrfcInForGW_NVRV(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)           = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)             = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
  END SUBROUTINE ElementalNonPondedCropFlows_ForACrop
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT PONDED-CROP SPECIFIC SURFACE INFLOWS FOR GW RECHARGE,INFILTRATION  AND PERCOLATION
  ! -------------------------------------------------------------
  SUBROUTINE ElementalPondedCropFlows_ForACrop(RootZone,AppGrid,iCrop,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
    TYPE(RootZone_v412_Type),INTENT(IN)              :: RootZone
    TYPE(AppGridType),INTENT(IN)                     :: AppGrid
    INTEGER,INTENT(IN)                               :: iCrop
    REAL(8),DIMENSION(AppGrid%NElements),INTENT(OUT) :: rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc

    !Local variables
    INTEGER :: indx,iElem,iDestElem
    REAL(8) :: rSurfaceFlow,rAreaFrac
    LOGICAL :: lNonPondedAg 
    
    !Initialize
    lNonPondedAg              = RootZone%Flags%lNonPondedAg_Defined
    rElemInfilt               = RootZone%PondedAgRootZone%Crops%IrigInfilt(iCrop,:)     &
                              + RootZone%PondedAgRootZone%Crops%PrecipInfilt(iCrop,:) 
    rElemPerc                 = RootZone%PondedAgRootZone%Crops%Perc(iCrop,:)    &
                              + RootZone%PondedAgRootZone%Crops%PercCh(iCrop,:) 
    rElemSrfcInForGW_Ag       = 0.0
    rElemSrfcInForGW_UrbIn    = 0.0
    rElemSrfcInForGW_UrbOut   = 0.0
    rElemSrfcInForGW_NVRV     = 0.0
    
    !Process ag flows going to gw at ponded ag crop
    rSurfaceFlow = 0.0
    DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
        iElem     = RootZone%ElemFlowToGW_Ag(indx)%iElement
        iDestElem = RootZone%ElemFlowToGW_Ag(indx)%iDest
        IF (iDestElem .EQ. -1) iDestElem = iElem
        
        !Runoff, return flow and pond drain
        IF (iElem .EQ. iDestElem) THEN
            rSurfaceFlow   = RootZone%PondedAgRootZone%Crops%Runoff(iCrop,iElem)     &
                           + RootZone%PondedAgRootZone%Crops%ReturnFlow(iCrop,iElem) &
                           + RootZone%PondedAgRootZone%Crops%Drain(iCrop,iElem)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            rElemSrfcInForGW_Ag(iDestElem) = rElemSrfcInForGW_Ag(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)         = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)           = rElemPerc(iDestElem) + rSurfaceFlow
        ELSE
            rSurfaceFlow   = SUM(RootZone%PondedAgRootZone%Crops%Runoff(:,iElem))     &
                           + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(:,iElem)) &
                           + SUM(RootZone%PondedAgRootZone%Crops%Drain(:,iElem))
            IF (lNonPondedAg) rSurfaceFlow = rSurfaceFlow + SUM(RootZone%NonPondedAgRootZone%Crops%Runoff(:,iElem))     &
                                                          + SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow(:,iElem))
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            rAreaFrac                        = RootZone%PondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                     = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_Ag(iDestElem)   = rElemSrfcInForGW_Ag(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)           = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)             = rElemPerc(iDestElem) + rSurfaceFlow
        END IF
    END DO
    
    !Process urban surface flows going to gw at ponded ag crop
    IF (RootZone%Flags%lUrban_Defined) THEN
        !Urban indoors return flows
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbIn)
            iElem     = RootZone%ElemFlowToGW_UrbIn(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbIn(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%rAW_UrbanIndoors(iElem)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                           = RootZone%PondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                        = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbIn(iDestElem)   = rElemSrfcInForGW_UrbIn(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)              = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)                = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
        
        !Urban outdoors surface flows
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbOut)
            iElem     = RootZone%ElemFlowToGW_UrbOut(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbOut(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%UrbanRootZone%UrbData%Runoff(iElem,1) + RootZone%UrbanRootZone%UrbData%ReturnFlow(iElem,1)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                            = RootZone%PondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                         = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbOut(iDestElem)   = rElemSrfcInForGW_UrbOut(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)               = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)                 = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !Process native and riparian veg surface flows going to gw at ponded ag lands (seperately for rice and refuge lands)
    IF (RootZone%Flags%lNVRV_Defined) THEN
        DO indx=1,SIZE(RootZone%ElemFlowToGW_NVRV)
            iElem     = RootZone%ElemFlowToGW_NVRV(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_NVRV(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%NVRVRootZone%NativeVeg%Runoff(iElem,1) + RootZone%NVRVRootZone%RiparianVeg%Runoff(iElem,1)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                          = RootZone%PondedAgRootZone%Crops%Area(iCrop,iDestElem) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                       = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_NVRV(iDestElem)   = rElemSrfcInForGW_NVRV(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
  END SUBROUTINE ElementalPondedCropFlows_ForACrop
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT URBAN SURFACE INFLOWS FOR GW RECHARGE,INFILTRATION  AND PERCOLATION
  ! -------------------------------------------------------------
  SUBROUTINE ElementalUrbanFlows(RootZone,AppGrid,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
    TYPE(RootZone_v412_Type),INTENT(IN)              :: RootZone
    TYPE(AppGridType),INTENT(IN)                     :: AppGrid
    REAL(8),DIMENSION(AppGrid%NElements),INTENT(OUT) :: rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc

    !Local variables
    INTEGER :: indx,iElem,iDestElem
    REAL(8) :: rSurfaceFlow,rAreaFrac
    LOGICAL :: lNonPondedAg,lPondedAg 
    
    !Initialize
    lNonPondedAg              = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg                 = RootZone%Flags%lPondedAg_Defined
    rElemInfilt               = RootZone%UrbanRootZone%UrbData%IrigInfilt(:,1)     &
                              + RootZone%UrbanRootZone%UrbData%PrecipInfilt(:,1) 
    rElemPerc                 = RootZone%UrbanRootZone%UrbData%Perc(:,1)    &
                              + RootZone%UrbanRootZone%UrbData%PercCh(:,1) 
    rElemSrfcInForGW_Ag       = 0.0
    rElemSrfcInForGW_UrbIn    = 0.0
    rElemSrfcInForGW_UrbOut   = 0.0
    rElemSrfcInForGW_NVRV     = 0.0
    
    !Process ag flows going to gw at urban lands
    IF (lNonPondedAg  .OR. lPondedAg) THEN
        DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
            iElem     = RootZone%ElemFlowToGW_Ag(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_Ag(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = 0.0
            IF (lNonPondedAg) rSurfaceFlow   = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff(:,iElem))     &
                                             + SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow(:,iElem))  
            IF (lPondedAg) rSurfaceFlow = rSurfaceFlow                                             &
                                        + SUM(RootZone%PondedAgRootZone%Crops%Runoff(:,iElem))     &
                                        + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(:,iElem)) & 
                                        + SUM(RootZone%PondedAgRootZone%Crops%Drain(:,iElem))
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                        = RootZone%UrbanRootZone%UrbData%Area(iDestElem,1) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                     = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_Ag(iDestElem)   = rElemSrfcInForGW_Ag(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)           = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)             = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !Process urban surface flows going to gw at urban lands
    !Urban indoors return flows
    DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbIn)
        iElem     = RootZone%ElemFlowToGW_UrbIn(indx)%iElement
        iDestElem = RootZone%ElemFlowToGW_UrbIn(indx)%iDest
        IF (iDestElem .EQ. -1) iDestElem = iElem
        
        rSurfaceFlow = RootZone%rAW_UrbanIndoors(iElem)
        IF (rSurfaceFlow .EQ. 0.0) CYCLE
        
        IF (iElem .EQ. iDestElem) THEN
            rElemSrfcInForGW_UrbIn(iDestElem) = rElemSrfcInForGW_UrbIn(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)            = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)              = rElemPerc(iDestElem) + rSurfaceFlow
        ELSE
            rAreaFrac                         = RootZone%UrbanRootZone%UrbData%Area(iDestElem,1) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                      = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbIn(iDestElem) = rElemSrfcInForGW_UrbIn(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)            = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)              = rElemPerc(iDestElem) + rSurfaceFlow
        END IF
    END DO
    
    !Urban outdoors surface flows
    DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbOut)
        iElem     = RootZone%ElemFlowToGW_UrbOut(indx)%iElement
        iDestElem = RootZone%ElemFlowToGW_UrbOut(indx)%iDest
        IF (iDestElem .EQ. -1) iDestElem = iElem
        
        rSurfaceFlow = RootZone%UrbanRootZone%UrbData%Runoff(iElem,1) + RootZone%UrbanRootZone%UrbData%ReturnFlow(iElem,1)
        IF (rSurfaceFlow .EQ. 0.0) CYCLE
        
        IF (iElem .EQ. iDestElem) THEN
            rElemSrfcInForGW_UrbOut(iDestElem) = rElemSrfcInForGW_UrbOut(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        ELSE
            rAreaFrac                          = RootZone%UrbanRootZone%UrbData%Area(iDestElem,1) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                       = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbOut(iDestElem) = rElemSrfcInForGW_UrbOut(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        END IF
    END DO
    
    !Process native and riparian veg surface flows going to gw at urban lands
    IF (RootZone%Flags%lNVRV_Defined) THEN
        DO indx=1,SIZE(RootZone%ElemFlowToGW_NVRV)
            iElem     = RootZone%ElemFlowToGW_NVRV(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_NVRV(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%NVRVRootZone%NativeVeg%Runoff(iElem,1) + RootZone%NVRVRootZone%RiparianVeg%Runoff(iElem,1)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                          = RootZone%UrbanRootZone%UrbData%Area(iDestElem,1) / AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                       = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_NVRV(iDestElem)   = rElemSrfcInForGW_NVRV(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
  END SUBROUTINE ElementalUrbanFlows
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ELEMENT NVRV SURFACE INFLOWS FOR GW RECHARGE,INFILTRATION  AND PERCOLATION
  ! -------------------------------------------------------------
  SUBROUTINE ElementalNVRVFlows(RootZone,AppGrid,rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc)
    TYPE(RootZone_v412_Type),INTENT(IN)              :: RootZone
    TYPE(AppGridType),INTENT(IN)                     :: AppGrid
    REAL(8),DIMENSION(AppGrid%NElements),INTENT(OUT) :: rElemSrfcInForGW_Ag,rElemSrfcInForGW_UrbIn,rElemSrfcInForGW_UrbOut,rElemSrfcInForGW_NVRV,rElemInfilt,rElemPerc

    !Local variables
    INTEGER :: indx,iElem,iDestElem
    REAL(8) :: rSurfaceFlow,rAreaFrac
    LOGICAL :: lNonPondedAg,lPondedAg 
    
    !Initialize
    lNonPondedAg            = RootZone%Flags%lNonPondedAg_Defined
    lPondedAg               = RootZone%Flags%lPondedAg_Defined
    rElemInfilt             = RootZone%NVRVRootZone%NativeVeg%PrecipInfilt(:,1)     &
                            + RootZone%NVRVRootZone%RiparianVeg%PrecipInfilt(:,1) 
    rElemPerc               = RootZone%NVRVRootZone%NativeVeg%Perc(:,1)     &
                            + RootZone%NVRVRootZone%NativeVeg%PercCh(:,1)   &
                            + RootZone%NVRVRootZone%RiparianVeg%Perc(:,1)   &
                            + RootZone%NVRVRootZone%RiparianVeg%PercCh(:,1)
    rElemSrfcInForGW_Ag     = 0.0
    rElemSrfcInForGW_UrbIn  = 0.0
    rElemSrfcInForGW_UrbOut = 0.0
    rElemSrfcInForGW_NVRV   = 0.0
    
    !Process ag flows going to gw at urban lands
    IF (lNonPondedAg  .OR.  lPondedAg) THEN
        DO indx=1,SIZE(RootZone%ElemFlowToGW_Ag)
            iElem     = RootZone%ElemFlowToGW_Ag(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_Ag(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = 0.0
            IF (lNonPondedAg) rSurfaceFlow   = SUM(RootZone%NonPondedAgRootZone%Crops%Runoff(:,iElem))     &
                                             + SUM(RootZone%NonPondedAgRootZone%Crops%ReturnFlow(:,iElem))  
            IF (lPondedAg) rSurfaceFlow = rSurfaceFlow                                             &
                                        + SUM(RootZone%PondedAgRootZone%Crops%Runoff(:,iElem))     &
                                        + SUM(RootZone%PondedAgRootZone%Crops%ReturnFlow(:,iElem)) & 
                                        + SUM(RootZone%PondedAgRootZone%Crops%Drain(:,iElem))
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                      = (RootZone%NVRVRootZone%NativeVeg%Area(iDestElem,1) + RootZone%NVRVRootZone%RiparianVeg%Area(iDestElem,1))/ AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                   = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_Ag(iDestElem) = rElemSrfcInForGW_Ag(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)         = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)           = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !Process urban surface flows going to gw at NVRV
    IF (RootZone%Flags%lUrban_Defined) THEN
        !Urban indoors return flows
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbIn)
            iElem     = RootZone%ElemFlowToGW_UrbIn(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbIn(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%rAW_UrbanIndoors(iElem)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                         = (RootZone%NVRVRootZone%NativeVeg%Area(iDestElem,1) + RootZone%NVRVRootZone%RiparianVeg%Area(iDestElem,1))/ AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                      = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbIn(iDestElem) = rElemSrfcInForGW_UrbIn(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)            = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)              = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
        
        !Urban outdoors surface flows
        DO indx=1,SIZE(RootZone%ElemFlowToGW_UrbOut)
            iElem     = RootZone%ElemFlowToGW_UrbOut(indx)%iElement
            iDestElem = RootZone%ElemFlowToGW_UrbOut(indx)%iDest
            IF (iDestElem .EQ. -1) CYCLE
            IF (iElem .EQ. iDestElem) CYCLE
            
            rSurfaceFlow = RootZone%UrbanRootZone%UrbData%Runoff(iElem,1) + RootZone%UrbanRootZone%UrbData%ReturnFlow(iElem,1)
            IF (rSurfaceFlow .EQ. 0.0) CYCLE
            
            rAreaFrac                          = (RootZone%NVRVRootZone%NativeVeg%Area(iDestElem,1) + RootZone%NVRVRootZone%RiparianVeg%Area(iDestElem,1))/ AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                       = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_UrbOut(iDestElem) = rElemSrfcInForGW_UrbOut(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        END DO
    END IF
    
    !Process native and riparian veg surface flows going to gw at NVRV
    DO indx=1,SIZE(RootZone%ElemFlowToGW_NVRV)
        iElem     = RootZone%ElemFlowToGW_NVRV(indx)%iElement
        iDestElem = RootZone%ElemFlowToGW_NVRV(indx)%iDest
        IF (iDestElem .EQ. -1) iDestElem = iElem
        
        rSurfaceFlow = RootZone%NVRVRootZone%NativeVeg%Runoff(iElem,1) + RootZone%NVRVRootZone%RiparianVeg%Runoff(iElem,1)
        IF (rSurfaceFlow .EQ. 0.0) CYCLE
        
        IF (iElem .EQ. iDestElem) THEN
            rElemSrfcInForGW_NVRV(iDestElem)   = rElemSrfcInForGW_NVRV(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        ELSE
            rAreaFrac                          = (RootZone%NVRVRootZone%NativeVeg%Area(iDestElem,1) + RootZone%NVRVRootZone%RiparianVeg%Area(iDestElem,1))/ AppGrid%AppElement(iDestElem)%Area
            rSurfaceFlow                       = rSurfaceFlow * rAreaFrac
            rElemSrfcInForGW_NVRV(iDestElem)   = rElemSrfcInForGW_NVRV(iDestElem) + rSurfaceFlow
            rElemInfilt(iDestElem)             = rElemInfilt(iDestElem) + rSurfaceFlow
            rElemPerc(iDestElem)               = rElemPerc(iDestElem) + rSurfaceFlow
        END IF
    END DO
    
  END SUBROUTINE ElementalNVRVFlows
  
  
  ! -------------------------------------------------------------
  ! --- CHECK POINTERS TO TIME SERIES DATA COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE CheckTSDataPointers(RootZone,iElemIDs,Precip,ET,iStat)
    TYPE(RootZone_v412_Type),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)                  :: iElemIDs(:)
    TYPE(PrecipitationType),INTENT(IN)  :: Precip
    TYPE(ETType),INTENT(IN)             :: ET
    INTEGER,INTENT(OUT)                 :: iStat
    
    !First call the inhereted method
    CALL RootZone%RootZone_v411_Type%CheckTSDataPointers(iElemIDs,Precip,ET,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Then check the pointers for the surface flow destinations
    CALL RootZone%SurfaceFlowDestinationFile%CheckColNum('the surface flow destination data file for agricultural surface flows',RootZone%iColSurfaceFlowDestination_Ag,.FALSE.,iStat)                           ;  IF (iStat .EQ. -1) RETURN
    CALL RootZone%SurfaceFlowDestinationFile%CheckColNum('the surface flow destination data file for urban indoors return flows',RootZone%iColSurfaceFlowDestination_UrbIndoors,.FALSE.,iStat)                   ;  IF (iStat .EQ. -1) RETURN
    CALL RootZone%SurfaceFlowDestinationFile%CheckColNum('the surface flow destination data file for urban outdoors surface flows',RootZone%iColSurfaceFlowDestination_UrbOutdoors,.FALSE.,iStat)                ;  IF (iStat .EQ. -1) RETURN
    CALL RootZone%SurfaceFlowDestinationFile%CheckColNum('the surface flow destination data file for surface flows from native and riparian vegetation',RootZone%iColSurfaceFlowDestination_NVRV,.FALSE.,iStat)
    
  END SUBROUTINE CheckTSDataPointers
  
END MODULE