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
MODULE IWFM_Misc_Exports
  USE,INTRINSIC :: ISO_C_BINDING  , ONLY: C_INT                                    , &
                                          C_CHAR                                   , &
                                          C_DOUBLE
  USE MessageLogger               , ONLY: SetLogFileName                           , &
                                          KillLogFile                              , &
                                          SetLastMessage                           , &
                                          GetLastMessage                           , &
                                          LogLastMessage                           , &
                                          f_iFatal
  USE GeneralUtilities            , ONLY: String_Copy_C_F                          , &
                                          String_Copy_F_C                          , &
                                          f_cLineFeed                              
  USE TimeSeriesUtilities         , ONLY: CTimeStep_To_RTimeStep                   , &
                                          NPeriods                                 , &
                                          IsTimeIntervalValid                      , &
                                          IncrementTimeStamp                       , &
                                          OPERATOR(.TSGT.)
  USE IOInterface                 , ONLY: GenericFileType
  USE Package_Misc                , ONLY: Package_Misc_GetVersion                  , &
                                          f_iFlowDest_Outside                      , &
                                          f_iFlowDest_StrmNode                     , &
                                          f_iFlowDest_Element                      , &
                                          f_iFlowDest_Lake                         , &
                                          f_iFlowDest_Subregion                    , &
                                          f_iFlowDest_GWElement                    , &
                                          f_iFlowDest_ElementSet                   , &
                                          f_iLocationType_Subregion                , &
                                          f_iLocationType_Zone                     , &
                                          f_iLocationType_Node                     , &
                                          f_iLocationType_Element                  , &
                                          f_iLocationType_StrmHydObs               , &
                                          f_iLocationType_GWHeadObs                , &
                                          f_iLocationType_SubsidenceObs            , &
                                          f_iLocationType_TileDrainObs             , &
                                          f_iLocationType_StrmReach                , &
                                          f_iLocationType_StrmNode                 , &
                                          f_iLocationType_Lake                     , &
                                          f_iLocationType_TileDrain                , &
                                          f_iLocationType_SmallWatershed           , &
                                          f_iLocationType_StrmNodeBud              , &
                                          f_iLocationType_Diversion                , &
                                          f_iLocationType_Bypass                   , &
                                          f_iDataUnitType_Length                   , &
                                          f_iDataUnitType_Area                     , &
                                          f_iDataUnitType_Volume                   , &
                                          f_iSupply_Diversion                      , &
                                          f_iSupply_ElemPump                       , &
                                          f_iSupply_Well                           , &
                                          f_iLandUse_Ag                            , &
                                          f_iLandUse_Urb                           , &
                                          f_iLandUse_UrbIn                         , &
                                          f_iLandUse_UrbOut                        , &
                                          f_iLandUse_NonPondedAg                   , & 
                                          f_iLandUse_Rice                          , & 
                                          f_iLandUse_Refuge                        , & 
                                          f_iLandUse_NVRV                                  
  USE Package_Budget              , ONLY: Package_Budget_GetVersion
  USE Package_ZBudget             , ONLY: Package_ZBudget_GetVersion               , &
                                          f_iZoneHorizontal                        , &
                                          f_iZoneVertical
  USE Package_Matrix              , ONLY: MatrixType
  USE Package_Discretization      , ONLY: Package_Discretization_GetVersion
  USE Package_PrecipitationET     , ONLY: Package_PrecipitationET_GetVersion
  USE Package_AppGW               , ONLY: f_iBudgetType_GW                         
  USE Package_ComponentConnectors , ONLY: Package_ComponentConnectors_GetVersion
  USE Package_GWZBudget           , ONLY: f_iZBudgetType_GW                     
  USE Package_UnsatZone           , ONLY: Package_UnsatZone_GetVersion
  USE Package_AppUnsatZone        , ONLY: f_iBudgetType_UnsatZone                  , &
                                          f_iZBudgetType_UnsatZone                 
  USE Package_AppStream           , ONLY: AppStreamType                            , &
                                          f_iBudgetType_StrmNode                   , &
                                          f_iBudgetType_StrmReach                  , &
                                          f_iBudgetType_DiverDetail                
  USE Package_AppLake             , ONLY: AppLakeType                              , &
                                          f_iBudgetType_Lake            
  USE Package_RootZone            , ONLY: RootZoneType                             , &
                                          f_iBudgetType_RootZone                   , &
                                          f_iBudgetType_LWU                        , &
                                          f_iBudgetType_NonPondedCrop_RZ           , &
                                          f_iBudgetType_NonPondedCrop_LWU          , &
                                          f_iBudgetType_PondedCrop_RZ              , &
                                          f_iBudgetType_PondedCrop_LWU             , &
                                          f_iZBudgetType_RootZone                  , &
                                          f_iZBudgetType_LWU 
  USE Package_AppSmallWatershed   , ONLY: f_iBudgetType_SWShed 
  USE IWFM_Core_Version           , ONLY: IWFM_Core
  USE IWFM_Util_VersionF          , ONLY: IWFM_Util
  IMPLICIT NONE
    
  
  ! -------------------------------------------------------------
  ! --- PUBLIC VARIABLES
  ! -------------------------------------------------------------
  PUBLIC

  
CONTAINS


    

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
  ! --- CLOSE MESSAGE FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_CloseLogFile(iStat) BIND(C,NAME='IW_CloseLogFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_CloseLogFile
    INTEGER(C_INT),INTENT(OUT) :: iStat
  
    iStat = 0
    CALL KillLogFile()
    
  END SUBROUTINE IW_CloseLogFile
  


  
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
  ! --- GET IWFM VERSION
  ! --- Note: This method is developed for mixed-language programming
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetVersion(iLen,cVer,iStat) BIND(C,NAME='IW_GetVersion')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetVersion
    INTEGER(C_INT),INTENT(IN)          :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cVer(iLen)
    INTEGER(C_INT),INTENT(OUT)         :: iStat
    
    !Local variables
    TYPE(RootZoneType)  :: RootZone
    TYPE(MatrixType)    :: Matrix
    TYPE(AppStreamType) :: AppStream
    TYPE(AppLakeType)   :: AppLake
    CHARACTER           :: cVer_F*iLen
    
    iStat = 0
    
    cVer_F = 'IWFM Core: ' // TRIM(IWFM_Core%GetVersion())                                         // f_cLineFeed // &
             'IWFM_Util.lib: ' // TRIM(IWFM_Util%GetVersion())                                     // f_cLineFeed // &
             'Package_Misc.lib: ' // TRIM(Package_Misc_GetVersion())                               // f_cLineFeed // &
             'Package_Discretization.lib: ' // TRIM(Package_Discretization_GetVersion())           // f_cLineFeed // &
             'Package_ComponentConnectors.lib: ' // TRIM(Package_ComponentConnectors_GetVersion()) // f_cLineFeed // &
             'Package_Budget.lib: ' // TRIM(Package_Budget_GetVersion())                           // f_cLineFeed // &
             'Package_ZBudget.lib: ' // TRIM(Package_ZBudget_GetVersion())                         // f_cLineFeed // &
             'Package_AppStream.lib: ' // TRIM(AppStream%GetVersion())                             // f_cLineFeed // &
             'Package_AppLake.lib: ' // TRIM(AppLake%GetVersion())                                 // f_cLineFeed // &
             'Package_UnsatZone.lib: ' // TRIM(Package_UnsatZone_GetVersion())                     // f_cLineFeed // &
             'Package_RootZone.lib: ' // TRIM(RootZone%GetVersion())                               // f_cLineFeed // &
             'Package_Matrix.lib: ' // TRIM(Matrix%GetVersion())                                   // f_cLineFeed // &
             'Package_PrecipitationET.lib: ' // TRIM(Package_PrecipitationET_GetVersion())  
    
    CALL String_Copy_F_C(cVer_F,cVer)
    
  END SUBROUTINE IW_GetVersion
  
  
  ! -------------------------------------------------------------
  ! --- GET IWFM_Util VERSION
  ! -------------------------------------------------------------
  SUBROUTINE IW_IWFMUtil_GetVersion(iLen,cVer,iStat) BIND(C,NAME='IW_IWFMUtil_GetVersion')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_IWFMUtil_GetVersion
    INTEGER(C_INT),INTENT(IN)          :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cVer(iLen)
    INTEGER(C_INT),INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER :: cVer_F*iLen
    
    iStat = 0
    
    cVer_F = IWFM_Util%GetVersion()
    CALL String_Copy_F_C(cVer_F,cVer)
    
  END SUBROUTINE IW_IWFMUtil_GetVersion
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TYPE IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetBudgetTypeIDs(iBudgetTypeID_GW                , &
                                 iBudgetTypeID_RootZone          , &
                                 iBudgetTypeID_LWU               , &
                                 iBudgetTypeID_NonPondedCrop_RZ  , &
                                 iBudgetTypeID_NonPondedCrop_LWU , &
                                 iBudgetTypeID_PondedCrop_RZ     , &
                                 iBudgetTypeID_PondedCrop_LWU    , &
                                 iBudgetTypeID_UnsatZone         , &
                                 iBudgetTypeID_StrmNode          , &
                                 iBudgetTypeID_StrmReach         , &
                                 iBudgetTypeID_DiverDetail       , &
                                 iBudgetTypeID_SWShed            , &
                                 iBudgetTypeID_Lake              , &
                                 iStat) BIND(C,NAME='IW_GetBudgetTypeIDs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetBudgetTypeIDs
    INTEGER(C_INT),INTENT(OUT) :: iBudgetTypeID_GW,iBudgetTypeID_RootZone,iBudgetTypeID_LWU,iBudgetTypeID_UnsatZone,iBudgetTypeID_StrmNode,iBudgetTypeID_StrmReach,iBudgetTypeID_SWShed,iBudgetTypeID_DiverDetail,iBudgetTypeID_NonPondedCrop_RZ,iBudgetTypeID_NonPondedCrop_LWU,iBudgetTypeID_PondedCrop_RZ,iBudgetTypeID_PondedCrop_LWU,iBudgetTypeID_Lake,iStat
    
    iStat                           = 0
    iBudgetTypeID_GW                = f_iBudgetType_GW
    iBudgetTypeID_RootZone          = f_iBudgetType_RootZone
    iBudgetTypeID_LWU               = f_iBudgetType_LWU
    iBudgetTypeID_UnsatZone         = f_iBudgetType_UnsatZone 
    iBudgetTypeID_StrmNode          = f_iBudgetType_StrmNode         
    iBudgetTypeID_StrmReach         = f_iBudgetType_StrmReach        
    iBudgetTypeID_SWShed            = f_iBudgetType_SWShed           
    iBudgetTypeID_DiverDetail       = f_iBudgetType_DiverDetail      
    iBudgetTypeID_NonPondedCrop_RZ  = f_iBudgetType_NonPondedCrop_RZ
    iBudgetTypeID_NonPondedCrop_LWU = f_iBudgetType_NonPondedCrop_LWU
    iBudgetTypeID_PondedCrop_RZ     = f_iBudgetType_PondedCrop_RZ   
    iBudgetTypeID_PondedCrop_LWU    = f_iBudgetType_PondedCrop_LWU   
    iBudgetTypeID_Lake              = f_iBudgetType_Lake   
    
  END SUBROUTINE IW_GetBudgetTypeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET Z-BUDGET TYPE IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetZBudgetTypeIDs(iZBudgetTypeID_GW,iZBudgetTypeID_RootZone,iZBudgetTypeID_LWU,iZBudgetTypeID_UnsatZone,iStat) BIND(C,NAME="IW_GetZBudgetTypeIDs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetZBudgetTypeIDs
    INTEGER(C_INT),INTENT(OUT) :: iZBudgetTypeID_GW,iZBudgetTypeID_RootZone,iZBudgetTypeID_LWU,iZBudgetTypeID_UnsatZone,iStat
    
    iStat                    = 0
    iZBudgetTypeID_GW        = f_iZBudgetType_GW
    iZBudgetTypeID_RootZone  = f_iZBudgetType_RootZone
    iZBudgetTypeID_LWU       = f_iZBudgetType_LWU
    iZBudgetTypeID_UnsatZone = f_iZBudgetType_UnsatZone 
    
  END SUBROUTINE IW_GetZBudgetTypeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET LAND USE TYPE IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeIDs(iLUTypeID_GenAg,iLUTypeID_Urb,iLUTypeID_NonPondedAg,iLUTypeID_Rice,iLUTypeID_Refuge,iLUTypeID_NVRV,iStat) BIND(C,NAME="IW_GetLandUseTypeIDs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeIDs
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_GenAg,iLUTypeID_Urb,iLUTypeID_NonPondedAg,iLUTypeID_Rice,iLUTypeID_Refuge,iLUTypeID_NVRV,iStat
    
    iStat                 = 0
    iLUTypeID_GenAg       = f_iLandUse_Ag
    iLUTypeID_Urb         = f_iLandUse_Urb
    iLUTypeID_NonPondedAg = f_iLandUse_NonPondedAg
    iLUTypeID_Rice        = f_iLandUse_Rice
    iLUTypeID_Refuge      = f_iLandUse_Refuge
    iLUTypeID_NVRV        = f_iLandUse_NVRV
    
  END SUBROUTINE IW_GetLandUseTypeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET LAND USE TYPE IDs (Version 1)
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeIDs_1(iLUTypeID_GenAg,iLUTypeID_GenUrb,iLUTypeID_NonPondedAg,iLUTypeID_Rice,iLUTypeID_Refuge,iLUTypeID_UrbIn,iLUTypeID_UrbOut,iLUTypeID_NVRV,iStat) BIND(C,NAME="IW_GetLandUseTypeIDs_1")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeIDs_1
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_GenAg,iLUTypeID_GenUrb,iLUTypeID_NonPondedAg,iLUTypeID_Rice,iLUTypeID_Refuge,iLUTypeID_UrbIn,iLUTypeID_UrbOut,iLUTypeID_NVRV,iStat
    
    iStat                 = 0
    iLUTypeID_GenAg       = f_iLandUse_Ag
    iLUTypeID_GenUrb      = f_iLandUse_Urb
    iLUTypeID_UrbIn       = f_iLandUse_UrbIn
    iLUTypeID_UrbOut      = f_iLandUse_UrbOut
    iLUTypeID_NonPondedAg = f_iLandUse_NonPondedAg
    iLUTypeID_Rice        = f_iLandUse_Rice
    iLUTypeID_Refuge      = f_iLandUse_Refuge
    iLUTypeID_NVRV        = f_iLandUse_NVRV
    
  END SUBROUTINE IW_GetLandUseTypeIDs_1
  
  
  ! -------------------------------------------------------------
  ! --- GET GENERAL AGRICULTURAL LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_GenAg(iLUTypeID_GenAg,iStat) BIND(C,NAME="IW_GetLandUseTypeID_GenAg")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_GenAg
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_GenAg,iStat
    
    iStat           = 0
    iLUTypeID_GenAg = f_iLandUse_Ag
    
  END SUBROUTINE IW_GetLandUseTypeID_GenAg
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_Urban(iLUTypeID_Urb,iStat) BIND(C,NAME="IW_GetLandUseTypeID_Urban")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_Urban
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_Urb,iStat
    
    iStat         = 0
    iLUTypeID_Urb = f_iLandUse_Urb
    
  END SUBROUTINE IW_GetLandUseTypeID_Urban
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN INDOORS LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_UrbIndoors(iLUTypeID_UrbIn,iStat) BIND(C,NAME="IW_GetLandUseTypeID_UrbIndoors")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_UrbIndoors
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_UrbIn,iStat
    
    iStat           = 0
    iLUTypeID_UrbIn = f_iLandUse_UrbIn
    
  END SUBROUTINE IW_GetLandUseTypeID_UrbIndoors
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN OUTDOORS LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_UrbOutdoors(iLUTypeID_UrbOut,iStat) BIND(C,NAME="IW_GetLandUseTypeID_UrbOutdoors")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_UrbOutdoors
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_UrbOut,iStat
    
    iStat            = 0
    iLUTypeID_UrbOut = f_iLandUse_UrbOut
    
  END SUBROUTINE IW_GetLandUseTypeID_UrbOutdoors
  
  
  ! -------------------------------------------------------------
  ! --- GET NON-PONDED AG LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_NonPondedAg(iLUTypeID_NonPondedAg,iStat) BIND(C,NAME="IW_GetLandUseTypeID_NonPondedAg")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_NonPondedAg
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_NonPondedAg,iStat
    
    iStat                 = 0
    iLUTypeID_NonPondedAg = f_iLandUse_NonPondedAg
    
  END SUBROUTINE IW_GetLandUseTypeID_NonPondedAg
  
  
  ! -------------------------------------------------------------
  ! --- GET RICE LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_Rice(iLUTypeID_Rice,iStat) BIND(C,NAME="IW_GetLandUseTypeID_Rice")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_Rice
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_Rice,iStat
    
    iStat          = 0
    iLUTypeID_Rice = f_iLandUse_Rice
    
  END SUBROUTINE IW_GetLandUseTypeID_Rice
  
  
  ! -------------------------------------------------------------
  ! --- GET REFUGE LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_Refuge(iLUTypeID_Refuge,iStat) BIND(C,NAME="IW_GetLandUseTypeID_Refuge")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_Refuge
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_Refuge,iStat
    
    iStat            = 0
    iLUTypeID_Refuge = f_iLandUse_Refuge
    
  END SUBROUTINE IW_GetLandUseTypeID_Refuge
  
  
  ! -------------------------------------------------------------
  ! --- GET NATIVE & RIPARIAN VEGETATION LAND USE TYPE ID
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLandUseTypeID_NVRV(iLUTypeID_NVRV,iStat) BIND(C,NAME="IW_GetLandUseTypeID_NVRV")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLandUseTypeID_NVRV
    INTEGER(C_INT),INTENT(OUT) :: iLUTypeID_NVRV,iStat
    
    iStat          = 0
    iLUTypeID_NVRV = f_iLandUse_NVRV
    
  END SUBROUTINE IW_GetLandUseTypeID_NVRV
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA UNIT TYPE IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetDataUnitTypeIDs(iDataUnitTypeID_Length,iDataUnitTypeID_Area,iDataUnitTypeID_Volume,iStat) BIND(C,NAME="IW_GetDataUnitTypeIDs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetDataUnitTypeIDs
    INTEGER(C_INT),INTENT(OUT) :: iDataUnitTypeID_Length,iDataUnitTypeID_Area,iDataUnitTypeID_Volume,iStat
    
    iStat                  = 0
    iDataUnitTypeID_Length = f_iDataUnitType_Length
    iDataUnitTypeID_Area   = f_iDataUnitType_Area
    iDataUnitTypeID_Volume = f_iDataUnitType_Volume
    
  END SUBROUTINE IW_GetDataUnitTypeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA UNIT TYPE ID FOR LENGTH
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetDataUnitTypeID_Length(iDataUnitTypeID,iStat) BIND(C,NAME="IW_GetDataUnitTypeID_Length")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetDataUnitTypeID_Length
    INTEGER(C_INT),INTENT(OUT) :: iDataUnitTypeID,iStat
    
    iStat           = 0
    iDataUnitTypeID = f_iDataUnitType_Length
    
  END SUBROUTINE IW_GetDataUnitTypeID_Length
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA UNIT TYPE ID FOR AREA
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetDataUnitTypeID_Area(iDataUnitTypeID,iStat) BIND(C,NAME="IW_GetDataUnitTypeID_Area")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetDataUnitTypeID_Area
    INTEGER(C_INT),INTENT(OUT) :: iDataUnitTypeID,iStat
    
    iStat           = 0
    iDataUnitTypeID = f_iDataUnitType_Area
    
  END SUBROUTINE IW_GetDataUnitTypeID_Area
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA UNIT TYPE ID FOR VOLUME
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetDataUnitTypeID_Volume(iDataUnitTypeID,iStat) BIND(C,NAME="IW_GetDataUnitTypeID_Volume")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetDataUnitTypeID_Volume
    INTEGER(C_INT),INTENT(OUT) :: iDataUnitTypeID,iStat
    
    iStat           = 0
    iDataUnitTypeID = f_iDataUnitType_Volume
    
  END SUBROUTINE IW_GetDataUnitTypeID_Volume
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE IDs 
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeIDs(iLocationTypeID_Node           , &
                                   iLocationTypeID_Element        , &
                                   iLocationTypeID_Subregion      , &
                                   iLocationTypeID_Zone           , &
                                   iLocationTypeID_Lake           , &
                                   iLocationTypeID_StrmNode       , &
                                   iLocationTypeID_StrmReach      , &
                                   iLocationTypeID_TileDrainObs   , &
                                   iLocationTypeID_SmallWatershed , &
                                   iLocationTypeID_GWHeadObs      , &
                                   iLocationTypeID_StrmHydObs     , &
                                   iLocationTypeID_SubsidenceObs  , &
                                   iLocationTypeID_StrmNodeBud    , &
                                   iStat) BIND(C,NAME="IW_GetLocationTypeIDs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeIDs
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID_Node,iLocationTypeID_Element,iLocationTypeID_Subregion,iLocationTypeID_Zone,iLocationTypeID_Lake,iLocationTypeID_StrmReach,iLocationTypeID_StrmNode,iLocationTypeID_StrmNodeBud,iLocationTypeID_StrmHydObs,iLocationTypeID_GWHeadObs,iLocationTypeID_SubsidenceObs,iLocationTypeID_SmallWatershed,iLocationTypeID_TileDrainObs,iStat
    
    iStat                          = 0
    iLocationTypeID_Node           = f_iLocationType_Node
    iLocationTypeID_Element        = f_iLocationType_Element
    iLocationTypeID_Subregion      = f_iLocationType_Subregion
    iLocationTypeID_Zone           = f_iLocationType_Zone
    iLocationTypeID_Lake           = f_iLocationType_Lake
    iLocationTypeID_StrmReach      = f_iLocationType_StrmReach
    iLocationTypeID_StrmNode       = f_iLocationType_StrmNode
    iLocationTypeID_StrmHydObs     = f_iLocationType_StrmHydObs
    iLocationTypeID_GWHeadObs      = f_iLocationType_GWHeadObs
    iLocationTypeID_SubsidenceObs  = f_iLocationType_SubsidenceObs
    iLocationTypeID_SmallWatershed = f_iLocationType_SmallWatershed
    iLocationTypeID_TileDrainObs   = f_iLocationType_TileDrainObs
    iLocationTypeID_StrmNodeBud    = f_iLocationType_StrmNodeBud
    
  END SUBROUTINE IW_GetLocationTypeIDs
  
    
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE IDs (OVERWRITES PREVIOUS VERSION)
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeIDs_1(iLocationTypeID_Node           , &
                                     iLocationTypeID_Element        , &
                                     iLocationTypeID_Subregion      , &
                                     iLocationTypeID_Zone           , &
                                     iLocationTypeID_Lake           , &
                                     iLocationTypeID_StrmNode       , &
                                     iLocationTypeID_StrmReach      , &
                                     iLocationTypeID_TileDrainObs   , &
                                     iLocationTypeID_SmallWatershed , &
                                     iLocationTypeID_GWHeadObs      , &
                                     iLocationTypeID_StrmHydObs     , &
                                     iLocationTypeID_SubsidenceObs  , &
                                     iLocationTypeID_StrmNodeBud    , &
                                     iLocationTypeID_Diversion      , &
                                     iLocationTypeID_Bypass         , &
                                     iStat) BIND(C,NAME="IW_GetLocationTypeIDs_1")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeIDs_1
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID_Node,iLocationTypeID_Element,iLocationTypeID_Subregion,iLocationTypeID_Zone,iLocationTypeID_Lake,iLocationTypeID_StrmReach,iLocationTypeID_StrmNode,iLocationTypeID_StrmNodeBud,iLocationTypeID_StrmHydObs,iLocationTypeID_GWHeadObs,iLocationTypeID_SubsidenceObs,iLocationTypeID_SmallWatershed,iLocationTypeID_TileDrainObs,iLocationTypeID_Diversion,iLocationTypeID_Bypass,iStat
    
    iStat                          = 0
    iLocationTypeID_Node           = f_iLocationType_Node
    iLocationTypeID_Element        = f_iLocationType_Element
    iLocationTypeID_Subregion      = f_iLocationType_Subregion
    iLocationTypeID_Zone           = f_iLocationType_Zone
    iLocationTypeID_Lake           = f_iLocationType_Lake
    iLocationTypeID_StrmReach      = f_iLocationType_StrmReach
    iLocationTypeID_StrmNode       = f_iLocationType_StrmNode
    iLocationTypeID_StrmHydObs     = f_iLocationType_StrmHydObs
    iLocationTypeID_GWHeadObs      = f_iLocationType_GWHeadObs
    iLocationTypeID_SubsidenceObs  = f_iLocationType_SubsidenceObs
    iLocationTypeID_SmallWatershed = f_iLocationType_SmallWatershed
    iLocationTypeID_TileDrainObs   = f_iLocationType_TileDrainObs
    iLocationTypeID_StrmNodeBud    = f_iLocationType_StrmNodeBud
    iLocationTypeID_Diversion      = f_iLocationType_Diversion
    iLocationTypeID_Bypass         = f_iLocationType_Bypass
    
  END SUBROUTINE IW_GetLocationTypeIDs_1
  
    
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_Subregion(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_Subregion")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_Subregion
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_Subregion

  END SUBROUTINE IW_GetLocationTypeID_Subregion
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR ZONE
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_Zone(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_Zone")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_Zone
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_Zone

  END SUBROUTINE IW_GetLocationTypeID_Zone
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR NODE
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_Node(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_Node")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_Node
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_Node

  END SUBROUTINE IW_GetLocationTypeID_Node
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR LAKE
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_Lake(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_Lake")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_Lake
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_Lake

  END SUBROUTINE IW_GetLocationTypeID_Lake
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR STREAM REACH
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_StrmReach(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_StrmReach")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_StrmReach
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_StrmReach

  END SUBROUTINE IW_GetLocationTypeID_StrmReach
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_StrmNode(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_StrmNode")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_StrmNode
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_StrmNode

  END SUBROUTINE IW_GetLocationTypeID_StrmNode
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID STREAM HYDROGRAPHS
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_StrmHydObs(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_StrmHydObs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_StrmHydObs
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_StrmHydObs

  END SUBROUTINE IW_GetLocationTypeID_StrmHydObs
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR STREAM NODES WITH PRINTED WATER BUDGET
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_StrmNodeBud(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_StrmNodeBud")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_StrmNodeBud
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_StrmNodeBud

  END SUBROUTINE IW_GetLocationTypeID_StrmNodeBud
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR GW HEAD OBS
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_GWHeadObs(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_GWHeadObs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_GWHeadObs
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_GWHeadObs
    
  END SUBROUTINE IW_GetLocationTypeID_GWHeadObs
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR SUBSIDENCE OBS
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_SubsidenceObs(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_SubsidenceObs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_SubsidenceObs
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_SubsidenceObs

  END SUBROUTINE IW_GetLocationTypeID_SubsidenceObs
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR SMALL WATERSHED
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_SmallWatershed(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_SmallWatershed")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_SmallWatershed
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_SmallWatershed

  END SUBROUTINE IW_GetLocationTypeID_SmallWatershed
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR TILE DRAINS
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_TileDrain(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_TileDrain")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_TileDrain
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_TileDrain

  END SUBROUTINE IW_GetLocationTypeID_TileDrain
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR TILE DRAINS WITH PRINTED HYDROGRAPH
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_TileDrainObs(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_TileDrainObs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_TileDrainObs
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_TileDrainObs

  END SUBROUTINE IW_GetLocationTypeID_TileDrainObs
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_Element(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_Element")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_Element
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_Element

  END SUBROUTINE IW_GetLocationTypeID_Element
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR DIVERSION
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_Diversion(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_Diversion")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_Diversion
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_Diversion

  END SUBROUTINE IW_GetLocationTypeID_Diversion
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION TYPE ID FOR BYPASS
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLocationTypeID_Bypass(iLocationTypeID,iStat) BIND(C,NAME="IW_GetLocationTypeID_Bypass")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLocationTypeID_Diversion
    INTEGER(C_INT),INTENT(OUT) :: iLocationTypeID,iStat
    
    iStat           = 0
    iLocationTypeID = f_iLocationType_Bypass

  END SUBROUTINE IW_GetLocationTypeID_Bypass
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE IDs
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeIDs(iFlowDestTypeID_Outside,iFlowDestTypeID_StrmNode,iFlowDestTypeID_Element,iFlowDestTypeID_Lake,iFlowDestTypeID_Subregion,iFlowDestTypeID_GWElement,iFlowDestTypeID_ElementSet,iStat) BIND(C,NAME="IW_GetFlowDestTypeIDs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeIDs
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID_Outside,iFlowDestTypeID_StrmNode,iFlowDestTypeID_Element,iFlowDestTypeID_Lake,iFlowDestTypeID_Subregion,iFlowDestTypeID_GWElement,iFlowDestTypeID_ElementSet,iStat
    
    iStat                      = 0
    iFlowDestTypeID_Outside    = f_iFlowDest_Outside
    iFlowDestTypeID_StrmNode   = f_iFlowDest_StrmNode
    iFlowDestTypeID_Element    = f_iFlowDest_Element
    iFlowDestTypeID_Lake       = f_iFlowDest_Lake
    iFlowDestTypeID_Subregion  = f_iFlowDest_Subregion
    iFlowDestTypeID_GWElement  = f_iFlowDest_GWElement
    iFlowDestTypeID_ElementSet = f_iFlowDest_ElementSet
    
  END SUBROUTINE IW_GetFlowDestTypeIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE ID FOR OUTSIDE THE MODEL
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeID_Outside(iFlowDestTypeID,iStat) BIND(C,NAME='IW_GetFlowDestTypeID_Outside')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeID_Outside
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID,iStat
    
    iStat           = 0
    iFlowDestTypeID = f_iFlowDest_Outside
    
  END SUBROUTINE IW_GetFlowDestTypeID_Outside
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE ID FOR STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeID_StrmNode(iFlowDestTypeID,iStat) BIND(C,NAME="IW_GetFlowDestTypeID_StrmNode")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeID_StrmNode
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID,iStat
    
    iStat           = 0
    iFlowDestTypeID = f_iFlowDest_StrmNode
    
  END SUBROUTINE IW_GetFlowDestTypeID_StrmNode
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE ID FOR ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeID_Element(iFlowDestTypeID,iStat) BIND(C,NAME="IW_GetFlowDestTypeID_Element")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeID_Element
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID,iStat
    
    iStat           = 0
    iFlowDestTypeID = f_iFlowDest_Element
    
  END SUBROUTINE IW_GetFlowDestTypeID_Element
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE ID FOR LAKE
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeID_Lake(iFlowDestTypeID,iStat) BIND(C,NAME="IW_GetFlowDestTypeID_Lake")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeID_Lake
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID,iStat
    
    iStat           = 0
    iFlowDestTypeID = f_iFlowDest_Lake
    
  END SUBROUTINE IW_GetFlowDestTypeID_Lake
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE ID FOR SUBREGION
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeID_Subregion(iFlowDestTypeID,iStat) BIND(C,NAME="IW_GetFlowDestTypeID_Subregion")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeID_Subregion
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID,iStat
    
    iStat           = 0
    iFlowDestTypeID = f_iFlowDest_Subregion
    
  END SUBROUTINE IW_GetFlowDestTypeID_Subregion
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE ID FOR GW ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeID_GWElement(iFlowDestTypeID,iStat) BIND(C,NAME="IW_GetFlowDestTypeID_GWElement")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeID_GWElement
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID,iStat
    
    iStat           = 0
    iFlowDestTypeID = f_iFlowDest_GWElement
    
  END SUBROUTINE IW_GetFlowDestTypeID_GWElement
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW DESTINATION TYPE ID FOR ELEMENT SET
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetFlowDestTypeID_ElementSet(iFlowDestTypeID,iStat) BIND(C,NAME="IW_GetFlowDestTypeID_ElementSet")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetFlowDestTypeID_ElementSet
    INTEGER(C_INT),INTENT(OUT) :: iFlowDestTypeID,iStat
    
    iStat           = 0
    iFlowDestTypeID = f_iFlowDest_ElementSet
    
  END SUBROUTINE IW_GetFlowDestTypeID_ElementSet
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY TYPE ID FOR DIVERSION
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetSupplyTypeID_Diversion(iSupplyTypeID,iStat) BIND(C,NAME="IW_GetSupplyTypeID_Diversion")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetSupplyTypeID_Diversion
    INTEGER(C_INT),INTENT(OUT) :: iSupplyTypeID,iStat
    
    iStat         = 0
    iSupplyTypeID = f_iSupply_Diversion

  END SUBROUTINE IW_GetSupplyTypeID_Diversion
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY TYPE ID FOR WELL
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetSupplyTypeID_Well(iSupplyTypeID,iStat) BIND(C,NAME="IW_GetSupplyTypeID_Well")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetSupplyTypeID_Well
    INTEGER(C_INT),INTENT(OUT) :: iSupplyTypeID,iStat
    
    iStat         = 0
    iSupplyTypeID = f_iSupply_Well

  END SUBROUTINE IW_GetSupplyTypeID_Well
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY TYPE ID FOR ELEMENT PUMPING
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetSupplyTypeID_ElemPump(iSupplyTypeID,iStat) BIND(C,NAME="IW_GetSupplyTypeID_ElemPump")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetSupplyTypeID_ElemPump
    INTEGER(C_INT),INTENT(OUT) :: iSupplyTypeID,iStat
    
    iStat         = 0
    iSupplyTypeID = f_iSupply_ElemPump

  END SUBROUTINE IW_GetSupplyTypeID_ElemPump
  
  
  ! -------------------------------------------------------------
  ! --- GET ZONE EXTENT ID FOR HORIZONTAL ZONE DEFINITION
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetZoneExtentID_Horizontal(iZoneExtentID,iStat) BIND(C,NAME="IW_GetZoneExtentID_Horizontal")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetZoneExtentID_Horizontal
    INTEGER(C_INT),INTENT(OUT) :: iZoneExtentID,iStat
    
    iStat         = 0
    iZoneExtentID = f_iZoneHorizontal
    
  END SUBROUTINE IW_GetZoneExtentID_Horizontal
  
  
  ! -------------------------------------------------------------
  ! --- GET ZONE EXTENT ID FOR VERTICAL ZONE DEFINITION
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetZoneExtentID_Vertical(iZoneExtentID,iStat) BIND(C,NAME="IW_GetZoneExtentID_Vertical")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetZoneExtentID_Vertical
    INTEGER(C_INT),INTENT(OUT) :: iZoneExtentID,iStat
    
    iStat         = 0
    iZoneExtentID = f_iZoneVertical
    
  END SUBROUTINE IW_GetZoneExtentID_Vertical
  
  
  ! -------------------------------------------------------------
  ! --- GET ZONE EXTENT IDs 
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetZoneExtentIDs(iZoneExtentID_Horizontal,iZoneExtentID_Vertical,iStat) BIND(C,NAME="IW_GetZoneExtentIDs")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetZoneExtentIDs
    INTEGER(C_INT),INTENT(OUT) :: iZoneExtentID_Horizontal,iZoneExtentID_Vertical,iStat
    
    iStat                    = 0
    iZoneExtentID_Horizontal = f_iZoneHorizontal
    iZoneExtentID_Vertical   = f_iZoneVertical
    
  END SUBROUTINE IW_GetZoneExtentIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TIME INETRVALS WITH A GIVEN LENGTH BETWEEN TWO DATES
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetNIntervals(cBeginDateAndTime,cEndDateAndTime,iLenDateAndTime,cInterval,iLenInterval,NIntervals,iStat) BIND(C,NAME='IW_GetNIntervals')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetNIntervals
    INTEGER(C_INT),INTENT(IN)         :: iLenDateAndTime,iLenInterval
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cBeginDateAndTime(iLenDateAndTime),cEndDateAndTime(iLenDateAndTime),cInterval(iLenInterval)
    INTEGER(C_INT),INTENT(OUT)        :: NIntervals,iStat
    
    !Local variables
    INTEGER   :: DELTAT_InMinutes,ErrorCode
    REAL(8)   :: DeltaT
    CHARACTER :: cBeginDateAndTime_F*iLenDateAndTime,cEndDateAndTime_F*iLenDateAndTime,cInterval_F*iLenInterval
    
    !Initilaize
    iStat = 0
    
    !Convert C strings to Fortran strings
    CALL String_Copy_C_F(cBeginDateAndTime,cBeginDateAndTime_F)
    CALL String_Copy_C_F(cEndDateAndTime,cEndDateAndTime_F)
    CALL String_Copy_C_F(cInterval,cInterval_F)
    
    CALL CTimeStep_To_RTimeStep(cInterval_F,DeltaT,DELTAT_InMinutes,ErrorCode)
    
    IF (ErrorCode .NE. 0) THEN
        NIntervals = 0
    ELSE
        NIntervals = NPeriods(DELTAT_InMinutes,cBeginDateAndTime_F,cEndDateAndTime_F)
    END IF
    
  END SUBROUTINE IW_GetNIntervals
  
  
  ! -------------------------------------------------------------
  ! --- GET LAST ERROR MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE IW_GetLastMessage(iLen,cErrorMessage,iStat) BIND (C,NAME='IW_GetLastMessage')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_GetLastMessage
    INTEGER(C_INT),INTENT(IN)            :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(INOUT) :: cErrorMessage(iLen)
    INTEGER(C_INT),INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=iLen) :: cErrorMessage_F

    !Initialize
    iStat = 0
    
    !Get message
    CALL GetLastMessage(cErrorMessage_F)
    
    !Convert message to C
    CALL String_Copy_F_C(cErrorMessage_F, cErrorMessage)
    
  END SUBROUTINE IW_GetLastMessage
  
  
  
  
  ! -------------------------------------------------------------
  ! --- LOG LAST ERROR MESSAGE TO MESSAGE FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_LogLastMessage(iStat) BIND (C,NAME='IW_LogLastMessage')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_LogLastMessage
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    !Initialize
    iStat = 0
    
    !Log message
    CALL LogLastMessage()
    
  END SUBROUTINE IW_LogLastMessage
  
  
  
  
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
  ! --- SET THE STANDARD OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_SetLogFile(iLen,cFileName,iStat) BIND(C,NAME='IW_SetLogFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_SetLogFile
    INTEGER(C_INT),INTENT(IN)         :: iLen 
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cFileName(iLen)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=iLen) :: cFileName_F
    
    !Initialize
    iStat = 0
    
    !Return if no filename is specified
    IF (iLen .EQ. 0) RETURN
    
    !Convert C string to Fortran string
    CALL String_Copy_C_F(cFileName, cFileName_F)
    
    CALL SetLogFileName(cFileName_F,iStat)

  END SUBROUTINE IW_SetLogFile    
  
  
  
  
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
  ! --- INCREMENT TIME WITH AN INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE IW_IncrementTime(iLenDateAndTime,cDateAndTime,iLenInterval,cInterval,iNCount,iStat) BIND(C,NAME='IW_IncrementTime')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_IncrementTime
    INTEGER(C_INT),INTENT(IN)            :: iLenDateAndTime,iLenInterval,iNCount 
    CHARACTER(KIND=C_CHAR),INTENT(INOUT) :: cDateAndTime(iLenDateAndTime)
    CHARACTER(KIND=C_CHAR),INTENT(IN)    :: cInterval(iLenInterval)
    INTEGER(C_INT),INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER :: cDateAndTime_F*iLenDateAndTime,cDateAndTimeNew*iLenDateAndTime,cInterval_F*iLenInterval
    INTEGER   :: Interval_InMinutes
    REAL(8)   :: rDummy
    
    !Initialize
    iStat = 0
    
    !Convert C strings to Fortran strings
    CALL String_Copy_C_F(cDateAndTime , cDateAndTime_F)
    CALL String_Copy_C_F(cInterval , cInterval_F)
    
    !Make sure interval is recognized
    IF (IsTimeIntervalValid(cInterval_F) .EQ. 0) THEN
        CALL SetLastMessage(cInterval_F // ' is not a recognized time interval!',f_iFatal,'IWFM_DLL')
        iStat = -1
        RETURN
    END IF
    
    !Interval in minutes
    CALL CTimeStep_To_RTimeStep(cInterval_F,rDummy,Interval_InMinutes)
    
    !Increment time
    cDateAndTimeNew = IncrementTimeStamp(cDateAndTime_F,Interval_InMinutes,iNCount)
    
    !Convert Fortran string to C String
    CALL String_Copy_F_C(cDateAndTimeNew , cDateAndTime)

  END SUBROUTINE IW_IncrementTime    
  

    
  ! -------------------------------------------------------------
  ! --- CHECK IF A TIME STAMP IS GREATER THAN ANOTHER ONE
  ! -------------------------------------------------------------
  SUBROUTINE IW_IsTimeGreaterThan(iLenDateAndTime,cDateAndTime1,cDateAndTime2,isGreaterThan,iStat) BIND(C,NAME='IW_IsTimeGreaterThan')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_IsTimeGreaterThan
    INTEGER(C_INT),INTENT(IN)            :: iLenDateAndTime
    CHARACTER(KIND=C_CHAR),INTENT(INOUT) :: cDateAndTime1(iLenDateAndTime),cDateAndTime2(iLenDateAndTime)
    INTEGER(C_INT),INTENT(OUT)           :: isGreaterThan,iStat
    
    !Local variables
    CHARACTER :: cDateAndTime1_F*iLenDateAndTime,cDateAndTime2_F*iLenDateAndTime
    
    !Initialize
    iStat = 0
    
    !Convert C strings to Fortran strings
    CALL String_Copy_C_F(cDateAndTime1 , cDateAndTime1_F)
    CALL String_Copy_C_F(cDateAndTime2 , cDateAndTime2_F)
    
    !Compare the times
    IF (cDateAndTime1_F .TSGT. cDateAndTime2_F) THEN
        isGreaterThan = 1
    ELSE
        isGreaterThan = -1
    END IF
    
  END SUBROUTINE IW_IsTimeGreaterThan  
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO TEST PASSING INTEGER AND REAL SCALARS
  ! -------------------------------------------------------------
  SUBROUTINE fooScalar(iArg,dArg) BIND(C,NAME="fooScalar")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: fooScalar
    INTEGER(C_INT),INTENT(INOUT) :: iArg
    REAL(C_DOUBLE),INTENT(INOUT) :: dArg
    
    iArg = iArg * 2
    dArg = dArg * 2.0
    
   END SUBROUTINE fooScalar
  

  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO TEST PASSING INTEGER AND REAL 1-D ARRAYS
  ! -------------------------------------------------------------
  SUBROUTINE foo1DArray(iArrayDim,iArray,idArrayDim,dArray) BIND(C,NAME="foo1DArray")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: foo1DArray
    INTEGER(C_INT),INTENT(IN)    :: iArrayDim,idArrayDim
    INTEGER(C_INT),INTENT(INOUT) :: iArray(iArrayDim)
    REAL(C_DOUBLE),INTENT(INOUT) :: dArray(idArrayDim)
    
    iArray = 5
    dArray = 3.2d0
    
   END SUBROUTINE foo1DArray
  

  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO TEST PASSING INTEGER AND REAL 2-D ARRAYS
  ! -------------------------------------------------------------
  SUBROUTINE foo2DArray(iDim1,iDim2,iArray,idDim1,idDim2,dArray) BIND(C,NAME="foo2DArray")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: foo2DArray
    INTEGER(C_INT),INTENT(IN) :: iDim1,iDim2,idDim1,idDim2
    INTEGER(C_INT),INTENT(INOUT) :: iArray(iDim1,iDim2)
    REAL(C_DOUBLE),INTENT(INOUT) :: dArray(idDim1,idDim2)
    
    !Local variables
    INTEGER :: iRow
    
    DO iRow=1,iDim1
        iArray(iRow,:) = iRow
    END DO
    
    DO iRow=1,idDim1
        dArray(iRow,:) = iRow
    END DO
    
  END SUBROUTINE foo2DArray
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO TEST PASSING A STRING TO API
  ! -------------------------------------------------------------
  SUBROUTINE fooStrPassed(iLen,cStrPassed) BIND (C,NAME="fooStrPassed")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: fooStrPassed
    INTEGER(C_INT),INTENT(IN)         :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cStrPassed(iLen)
    
    !Local variable
    INTEGER               :: iStat
    CHARACTER(LEN=iLen)   :: cStrPassed_F
    TYPE(GenericFileType) :: aFile
    
    CALL String_Copy_C_F(cStrPassed , cStrPassed_F)
    
    CALL aFile%New(FileName='IW_API_Test.txt',InputFile=.FALSE.,iStat=iStat)
    CALL aFile%WriteData(cStrPassed_F)
    CALL aFile%Kill()
    
  END SUBROUTINE fooStrPassed


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO TEST RECEIVING A STRING FROM API
  ! -------------------------------------------------------------
  SUBROUTINE fooStrReceived(iLen,cStrRecvd) BIND (C,NAME="fooStrReceived")
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: fooStrReceived
    INTEGER(C_INT),INTENT(IN)          :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cStrRecvd(iLen)
    
    !Local variable
    CHARACTER(LEN=iLen) :: cStr_F  
    
    cStr_F = 'This is another test!'
    
    CALL String_Copy_F_C(cStr_F , cStrRecvd)
    
  END SUBROUTINE fooStrReceived

END MODULE