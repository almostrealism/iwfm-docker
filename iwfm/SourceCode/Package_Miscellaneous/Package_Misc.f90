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
MODULE Package_Misc
  USE Class_Version              , ONLY: VersionType
  USE Class_GeneralHeadBoundary  , ONLY: GeneralHeadBoundaryType
  USE Class_PairedData           , ONLY: PairedDataType
  USE Class_SolverData           , ONLY: SolverDataType
  USE Opening_screen             , ONLY: PRINT_SCREEN            ,  &
                                         GET_MAIN_FILE   
  USE TSDFileHandler             , ONLY: IntTSDataInFileType     ,  &
                                         RealTSDataInFileType    ,  &
                                         Real2DTSDataInFileType  ,  &
                                         PrepareTSDOutputFile    ,  &
                                         ReadTSData
  USE AbstractFunction           , ONLY: AbstractFunctionType
  USE Class_BaseHydrograph       , ONLY: BaseHydrographType     , &
                                         HydOutputType          , &
                                         f_iHyd_AtXY            , &
                                         f_iHyd_AtNode          , &
                                         f_iHyd_GWHead          , &
                                         f_iHyd_Subsidence
  USE Class_TecplotOutput        , ONLY: TecplotOutputType
  IMPLICIT NONE
  
  
  PUBLIC 
  
  
  ! -------------------------------------------------------------
  ! --- MODEL COMPONENT IDENTIFIERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iStrmComp      = 1  , &
                                 f_iLakeComp      = 2  , &
                                 f_iGWComp        = 3  , &
                                 f_iRootZoneComp  = 4  , &
                                 f_iUnsatZoneComp = 5  , &
                                 f_iSWShedComp    = 6
  INTEGER,PARAMETER,PRIVATE   :: f_iNMaxComps     = 6  
  CHARACTER(LEN=16),PARAMETER :: f_cCompNames(f_iNMaxComps) = ["STREAM" , "LAKE" , "GROUNDWATER" , "ROOT ZONE" , "UNSATURATED ZONE" , "SMALL WATERSHEDS"]
  
  
  ! -------------------------------------------------------------
  ! --- DESTINATION AS A GROUP OF ELEMENTS FOR A FLOW TERM
  ! -------------------------------------------------------------
  TYPE ElemGroupType
      INTEGER             :: ID        = 0
      INTEGER             :: NElems    = 0
      INTEGER,ALLOCATABLE :: iElems(:)
  END TYPE ElemGroupType
  
  
  ! -------------------------------------------------------------
  ! --- DESTINATION DATA TYPE FOR A FLOW TERM
  ! -------------------------------------------------------------
  TYPE FlowDestinationType
    INTEGER             :: iDestType   = 0    !Destination type
    INTEGER             :: iDest       = 0    !Destination index number
    INTEGER             :: iDestRegion = 0    !Subregion that the destination belongs to
    TYPE(ElemGroupType) :: iDestElems         !List of elements as destination
  END TYPE FlowDestinationType
  
  
  ! -------------------------------------------------------------
  ! --- SURFACE FLOW DESTINATION TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iFlowDest_Outside    = 0  , &
                       f_iFlowDest_StrmNode   = 1  , &
                       f_iFlowDest_Element    = 2  , &
                       f_iFlowDest_Lake       = 3  , &
                       f_iFlowDest_Subregion  = 4  , &
                       f_iFlowDest_GWElement  = 5  , &
                       f_iFlowDest_ElementSet = 6
                       
                       
  ! -------------------------------------------------------------
  ! --- LOCATION TYPES FOR PRE- AND POST-PROCESSING TOOLS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iLocationType_StrmNode       = 1  , &
                       f_iLocationType_Element        = 2  , &
                       f_iLocationType_Lake           = 3  , &
                       f_iLocationType_Subregion      = 4  , &
!                      f_iLocationType_GWElement      = 5  , &  //These location types
!                      f_iLocationType_ElementSet     = 6  , &  // are not used
                       f_iLocationType_Zone           = 7  , &
                       f_iLocationType_Node           = 8  , &
                       f_iLocationType_GWHeadObs      = 9  , &
                       f_iLocationType_SubsidenceObs  = 10 , &
                       f_iLocationType_StrmReach      = 11 , &
                       f_iLocationType_StrmHydObs     = 12 , &
                       f_iLocationType_TileDrainObs   = 13 , &  !Tile drain with a hydrograph print-out
                       f_iLocationType_SmallWatershed = 14 , &
                       f_iLocationType_Bypass         = 15 , &
                       f_iLocationType_Diversion      = 16 , &
                       f_iLocationType_TileDrain      = 17 , &  !Any tile drain, with or without hydrograph print-out
                       f_iLocationType_StrmNodeBud    = 18
 
  
  ! -------------------------------------------------------------
  ! --- FLAG THAT SAYS ALL LOCATION IDs ARE INCLUDED IN A LIST
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iAllLocationIDsListed = -1
  
  
  ! -------------------------------------------------------------
  ! --- WATER SUPPLY TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iSupply_UpstrmElemRunoff = 5 , &
                       f_iSupply_Diversion        = 6 , &
                       f_iSupply_ElemPump         = 7 , &
                       f_iSupply_Well             = 8 , &
                       f_iSupply_Pumping          = 3      !Defines general pumping independent of well or element pumping
                    
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS FOR LAND USE TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iAg          = 1 , &   !Flag to specify that some entity is for General Ag
                       f_iUrb         = 2 , &   !Flag to specify that some entity is for Urban
                       f_iNonPondedAg = 3 , &   !Flag to specify that some entity is for Non-ponded Ag
                       f_iRice        = 4 , &   !Flag to specify that some entity is for Rice
                       f_iRefuge      = 5 , &   !Flag to specify that some entity is for Refuge
                       f_iNVRV        = 6       !Flag to specify that some entity is for Native & Riparian Veg.
  
  
  ! -------------------------------------------------------------
  ! --- DATA UNIT TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iDataUnitType_Length = 0  , &
                       f_iDataUnitType_Area   = 1  , &
                       f_iDataUnitType_Volume = 2 
  
  
  ! -------------------------------------------------------------
  ! --- PARAMETERS USED IN SMOOTHING
  ! -------------------------------------------------------------
  REAL(8),PARAMETER :: f_rSmoothMaxP  = 1d-4  , &  !Parameter to smooth max function
                       f_rSmoothStepP = 10d0       !Parameter to smooth step function
  
  
  ! -------------------------------------------------------------
  ! --- VERSION RELEATED DATA
  ! -------------------------------------------------------------
  INTEGER,PRIVATE,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PRIVATE,PARAMETER :: cVersion ='4.0.0000'
  INCLUDE 'Package_Misc_Revision.fi'


  
  
CONTAINS
    

    
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBERS
  ! -------------------------------------------------------------
  FUNCTION Package_Misc_GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(VersionType) :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) 
    
  END FUNCTION Package_Misc_GetVersion

END MODULE Package_Misc