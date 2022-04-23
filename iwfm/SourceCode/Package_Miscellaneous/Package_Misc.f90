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
MODULE Package_Misc
  USE Class_Version              , ONLY: VersionType
  USE Class_GeneralHeadBoundary
  USE Class_PairedData           , ONLY: PairedDataType
  USE Class_SolverData
  USE Opening_screen
  USE TSDFileHandler
  USE AbstractFunction           , ONLY: AbstractFunctionType
  IMPLICIT NONE
    
  
  ! -------------------------------------------------------------
  ! --- MODEL COMPONENT IDENTIFIERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: nMaxComps      = 6  , &
                       iStrmComp      = 1  , &
                       iLakeComp      = 2  , &
                       iGWComp        = 3  , &
                       iRootZoneComp  = 4  , &
                       iUnsatZoneComp = 5  , &
                       iSWShedComp    = 6
  CHARACTER(LEN=16),PARAMETER :: cCompNames(nMaxComps) = ["STREAM" , "LAKE" , "GROUNDWATER" , "ROOT ZONE" , "UNSATURATED ZONE" , "SMALL WATERSHEDS"]
  
  
  ! -------------------------------------------------------------
  ! --- DESTINATION AS A GROUP OF ELEMENTS FOR A FLOW TERM
  ! -------------------------------------------------------------
  TYPE ElemGroupType
      INTEGER             :: NElems    = 0
      INTEGER,ALLOCATABLE :: iElems(:)
  END TYPE ElemGroupType
  
  
  ! -------------------------------------------------------------
  ! --- DESTINATION DATA TYPE FOR A FLOW TERM
  ! -------------------------------------------------------------
  TYPE FlowDestinationType
    INTEGER             :: iDestType   = 0    !Destination type
    INTEGER             :: iDest       = 0    !Destination ID number
    INTEGER             :: iDestRegion = 0    !Subregion that the destination belongs to
    TYPE(ElemGroupType) :: iDestElems         !List of elements as destination
  END TYPE FlowDestinationType
  
  
  ! -------------------------------------------------------------
  ! --- SURFACE FLOW DESTINATION TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: FlowDest_Outside    = 0  , &
                       FlowDest_StrmNode   = 1  , &
                       FlowDest_Element    = 2  , &
                       FlowDest_Lake       = 3  , &
                       FlowDest_Subregion  = 4  , &
                       FlowDest_GWElement  = 5  , &
                       FlowDest_ElementSet = 6
                       
                       
  ! -------------------------------------------------------------
  ! --- WATER SUPPLY TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: Supply_Diversion_Ag     = 1 , &
                       Supply_Diversion_Urb    = 2 , &
                       Supply_Pumping_Ag       = 3 , &
                       Supply_Pumping_Urb      = 4 , &
                       Supply_UpstrmElemRunoff = 5 , &
                       iSupply_Diversion       = 6 , &
                       iSupply_ElemPump        = 7 , &
                       iSupply_Well            = 8
  
  
  ! -------------------------------------------------------------
  ! --- LOCATION TYPES FOR PRE- AND POST-PROCESSING TOOLS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iLocationType_Subregion      = 1  , &
                       iLocationType_Zone           = 2  , &
                       iLocationType_Node           = 3  , &
                       iLocationType_GWHeadObs      = 4  , &
                       iLocationType_SubsidenceObs  = 5  , &
                       iLocationType_StrmReach      = 6  , &
                       iLocationType_StrmNode       = 7  , &
                       iLocationType_StrmHydObs     = 8  , &
                       iLocationType_Lake           = 9  , &
                       iLocationType_TileDrain      = 10 , &
                       iLocationType_SmallWatershed = 11 , &
                       iLocationType_Element        = 12 
  
  
  ! -------------------------------------------------------------
  ! --- FLAG THAT SAYS ALL LOCATION IDs ARE INCLUDED IN A LIST
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iAllLocationIDsListed = -1
  
  
  ! -------------------------------------------------------------
  ! --- DATA UNIT TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iDataUnitType_Length = 0  , &
                       iDataUnitType_Area   = 1  , &
                       iDataUnitType_Volume = 2 
  
  
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