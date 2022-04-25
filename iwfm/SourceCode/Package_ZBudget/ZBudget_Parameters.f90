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
MODULE ZBudget_Parameters
  USE Package_Budget  , ONLY: ModifiedAgSupplyReq       , &
                              f_iAR                     , &
                              f_iVR                     , &
                              f_iVR_lwu_PotCUAW         , &
                              f_iVR_lwu_AgSupplyReq     , &
                              f_iVR_lwu_AgPump          , &
                              f_iVR_lwu_AgDiv           , &
                              f_iVR_lwu_AgOthIn         , &
                              f_iVR_lwu_AgShort         , &
                              f_iVLB                    , &
                              f_iVLE                    , &
                              f_iColumnHeaderLen        , &
                              f_iMaxLocationNameLen
  IMPLICIT NONE
    
  PUBLIC 
  
  
  ! -------------------------------------------------------------
  ! --- CHARACTER VARIABLE LENGTHS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iMaxDataNameLen   = 60     !Maximum length for Z-Budget data names
  

  ! -------------------------------------------------------------
  ! --- FLOW TYPE FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iStorageType           = 1 , &
                       f_iVerticalFlowType      = 2 , &
                       f_iFaceFlowType          = 3 , &
                       f_iElemDataType          = 4 
  
  
  ! -------------------------------------------------------------
  ! --- TYPES OF ZONATION
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iZoneHorizontal = 1  , &
                       f_iZoneVertical   = 0 
  
  
  ! -------------------------------------------------------------
  ! --- UNDEFINED ZONE
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iUndefinedZone = -99
  

  ! -------------------------------------------------------------
  ! --- TEXT MARKERS TO MODIFY TITLES
  ! -------------------------------------------------------------
  CHARACTER(LEN=1),PARAMETER :: f_cMarkerChar       = '@' , &
                                f_cLengthUnitMarker = '1' , &
                                f_cAreaUnitMarker   = '2' , &
                                f_cVolumeUnitMarker = '3'
  
  
  ! -------------------------------------------------------------
  ! --- PATH FOR ATTRIBUTES AND HEADER
  ! -------------------------------------------------------------
  CHARACTER(LEN=11),PARAMETER :: f_cAttributesDir = '/Attributes'
  
     
END MODULE