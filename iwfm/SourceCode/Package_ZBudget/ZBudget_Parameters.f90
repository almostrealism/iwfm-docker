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
MODULE ZBudget_Parameters
  USE Package_Budget  , ONLY: ModifiedAgSupplyReq       , &
                              AR                        , &
                              VR                        , &
                              VR_lwu_PotCUAW            , &
                              VR_lwu_AgSupplyReq        , &
                              VR_lwu_AgPump             , &
                              VR_lwu_AgDiv              , &
                              VR_lwu_AgOthIn            , &
                              VR_lwu_AgShort            , &
                              VLB                       , &
                              VLE                       , &
                              ColumnHeaderLen           , &
                              MaxLocationNameLen
  IMPLICIT NONE
    
  PUBLIC 
  
  
  ! -------------------------------------------------------------
  ! --- CHARACTER VARIABLE LENGTHS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iMaxDataNameLen   = 60     !Maximum length for Z-Budget data names
  

  ! -------------------------------------------------------------
  ! --- FLOW TYPE FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iStorageType           = 1 , &
                       iVerticalFlowType      = 2 , &
                       iFaceFlowType          = 3 , &
                       iElemDataType          = 4 
  
  
  ! -------------------------------------------------------------
  ! --- TYPES OF ZONATION
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iZoneHorizontal = 1  , &
                       iZoneVertical   = 0 
  
  
  ! -------------------------------------------------------------
  ! --- UNDEFINED ZONE
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iUndefinedZone = -99
  

  ! -------------------------------------------------------------
  ! --- TEXT MARKERS TO MODIFY TITLES
  ! -------------------------------------------------------------
  CHARACTER(LEN=1),PARAMETER :: MarkerChar       = '@' , &
                                LengthUnitMarker = '1' , &
                                AreaUnitMarker   = '2' , &
                                VolumeUnitMarker = '3'
  
  
  ! -------------------------------------------------------------
  ! --- PATH FOR ATTRIBUTES AND HEADER
  ! -------------------------------------------------------------
  CHARACTER(LEN=11),PARAMETER :: cAttributesDir = '/Attributes'
  
     
END MODULE