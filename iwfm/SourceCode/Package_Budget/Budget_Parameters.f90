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
MODULE Budget_Parameters
  IMPLICIT NONE

  PUBLIC

  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR COLUMNS, MAX TITLE LENGTH FOR ASCII FILE OUTPUTS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iVR                  = 1   , &     !Volumetric rate
                       f_iVLB                 = 2   , &     !Volume at the beginning of time step
                       f_iVLE                 = 3   , &     !Volume at the end of time step
                       f_iAR                  = 4   , &     !Area
                       f_iLT                  = 5   , &     !Length
                       f_iVR_lwu_PotCUAW      = 6   , &     !Volumetric rate - specific for Potentail CUAW column in Land & Water Use Budget
                       f_iVR_lwu_AgSupplyReq  = 7   , &     !Volumetric rate - specific for Agricultural Supply Requirement column in Land & Water Use Budget
                       f_iVR_lwu_AgShort      = 8   , &     !Volumetric rate - specific for Agricultural Shortage column in Land & WAter Use Budget
                       f_iVR_lwu_AgPump       = 9   , &     !Volumetric rate - specific for Agricultural Pumping column in Land & WAter Use Budget
                       f_iVR_lwu_AgDiv        = 10  , &     !Volumetric rate - specific for Agricultural Diversions column in Land & WAter Use Budget
                       f_iVR_lwu_AgOthIn      = 11  , &     !Volumetric rate - specific for Agricultural Other Inflows column in Land & WAter Use Budget
                       f_iMaxTitleLen         = 1000, &
                       f_iFormatSpecLen       = 500 , &
                       f_iMaxLocationNameLen  = 100 , &
                       f_iColumnHeaderLen     = 100 , &
                       f_iBudgetDescriptorLen = 100 , &
                       f_iPathNameLen         = 80  , &
                       f_iDSSDataUnitLen      = 8
  INTEGER,PARAMETER :: f_iPER_CUM         = 1   , &
                       f_iPER_AVER        = 2
  CHARACTER(LEN=8),PARAMETER :: f_cDataTypes(2)       = ['PER-CUM ','PER-AVER']
  CHARACTER(LEN=9),PARAMETER :: f_cLocationNameMarker = '@LOCNAME@'
  CHARACTER(LEN=6),PARAMETER :: f_cAreaMarker         = '@AREA@'
  CHARACTER(LEN=8),PARAMETER :: f_cLengthUnitMarker   = '@UNITLT@'
  CHARACTER(LEN=8),PARAMETER :: f_cAreaUnitMarker     = '@UNITAR@'
  CHARACTER(LEN=8),PARAMETER :: f_cVolumeUnitMarker   = '@UNITVL@'

END MODULE
