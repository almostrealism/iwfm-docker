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
MODULE Class_BaseTileDrain
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
  PUBLIC
  
  
  ! -------------------------------------------------------------
  ! --- GENERIC TILE DRAIN TYPE
  ! -------------------------------------------------------------
  TYPE BaseTileDrainType
      INTEGER :: ID           = 0
      INTEGER :: iGWNode      = 0
      INTEGER :: iGWNodeLayer = 0
      REAL(8) :: rElevation   = 0.0
      REAL(8) :: rConductance = 0.0
      REAL(8) :: rFlow        = 0.0
  END TYPE BaseTileDrainType 

  
END MODULE