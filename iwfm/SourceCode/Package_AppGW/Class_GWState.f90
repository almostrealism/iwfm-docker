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
MODULE Class_GWState
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
  PUBLIC :: GWStateType
  
  
  ! -------------------------------------------------------------
  ! --- GW STATE DATA TYPE
  ! -------------------------------------------------------------
  TYPE GWStateType
      REAL(8) :: Head           = 0.0    !Groundwater head at current time step
      REAL(8) :: Head_P         = 0.0    !Groundwater head at the previous time step
      REAL(8) :: Vx             = 0.0    !Groundwater velocity in x-direction
      REAL(8) :: Vy             = 0.0    !Groundwater velocity in y-direction
      REAL(8) :: Vz             = 0.0    !Groundwater velocity in z-direction
      REAL(8) :: Storativity_P  = 0.0    !Storage coefficient used at the previous time step
  END TYPE GWStateType
  
  
END MODULE