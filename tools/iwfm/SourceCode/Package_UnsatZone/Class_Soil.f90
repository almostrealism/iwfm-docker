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
MODULE Class_Soil
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
  ! --- METHODS TO REPRESENT UNSATURATED HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iCampbell            = 1                         , &
                       f_ivanGenuchten        = 2                         , &
                       f_iKunsatMethodList(2) = [f_iCampbell , f_ivanGenuchten]   


  ! -------------------------------------------------------------
  ! --- SOIL DATA TYPE
  ! -------------------------------------------------------------
  TYPE SoilType
    REAL(8) :: TotalPorosity = 0.0
    REAL(8) :: Lambda        = 0.0              !Pore size distribution index
    REAL(8) :: HydCond       = 0.0              !Saturated hydraulic conductivity
    INTEGER :: KunsatMethod  = f_ivanGenuchten  !Method to represent unsaturated hydraulic conductivity as a function of moisture content
  END TYPE SoilType
  

  ! -------------------------------------------------------------
  ! --- ROOT ZONE SOIL DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(SoilType) :: RootZoneSoilType
    REAL(8) :: FieldCapacity = 0.0   
    REAL(8) :: WiltingPoint  = 0.0
  END TYPE RootZoneSoilType
  
END MODULE