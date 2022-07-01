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
MODULE Class_GenericLandUse
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
  PUBLIC :: GenericLandUseType 
                                

  ! -------------------------------------------------------------
  ! --- ROOT ZONE CORE DATA TYPE
  ! -------------------------------------------------------------
  TYPE GenericLandUseType
    REAL(8) :: SMax                         = 0.0     !Maximum soil retention parameter
    INTEGER :: iColETc                      = 0       !Column number in the ETc database
    REAL(8) :: ETa                          = 0.0     !Actual ET
    REAL(8) :: Runoff                       = 0.0     !Direct runoff due to precip
    REAL(8) :: PrecipInfilt                 = 0.0     !Infiltration due to precipitation
    REAL(8) :: SoilM_Precip_P_BeforeUpdate  = 0.0     !Soil moisture at the beginning of time step due to precipitation, but before it is possibly updated due to land use area change
    REAL(8) :: SoilM_Precip_P               = 0.0     !Soil moisture at the beginning of time step due to precipitation
    REAL(8) :: SoilM_Precip                 = 0.0     !Soil moisture at the end of time step due to precipitation
    REAL(8) :: SoilM_AW_P_BeforeUpdate      = 0.0     !Soil moisture at the beginning of time step due to irrigation, but before it is possibly updated due to land use area change
    REAL(8) :: SoilM_AW_P                   = 0.0     !Soil moisture at the beginning of time step due to irrigation
    REAL(8) :: SoilM_AW                     = 0.0     !Soil moisture at the end of time step due to irrigation
    REAL(8) :: SoilM_Oth_P_BeforeUpdate     = 0.0     !Soil moisture at the beginning of time step due to user-defined source of water, but before it is possibly updated due to land use area change
    REAL(8) :: SoilM_Oth_P                  = 0.0     !Soil moisture at the beginning of time step due to user-defined source of water
    REAL(8) :: SoilM_Oth                    = 0.0     !Soil moisture at the end of time step due to user-defined source of water
    REAL(8) :: SoilMCh                      = 0.0     !Soil moisture chnage due to contraction/expansion of land use area
    REAL(8) :: Area_P                       = 0.0     !Land use area before update from land use area data file at the beginning of time step
    REAL(8) :: Area                         = 0.0     !Land use area after update from land use area data file at the beginning of time step
    REAL(8) :: Perc                         = 0.0     !Percolation
    REAL(8) :: PercCh                       = 0.0     !Percolation due to soil moisture exceeding total porosity when land use areas are modified and soil moisture redistributed
    REAL(8) :: GMExcess                     = 0.0     !Excess general moisture inflow due to stoarge capacity of the root zone
  END TYPE GenericLandUseType
        

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_GenericLandUse::'
  
  
  

  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

    


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************


END MODULE