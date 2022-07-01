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
MODULE Class_GenericLandUseGW
  USE Class_GenericLandUse  , ONLY: GenericLandUseType 
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
  PUBLIC :: GenericLandUseType    , &
            GenericLandUseGWType  , &
            ComputeETFromGW_Max
                                

  ! -------------------------------------------------------------
  ! --- ROOT ZONE CORE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: GenericLandUseGWType
      REAL(8) :: ETFromGW_Max    = 0.0     !Potential maximum inflow from groundwater (unit rate)
      REAL(8) :: ETFromGW_Actual = 0.0     !Actual inflow of groundwater into the root zone (volumetric rate)
  END TYPE GenericLandUseGWType

  
  
  
CONTAINS
    
    

    
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
  ! --- COMPUTE MAXIMUM POSSIBLE ET FROM GW 
  ! -------------------------------------------------------------
  SUBROUTINE ComputeETFromGW_Max(DepthToGW,Sy,RootDepth,CapillaryRise,Area,ETFromGW_Max)
    REAL(8),INTENT(IN)  :: DepthToGW(:),Sy(:),RootDepth(:),CapillaryRise(:),Area(:,:)
    REAL(8),INTENT(OUT) :: ETFromGW_Max(:,:)
    
    !Local variables
    INTEGER :: indxElem,indxLU,NLandUse
    REAL(8) :: rDGW,rSy,rRZ,rRise,rIntersect
    
    !Initialize
    NLandUse = SIZE(RootDepth)
    
    !Compute
    DO indxElem=1,SIZE(DepthToGW)
        rDGW  = DepthToGW(indxElem)
        rRise = CapillaryRise(indxElem) 
        rSy   = Sy(indxElem)
        DO indxLU=1,NLandUse
            !No ET from GW if land use area is zero
            IF (Area(indxLU,indxElem) .EQ. 0.0) THEN
                ETFromGW_Max(indxLU,indxElem) = 0.0
                CYCLE
            END IF
            
            !Root depth
            rRZ = RootDepth(indxLU)
            
            !GW is below root zone
            IF (rDGW .GT. rRZ) THEN
                !Capillary rise does not reach into root zone
                IF (rRise+rRZ .LE. rDGW) THEN
                    ETFromGW_Max(indxLU,indxElem) = 0.0
                    
                !Capillary rise extends into root zone 
                ELSE
                    !There is no capillary rise
                    IF (rRise .EQ. 0.0) THEN
                        ETFromGW_Max(indxLU,indxElem) = 0.0
                       
                    !Capillary rise does not exceed land surface
                    ELSEIF (rDGW .GT. rRise) THEN
                        rIntersect = rRZ - rDGW + rRise
                        ETFromGW_Max(indxLU,indxElem) = 0.5 * rIntersect * rIntersect * rSy / rRise
                    
                    !Capillary rise exceeds land surface
                    ELSE
                        ETFromGW_Max(indxLU,indxElem) = rRZ * rSy * (0.5 * rRZ + rRise - rDGW) / rRise
                    END IF
                END IF
                
            !GW is above root zone
            ELSE
                !Capillary rise does not exceed land surface
                IF (rDGW .GT. rRise) THEN
                    ETFromGW_Max(indxLU,indxElem) = 0.5 * rRise * rSy + rSy * (rRZ - rDGW)
                    
                !Capillary rise exceeds land surface
                ELSE
                    IF (rRise .EQ. 0.0) THEN
                        ETFromGW_Max(indxLU,indxElem) = rSy * (rRZ - rDGW)
                    ELSE
                        ETFromGW_Max(indxLU,indxElem) = 0.5 * rSy * (2.0 * rRise * rRZ - rDGW * rDGW) / rRise
                    END IF
                END IF
            END IF
            
        END DO
    END DO
    
  END SUBROUTINE ComputeETFromGW_Max

END MODULE