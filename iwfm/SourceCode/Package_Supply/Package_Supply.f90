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
MODULE Package_Supply
  USE MessageLogger
  USE Package_Misc
  USE Package_Discretization
  USE Package_AppGW               , ONLY: AppGWType
  USE Package_AppStream           , ONLY: AppStreamType
  USE Package_RootZone
  USE Class_IrigFracFile
  USE SupplyAdjustment
  USE Package_ComponentConnectors , ONLY: SupplyDestinationConnectorType
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: Supply                                     ,  &
                                                       
            IrigFracFileType                           ,  &
            
            SupplyAdjustmentType                       ,  &
            iAdjustNone                                ,  &
            iAdjustPump                                ,  &
            iAdjustDiver                               ,  &
            iAdjustPumpDiver                           ,  &
            iAdjustForNone                             ,  &
            iAdjustForAg                               ,  &
            iAdjustForUrb                              ,  &
            iAdjustForAgUrb                            
  
           
            
  
  
CONTAINS


  ! -------------------------------------------------------------
  ! --- DEFINE WATER SUPPLY TO EACH DEMAND LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE Supply(AppGrid,AppGW,AppStream,DiverDestConnector,WellDestConnector,ElemPumpDestConnector,RootZone)
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    TYPE(AppGWType),INTENT(IN)                      :: AppGW
    TYPE(AppStreamType),INTENT(IN)                  :: AppStream
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: DiverDestConnector,WellDestConnector,ElemPumpDestConnector
    TYPE(RootZoneType)                              :: RootZone
  
    !Local variables
    INTEGER                                       :: iDemandCalcLocation
    REAL(8),DIMENSION(AppGrid%NElements),TARGET   :: DiverToElem_Ag,DiverToElem_Urb, &
                                                     PumpToElem_Ag,PumpToElem_Urb
    REAL(8),DIMENSION(AppGrid%NSubregions),TARGET :: DiverToRegion_Ag,DiverToRegion_Urb, &
                                                     PumpToRegion_Ag,PumpToRegion_Urb
    REAL(8),POINTER                               :: pDiver_Ag(:),pDiver_Urb(:),pPump_Ag(:),pPump_Urb(:)
    
    !Inform user
    CALL EchoProgress('Compiling water supply')

    !Initialize 
    iDemandCalcLocation = RootZone%GetDemandCalcLocation()
    CALL RootZone%ZeroSupply()
    IF (iDemandCalcLocation .EQ. FlowDest_Element) THEN
        pDiver_Ag  => DiverToElem_Ag
        pDiver_Urb => DiverToElem_Urb
        pPump_Ag   => PumpToElem_Ag
        pPump_Urb  => PumpToElem_Urb
    ELSE
        pDiver_Ag  => DiverToRegion_Ag
        pDiver_Urb => DiverToRegion_Urb
        pPump_Ag   => PumpToRegion_Ag
        pPump_Urb  => PumpToRegion_Urb
    END IF
        
    !Get supply due to deliveries and pass it to root zone
    !NOTE: If supply adjustment is on for diversions, original data is over-writen by adjustment data
    !      since deliveries, fractions, etc do change during adjustment.
    IF (AppStream%GetNDiver() .GT. 0) THEN
      CALL AppStream%GetSupply(DiverDestConnector,pDiver_Ag,pDiver_Urb)
      CALL RootZone%SetSupply(pDiver_Ag,Supply_Diversion_Ag) 
      CALL RootZone%SetSupply(pDiver_Urb,Supply_Diversion_Urb) 
    END IF
    
    !Applied water due to pumping
    IF (AppGW%IsPumpingDefined()) THEN
      CALL AppGW%GetSupply(WellDestConnector,ElemPumpDestConnector,pPump_Ag,pPump_Urb)
      CALL RootZone%SetSupply(pPump_Ag,Supply_Pumping_Ag) 
      CALL RootZone%SetSupply(pPump_Urb,Supply_Pumping_Urb) 
    END IF
  
  END SUBROUTINE Supply
  

END MODULE