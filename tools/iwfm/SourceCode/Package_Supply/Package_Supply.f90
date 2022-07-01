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
MODULE Package_Supply
  USE MessageLogger               , ONLY: EchoProgress
  USE Package_Misc                , ONLY: f_iFlowDest_Element               , &
                                          f_iSupply_Diversion               , &
                                          f_iSupply_Pumping                 , &
                                          f_iAg                             , &
                                          f_iUrb
  USE Package_Discretization      , ONLY: AppGridType
  USE Package_AppGW               , ONLY: AppGWType
  USE Package_AppStream           , ONLY: AppStreamType
  USE Package_RootZone            , ONLY: RootZoneType
  USE Class_IrigFracFile          , ONLY: IrigFracFileType
  USE SupplyAdjustment            , ONLY: SupplyAdjustmentType              , &
                                          f_iAdjustNone                     , &
                                          f_iAdjustPump                     , &
                                          f_iAdjustDiver                    , &
                                          f_iAdjustPumpDiver                , &
                                          f_iAdjustForNone                  , &
                                          f_iAdjustForAg                    , &
                                          f_iAdjustForUrb                   , &
                                          f_iAdjustForAgUrb                            
  USE Package_ComponentConnectors , ONLY: SupplyDestinationConnectorType
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: Supply                                     ,  &
                                                       
            IrigFracFileType                           ,  &
            
            SupplyAdjustmentType                       ,  &
            f_iAdjustNone                              ,  &
            f_iAdjustPump                              ,  &
            f_iAdjustDiver                             ,  &
            f_iAdjustPumpDiver                         ,  &
            f_iAdjustForNone                           ,  &
            f_iAdjustForAg                             ,  &
            f_iAdjustForUrb                            ,  &
            f_iAdjustForAgUrb                            
  
           
            
  
  
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
    IF (iDemandCalcLocation .EQ. f_iFlowDest_Element) THEN
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
      CALL RootZone%SetSupply(pDiver_Ag,f_iSupply_Diversion,f_iAg) 
      CALL RootZone%SetSupply(pDiver_Urb,f_iSupply_Diversion,f_iUrb) 
    END IF
    
    !Applied water due to pumping
    IF (AppGW%IsPumpingDefined()) THEN
      CALL AppGW%GetSupply(WellDestConnector,ElemPumpDestConnector,pPump_Ag,pPump_Urb)
      CALL RootZone%SetSupply(pPump_Ag,f_iSupply_Pumping,f_iAg) 
      CALL RootZone%SetSupply(pPump_Urb,f_iSupply_Pumping,f_iUrb) 
    END IF
  
  END SUBROUTINE Supply
  

END MODULE