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
MODULE GWZBudget_Parameters
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- FLOW ID NUMBERS FOR USE IN GW ZONE BUDGET OUTPUT
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: NFlowID             = 18 , & !Total number of flow id's
                       StorageID           = 1  , & !Storage
                       StrmGWXID           = 2  , & !Stream-aquifer interaction
                       TileDrainID         = 3  , & !Tile drain
                       SubIrigID           = 4  , & !Subsurface irrigation
                       SubsidenceID        = 5  , & !Subsidence
                       DeepPercID          = 6  , & !Deep perc
                       FlowBCID            = 7  , & !Specified flow b.c.
                       HeadBCID            = 8  , & !Specified head b.c.
                       GenHeadBCID         = 9  , & !General head b.c.
                       ConstGenHeadBCID    = 10 , & !Constrained general head boundary condition
                       SmallWShedBaseFlowID= 11 , & !Baseflow from small watersheds
                       SmallWShedPercID    = 12 , & !Percolation from creeks from small watersheds
                       DivRecoverLossID    = 13 , & !Recoverable loss from diversions
                       BypassRecoverLossID = 14 , & !Recoverable loss from bypasses
                       LakeGWXID           = 15 , & !Lake-aquifer interaction
                       ElemPumpID          = 16 , & !Element pumping/recharge
                       WellPumpID          = 17 , & !Well pumping/recharge
                       FlowToRootZoneID    = 18     !ET from groundwater
  
  
  ! -------------------------------------------------------------
  ! --- FLOW NAMES FOR USE IN GW ZONE BUDGET OUTPUT
  ! -------------------------------------------------------------
  CHARACTER(LEN=27),PARAMETER :: FlowNames(NFlowID) = ['GW Storage                 '  , &              
                                                       'Streams                    '  , &      
                                                       'Tile Drains                '  , &      
                                                       'Subsurface Irrigation      '  , &      
                                                       'Subsidence                 '  , &      
                                                       'Deep Percolation           '  , &      
                                                       'Specified Flow BC          '  , &      
                                                       'Specified Head BC          '  , &      
                                                       'General Head BC            '  , &  
                                                       'Constrained General Head BC'  , &
                                                       'Small Watershed Baseflow   '  , &      
                                                       'Small Watershed Percolation'  , &      
                                                       'Diversion Recoverable Loss '  , &      
                                                       'Bypass Recoverable Loss    '  , &      
                                                       'Lakes                      '  , &      
                                                       'Pumping by Element         '  , &      
                                                       'Pumping by Well            '  , &
                                                       'Root Water Uptake          '  ]
                

  ! -------------------------------------------------------------
  ! --- FLOW NAMES FOR USE IN DSS OUTPUT
  ! -------------------------------------------------------------
  CHARACTER(LEN=20),PARAMETER :: DSSFParts(NFlowID) = ['STORAGE             ' , &              
                                                       'STREAMS             ' , &      
                                                       'TILE_DRAINS         ' , &      
                                                       'SUB_IRRIG           ' , &      
                                                       'SUBSIDENCE          ' , &      
                                                       'DEEP_PERC           ' , &      
                                                       'FLOW_BC             ' , &      
                                                       'HEAD_BC             ' , &      
                                                       'GENERAL_HEAD_BC     ' , &
                                                       'CNSTRT_GENRL_HEAD_BC' , &
                                                       'SMALL_WSHED_BASEFLOW' , &      
                                                       'SMALL_WSHED_PERC    ' , &      
                                                       'DIVER_RCVRBL_LOSS   ' , &      
                                                       'BYPASS_RCVRBL_LOSS  ' , &      
                                                       'LAKES               ' , &      
                                                       'ELEM_PUMP           ' , &      
                                                       'WELL_PUMP           ' , &
                                                       'FLOW_TO_ROOT_ZONE   ' ]
  
END MODULE