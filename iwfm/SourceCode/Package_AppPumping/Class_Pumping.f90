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
MODULE Class_Pumping
  USE Package_Misc                 , ONLY: FlowDestinationType                      , &
                                           f_iFlowDest_Outside                      , &
                                           f_iFlowDest_Element                      , &
                                           f_iFlowDest_Subregion                    , &
                                           f_iFlowDest_ElementSet
  USE Package_ComponentConnectors  , ONLY: SupplyType                               , &
                                           SupplyDestinationConnectorType           , &
                                           Supply_GetPurpose
  USE Package_Discretization       , ONLY: AppGridType                              , &
                                           StratigraphyType                         
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
  PUBLIC :: PumpingType                    , &
            GetPumpPurpose                 , &
            UpdatePumpValues               , &
            NormalizerFPumpColRaw          , &
            DistributePumpToNodes          , &
            ComputePumpActual              , &
            ComputerFPumpCol               , &
            f_iDestTypeArray               , &
            f_iDistTypeArray
  
  
  ! -------------------------------------------------------------
  ! --- PUMPING DELIVERY DESTINATION TYPES
  ! -------------------------------------------------------------
  !  *** Note: -1 represents pumping occuring in the same element the pumps are located
  INTEGER,PARAMETER ::f_iDestTypeArray(5) = [-1,f_iFlowDest_Outside,f_iFlowDest_Element,f_iFlowDest_Subregion,f_iFlowDest_ElementSet]
                       
                       
  ! -------------------------------------------------------------
  ! --- METHODS TO COMPUTE THE FRACTION TO DISTRIBUTE PUMPING DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iDist_UserFrac    = 0  , &
                       f_iDist_TotalArea   = 1  , &
                       f_iDist_AgUrbArea   = 2  , &
                       f_iDist_AgArea      = 3  , &
                       f_iDist_UrbArea     = 4  , &
                       f_iDistTypeArray(5) = [f_iDist_UserFrac    , &
                                              f_iDist_TotalArea   , &
                                              f_iDist_AgUrbArea   , &
                                              f_iDist_AgArea      , &
                                              f_iDist_UrbArea     ] 


  ! -------------------------------------------------------------
  ! --- PUMPING TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(SupplyType) :: PumpingType
      INTEGER             :: Element                 = 0                  !Element number where pumping occurs
      INTEGER             :: iColPump                = 0                  !Column number for pumping in pumping database
      INTEGER             :: iDistMethod             = f_iDist_UserFrac   !Method to compute rFPumpCol
      REAL(8)             :: rFPumpColRaw            = 1.0                !Initial fraction of pumping in column iColPump that applies to the pumping location; this is static
      REAL(8)             :: rFPumpCol               = 1.0                !Modified fraction (based on iDistMethod) of pumping in column iColPump that applies to the pumping location; this is dynamic
      REAL(8),ALLOCATABLE :: rLayerFactor(:)                              !Fraction of pumping that applies to an aquifer layer
      REAL(8),ALLOCATABLE :: rNodePumpRequired(:,:)                       !Required pumping at layers and nodes surrounding the element that pump is located in given for (node,layer); this value is computed once when required pumping is read in or computed via supply adjustment
      REAL(8),ALLOCATABLE :: rNodePumpActual(:,:)                         !Actual pumping at layers and nodes surrounding the element that pump is located in given for (node,layer); this value is computed at each NR iteration
      REAL(8)             :: PumpRead                = 0.0                !Pumping value read from the file
      INTEGER             :: iColPumpMax             = 0                  !Column number in the pumping database for maximum pumping
      REAL(8)             :: rFPumpMaxCol            = 0.0                !Fraction of maximum pumping in column iPumpMaxCol that applies to the pumping location         
      REAL(8)             :: PumpMax                 = -HUGE(0d0)         !Maximum pumping amount
  END TYPE PumpingType
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_Pumping::'
  
  
  
  
CONTAINS



    
  ! -------------------------------------------------------------
  ! --- GET THE PURPOSE OF PUMPING (IF IT SERVES AG, URBAN OR BOTH) BEFORE SUPPLY ADJUSTMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpPurpose(Pumping,iAgOrUrban,iStat)
    CLASS(PumpingType),INTENT(IN) :: Pumping(:)
    INTEGER,INTENT(OUT)           :: iAgOrUrban(:),iStat
    
    CALL Supply_GetPurpose(Pumping,iAgOrUrban,iStat)
    
  END SUBROUTINE GetPumpPurpose
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE PUMPING VALUES
  ! -------------------------------------------------------------
  SUBROUTINE UpdatePumpValues(PumpData,PumpingRates)
    CLASS(PumpingType) :: PumpData(:)
    REAL(8),INTENT(IN) :: PumpingRates(:)
    
    !Local variables
    INTEGER :: indxPump
    REAL(8) :: rPumpMax,rPump
    
    !Reset maximum pumping
    DO indxPump=1,SIZE(PumpData)
        PumpData(indxPump)%PumpMax = -HUGE(1d0)
    END DO
    
    !Process pumping rates
    DO indxPump=1,SIZE(PumpData)
        !Maximum pumping rate
        IF (PumpData(indxPump)%iColPumpMax .GT. 0) THEN
            rPumpMax = PumpingRates(PumpData(indxPump)%iColPumpMax)
            IF (rPumpMax .LE. 0.0) PumpData(indxPump)%PumpMax = rPumpMax * PumpData(indxPump)%rFPumpMaxCol
        END IF
        
        !Pumping rate
        IF (PumpData(indxPump)%iColPump .LE. 0) CYCLE
        rPump = PumpingRates(PumpData(indxPump)%iColPump) * PumpData(indxPump)%rFPumpCol
        IF (rPump .LT. 0.0) THEN
            IF (rPump .LT. PumpData(indxPump)%PumpMax) rPump = PumpData(indxPump)%PumpMax
        END IF
        PumpData(indxPump)%PumpRead = rPump
        !Supply can only be positive (assign pumping as supply, not injections)
        IF (rPump .LT. 0.0) THEN
            PumpData(indxPump)%SupplyRequired = -rPump
            PumpData(indxPump)%SupplyActual   = -rPump
        ELSE
            PumpData(indxPump)%SupplyRequired = 0.0
            PumpData(indxPump)%SupplyActual   = 0.0
        END IF
    END DO
            
  END SUBROUTINE UpdatePumpValues
  

  ! -------------------------------------------------------------
  ! --- NORMALIZE RAW PUMPING COLUMN DISTRIBUTION FACTORS
  ! -------------------------------------------------------------
  SUBROUTINE NormalizerFPumpColRaw(iColPump,rFPumpColRaw)
    INTEGER,INTENT(IN) :: iColPump(:)
    REAL(8)            :: rFPumpColRaw(:)
    
    !Local variables
    INTEGER             :: indx,indxColPump,ErrorCode,iMaxCol
    REAL(8),ALLOCATABLE :: W(:)
    
    !Largest column number referenced
    iMaxCol = MAXVAL(iColPump)
    
    !If no pumping columns are refered, return
    IF (iMaxCol .LE. 0) RETURN
    
    !Allocate memory for temporary work array
    ALLOCATE (W(iMaxCol))
    W = 0.0
    
    !Normalize
    DO indx=1,SIZE(iColPump)
        indxColPump = iColPump(indx)
        IF (indxColPump .LE. 0) CYCLE
        W(indxColPump) =  W(indxColPump) + rFPumpColRaw(indx)
    END DO
    DO indx=1,SIZE(iColPump)
        indxColPump = iColPump(indx)
        IF (indxColPump .LE. 0) CYCLE
        IF (W(indxColPump) .GT. 0.0) rFPumpColRaw(indx) = rFPumpColRaw(indx) / W(indxColPump)
    END DO
    
    !Free memory
    DEALLOCATE (W , STAT=ErrorCode)
    
  END SUBROUTINE NormalizerFPumpColRaw
   
  
  ! -------------------------------------------------------------
  ! --- COMPUTE PUMPING COLUMN DISTRIBUTION FACTORS
  ! -------------------------------------------------------------
  SUBROUTINE ComputerFPumpCol(Pumping,PumpDestConnector,AppGrid,iDemandCalcLocation,DestAgArea,DestUrbArea)
    CLASS(PumpingType)                              :: Pumping(:)
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: PumpDestConnector
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    INTEGER,INTENT(IN)                              :: iDemandCalcLocation
    REAL(8),INTENT(IN)                              :: DestAgArea(:),DestUrbArea(:)
        
    !Local variables
    INTEGER             :: indx,ErrorCode,iColPump,iDestTypeRead,iRegion,indxRegion,indxElem,iElem,iMaxCol
    REAL(8)             :: rValue,rFPumpColWork(SIZE(Pumping))
    REAL(8),ALLOCATABLE :: W(:)
    INTEGER,ALLOCATABLE :: iRegionArrayIn(:),iRegionArrayOut(:)
    
    !Allocate memory for temporary work array
    iMaxCol = -1
    DO indx=1,SIZE(Pumping)
        IF (Pumping(indx)%iColPump .GT. iMaxCol) iMaxCol = Pumping(indx)%iColPump
    END DO
    ALLOCATE (W(iMaxCol))
    W = 0.0
    
    !Compute new pumping column fractions
    SELECT CASE (iDemandCalcLocation)
        !Demand will be calculated at element level (the DestAgArea and DestUrbArea must be defined at element level)
        !------------------------------------------
        CASE (f_iFlowDest_Element)
            DO indx=1,SIZE(Pumping)
                iDestTypeRead = Pumping(indx)%Destination%iDestType
                iColPump      = Pumping(indx)%iColPump
                IF (iColPump .LE. 0) CYCLE
                
                !Compute new fractions based on distribution method
                SELECT CASE (Pumping(indx)%iDistMethod)
                    !Use the user-specified fractions
                    CASE (f_iDist_UserFrac)
                        rValue = 1.0
                        
                    !Use total area at destination
                    CASE (f_iDist_TotalArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = AppGrid%AppSubregion(iRegion)%Area
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxElem=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iElem  = PumpDestConnector%SupplyToDestination(indx)%iDests(indxElem)
                                rValue = rValue + AppGrid%AppElement(iElem)%Area
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF
                        
                    !Use developed area at destination
                    CASE (f_iDist_AgUrbArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = 0.0
                            DO indxElem=1,AppGrid%AppSubregion(iRegion)%NRegionElements
                                iElem  = AppGrid%AppSubregion(iRegion)%RegionElements(indxElem)
                                rValue = rValue + DestAgArea(iElem) + DestUrbArea(iElem)
                            END DO
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxElem=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iElem  = PumpDestConnector%SupplyToDestination(indx)%iDests(indxElem)
                                rValue = rValue + DestAgArea(iElem) + DestUrbArea(iElem)
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF
                        
                    !Use ag area at destination
                    CASE (f_iDist_AgArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = 0.0
                            DO indxElem=1,AppGrid%AppSubregion(iRegion)%NRegionElements
                                iElem  = AppGrid%AppSubregion(iRegion)%RegionElements(indxElem)
                                rValue = rValue + DestAgArea(iElem)
                            END DO
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxElem=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iElem  = PumpDestConnector%SupplyToDestination(indx)%iDests(indxElem)
                                rValue = rValue + DestAgArea(iElem)
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF

                    !Use urban area at destination
                    CASE (f_iDist_UrbArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = 0.0
                            DO indxElem=1,AppGrid%AppSubregion(iRegion)%NRegionElements
                                iElem  = AppGrid%AppSubregion(iRegion)%RegionElements(indxElem)
                                rValue = rValue + DestUrbArea(iElem)
                            END DO
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxElem=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iElem  = PumpDestConnector%SupplyToDestination(indx)%iDests(indxElem)
                                rValue = rValue + DestUrbArea(iElem)
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF

                END SELECT
                rFPumpColWork(indx) = Pumping(indx)%rFPumpColRaw * rValue
                W(iColPump)         = W(iColPump) + rFPumpColWork(indx)
            END DO
        
        !Demand will be calculated at subregion level (the AgArea and UrbArea must be defined at subregion level)
        !--------------------------------------------
        CASE (f_iFlowDest_Subregion)
            DO indx=1,SIZE(Pumping)
                iDestTypeRead = Pumping(indx)%Destination%iDestType
                iColPump      = Pumping(indx)%iColPump
                IF (iColPump .LE. 0) CYCLE
                
                !Compute new fractions based on distribution method
                SELECT CASE (Pumping(indx)%iDistMethod)
                    !Use the user-specified fractions
                    CASE (f_iDist_UserFrac)
                        rValue = 1.0
                        
                    !Use total area at destination
                    CASE (f_iDist_TotalArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = AppGrid%AppSubregion(iRegion)%Area
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxRegion=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iRegion = PumpDestConnector%SupplyToDestination(indx)%iDests(indxRegion)
                                rValue  = rValue + AppGrid%AppSubregion(iRegion)%Area
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF
                        
                    !Use developed area at destination
                    CASE (f_iDist_AgUrbArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = DestAgArea(iRegion) + DestUrbArea(iRegion)
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxRegion=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iRegion = PumpDestConnector%SupplyToDestination(indx)%iDests(indxRegion)
                                rValue  = rValue + DestAgArea(iRegion) + DestUrbArea(iRegion)
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF
                        
                    !Use ag area at destination
                    CASE (f_iDist_AgArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = DestAgArea(iRegion)
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxRegion=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iRegion = PumpDestConnector%SupplyToDestination(indx)%iDests(indxRegion)
                                rValue  = rValue + DestAgArea(iRegion)
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF
                        
                    !Use urban area at destination
                    CASE (f_iDist_UrbArea)
                        !Served destination is a subregion
                        IF (iDestTypeRead .EQ. f_iFlowDest_Subregion) THEN
                            iRegion = Pumping(indx)%Destination%iDest
                            rValue  = DestUrbArea(iRegion)
                        !Served destination is an element or element set
                        ELSEIF (iDestTypeRead .EQ. f_iFlowDest_Element   .OR.   iDestTypeRead .EQ. f_iFlowDest_ElementSet) THEN
                            rValue = 0.0
                            DO indxRegion=1,PumpDestConnector%SupplyToDestination(indx)%nDest
                                iRegion = PumpDestConnector%SupplyToDestination(indx)%iDests(indxRegion)
                                rValue  = rValue + DestUrbArea(iRegion)
                            END DO
                        !Served destination is outside 
                        ELSE
                            rValue = 1.0
                        END IF

                END SELECT
                rFPumpColWork(indx) = Pumping(indx)%rFPumpColRaw * rValue
                W(iColPump)         = W(iColPump) + rFPumpColWork(indx)
            END DO
        
    END SELECT
        
    !Normalize fractions
    DO indx=1,SIZE(Pumping)
        iColPump = Pumping(indx)%iColPump
        IF (iColPump .LE. 0) CYCLE
        IF (W(iColPump) .GT. 0.0) THEN
            Pumping(indx)%rFPumpCol = rFPumpColWork(indx) / W(iColPump)
        ELSE
            Pumping(indx)%rFPumpCol = 0.0
        END IF        
    END DO
        
    !Free memory
    DEALLOCATE (W , iRegionArrayIn , iRegionArrayOut , STAT=ErrorCode)

  END SUBROUTINE ComputerFPumpCol
  
  
  ! -------------------------------------------------------------
  ! --- DISTRIBUTE PUMPING TO NODES
  ! -------------------------------------------------------------
  SUBROUTINE DistributePumpToNodes(Pumping,AppGrid,Stratigraphy,HHydCond,HeadGW,NodePump)
    CLASS(PumpingType)                :: Pumping(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: HHydCond(:,:),HeadGW(:,:)
    REAL(8)                           :: NodePump(:,:)
    
    !Local variables
    INTEGER :: iElem,NVertex,indxVertex,indxLayer,NLayers,iNode,Vertex(4),indxPump, &
               NNodes
    REAL(8) :: rNodeFactor,rLayerFactor,PumpDist(4,Stratigraphy%NLayers),PumpTotal, &
               TopElev,BottomElev,Head,PumpRequired
    
    !Initialize
    NNodes  = AppGrid%NNodes
    NLayers = Stratigraphy%NLayers
    
    !Distribute pumping to nodes and layers
    Pump_Loop : DO indxPump=1,SIZE(Pumping)
                    Pumping(indxPump)%rNodePumpRequired   = 0.0
                    Pumping(indxPump)%rNodePumpActual     = 0.0
                    iElem                                 = Pumping(indxPump)%Element
                    PumpRequired                          = 0.0
                    !Pumping or recharge?
                    IF (Pumping(indxPump)%SupplyRequired .GT. 0.0) THEN
                        PumpRequired = -Pumping(indxPump)%SupplyRequired                                    !Pumping
                    ELSE
                        IF (Pumping(indxPump)%PumpRead .GT. 0.0) PumpRequired = Pumping(indxPump)%PumpRead  !Recharge
                    END IF
                    NVertex   = AppGrid%NVertex(iElem)
                    Vertex    = AppGrid%Vertex(:,iElem)
                    PumpDist  = 0.0
                    PumpTotal = 0.0
    Layer_Loop:     DO indxLayer=1,NLayers
                        rLayerFactor = Pumping(indxPump)%rLayerFactor(indxLayer)
    Node_Loop :         DO indxVertex=1,NVertex
                            rNodeFactor = AppGrid%AppElement(iElem)%VertexAreaFraction(indxVertex)
                            iNode       = Vertex(indxVertex)
                            TopElev     = Stratigraphy%TopElev(iNode,indxLayer)
                            BottomElev  = Stratigraphy%BottomElev(iNode,indxLayer)
                            Head        = HeadGW(iNode,indxLayer)
                            
                            !If inactive node, cycle
                            IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) CYCLE
                            
                            !Recharge
                            IF (PumpRequired .GT. 0.0) THEN
                                PumpDist(indxVertex,indxLayer) = rNodeFactor * rLayerFactor * HHydCond(iNode,indxLayer)*(TopElev-BottomElev)
                            
                            !Pumping
                            ELSE
                                IF (Head .GT. BottomElev) &
                                    PumpDist(indxVertex,indxLayer) = rNodeFactor * rLayerFactor * HHydCond(iNode,indxLayer)*(MIN(Head,TopELev) - BottomElev)
                            END IF
                        END DO Node_Loop
                    END DO Layer_Loop
                    
                    !Total of distribution fractions
                    PumpTotal = SUM(PACK(PumpDist,MASK=.TRUE.))
                    
                    !Now, compute pumping at each node
                    IF (PumpTotal .GT. 0.0) THEN
                        DO indxLayer=1,NLayers
                            DO indxVertex=1,NVertex
                                iNode                                                     = Vertex(indxVertex)
                                Pumping(indxPump)%rNodePumpRequired(indxVertex,indxLayer) = PumpRequired * PumpDist(indxVertex,indxLayer) / PumpTotal
                                Pumping(indxPump)%rNodePumpActual(indxVertex,indxLayer)   = Pumping(indxPump)%rNodePumpRequired(indxVertex,indxLayer)
                                NodePump(iNode,indxLayer)                                 = NodePump(iNode,indxLayer) + PumpRequired*PumpDist(indxVertex,indxLayer)/PumpTotal
                            END DO
                        END DO
                    ELSE
                        Pumping(indxPump)%SupplyActual = 0.0
                    END IF
                END DO Pump_Loop

  END SUBROUTINE DistributePumpToNodes
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ACTUAL WELL/ELEMENT PUMPING AND FACTORS TO DISTRIBUTE ACTUAL PUMPING TO NODES
  ! -------------------------------------------------------------
  SUBROUTINE ComputePumpActual(Pumping,AppGrid,NLayers,rNodalPumpActual,rNodalPumpRequired)
    CLASS(PumpingType)           :: Pumping(:)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: NLayers
    REAL(8),INTENT(IN)           :: rNodalPumpActual(:,:),rNodalPumpRequired(:,:)
    
    !Local variables
    INTEGER :: indxPump,iElem,NVertex,Vertex(4),indxNode,indxLayer,iNode
    REAL(8) :: rSupplyRequired,rNodePumpDelta,rPumpNode,rPumpDecrease
    
    !Loop over pumping locations
    DO indxPump=1,SIZE(Pumping)
        !Required pumping
        rSupplyRequired = SUM(Pumping(indxPump)%rNodePumpRequired) !Note that sum of rNodePumpRequired can be less than Pumping(indxPump)%SupplyRequired because there may not be enough water at nodes to service SupplyRequirement
        
        !Cycle if it is a recharge (recharge is never modified)
        IF (rSupplyRequired .GE. 0.0) CYCLE
        
        !Element information
        iElem   = Pumping(indxPump)%Element
        NVertex = AppGrid%NVertex(iElem)
        Vertex  = AppGrid%Vertex(:,iElem)
        
        !Loop over nodes and layers, and reduce pumping based on reductions at the node/layer level
        Pumping(indxPump)%SupplyActual    = -rSupplyRequired
        Pumping(indxPump)%rNodePumpActual = Pumping(indxPump)%rNodePumpRequired
        DO indxLayer=1,NLayers
            DO indxNode=1,NVertex
                iNode          = Vertex(indxNode)
                rNodePumpDelta = rNodalPumpRequired(iNode,indxLayer) - rNodalPumpActual(iNode,indxLayer) 
                IF (rNodePumpDelta .NE. 0.0) THEN
                    rPumpNode                                             = Pumping(indxPump)%rNodePumpRequired(indxNode,indxLayer)
                    IF (rPumpNode .EQ. 0.0) CYCLE
                    rPumpDecrease                                         = rNodePumpDelta * rPumpNode / rNodalPumpRequired(iNode,indxLayer)
                    Pumping(indxPump)%rNodePumpActual(indxNode,indxLayer) = Pumping(indxPump)%rNodePumpActual(indxNode,indxLayer) - rPumpDecrease
                    Pumping(indxPump)%SupplyActual                        = Pumping(indxPump)%SupplyActual + rPumpDecrease
                END IF
            END DO            
        END DO
        
        !To protect against round-off errors
        Pumping(indxPump)%SupplyActual = MAX(Pumping(indxPump)%SupplyActual , 0d0)
        
    END DO
  
  END SUBROUTINE ComputePumpActual
  
END MODULE