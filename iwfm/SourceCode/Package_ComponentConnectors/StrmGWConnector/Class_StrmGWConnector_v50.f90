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
!
!  This version of the stream-groundwater connector uses wetted perimeter
!  as a function of stream or groundwater head, whichever is greater, and
!  the conductivity values are conductance per length per width of the 
!  channel.
!
!***********************************************************************
MODULE Class_StrmGWConnector_v50
  USE MessageLogger              , ONLY: SetLastMessage          , &
                                         LogMessage              , &
                                         MessageArray            , &
                                         f_iWarn                 , &
                                         f_iFatal
  USE GeneralUtilities           , ONLY: IntToText               , &
                                         StripTextUntilCharacter , &
                                         CleanSpecialCharacters  , &
                                         ConvertID_To_Index
  USE IOInterface                , ONLY: GenericFileType 
  USE Package_Discretization     , ONLY: AppGridType             , &
                                         StratigraphyType
  USE Package_Misc               , ONLY: AbstractFunctionType    , &
                                         f_iStrmComp             , &
                                         f_iGWComp               , &
                                         f_rSmoothMaxP
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnectorType , &
                                         iDisconnectAtTopOfBed   , &
                                         iDisconnectAtBottomOfBed
  USE Package_Matrix             , ONLY: MatrixType              
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
  PUBLIC :: StrmGWConnector_v50_Type    
  
  
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseStrmGWConnectorType) :: StrmGWConnector_v50_Type
      PRIVATE
  CONTAINS
      PROCEDURE,PASS :: SetDisconnectElevations
      PROCEDURE,PASS :: ComputeStrmGWFlow_AtMinHead 
      PROCEDURE,PASS :: Simulate                    => StrmGWConnector_v50_Simulate
      PROCEDURE,PASS :: CompileConductance          => StrmGWConnector_v50_CompileConductance
  END TYPE StrmGWConnector_v50_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmGWConnector_v50::'
  
  
  
  
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
  ! --- COMPILE STREAM-GW CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v50_CompileConductance(Connector,InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
    CLASS(StrmGWConnector_v50_Type)   :: Connector
    TYPE(GenericFileType)             :: InFile
    TYPE(AppGridType),INTENT(IN)      :: AppGrid                                     !Not used in this version
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes),UpstrmNodes(:),DownstrmNodes(:)  !UpstrmNodes and DownstrmNodes are not used in this version
    REAL(8),INTENT(IN)                :: BottomElevs(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38)   :: ThisProcedure = ModName // 'StrmGWConnector_v50_CompileConductance'
    INTEGER                        :: indxNode,iNode,ErrorCode,iGWNode,iLayer,iStrmNodeID,iGWNodeID,iInteractionType,indxReach, &
                                      iUpstrmNode,iDownstrmNode,iGWUpstrmNode
    REAL(8)                        :: FACTK,FACTL,DummyArray(NStrmNodes,3),B_DISTANCE,F_DISTANCE,CA,CB
    REAL(8),DIMENSION(NStrmNodes)  :: Conductivity,BedThick
    CHARACTER                      :: ALine*500,TimeUnitConductance*6
    LOGICAL                        :: lProcessed(NStrmNodes)
    INTEGER,ALLOCATABLE            :: iGWNodes(:)
    CHARACTER(LEN=100),ALLOCATABLE :: cWorkArray(:)
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    CALL Connector%GetAllGWNodes(iGWNodes)
    
    !Read data
    !BACKWARD COMPATIBILITY: Check if there are 3 or 4 lines of entry
    CALL InFile%ReadData(cWorkArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (SIZE(cWorkArray) .EQ. 3) THEN
        READ (cWorkArray(1),*) FACTK
        ALine = cWorkArray(2)
        CALL CleanSpecialCharacters(ALine)
        TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
        READ(cWorkArray(3),*) FACTL
    ELSEIF (SIZE(cWorkArray) .EQ. 4) THEN
        READ (cWorkArray(1),*) FACTK
        ALine = cWorkArray(2)
        CALL CleanSpecialCharacters(ALine)
        TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
        READ(cWorkArray(3),*) FACTL
        READ(cWorkArray(4),*) iInteractionType
        CALL Connector%SetInteractionType(iInteractionType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ELSE
        CALL SetLastMessage('An error occured reading conversion factors for stream bed data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Initialize conductance
    DO indxNode=1,NStrmNodes
        iStrmNodeID = INT(DummyArray(indxNode,1))
        CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,iNode)
        IF (iNode .EQ. 0) THEN 
            CALL SetLastMessage('Stream node '//TRIM(IntToText(iStrmNodeID))//' listed for stream bed parameters is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (lProcessed(iNode)) THEN
            CALL SetLastMessage('Stream bed parameters for stream node '//TRIM(IntToText(iStrmNodeID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iNode)   = .TRUE.
        iGWNode             = iGWNodes(iNode)
        iLayer              = Connector%iLayer(iNode)
        Conductivity(iNode) = DummyArray(indxNode,2)*FACTK
        BedThick(iNode)     = DummyArray(indxNode,3)*FACTL
        IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
            IF (BottomElevs(iNode)-BedThick(iNode) .LT. Stratigraphy%BottomElev(iGWNode,iLayer)) THEN
                iGWNodeID       = AppGrid%AppNode(iGWNode)%ID
                BedThick(iNode) = BottomElevs(iNode) - Stratigraphy%BottomElev(iGWNode,iLayer)
                MessageArray(1) = 'Stream bed thickness at stream node ' // TRIM(IntToText(iStrmNodeID)) // ' and GW node '// TRIM(IntToText(iGWNodeID)) // ' penetrates into second active aquifer layer!'
                MessageArray(2) = 'It is adjusted to penetrate only into the top active layer.'
                CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure) 
            END IF
        END IF
        Conductivity(iNode) = Conductivity(iNode) / BedThick(iNode)
    END DO
    
    !Compute conductance by multiplying it with the length associated with the stream node (does not include wetted perimeter since it changes with stage dynamically)
    DO indxReach=1,SIZE(UpstrmNodes)
        iUpstrmNode   = UpstrmNodes(indxReach)
        iDownstrmNode = DownstrmNodes(indxReach)
        B_DISTANCE    = 0.0
        DO indxNode=iUpstrmNode+1,iDownstrmNode
            iGWUpstrmNode = iGWNodes(indxNode-1)
            iGWNode       = iGWNodes(indxNode)
            iLayer        = Connector%iLayer(indxNode)
            CA                        = AppGrid%X(iGWUpstrmNode) - AppGrid%X(iGWNode)
            CB                        = AppGrid%Y(iGWUpstrmNode) - AppGrid%Y(iGWNode)
            F_DISTANCE                = SQRT(CA*CA + CB*CB)/2d0
            Conductivity(indxNode-1)  = Conductivity(indxNode-1)*(F_DISTANCE+B_DISTANCE)
            B_DISTANCE                = F_DISTANCE
        END DO
        Conductivity(iDownstrmNode) = Conductivity(iDownstrmNode)*B_DISTANCE
    END DO
    
    !Allocate memory
    ALLOCATE (Connector%Conductance(NStrmNodes) , Connector%StrmGWFlow(NStrmNodes) , Connector%rBedThickness(NStrmNodes) , Connector%rDisconnectElev(NStrmNodes) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream-gw connection data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store information
    Connector%Conductance         = Conductivity
    Connector%rBedThickness       = BedThick
    Connector%TimeUnitConductance = TimeUnitConductance
    Connector%StrmGWFlow          = 0.0
    
    !Clear memory
    DEALLOCATE (iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE StrmGWConnector_v50_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- SET DISCONNECT ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE SetDisconnectElevations(Connector,rBottomElevs)
    CLASS(StrmGWConnector_v50_Type) :: Connector
    REAL(8),INTENT(IN)              :: rBottomElevs(:)
    
    IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
        Connector%rDisconnectElev = rBottomElevs - Connector%rBedThickness
    ELSE
        Connector%rDisconnectElev = rBottomElevs
    END IF
    
  END SUBROUTINE SetDisconnectElevations
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE STREAM-GW INTERACTION
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v50_Simulate(Connector,iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
    CLASS(StrmGWConnector_v50_Type)                 :: Connector
    INTEGER,INTENT(IN)                              :: iNNodes
    REAL(8),INTENT(IN)                              :: rGWHeads(:),rStrmHeads(:),rAvailableFlows(:) 
    TYPE(MatrixType)                                :: Matrix
    CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)                   
    REAL(8),OPTIONAL,INTENT(IN)                     :: rMaxElevs(:)     
    
    !Local variables
    INTEGER           :: indxStrm,iGWNode,iNodes_Connect(2)
    REAL(8)           :: rUnitConductance,rFractionForGW,rGWHead,rDiff_GW,rDiffGWSQRT,rWetPerimeter,rdWetPerimeter,   &
                         rConductance,rNodeAvailableFlow,rHeadDiff,rStrmGWFlow,rUpdateCOEFF_Keep(2),rUpdateCOEFF(2),  &
                         rUpdateRHS(2),rStrmGWFlowAdj,rStrmGWFlowAdjSQRT,rDStrmGWFlowAdj
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iStrmComp , f_iGWComp]
    
    !Initialize
    Connector%StrmGWFlow = 0.0
    
    !Compute stream-gw interaction at each stream node; also update the matrix equation
    DO indxStrm=1,SIZE(rStrmHeads)
        !Corresponding GW node
        iGWNode        = (Connector%iLayer(indxStrm)-1) * iNNodes + Connector%iGWNode(indxStrm)
        rFractionForGW = Connector%rFractionForGW(indxStrm)
      
        !Unit conductance
        rUnitConductance = Connector%Conductance(indxStrm)      !For this version of StrmGWConnector, original conductance does not include wetted perimeter
                
        !Head differences
        rGWHead     = MIN(rGWHeads(indxStrm) , rMaxElevs(indxStrm))
        rDiff_GW    = rGWHead - Connector%rDisconnectElev(indxStrm)
        rDiffGWSQRT = SQRT(rDiff_GW*rDiff_GW + f_rSmoothMaxP)
        
        !Wetted perimeter and conductance
        CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(MAX(rGWHead,rStrmHeads(indxStrm)),rWetPerimeter,rdWetPerimeter) 
        rConductance = rUnitConductance * rWetPerimeter
        
        !Available flow for node
        rNodeAvailableFlow = rAvailableFlows(indxStrm)
                
        !Calculate stream-gw interaction and update Jacobian
        !---------------------------------------------------
        rHeadDiff         = rStrmHeads(indxStrm) - MAX(rGWHead , Connector%rDisconnectElev(indxStrm))
        rStrmGWFlow       = rConductance * rHeadDiff
        iNodes_Connect(1) = indxStrm
        iNodes_Connect(2) = iGWNode

        !Stream is gaining; no need to worry about drying stream (i.e. stream-gw flow is not a function of upstream flows)
        !Also, WetPerimeter is a function of gw head
        IF (rStrmGWFlow .LT. 0.0) THEN
            Connector%StrmGWFlow(indxStrm) = rStrmGWFlow
            
            !Update Jacobian - entries for stream node 
            rUpdateCOEFF_Keep(1) = rConductance
            rUpdateCOEFF_Keep(2) = rUnitConductance * rdWetPerimeter * rHeadDiff - 0.5d0 * rConductance * (1d0+rDiff_GW/rDiffGWSQRT) 
            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Update Jacobian - entries for groundwater node
            rUpdateCOEFF = -rFractionForGW * rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
                                
        !Stream is losing; we need to limit stream loss to available flow
        !Also, WetPerimeter is a function of stream head
        ELSE
            Connector%StrmGWFlow(indxStrm) = MIN(rStrmGWFlow,rNodeAvailableFlow)
            rStrmGWFlowAdj                 = rNodeAvailableFlow - rStrmGWFlow
            rStrmGWFlowAdjSQRT             = SQRT(rStrmGWFlowAdj*rStrmGWFlowAdj + f_rSmoothMaxP)
            rDStrmGWFlowAdj                = 0.5d0 * (1d0 + rStrmGWFlowAdj / rStrmGWFlowAdjSQRT)
            
            !Update Jacobian - entries for stream node 
            rUpdateCOEFF_Keep(1) = (rConductance + rUnitConductance*rdWetPerimeter*rHeadDiff) * rDStrmGWFlowAdj
            rUpdateCOEFF_Keep(2) = -0.5d0 * rConductance * (1d0+rDiff_GW/rDiffGWSQRT) * rDStrmGWFlowAdj
            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Update Jacobian - entries for groundwater node
            rUpdateCOEFF = -rFractionForGW * rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
        END IF

        !Update RHS 
        rUpdateRHS(1) = Connector%StrmGWFlow(indxStrm)
        rUpdateRHS(2) = -Connector%StrmGWFlow(indxStrm) * rFractionForGW
        CALL Matrix%UpdateRHS(iCompIDs,iNodes_Connect,rUpdateRHS)
        
    END DO    
    
  END SUBROUTINE StrmGWConnector_v50_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION WITHOUT UPDATING MATRIX
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE ComputeStrmGWFlow_AtMinHead(Connector,rStrmBottomElev,rGWHead,rMaxElev,WetPerimeterFunction,rFlows)
    CLASS(StrmGWConnector_v50_Type)        :: Connector
    REAL(8),INTENT(IN)                     :: rStrmBottomElev(:),rGWHead(:),rMaxElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)       !This is the wetted perimeter function defined using Manning's formula 
    REAL(8),INTENT(OUT)                    :: rFlows(:)
    
    !Local variables
    INTEGER :: indxStrm
    REAL(8) :: rWetPerimeter,rConductance
    
    DO indxStrm=1,SIZE(rGWHead)
        
        !If gw head is below the stream bottom flow will be zero
        IF (rGWHead(indxStrm) .LE. rStrmBottomElev(indxStrm)) THEN
            rFlows(indxStrm) = 0.0
            CYCLE
        END IF
        
        !Wetted perimeter and conductance
        rWetPerimeter = WetPerimeterFunction(indxStrm)%Evaluate(MIN(rGWHead(indxStrm),rMaxElev(indxStrm)))
        rConductance  = Connector%Conductance(indxStrm) * rWetPerimeter
        
        !Stream-gw interaction at min stream head
        rFlows(indxStrm) = rConductance * (rStrmBottomElev(indxStrm) - MIN(rGWHead(indxStrm),rMaxElev(indxStrm))) 

    END DO
    
  END SUBROUTINE ComputeStrmGWFlow_AtMinHead

END MODULE