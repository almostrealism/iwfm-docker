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
MODULE Class_StrmGWConnector_v40
  USE MessageLogger              , ONLY: SetLastMessage           , &
                                         LogMessage               , &
                                         MessageArray             , &
                                         f_iWarn                  , &
                                         f_iFatal
  USE IOInterface                , ONLY: GenericFileType 
  USE GeneralUtilities           , ONLY: StripTextUntilCharacter  , &
                                         IntToText                , &
                                         CleanSpecialCharacters   , &
                                         ConvertID_To_Index
  USE Package_Discretization
  USE Package_Misc               , ONLY: AbstractFunctionType     , &
                                         f_iStrmComp              , &
                                         f_iGWComp                , &
                                         f_rSmoothMaxP
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnectorType  , &
                                         iDisconnectAtTopOfBed    , &
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
  PUBLIC :: StrmGWConnector_v40_Type                    
  
  
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseStrmGWConnectorType) :: StrmGWConnector_v40_Type
      PRIVATE
  CONTAINS
      PROCEDURE,PASS :: Simulate           => StrmGWConnector_v40_Simulate
      PROCEDURE,PASS :: CompileConductance => StrmGWConnector_v40_CompileConductance
  END TYPE StrmGWConnector_v40_Type
    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmGWConnector_v40::'
  
  
  
  
CONTAINS




  ! -------------------------------------------------------------
  ! --- COMPILE CONDUCTANCE FOR STREAM-GW CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v40_CompileConductance(Connector,InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
    CLASS(StrmGWConnector_v40_Type)   :: Connector
    TYPE(GenericFileType)             :: InFile
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes),UpstrmNodes(:),DownstrmNodes(:)
    REAL(8),INTENT(IN)                :: BottomElevs(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38)  :: ThisProcedure = ModName // 'StrmGWConnector_v40_CompileConductance'
    INTEGER                       :: indxReach,indxNode,iGWNode,iGWUpstrmNode,iUpstrmNode, &
                                     iDownstrmNode,iNode,ErrorCode,iLayer,iStrmNodeID,     &
                                     iGWNodeID,iInteractionType
    REAL(8)                       :: B_DISTANCE,F_DISTANCE,CA,CB,FACTK,FACTL,              &
                                     DummyArray(NStrmNodes,4)
    REAL(8),DIMENSION(NStrmNodes) :: BedThick,WetPerimeter,Conductivity
    CHARACTER                     :: ALine*500,TimeUnitConductance*6
    LOGICAL                       :: lProcessed(NStrmNodes)
    INTEGER,ALLOCATABLE           :: iGWNodes(:)
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    CALL Connector%GetAllGWNodes(iGWNodes)
    
    !Read data
    CALL InFile%ReadData(FACTK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(ALine)
    TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL InFile%ReadData(FACTL,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Assumption for stream-aquifer disconnection
    CALL InFile%ReadData(iInteractionType,iStat)  
    IF (iStat .EQ. 0) THEN
        CALL Connector%SetInteractionType(iInteractionType,iStat)  
        IF (iStat .EQ. -1) RETURN
    ELSE
        iStat = 0
    END IF
    
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
        Conductivity(iNode) = DummyArray(indxNode,2)*FACTK
        BedThick(iNode)     = DummyArray(indxNode,3)*FACTL
        WetPerimeter(iNode) = DummyArray(indxNode,4)*FACTL
    END DO
    
    !Compute conductance
    DO indxReach=1,SIZE(UpstrmNodes)
        iUpstrmNode   = UpstrmNodes(indxReach)
        iDownstrmNode = DownstrmNodes(indxReach)
        B_DISTANCE    = 0.0
        DO indxNode=iUpstrmNode+1,iDownstrmNode
            iGWUpstrmNode = iGWNodes(indxNode-1)
            iGWNode       = iGWNodes(indxNode)
            iLayer        = Connector%iLayer(indxNode)
            IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
                IF (BottomElevs(indxNode)-BedThick(indxNode) .LT. Stratigraphy%BottomElev(iGWNode,iLayer)) THEN
                    iStrmNodeID        = iStrmNodeIDs(indxNode)
                    iGWNodeID          = AppGrid%AppNode(iGWNode)%ID
                    BedThick(indxNode) = BottomElevs(indxNode) - Stratigraphy%BottomElev(iGWNode,iLayer)
                    MessageArray(1)    = 'Stream bed thickness at stream node ' // TRIM(IntToText(iStrmNodeID)) // ' and GW node '// TRIM(IntToText(iGWNodeID)) // ' penetrates into second active aquifer layer!'
                    MessageArray(2)    = 'It is adjusted to penetrate only into the top active layer.'
                    CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure) 
                END IF
            END IF
            CA                        = AppGrid%X(iGWUpstrmNode) - AppGrid%X(iGWNode)
            CB                        = AppGrid%Y(iGWUpstrmNode) - AppGrid%Y(iGWNode)
            F_DISTANCE                = SQRT(CA*CA + CB*CB)/2d0
            Conductivity(indxNode-1)  = Conductivity(indxNode-1)*WetPerimeter(indxNode-1)*(F_DISTANCE+B_DISTANCE)/BedThick(indxNode-1)
            B_DISTANCE                = F_DISTANCE
        END DO
        Conductivity(iDownstrmNode) = Conductivity(iDownstrmNode)*WetPerimeter(iDownstrmNode)*B_DISTANCE/BedThick(iDownstrmNode)
    END DO
    
    !Allocate memory
    ALLOCATE (Connector%Conductance(NStrmNodes) , Connector%rBedThickness(NStrmNodes) , Connector%StrmGWFlow(NStrmNodes) , Connector%rDisconnectElev(NStrmNodes) , STAT=ErrorCode)
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
    IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
        Connector%rDisconnectElev = BottomElevs - Connector%rBedThickness
    ELSE
        Connector%rDisconnectElev = BottomElevs
    END IF
    
    !Clear memory
    DEALLOCATE (iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE StrmGWConnector_v40_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE STREAM-GW INTERACTION
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v40_Simulate(Connector,iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
    CLASS(StrmGWConnector_v40_Type)                 :: Connector
    INTEGER,INTENT(IN)                              :: iNNodes
    REAL(8),INTENT(IN)                              :: rGWHeads(:),rStrmHeads(:),rAvailableFlows(:)
    TYPE(MatrixType)                                :: Matrix
    CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)  !Not used in this version                    
    REAL(8),OPTIONAL,INTENT(IN)                     :: rMaxElevs(:)             !Not used in this version
    
    !Local variables
    INTEGER           :: iGWNode,iNodes_Connect(2),iNodes_RHS(2),indxStrm
    REAL(8)           :: Conductance,rUpdateCOEFF(2),rUpdateCOEFF_Keep(2),rUpdateRHS(2),rDiff_GW,rGWHead,           &
                         rDiffGWSQRT,rStrmGWFlow,rStrmGWFlowAdj,rStrmGWFlowAdjSQRT,rDStrmGWFlowAdj,rFractionForGW,  &
                         rNodeAvailableFlow
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iStrmComp , f_iGWComp]
                         
    
    !Update matrix equations
    DO indxStrm=1,SIZE(rStrmHeads)
        !Corresponding GW node and conductance
        iGWNode        = (Connector%iLayer(indxStrm)-1) * iNNodes + Connector%iGWNode(indxStrm)
        Conductance    = Connector%Conductance(indxStrm)
        rFractionForGW = Connector%rFractionForGW(indxStrm)
        
        !Head differences
        rGWHead     = rGWHeads(indxStrm)
        rDiff_GW    = rGWHead - Connector%rDisconnectElev(indxStrm)
        rDiffGWSQRT = SQRT(rDiff_GW*rDiff_GW + f_rSmoothMaxP)
        
        !Available flow for node
        rNodeAvailableFlow = rAvailableFlows(indxStrm)
                
        !Calculate stream-gw interaction and update of Jacobian
        !--------------------------------------------
        rStrmGWFlow = Conductance * (rStrmHeads(indxStrm) - MAX(rGWHead,Connector%rDisconnectElev(indxStrm)))
        !Stream is gaining; no need to worry about drying stream (i.e. stream-gw flow is not a function of upstream flows)
        IF (rStrmGWFlow .LT. 0.0) THEN
            Connector%StrmGWFlow(indxStrm) = rStrmGWFlow
            iNodes_Connect(1)              = indxStrm
            iNodes_Connect(2)              = iGWNode
            
            !Update Jacobian - entries for stream node 
            rUpdateCOEFF_Keep(1) = Conductance
            rUpdateCOEFF_Keep(2) = -0.5d0 * Conductance * (1d0+rDiff_GW/rDiffGWSQRT) 
            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Update Jacobian - entries for groundwater node
            rUpdateCOEFF = -rFractionForGW * rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
                                
        !Stream is losing; we need to limit stream loss to available flow
        ELSE
            rStrmGWFlowAdj     = rNodeAvailableFlow - rStrmGWFlow 
            rStrmGWFlowAdjSQRT = SQRT(rStrmGWFlowAdj*rStrmGWFlowAdj + f_rSmoothMaxP)
            rDStrmGWFlowAdj    = 0.5d0 * (1d0 + rStrmGWFlowAdj / rStrmGWFlowAdjSQRT)
            iNodes_Connect(1)  = indxStrm
            iNodes_Connect(2)  = iGWNode
            
            !Update Jacobian - entries for stream node 
            rUpdateCOEFF_Keep(1) = Conductance * rDStrmGWFlowAdj
            rUpdateCOEFF_Keep(2) = -0.5d0 * Conductance * (1d0+rDiff_GW/rDiffGWSQRT) * rDStrmGWFlowAdj
            rUpdateCOEFF         = rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Update Jacobian - entries for groundwater node
            rUpdateCOEFF = -rFractionForGW * rUpdateCOEFF_Keep
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs,iNodes_Connect,rUpdateCOEFF)
            
            !Store flow exchange
            Connector%StrmGWFlow(indxStrm) = MIN(rNodeAvailableFlow , rStrmGWFlow)

        END IF

        !Update RHS 
        iNodes_RHS(1) = indxStrm
        iNodes_RHS(2) = iGWNode
        rUpdateRHS(1) = Connector%StrmGWFlow(indxStrm)
        rUpdateRHS(2) = -Connector%StrmGWFlow(indxStrm) * rFractionForGW
        CALL Matrix%UpdateRHS(iCompIDs,iNodes_RHS,rUpdateRHS)
        
    END DO
    
  END SUBROUTINE StrmGWConnector_v40_Simulate
    
END MODULE