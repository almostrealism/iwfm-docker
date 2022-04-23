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
!
!  This version of the stream-groundwater connector uses wetted perimeter
!  as a function of stream or groundwater head, whichever is greater, and
!  the conductivity values are conductance per length per width of the 
!  channel.
!
!***********************************************************************
MODULE Class_StrmGWConnector_v50
  USE MessageLogger              , ONLY: SetLastMessage          , &
                                         MessageArray            , &
                                         iFatal
  USE GeneralUtilities
  USE IOInterface
  USE PAckage_Discretization
  USE Package_Misc               , ONLY: AbstractFunctionType    , &
                                         iStrmComp               , &
                                         iGWComp
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnectorType 
  USE Package_Matrix             , ONLY: MatrixType              , &
                                         ConnectivityListType
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
  PUBLIC :: StrmGWConnector_v50_Type                   , &
            StrmGWConnector_v50_UpdateMatrix_ForBypass , &
            StrmGWConnector_v50_RegisterWithMatrix

  
  
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseStrmGWConnectorType) :: StrmGWConnector_v50_Type
      PRIVATE
  CONTAINS
      PROCEDURE,PASS :: ComputeStrmGWFlow_AtMinHead   => StrmGWConnector_v50_ComputeStrmGWFlow_AtMinHead
      PROCEDURE,PASS :: Simulate                      => StrmGWConnector_v50_Simulate
      PROCEDURE,PASS :: CompileConductance            => StrmGWConnector_v50_CompileConductance
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
  ! --- ADD CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v50_RegisterWithMatrix(Connector,lUpstrmNode,AppGrid,Matrix,iStat)
    CLASS(StrmGWConnector_v50_Type),INTENT(IN) :: Connector
    LOGICAL,INTENT(IN)                         :: lUpstrmNode(:)
    TYPE(AppGridTYpe),INTENT(IN)               :: AppGrid
    TYPE(MatrixType)                           :: Matrix
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local varaibles
    INTEGER :: indxNode,GWNode(2),StrmNode(1),NNodes,GWNode_Prev
    
    !Initialize
    iStat  = 0
    NNodes = AppGrid%NNodes
    
    !Add connectivity
    DO indxNode=1,SIZE(Connector%iGWNode)
        StrmNode(1) = indxNode
        GWNode(1)   = (Connector%iLayer(indxNode)-1)*NNodes + Connector%iGWNode(indxNode)
        IF (lUpstrmNode(indxNode)) THEN
            CALL Matrix%AddConnectivity(iStrmComp,indxNode,iGWComp,GWNode(1:1),iStat)
        ELSE
            GwNode(2) = GWNode_Prev
            CALL Matrix%AddConnectivity(iStrmComp,indxNode,iGWComp,GWNode,iStat)
        END IF 
        IF (iStat .EQ. -1) RETURN
        CALL Matrix%AddConnectivity(iGWComp,GWNode(1),iStrmComp,StrmNode,iStat)
        IF (iStat .EQ. -1) RETURN
        GWNode_Prev = GWNode(1)
    END DO
    
  END SUBROUTINE StrmGWConnector_v50_RegisterWithMatrix

  
  ! -------------------------------------------------------------
  ! --- COMPUTE DERIVATES
  ! -------------------------------------------------------------
  SUBROUTINE ComputeDerivatives_AtStrmNode(iNode,UnitConductance,StrmHead,GWHead,DeltaX,StrmBottomElev,MaxElev,WetPerimeterFunction,dFlow_dStrmHead,dFlow_dGWHead)
    INTEGER,INTENT(IN)                        :: iNode
    REAL(8),INTENT(IN)                        :: UnitConductance,StrmHead,GWHead,DeltaX,StrmBottomElev,MaxElev
    CLASS(AbstractFunctionType),INTENT(IN)    :: WetPerimeterFunction                    !In this case, this is the wetted perimeter function defined using Manning's formula 
    REAL(8),INTENT(OUT)                       :: dFlow_dStrmHead,dFlow_dGWHead
    
    !Local variables
    REAL(8) :: rGWHead,HeadDiff,dHeadDiff_dHs,dHeadDiff_dHg,WetPerimeter,dWetPerimeter_dHs,dWetPerimeter_dHg,Coeff
    
    IF (StrmHead .GE. StrmBottomElev) THEN
        IF (GWHead .GE. StrmBottomElev) THEN
            rGWHead       = MIN(GWHead , MaxElev)
            HeadDiff      = StrmHead - rGWHead
            dHeadDiff_dHs = 1.0
            dHeadDiff_dHg = -1.0
            IF (rGWHead .GE. StrmHead) THEN
                CALL WetPerimeterFunction%EvaluateAndDerivative(rGWHead,WetPerimeter,dWetPerimeter_dHg)
                dWetPerimeter_dHs = 0.0
            ELSE
                CALL WetPerimeterFunction%EvaluateAndDerivative(StrmHead,WetPerimeter,dWetPerimeter_dHs)
                dWetPerimeter_dHg = 0.0
            END IF
        ELSE   
            HeadDiff      = StrmHead - StrmBottomElev
            dHeadDiff_dHs = 1.0
            dHeadDiff_dHg = 0.0
            CALL  WetPerimeterFunction%EvaluateAndDerivative(StrmHead,WetPerimeter,dWetPerimeter_dHs) 
            dWetPerimeter_dHg = 0.0
        END IF
    ELSE
        IF (GWHead .GE. StrmBottomElev) THEN
            rGWHead       = MIN(GWHead , MaxElev)
            HeadDiff      = StrmBottomElev - rGWHead
            dHeadDiff_dHs = 0.0
            dHeadDiff_dHg = -1.0
            CALL  WetPerimeterFunction%EvaluateAndDerivative(rGWHead,WetPerimeter,dWetPerimeter_dHg) 
            dWetPerimeter_dHs = 0.0
        ELSE
            HeadDiff          = 0.0
            dHeadDiff_dHs     = 0.0
            dHeadDiff_dHg     = 0.0
            WetPerimeter      = 0.0
            dWetPerimeter_dHs = 0.0
            dWetPerimeter_dHg = 0.0
        END IF
    END IF
    
    !Compute derivatives
    Coeff           = 0.5D0 * UnitConductance * DeltaX
    dFlow_dStrmHead = Coeff * (dWetPerimeter_dHs * HeadDiff + WetPerimeter * dHeadDiff_dHs)
    dFlow_dGWHead   = Coeff * (dWetPerimeter_dHg * HeadDiff - WetPerimeter * dHeadDiff_dHg)

  END SUBROUTINE ComputeDerivatives_AtStrmNode
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE MATRIX FOR BYPASS NODES
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v50_UpdateMatrix_ForBypass(Connector,iStrmNode,StrmHead,GWHead,DeltaX,StrmBottomElev,MaxElev,dBypass_dFlow,WetPerimeterFunction,Matrix)
    TYPE(StrmGWConnector_v50_Type),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                        :: iStrmNode
    REAL(8),INTENT(IN)                        :: StrmHead(:),GWHead(:),DeltaX(:),StrmBottomElev(:),MaxElev(:),dBypass_dFlow
    CLASS(AbstractFunctionType),INTENT(IN)    :: WetPerimeterFunction(:)                    !In this case, this is the wetted perimeter function defined using Manning's formula 
    TYPE(MatrixType)                          :: Matrix
    
    !Local variables
    INTEGER :: iCompIDs_Connect(2),iNodeIDs_Connect(2),iGWNode_Prev
    REAL(8) :: rUpdateValues(2),dFlow_dStrmHead_Prev,dFlow_dGWHead_Prev
    
    !Get the associated gw node
    iGWNode_Prev = Connector%iGWNode(iStrmNode-1)
    
    !Compute the derivatives
    CALL ComputeDerivatives_AtStrmNode(iStrmNode-1,Connector%Conductance(iStrmNode-1),StrmHead(iStrmNode-1),GWHead(iStrmNode-1),DeltaX(iStrmNode),StrmBottomElev(iStrmNode-1),MaxElev(iStrmNode-1),WetPerimeterFunction(iStrmNode-1),dFlow_dStrmHead_Prev,dFlow_dGWHead_Prev)
    
    !Update COEFF matrix for row iStrmNode
    iCompIDs_Connect(1) = iStrmComp
    iCompIDs_Connect(2) = iGWComp
    iNodeIDs_Connect(1) = iStrmNode - 1
    iNodeIDs_Connect(2) = iGWNode_Prev
    rUpdateValues(1)    = -dBypass_dFlow * dFlow_dStrmHead_Prev
    rUpdateValues(2)    = -dBypass_dFlow * dFlow_dGWHead_Prev
    CALL Matrix%UpdateCOEFF(iStrmComp,iStrmNode,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
        
  END SUBROUTINE StrmGWConnector_v50_UpdateMatrix_ForBypass

  
  ! -------------------------------------------------------------
  ! --- COMPILE STREAM-GW CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v50_CompileConductance(Connector,InFile,AppGrid,NStrmNodes,UpstrmNodes,DownstrmNodes,iStat)
    CLASS(StrmGWConnector_v50_Type) :: Connector
    TYPE(GenericFileType)           :: InFile
    TYPE(AppGridType),INTENT(IN)    :: AppGrid                                     !Not used in this version
    INTEGER,INTENT(IN)              :: NStrmNodes,UpstrmNodes(:),DownstrmNodes(:)  !UpstrmNodes and DownstrmNodes are not used in this version
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38)  :: ThisProcedure = ModName // 'StrmGWConnector_v50_CompileConductance'
    INTEGER                       :: indxNode,iNode,ErrorCode
    REAL(8)                       :: FACTK,FACTL,DummyArray(NStrmNodes,3)
    REAL(8),DIMENSION(NStrmNodes) :: Conductivity,BedThick
    CHARACTER                     :: ALine*500,TimeUnitConductance*6
    INTEGER,ALLOCATABLE           :: iGWNodes(:)
    
    !Initialize
    iStat = 0
    CALL Connector%GetAllGWNodes(iGWNodes)
    
    !Read data
    CALL InFile%ReadData(FACTK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL InFile%ReadData(FACTL,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    DO indxNode=1,NStrmNodes
      iNode = INT(DummyArray(indxNode,1))
      IF (iNode .NE. indxNode) THEN 
        MessageArray(1)='Parameters for stream nodes should be entered sequentialy.'
        MessageArray(2)='Expected stream node='//TRIM(IntToText(indxNode))
        MessageArray(3)='Entered stream node ='//TRIM(IntToText(iNode))
        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
      Conductivity(indxNode) = DummyArray(indxNode,2)*FACTK
      BedThick(indxNode)     = DummyArray(indxNode,3)*FACTL
      Conductivity(indxNode) = Conductivity(indxNode) / BedThick(indxNode)
    END DO

    !Allocate memory
    ALLOCATE (Connector%Conductance(NStrmNodes) , Connector%StrmGWFlow(NStrmNodes) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream-gw connection data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store information
    Connector%Conductance         = Conductivity
    Connector%TimeUnitConductance = TimeUnitConductance
    Connector%StrmGWFlow          = 0.0
    
    !Clear memory
    DEALLOCATE (iGWNodes , STAT=ErrorCode)
    
  END SUBROUTINE StrmGWConnector_v50_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v50_Simulate(Connector,NNodes,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,Matrix,DeltaX,MaxElev)
    CLASS(StrmGWConnector_v50_Type)        :: Connector
    INTEGER,INTENT(IN)                     :: NNodes
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)   !In this case, this is the wetted perimeter function defined using Manning's formula 
    TYPE(MatrixType)                       :: Matrix
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:),MaxElev(:)
    
    !Local variables
    INTEGER :: indxStrm,NStrmNodes,iGWNode,iCompIDs(3),iNodeIDs(3),iCompIDs_Connect(4),iGWNode_Prev, &
               iNodeIDs_Connect(4)
    REAL(8) :: HeadDiff,UnitConductance,rInteraction,WetPerimeter,rUpdateValues(4),rInteraction_Prev,       &
               dWetPerimeter_dHs,dWetPerimeter_dHg,dHeadDiff_dHs,HeadDiff_Prev,WetPerimeter_Prev,Coeff,     &
               dWetPerimeter_dHs_Prev,dWetPerimeter_dHg_Prev,dHeadDiff_dHg,dHeadDiff_dHs_Prev,Coeff_Prev,   &
               dHeadDiff_dHg_Prev,rUpdateRHS(3),UnitConductance_Prev,rGWHead
    
    !Initialize
    NStrmNodes           = SIZE(StrmHead)
    Connector%StrmGWFlow = 0.0
    
    !Compute stream-gw interaction at each stream node; also update the matrix equation
    DO indxStrm=1,NStrmNodes
        
        !Corresponding GW node
        iGWNode = (Connector%iLayer(indxStrm)-1) * NNodes + Connector%iGWNode(indxStrm)
      
        !Unit conductance
        UnitConductance  = Connector%Conductance(indxStrm)      !For this version of StrmGWConnector, original conductance does not include wetted perimeter
                
        IF (StrmHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
            IF (GWHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
                rGWHead       = MIN(GWHead(indxStrm) , MaxElev(indxStrm))
                HeadDiff      = StrmHead(indxStrm) - rGWHead
                dHeadDiff_dHs = 1.0
                dHeadDiff_dHg = -1.0
                IF (rGWHead .GE. StrmHead(indxStrm)) THEN
                    CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(rGWHead,WetPerimeter,dWetPerimeter_dHg)
                    dWetPerimeter_dHs = 0.0
                ELSE
                    CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(StrmHead(indxStrm),WetPerimeter,dWetPerimeter_dHs)
                    dWetPerimeter_dHg = 0.0
                END IF
            ELSE   
                HeadDiff      = StrmHead(indxStrm) - StrmBottomElev(indxStrm)
                dHeadDiff_dHs = 1.0
                dHeadDiff_dHg = 0.0
                CALL  WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(StrmHead(indxStrm),WetPerimeter,dWetPerimeter_dHs) 
                dWetPerimeter_dHg = 0.0
            END IF
        ELSE
            IF (GWHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
                rGWHead       = MIN(GWHead(indxStrm) , MaxElev(indxStrm))
                HeadDiff      = StrmBottomElev(indxStrm) - rGWHead
                dHeadDiff_dHs = 0.0
                dHeadDiff_dHg = -1.0
                CALL  WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(rGWHead,WetPerimeter,dWetPerimeter_dHg) 
                dWetPerimeter_dHs = 0.0
            ELSE
                HeadDiff          = 0.0
                dHeadDiff_dHs     = 0.0
                dHeadDiff_dHg     = 0.0
                WetPerimeter      = 0.0
                dWetPerimeter_dHs = 0.0
                dWetPerimeter_dHg = 0.0
            END IF
        END IF
        
        !If DeltaX is zero, it is an upstream boundary node; advance data and go to next node
        IF (DeltaX(indxStrm) .EQ. 0.0) THEN
            iGWNode_Prev           = iGWNode
            UnitConductance_Prev   = UnitConductance
            HeadDiff_Prev          = HeadDiff         
            dHeadDiff_dHs_Prev     = dHeadDiff_dHs    
            dHeadDiff_dHg_Prev     = dHeadDiff_dHg    
            WetPerimeter_Prev      = WetPerimeter     
            dWetPerimeter_dHs_Prev = dWetPerimeter_dHs
            dWetPerimeter_dHg_Prev = dWetPerimeter_dHg
            CYCLE
        END IF
        
        !Coefficients that will be used regularly
        Coeff      = 0.5D0 * UnitConductance      * DeltaX(indxStrm)
        Coeff_Prev = 0.5D0 * UnitConductance_Prev * DeltaX(indxStrm)
        
        !Calculate and store stream-gw interaction
        rInteraction                     = Coeff      * WetPerimeter      * HeadDiff
        rInteraction_Prev                = Coeff_Prev * WetPerimeter_Prev * HeadDiff_Prev
        Connector%StrmGWFlow(indxStrm)   = Connector%StrmGWFlow(indxStrm)   + rInteraction
        Connector%StrmGWFlow(indxStrm-1) = Connector%StrmGWFlow(indxStrm-1) + rInteraction_Prev
        
        !Update RHS vector
        iCompIDs(1)   = iStrmComp
        iCompIDs(2:3) = iGWComp
        iNodeIDs(1)   = indxStrm
        iNodeIDs(2)   = iGWNode_Prev
        iNodeIDs(3)   = iGWNode
        rUpdateRHS(1) = rInteraction + rInteraction_Prev
        rUpdateRHS(2) = -rInteraction_Prev
        rUpdateRHS(3) = -rInteraction
        CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateRHS)
        
        !Update COEFF matrix for row indxStrm
        iCompIDs_Connect(1:2) = iStrmComp
        iCompIDs_Connect(3:4) = iGWComp
        iNodeIDs_Connect(1)   = indxStrm - 1
        iNodeIDs_Connect(2)   = indxStrm
        iNodeIDs_Connect(3)   = iGWNode_Prev
        iNodeIDs_Connect(4)   = iGWNode
        rUpdateValues(1)      = Coeff_Prev * (dWetPerimeter_dHs_Prev * HeadDiff_Prev + WetPerimeter_Prev * dHeadDiff_dHs_Prev)
        rUpdateValues(2)      = Coeff      * (dWetPerimeter_dHs * HeadDiff + WetPerimeter * dHeadDiff_dHs)
        rUpdateValues(3)      = Coeff_Prev * (dWetPerimeter_dHg_Prev * HeadDiff_Prev + WetPerimeter_Prev * dHeadDiff_dHg_Prev)
        rUpdateValues(4)      = Coeff      * (dWetPerimeter_dHg * HeadDiff + WetPerimeter * dHeadDiff_dHg)
        CALL Matrix%UpdateCOEFF(iStrmComp,indxStrm,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
        
        !Update COEFF matrix for row iGWNode_Prev
        iCompIDs_Connect(1) = iStrmComp
        iCompIDs_Connect(2) = iGWComp
        iNodeIDs_Connect(1) = indxStrm - 1
        iNodeIDs_Connect(2) = iGWNode_Prev
        rUpdateValues(1)    = -Coeff_Prev * (dWetPerimeter_dHs_Prev * HeadDiff_Prev + WetPerimeter_Prev * dHeadDiff_dHs_Prev)
        rUpdateValues(2)    = -Coeff_Prev * (dWetPerimeter_dHg_Prev * HeadDiff_Prev + WetPerimeter_Prev * dHeadDiff_dHg_Prev)
        CALL Matrix%UpdateCOEFF(iGWComp,iGWNode_Prev,iCompIDs_Connect(1:2),iNodeIDs_Connect(1:2),rUpdateValues(1:2))
        
        !Update COEFF matrix for row iGWNode
        iCompIDs_Connect(1) = iStrmComp
        iCompIDs_Connect(2) = iGWComp
        iNodeIDs_Connect(1) = indxStrm
        iNodeIDs_Connect(2) = iGWNode
        rUpdateValues(1)    = -Coeff * (dWetPerimeter_dHs * HeadDiff + WetPerimeter * dHeadDiff_dHs)
        rUpdateValues(2)    = -Coeff * (dWetPerimeter_dHg * HeadDiff + WetPerimeter * dHeadDiff_dHg)
        CALL Matrix%UpdateCOEFF(iGWComp,iGWNode,iCompIDs_Connect(1:2),iNodeIDs_Connect(1:2),rUpdateValues(1:2))
        
        !Advance data
        iGWNode_Prev           = iGWNode
        UnitConductance_Prev   = UnitConductance
        HeadDiff_Prev          = HeadDiff         
        dHeadDiff_dHs_Prev     = dHeadDiff_dHs    
        dHeadDiff_dHg_Prev     = dHeadDiff_dHg    
        WetPerimeter_Prev      = WetPerimeter     
        dWetPerimeter_dHs_Prev = dWetPerimeter_dHs
        dWetPerimeter_dHg_Prev = dWetPerimeter_dHg
        
    END DO
    
  END SUBROUTINE StrmGWConnector_v50_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION WITHOUT UPDATING MATRIX
  ! --- *** Note: + flow means loosing stream
  ! --- *** Note: Stream head is assumed to be always at or above stream bed elevation
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v50_ComputeStrmGWFlow_AtMinHead(Connector,Flows,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,DeltaX)
    CLASS(StrmGWConnector_v50_Type)        :: Connector
    REAL(8),INTENT(OUT)                    :: Flows(:)
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)                 !In this case, this is the wetted perimeter function defined using Manning's formula 
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:)
    
    !Local variables
    INTEGER :: indxStrm,NStrmNodes
    REAL(8) :: UnitConductance,HeadDiff,WetPerimeter,UnitConductance_Prev,HeadDiff_Prev,  &
               WetPerimeter_Prev,HeadDiff_AtMinHead,WetPerimeter_AtMinHead
    
    !Initialize
    NStrmNodes = SIZE(StrmHead)
    
    DO indxStrm=1,NStrmNodes
        
        !Unit conductance
        UnitConductance  = Connector%Conductance(indxStrm)      !For this version of StrmGWConnector, original conductance does not include wetted perimeter
                
        !Head difference and wetted perimeter at minimum stream head,current stream head 
        IF (GWHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
            HeadDiff_AtMinHead     = StrmBottomElev(indxStrm) - GWHead(indxStrm) 
            WetPerimeter_atMinHead = WetPerimeterFunction(indxStrm)%Evaluate(GWHead(indxStrm))
            HeadDiff               = StrmHead(indxStrm) - GWHead(indxStrm) 
            IF (GWHead(indxStrm) .GE. StrmHead(indxStrm)) THEN
                WetPerimeter = WetPerimeterFunction(indxStrm)%Evaluate(GWHead(indxStrm))
            ELSE
                WetPerimeter = WetPerimeterFunction(indxStrm)%Evaluate(StrmHead(indxStrm))
            END IF
        ELSE   
            HeadDiff_AtMinHead     = 0.0
            WetPerimeter_AtMinHead = 0.0 
            HeadDiff               = StrmHead(indxStrm) - StrmBottomElev(indxStrm)
            WetPerimeter           = WetPerimeterFunction(indxStrm)%Evaluate(StrmHead(indxStrm)) 
        END IF
        
        !If DeltaX is zero, it is an upstream boundary node; advance data and go to next node
        IF (DeltaX(indxStrm) .EQ. 0.0) THEN
            UnitConductance_Prev = UnitConductance
            HeadDiff_Prev        = HeadDiff         
            WetPerimeter_Prev    = WetPerimeter
            Flows(indxStrm)      = 0.0
            CYCLE
        END IF
        
        !Stream-gw interaction
        Flows(indxStrm) = 0.5D0 * UnitConductance      * WetPerimeter_AtMinHead * DeltaX(indxStrm) * HeadDiff_AtMinHead      &
                        + 0.5D0 * UnitConductance_Prev * WetPerimeter_Prev      * DeltaX(indxStrm) * HeadDiff_Prev
        
        !Advance data
        UnitConductance_Prev = UnitConductance
        HeadDiff_Prev        = HeadDiff         
        WetPerimeter_Prev    = WetPerimeter     

    END DO
    
  END SUBROUTINE StrmGWConnector_v50_ComputeStrmGWFlow_AtMinHead



END MODULE