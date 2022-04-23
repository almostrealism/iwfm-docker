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
MODULE Class_StrmGWConnector_v40
  USE MessageLogger              , ONLY: SetLastMessage           , &
                                         MessageArray             , &
                                         iFatal
  USE IOInterface
  USE TimeSeriesUtilities
  USE GeneralUtilities
  USE Package_Discretization
  USE Package_Misc               , ONLY: AbstractFunctionType     , &
                                         iStrmComp                , &
                                         iGWComp
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnectorType  
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
      PROCEDURE,PASS :: ComputeStrmGWFlow_AtMinHead  => StrmGWConnector_v40_ComputeStrmGWFlow_AtMinHead
      PROCEDURE,PASS :: Simulate                     => StrmGWConnector_v40_Simulate
      PROCEDURE,PASS :: CompileConductance           => StrmGWConnector_v40_CompileConductance
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
  SUBROUTINE StrmGWConnector_v40_CompileConductance(Connector,InFile,AppGrid,NStrmNodes,UpstrmNodes,DownstrmNodes,iStat)
    CLASS(StrmGWConnector_v40_Type) :: Connector
    TYPE(GenericFileType)           :: InFile
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    INTEGER,INTENT(IN)              :: NStrmNodes,UpstrmNodes(:),DownstrmNodes(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38)  :: ThisProcedure = ModName // 'StrmGWConnector_v40_CompileConductance'
    INTEGER                       :: indxReach,indxNode,iGWNode,iGWUpstrmNode,iUpstrmNode, &
                                     iDownstrmNode,iNode,ErrorCode
    REAL(8)                       :: B_DISTANCE,F_DISTANCE,CA,CB,FACTK,FACTL,              &
                                     DummyArray(NStrmNodes,4)
    REAL(8),DIMENSION(NStrmNodes) :: BedThick,WetPerimeter,Conductivity
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
    CALL InFile%ReadData(FACTL,iStat)  ;  IF (iStat .EQ. -1) RETURN
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
      WetPerimeter(indxNode) = DummyArray(indxNode,4)*FACTL
    END DO

    !Compute conductance
    DO indxReach=1,SIZE(UpstrmNodes)
      iUpstrmNode   = UpstrmNodes(indxReach)
      iDownstrmNode = DownstrmNodes(indxReach)
      B_DISTANCE    = 0.0
      DO indxNode=iUpstrmNode+1,iDownstrmNode
        iGWUpstrmNode             = iGWNodes(indxNode-1)
        iGWNode                   = iGWNodes(indxNode)
        CA                        = AppGrid%Node(iGWUpstrmNode)%X - AppGrid%Node(iGWNode)%X
        CB                        = AppGrid%Node(iGWUpstrmNode)%Y - AppGrid%Node(iGWNode)%Y
        F_DISTANCE                = SQRT(CA*CA + CB*CB)/2d0
        Conductivity(indxNode-1)  = Conductivity(indxNode-1)*WetPerimeter(indxNode-1)*(F_DISTANCE+B_DISTANCE)/BedThick(indxNode-1)
        B_DISTANCE                = F_DISTANCE
      END DO
      Conductivity(iDownstrmNode) = Conductivity(iDownstrmNode)*WetPerimeter(iDownstrmNode)*B_DISTANCE/BedThick(iDownstrmNode)
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
    
  END SUBROUTINE StrmGWConnector_v40_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v40_Simulate(Connector,NNodes,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,Matrix,DeltaX,MaxElev)
    CLASS(StrmGWConnector_v40_Type)        :: Connector
    INTEGER,INTENT(IN)                     :: NNodes
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)              !This is not used in this version
    TYPE(MatrixType)                       :: Matrix
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:),MaxElev(:)                 !These are not used in this version
    
    !Local variables
    INTEGER :: indxStrm,iGWNode,NStrmNodes,iNodeIDs_Connect(2),iCompIDs_Connect(2), &
               iCompIDs(2),iNodeIDs(2)
    REAL(8) :: HeadDiff,Conductance,rInteraction,rUpdateValues(2) 
               
    
    !Initialize
    NStrmNodes = SIZE(StrmHead)
    
    !Compute stream-gw interaction at each stream node; also update the matrix equation
    DO indxStrm=1,NStrmNodes
      !Corresponding GW node and conductance
      iGWNode     = (Connector%iLayer(indxStrm)-1) * NNodes + Connector%iGWNode(indxStrm)
      Conductance = Connector%Conductance(indxStrm)

      IF (StrmHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
        IF (GWHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
          HeadDiff            = StrmHead(indxStrm) - GWHead(indxStrm)
          iCompIDs_Connect(1) = iStrmComp
          iCompIDs_Connect(2) = iGWComp
          iNodeIDs_Connect(1) = indxStrm                    
          iNodeIDs_Connect(2) = iGWNode  
          rUpdateValues(1)    = Conductance
          rUpdateValues(2)    = -Conductance  
          CALL Matrix%UpdateCOEFF(iStrmComp,indxStrm,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
          rUpdateValues(1)    = -Conductance  
          rUpdateValues(2)    = Conductance  
          CALL Matrix%UpdateCOEFF(iGWComp,iGWNode,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
        ELSE
          HeadDiff            = StrmHead(indxStrm) - StrmBottomElev(indxStrm)
          iCompIDs_Connect(1) = iStrmComp
          iNodeIDs_Connect(1) = indxStrm                    
          rUpdateValues(1)    = Conductance
          CALL Matrix%UpdateCOEFF(iStrmComp,indxStrm,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
          rUpdateValues(1)    = -Conductance
          CALL Matrix%UpdateCOEFF(iGWComp,iGWNode,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
        END IF
      ELSE
        IF (GWHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
          HeadDiff            = StrmBottomElev(indxStrm) - GWHead(indxStrm)
          iCompIDs_Connect(1) = iGWComp
          iNodeIDs_Connect(1) = iGWNode                    
          rUpdateValues(1)    = -Conductance
          CALL Matrix%UpdateCOEFF(iStrmComp,indxStrm,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
          rUpdateValues(1)    = Conductance
          CALL Matrix%UpdateCOEFF(iGWComp,iGWNode,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
        ELSE
          HeadDiff = 0.0
        END IF
      END IF
      
      rInteraction                   = Conductance * HeadDiff
      Connector%StrmGWFlow(indxStrm) = rInteraction
      iCompIDs(1)                    = iStrmComp
      iCompIDs(2)                    = iGWComp
      iNodeIDs(1)                    = indxStrm
      iNodeIDs(2)                    = iGWNode
      rUpdateValues(1)               = rInteraction
      rUpdateValues(2)               = -rInteraction
      CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateValues)
      
    END DO
    
  END SUBROUTINE StrmGWConnector_v40_Simulate
  

  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION WITHOUT UPDATING MATRIX
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v40_ComputeStrmGWFlow_AtMinHead(Connector,Flows,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,DeltaX)
    CLASS(StrmGWConnector_v40_Type)        :: Connector
    REAL(8),INTENT(OUT)                    :: Flows(:)
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)  !StrmHead is not used in this version
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)                  !Not used in this version 
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:)                                !Not used in this version
    
    !Local variables
    INTEGER :: indxStrm
    REAL(8) :: HeadDiff
                   
    !Compute stream-gw interaction at each stream node when stream is dry
    DO indxStrm=1,SIZE(GWHead)
        IF (GWHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
            HeadDiff        = StrmBottomElev(indxStrm) - GWHead(indxStrm)
            Flows(indxStrm) = Connector%Conductance(indxStrm) * HeadDiff      
        ELSE
            Flows(indxStrm) = 0.0
        END IF      
    END DO

  END SUBROUTINE StrmGWConnector_v40_ComputeStrmGWFlow_AtMinHead


END MODULE