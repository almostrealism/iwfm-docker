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
MODULE Class_StrmGWConnector_v42
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
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnectorType  , &
                                         BaseStrmGWConnector_Kill
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
  PUBLIC :: StrmGWConnector_v42_Type                    
  
  
  ! -------------------------------------------------------------
  ! --- INTERACTING GW NODE LIST
  ! -------------------------------------------------------------
  TYPE GWNodeListType
      INTEGER             :: nGWNodes    = 0    !Number of groundwater nodes interacting with the stream at a stream node
      INTEGER,ALLOCATABLE :: iGWNodes(:)        !List of groundwater nodes; first gw node is used to compute the length along the stream which is used in calculating the conductance
      INTEGER,ALLOCATABLE :: iLayers(:)         !Aquifer layer numbers that the stream node interacts with
      REAL(8),ALLOCATABLE :: Conductance(:)     !Stream bed conductance at each groundwater node
      REAL(8),ALLOCATABLE :: StrmGWFlow(:)      !Stream-aquifer interaction at each gw node associated with the stream node
  END TYPE GWNodeListType
  
      
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseStrmGWConnectorType) :: StrmGWConnector_v42_Type
      PRIVATE
      INTEGER                          :: nTotalGWNodes = 0               !Total number of gw nodes (counting doubles at confluences) that interact with streams
      TYPE(GWNodeListType),ALLOCATABLE :: GWNodeList(:)                   !GW node list interacting with the stream at each stream node
  CONTAINS
      PROCEDURE,PASS :: AddGWNodesToStrmNode
      PROCEDURE,PASS :: Kill                                     => StrmGWConnector_v42_Kill                          !Overrides the original method from base class
      PROCEDURE,PASS :: StrmGWConnector_v42_Kill
      PROCEDURE,PASS :: GetnTotalGWNodes
      PROCEDURE,PASS :: GetGWNodesAtStrmNode
      PROCEDURE,PASS :: GetAllGWNodes                            => StrmGWConnector_v42_GetAllGWNodes                 !Overrides the original method from base class
      PROCEDURE,PASS :: GetAllLayers                             => StrmGWConnector_v42_GetAllLayers                  !Overrides the original method from base class
      PROCEDURE,PASS :: GetFlowAtGWNode                          => StrmGWConnector_v42_GetFlowAtGWNode               !Overrides the original method from base class
      PROCEDURE,PASS :: GetSubregionalFlows                      => StrmGWConnector_v42_GetSubregionalFlows           !Overrides the original method from base class
      PROCEDURE,PASS :: GetGWHeadsAtStrmNodes                    => StrmGWConnector_v42_GetGWHeadsAtStrmNodes         !Overrides the original method from base class
      PROCEDURE,PASS :: RegisterWithMatrix                       => StrmGWConnector_v42_RegisterWithMatrix            !Overrides the original method from base class
      PROCEDURE,PASS :: BaseStrmGWConnector_ReadPreprocessedData => StrmGWConnector_v42_ReadPreprocessedData          !Overrides the original method from base class
      PROCEDURE,PASS :: WritePreprocessedData                    => StrmGWConnector_v42_WritePreprocessedData         !Overrides the original method from base class
      PROCEDURE,PASS :: ComputeStrmGWFlow_AtMinHead              => StrmGWConnector_v42_ComputeStrmGWFlow_AtMinHead
      PROCEDURE,PASS :: Simulate                                 => StrmGWConnector_v42_Simulate
      PROCEDURE,PASS :: CompileConductance                       => StrmGWConnector_v42_CompileConductance
      PROCEDURE,PASS :: ConvertTimeUnit                          => StrmGWConnector_v42_ConvertTimeUnit               !Overrides the original method from base class
  END TYPE StrmGWConnector_v42_Type
    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: MaxnGWNodes = 100   !Maximum number of groundwater nodes that can be associated with a stream node
  INTEGER,PARAMETER                   :: ModNameLen  = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'Class_StrmGWConnector_v42::'
  
  
  
  
CONTAINS



    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! ---ADD GW NODE TO A STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE AddGWNodesToStrmNode(Connector,NStrmNodes,iStrmNode,iGWNodes,iLayers)
    CLASS(StrmGWConnector_v42_Type) :: Connector
    INTEGER,INTENT(IN)              :: NStrmNodes,iStrmNode,iGWNodes(:),iLayers(:)
    
    !Local variables
    INTEGER             :: nGWNodes,iDim
    INTEGER,ALLOCATABLE :: iTempGWNodes(:),iTempLayers(:)
    
    !Add data to stream
    IF (ALLOCATED(Connector%GWNodeList)) THEN
        ASSOCIATE (pData => Connector%GWNodeList(iStrmNode))
            nGWNodes = pData%nGWNodes
            iDim     = nGWNodes + SIZE(iGWNodes)
            ALLOCATE (iTempGWNodes(iDim) , iTempLayers(iDim))
            iTempGWNodes(1:nGWNodes)  = pData%iGWNodes     
            iTempGWNodes(nGWNodes+1:) = iGWNodes  ;  CALL MOVE_ALLOC(iTempGWNodes , pData%iGWNodes)
            iTempLayers(1:nGWNodes)   = pData%iLayers      ;  iTempLayers(nGWNodes+1:)  = iLayers   ;  CALL MOVE_ALLOC(iTempLayers  , pData%iLayers)
            pData%nGWNodes            = iDim
        END ASSOCIATE
        
    ELSE
        ALLOCATE (Connector%GWNodeList(NStrmNodes))
        ASSOCIATE (pData => Connector%GWNodeList(iStrmNode))
            nGWNodes = SIZE(iGWNodes)
            ALLOCATE (pData%iGWNodes(nGWNodes) , pData%iLayers(nGWNodes)) 
            pData%nGWNodes    = nGWNodes
            pData%iGWNodes    = iGWNodes
            pData%iLayers     = iLayers
        END ASSOCIATE
    END IF
    
    Connector%nTotalGWNodes = Connector%nTotalGWNodes + SIZE(iGWNodes)
       
  END SUBROUTINE AddGWNodesToStrmNode
  
  
  ! -------------------------------------------------------------
  ! --- READ PRE-PROCESSED STREAM-GW CONNECTOR DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_ReadPreprocessedData(Connector,InFile,iStat)
    CLASS(StrmGWConnector_v42_Type) :: Connector
    TYPE(GenericFileType)           :: InFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+40) :: ThisProcedure = ModName // 'StrmGWConnector_v42_ReadPreprocessedData'
    INTEGER                      :: nStrmNodes,ErrorCode,indx,nGWNodes
    CHARACTER                    :: cErrMsg*500
    
    CALL InFile%ReadData(Connector%nTotalGWNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(nStrmNodes,iStat)               ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Connector%GWNodeList(nStrmNodes)  , &
              STAT = ErrorCode                  , &
              ERRMSG = cErrMsg                  )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for stream-gw connection data!'//NEW_LINE('x')//TRIM(cErrMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    DO indx=1,nStrmNodes
        CALL Infile%ReadData(nGWNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Connector%GWNodeList(indx)%nGWNodes = nGWNodes
        ALLOCATE (Connector%GWNodeList(indx)%iGWNodes(nGWNodes) , Connector%GWNodeList(indx)%iLayers(nGWNodes))
        CALL InFile%ReadData(Connector%GWNodeList(indx)%iGWNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(Connector%GWNodeList(indx)%iLayers,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
        
  END SUBROUTINE StrmGWConnector_v42_ReadPreprocessedData
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- KILL CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_Kill(Connector)
    CLASS(StrmGWConnector_v42_Type) :: Connector
    
    !Local variables
    INTEGER                        :: ErrorCode,indxStrm
    TYPE(StrmGWConnector_v42_Type) :: Dummy
    
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        DEALLOCATE (Connector%GWNodeLIst(indxStrm)%iGWNodes , Connector%GWNodeLIst(indxStrm)%iLayers , Connector%GWNodeLIst(indxStrm)%Conductance , Connector%GWNodeLIst(indxStrm)%StrmGWFlow , STAT=ErrorCode)
    END DO
    DEALLOCATE (Connector%GWNodeList , STAT=ErrorCode)
    Connector%nTotalGWNodes = Dummy%nTotalGWNodes

    CALL BaseStrmGWConnector_Kill(Connector)
    
  END SUBROUTINE StrmGWConnector_v42_Kill
  
  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- WRITE PRE-PROCESSED DATA 
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_WritePreprocessedData(Connector,OutFile)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    TYPE(GenericFileType)                      :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    CALL Outfile%WriteData(Connector%iVersion)
    CALL Outfile%WriteData(Connector%nTotalGWNodes)
    CALL Outfile%WriteData(SIZE(Connector%GWNodeList))
    DO indx=1,SIZE(Connector%GWNodeList)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%nGWNodes)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%iGWNodes)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%iLayers)
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_WritePreprocessedData
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT SUBREGIONS 
  ! -------------------------------------------------------------
  FUNCTION StrmGWConnector_v42_GetSubregionalFlows(Connector,AppGrid) RESULT(Flows)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    REAL(8)                                    :: Flows(AppGrid%NSubregions)
    
    !Local variables
    INTEGER             :: indxStrm,indx,nGWNodes
    REAL(8)             :: StrmGWFlows(Connector%nTotalGWNodes)
    INTEGER,ALLOCATABLE :: iGWNodes(:)
    
    !Compile gw nodes list and stream-gw flows at these nodes
    CALL Connector%GetAllGWNodes(iGWNodes)
    indx = 0
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        nGWNodes = Connector%GWNodeList(indxStrm)%nGWNodes
        StrmGWFlows(indx+1:indx+nGWNodes) = Connector%GWNodeList(indxStrm)%StrmGWFlow
        indx                              = indx + nGWNodes
    END DO
    
    !Compute regional flows
    Flows = AppGrid%AccumSomeNodeValuesToSubregions(iGWNodes,StrmGWFlows)
    
  END FUNCTION StrmGWConnector_v42_GetSubregionalFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW AT GROUNDWATER NODE AT A LAYER
  ! -------------------------------------------------------------
  FUNCTION StrmGWConnector_v42_GetFlowAtGWNode(Connector,iNode,iLayer) RESULT(Flow)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                         :: iNode,iLayer
    REAL(8)                                    :: Flow
    
    !Local variables
    INTEGER :: indxStrm,indxGW
    
    !Initialize
    Flow = 0.0
    
    !Get flow
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        DO indxGW=1,Connector%GWNodeList(indxStrm)%nGWNodes
            IF (Connector%GWNodeList(indxStrm)%iGWNodes(indxGW) .EQ. iNode) THEN
                IF (Connector%GWNodeList(indxStrm)%iLayers(indxGW) .EQ. iLayer) Flow = Flow + Connector%GWNodeList(indxStrm)%StrmGWFlow(indxGW)
            END IF
        END DO
    END DO
    
  END FUNCTION StrmGWConnector_v42_GetFlowAtGWNode
    
    
  ! -------------------------------------------------------------
  ! --- GET TOTAL NUMBER OF GW NODES  
  ! -------------------------------------------------------------
  FUNCTION GetnTotalGWNodes(Connector) RESULT(nTotalGWNodes)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    INTEGER                                    :: nTotalGWNodes
    
    nTotalGWNodes = Connector%nTotalGWNodes
    
  END FUNCTION GetnTotalGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW NODES 
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_GetAllGWNodes(Connector,GWNodes)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    INTEGER,ALLOCATABLE                        :: GWNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode,indx,iCount
    
    !Initialize
    DEALLOCATE (GWNodes , STAT=ErrorCode)
    
    !First count how many gw nodes in total
    ALLOCATE (GWNodes(Connector%nTotalGWNodes))
    
    !Compile gw nodes
    iCount = 0
    DO indx=1,SIZE(Connector%GWNodeList)
        GWNodes(iCount+1:iCount+Connector%GWNodeList(indx)%nGWNodes) = Connector%GWNodeList(indx)%iGWNodes
        iCount                                                       = iCount + Connector%GWNodeList(indx)%nGWNodes
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_GetAllGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET GW NODE(S) AT A STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE GetGWNodesAtStrmNode(Connector,iStrmNode,iGWNodes)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                         :: iStrmNode
    INTEGER,ALLOCATABLE                        :: iGWNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (iGWNodes , STAT=ErrorCode)
    ALLOCATE (iGWNodes(Connector%GWNodeList(iStrmNode)%nGWNodes))
    
    iGWNodes = Connector%GWNodeList(iStrmNode)%iGWNodes

  END SUBROUTINE GetGWNodesAtStrmNode
  
 
  ! -------------------------------------------------------------
  ! --- GET ALL LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_GetAllLayers(Connector,Layers)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    INTEGER,ALLOCATABLE                        :: Layers(:)
    
    !Local variables
    INTEGER :: ErrorCode,indx,iCount
    
    !Initialize
    DEALLOCATE (Layers , STAT=ErrorCode)

    !First count how many gw nodes in total
    ALLOCATE (Layers(Connector%nTotalGWNodes))
    
    !Compile layers
    iCount = 0
    DO indx=1,SIZE(Connector%GWNodeList)
        Layers(iCount+1:iCount+Connector%GWNodeList(indx)%nGWNodes) = Connector%GWNodeList(indx)%iLayers
        iCount                                                      = iCount + Connector%GWNodeList(indx)%nGWNodes
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_GetAllLayers
  
 
  ! -------------------------------------------------------------
  ! --- GET GW HEADS AT STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_GetGWHeadsAtStrmNodes(Connector,GWHeads,GWHeadsAtStrmNodes)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    REAL(8),INTENT(IN)                         :: GWHeads(:,:)
    REAL(8),INTENT(OUT)                        :: GWHeadsAtStrmNodes(:)
    
    !Local variables
    INTEGER :: indx,iCount,indxGW,nGWNodes
    
    iCount = 0
    DO indx=1,SIZE(Connector%GWNodeList)
        nGWNodes = Connector%GWNodeList(indx)%nGWNodes
        DO indxGW=1,nGWNodes
            iCount                     = iCount + 1
            GWHeadsAtStrmNodes(iCount) = GWHeads(Connector%GWNodeList(indx)%iGWNodes(indxGW),Connector%GWNodeList(indx)%iLayers(indxGW))
        END DO
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_GetGWHeadsAtStrmNodes 

  
  
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
  ! --- COMPILE CONDUCTANCE FOR STREAM-GW CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_CompileConductance(Connector,InFile,AppGrid,NStrmNodes,UpstrmNodes,DownstrmNodes,iStat)
    CLASS(StrmGWConnector_v42_Type) :: Connector
    TYPE(GenericFileType)           :: InFile
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    INTEGER,INTENT(IN)              :: NStrmNodes
    INTEGER,INTENT(IN)              :: UpstrmNodes(:),DownstrmNodes(:) 
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38) :: ThisProcedure = ModName // 'StrmGWConnector_v42_CompileConductance'
    INTEGER                      :: indxNode,iGWNode,iElem,iStrmNode,iLoc,nGWNodes,indx1,iGWNode1,indxReach, &
                                    indx,iUpstrmNode,iDownstrmNode,iGWUpstrmNode
    REAL(8)                      :: FACTK,FACTL,DummyArray3(3),WetPerimeter(MaxnGWNodes,NStrmNodes),CA,CB,   &
                                    Conductivity(MaxnGWNodes,NStrmNodes),BedThick(MaxnGWNodes,NStrmNodes),   &
                                    Distance,X(2),Y(2),DummyArray5(5),Length(MaxnGWNodes-1),B_DISTANCE,      &
                                    F_DISTANCE
    CHARACTER                    :: ALine*500
    
    !Read data
    CALL InFile%ReadData(FACTK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    Connector%TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL InFile%ReadData(FACTL,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory for stream-gw interaction at each stream node (represents cumulative of the interaction if stream node intrecats with multiple gw nodes)
    ALLOCATE (Connector%StrmGWFlow(NStrmNodes))
    Connector%StrmGWFlow = 0.0
    
    !Read data
    DO indxNode=1,NStrmNodes
        !Number of gw nodes associated with the stream node
        nGWNodes = Connector%GWNodeList(indxNode)%nGWNodes
        
        !Read data for the stream node
        CALL InFile%ReadData(DummyArray5,iStat)  ;  IF (iStat .EQ. -1) RETURN
        iStrmNode = INT(DummyArray5(1))
        IF (iStrmNode .NE. indxNode) THEN 
            MessageArray(1) = 'Parameters for stream nodes should be entered sequentialy.'
            MessageArray(2) = 'Expected stream node='//TRIM(IntToText(indxNode))
            MessageArray(3) = 'Entered stream node ='//TRIM(IntToText(iStrmNode))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1 
            RETURN
        END IF
        WetPerimeter(1,indxNode) = DummyArray5(2) * FACTL
        iGWNode                  = INT(DummyArray5(3))  
        iLoc                     = LocateInList(iGWNode , Connector%GWNodeList(indxNode)%iGWNodes)
        IF (iLoc .EQ. 0) THEN
            MessageArray(1) = 'Stream bed parameters at stream node '//TRIM(IntToText(iStrmNode))//' are listed for groundwater node '
            MessageArray(2) = TRIM(IntToText(iGWNode))//', but this groundwater node is not associated with the stream node!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Conductivity(iLoc,indxNode) = DummyArray5(4) * FACTK
        BedThick(iLoc,indxNode)     = DummyArray5(5) * FACTL
        DO indx=2,Connector%GWNodeList(indxNode)%nGWNodes
            CALL InFile%ReadData(DummyArray3,iStat)  ;  IF (iStat .EQ. -1) RETURN
            iGWNode = INT(DummyArray3(1))  
            iLoc    = LocateInList(iGWNode , Connector%GWNodeList(indxNode)%iGWNodes)
            IF (iLoc .EQ. 0) THEN
                MessageArray(1) = 'Stream bed parameters at stream node '//TRIM(IntToText(iStrmNode))//' are listed for groundwater node '//TRIM(IntToText(iGWNode))//','
                MessageArray(2) = 'but this groundwater node is not associated with the stream node!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Conductivity(iLoc,indxNode) = DummyArray3(2) * FACTK
            BedThick(iLoc,indxNode)     = DummyArray3(3) * FACTL
        END DO
        
        !Distribute wetted perimeter to multiple gw nodes 
        IF (nGWNodes .GT. 1) THEN
            !Find the effective length of each node 
            Length(1:nGWNodes) = 0.0
            DO indx=1,nGWNodes-1
                iGWNode = Connector%GWNodeList(indxNode)%iGWNodes(indx)
                X(1)    = AppGrid%Node(iGWNode)%X
                Y(1)    = AppGrid%Node(iGWNode)%Y
                DO indx1=indx+1,nGWNodes
                    iGWNode1 = Connector%GWNodeList(indxNode)%iGWNodes(indx1)
                    iElem    = AppGrid%GetElementGivenVertices([iGWNode,iGWNode1])
                    IF (iElem .EQ. 0) CYCLE
                    X(2)     = AppGrid%Node(iGWNode1)%X
                    Y(2)     = AppGrid%Node(iGWNode1)%Y
                    Distance = 0.5 * SQRT((X(1)-X(2))*(X(1)-X(2)) + (Y(1)-Y(2))*(Y(1)-Y(2)))
                    Length(indx)  = Length(indx) + Distance
                    Length(indx1) = Length(indx1) + Distance
                END DO
            END DO
            !Distribute wetted perimeter to each gw node with respect to their associated length
            CALL NormalizeArray(Length(1:nGWNodes))
            WetPerimeter(1:nGWNodes,indxNode) = WetPerimeter(1,indxNode) * Length(1:nGWNodes)
        END IF
        
        !Allocate memory for conductance
        ALLOCATE (Connector%GWNodeList(indxNode)%Conductance(nGWNodes) , Connector%GWNodeList(indxNode)%StrmGWFlow(nGWNodes))
        Connector%GWNodeList(indxNode)%StrmGWFlow(nGWNodes) = 0.0
        
    END DO
    
    !Compute conductance
    DO indxReach=1,SIZE(UpstrmNodes)
        iUpstrmNode   = UpstrmNodes(indxReach)
        iDownstrmNode = DownstrmNodes(indxReach)
        B_DISTANCE    = 0.0
        DO indxNode=iUpstrmNode+1,iDownstrmNode
            nGWNodes = Connector%GWNodeList(indxNode-1)%nGWNodes
            !Effective stream length
            iGWUpstrmNode = Connector%GWNodeList(indxNode-1)%iGWNodes(1)
            iGWNode       = Connector%GWNodeList(indxNode)%iGWNodes(1)
            CA            = AppGrid%Node(iGWUpstrmNode)%X - AppGrid%Node(iGWNode)%X
            CB            = AppGrid%Node(iGWUpstrmNode)%Y - AppGrid%Node(iGWNode)%Y
            F_DISTANCE    = SQRT(CA*CA + CB*CB)/2d0
            !Conductivity
            Connector%GWNodeList(indxNode-1)%Conductance = Conductivity(1:nGWNodes,indxNode-1) * WetPerimeter(1:nGWNodes,indxNode-1) * (B_DISTANCE+F_DISTANCE) / BedThick(1:nGWNodes,indxNode-1)
            !Advance distance
            B_DISTANCE = F_DISTANCE
        END DO
        nGWNodes                                        = Connector%GWNodeList(iDownstrmNode)%nGWNodes
        Connector%GWNodeList(iDownstrmNode)%Conductance = Conductivity(1:nGWNodes,iDownstrmNode) * WetPerimeter(1:nGWNodes,iDownstrmNode) * B_DISTANCE / BedThick(1:nGWNodes,iDownstrmNode)
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_Simulate(Connector,NNodes,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,Matrix,DeltaX,MaxElev)
    CLASS(StrmGWConnector_v42_Type)        :: Connector
    INTEGER,INTENT(IN)                     :: NNodes
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)              !This is not used in this version
    TYPE(MatrixType)                       :: Matrix
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:),MaxElev(:)                 !These are not used in this version
    
    !Local variables
    INTEGER :: indxStrm,iGWNode,NStrmNodes,iNodeIDs_Connect(2),iCompIDs_Connect(2), &
               iCompIDs(2),iNodeIDs(2),indxGW,indx
    REAL(8) :: HeadDiff,Conductance,rInteraction,rUpdateValues(2) 
               
    
    !Initialize
    NStrmNodes = SIZE(Connector%GWNodeList)
    indx       = 0
    
    !Compute stream-gw interaction at each stream node; also update the matrix equation
    DO indxStrm=1,NStrmNodes
        Connector%StrmGWFlow(indxStrm) = 0.0
        
        ASSOCIATE (pData => Connector%GWNodeList(indxStrm))
            DO indxGW=1,pData%nGWNodes
                indx = indx + 1
                
                !Corresponding GW node and conductance
                iGWNode     = (pData%iLayers(indxGW)-1) * NNodes + pData%iGWNodes(indxGW)
                Conductance = pData%Conductance(indxGW)
                
                IF (StrmHead(indxStrm) .GE. StrmBottomElev(indxStrm)) THEN
                    IF (GWHead(indx) .GE. StrmBottomElev(indxStrm)) THEN
                        HeadDiff            = StrmHead(indxStrm) - GWHead(indx)
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
                    IF (GWHead(indx) .GE. StrmBottomElev(indxStrm)) THEN
                        HeadDiff            = StrmBottomElev(indxStrm) - GWHead(indx)
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
                pData%StrmGWFlow(indxGW)       = rInteraction
                Connector%StrmGWFlow(indxStrm) = Connector%StrmGWFlow(indxStrm) + rInteraction
                iCompIDs(1)                    = iStrmComp
                iCompIDs(2)                    = iGWComp
                iNodeIDs(1)                    = indxStrm
                iNodeIDs(2)                    = iGWNode
                rUpdateValues(1)               = rInteraction
                rUpdateValues(2)               = -rInteraction
                CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateValues)
            END DO  
            
        END ASSOCIATE
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_Simulate
  

  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION WITHOUT UPDATING MATRIX
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_ComputeStrmGWFlow_AtMinHead(Connector,Flows,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,DeltaX)
    CLASS(StrmGWConnector_v42_Type)        :: Connector
    REAL(8),INTENT(OUT)                    :: Flows(:)
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)  !StrmHead is not used in this version
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)                  !Not used in this version 
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:)                                !Not used in this version
    
    !Local variables
    INTEGER :: indxStrm,indxGW,indx
    REAL(8) :: HeadDiff
    
    !Initialize
    Flows = 0.0
    indx  = 0
                   
    !Compute stream-gw interaction at each stream node when stream is dry
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        indx = indx + 1
        DO indxGW=1,Connector%GWNodeList(indxStrm)%nGWNodes
            IF (GWHead(indx) .GE. StrmBottomElev(indxStrm)) THEN
                HeadDiff        = StrmBottomElev(indxStrm) - GWHead(indx)
                Flows(indxStrm) = Flows(indxStrm) + Connector%GWNodeList(indxStrm)%Conductance(indxGW) * HeadDiff      
            END IF  
        END DO
    END DO

  END SUBROUTINE StrmGWConnector_v42_ComputeStrmGWFlow_AtMinHead


  ! -------------------------------------------------------------
  ! --- ADD CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_RegisterWithMatrix(Connector,AppGrid,Matrix,iStat)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    TYPE(AppGridTYpe),INTENT(IN)               :: AppGrid
    TYPE(MatrixType)                           :: Matrix
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local varaibles
    INTEGER :: indxNode,GWNodes(MaxnGWNodes),StrmNode(1),NNodes,nGWNodes,indx
    
    !Initialize
    iStat  = 0
    NNodes = AppGrid%NNodes
    
    !Add connectivity
    DO indxNode=1,SIZE(Connector%GWNodeList)
        nGWNodes            = Connector%GWNodeList(indxNode)%nGWNodes
        StrmNode(1)         = indxNode
        GWNodes(1:nGWNodes) = (Connector%GWNodeList(indxNode)%iLayers-1)*NNodes + Connector%GWNodeList(indxNode)%iGWNodes
        CALL Matrix%AddConnectivity(iStrmComp,indxNode,iGWComp,GWNodes(1:nGWNodes),iStat)
        IF (iStat .EQ. -1) RETURN
        DO indx=1,nGWNodes
            CALL Matrix%AddConnectivity(iGWComp,GWNodes(indx),iStrmComp,StrmNode,iStat)
            IF (iStat .EQ. -1) RETURN
        END DO
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_RegisterWithMatrix
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF CONNECTOR RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_ConvertTimeUnit(Connector,NewUnit)
    CLASS(StrmGWConnector_v42_Type) :: Connector
    CHARACTER(LEN=*),INTENT(IN)     :: NewUnit
    
    !Local variables
    INTEGER :: indx
    REAL(8) :: Factor
  
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert conductance time unit
    Factor                           = TimeIntervalConversion(NewUnit,Connector%TimeUnitConductance)
    Connector%TimeUnitConductance    = NewUnit
    DO indx=1,SIZE(Connector%GWNodeList)
        Connector%GWNodeList(indx)%Conductance = Connector%GWNodeList(indx)%Conductance * Factor
    END DO

  END SUBROUTINE StrmGWConnector_v42_ConvertTimeUnit 
  
  

  
END MODULE