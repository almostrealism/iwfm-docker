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
MODULE Class_StrmGWConnector_v42
  USE MessageLogger              , ONLY: SetLastMessage           , &
                                         LogMessage               , &
                                         MessageArray             , &
                                         f_iFatal                 , &
                                         f_iWarn
  USE IOInterface                , ONLY: GenericFileType
  USE TimeSeriesUtilities        , ONLY: TimeIntervalConversion
  USE GeneralUtilities           , ONLY: IntToText                , &
                                         StripTextUntilCharacter  , &
                                         CleanSpecialCharacters   , &
                                         LocateInList             , &
                                         NormalizeArray           , &
                                         ConvertID_To_Index    
  USE Package_Discretization     , ONLY: AppGridType              , &
                                         StratigraphyType
  USE Package_Misc               , ONLY: AbstractFunctionType     , &
                                         f_iStrmComp              , &
                                         f_iGWComp                , &
                                         f_rSmoothMaxP
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnectorType  , &
                                         BaseStrmGWConnector_Kill , &
                                         iDisconnectAtTopOfBed    , &
                                         iDisconnectAtBottomOfBed
  USE Package_Matrix             , ONLY: MatrixType               , &
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
  PUBLIC :: StrmGWConnector_v42_Type  , &
            MaxnGWNodes 
  
  
  ! -------------------------------------------------------------
  ! --- INTERACTING GW NODE LIST
  ! -------------------------------------------------------------
  TYPE GWNodeListType
      INTEGER             :: nGWNodes    = 0    !Number of groundwater nodes interacting with the stream at a stream node
      INTEGER,ALLOCATABLE :: iGWNodes(:)        !List of groundwater nodes; first gw node is used to compute the length along the stream which is used in calculating the conductance
      INTEGER,ALLOCATABLE :: iGWNodeRanks(:)    !Ranking of gw nodes in terms of which order  they will be inundated as stream flow depth increases (not used in v42, but in v4.21) 
      INTEGER,ALLOCATABLE :: iLayers(:)         !Aquifer layer numbers that the stream node interacts with
      REAL(8),ALLOCATABLE :: Conductance(:)     !Stream bed conductance at each groundwater node
      REAL(8),ALLOCATABLE :: rBedThickness(:)   !Stream bed thickness at each groundwater node
      REAL(8),ALLOCATABLE :: rDisconnectElev(:) !Elevation at which stream and gw disconnect
      REAL(8),ALLOCATABLE :: rDistFrac(:)       !Fractions to distribute available stream flow to individual gw nodes 
      REAL(8),ALLOCATABLE :: rFractionForGW(:)  !Fraction of stream-gw interaction to be applied to gw nodes for fractional stream-gw interaction
      REAL(8),ALLOCATABLE :: StrmGWFlow(:)      !Stream-aquifer interaction at each gw node associated with the stream node
  END TYPE GWNodeListType
  
      
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseStrmGWConnectorType) :: StrmGWConnector_v42_Type
      INTEGER                           :: nTotalGWNodes = 0               !Total number of gw nodes (counting doubles at confluences) that interact with streams
      TYPE(GWNodeListType),ALLOCATABLE  :: GWNodeList(:)                   !GW node list interacting with the stream at each stream node
  CONTAINS
      PROCEDURE,PASS :: AddGWNodesToStrmNode
      PROCEDURE,PASS :: Kill                                     => StrmGWConnector_v42_Kill                          !Overrides the original method from base class
      PROCEDURE,PASS :: GetnTotalGWNodes
      PROCEDURE,PASS :: GetGWNodesAtStrmNode
      PROCEDURE,PASS :: GetAllGWNodes                            => StrmGWConnector_v42_GetAllGWNodes                 !Overrides the original method from base class
      PROCEDURE,PASS :: GetAllLayers                             => StrmGWConnector_v42_GetAllLayers                  !Overrides the original method from base class
      PROCEDURE,PASS :: GetFlowAtGWNode                          => StrmGWConnector_v42_GetFlowAtGWNode               !Overrides the original method from base class
      PROCEDURE,PASS :: GetFlowAtAllStrmNodes                    => StrmGWConnector_v42_GetFlowAtAllStrmNodes         !Overrides the original method from base class
      PROCEDURE,PASS :: GetTotalFlowAtAllStrmNodes
      PROCEDURE,PASS :: GetFlowAtSomeStrmNodes                   => StrmGWConnector_v42_GetFlowAtSomeStrmNodes        !Overrides the original method from base class
      PROCEDURE,PASS :: GetSubregionalFlows                      => StrmGWConnector_v42_GetSubregionalFlows           !Overrides the original method from base class
      PROCEDURE,PASS :: GetGWHeadsAtStrmNodes                    => StrmGWConnector_v42_GetGWHeadsAtStrmNodes         !Overrides the original method from base class
      PROCEDURE,PASS :: SetFractionsForGW                        => StrmGWConnector_v42_SetFractionsForGW             !Overrides the original method from base class
      PROCEDURE,PASS :: RegisterWithMatrix                       => StrmGWConnector_v42_RegisterWithMatrix            !Overrides the original method from base class
      PROCEDURE,PASS :: BaseStrmGWConnector_ReadPreprocessedData => StrmGWConnector_v42_ReadPreprocessedData          !Overrides the original method from base class
      PROCEDURE,PASS :: WritePreprocessedData                    => StrmGWConnector_v42_WritePreprocessedData         !Overrides the original method from base class
      PROCEDURE,PASS :: Simulate                                 => StrmGWConnector_v42_Simulate
      PROCEDURE,PASS :: CompileConductance                       => StrmGWConnector_v42_CompileConductance
      PROCEDURE,PASS :: ConvertTimeUnit                          => StrmGWConnector_v42_ConvertTimeUnit               !Overrides the original method from base class
  END TYPE StrmGWConnector_v42_Type
    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: MaxnGWNodes = 1000   !Maximum number of groundwater nodes that can be associated with a stream node
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
  SUBROUTINE AddGWNodesToStrmNode(Connector,NStrmNodes,iStrmNode,iGWNodes,iLayers,iRanks)
    CLASS(StrmGWConnector_v42_Type) :: Connector
    INTEGER,INTENT(IN)              :: NStrmNodes,iStrmNode,iGWNodes(:),iLayers(:)
    INTEGER,INTENT(IN)              :: iRanks(:)                                   !Not used in this version
    
    !Local variables
    INTEGER             :: nGWNodes,iDim
    INTEGER,ALLOCATABLE :: iTempGWNodes(:),iTempLayers(:)
    REAL(8),ALLOCATABLE :: rTempFractions(:)
    
    !Add data to stream
    IF (ALLOCATED(Connector%GWNodeList)) THEN
        ASSOCIATE (pData => Connector%GWNodeList(iStrmNode))
            nGWNodes = pData%nGWNodes
            iDim     = nGWNodes + SIZE(iGWNodes)
            ALLOCATE (iTempGWNodes(iDim) , iTempLayers(iDim) , rTempFractions(iDim))
            iTempGWNodes(1:nGWNodes)    = pData%iGWNodes     
            iTempGWNodes(nGWNodes+1:)   = iGWNodes  ;  CALL MOVE_ALLOC(iTempGWNodes   , pData%iGWNodes)
            iTempLayers(1:nGWNodes)     = pData%iLayers                               
            iTempLayers(nGWNodes+1:)    = iLayers   ;  CALL MOVE_ALLOC(iTempLayers    , pData%iLayers)
            rTempFractions(1:nGWNodes)  = pData%rFractionForGW
            rTempFractions(nGWNodes+1:) = 1.0       ;  CALL MOVE_ALLOC(rTempFractions , pData%rFractionForGW) 
            pData%nGWNodes              = iDim
        END ASSOCIATE
        
    ELSE
        ALLOCATE (Connector%GWNodeList(NStrmNodes))
        ASSOCIATE (pData => Connector%GWNodeList(iStrmNode))
            nGWNodes = SIZE(iGWNodes)
            ALLOCATE (pData%iGWNodes(nGWNodes) , pData%iLayers(nGWNodes) , pData%rFractionForGW(nGWNodes)) 
            pData%nGWNodes       = nGWNodes
            pData%iGWNodes       = iGWNodes
            pData%iLayers        = iLayers
            pData%rFractionForGW = 1.0     !This is the deafult
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
        CALL SetLastMessage('Error in allocating memory for stream-gw connection data!'//NEW_LINE('x')//TRIM(cErrMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    DO indx=1,nStrmNodes
        CALL Infile%ReadData(nGWNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Connector%GWNodeList(indx)%nGWNodes = nGWNodes
        ALLOCATE (Connector%GWNodeList(indx)%iGWNodes(nGWNodes) , Connector%GWNodeList(indx)%iLayers(nGWNodes) , Connector%GWNodeList(indx)%rFractionForGW(nGWNodes))
        CALL InFile%ReadData(Connector%GWNodeList(indx)%iGWNodes,iStat)          ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(Connector%GWNodeList(indx)%iLayers,iStat)           ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(Connector%GWNodeList(indx)%rFractionForGW,iStat)    ;  IF (iStat .EQ. -1) RETURN
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
        DEALLOCATE (Connector%GWNodeList(indxStrm)%iGWNodes       , &
                    Connector%GWNodeList(indxStrm)%iLayers        , &
                    Connector%GWNodeList(indxStrm)%Conductance    , &
                    Connector%GWNodeList(indxStrm)%rDisconnectElev, &
                    Connector%GWNodeList(indxStrm)%rDistFrac      , &
                    Connector%GWNodeList(indxStrm)%rFractionForGW , &
                    Connector%GWNodeList(indxStrm)%StrmGWFlow     , &
                    STAT=ErrorCode                                )
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
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%rFractionForGW)
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
  FUNCTION StrmGWConnector_v42_GetSubregionalFlows(Connector,AppGrid,lInsideModel) RESULT(Flows)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)               :: AppGrid
    LOGICAL,INTENT(IN)                         :: lInsideModel
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
        IF (lInsideModel) THEN
            StrmGWFlows(indx+1:indx+nGWNodes) = Connector%GWNodeList(indxStrm)%StrmGWFlow * Connector%GWNodeList(indxStrm)%rFractionForGW
        ELSE
            StrmGWFlows(indx+1:indx+nGWNodes) = Connector%GWNodeList(indxStrm)%StrmGWFlow * (1D0 - Connector%GWNodeList(indxStrm)%rFractionForGW)
        END IF
        indx = indx + nGWNodes
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
                IF (Connector%GWNodeList(indxStrm)%iLayers(indxGW) .EQ. iLayer) Flow = Flow + Connector%GWNodeList(indxStrm)%StrmGWFlow(indxGW) * Connector%GWNodeList(indxStrm)%rFractionForGW(indxGW)
            END IF
        END DO
    END DO
    
  END FUNCTION StrmGWConnector_v42_GetFlowAtGWNode
    
    
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT EVERY STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE GetTotalFlowAtAllStrmNodes(Connector,Flow)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    REAL(8),INTENT(OUT)                        :: Flow(:) 
    
    Flow = Connector%StrmGWFlow 
      
  END SUBROUTINE GetTotalFlowAtAllStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM-GW FLOW AT EVERY STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_GetFlowAtAllStrmNodes(Connector,lInsideModel,Flow)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    LOGICAL,INTENT(IN)                         :: lInsideModel
    REAL(8),INTENT(OUT)                        :: Flow(:) 
    
    !Local variables
    INTEGER :: indxStrm
    
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        IF (lInsideModel) THEN
            Flow(indxStrm) = SUM(Connector%GWNodeList(indxStrm)%StrmGWFlow * Connector%GWNodeList(indxStrm)%rFractionForGW)
        ELSE
            Flow(indxStrm) = SUM(Connector%GWNodeList(indxStrm)%StrmGWFlow * (1D0 - Connector%GWNodeList(indxStrm)%rFractionForGW))
        END IF
    END DO
      
  END SUBROUTINE StrmGWConnector_v42_GetFlowAtAllStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT A SET OF STREAM NODES 
  ! -------------------------------------------------------------
  FUNCTION StrmGWConnector_v42_GetFlowAtSomeStrmNodes(Connector,iNodeBegin,iNodeEnd,lInsideModel) RESULT(Flow)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                        :: iNodeBegin,iNodeEnd
    LOGICAL,INTENT(IN)                        :: lInsideModel
    REAL(8)                                   :: Flow 
    
    !Local variables
    INTEGER :: indxStrm
    
    !Initialize
    Flow = 0.0
    
    IF (lInsideModel) THEN
        DO indxStrm=iNodeBegin,iNodeEnd
            Flow = Flow + SUM(Connector%GWNodeList(indxStrm)%StrmGWFlow * Connector%GWNodeList(indxStrm)%rFractionForGW)
        END DO
    ELSE
        DO indxStrm=iNodeBegin,iNodeEnd
            Flow = Flow + SUM(Connector%GWNodeList(indxStrm)%StrmGWFlow * (1D0 - Connector%GWNodeList(indxStrm)%rFractionForGW))
        END DO
    END IF
  
  END FUNCTION StrmGWConnector_v42_GetFlowAtSomeStrmNodes
  
  
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
  PURE SUBROUTINE GetGWNodesAtStrmNode(Connector,iStrmNode,iGWNodes)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                         :: iStrmNode
    INTEGER,ALLOCATABLE,INTENT(INOUT)          :: iGWNodes(:)
    
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
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- SET STREAM NODES AND FRACTION OF STREAM-GW INTERACTION TO BE APPLIED TO GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_SetFractionsForGW(Connector,iStrmNodes,rFractions,iStat)
    CLASS(StrmGWConnector_v42_Type) :: Connector
    INTEGER,INTENT(IN)              :: iStrmNodes(:)
    REAL(8),INTENT(IN)              :: rFractions(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+37),PARAMETER :: ThisProcedure = ModName // 'StrmGWConnector_v42_SetFractionsForGW'
    INTEGER                                :: indx,indx1
    
    !Initailize
    iStat = 0
    
    !Check for errors
    DO indx=1,SIZE(iStrmNodes)
        !Make sure the same stream node is not listed more than once
        DO indx1=indx+1,SIZE(iStrmNodes)
            IF (iStrmNodes(indx) .EQ. iStrmNodes(indx1)) THEN
                MessageArray(1) = 'Stream node '// TRIM(IntToText(iStrmNodes(indx))) // ' is listed more than once for defining '
                MessageArray(2) = 'fraction of the stream-aquifer interaction to be applied to the corresponding groundwater node.'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Check that the fractions are less than or equal to 1.0
        IF (rFractions(indx).GT.1.0  .OR.  rFractions(indx).LT.0.0) THEN
            MessageArray(1) = 'Fraction of stream-aquifer interaction at stream node ' // TRIM(IntToText(iStrmNodes(indx))) 
            MessageArray(2) = ' to be applied to the corresponding groundwater node must be between 0.0 and 1.0.' 
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Store information
        Connector%GWNodeList(iStrmNodes(indx))%rFractionForGW(1) = rFractions(indx)
    END DO
     
    
  END SUBROUTINE StrmGWConnector_v42_SetFractionsForGW

  

  
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
  SUBROUTINE StrmGWConnector_v42_CompileConductance(Connector,InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
    CLASS(StrmGWConnector_v42_Type)   :: Connector
    TYPE(GenericFileType)             :: InFile
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes),UpstrmNodes(:),DownstrmNodes(:) 
    REAL(8),INTENT(IN)                :: BottomElevs(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38) :: ThisProcedure = ModName // 'StrmGWConnector_v42_CompileConductance'
    INTEGER                      :: indxNode,iGWNodeID,iElem,iStrmNodeID,iLoc,nGWNodes,indx1,iGWNode1,indxReach, &
                                    indx,iUpstrmNode,iDownstrmNode,iGWUpstrmNode,iLayer,iNode,iGWNode,           &
                                    iGWNodeIDs(AppGrid%NNodes),iInteractionType
    REAL(8)                      :: FACTK,FACTL,DummyArray3(3),WetPerimeter(MaxnGWNodes,NStrmNodes),CA,CB,       &
                                    Conductivity(MaxnGWNodes,NStrmNodes),BedThick(MaxnGWNodes,NStrmNodes),       &
                                    Distance,X(2),Y(2),DummyArray5(5),Length(MaxnGWNodes-1),B_DISTANCE,          &
                                    F_DISTANCE,rSumConductance
    CHARACTER                    :: ALine*1000
    LOGICAL                      :: lProcessed(NStrmNodes)

    !Initialize
    lProcessed = .FALSE.
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Read data
    CALL InFile%ReadData(FACTK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    Connector%TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL InFile%ReadData(FACTL,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory for stream bed thickness and stream-gw interaction at each stream node (represents cumulative of the interaction if stream node intrecats with multiple gw nodes)
    ALLOCATE (Connector%StrmGWFlow(NStrmNodes))
    Connector%StrmGWFlow = 0.0
    
    !Read data
    DO indxNode=1,NStrmNodes
        !Read data for the stream node
        CALL InFile%ReadData(DummyArray5,iStat)  ;  IF (iStat .EQ. -1) RETURN
        iStrmNodeID = INT(DummyArray5(1))
        CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,iNode)
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Stream node '//TRIM(IntToText(iStrmNodeID))//' listed for stream bed parameters is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Check if this node was processed before
        IF (lProcessed(iNode)) THEN
            CALL SetLastMessage('Stream bed parameters for stream node '//TRIM(IntToText(iStrmNodeID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iNode) = .TRUE.
        
        !Number of gw nodes associated with the stream node
        nGWNodes = Connector%GWNodeList(iNode)%nGWNodes
        
        !Allocate memory for connector data at stream node
        ALLOCATE (Connector%GWNodeList(iNode)%Conductance(nGWNodes) , Connector%GWNodeList(iNode)%StrmGWFlow(nGWNodes)  ,  Connector%GWNodeList(iNode)%rBedThickness(nGWNodes) ,  Connector%GWNodeList(iNode)%rDisconnectElev(nGWNodes) ,  Connector%GWNodeList(iNode)%rDistFrac(nGWNodes))
        Connector%GWNodeList(iNode)%StrmGWFlow(nGWNodes) = 0.0
        
        !Parameters
        WetPerimeter(1,iNode) = DummyArray5(2) * FACTL
        iGWNodeID             = INT(DummyArray5(3))
        CALL ConvertID_To_Index(iGWNodeID,iGWNodeIDs,iGWNode)
        IF (iGWNode .EQ. 0) THEN
            CALL SetLastMessage('Groundwater node '//TRIM(IntToText(iGWNodeID))//' listed for stream node '//TRIM(IntToText(iStrmNodeID))//' for stream bed parameters is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iLoc = LocateInList(iGWNode , Connector%GWNodeList(iNode)%iGWNodes)
        IF (iLoc .EQ. 0) THEN
            MessageArray(1) = 'Stream bed parameters at stream node '//TRIM(IntToText(iStrmNodeID))//' are listed for groundwater node '
            MessageArray(2) = TRIM(IntToText(iGWNodeID))//', but this groundwater node is not associated with the stream node!'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Conductivity(iLoc,iNode) = DummyArray5(4) * FACTK
        BedThick(iLoc,iNode)     = DummyArray5(5) * FACTL
        DO indx=2,Connector%GWNodeList(iNode)%nGWNodes
            CALL InFile%ReadData(DummyArray3,iStat)  ;  IF (iStat .EQ. -1) RETURN
            iGWNodeID = INT(DummyArray3(1))  
            CALL ConvertID_To_Index(iGWNodeID,iGWNodeIDs,iGWNode)
            iLoc = LocateInList(iGWNode , Connector%GWNodeList(iNode)%iGWNodes)
            IF (iLoc .EQ. 0) THEN
                MessageArray(1) = 'Stream bed parameters at stream node '//TRIM(IntToText(iStrmNodeID))//' are listed for groundwater node '//TRIM(IntToText(iGWNodeID))//','
                MessageArray(2) = 'but this groundwater node is not associated with the stream node!'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Conductivity(iLoc,iNode) = DummyArray3(2) * FACTK
            BedThick(iLoc,iNode)     = DummyArray3(3) * FACTL
        END DO
        
        !Distribute wetted perimeter to multiple gw nodes 
        IF (nGWNodes .GT. 1) THEN
            !Find the effective length of each node 
            Length(1:nGWNodes) = 0.0
            DO indx=1,nGWNodes-1
                iGWNode = Connector%GWNodeList(iNode)%iGWNodes(indx)
                X(1)    = AppGrid%X(iGWNode)
                Y(1)    = AppGrid%Y(iGWNode)
                DO indx1=indx+1,nGWNodes
                    iGWNode1 = Connector%GWNodeList(iNode)%iGWNodes(indx1)
                    iElem    = AppGrid%GetElementGivenVertices([iGWNode,iGWNode1])
                    IF (iElem .EQ. 0) CYCLE
                    X(2)     = AppGrid%X(iGWNode1)
                    Y(2)     = AppGrid%Y(iGWNode1)
                    Distance = 0.5 * SQRT((X(1)-X(2))*(X(1)-X(2)) + (Y(1)-Y(2))*(Y(1)-Y(2)))
                    Length(indx)  = Length(indx) + Distance
                    Length(indx1) = Length(indx1) + Distance
                END DO
            END DO
            !Distribute wetted perimeter to each gw node with respect to their associated length
            CALL NormalizeArray(Length(1:nGWNodes))
            WetPerimeter(1:nGWNodes,iNode) = WetPerimeter(1,iNode) * Length(1:nGWNodes)
        END IF
    END DO
    
    !Assumption for stream-aquifer disconnection; BACKWARD COMPATIBILITY: Check if this part of the data file exists
    CALL InFile%ReadData(iInteractionType,iStat)
    IF (iStat .EQ. 0) THEN
        CALL Connector%SetInteractionType(iInteractionType,iStat) 
        IF (iStat .EQ. -1) RETURN
    ELSE
        iStat = 0
    END IF
    
    !Update BedThick if necessary
    IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
        DO indxNode=1,NStrmNodes
            DO indx=1,Connector%GWNodeList(indxNode)%nGWNodes
                iGWNode = Connector%GWNodeList(indxNode)%iGWNodes(indx)
                iLayer  = Connector%GWNodeList(indxNode)%iLayers(indx)
                IF (BottomElevs(indxNode)-BedThick(indx,indxNode) .LT. Stratigraphy%BottomElev(iGWNode,iLayer)) THEN
                    BedThick(indx,indxNode) = BottomElevs(indxNode) - Stratigraphy%BottomElev(iGWNode,iLayer)
                    iStrmNodeID             = iStrmNodeIDs(indxNode)
                    iGWNodeID               = iGWNodeIDs(iGWNode)
                    !Warn the user about the modification
                    MessageArray(1)         = 'Stream bed thickness at stream node ' // TRIM(IntToText(iStrmNodeID)) // ' and GW node '// TRIM(IntToText(iGWNodeID)) // ' penetrates into second active aquifer layer!'
                    MessageArray(2)         = 'It is adjusted to penetrate only into the top active layer.'
                    CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure) 
                    !Check that bed thickness is not zero or less
                    IF (BedThick(indx,indxNode) .LE. 0.0) THEN
                        MessageArray(1) = 'Stream bed thickness at stream node ' // TRIM(IntToText(iStrmNodeID)) // ' and GW node '// TRIM(IntToText(iGWNodeID)) // ' is less than or equal to zero!'
                        MessageArray(2) = 'Check the startigraphy and bed thickness at this location.'
                        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure) 
                        iStat = -1
                        RETURN
                    END IF
                END IF
            END DO
        END DO
    END IF
        
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
            CA            = AppGrid%X(iGWUpstrmNode) - AppGrid%X(iGWNode)
            CB            = AppGrid%Y(iGWUpstrmNode) - AppGrid%Y(iGWNode)
            F_DISTANCE    = SQRT(CA*CA + CB*CB)/2d0
            !Conductivity
            Connector%GWNodeList(indxNode-1)%Conductance = Conductivity(1:nGWNodes,indxNode-1) * WetPerimeter(1:nGWNodes,indxNode-1) * (B_DISTANCE+F_DISTANCE) / BedThick(1:nGWNodes,indxNode-1)
            !Stream flow distribution factors 
            rSumConductance = SUM(Connector%GWNodeList(indxNode-1)%Conductance)
            IF (rSumConductance .EQ. 0.0) THEN
                Connector%GWNodeList(indxNode-1)%rDistFrac = 1D0 / REAL(nGWNodes,8)
            ELSE
                Connector%GWNodeList(indxNode-1)%rDistFrac = Connector%GWNodeList(indxNode-1)%Conductance / rSumConductance
            END IF
            !Bed thickness
            Connector%GWNodeList(indxNode-1)%rBedThickness = BedThick(1:nGWNodes,indxNode-1)
            !Advance distance
            B_DISTANCE = F_DISTANCE
        END DO
        nGWNodes                                          = Connector%GWNodeList(iDownstrmNode)%nGWNodes
        Connector%GWNodeList(iDownstrmNode)%Conductance   = Conductivity(1:nGWNodes,iDownstrmNode) * WetPerimeter(1:nGWNodes,iDownstrmNode) * B_DISTANCE / BedThick(1:nGWNodes,iDownstrmNode)
        Connector%GWNodeList(iDownstrmNode)%rBedThickness = BedThick(1:nGWNodes,iDownstrmNode)
        rSumConductance                                   = SUM(Connector%GWNodeList(iDownstrmNode)%Conductance)
        IF (rSumConductance .EQ. 0.0) THEN
            Connector%GWNodeList(iDownstrmNode)%rDistFrac = 1D0 / REAL(nGWNodes,8)
        ELSE
            Connector%GWNodeList(iDownstrmNode)%rDistFrac = Connector%GWNodeList(iDownstrmNode)%Conductance / rSumConductance
        END IF
    END DO
    
    !Compute elevation where stream and gw disconnect
    DO indxNode=1,NStrmNodes
        IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
            Connector%GWNodeList(indxNode)%rDisconnectElev = BottomElevs(indxNode) - Connector%GWNodeList(indxNode)%rBedThickness
        ELSE
            Connector%GWNodeList(indxNode)%rDisconnectElev = BottomElevs(indxNode)
        END IF
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE STREAM-GW INTERACTION
  ! --- *** Note: + flow means losing stream
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_Simulate(Connector,iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
    CLASS(StrmGWConnector_v42_Type)                 :: Connector
    INTEGER,INTENT(IN)                              :: iNNodes
    REAL(8),INTENT(IN)                              :: rGWHeads(:),rStrmHeads(:),rAvailableFlows(:)
    TYPE(MatrixType)                                :: Matrix
    CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)  !Not used in this version                  
    REAL(8),OPTIONAL,INTENT(IN)                     :: rMaxElevs(:)             !Not used in this version
    
    !Local variables
    INTEGER           :: iGWNode,iNodes_Connect(2),iNodes_RHS(500),indxGW,indx,nGWNodes,iOffset,indxStrm
    REAL(8)           :: Conductance,rUpdateCOEFF(2),rUpdateCOEFF_Keep(2),rUpdateRHS(500),rDiff_GW,rGWHead,     &
                         rDiffGWSQRT,rStrmGWFlow,rStrmGWFlowAdj,rStrmGWFlowAdjSQRT,rDStrmGWFlowAdj,rStrmHead,   &
                         rNodeAvailableFlow
    INTEGER,PARAMETER :: iCompIDs_RHS(500)   = [f_iStrmComp , (f_iGWComp,indx=2,500)]  , &
                         iCompIDs_Connect(2) = [f_iStrmComp , f_iGWComp]
                             
    !Update matrix equations
    iOffSet = 0
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        rStrmHead = rStrmHeads(indxStrm)
        
        ASSOCIATE (pData => Connector%GWNodeList(indxStrm))
            iNodes_RHS(1) = indxStrm
            nGWNodes      = pData%nGWNodes

            !Loop over each connected gw node
            DO indxGW=1,nGWNodes
                !Corresponding GW node and conductance
                Conductance = pData%Conductance(indxGW)
                IF (Conductance .EQ. 0.0) THEN
                    pData%StrmGWFlow(indxGW) = 0.0
                    CYCLE
                END IF
                iGWNode              = (pData%iLayers(indxGW)-1) * iNNodes + pData%iGWNodes(indxGW)
                iNodes_RHS(1+indxGW) = iGWNode

                !Head differences
                rGWHead     = rGWHeads(iOffset+indxGW)
                rDiff_GW    = rGWHead - pData%rDisconnectElev(indxGW)
                rDiffGWSQRT = SQRT(rDiff_GW*rDiff_GW + f_rSmoothMaxP)
                
                !Available flow for node
                rNodeAvailableFlow = rAvailableFlows(indxStrm) * pData%rDistFrac(indxGW)
                
                !Calculate stream-gw interaction and update of Jacobian
                !--------------------------------------------
                rStrmGWFlow = Conductance * (rStrmHead - MAX(rGWHead,pData%rDisconnectElev(indxGW)))
                !Stream is gaining; no need to worry about drying stream (i.e. stream-gw flow is not a function of upstream flows)
                IF (rStrmGWFlow .LT. 0.0) THEN
                    pData%StrmGWFlow(indxGW) = rStrmGWFlow
                    iNodes_Connect(1)        = indxStrm
                    iNodes_Connect(2)        = iGWNode
                    
                    !Update Jacobian - entries for stream node 
                    rUpdateCOEFF_Keep(1) = Conductance
                    rUpdateCOEFF_Keep(2) = -0.5d0 * Conductance * (1d0+rDiff_GW/rDiffGWSQRT) 
                    rUpdateCOEFF         = rUpdateCOEFF_Keep
                    CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs_Connect,iNodes_Connect,rUpdateCOEFF)
                    
                    !Update Jacobian - entries for groundwater node
                    rUpdateCOEFF = -pData%rFractionForGW(indxGW) * rUpdateCOEFF_Keep
                    CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs_Connect,iNodes_Connect,rUpdateCOEFF)
                                        
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
                    CALL Matrix%UpdateCOEFF(f_iStrmComp,indxStrm,2,iCompIDs_Connect,iNodes_Connect,rUpdateCOEFF)
                    
                    !Update Jacobian - entries for groundwater node
                    rUpdateCOEFF = -pData%rFractionForGW(indxGW) * rUpdateCOEFF_Keep
                    CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs_Connect,iNodes_Connect,rUpdateCOEFF)
                    
                    !Store flow exchange
                    pData%StrmGWFlow(indxGW) = MIN(rNodeAvailableFlow , rStrmGWFlow)
                END IF

            END DO

            !Total stream-gw interaction
            Connector%StrmGWFlow(indxStrm) = SUM(pData%StrmGWFlow)
            
            !Update RHS 
            rUpdateRHS(1)            = Connector%StrmGWFlow(indxStrm)
            rUpdateRHS(2:1+nGWNodes) = -pData%StrmGWFlow * pData%rFractionForGW
            CALL Matrix%UpdateRHS(iCompIDs_RHS(1:1+nGWNodes),iNodes_RHS(1:1+nGWNodes),rUpdateRHS(1:1+nGWNodes))
        END ASSOCIATE
        
        !Update iOffset
        iOffset = iOffset + nGWNodes
    END DO
    
  END SUBROUTINE StrmGWConnector_v42_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- ADD CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v42_RegisterWithMatrix(Connector,StrmConnectivity,AppGrid,Matrix,iStat)
    CLASS(StrmGWConnector_v42_Type),INTENT(IN) :: Connector
    TYPE(ConnectivityListType),INTENT(IN)      :: StrmConnectivity(:)
    TYPE(AppGridTYpe),INTENT(IN)               :: AppGrid
    TYPE(MatrixType)                           :: Matrix
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local varaibles
    INTEGER :: indxNode,GWNodes(MaxnGWNodes),StrmNode(20),NNodes,nGWNodes,indx,iNUpstrmNodes
    
    !Initialize
    iStat  = 0
    NNodes = AppGrid%NNodes
    
    !Add connectivity
    DO indxNode=1,SIZE(Connector%GWNodeList)
        nGWNodes            = Connector%GWNodeList(indxNode)%nGWNodes
        GWNodes(1:nGWNodes) = (Connector%GWNodeList(indxNode)%iLayers-1)*NNodes + Connector%GWNodeList(indxNode)%iGWNodes
        CALL Matrix%AddConnectivity(f_iStrmComp,indxNode,f_iGWComp,GWNodes(1:nGWNodes),iStat)
        IF (iStat .EQ. -1) RETURN
        iNUpstrmNodes = StrmConnectivity(indxNode)%nConnectedNodes
        StrmNode(1)   = indxNode
        DO indx=1,nGWNodes
            StrmNode(2:iNUpstrmNodes+1) = StrmConnectivity(indxNode)%ConnectedNodes
            CALL Matrix%AddConnectivity(f_iGWComp,GWNodes(indx),f_iStrmComp,StrmNode(1:iNUpstrmNodes+1),iStat)
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