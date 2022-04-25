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
MODULE Class_StrmGWConnector_v421
  USE MessageLogger              , ONLY: SetLastMessage            , &
                                         LogMessage                , & 
                                         MessageArray              , &
                                         f_iWarn                   , &
                                         f_iFatal 
  USE GeneralUtilities           , ONLY: StripTextUntilCharacter   , &
                                         IntToText                 , &
                                         CleanSpecialCharacters    , &
                                         LocateInList              , &
                                         NormalizeArray            , &
                                         ConvertID_to_Index
  USE IOInterface                , ONLY: GenericFileType
  USE Package_Discretization     , ONLY: AppGridType               , &
                                         StratigraphyType
  USE Package_Misc               , ONLY: AbstractFunctionType      , &
                                         f_iStrmComp               , &
                                         f_iGWComp                 , &
                                         f_rSmoothMaxP
  USE Class_BaseStrmGWConnector  , ONLY: BaseStrmGWConnector_Kill  , &
                                         iDisconnectAtBottomOfBed
  USE Class_StrmGWConnector_v42  , ONLY: StrmGWConnector_v42_Type  , &
                                         MaxnGWNodes
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
  PUBLIC :: StrmGWConnector_v421_Type                    
  
  
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! --- Note: For this version:
  ! ---       Conductance = unit conductance per length of the stream cross-section 
  ! ---       rDistFrac   = length of cross-section associated with each gw node connected to a stream node
  ! -------------------------------------------------------------
  TYPE,EXTENDS(StrmGWConnector_v42_Type) :: StrmGWConnector_v421_Type
      PRIVATE
  CONTAINS
      PROCEDURE,PASS :: AddGWNodesToStrmNode                     => StrmGWConnector_v421_AddGWNodesToStrmNode    !Overrides the original method from parent class
      PROCEDURE,PASS :: BaseStrmGWConnector_ReadPreprocessedData => StrmGWConnector_v421_ReadPreprocessedData    !Overrides the original method from base class
      PROCEDURE,PASS :: Kill                                     => StrmGWConnector_v421_Kill                    !Overrides the original method from parent class
      PROCEDURE,PASS :: WritePreprocessedData                    => StrmGWConnector_v421_WritePreprocessedData   !Overrides the original method from parent class
      PROCEDURE,PASS :: CompileConductance                       => StrmGWConnector_v421_CompileConductance      !Overrides the original method from parent class
      PROCEDURE,PASS :: Simulate                                 => StrmGWConnector_v421_Simulate                !Overrides the original method from parent class
  END TYPE StrmGWConnector_v421_Type
    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen  = 28
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'Class_StrmGWConnector_v421::'
  
  
  
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
  SUBROUTINE StrmGWConnector_v421_AddGWNodesToStrmNode(Connector,NStrmNodes,iStrmNode,iGWNodes,iLayers,iRanks)
    CLASS(StrmGWConnector_v421_Type) :: Connector
    INTEGER,INTENT(IN)               :: NStrmNodes,iStrmNode,iGWNodes(:),iLayers(:),iRanks(:)
    
    !Local variables
    INTEGER :: nGWNodes
    
    !Allocate GW node list, if not allocated
    IF (.NOT. ALLOCATED(Connector%GWNodeList)) ALLOCATE (Connector%GWNodeList(NStrmNodes))
    
    !Add data to stream
    ASSOCIATE (pData => Connector%GWNodeList(iStrmNode))
        nGWNodes = SIZE(iGWNodes)
        ALLOCATE (pData%iGWNodes(nGWNodes)      , &
                  pData%iLayers(nGWNodes)       , &
                  pData%iGWNodeRanks(nGWNodes)  , &
                  pData%rFractionForGW(nGWNodes)) 
        pData%nGWNodes       = nGWNodes
        pData%iGWNodes       = iGWNodes
        pData%iLayers        = iLayers
        pData%iGWNodeRanks   = iRanks
        pData%rFractionForGW = 1.0     !This is the default
    END ASSOCIATE
    
    Connector%nTotalGWNodes = Connector%nTotalGWNodes + nGWNodes
       
  END SUBROUTINE StrmGWConnector_v421_AddGWNodesToStrmNode
  
  
  ! -------------------------------------------------------------
  ! --- READ PRE-PROCESSED STREAM-GW CONNECTOR DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v421_ReadPreprocessedData(Connector,InFile,iStat)
    CLASS(StrmGWConnector_v421_Type) :: Connector
    TYPE(GenericFileType)            :: InFile
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+41) :: ThisProcedure = ModName // 'StrmGWConnector_v421_ReadPreprocessedData'
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
        ALLOCATE (Connector%GWNodeList(indx)%iGWNodes(nGWNodes)       , &
                  Connector%GWNodeList(indx)%iLayers(nGWNodes)        , &
                  Connector%GWNodeList(indx)%iGWNodeRanks(nGWNodes)   , &
                  Connector%GWNodeList(indx)%rFractionForGW(nGWNodes) )
        CALL InFile%ReadData(Connector%GWNodeList(indx)%iGWNodes,iStat)          ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(Connector%GWNodeList(indx)%iLayers,iStat)           ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(Connector%GWNodeList(indx)%iGWNodeRanks,iStat)      ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(Connector%GWNodeList(indx)%rFractionForGW,iStat)    ;  IF (iStat .EQ. -1) RETURN
    END DO
        
  END SUBROUTINE StrmGWConnector_v421_ReadPreprocessedData
  
  
  

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
  SUBROUTINE StrmGWConnector_v421_Kill(Connector)
    CLASS(StrmGWConnector_v421_Type) :: Connector
    
    !Local variables
    INTEGER                         :: ErrorCode,indxStrm
    TYPE(StrmGWConnector_v421_Type) :: Dummy
    
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        DEALLOCATE (Connector%GWNodeList(indxStrm)%iGWNodes       , &
                    Connector%GWNodeList(indxStrm)%iLayers        , &
                    Connector%GWNodeList(indxStrm)%iGWNodeRanks   , &
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
    
  END SUBROUTINE StrmGWConnector_v421_Kill
  
  
  
  
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
  SUBROUTINE StrmGWConnector_v421_WritePreprocessedData(Connector,OutFile)
    CLASS(StrmGWConnector_v421_Type),INTENT(IN) :: Connector
    TYPE(GenericFileType)                       :: OutFile
    
    !Local variables
    INTEGER :: indx
    
    CALL Outfile%WriteData(Connector%iVersion)
    CALL Outfile%WriteData(Connector%nTotalGWNodes)
    CALL Outfile%WriteData(SIZE(Connector%GWNodeList))
    DO indx=1,SIZE(Connector%GWNodeList)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%nGWNodes)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%iGWNodes)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%iLayers)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%iGWNodeRanks)
       CALL Outfile%WriteData(Connector%GWNodeList(indx)%rFractionForGW)
    END DO
    
  END SUBROUTINE StrmGWConnector_v421_WritePreprocessedData

  

  
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
  SUBROUTINE StrmGWConnector_v421_CompileConductance(Connector,InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
    CLASS(StrmGWConnector_v421_Type)  :: Connector
    TYPE(GenericFileType)             :: InFile
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes),UpstrmNodes(:),DownstrmNodes(:) 
    REAL(8),INTENT(IN)                :: BottomElevs(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+39) :: ThisProcedure = ModName // 'StrmGWConnector_v421_CompileConductance'
    INTEGER                      :: indxNode,iGWNodeID,iStrmNodeID,iLoc,nGWNodes,iGWNode1,indxReach,iGWNode2,  &
                                    indx,iUpstrmNode,iDownstrmNode,iGWUpstrmNode,iLayer,iNode,iGWNode,         &
                                    iGWNodeIDs(AppGrid%NNodes),iInteractionType,iFace,indxFace
    REAL(8)                      :: FACTK,FACTL,rDummyArray3(3),CA,CB,rDummyArray4(4),B_DISTANCE,F_DISTANCE,   &
                                    Conductivity(MaxnGWNodes,NStrmNodes),BedThick(MaxnGWNodes,NStrmNodes)
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
        CALL InFile%ReadData(rDummyArray4,iStat)  ;  IF (iStat .EQ. -1) RETURN
        iStrmNodeID = INT(rDummyArray4(1))
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
        iGWNodeID = INT(rDummyArray4(2))
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
        Conductivity(iLoc,iNode) = rDummyArray4(3) * FACTK
        BedThick(iLoc,iNode)     = rDummyArray4(4) * FACTL
        DO indx=2,Connector%GWNodeList(iNode)%nGWNodes
            CALL InFile%ReadData(rDummyArray3,iStat)  ;  IF (iStat .EQ. -1) RETURN
            iGWNodeID = INT(rDummyArray3(1))  
            CALL ConvertID_To_Index(iGWNodeID,iGWNodeIDs,iGWNode)
            iLoc = LocateInList(iGWNode , Connector%GWNodeList(iNode)%iGWNodes)
            IF (iLoc .EQ. 0) THEN
                MessageArray(1) = 'Stream bed parameters at stream node '//TRIM(IntToText(iStrmNodeID))//' are listed for groundwater node '//TRIM(IntToText(iGWNodeID))//','
                MessageArray(2) = 'but this groundwater node is not associated with the stream node!'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Conductivity(iLoc,iNode) = rDummyArray3(2) * FACTK
            BedThick(iLoc,iNode)     = rDummyArray3(3) * FACTL
        END DO
        
        !Calculate lengths associated with each gw node and store them in rDistFrac array 
        IF (nGWNodes .GT. 1) THEN
            !Find the effective length of each node 
            Connector%GWNodeList(iNode)%rDistFrac = 0.0
            DO indx=1,nGWNodes
                iGWNode1 = Connector%GWNodeList(iNode)%iGWNodes(indx)
                DO indxFace=1,AppGrid%AppNode(iGWNode1)%NFaceID
                    iFace = AppGrid%AppNode(iGWNode1)%FaceID(indxFace)
                    IF (AppGrid%AppFace%Node(1,iFace) .EQ. iGWNode1) THEN
                        iGWNode2 = AppGrid%AppFace%Node(2,iFace)
                    ELSE
                        iGWNode2 = AppGrid%AppFace%Node(1,iFace)
                    END IF
                    iLoc = LocateInList(iGWNode2,Connector%GWNodeList(iNode)%iGWNodes(1:nGWNodes))
                    IF (iLoc .GT. 0) THEN
                        Connector%GWNodeList(iNode)%rDistFrac(indx) = Connector%GWNodeList(iNode)%rDistFrac(indx) + 0.5d0 * AppGrid%AppFace%Length(iFace)
                    END IF
                END DO
            END DO
        ELSE
            Connector%GWNodeList(iNode)%rDistFrac = 1.0
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
            Connector%GWNodeList(indxNode-1)%Conductance = Conductivity(1:nGWNodes,indxNode-1) * (B_DISTANCE+F_DISTANCE) / BedThick(1:nGWNodes,indxNode-1)
            !Bed thickness
            Connector%GWNodeList(indxNode-1)%rBedThickness = BedThick(1:nGWNodes,indxNode-1)
            !Advance distance
            B_DISTANCE = F_DISTANCE
        END DO
        nGWNodes                                          = Connector%GWNodeList(iDownstrmNode)%nGWNodes
        Connector%GWNodeList(iDownstrmNode)%Conductance   = Conductivity(1:nGWNodes,iDownstrmNode) * B_DISTANCE / BedThick(1:nGWNodes,iDownstrmNode)
        Connector%GWNodeList(iDownstrmNode)%rBedThickness = BedThick(1:nGWNodes,iDownstrmNode)
    END DO
    
    !Compute elevation where stream and gw disconnect
    DO indxNode=1,NStrmNodes
        IF (Connector%iInteractionType .EQ. iDisconnectAtBottomOfBed) THEN
            Connector%GWNodeList(indxNode)%rDisconnectElev = BottomElevs(indxNode) - Connector%GWNodeList(indxNode)%rBedThickness
        ELSE
            Connector%GWNodeList(indxNode)%rDisconnectElev = BottomElevs(indxNode)
        END IF
    END DO
    
  END SUBROUTINE StrmGWConnector_v421_CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE STREAM-GW INTERACTION
  ! --- *** Note: + flow means losing stream
  ! -------------------------------------------------------------
  SUBROUTINE StrmGWConnector_v421_Simulate(Connector,iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
    CLASS(StrmGWConnector_v421_Type)                :: Connector
    INTEGER,INTENT(IN)                              :: iNNodes
    REAL(8),INTENT(IN)                              :: rGWHeads(:),rStrmHeads(:),rAvailableFlows(:)
    TYPE(MatrixType)                                :: Matrix
    CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)                   
    REAL(8),OPTIONAL,INTENT(IN)                     :: rMaxElevs(:)             !Not used in this versions
    
    !Local variables
    INTEGER           :: iGWNode,iNodes_Connect(2),iNodes_RHS(500),indxGW,indx,nGWNodes,iOffset,indxStrm,       &
                         iRank,iCount,inNodesWithRank,indxS,indxL    
    REAL(8)           :: rUpdateCOEFF(2),rUpdateCOEFF_Keep(2),rUpdateRHS(500),rDiff_GW,rGWHead,rDiffGWSQRT,     &
                         rStrmGWFlow,rStrmGWFlowAdj,rStrmGWFlowAdjSQRT,rDStrmGWFlowAdj,rStrmHead,rWetPerimeter, &
                         rNodeAvailableFlow,rdWetPerimeter,rWetPerimeterWork,rConductance(MaxnGWNodes),         &
                         rFactors(MaxnGWNodes)
    INTEGER,PARAMETER :: iCompIDs_RHS(500)   = [f_iStrmComp , (f_iGWComp,indx=2,500)]  , &
                         iCompIDs_Connect(2) = [f_iStrmComp , f_iGWComp]
                             
    !Update matrix equations
    iOffSet = 0
    DO indxStrm=1,SIZE(Connector%GWNodeList)
        rStrmHead = rStrmHeads(indxStrm)
        
        !Wetted perimeter
        CALL WetPerimeterFunction(indxStrm)%EvaluateAndDerivative(rStrmHead,rWetPerimeter,rdWetPerimeter) 
        
        ASSOCIATE (pData => Connector%GWNodeList(indxStrm))
            iNodes_RHS(1) = indxStrm
            nGWNodes      = pData%nGWNodes
            
            !Calculate distribution of wetted perimeter to gw nodes
            IF (nGWNodes .EQ. 1) THEN
                rConductance(1) = pData%Conductance(1) * rWetPerimeter
                rFactors(1)     = 1.0
            ELSE
                rWetPerimeterWork        = rWetPerimeter
                rConductance(1:nGWNodes) = 0.0
                iRank                    = pData%iGWNodeRanks(1)
                iCount                   = 0  
                DO 
                    !Number of gw nodes with the same rank (gw nodes are listed in order of their ranks)
                    inNodesWithRank = COUNT(pData%iGWNodeRanks .EQ. iRank)
                    indxS = iCount + 1
                    indxL = iCount + inNodesWithRank
                    !If the nodes with the same rank will be inundated
                    IF (SUM(pData%rDistFrac(indxS:indxL)) .LE. rWetPerimeterWork) THEN
                        rConductance(indxS:indxL) = pData%Conductance(indxS:indxL) * pData%rDistFrac(indxS:indxL)
                        rWetPerimeterWork         = rWetPerimeterWork - SUM(pData%rDistFrac(indxS:indxL)) 
                        IF (rWetPerimeterWork .LE. 0.0) EXIT
                    !If the nodes with the same rank won't be all inundated, then distribute wetted perimeter proportionally w.r.t. associated length
                    ELSE
                        rFactors(1:inNodesWithRank) = pData%rDistFrac(indxS:indxL)
                        CALL NormalizeArray(rFactors(1:inNodesWithRank))
                        rConductance(indxS:indxL) = pData%Conductance(indxS:indxL) * rWetPerimeterWork * rFactors(1:inNodesWithRank)
                        EXIT
                    END IF
                    iCount = indxL
                    IF (iCount .EQ. nGWNodes) EXIT
                    iRank = pData%iGWNodeRanks(iCount+1)
                END DO
                
                !Factors to distribute available flow to nodes that are inundated
                rFactors(1:nGWNodes) = rConductance(1:nGWNodes)
                CALL NormalizeArray(rFactors(1:nGWNodes))
            END IF

            !Loop over each connected gw node
            DO indxGW=1,nGWNodes
                !Corresponding GW node, conductance and bed thickness
                iGWNode              = (pData%iLayers(indxGW)-1) * iNNodes + pData%iGWNodes(indxGW)
                iNodes_RHS(1+indxGW) = iGWNode
                IF (rConductance(indxGW) .EQ. 0.0) THEN
                    pData%StrmGWFlow(indxGW) = 0.0
                    CYCLE
                END IF

                !Head differences
                rGWHead     = rGWHeads(iOffset+indxGW)
                rDiff_GW    = rGWHead - pData%rDisconnectElev(indxGW)
                rDiffGWSQRT = SQRT(rDiff_GW*rDiff_GW + f_rSmoothMaxP)
                
                !Available flow for node
                rNodeAvailableFlow = rAvailableFlows(indxStrm) * rFactors(indxGW)
                
                !Calculate stream-gw interaction and update of Jacobian
                !--------------------------------------------
                rStrmGWFlow = rConductance(indxGW) * (rStrmHead - MAX(rGWHead,pData%rDisconnectElev(indxGW)))
                !Stream is gaining; no need to worry about drying stream (i.e. stream-gw flow is not a function of upstream flows)
                IF (rStrmGWFlow .LT. 0.0) THEN
                    pData%StrmGWFlow(indxGW) = rStrmGWFlow
                    iNodes_Connect(1)        = indxStrm
                    iNodes_Connect(2)        = iGWNode
                    
                    !Update Jacobian - entries for stream node 
                    rUpdateCOEFF_Keep(1) = rConductance(indxGW)
                    rUpdateCOEFF_Keep(2) = -0.5d0 * rConductance(indxGW) * (1d0+rDiff_GW/rDiffGWSQRT) 
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
                    rUpdateCOEFF_Keep(1) = rConductance(indxGW) * rDStrmGWFlowAdj
                    rUpdateCOEFF_Keep(2) = -0.5d0 * rConductance(indxGW) * (1d0+rDiff_GW/rDiffGWSQRT) * rDStrmGWFlowAdj
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
    
  END SUBROUTINE StrmGWConnector_v421_Simulate  
  
  
  
END MODULE