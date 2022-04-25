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
MODULE Class_StrmGWConnector
  USE GeneralUtilities
  USe MessageLogger             , ONLY: SetLastMessage        , &
                                        EchoProgress          , &
                                        MessageArray          , &
                                        f_iFatal
  USE IOInterface
  USE Package_Discretization
  USE Package_Matrix            , ONLY: MatrixType               , &
                                        ConnectivityListType
  USE Package_Misc              , ONLY: AbstractFunctionType
  USE Class_BaseStrmGWConnector , ONLY: BaseStrmGWConnectorType 
  USE Class_StrmGWConnector_v40
  USE Class_StrmGWConnector_v41
  USE Class_StrmGWConnector_v42
  USE Class_StrmGWConnector_v421
  USE Class_StrmGWConnector_v50
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
  PUBLIC :: StrmGWConnectorType       
  
  
  ! -------------------------------------------------------------
  ! --- GENERIC STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE StrmGWConnectorType
      PRIVATE
      LOGICAL                                    :: lDefined = .FALSE.
      CLASS(BaseStrmGWConnectorType),ALLOCATABLE :: Me
  CONTAINS
      PROCEDURE,PASS :: ReadPreprocessedData
      PROCEDURE,PASS :: AddGWNodes
      PROCEDURE,PASS :: AddGWNodesToStrmNode
      PROCEDURE,PASS :: CompileConductance
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: IsDefined
      PROCEDURE,PASS :: GetnTotalGWNodes
      PROCEDURE,PASS :: GetSubregionalFlows
      PROCEDURE,PASS :: GetFlowAtAllStrmNodes
      PROCEDURE,PASS :: GetTotalFlowAtAllStrmNodes
      PROCEDURE,PASS :: GetFlowAtSomeStrmNodes
      PROCEDURE,PASS :: GetFlowAtGWNode
      PROCEDURE,PASS :: GetLayer
      PROCEDURE,PASS :: GetAllLayers
      PROCEDURE,PASS :: GetAllGWNodes
      PROCEDURE,PASS :: GetGWNode
      PROCEDURE,PASS :: GetGWNodesAtStrmNode
      PROCEDURE,PASS :: GetGWHeadsAtStrmNodes
      PROCEDURE,PASS :: SetInteractionType
      PROCEDURE,PASS :: SetConductance
      PROCEDURE,PASS :: SetFractionsForGW
      PROCEDURE,PASS :: SetStrmGWFlow
      PROCEDURE,PASS :: SetDisconnectElevations
      PROCEDURE,PASS :: WritePreprocessedData
      PROCEDURE,PASS :: ConvertTimeUnit
      PROCEDURE,PASS :: ComputeStrmGWFlow_AtMinHead
      PROCEDURE,PASS :: Simulate
      PROCEDURE,PASS :: RegisterWithMatrix
      GENERIC        :: New      => ReadPreprocessedData              , &
                                    AddGWNodes 
  END TYPE StrmGWConnectorType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmGWConnector::'

  
  
  
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
  ! --- READ PRE-PROCESSED STREAM-GW CONNECTOR DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadPreprocessedData(Connector,InFile,iStat)
    CLASS(StrmGWConnectorType) :: Connector
    TYPE(GenericFileType)      :: InFile
    INTEGER,INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadPreprocessedData'
    INTEGER                      :: iVersion
    
    !Read version number
    CALL InFile%ReadData(iVersion,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    SELECT CASE (iVersion)
        CASE (0)
            RETURN
            
        CASE (40)
            ALLOCATE (StrmGWConnector_v40_Type :: Connector%Me)
            CALL Connector%Me%New(InFile,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE (41)
            ALLOCATE (StrmGWConnector_v41_Type :: Connector%Me)
            CALL Connector%Me%New(InFile,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE (42)
            ALLOCATE (StrmGWConnector_v42_Type :: Connector%Me)
            CALL Connector%Me%New(InFile,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.

        CASE (421)
            ALLOCATE (StrmGWConnector_v421_Type :: Connector%Me)
            CALL Connector%Me%New(InFile,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.

        CASE (50)
            ALLOCATE (StrmGWConnector_v50_Type :: Connector%Me)
            CALL Connector%Me%New(InFile,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Version number '//TRIM(IntToText(iVersion))//' for stream-groundwater interaction is not recognized!',f_iFatal,ThisProcedure) 
            iStat = -1
    END SELECT 
       
  END SUBROUTINE ReadPreprocessedData
  
  
  ! -------------------------------------------------------------
  ! --- SET GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE AddGWNodes(Connector,iVersion,iGWNodes,iLayers,iStat)
    CLASS(StrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)         :: iVersion,iGWNodes(:),iLayers(:)
    INTEGER,INTENT(OUT)        :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'AddGWNodes'
    
    SELECT CASE (iVersion)
        CASE (40)
            ALLOCATE (StrmGWConnector_v40_Type :: Connector%Me)
            CALL Connector%Me%New(iGWNodes,iLayers,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE (41)
            ALLOCATE (StrmGWConnector_v41_Type :: Connector%Me)
            CALL Connector%Me%New(iGWNodes,iLayers,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE (42)
            CALL SetLastMessage('AddGWNodes method is not defined for stream-groundwater interaction component version 4.2!',f_iFatal,ThisProcedure)
            iStat = -1
            
        CASE (50)
            ALLOCATE (StrmGWConnector_v50_Type :: Connector%Me)
            CALL Connector%Me%New(iGWNodes,iLayers,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Version number '//TRIM(IntToText(iVersion))//' for stream-groundwater interaction is not recognized!',f_iFatal,ThisProcedure) 
            iStat = -1
    END SELECT 

  END SUBROUTINE AddGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- ADD GW NODE FOR A STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE AddGWNodesToStrmNode(Connector,iVersion,NStrmNodes,iStrmNode,iGWNodes,iLayers,iRanks,iStat)
    CLASS(StrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)         :: iVersion,NStrmNodes,iStrmNode,iGWNodes(:),iLayers(:),iRanks(:)
    INTEGER,INTENT(OUT)        :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'AddGWNodesToStrmNode'
    
    SELECT CASE (iVersion)
        CASE (40)
            CALL SetLastMessage('AddGWNodeToStrmNode method is not defined for stream-groundwater interaction component version 4.0!',f_iFatal,ThisProcedure) 
            iStat = -1
            
        CASE (41)
            CALL SetLastMessage('AddGWNodeToStrmNode method is not defined for stream-groundwater interaction component version 4.1!',f_iFatal,ThisProcedure) 
            iStat = -1
            
        CASE (42)
            IF (ALLOCATED(Connector%Me)) THEN
                SELECT TYPE (p => Connector%Me)
                    TYPE IS (StrmGWConnector_v42_Type)
                        CALL p%AddGWNodesToStrmNode(NStrmNodes,iStrmNode,iGWNodes,iLayers,iRanks)
                END SELECT
            ELSE
                ALLOCATE (StrmGWConnector_v42_Type :: Connector%Me)
                SELECT TYPE (p => Connector%Me)
                    TYPE IS (StrmGWConnector_v42_Type)
                        CALL p%AddGWNodesToStrmNode(NStrmNodes,iStrmNode,iGWNodes,iLayers,iRanks)
                END SELECT
                Connector%Me%iVersion = iVersion
                Connector%lDefined    = .TRUE.
            END IF
            
        CASE (421)
            IF (ALLOCATED(Connector%Me)) THEN
                SELECT TYPE (p => Connector%Me)
                    TYPE IS (StrmGWConnector_v421_Type)
                        CALL p%AddGWNodesToStrmNode(NStrmNodes,iStrmNode,iGWNodes,iLayers,iRanks)
                END SELECT
            ELSE
                ALLOCATE (StrmGWConnector_v421_Type :: Connector%Me)
                SELECT TYPE (p => Connector%Me)
                    TYPE IS (StrmGWConnector_v421_Type)
                        CALL p%AddGWNodesToStrmNode(NStrmNodes,iStrmNode,iGWNodes,iLayers,iRanks)
                END SELECT
                Connector%Me%iVersion = iVersion
                Connector%lDefined    = .TRUE.
            END IF
            
        CASE (50)
            CALL SetLastMessage('AddGWNodeToStrmNode method is not defined for stream-groundwater interaction component version 5.0!',f_iFatal,ThisProcedure)
            iStat = -1
            
        CASE DEFAULT
            CALL SetLastMessage('Version number '//TRIM(IntToText(iVersion))//' for stream-groundwater interaction is not recognized!',f_iFatal,ThisProcedure) 
            iStat = -1
    END SELECT 

  END SUBROUTINE AddGWNodesToStrmNode

  
  
  
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
  ! --- KILL STREAM-GW CONNECTOR OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Connector)
    CLASS(StrmGWConnectorType) :: Connector
    
    !Local variables
    INTEGER :: ErrorCode
    
    IF (Connector%lDefined) THEN
        CALL Connector%Me%Kill()
        Connector%lDefined = .FALSE.
        DEALLOCATE (Connector%Me , STAT=ErrorCode)
    END IF
    
  END SUBROUTINE Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- IS STREAM-GW CONNECTOR DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(Connector) RESULT(lDefined)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    LOGICAL                               :: lDefined
    
    lDefined = Connector%lDefined
    
  END FUNCTION IsDefined
  
  
  
  
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
  ! --- GET TOTAL NUMBER OF GW NODES INTERACTING WITH STREAMS (COUNTING DOUBLES)
  ! -------------------------------------------------------------
  FUNCTION GetnTotalGWNodes(Connector) RESULT(nTotalGWNodes)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER                               :: nTotalGWNodes

    IF (Connector%lDefined) THEN
        SELECT TYPE (p => Connector%Me)
            CLASS IS (StrmGWConnector_v42_Type)
                nTotalGWNodes = p%GetnTotalGWNodes()
            CLASS DEFAULT
                nTotalGWNodes = SIZE(p%iGWNode)    
        END SELECT
    ELSE
        nTotalGWNodes = 0
    END IF

  END FUNCTION GetnTotalGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT SUBREGIONS 
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalFlows(Connector,AppGrid,lInsideModel) RESULT(Flows)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    LOGICAL,INTENT(IN)                    :: lInsideModel
    REAL(8)                               :: Flows(AppGrid%NSubregions)
    
    IF (Connector%lDefined) THEN
        Flows = Connector%Me%GetSubregionalFlows(AppGrid,lInsideModel)
    ELSE
        Flows = 0.0
    END IF
  
  END FUNCTION GetSubregionalFlows


  ! -------------------------------------------------------------
  ! --- GET TOTAL (INSIDE AND OUTSIDE MODEL) STREAM-GW FLOW AT EVERY STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE GetTotalFlowAtAllStrmNodes(Connector,Flow)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    REAL(8),INTENT(OUT)                   :: Flow(:) 

    IF (Connector%lDefined) THEN
        SELECT TYPE (p => Connector%Me)
            CLASS IS (StrmGWConnector_v42_Type)
                CALL p%GetTotalFlowAtAllStrmNodes(Flow)
        END SELECT
    ELSE
        Flow = 0.0
    END IF
  
  END SUBROUTINE GetTotalFlowAtAllStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM-GW FLOW AT EVERY STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE GetFlowAtAllStrmNodes(Connector,lInsideModel,Flow)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    LOGICAL,INTENT(IN)                    :: lInsideModel
    REAL(8),INTENT(OUT)                   :: Flow(:) 

    IF (Connector%lDefined) THEN
        CALL Connector%Me%GetFlowAtAllStrmNodes(lInsideModel,Flow)
    ELSE
        Flow = 0.0
    END IF
  
  END SUBROUTINE GetFlowAtAllStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT A SET OF STREAM NODES 
  ! -------------------------------------------------------------
  FUNCTION GetFlowAtSomeStrmNodes(Connector,iNodeBegin,iNodeEnd,lInsideModel) RESULT(Flow)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iNodeBegin,iNodeEnd
    LOGICAL,INTENT(IN)                    :: lInsideModel
    REAL(8)                               :: Flow 

    IF (Connector%lDefined) THEN
        Flow = Connector%Me%GetFlowAtSomeStrmNodes(iNodeBegin,iNodeEnd,lInsideModel)
    ELSE
        Flow = 0.0
    END IF
  
  END FUNCTION GetFlowAtSomeStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW AT GROUNDWATER NODE AT A LAYER
  ! -------------------------------------------------------------
  FUNCTION GetFlowAtGWNode(Connector,iNode,iLayer) RESULT(Flow)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iNode,iLayer
    REAL(8)                               :: Flow

    IF (Connector%lDefined) THEN
        Flow = Connector%Me%GetFlowAtGWNode(iNode,iLayer)
    ELSE
        Flow = 0.0
    END IF
  
  END FUNCTION GetFlowAtGWNode
  
  
  ! -------------------------------------------------------------
  ! --- GET LAYER FOR GW NODE
  ! -------------------------------------------------------------
  FUNCTION GetLayer(Connector,iNode) RESULT(Layer)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iNode
    INTEGER                               :: Layer

    IF (Connector%lDefined) THEN
        Layer = Connector%Me%GetLayer(iNode)
    ELSE
        Layer = 0
    END IF
  
  END FUNCTION GetLayer

  
  ! -------------------------------------------------------------
  ! --- GET ALL LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE GetAllLayers(Connector,Layers)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,ALLOCATABLE                   :: Layers(:)

    IF (Connector%lDefined) CALL Connector%Me%GetAllLayers(Layers)
    
  END SUBROUTINE GetAllLayers
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetAllGWNodes(Connector,GWNodes)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,ALLOCATABLE                   :: GWNodes(:)
    
    !Local variables
    INTEGER,ALLOCATABLE :: GWNodesLocal(:)

    IF (Connector%lDefined) CALL Connector%Me%GetAllGWNodes(GWNodesLocal)
    ALLOCATE (GWNodes , SOURCE=GWNodesLocal)
  
  END SUBROUTINE GetAllGWNodes
  

  ! -------------------------------------------------------------
  ! --- GET GW NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetGWNode(Connector,iNode) RESULT(GWNode)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iNode
    INTEGER                               :: GWNode
    
    !Local variables
    INTEGER,ALLOCATABLE :: iGWNodes(:)

    IF (.NOT. Connector%lDefined) THEN
        GWNode = 0
        RETURN
    END IF
    
    SELECT TYPE (p => Connector%Me)
        !Return the first GW node associated with a stream node for version 4.2
        CLASS IS (StrmGWConnector_v42_Type)
            CALL p%GetGWNodesAtStrmNode(iNode,iGWNodes)
            GWNode = iGWNodes(1)
            
        CLASS DEFAULT
            GWNode = Connector%Me%GetGWNode(iNode)
    END SELECT    

  END FUNCTION GetGWNode
  
  
  ! -------------------------------------------------------------
  ! --- GET GW NODE(S) AT A STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE GetGWNodesAtStrmNode(Connector,iStrmNode,iGWNodes,iStat)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iStrmNode
    INTEGER,ALLOCATABLE                   :: iGWNodes(:)
    INTEGER,INTENT(OUT)                   :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+20),PARAMETER :: ThisProcedure = ModName // 'GetGWNodesAtStrmNode'
    
    !Initialize
    iStat = 0
    
    IF (Connector%lDefined) THEN
        SELECT TYPE (p => Connector%Me)
            CLASS IS (StrmGWConnector_v42_Type)
                CALL p%GetGWNodesAtStrmNode(iStrmNode,iGWNodes)
                
            CLASS DEFAULT
                ALLOCATE (iGWNodes(1))  
                iGWNodes(1) = p%GetGWNode(iStrmNode)
        END SELECT    
    END IF

  END SUBROUTINE GetGWNodesAtStrmNode
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEADS AT STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHeadsAtStrmNodes(Connector,GWHeads,GWHeadsAtStrmNodes)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    REAL(8),INTENT(IN)                    :: GWHeads(:,:)
    REAL(8),INTENT(OUT)                   :: GWHeadsAtStrmNodes(:)
    
    CALL Connector%Me%GetGWHeadsAtStrmNodes(GWHeads,GWHeadsAtStrmNodes)        
    
  END SUBROUTINE GetGWHeadsAtStrmNodes 

  
  
  
  
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
  ! --- SET DISCONNECT ELEVATIONS (ONLY AVAILBLE FOR v5.0)
  ! -------------------------------------------------------------
  SUBROUTINE SetDisconnectElevations(Connector,rBottomElevs)
    CLASS(StrmGWConnectorType) :: Connector
    REAL(8),INTENT(IN)         :: rBottomElevs(:)
    
    IF (Connector%lDefined) THEN
        SELECT TYPE (p => Connector%Me)
            CLASS IS (StrmGWConnector_v50_Type) 
                CALL p%SetDisconnectElevations(rBottomElevs)
        END SELECT
    END IF
    
  END SUBROUTINE SetDisconnectElevations
    
  
  ! -------------------------------------------------------------
  ! --- SET STREAM-GW INTERACTION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE SetInteractionType(Connector,iInteractionType,iStat)
    CLASS(StrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)         :: iInteractionType
    INTEGER,INTENT(OUT)        :: iStat
    
    IF (Connector%lDefined) THEN
        CALL Connector%Me%SetInteractionType(iInteractionType,iStat)
    ELSE
        iStat = 0
    END IF

  END SUBROUTINE SetInteractionType
  
  
  ! -------------------------------------------------------------
  ! --- SET THE CONDUCTANCE
  ! -------------------------------------------------------------
  SUBROUTINE SetConductance(Connector,TimeUnitConductance,Conductance)
    CLASS(StrmGWConnectorType)  :: Connector
    CHARACTER(LEN=6),INTENT(IN) :: TimeUnitConductance
    REAL(8),INTENT(IN)          :: Conductance(:)

    IF (Connector%lDefined) CALL Connector%Me%SetConductance(TimeUnitConductance,Conductance)
    
  END SUBROUTINE SetConductance
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM-GW INTERACTION AT A NODE
  ! --- * Note: This should be necsaary only for specified-stage type stream nodes
  ! -------------------------------------------------------------
  SUBROUTINE SetStrmGWFlow(Connector,iNode,StrmGWFlow)
    CLASS(StrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)         :: iNode
    REAL(8),INTENT(IN)         :: StrmGWFlow

    IF (Connector%lDefined) CALL Connector%Me%SetStrmGWFlow(iNode,StrmGWFlow)
    
  END SUBROUTINE SetStrmGWFlow
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM NODES AND FRACTION OF STREAM-GW INTERACTION TO BE APPLIED TO GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE SetFractionsForGW(Connector,iStrmNodes,rFractions,iStat)
    CLASS(StrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)         :: iStrmNodes(:)
    REAL(8),INTENT(IN)         :: rFractions(:)
    INTEGER,INTENT(OUT)        :: iStat

    IF (Connector%lDefined) THEN
        CALL Connector%Me%SetFractionsForGW(iStrmNodes,rFractions,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE SetFractionsForGW
  
  
  
  
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
  SUBROUTINE WritePreprocessedData(Connector,OutFile)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(GenericFileType)                 :: OutFile

    IF (Connector%lDefined) THEN
        CALL Connector%Me%WritePreprocessedData(OutFile)
    ELSE
        CALL Outfile%WriteData(0)
    END IF
    
  END SUBROUTINE WritePreprocessedData
  
  
  
  
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
  ! --- ADD CONDUCTANCE RELATED DATA TO CONNECTOR
  ! ---  Note: Assumes GW nodes are already defined
  ! -------------------------------------------------------------
  SUBROUTINE CompileConductance(Connector,InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
    CLASS(StrmGWConnectorType)        :: Connector
    TYPE(GenericFileType)             :: InFile
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes),UpstrmNodes(:),DownstrmNodes(:)
    REAL(8),INTENT(IN)                :: BottomElevs(:)
    INTEGER,INTENT(OUT)               :: iStat

    IF (Connector%lDefined) THEN
        CALL Connector%Me%CompileConductance(InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE CompileConductance
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF CONNECTOR RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(Connector,NewUnit)
    CLASS(StrmGWConnectorType)  :: Connector
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    IF (Connector%lDefined) CALL Connector%Me%ConvertTimeUnit(NewUnit)
    
  END SUBROUTINE ConvertTimeUnit
    

  ! -------------------------------------------------------------
  ! --- SIMULATE STREAM-GW INTERACTION
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(Connector,iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
    CLASS(StrmGWConnectorType)                      :: Connector
    INTEGER,INTENT(IN)                              :: iNNodes
    REAL(8),INTENT(IN)                              :: rGWHeads(:),rStrmHeads(:),rAvailableFlows(:)
    TYPE(MatrixType)                                :: Matrix
    CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)                     
    REAL(8),OPTIONAL,INTENT(IN)                     :: rMaxElevs(:)

    IF (Connector%lDefined) THEN
        CALL Connector%Me%Simulate(iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
    END IF
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION WITHOUT UPDATING MATRIX
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE ComputeStrmGWFlow_AtMinHead(Connector,rStrmBottomElev,rGWHead,rMaxElev,WetPerimeterFunction,rFlows)
    CLASS(StrmGWConnectorType)             :: Connector
    REAL(8),INTENT(IN)                     :: rStrmBottomElev(:),rGWHead(:),rMaxElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)
    REAL(8),INTENT(OUT)                    :: rFlows(:)

    IF (Connector%lDefined) THEN
        SELECT TYPE (p => Connector%Me)
            CLASS IS (StrmGWConnector_v50_Type)
                CALL p%ComputeStrmGWFlow_AtMinHead(rStrmBottomElev,rGWHead,rMaxElev,WetPerimeterFunction,rFlows)
        END SELECT
    END IF
    
  END SUBROUTINE ComputeStrmGWFlow_AtMinHead
  
  
  ! -------------------------------------------------------------
  ! --- ADD STREAM-GW CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(Connector,StrmConnectivity,AppGrid,Matrix,iStat)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(ConnectivityListType),INTENT(IN) :: StrmConnectivity(:)
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(MatrixType)                      :: Matrix
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Initialize
    iStat = 0
    
    !Return if stream-gw connector is not defined
    IF (.NOT. Connector%lDefined) RETURN
    
    !Inform user
    CALL EchoProgress('Registering stream-groundwater connector with matrix...')
    
    !Register connectivity with matrix
    CALL Connector%Me%RegisterWithMatrix(StrmConnectivity,AppGrid,Matrix,iStat)
    
  END SUBROUTINE RegisterWithMatrix


END MODULE