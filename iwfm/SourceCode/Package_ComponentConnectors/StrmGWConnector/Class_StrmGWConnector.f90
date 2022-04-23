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
MODULE Class_StrmGWConnector
  USE GeneralUtilities
  USe MessageLogger             , ONLY: SetLastMessage        , &
                                        EchoProgress          , &
                                        MessageArray          , &
                                        iFatal
  USE IOInterface
  USE Package_Discretization
  USE Package_Matrix            , ONLY: MatrixType
  USE Package_Misc              , ONLY: AbstractFunctionType
  USE Class_BaseStrmGWConnector
  USE Class_StrmGWConnector_v40
  USE Class_StrmGWConnector_v41
  USE Class_StrmGWConnector_v42
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
      PROCEDURE,PASS :: GetFlowAtSomeStrmNodes
      PROCEDURE,PASS :: GetFlowAtGWNode
      PROCEDURE,PASS :: GetLayer
      PROCEDURE,PASS :: GetAllLayers
      PROCEDURE,PASS :: GetAllGWNodes
      PROCEDURE,PASS :: GetGWNode
      PROCEDURE,PASS :: GetGWNodesAtStrmNode
      PROCEDURE,PASS :: GetGWHeadsAtStrmNodes
      PROCEDURE,PASS :: SetConductance
      PROCEDURE,PASS :: SetStrmGWFlow
      PROCEDURE,PASS :: WritePreprocessedData
      PROCEDURE,PASS :: ConvertTimeUnit
      PROCEDURE,PASS :: ComputeStrmGWFlow_AtMinHead
      PROCEDURE,PASS :: Simulate
      PROCEDURE,PASS :: RegisterWithMatrix
      PROCEDURE,PASS :: UpdateMatrix_ForBypass
      GENERIC        :: New => ReadPreprocessedData  , &
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

        CASE (50)
            ALLOCATE (StrmGWConnector_v50_Type :: Connector%Me)
            CALL Connector%Me%New(InFile,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Version number '//TRIM(IntToText(iVersion))//' for stream-groundwater interaction is not recognized!',iFatal,ThisProcedure) 
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
            CALL SetLastMessage('AddGWNodes method is not defined for stream-groundwater interaction component version 4.2!',iFatal,ThisProcedure)
            iStat = -1
            
        CASE (50)
            ALLOCATE (StrmGWConnector_v50_Type :: Connector%Me)
            CALL Connector%Me%New(iGWNodes,iLayers,iStat)
            IF (iStat .EQ. -1) RETURN
            Connector%Me%iVersion = iVersion
            Connector%lDefined    = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Version number '//TRIM(IntToText(iVersion))//' for stream-groundwater interaction is not recognized!',iFatal,ThisProcedure) 
            iStat = -1
    END SELECT 

  END SUBROUTINE AddGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- ADD GW NODE FOR A STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE AddGWNodesToStrmNode(Connector,iVersion,NStrmNodes,iStrmNode,iGWNodes,iLayers,iStat)
    CLASS(StrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)         :: iVersion,NStrmNodes,iStrmNode,iGWNodes(:),iLayers(:)
    INTEGER,INTENT(OUT)        :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'AddGWNodesToStrmNode'
    
    SELECT CASE (iVersion)
        CASE (40)
            CALL SetLastMessage('AddGWNodeToStrmNode method is not defined for stream-groundwater interaction component version 4.0!',iFatal,ThisProcedure) 
            iStat = -1
            
        CASE (41)
            CALL SetLastMessage('AddGWNodeToStrmNode method is not defined for stream-groundwater interaction component version 4.1!',iFatal,ThisProcedure) 
            iStat = -1
            
        CASE (42)
            IF (ALLOCATED(Connector%Me)) THEN
                SELECT TYPE (p => Connector%Me)
                    TYPE IS (StrmGWConnector_v42_Type)
                        CALL p%AddGWNodesToStrmNode(NStrmNodes,iStrmNode,iGWNodes,iLayers)
                END SELECT
            ELSE
                ALLOCATE (StrmGWConnector_v42_Type :: Connector%Me)
                SELECT TYPE (p => Connector%Me)
                    TYPE IS (StrmGWConnector_v42_Type)
                        CALL p%AddGWNodesToStrmNode(NStrmNodes,iStrmNode,iGWNodes,iLayers)
                END SELECT
                Connector%Me%iVersion = iVersion
                Connector%lDefined    = .TRUE.
            END IF
            
        CASE (50)
            CALL SetLastMessage('AddGWNodeToStrmNode method is not defined for stream-groundwater interaction component version 5.0!',iFatal,ThisProcedure)
            iStat = -1
            
        CASE DEFAULT
            CALL SetLastMessage('Version number '//TRIM(IntToText(iVersion))//' for stream-groundwater interaction is not recognized!',iFatal,ThisProcedure) 
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
            TYPE IS (StrmGWConnector_v42_Type)
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
  FUNCTION GetSubregionalFlows(Connector,AppGrid) RESULT(Flows)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    REAL(8)                               :: Flows(AppGrid%NSubregions)
    
    IF (Connector%lDefined) THEN
        Flows = Connector%Me%GetSubregionalFlows(AppGrid)
    ELSE
        Flows = 0.0
    END IF
  
  END FUNCTION GetSubregionalFlows


  ! -------------------------------------------------------------
  ! --- GET STREAM-GW FLOW AT EVERY STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE GetFlowAtAllStrmNodes(Connector,Flow)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    REAL(8),INTENT(OUT)                   :: Flow(:) 

    IF (Connector%lDefined) THEN
        CALL Connector%Me%GetFlowAtAllStrmNodes(Flow)
    ELSE
        Flow = 0.0
    END IF
  
  END SUBROUTINE GetFlowAtAllStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT A SET OF STREAM NODES 
  ! -------------------------------------------------------------
  FUNCTION GetFlowAtSomeStrmNodes(Connector,iNodeBegin,iNodeEnd) RESULT(Flow)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iNodeBegin,iNodeEnd
    REAL(8)                               :: Flow 

    IF (Connector%lDefined) THEN
        Flow = Connector%Me%GetFlowAtSomeStrmNodes(iNodeBegin,iNodeEnd)
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
  FUNCTION GetGWNode(Connector,iNode) RESULT(GWNode)
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
        TYPE IS (StrmGWConnector_v42_Type)
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
            TYPE IS (StrmGWConnector_v42_Type)
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
  ! --- UPDATE MATRIX FOR BYPASS
  ! -------------------------------------------------------------
  SUBROUTINE UpdateMatrix_ForBypass(Connector,iNode,StrmHead,GWHead,DeltaX,StrmBottomElev,MaxElev,dBypass_dFlow,WetPerimeterFunction,Matrix)
    CLASS(StrmGWConnectorType),INTENT(IN)  :: Connector
    INTEGER,INTENT(IN)                     :: iNode
    REAL(8),INTENT(IN)                     :: StrmHead(:),GWHead(:),DeltaX(:),StrmBottomElev(:),MaxElev(:),dBypass_dFlow
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)                    !In this case, this is the wetted perimeter function defined using Manning's formula 
    TYPE(MatrixType)                       :: Matrix
    
    SELECT TYPE (p => Connector%Me)
        CLASS IS (StrmGWConnector_v50_Type)
            CALL StrmGWConnector_v50_UpdateMatrix_ForBypass(p,iNode,StrmHead,GWHead,DeltaX,StrmBottomElev,MaxElev,dBypass_dFlow,WetPerimeterFunction,Matrix)
    END SELECT
    
  END SUBROUTINE UpdateMatrix_ForBypass
  
  
  ! -------------------------------------------------------------
  ! --- ADD CONDUCTANCE RELATED DATA TO CONNECTOR
  ! ---  Note: Assumes GW nodes are already defined
  ! -------------------------------------------------------------
  SUBROUTINE CompileConductance(Connector,InFile,AppGrid,NStrmNodes,UpstrmNodes,DownstrmNodes,iStat)
    CLASS(StrmGWConnectorType)   :: Connector
    TYPE(GenericFileType)        :: InFile
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: NStrmNodes,UpstrmNodes(:),DownstrmNodes(:)
    INTEGER,INTENT(OUT)          :: iStat

    IF (Connector%lDefined) THEN
        CALL Connector%Me%CompileConductance(InFile,AppGrid,NStrmNodes,UpstrmNodes,DownstrmNodes,iStat)
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
  ! --- COMPUTE STREAM-GW INTERACTION
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(Connector,NNodes,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,Matrix,DeltaX,MaxElev)
    CLASS(StrmGWConnectorType)             :: Connector
    INTEGER,INTENT(IN)                     :: NNodes
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)
    TYPE(MatrixType)                       :: Matrix
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:),MaxElev(:)

    IF (Connector%lDefined) THEN
        IF (PRESENT(DeltaX)) THEN
            CALL Connector%Me%Simulate(NNodes,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,Matrix,DeltaX,MaxElev)
        ELSE
            CALL Connector%Me%Simulate(NNodes,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,Matrix)
        END IF
    END IF
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE STREAM-GW INTERACTION WITHOUT UPDATING MATRIX
  ! --- *** Note: + flow means loosing stream
  ! -------------------------------------------------------------
  SUBROUTINE ComputeStrmGWFlow_AtMinHead(Connector,Flows,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,DeltaX)
    CLASS(StrmGWConnectorType)             :: Connector
    REAL(8),INTENT(OUT)                    :: Flows(:)
    REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
    CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)
    REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:)

    IF (Connector%lDefined) THEN
        IF (PRESENT(DeltaX)) THEN
            CALL Connector%Me%ComputeStrmGWFlow_AtMinHead(Flows,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,DeltaX)
        ELSE
            CALL Connector%Me%ComputeStrmGWFlow_AtMinHead(Flows,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction)
        END IF
    END IF
    
  END SUBROUTINE ComputeStrmGWFlow_AtMinHead
  
  
  ! -------------------------------------------------------------
  ! --- ADD STREAM-GW CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(Connector,AppGrid,lUpstrmNode,Matrix,iStat)
    CLASS(StrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    LOGICAL,INTENT(IN)                    :: lUpstrmNode(:)
    TYPE(MatrixType)                      :: Matrix
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Initialize
    iStat = 0
    
    !Return if stream-gw connector is not defined
    IF (.NOT. Connector%lDefined) RETURN
    
    !Inform user
    CALL EchoProgress('Registering stream-groundwater connector with matrix...')
    
    !Register connectivity with matrix
    SELECT TYPE (p => Connector%Me)
        TYPE IS (StrmGWConnector_v50_Type)
            !Connectivity for version 5.0 is different
            CALL StrmGWConnector_v50_RegisterWithMatrix(p,lUpstrmNode,AppGrid,Matrix,iStat)
        CLASS DEFAULT
            CALL p%RegisterWithMatrix(AppGrid,Matrix,iStat)        
    END SELECT
    
  END SUBROUTINE RegisterWithMatrix


END MODULE