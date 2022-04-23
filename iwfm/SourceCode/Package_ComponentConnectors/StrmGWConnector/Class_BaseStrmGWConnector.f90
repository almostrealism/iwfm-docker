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
MODULE Class_BaseStrmGWConnector
  USE MessageLogger          , ONLY: SetLastMessage       , &
                                     iFatal
  USE IOInterface
  USE TimeSeriesUtilities
  USE GeneralUtilities
  USE Package_Discretization
  USE Package_Misc           , ONLY: AbstractFunctionType , &
                                     iStrmComp            , &
                                     iGWComp        
  USE Package_Matrix         , ONLY: MatrixType
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
  PUBLIC :: BaseStrmGWConnectorType  , &
            BaseStrmGWConnector_Kill
  
  
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT:: BaseStrmGWConnectorType
    INTEGER             :: iVersion            = 0   !Version number
    CHARACTER(LEN=6)    :: TimeUnitConductance = ''  !Time unit of conductance
    INTEGER,ALLOCATABLE :: iGWNode(:)                !GW node number for each stream node
    INTEGER,ALLOCATABLE :: iLayer(:)                 !Aquifer layer number that the stream nodes interact with
    REAL(8),ALLOCATABLE :: Conductance(:)            !Stream bed conductance
    REAL(8),ALLOCATABLE :: StrmGWFlow(:)             !Stream-gw interaction (+ is flow from stream to gw)
  CONTAINS
    PROCEDURE(Abstract_StrmGWConnector_ComputeStrmGWFlow_AtMinHead),PASS,DEFERRED  :: ComputeStrmGWFlow_AtMinHead
    PROCEDURE(Abstract_StrmGWConnector_Simulate),PASS,DEFERRED                     :: Simulate 
    PROCEDURE(Abstract_StrmGWConnector_CompileConductance),PASS,DEFERRED           :: CompileConductance
    PROCEDURE,PASS                                                                 :: BaseStrmGWConnector_AddGWNodes
    PROCEDURE,PASS                                                                 :: BaseStrmGWConnector_ReadPreprocessedData
    PROCEDURE,PASS                                                                 :: Kill                    => BaseStrmGWConnector_Kill
    PROCEDURE,PASS                                                                 :: GetAllLayers            => BaseStrmGWConnector_GetAllLayers           
    PROCEDURE,PASS                                                                 :: GetLayer                => BaseStrmGWConnector_GetLayer               
    PROCEDURE,PASS                                                                 :: GetFlowAtGWNode         => BaseStrmGWConnector_GetFlowAtGWNode        
    PROCEDURE,PASS                                                                 :: GetFlowAtAllStrmNodes   => BaseStrmGWConnector_GetFlowAtAllStrmNodes 
    PROCEDURE,PASS                                                                 :: GetFlowAtSomeStrmNodes  => BaseStrmGWConnector_GetFlowAtSomeStrmNodes 
    PROCEDURE,PASS                                                                 :: GetSubregionalFlows     => BaseStrmGWConnector_GetSubregionalFlows    
    PROCEDURE,PASS                                                                 :: GetAllGWNodes           => BaseStrmGWConnector_GetAllGWNodes          
    PROCEDURE,PASS                                                                 :: GetGWNode               => BaseStrmGWConnector_GetGWNode              
    PROCEDURE,PASS                                                                 :: GetGWHeadsAtStrmNodes   => BaseStrmGWConnector_GetGWHeadsAtStrmNodes
    PROCEDURE,PASS                                                                 :: SetConductance          => BaseStrmGWConnector_SetConductance         
    PROCEDURE,PASS                                                                 :: SetStrmGWFlow           => BaseStrmGWConnector_SetStrmGWFlow          
    PROCEDURE,PASS                                                                 :: WritePreprocessedData   => BaseStrmGWConnector_WritePreprocessedData  
    PROCEDURE,PASS                                                                 :: ConvertTimeUnit         => BaseStrmGWConnector_ConvertTimeUnit  
    PROCEDURE,PASS                                                                 :: RegisterWithMatrix      => BaseStrmGWConnector_RegisterWithMatrix
    GENERIC                                                                        :: New                     => BaseStrmGWConnector_AddGWNodes           , &
                                                                                                                 BaseStrmGWConnector_ReadPreprocessedData 
  END TYPE BaseStrmGWConnectorType
    
  
  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE
  
      SUBROUTINE Abstract_StrmGWConnector_ComputeStrmGWFlow_AtMinHead(Connector,Flows,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,DeltaX)
        IMPORT                                 :: BaseStrmGWConnectorType,AbstractFunctionType
        CLASS(BaseStrmGWConnectorType)         :: Connector
        REAL(8),INTENT(OUT)                    :: Flows(:)
        REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
        CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)
        REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:)
      END SUBROUTINE Abstract_StrmGWConnector_ComputeStrmGWFlow_AtMinHead
      
      
      SUBROUTINE Abstract_StrmGWConnector_Simulate(Connector,NNodes,GWHead,StrmHead,StrmBottomElev,WetPerimeterFunction,Matrix,DeltaX,MaxElev)
        IMPORT                                 :: BaseStrmGWConnectorType,AbstractFunctionType,MatrixType
        CLASS(BaseStrmGWConnectorType)         :: Connector
        INTEGER,INTENT(IN)                     :: NNodes
        REAL(8),INTENT(IN)                     :: GWHead(:),StrmHead(:),StrmBottomElev(:)
        CLASS(AbstractFunctionType),INTENT(IN) :: WetPerimeterFunction(:)
        TYPE(MatrixType)                       :: Matrix
        REAL(8),OPTIONAL,INTENT(IN)            :: DeltaX(:),MaxElev(:)
      END SUBROUTINE Abstract_StrmGWConnector_Simulate
      
      
      SUBROUTINE Abstract_StrmGWConnector_CompileConductance(Connector,InFile,AppGrid,NStrmNodes,UpstrmNodes,DownstrmNodes,iStat)
        IMPORT                         :: BaseStrmGWConnectorType,GenericFileType,AppGridType
        CLASS(BaseStrmGWConnectorType) :: Connector
        TYPE(GenericFileType)          :: InFile
        TYPE(AppGridType),INTENT(IN)   :: AppGrid
        INTEGER,INTENT(IN)             :: NStrmNodes,UpstrmNodes(:),DownstrmNodes(:)
        INTEGER,INTENT(OUT)            :: iStat
      END SUBROUTINE Abstract_StrmGWConnector_CompileConductance
      
  END INTERFACE

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_BaseStrmGWConnector::'
  
  
  
  
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
  SUBROUTINE BaseStrmGWConnector_ReadPreprocessedData(Connector,InFile,iStat)
    CLASS(BaseStrmGWConnectorType) :: Connector
    TYPE(GenericFileType)          :: InFile
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+40) :: ThisProcedure = ModName // 'BaseStrmGWConnector_ReadPreprocessedData'
    INTEGER                      :: nNodes,ErrorCode
    CHARACTER                    :: cErrMsg*500
    
    CALL InFile%ReadData(nNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Connector%iGWNode(nNodes)     , &
              Connector%iLayer(nNodes)      , &
              STAT = ErrorCode              , &
              ERRMSG = cErrMsg              )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for stream-gw connection data!'//NEW_LINE('x')//TRIM(cErrMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    CALL InFile%ReadData(Connector%iGWNode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(Connector%iLayer,iStat)  
    
  END SUBROUTINE BaseStrmGWConnector_ReadPreprocessedData
  
  
  ! -------------------------------------------------------------
  ! --- SET GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_AddGWNodes(Connector,iGWNodes,iLayers,iStat)
    CLASS(BaseStrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)             :: iGWNodes(:),iLayers(:)
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'BaseStrmGWConnector_AddGWNodes'
    INTEGER                      :: NStrmNodes,ErrorCode
    CHARACTER                    :: cErrorMsg*300
    
    !Initialize
    iStat      = 0
    NStrmNodes = SIZE(iGWNodes)
    
    !Allocate memory
    ALLOCATE (Connector%iGWNode(NStrmNodes) , Connector%iLayer(NStrmNodes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream-gw connection data!'//NEW_LINE('x')//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store information
    Connector%iGWNode = iGWNodes
    Connector%iLayer  = iLayers
    
  END SUBROUTINE BaseStrmGWConnector_AddGWNodes
  
    
  
  
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
  SUBROUTINE BaseStrmGWConnector_Kill(Connector)
    CLASS(BaseStrmGWConnectorType) :: Connector
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (Connector%iGWNode , Connector%iLayer , Connector%Conductance , Connector%StrmGWFlow , STAT=ErrorCode)
    Connector%iVersion            = 0
    Connector%TimeUnitConductance = ''
    
  END SUBROUTINE BaseStrmGWConnector_Kill
  
  
  
  
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
  FUNCTION BaseStrmGWConnector_GetSubregionalFlows(Connector,AppGrid) RESULT(Flows)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    REAL(8)                                   :: Flows(AppGrid%NSubregions)
    
    !Compute regional flows
    Flows = AppGrid%AccumSomeNodeValuesToSubregions(Connector%iGWNode,Connector%StrmGWFlow)
    
  END FUNCTION BaseStrmGWConnector_GetSubregionalFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM-GW FLOW AT EVERY STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_GetFlowAtAllStrmNodes(Connector,Flow)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    REAL(8),INTENT(OUT)                       :: Flow(SIZE(Connector%StrmGWFlow)) 
    
    Flow = Connector%StrmGWFlow
  
  END SUBROUTINE BaseStrmGWConnector_GetFlowAtAllStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT A SET OF STREAM NODES 
  ! -------------------------------------------------------------
  FUNCTION BaseStrmGWConnector_GetFlowAtSomeStrmNodes(Connector,iNodeBegin,iNodeEnd) RESULT(Flow)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                        :: iNodeBegin,iNodeEnd
    REAL(8)                                   :: Flow 
    
    Flow = SUM(Connector%StrmGWFlow(iNodeBegin:iNodeEnd))
  
  END FUNCTION BaseStrmGWConnector_GetFlowAtSomeStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW AT GROUNDWATER NODE AT A LAYER
  ! -------------------------------------------------------------
  FUNCTION BaseStrmGWConnector_GetFlowAtGWNode(Connector,iNode,iLayer) RESULT(Flow)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                        :: iNode,iLayer
    REAL(8)                                   :: Flow
    
    !Local variables
    INTEGER :: indxNode
    
    !Initialize
    Flow = 0.0
    
    !Get flow
    DO indxNode=1,SIZE(Connector%iGWNode)
      IF (Connector%iGWNode(indxNode) .EQ. iNode) THEN
        IF (Connector%iLayer(indxNode) .EQ. iLayer) Flow = Flow + Connector%StrmGWFlow(indxNode)
      END IF
    END DO
    
  END FUNCTION BaseStrmGWConnector_GetFlowAtGWNode
    
    
  ! -------------------------------------------------------------
  ! --- GET LAYER FOR GW NODE
  ! -------------------------------------------------------------
  FUNCTION BaseStrmGWConnector_GetLayer(Connector,iNode) RESULT(Layer)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                        :: iNode
    INTEGER                                   :: Layer
    
    Layer = Connector%iLayer(iNode)
    
  END FUNCTION BaseStrmGWConnector_GetLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_GetAllLayers(Connector,Layers)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,ALLOCATABLE                       :: Layers(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (Layers , STAT=ErrorCode)
    ALLOCATE (Layers(SIZE(Connector%iLayer)))
    
    Layers = Connector%iLayer
    
  END SUBROUTINE BaseStrmGWConnector_GetAllLayers
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_GetAllGWNodes(Connector,GWNodes)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,ALLOCATABLE                       :: GWNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (GWNodes , STAT=ErrorCode)
    ALLOCATE (GWNodes(SIZE(Connector%iGWNode)))
    
    GWNodes = Connector%iGWNode
    
  END SUBROUTINE BaseStrmGWConnector_GetAllGWNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET GW NODE
  ! -------------------------------------------------------------
  FUNCTION BaseStrmGWConnector_GetGWNode(Connector,iNode) RESULT(GWNode)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                        :: iNode
    INTEGER                                   :: GWNode
    
    GWNode = Connector%iGWNode(iNode)
    
  END FUNCTION BaseStrmGWConnector_GetGWNode
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEADS AT STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_GetGWHeadsAtStrmNodes(Connector,GWHeads,GWHeadsAtStrmNodes)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    REAL(8),INTENT(IN)                        :: GWHeads(:,:)
    REAL(8),INTENT(OUT)                       :: GWHeadsAtStrmNodes(:)
    
    !Local variables
    INTEGER :: indxStrm
    
    DO indxStrm=1,SIZE(Connector%iGWNode)
        GWHeadsAtStrmNodes(indxStrm) = GWHeads(Connector%iGWNode(indxStrm),Connector%iLayer(indxStrm))
    END DO
    
  END SUBROUTINE BaseStrmGWConnector_GetGWHeadsAtStrmNodes 
  


  
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
  SUBROUTINE BaseStrmGWConnector_SetConductance(Connector,TimeUnitConductance,Conductance)
    CLASS(BaseStrmGWConnectorType) :: Connector
    CHARACTER(LEN=6),INTENT(IN)    :: TimeUnitConductance
    REAL(8),INTENT(IN)             :: Conductance(:)
    
    !Allocate memeory if not allocated
    IF (.NOT. ALLOCATED(Connector%Conductance)) ALLOCATE (Connector%Conductance(SIZE(Conductance)))
    
    !Store data
    Connector%Conductance         = Conductance
    Connector%TimeUnitConductance = TimeUnitConductance
    
  END SUBROUTINE BaseStrmGWConnector_SetConductance
  
  
  ! -------------------------------------------------------------
  ! --- SET STREAM-GW INTERACTION AT A NODE
  ! --- * Note: This should be necsaary only for specified-stage type stream nodes
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_SetStrmGWFlow(Connector,iNode,StrmGWFlow)
    CLASS(BaseStrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)             :: iNode
    REAL(8),INTENT(IN)             :: StrmGWFlow
     
    Connector%StrmGWFlow(iNode) = StrmGWFlow
    
  END SUBROUTINE BaseStrmGWConnector_SetStrmGWFlow


  
   
  
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
  SUBROUTINE BaseStrmGWConnector_WritePreprocessedData(Connector,OutFile)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(GenericFileType)                     :: OutFile
    
    CALL Outfile%WriteData(Connector%iVersion)
    CALL Outfile%WriteData(SIZE(Connector%iGWNode))
    CALL Outfile%WriteData(Connector%iGWNode)
    CALL Outfile%WriteData(COnnector%iLayer)
    
  END SUBROUTINE BaseStrmGWConnector_WritePreprocessedData
  
  
  
  
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
  ! --- CONVERT TIME UNIT OF CONNECTOR RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_ConvertTimeUnit(Connector,NewUnit)
    CLASS(BaseStrmGWConnectorType) :: Connector
    CHARACTER(LEN=*),INTENT(IN)    :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
  
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert conductance time unit
    Factor                        = TimeIntervalConversion(NewUnit,Connector%TimeUnitConductance)
    Connector%TimeUnitConductance = NewUnit
    Connector%Conductance         = Connector%Conductance * Factor

  END SUBROUTINE BaseStrmGWConnector_ConvertTimeUnit 
  
  
  ! -------------------------------------------------------------
  ! --- ADD CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_RegisterWithMatrix(Connector,AppGrid,Matrix,iStat)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridTYpe),INTENT(IN)              :: AppGrid
    TYPE(MatrixType)                          :: Matrix
    INTEGER,INTENT(OUT)                       :: iStat
    
    !Local varaibles
    INTEGER :: indxNode,GWNode(1),StrmNode(1),NNodes
    
    !Initialize
    iStat  = 0
    NNodes = AppGrid%NNodes
    
    !Add connectivity
    DO indxNode=1,SIZE(Connector%iGWNode)
        StrmNode(1) = indxNode
        GWNode(1)   = (Connector%iLayer(indxNode)-1)*NNodes + Connector%iGWNode(indxNode)
        CALL Matrix%AddConnectivity(iStrmComp,indxNode,iGWComp,GWNode,iStat)     ;  IF (iStat .EQ. -1) RETURN
        CALL Matrix%AddConnectivity(iGWComp,GWNode(1),iStrmComp,StrmNode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE BaseStrmGWConnector_RegisterWithMatrix
  
END MODULE