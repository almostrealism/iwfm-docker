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
MODULE Class_BaseStrmGWConnector
  USE MessageLogger          , ONLY: SetLastMessage       , &
                                     MessageArray         , &
                                     f_iFatal
  USE IOInterface
  USE TimeSeriesUtilities
  USE GeneralUtilities
  USE Package_Discretization , ONLY: AppGridType          , &
                                     StratigraphyType
  USE Package_Misc           , ONLY: AbstractFunctionType , &
                                     f_iStrmComp          , &
                                     f_iGWComp        
  USE Package_Matrix         , ONLY: MatrixType           , &
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
  PUBLIC :: BaseStrmGWConnectorType  , &
            BaseStrmGWConnector_Kill , &
            iDisconnectAtTopOfBed    , &
            iDisconnectAtBottomOfBed
  
  
  ! -------------------------------------------------------------
  ! --- PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iDisconnectAtTopOfBed    = 1 , &
                       iDisconnectAtBottomOfBed = 2 , &
                       iDisconnectTypeArray(2)  = [iDisconnectAtTopOfBed    , &
                                                   iDisconnectAtBottomOfBed ]
  
  
  ! -------------------------------------------------------------
  ! --- STREAM-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT:: BaseStrmGWConnectorType
    INTEGER             :: iVersion            = 0                     !Version number
    INTEGER             :: iInteractionType    = iDisconnectAtTopOfBed !Flag describing how stream-gw interaction will be computed
    CHARACTER(LEN=6)    :: TimeUnitConductance = ''                    !Time unit of conductance
    INTEGER,ALLOCATABLE :: iGWNode(:)                                  !GW node number for each stream node
    INTEGER,ALLOCATABLE :: iLayer(:)                                   !Aquifer layer number that the stream nodes interact with
    REAL(8),ALLOCATABLE :: Conductance(:)                              !Stream bed conductance
    REAL(8),ALLOCATABLE :: rBedThickness(:)                            !Stream bed thickness
    REAL(8),ALLOCATABLE :: rDisconnectElev(:)                          !Elevation at which stream and gw disconnect 
    REAL(8),ALLOCATABLE :: rFractionForGW(:)                           !Fraction of the stream-aquifer interaction that the groundwater node is exposed to
    REAL(8),ALLOCATABLE :: StrmGWFlow(:)                               !Stream-gw interaction (+ is flow from stream to gw)
  CONTAINS
    PROCEDURE(Abstract_StrmGWConnector_Simulate),PASS,DEFERRED           :: Simulate 
    PROCEDURE(Abstract_StrmGWConnector_CompileConductance),PASS,DEFERRED :: CompileConductance
    PROCEDURE,PASS                                                       :: BaseStrmGWConnector_AddGWNodes
    PROCEDURE,PASS                                                       :: BaseStrmGWConnector_ReadPreprocessedData
    PROCEDURE,PASS                                                       :: Kill                    => BaseStrmGWConnector_Kill
    PROCEDURE,PASS                                                       :: GetAllLayers            => BaseStrmGWConnector_GetAllLayers           
    PROCEDURE,PASS                                                       :: GetLayer                => BaseStrmGWConnector_GetLayer               
    PROCEDURE,PASS                                                       :: GetFlowAtGWNode         => BaseStrmGWConnector_GetFlowAtGWNode        
    PROCEDURE,PASS                                                       :: GetFlowAtAllStrmNodes   => BaseStrmGWConnector_GetFlowAtAllStrmNodes 
    PROCEDURE,PASS                                                       :: GetFlowAtSomeStrmNodes  => BaseStrmGWConnector_GetFlowAtSomeStrmNodes 
    PROCEDURE,PASS                                                       :: GetSubregionalFlows     => BaseStrmGWConnector_GetSubregionalFlows    
    PROCEDURE,PASS                                                       :: GetAllGWNodes           => BaseStrmGWConnector_GetAllGWNodes          
    PROCEDURE,PASS                                                       :: GetGWNode               => BaseStrmGWConnector_GetGWNode              
    PROCEDURE,PASS                                                       :: GetGWHeadsAtStrmNodes   => BaseStrmGWConnector_GetGWHeadsAtStrmNodes
    PROCEDURE,PASS                                                       :: SetInteractionType      => BaseStrmGWConnector_SetInteractionType         
    PROCEDURE,PASS                                                       :: SetConductance          => BaseStrmGWConnector_SetConductance 
    PROCEDURE,PASS                                                       :: SetFractionsForGW       => BaseStrmGWConnector_SetFractionsForGW
    PROCEDURE,PASS                                                       :: SetStrmGWFlow           => BaseStrmGWConnector_SetStrmGWFlow          
    PROCEDURE,PASS                                                       :: WritePreprocessedData   => BaseStrmGWConnector_WritePreprocessedData  
    PROCEDURE,PASS                                                       :: ConvertTimeUnit         => BaseStrmGWConnector_ConvertTimeUnit  
    PROCEDURE,PASS                                                       :: RegisterWithMatrix      => BaseStrmGWConnector_RegisterWithMatrix
    GENERIC                                                              :: New                     => BaseStrmGWConnector_AddGWNodes           , &
                                                                                                       BaseStrmGWConnector_ReadPreprocessedData 
  END TYPE BaseStrmGWConnectorType
    
  
  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE
  
      SUBROUTINE Abstract_StrmGWConnector_Simulate(Connector,iNNodes,rGWHeads,rStrmHeads,rAvailableFlows,Matrix,WetPerimeterFunction,rMaxElevs)
        IMPORT                                          :: BaseStrmGWConnectorType,AbstractFunctionType,MatrixType
        CLASS(BaseStrmGWConnectorType)                  :: Connector
        INTEGER,INTENT(IN)                              :: iNNodes
        REAL(8),INTENT(IN)                              :: rGWHeads(:),rStrmHeads(:),rAvailableFlows(:)
        TYPE(MatrixType)                                :: Matrix
        CLASS(AbstractFunctionType),OPTIONAL,INTENT(IN) :: WetPerimeterFunction(:)                     
        REAL(8),OPTIONAL,INTENT(IN)                     :: rMaxElevs(:)
      END SUBROUTINE Abstract_StrmGWConnector_Simulate
      
      
      SUBROUTINE Abstract_StrmGWConnector_CompileConductance(Connector,InFile,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,UpstrmNodes,DownstrmNodes,BottomElevs,iStat)
        IMPORT                            :: BaseStrmGWConnectorType,GenericFileType,AppGridType,StratigraphyType
        CLASS(BaseStrmGWConnectorType)    :: Connector
        TYPE(GenericFileType)             :: InFile
        TYPE(AppGridType),INTENT(IN)      :: AppGrid
        TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
        INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes),UpstrmNodes(:),DownstrmNodes(:)
        REAL(8),INTENT(IN)                :: BottomElevs(:)
        INTEGER,INTENT(OUT)               :: iStat
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
    ALLOCATE (Connector%iGWNode(nNodes)       , &
              Connector%iLayer(nNodes)        , &
              Connector%rFractionForGW(nNodes), &
              STAT = ErrorCode                , &
              ERRMSG = cErrMsg                )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for stream-gw connection data!'//NEW_LINE('x')//TRIM(cErrMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    CALL InFile%ReadData(Connector%iGWNode,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(Connector%iLayer,iStat)         ;  IF (iStat .EQ. -1) RETURN  
    CALL InFile%ReadData(Connector%rFractionForGW,iStat)    
    
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
    ALLOCATE (Connector%iGWNode(NStrmNodes) , Connector%iLayer(NStrmNodes) , Connector%rFractionForGW(NStrmNodes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for stream-gw connection data!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store information
    Connector%iGWNode        = iGWNodes
    Connector%iLayer         = iLayers
    Connector%rFractionForGW = 1.0      !This is the default
    
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
    
    DEALLOCATE (Connector%iGWNode , Connector%iLayer , Connector%Conductance , Connector%rBedThickness , Connector%rDisconnectElev, Connector%rFractionForGW , Connector%StrmGWFlow , STAT=ErrorCode)
    Connector%iVersion            = 0
    Connector%iInteractionType    = iDisconnectAtTopOfBed
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
  FUNCTION BaseStrmGWConnector_GetSubregionalFlows(Connector,AppGrid,lInsideModel) RESULT(Flows)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    LOGICAL,INTENT(IN)                        :: lInsideModel
    REAL(8)                                   :: Flows(AppGrid%NSubregions)
    
    !Local variables
    REAL(8) :: StrmGWFlow(SIZE(Connector%StrmGWFlow))
    
    !Initialize
    IF (lInsideModel) THEN
        StrmGWFlow = Connector%StrmGWFlow * Connector%rFractionForGW
    ELSE
        StrmGWFlow = Connector%StrmGWFlow * (1D0 - Connector%rFractionForGW)
    END IF
    
    !Compute regional flows
    Flows = AppGrid%AccumSomeNodeValuesToSubregions(Connector%iGWNode,StrmGWFlow)
    
  END FUNCTION BaseStrmGWConnector_GetSubregionalFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM-GW FLOW AT EVERY STREAM NODE 
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_GetFlowAtAllStrmNodes(Connector,lInsideModel,Flow)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    LOGICAL,INTENT(IN)                        :: lInsideModel
    REAL(8),INTENT(OUT)                       :: Flow(:) 
    
    IF (lInsideModel) THEN
        Flow = Connector%StrmGWFlow * Connector%rFractionForGW
    ELSE
        Flow = Connector%StrmGWFlow * (1D0 - Connector%rFractionForGW)
    END IF
  
  END SUBROUTINE BaseStrmGWConnector_GetFlowAtAllStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL STREAM-GW FLOW AT A SET OF STREAM NODES 
  ! -------------------------------------------------------------
  FUNCTION BaseStrmGWConnector_GetFlowAtSomeStrmNodes(Connector,iNodeBegin,iNodeEnd,lInsideModel) RESULT(Flow)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                        :: iNodeBegin,iNodeEnd
    LOGICAL,INTENT(IN)                        :: lInsideModel
    REAL(8)                                   :: Flow 
    
    IF (lInsideModel) THEN
        Flow = SUM(Connector%StrmGWFlow(iNodeBegin:iNodeEnd) * Connector%rFractionForGW(iNodeBegin:iNodeEnd))
    ELSE
        Flow = SUM(Connector%StrmGWFlow(iNodeBegin:iNodeEnd) * (1D0 - Connector%rFractionForGW(iNodeBegin:iNodeEnd)))
    END IF
  
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
        IF (Connector%iLayer(indxNode) .EQ. iLayer) Flow = Flow + Connector%StrmGWFlow(indxNode) * Connector%rFractionForGW(indxNode)
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
  PURE FUNCTION BaseStrmGWConnector_GetGWNode(Connector,iNode) RESULT(GWNode)
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
  ! --- SET STREAM-GW INTERACTION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_SetInteractionType(Connector,iInteractionType,iStat)
    CLASS(BaseStrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)             :: iInteractionType
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+38),PARAMETER :: ThisProcedure = ModName // 'BaseStrmGWConnector_SetInteractionType'
    
    !Initialize
    iStat = 0
    
    !Check if the flag is recognized
    IF (LocateInList(iInteractionType,iDisconnectTypeArray) .EQ. 0) THEN
        MessageArray(1) = 'While reading Main Stream Parameters Data File, the flag (INTRCTYPE) used to'
        MessageArray(2) = 'describe hydraulic disconnection between stream and groundwater is not recognized!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store flag
    Connector%iInteractionType = iInteractionType
    
  END SUBROUTINE BaseStrmGWConnector_SetInteractionType
  
  
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
  ! --- * Note: This should be necessary only for specified-stage type stream nodes
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_SetStrmGWFlow(Connector,iNode,StrmGWFlow)
    CLASS(BaseStrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)             :: iNode
    REAL(8),INTENT(IN)             :: StrmGWFlow
     
    Connector%StrmGWFlow(iNode) = StrmGWFlow
    
  END SUBROUTINE BaseStrmGWConnector_SetStrmGWFlow


  ! -------------------------------------------------------------
  ! --- SET STREAM NODES AND FRACTION OF STREAM-GW INTERACTION TO BE APPLIED TO GW NODES
  ! -------------------------------------------------------------
  SUBROUTINE BaseStrmGWConnector_SetFractionsForGW(Connector,iStrmNodes,rFractions,iStat)
    CLASS(BaseStrmGWConnectorType) :: Connector
    INTEGER,INTENT(IN)             :: iStrmNodes(:)
    REAL(8),INTENT(IN)             :: rFractions(:)
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+37),PARAMETER :: ThisProcedure = ModName // 'BaseStrmGWConnector_SetFractionsForGW'
    INTEGER                                :: indx,indx1
    
    !Initailize
    iStat = 0
    
    !Check for errors
    DO indx=1,SIZE(iStrmNodes)
        !MAke sure the same stream node is not listed more than once
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
            MessageArray(2) = ' to be applied to corresponding groundwater node must be between 0.0 and 1.0.' 
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO
     
    Connector%rFractionForGW(iStrmNodes) = rFractions
    
  END SUBROUTINE BaseStrmGWConnector_SetFractionsForGW
  
   
  
  
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
    CALL Outfile%WriteData(Connector%iLayer)
    CALL Outfile%WriteData(Connector%rFractionForGW)
    
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
  SUBROUTINE BaseStrmGWConnector_RegisterWithMatrix(Connector,StrmConnectivity,AppGrid,Matrix,iStat)
    CLASS(BaseStrmGWConnectorType),INTENT(IN) :: Connector
    TYPE(ConnectivityListType),INTENT(IN)     :: StrmConnectivity(:)  
    TYPE(AppGridTYpe),INTENT(IN)              :: AppGrid
    TYPE(MatrixType)                          :: Matrix
    INTEGER,INTENT(OUT)                       :: iStat
    
    !Local varaibles
    INTEGER :: indxNode,GWNode(1),StrmNodes(20),NNodes,iNUpstrmNodes
    
    !Initialize
    iStat  = 0
    NNodes = AppGrid%NNodes
    
    !Add connectivity
    DO indxNode=1,SIZE(Connector%iGWNode)
        GWNode(1)    = (Connector%iLayer(indxNode)-1)*NNodes + Connector%iGWNode(indxNode)
        CALL Matrix%AddConnectivity(f_iStrmComp,indxNode,f_iGWComp,GWNode,iStat)     ;  IF (iStat .EQ. -1) RETURN
        iNUpstrmNodes                = StrmConnectivity(indxNode)%nConnectedNodes
        StrmNodes(1)                 = indxNode
        StrmNodes(2:iNUpstrmNodes+1) = StrmConnectivity(indxNode)%ConnectedNodes 
        CALL Matrix%AddConnectivity(f_iGWComp,GWNode(1),f_iStrmComp,StrmNodes(1:iNUpstrmNodes+1),iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE BaseStrmGWConnector_RegisterWithMatrix
  
END MODULE