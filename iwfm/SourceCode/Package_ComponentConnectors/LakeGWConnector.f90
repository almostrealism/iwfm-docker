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
MODULE LakeGWConnector
  USE MessageLogger          , ONLY: SetLastMessage  , &
                                     EchoProgress    , &
                                     f_iFatal
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE IOInterface
  USE Package_Discretization
  USE Package_Misc           , ONLY: f_iLakeComp     , &
                                     f_iGWComp
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
  PUBLIC :: LakeGWConnectorType               
  
  
  ! -------------------------------------------------------------
  ! --- LAKE-GW CONNECTOR FOR ELEMENT TYPE
  ! -------------------------------------------------------------
  TYPE ElemLakeGWConnectorType
    PRIVATE
    INTEGER             :: NGWNode         = 0    !Number of nodes for lake element (equal to NVertex from the grid)
    INTEGER             :: iElemID         = 0    !Element ID
    INTEGER,ALLOCATABLE :: iGWNode(:)             !GW node numbers for a lake element
    INTEGER,ALLOCATABLE :: iLayer(:)              !Aquifer layer number that each lake node interacts with
    REAL(8),ALLOCATABLE :: Conductance(:)         !Lake bed conductance
    REAL(8),ALLOCATABLE :: Flow(:)                !Lake-gw interaction (+ is flow from lake to gw)
  END TYPE ElemLakeGWConnectorType
  
  
  ! -------------------------------------------------------------
  ! --- INDIVIDUAL LAKE-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE SingleLakeGWConnectorType
    PRIVATE
    INTEGER                                   :: NElems            = 0
    TYPE(ElemLakeGWConnectorType),ALLOCATABLE :: ElemConnectors(:)
  END TYPE SingleLakeGWConnectorType
  
  
  ! -------------------------------------------------------------
  ! --- LAKE-GW CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE LakeGWConnectorType
      PRIVATE
      LOGICAL                                     :: lDefined            = .FALSE.
      CHARACTER(LEN=6)                            :: TimeUnitConductance = ''      !Time unit of conductance
      TYPE(SingleLakeGWConnectorType),ALLOCATABLE :: Lakes(:)                      !List of connectors for all lakes
  CONTAINS
      PROCEDURE,PASS :: AddGWNodes
      PROCEDURE,PASS :: Kill 
      PROCEDURE,PASS :: IsDefined
      PROCEDURE,PASS :: GetFlowAtLakes 
      PROCEDURE,PASS :: GetFlowAtLakeElements
      PROCEDURE,PASS :: GetFlowAtAllLakeElements
      PROCEDURE,PASS :: GetFlowAtGWNode        
      PROCEDURE,PASS :: GetSubregionalFlows    
      PROCEDURE,PASS :: SetConductance
      PROCEDURE,PASS :: ReadPreprocessedData
      PROCEDURE,PASS :: WritePreprocessedData
      PROCEDURE,PASS :: RegisterWithMatrix
      PROCEDURE,PASS :: Simulate
      PROCEDURE,PASS :: ConvertTimeUnit        
      PROCEDURE,PASS :: ComputeLakeGWFlow
      GENERIC        :: New => ReadPreprocessedData
  END TYPE LakeGWConnectorType 

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 17
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'LakeGWConnector::'
  
  
  
  
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
  ! --- ADD GW NODES THAT CONNECT TO A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE AddGWNodes(Connector,iLakeNo,iElems,AppGrid,Stratigraphy)
    CLASS(LakeGWConnectorType)        :: Connector
    INTEGER,INTENT(IN)                :: iLakeNo,iElems(:)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    
    !Local variables
    INTEGER                                     :: iDim,indxElem,iElem,NVertex,Vertex(4)
    TYPE(SingleLakeGWConnectorType),ALLOCATABLE :: Temp_LakeData(:)
    
    !Allocate memory for the lake
    iDim = SIZE(Connector%Lakes)
    IF (iLakeNo .GT. iDim) THEN
        ALLOCATE (Temp_LakeData(iLakeNo))
        IF (iDim .GT. 0) Temp_LakeData(1:iDim) = Connector%Lakes
        CALL MOVE_ALLOC(Temp_LakeData , Connector%Lakes)
    END IF
    
    !Add lake data to the connector
    ASSOCIATE (pLake => Connector%Lakes(iLakeNo))
    
      !Allocate memory
      pLake%NElems = SIZE(iElems)
      ALLOCATE (pLake%ElemConnectors(SIZE(iElems)))
    
      !Process elements
      DO indxElem=1,SIZE(iElems)
        iElem   = iElems(indxElem)
        NVertex = AppGrid%NVertex(iElem)
        Vertex  = AppGrid%Vertex(:,iElem)
        ALLOCATE (pLake%ElemConnectors(indxElem)%iGWNode(NVertex)     , &
                  pLake%ElemConnectors(indxElem)%iLayer(NVertex)      , &
                  pLake%ElemConnectors(indxElem)%Conductance(NVertex) , &
                  pLAke%ElemConnectors(indxElem)%Flow(NVertex)        )
        pLake%ElemConnectors(indxElem)%NGWNode     = NVertex
        pLake%ElemConnectors(indxElem)%iElemID     = iElem
        pLake%ElemConnectors(indxElem)%iGWNode     = Vertex(1:NVertex)
        pLake%ElemConnectors(indxElem)%iLayer      = Stratigraphy%TopActiveLayer(Vertex(1:NVertex))        
      END DO
      
    END ASSOCIATE
    
    Connector%lDefined = .TRUE.
    
  END SUBROUTINE AddGWNodes
  
      
  
  
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
  ! --- KILL THE LakeGWConnector
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Connector)
    CLASS(LakeGWConnectorType) :: Connector
    
    !Local variables
    INTEGER                   :: ErrorCode
    TYPE(LakeGWConnectorType) :: Dummy
    
    DEALLOCATE (Connector%Lakes , STAT=ErrorCode)
    Connector%TimeUnitConductance = Dummy%TimeUnitConductance
    
    Connector%lDefined = .FALSE.
    
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
  ! --- IS LAKE-GW CONNECTOR DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(Connector) RESULT(lDefined)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
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
  ! --- GET SUBREGIONAL LAKE-GW INTERACTIONS
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalFlows(Connector,AppGrid) RESULT(Flows)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    REAL(8)                               :: Flows(AppGrid%NSubregions)
    
    !Local variables
    INTEGER :: indxLake,indxElem,iElem,iRegion
    
    !Initialize
    Flows = 0.0
    
    DO indxLake=1,SIZE(Connector%Lakes)
      ASSOCIATE (pElemConnectors => Connector%Lakes(indxLake)%ElemConnectors)
        DO indxElem=1,Connector%Lakes(indxLake)%NElems
          iElem          = pElemConnectors(indxElem)%iElemID
          iRegion        = AppGrid%AppElement(iElem)%Subregion
          Flows(iRegion) = Flows(iRegion) + SUM(pElemConnectors(indxElem)%Flow)
        END DO
      END ASSOCIATE
    END DO
    
  END FUNCTION GetSubregionalFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE-GW INTERACTION AT ALL LAKES
  ! -------------------------------------------------------------
  FUNCTION GetFlowAtLakes(Connector) RESULT(Flows)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    REAL(8)                               :: Flows(SIZE(Connector%Lakes))
    
    !Local variables
    INTEGER :: indxLake,indxElem
    
    !Initialize
    Flows = 0.0
    
    DO indxLake=1,SIZE(Flows)
      ASSOCIATE (pElemConnectors => Connector%Lakes(indxLake)%ElemConnectors)
        DO indxElem=1,Connector%Lakes(indxLake)%NElems
          Flows(indxLake) = Flows(indxLake) + SUM(pElemConnectors(indxElem)%Flow)
        END DO
      END ASSOCIATE
    END DO
    
  END FUNCTION GetFlowAtLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE-GW INTERACTION AT A GW NODE
  ! -------------------------------------------------------------
  FUNCTION GetFlowAtGWNode(Connector,iElem,iGWNode,iLayer) RESULT(Flow)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iElem,iGWNode,iLayer
    REAL(8)                               :: Flow 
    
    !Local variables
    INTEGER :: indxLake,indxElem,indxNode
    
    !Initialize
    Flow = 0.0
    
    DO indxLake=1,SIZE(Connector%Lakes)
      ASSOCIATE (pElemConnectors => Connector%Lakes(indxLake)%ElemConnectors)
        DO indxElem=1,Connector%Lakes(indxLake)%NElems
          IF (pElemConnectors(indxElem)%iElemID .NE. iElem) CYCLE
          ASSOCIATE (piGWNode => pElemConnectors(indxElem)%iGWNode , &
                     piLayer  => pElemConnectors(indxElem)%iLayer  )
            DO indxNode=1,pElemConnectors(indxElem)%NGWNode
              IF (piGWNode(indxNode) .EQ. iGWNode) THEN
                IF (piLayer(indxNode) .EQ. iLayer) THEN
                  Flow = pElemConnectors(indxElem)%Flow(indxNode)
                  RETURN
                END IF
              END IF
            END DO       
          END ASSOCIATE
        END DO
      END ASSOCIATE
    END DO
    
  END FUNCTION GetFlowAtGWNode
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE-GW INTERACTION AT ELEMENTS OF A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetFlowAtLakeElements(Connector,iLake,Flows)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: iLake
    REAL(8),ALLOCATABLE                   :: Flows(:)
    
    !Local variables
    INTEGER :: ErrorCode,indxElem
    
    DEALLOCATE (Flows , STAT=ErrorCode)
    ALLOCATE (Flows(Connector%Lakes(iLake)%NElems))
    
    !Compile element flows
    DO indxElem=1,Connector%Lakes(iLake)%NElems
        Flows(indxElem) =SUM(Connector%Lakes(iLake)%ElemConnectors(indxElem)%Flow)
    END DO
    
  END SUBROUTINE GetFlowAtLakeElements
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE-GW INTERACTION AT ALL ELEMENTS OF ALL LAKES
  ! -------------------------------------------------------------
  SUBROUTINE GetFlowAtAllLakeElements(Connector,Flows)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    REAL(8),ALLOCATABLE                   :: Flows(:)
    
    !Local variables
    INTEGER :: ErrorCode,indxElem,indxLake,iCount
    
    DEALLOCATE (Flows , STAT=ErrorCode)
    ALLOCATE (Flows(SUM(Connector%Lakes%NElems)))
    
    !Compile element flows
    iCount = 1
    DO indxLake=1,SIZE(Connector%Lakes)
        DO indxElem=1,Connector%Lakes(indxLake)%NElems
            Flows(iCount) = SUM(Connector%Lakes(indxLake)%ElemConnectors(indxElem)%Flow)
            iCount        = iCount +1
        END DO
    END DO
    
  END SUBROUTINE GetFlowAtAllLakeElements
  
  
  
    
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
  ! --- SET CONDUCTANCE
  ! -------------------------------------------------------------
  SUBROUTINE SetConductance(Connector,AppGrid,iLakeNo,TimeUnitConductance,Conductance,iStat)
    CLASS(LakeGWConnectorType)   :: Connector
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: iLakeNo
    CHARACTER(LEN=*),INTENT(IN)  :: TimeUnitConductance
    REAL(8),INTENT(IN)           :: Conductance
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'SetConductance'
    INTEGER                      :: iElem,indxElem
    
    !Initialize
    iStat = 0
    
    ASSOCIATE (pLake => Connector%Lakes(iLakeNo))
    
      !Process elements
      DO indxElem=1,pLake%NElems
          iElem                                      = pLake%ElemConnectors(indxElem)%iElemID
          pLake%ElemConnectors(indxElem)%Conductance = Conductance * AppGrid%AppElement(iElem)%VertexArea
          pLake%ElemConnectors(indxElem)%Flow        = 0.0
          
          !Make sure that time unit for conductance is consistent within all lakes
          IF (Connector%TimeUnitConductance .NE. '') THEN
              IF (TRIM(Connector%TimeUnitConductance) .NE. TRIM(ADJUSTL(UpperCase(TimeUnitConductance)))) THEN
                  CALL SetLastMessage('Time unit for conductance between lakes is not consistent!',f_iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
              END IF
          ELSE
              Connector%TimeUnitConductance = ADJUSTL(UpperCase(TimeUnitConductance))
          END IF
      END DO
      
    END ASSOCIATE
    
  END SUBROUTINE SetConductance

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ PRE-PROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadPreprocessedData(Connector,InFile,iStat)
    CLASS(LakeGWConnectorType),INTENT(OUT) :: Connector
    TYPE(GenericFileType)                  :: InFile
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    INTEGER :: nLakes,indxLake,indxElem,nGWNode
    
    CALL InFile%ReadData(nLakes,iStat)  
    IF (iStat .EQ. -1) RETURN
    IF (nLakes .EQ. 0) RETURN
    
    ALLOCATE (Connector%Lakes(nLakes))
    DO indxLake=1,nLakes
        ASSOCIATE (pLake => Connector%Lakes(indxLake))
            CALL InFile%ReadData(pLake%NElems,iStat)  ;  IF (iStat .EQ. -1) RETURN
            ALLOCATE (pLake%ElemConnectors(pLake%NElems))
            DO indxElem=1,pLake%NElems
                CALL InFile%ReadData(nGWNode,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  pLake%ElemConnectors(indxElem)%NGWNode = nGWNode
                CALL InFile%ReadData(pLake%ElemConnectors(indxElem)%iElemID,iStat)  ;  IF (iStat .EQ. -1) RETURN
                ALLOCATE (pLake%ElemConnectors(indxElem)%iGWNode(nGWNode)     , &
                          pLake%ElemConnectors(indxElem)%iLayer(nGWNode)      , &
                          pLake%ElemConnectors(indxElem)%Conductance(nGWNode) , &
                          pLake%ElemConnectors(indxElem)%Flow(nGWNode)        )
                CALL InFile%ReadData(pLake%ElemConnectors(indxElem)%iGWNode,iStat)  ;  IF (iStat .EQ. -1) RETURN
                CALL InFile%ReadData(pLake%ElemConnectors(indxElem)%iLayer,iStat)   ;  IF (iStat .EQ. -1) RETURN
            END DO
        END ASSOCIATE
    END DO
    
    Connector%lDefined = .TRUE.
    
  END SUBROUTINE ReadPreprocessedData

  

  
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
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    TYPE(GenericFileType)                 :: OutFile
    
    !Local variables
    INTEGER :: indxLake,indxElem,nLakes
    
    nLakes = SIZE(Connector%Lakes)
    CALL Outfile%WriteData(nLakes)
    IF (nLakes .EQ. 0) RETURN
    
    DO indxLake=1,nLakes
        ASSOCIATE (pLake => Connector%Lakes(indxLake))
            CALL Outfile%WriteData(pLake%NElems)
            DO indxElem=1,pLake%NElems
                CALL Outfile%WriteData(pLake%ElemConnectors(indxElem)%NGWNode)
                CALL Outfile%WriteData(pLake%ElemConnectors(indxElem)%iElemID)
                CALL Outfile%WriteData(pLake%ElemConnectors(indxElem)%iGWNode)
                CALL Outfile%WriteData(pLake%ElemConnectors(indxElem)%iLayer)
            END DO
        END ASSOCIATE
    END DO
    
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
  ! --- SIMULATE
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(Connector,LakeElevs,GWHeads,GSElevs,Matrix)
    CLASS(LakeGWConnectorType) :: Connector
    REAL(8),INTENT(IN)         :: LakeElevs(:),GWHeads(:,:),GSElevs(:)
    TYPE(MatrixType)           :: Matrix
    
    !Local variables
    INTEGER           :: NVertex,indxLake,indxElem,indxNode,iNode,NNodes,     &
                         iCompIDs_Connect(2),iNodeIDs_Connect(2),iGWNode,     &
                         iNodeIDs(2),NLakes
    REAL(8)           :: GSElev,rGWHead,rElev,rFlow,rConduc,rUpdateValues(2)
    INTEGER,PARAMETER :: iCompIDs(2) = [f_iLakeComp,f_iGWComp]
    
    !Initialize
    NNodes = SIZE(GSElevs)
    NLakes = SIZE(LakeElevs)
    
    DO indxLake=1,NLakes
      ASSOCIATE (pElemConnectors => Connector%Lakes(indxLake)%ElemConnectors)
        rElev       = LakeElevs(indxLake)
        iNodeIDs(1) = indxLake

        DO indxElem=1,Connector%Lakes(indxLake)%NElems
          ASSOCIATE (piGWNode     => pElemConnectors(indxElem)%iGWNode     , &
                     piLayer      => pElemConnectors(indxElem)%iLayer      , &
                     pConductance => pElemConnectors(indxElem)%Conductance , &
                     pFlow        => pElemConnectors(indxElem)%Flow        )
            NVertex = pElemConnectors(indxElem)%NGWNode
            DO indxNode=1,NVertex
              iNode   = piGWNode(indxNode)
              iGWNode = (piLayer(indxNode)-1)*NNodes + iNode
              GSElev  = GSElevs(iNode)
              rGWHead = GWHeads(iNode,piLayer(indxNode))
              rConduc = pConductance(indxNode)
            
              !Lake-gw interaction at current lake elevation and their effect on COEFF and RHS arrays
              IF (rElev .GE. GSElev) THEN
                IF (rGWHead .GE. GSElev) THEN
                  rFlow               = rConduc * (rElev-rGWHead)
                  iCompIDs_Connect(1) = f_iLakeComp
                  iCompIDs_Connect(2) = f_iGWComp
                  iNodeIDs_Connect(1) = indxLake
                  iNodeIDs_Connect(2) = iGWNode
                  rUpdateValues(1)    =  rConduc
                  rUpdateValues(2)    = -rConduc
                  CALL Matrix%UpdateCOEFF(f_iLakeComp,indxLake,2,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
                  rUpdateValues(1)    = -rConduc
                  rUpdateValues(2)    =  rConduc
                  CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,2,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
                ELSE
                  rFlow               = rConduc * (rElev-GSElev)
                  iCompIDs_Connect(1) = f_iLakeComp
                  iNodeIDs_Connect(1) = indxLake
                  rUpdateValues(1)    =  rConduc
                  CALL Matrix%UpdateCOEFF(f_iLakeComp,indxLake,1,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
                  rUpdateValues(1)    = -rConduc
                  CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,1,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
                END IF
              ELSE
                IF (rGWHead .GE. GSElev) THEN
                  rFlow                 = rConduc * (GSElev-rGWHead)
                  iCompIDs_Connect(1) = f_iGWComp
                  iNodeIDs_Connect(1) = iGWNode
                  rUpdateValues(1)    = -rConduc
                  CALL Matrix%UpdateCOEFF(f_iLakeComp,indxLake,1,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
                  rUpdateValues(1)    =  rConduc
                  CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,1,iCompIDs_Connect(1:1),iNodeIDs_Connect(1:1),rUpdateValues(1:1))
                ELSE
                  rFlow = 0.0
                END IF
              END IF
              pFlow(indxNode)  = rFlow
              iNodeIDs(2)      = iGWNode
              rUpdateValues(1) =  rFlow
              rUpdateValues(2) = -rFlow
              CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateValues)
            END DO
          END ASSOCIATE  
        END DO
      END ASSOCIATE
    END DO
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE LAKE-GW INTERACTION
  ! -------------------------------------------------------------
  FUNCTION ComputeLakeGWFlow(Connector,NLakes,LakeElevs,GWHeads,GSElevs) RESULT(Flows)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                    :: NLakes
    REAL(8),INTENT(IN)                    :: LakeElevs(:),GWHeads(:,:),GSElevs(:)
    REAL(8)                               :: Flows(NLakes)
    
    !Local variables
    INTEGER :: indxLake,indxElem,indxNode,iNode
    REAL(8) :: GSElev,rElev,rGWHead,rConduc,rFlow
    
    !Initialize
    Flows  = 0.0
    
    DO indxLake=1,NLakes
      rElev = LakeElevs(indxLake)
      DO indxElem=1,Connector%Lakes(indxLake)%NElems
      
        ASSOCIATE (pElemConnector => Connector%Lakes(indxLake)%ElemConnectors(indxElem))
          DO indxNode=1,pElemConnector%NGWNode
            iNode   = pElemConnector%iGWNode(indxNode)
            GSElev  = GSElevs(iNode)
            rGWHead = GWHeads(iNode,pElemConnector%iLayer(indxNode))
            rConduc = pElemConnector%Conductance(indxNode)
            IF (rElev .GE. GSElev) THEN
              IF (rGWHead .GE. GSElev) THEN
                rFlow = rConduc * (rElev-rGWHead)
              ELSE
                rFlow = rConduc * (rElev-GSElev)
              END IF
            ELSE
              IF (rGWHead .GE. GSElev) THEN
                rFlow = rConduc * (GSElev-rGWHead)
              ELSE
                rFlow = 0.0
              END IF
            END IF
            Flows(indxLake) = Flows(indxLake) + rFlow
          END DO
        END ASSOCIATE
        
      END DO
    END DO

  END FUNCTION ComputeLakeGWFlow

  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF CONNECTOR RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(Connector,NewUnit)
    CLASS(LakeGWConnectorType)  :: Connector
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    INTEGER :: indxLake,indxElem
    REAL(8) :: Factor
    
    !If the connector is not defined, return
    IF (.NOT. Connector%lDefined) RETURN
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert rating table flow time unit
    Factor                        = TimeIntervalConversion(NewUnit,Connector%TimeUnitConductance)
    Connector%TimeUnitConductance = NewUnit
    DO indxLake=1,SIZE(Connector%Lakes)
      DO indxElem=1,Connector%Lakes(indxLake)%NElems
        Connector%Lakes(indxLake)%ElemConnectors(indxElem)%Conductance = Connector%Lakes(indxLake)%ElemConnectors(indxElem)%Conductance * Factor
      END DO
    END DO
        
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- ADD LAKE-GW CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(Connector,AppGrid,Matrix,iStat)
    CLASS(LakeGWConnectorType),INTENT(IN) :: Connector
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(MatrixType)                      :: Matrix
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'RegisterWithMatrix'
    INTEGER                      :: indxLake,LakeID(1),nGWNodes,indxElem,indxNode,NNodes
    INTEGER,ALLOCATABLE          :: GWNodes(:)
    
    !INitialize
    iStat = 0
    
    !Inform user
    IF (SIZE(Connector%Lakes) .GT. 0) &
        CALL EchoProgress('Registering lake-groundwater connector with matrix...')
    
    !Initialize
    NNodes = AppGrid%NNodes
    
    !Add connectivity
    DO indxLake=1,SIZE(Connector%Lakes)
        LakeID(1) = indxLake
        ASSOCIATE (pLake => Connector%Lakes(indxLake))
            DO indxElem=1,pLake%NElems
                nGWNodes = pLake%ElemConnectors(indxElem)%NGWNode
                CALL AllocArray(GWNodes,nGWNodes,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
                GWNodes = pLake%ElemConnectors(indxElem)%iGWNode
                GWNodes = (pLake%ElemConnectors(indxElem)%iLayer-1)*NNodes + GWNodes
                CALL Matrix%AddConnectivity(f_iLakeComp,indxLake,f_iGWComp,GWNodes,iStat)
                IF (iStat .EQ. -1) RETURN
                DO indxNode=1,nGWNodes
                    CALL Matrix%AddConnectivity(f_iGWComp,GWNodes(indxNode),f_iLakeComp,LakeID,iStat)
                    IF (iStat .EQ. -1) RETURN
                END DO
            END DO
        END ASSOCIATE
    END DO
    
  END SUBROUTINE RegisterWithMatrix

  
END MODULE