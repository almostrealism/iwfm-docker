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
MODULE Class_GWZBudget
  USE MessageLogger               , ONLY: SetLastMessage            , &
                                          EchoProgress              , &
                                          f_iFatal                    
  USE GeneralUtilities            , ONLY: IntToText                 , &
                                          AllocArray                , &
                                          LocateInList              , &
                                          ArrangeText               , &
                                          StripTextUntilCharacter   , &
                                          ShellSort
  USE TimeSeriesUtilities         , ONLY: TimeStepType              , &
                                          IncrementTimeStamp        , &
                                          TimeStampToJulian         , &
                                          CTimeStep_To_RTimeStep    , &
                                          f_cRecognizedIntervals    , &
                                          f_iTimeStampLength
  USE IOInterface                 , ONLY: f_iUNKNOWN
  USE Package_Misc                , ONLY: f_iAllLocationIDsListed   , &
                                          f_iLocationType_Zone      , &
                                          f_iDataUnitType_Volume    , &
                                          f_iGWComp
  USE GWZBudget_Parameters         
  USE Package_Budget              , ONLY: f_iVR
  USE Package_ZBudget             , ONLY: ZBudgetHeaderType         , &
                                          ZBudgetType               , &
                                          SystemDataType            , &
                                          ZoneListType              , &
                                          f_iStorageType            , &
                                          f_iVerticalFlowType       , &
                                          f_iFaceFlowType           , &
                                          f_iElemDataType           , &
                                          f_iColumnHeaderLen
  USE Package_Discretization      , ONLY: AppGridType               , &
                                          StratigraphyType          
  USE Package_AppGW               , ONLY: AppGWType                 , &
                                          f_iSpFlowBCID             , &
                                          f_iSpHeadBCID             , &
                                          f_iGHBCID                 , &
                                          f_iConstrainedGHBCID      , &
                                          f_iTileDrain              , &
                                          f_iSubIrig                , &
                                          f_iPump_ElemPump          , &
                                          f_iPump_Well                
  USE Package_AppStream           , ONLY: AppStreamType             , &
                                          f_iDiverRecvLoss          , &
                                          f_iBypassRecvLoss           
  USE Package_AppLake             , ONLY: AppLakeType               
  USE Package_ComponentConnectors , ONLY: StrmGWConnectorType       , &
                                          LakeGWConnectorType       
  USE Package_AppSmallWatershed   , ONLY: AppSmallWatershedType     , &
                                          SWShedBaseFlowBCID        , &
                                          SWShedPercFlowBCID 
  USE IWFM_Core_Version           , ONLY: IWFM_Core
  IMPLICIT NONE
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! *** This module is responsible for computing face flows for Groundwater
! *** Zone Budget output and for Groundwater Subregional Budget output.  
! *** It solves a set of equations at each node to compute element face  
! *** flows.  At each node, the coefficient matrix has the following 
! *** form (assuming 5 faces connecting at a node:
! ***            --                   --
! ***            |  1  -1   0   0   0  |
! ***            |  0   1  -1   0   0  |
! ***            |  0   0   1  -1   0  |
! ***            |  0   0   0   1  -1  |
! ***            |  a1  a2  a3  a4  a5 |
! ***            --                   --
! *** 
! *** where a1 - a5 are coefficients calculated by using the irrotationality
! *** of the flow field (see Dogrul and Kadir, 2006, Journal of Hydraulic 
! *** Engineering, 132(11), 1206-1214). 
! ***
! *** The local face indices connecting at each node are stored in 
! *** AppGrid%AppNode%FaceID array in a counter-clockwise fashion. This order 
! *** is consistent with the coefficient matrix above to solve for face flows  
! *** at each node. Coefficients a1 - a5 are stored in array IrrotationalCoeff.
! ***
! *** For a boundary node, it is assumed that the boundary flux is uniform at
! *** the node.  Then the last equation to be solved is a1 * Q1 + a5 * Q5 = 0.0
! *** with a2 through a4 being zero.
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  
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
  PUBLIC :: GWZBudgetType     , &
            f_iZBudgetType_GW
  

  ! -------------------------------------------------------------
  ! --- RHS VECTOR AT A NODE FOR FACE FLOW COMPUTATIONS
  ! --- *Note: Assumes maximum 20 faces connecting at a node
  ! -------------------------------------------------------------
    TYPE RHSVectorType
        REAL(8) :: RHS(20)          
    END TYPE RHSVectorType

    
  ! -------------------------------------------------------------
  ! --- DATABASE FOR ZONE BUDGET SOLUTIONS
  ! -------------------------------------------------------------
  TYPE,EXTENDS(ZBudgetType) :: GWZBudgetType
      PRIVATE
      LOGICAL             :: lComputeFaceFlows       = .FALSE.
      LOGICAL             :: lComputeZBudgetFlows    = .FALSE.
      LOGICAL             :: lComputeNodalVelocities = .FALSE.
      INTEGER             :: NModelFlowTypes         = 0
      INTEGER,ALLOCATABLE :: ModelFlowTypes(:)                 !Flow types simulated in the model
      INTEGER,ALLOCATABLE :: ActiveLayerBelow(:,:)             !Active layer below each node for (node,layer) combination except last layer
      INTEGER,ALLOCATABLE :: IDR(:)
  CONTAINS
      PROCEDURE,PASS   :: Create
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetNColumns
      PROCEDURE,PASS   :: GetColumnTitles
      PROCEDURE,PASS   :: GetZBudget_List
      PROCEDURE,PASS   :: GetMonthlyFlows_GivenGWZBudget
      PROCEDURE,NOPASS :: GetMonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetCumGWStorChange_GivenGWZBudget
      PROCEDURE,NOPASS :: GetCumGWStorChange_GivenFile
      PROCEDURE,PASS   :: GetTSData
      PROCEDURE,PASS   :: GetOutFileName
      PROCEDURE,PASS   :: IsComputed
      PROCEDURE,PASS   :: IsOutFileDefined
      PROCEDURE,PASS   :: PrintResults
      GENERIC          :: New                     => Create
      GENERIC          :: GetMonthlyFlows         => GetMonthlyFlows_GivenFile              , &
                                                     GetMonthlyFlows_GivenGWZBudget
      GENERIC          :: GetCumGWStorChange      => GetCumGWStorChange_GivenFile           , &
                                                     GetCumGWStorChange_GivenGWZBudget
  END TYPE GWZBudgetType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iZBudgetType_GW        = f_iGWComp*1000 + 1 
  CHARACTER(LEN=23),PARAMETER :: f_cDescription_GWZBudget = 'Groundwater zone budget'
  
  
  ! -------------------------------------------------------------
  ! --- MISC ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 17
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_GWZBudget::'

  
 
  
CONTAINS
    
    
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CREATE NEW GW Z-BUDGET DATA FILE FOR POPULATING
  ! -------------------------------------------------------------
  SUBROUTINE Create(GWZBudget,IsForInquiry,cZBudgetOutFileName,AppGrid,Stratigraphy,AppGW,AppStream,AppLake,AppSWShed,StrmGWConnector,TimeStep,NTIME,lDeepPerc,lRootZone_Defined,iStat)
    CLASS(GWZBudgetType)                   :: GWZBudget
    LOGICAL,INTENT(IN)                     :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)            :: cZBudgetOutFileName
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppLakeType),INTENT(IN)           :: AppLake
    TYPE(AppSmallWaterShedType),INTENT(IN) :: AppSWShed
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    INTEGER,INTENT(IN)                     :: NTIME
    LOGICAL,INTENT(IN)                     :: lDeepPerc,lRootZone_Defined
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    INTEGER                 :: indxLayer,ErrorCode
    TYPE(ZBudgetHeaderType) :: Header
    TYPE(SystemDataType)    :: SystemData
    INTEGER,ALLOCATABLE     :: IDRLayers(:)
    TYPE(TimeStepType)      :: TimeStepLocal
    
    !Initialize
    iStat = 0
    IF (AppGW%IsGWBudgetGenerated()) GWZBudget%lComputeFaceFlows    = .TRUE.
    IF (cZBudgetOutFileName .NE. '') GWZBudget%lComputeZBudgetFlows = .TRUE.
    IF (AppGW%IsVelocityTecplotDefined()  .OR.  AppGW%IsCellVelocityOutputDefined()) THEN
        GWZBudget%lComputeFaceFlows       = .TRUE.
        GWZBudget%lComputeNodalVelocities = .TRUE.
    END IF
    
    !Return if groundwater budget and Z-Budget flows are not computed
    IF (.NOT. GWZBudget%lComputeFaceFlows) THEN
        IF (.NOT. GWZBudget%lComputeZBudgetFlows) RETURN
    END IF 
    
    !If opened for inquiry, open file and return
    IF (IsForInquiry) THEN
        IF (cZBudgetOutFileName .NE. '') CALL GWZBudget%New(cZBudgetOutFileName,iStat)
        RETURN
    END IF
    
    !Active layer below each node
    IF (Stratigraphy%NLayers .GT. 1) THEN
        ALLOCATE (GWZBudget%ActiveLayerBelow(AppGrid%NNodes,Stratigraphy%NLayers-1))
        DO indxLayer=1,Stratigraphy%NLayers-1
            GWZBudget%ActiveLayerBelow(:,indxLayer) = Stratigraphy%GetAllActiveLayerBelow(indxLayer)
        END DO
    END IF
    
    !Groundwater nodes corresponding to streamn nodes
    CALL StrmGWConnector%GetAllGWNodes(GWZBudget%IDR)
    CALL StrmGWConnector%GetAllLayers(IDRLayers)
    GWZBudget%IDR = (IDRLayers - 1) * AppGrid%NNodes + GWZBudget%IDR
    DEALLOCATE (IDRLayers , STAT=ErrorCode)
    
    !Time step received shows the timestamp at t=0; advance time to show that Z-Budget output is at t = 1
    TimeStepLocal                    = TimeStep
    TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes,1)
    TimeStepLocal%CurrentTimeStep    = 1
    
    !Compile header for Z-Budget and instantiate Z-Budget for output 
    CALL CompileHeaderSystemData(TimeStep,lDeepPerc,lRootZone_Defined,AppGW,AppStream,AppLake,AppSWShed,StrmGWConnector,AppGrid,Stratigraphy,Header,SystemData,GWZBudget%NModelFlowTypes,GWZBudget%ModelFlowTypes,iStat)
    IF (iStat .EQ. -1) RETURN
    CALL GWZBudget%ZBudgetType%New(cZBudgetOutFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE Create
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL GW Z-BUDGET DATA FILE FOR POPULATING
  ! -------------------------------------------------------------
  SUBROUTINE Kill(ZBudget)
    CLASS(GWZBudgetType) :: ZBudget
    
    !Local variables
    INTEGER              :: ErrorCode
    TYPE (GWZBudgetType) :: Dummy
    
    !Deallocate memory
    DEALLOCATE (ZBudget%ModelFlowTypes , ZBudget%ActiveLayerBelow , ZBudget%IDR , STAT=ErrorCode)
    
    !Kill the parent data type
    CALL ZBudget%ZBudgetType%Kill()
    
    !Reset arguments to their default values
    SELECT TYPE (ZBudget)
        TYPE IS(GWZBudgetType)
            ZBudget = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  
  
  
  
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
  ! --- GET Z-BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_List(ZBudget,iZBudgetTypeList,cZBudgetDescriptions,cZBudgetFiles)
    CLASS(GWZBudgetType),INTENT(IN)           :: ZBudget
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iZBudgetTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cZBudgetDescriptions(:),cZBudgetFiles(:)
    
    !Local variables
    INTEGER                  :: iErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (iZBudgetTypeList , cZBudgetDescriptions , cZBudgetFiles , STAT=iErrorCode)
         
    !Get the list if there is a Z-Budget generated
    IF (ZBudget%IsOutFileDefined()) THEN
        ALLOCATE (iZBudgetTypeList(1) , cZBudgetDescriptions(1) , cZBudgetFiles(1))
        CALL ZBudget%File%GetName(cFileName)
        cZBudgetFiles(1)        = cFileName
        iZBudgetTypeList(1)     = f_iZBudgetType_GW
        cZBudgetDescriptions(1) = f_cDescription_GWZBudget
    ELSE
        ALLOCATE (iZBudgetTypeList(0) , cZBudgetDescriptions(0) , cZBudgetFiles(0))
    END IF
     
  END SUBROUTINE GetZBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS FOR A ZONE (EXCLUDE TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE GetNColumns(ZBudget,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,iNCol,iStat)
    CLASS(GWZBudgetType),INTENT(IN) :: ZBudget
    INTEGER,INTENT(IN)              :: iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    INTEGER,INTENT(OUT)             :: iNCol,iStat
    
    !Local variables
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColumnHeaders(:)
    INTEGER,ALLOCATABLE                           :: iColumnList(:) 
    TYPE(ZoneListType)                            :: ZoneList
    INTEGER                                       :: iZonesWithNames(0),indx
    CHARACTER(LEN=1)                              :: cZoneNames(0)
    
    !Compile zone information
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the sub-data; first column will be Time so that will be eliminated later
    CALL ZBudget%GetFullColumnHeaders('area units','volume units',cColumnHeaders,iStat)
    IF (iStat .NE. 0) RETURN
    iNCol = SIZE(cColumnHeaders)
    ALLOCATE (iColumnList(iNCol))
    iColumnList = [(indx,indx=1,iNCol)]
    
    !Now get the number of diversified columns
    CALL ZBudget%GetNDiversifiedColumns(ZoneList,iZoneID,iColumnList,iNCol,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Remove time column from the count
    iNCol = iNCol - 1
    
  END SUBROUTINE GetNColumns

  
  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES FOR A ZONE (EXCLUDE TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE GetColumnTitles(ZBudget,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(GWZBudgetType),INTENT(IN)          :: ZBudget
    INTEGER,INTENT(IN)                       :: iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER,ALLOCATABLE                           :: iColumnList(:),iDummyIntArray(:) 
    TYPE(ZoneListType)                            :: ZoneList
    INTEGER                                       :: iZonesWithNames(0),indx,iNCol
    CHARACTER(LEN=1)                              :: cZoneNames(0)
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    !Compile zone information
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get the undiversified column titles; first column will be Time so that will be eliminated later
    CALL ZBudget%GetFullColumnHeaders(cUnitAR,cUnitVL,cColTitles_Local,iStat)
    IF (iStat .NE. 0) RETURN
    iNCol = SIZE(cColTitles_Local)
    ALLOCATE (iColumnList(iNCol))
    iColumnList = [(indx,indx=1,iNCol)]
    
    !Now get the diversified column titles
    CALL ZBudget%GetFullColumnHeaders(cUnitAR,cUnitVL,cColTitles_Local,iStat,ZoneList,iZoneID,iColumnList,iDummyIntArray)
    IF (iStat .NE. 0) RETURN
    
    !Remove time column from titles
    iNCol = iNCol - 1
    ALLOCATE(cColTitles(iNCol))
    cColTitles = cColTitles_Local(2:)
    
  END SUBROUTINE GetColumnTitles

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY ZONE BUDGET FLOWS FOR AN INTERVAL FOR A SELECTED ZONE FROM A GWZBudget OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GetMonthlyFlows_GivenGWZBudget(GWZBudget,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(GWZBudgetType),INTENT(IN)          :: GWZBudget 
    INTEGER,INTENT(IN)                       :: iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate    !Assumes cBeginDate and cEndDate are properly set for monthly average values
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)            !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat 
    
    !Local variables
    INTEGER            :: iZonesWithNames(0)  
    CHARACTER          :: cZoneNames(0)*1   
    TYPE(ZoneListType) :: ZoneList
    
    IF (GWZBudget%IsOutfileDefined()) THEN
        !Generate zone list
        CALL ZoneList%New(GWZBudget%Header%iNData,GWZBudget%Header%lFaceFlows_Defined,GWZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Retrieve data
        CALL GetMonthlyFlows_GivenFile(GWZBudget%ZBudgetType,ZoneList,iZoneID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    ELSE
        iStat = 0
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
    END IF
    
  END SUBROUTINE GetMonthlyFlows_GivenGWZBudget
  
  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY ZONE BUDGET FLOWS FOR AN INTERVAL FOR A SELECTED ZONE FROM A ZBUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetMonthlyFlows_GivenFile(ZBudget,ZoneList,iZoneID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(ZBudgetType),INTENT(IN)             :: ZBudget                !Assumes ZBudget file is already open 
    TYPE(ZoneListType),INTENT(IN)            :: ZoneList
    INTEGER,INTENT(IN)                       :: iZoneID
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate    !Assumes cBeginDate and cEndDate are properly set for monthly average values
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)            !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat 
    
    !Local variables
    INTEGER                                       :: indx,iNCols,iNTimeSteps,iNPopulatedValues,iNAdjZones,iDimFlows,indxTime
    INTEGER,ALLOCATABLE                           :: iColList(:),iDummyArray(:),iDataUnitTypes(:),iInflowCols(:),iOutflowCols(:)
    REAL(8),ALLOCATABLE                           :: rValues(:,:)
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColumnHeaders(:)
    TYPE(TimeStepType)                            :: TimeStep
    
    !Initialize
    iStat = 0
    
    !Get number of time steps stored in the ZBudget file
    CALL ZBudget%GetTimeStepRelatedData(iNTimeSteps,TimeStep)
    
    !Get the diversified column titles first
    CALL ZBudget%GetFullColumnHeaders('area units','volume units',cColumnHeaders,iStat)  ;  IF (iStat .NE. 0) RETURN
    iNCols = SIZE(cColumnHeaders)
    ALLOCATE (iColList(iNCols))
    iColList = [(indx,indx=1,iNCols)]
    DEALLOCATE (cColumnHeaders)
    CALL ZBudget%GetFullColumnHeaders('area units','volume units',cColumnHeaders,iStat,ZoneList,iZoneID,iColList,iDummyArray)  
    IF (iStat .NE. 0) RETURN
    
    !Now get the number of diversified columns; subtract 1 to eliminate the Time column
    iNCols = SIZE(cColumnHeaders) - 1
    DEALLOCATE (iColList)
    ALLOCATE (iColList(iNCols) , rValues(iNCols+1,iNTimeSteps) , iDataUnitTypes(iNCols))  !rValues need to include Time column so add 1 to iNCols
    iColList = [(indx,indx=1,iNCols)]
         
    !Read data for the interval
    CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,'1MON',cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iNPopulatedValues,rValues,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Compile inflow and outflow columns including those for adjacent zones
    iNAdjZones = ZoneList%GetNAdjacentZones(iZoneID)
    iDimFlows  = SIZE(ZBudget%Header%iErrorInCols) + iNAdjZones
    ALLOCATE (rFlows(iDimFlows,iNPopulatedValues),iInflowCols(iDimFlows),iOutflowCols(iDimFlows),cFlowNames(iDimFlows))
    iInflowCols  = [(indx,indx=2,2*iDimFlows,2)]
    iOutflowCols = [(indx+1,indx=2,2*iDimFlows,2)]
    
    !Compile monthly z-budget flows
    DO indxTime=1,iNPopulatedValues
        rFlows(:,indxTime)  = rValues(iInflowCols,indxTime) - rValues(iOutflowCols,indxTime)
    END DO 

    !Compile flow names
    DO indx=1,iDimFlows-iNAdjZones
        cFlowNames(indx) = StripTextUntilCharacter(cColumnHeaders(2*indx+1),'_')  !Skip Time column in cColumnHeaders
    END DO
    DO indx=iDimFlows-iNAdjZones+1,iDimFlows
        cFlowNames(indx) = StripTextUntilCharacter(cColumnHeaders(2*indx),'(')
    END DO   
    
    !Clear memory
    CALL ZoneList%Kill()
    
  END SUBROUTINE GetMonthlyFlows_GivenFile
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE CHANGE IN STORAGE FOR A ZONE FROM GWZBudget OBJECT 
  ! -------------------------------------------------------------
  SUBROUTINE GetCumGWStorChange_GivenGWZBudget(GWZBudget,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutDates,rCumStorChange,iStat)
    CLASS(GWZBudgetType),INTENT(IN)          :: GWZBudget     
    INTEGER,INTENT(IN)                       :: iZoneID,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate,cOutputInterval
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rOutDates(:),rCumStorChange(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33) :: ThisProcedure = ModName // 'GetCumGWStorChange_GivenGWZBudget'
    INTEGER                      :: iZonesWithNames(0)  
    CHARACTER                    :: cZoneNames(0)*1   
    TYPE(ZoneListType)           :: ZoneList
    
    IF (GWZBudget%IsOutfileDefined()) THEN
        !Generate zone list
        CALL ZoneList%New(GWZBudget%Header%iNData,GWZBudget%Header%lFaceFlows_Defined,GWZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)
        IF (iStat .NE. 0) RETURN
        
        !Retrieve data
        CALL GetCumGWStorChange_GivenFile(GWZBudget%ZBudgetType,ZoneList,iZoneID,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutDates,rCumStorChange,iStat)
    ELSE
        CALL SetLastMessage('Groundwater ZBudget is not part of model output to retrieve zonal cumulative storage change!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    
  END SUBROUTINE GetCumGWStorChange_GivenGWZBudget
  
  
  ! -------------------------------------------------------------
  ! --- GET CUMULATIVE CHANGE IN STORAGE FOR A ZONE FROM ZBUDGET OUTPUT 
  ! -------------------------------------------------------------
  SUBROUTINE GetCumGWStorChange_GivenFile(ZBudget,ZoneList,iZoneID,cBeginDate,cEndDate,cOutputInterval,rFactVL,rOutDates,rCumStorChange,iStat)
    TYPE(ZBudgetType),INTENT(IN)             :: ZBudget      !Assumes ZBudget file is already open
    TYPE(ZoneListType),INTENT(IN)            :: ZoneList     !Assumes zone list has already been compiled
    INTEGER,INTENT(IN)                       :: iZoneID
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate,cOutputInterval
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rOutDates(:),rCumStorChange(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER,PARAMETER                 :: iColList(2) = [1,2]             
    INTEGER                           :: iDimActual,iNTimeSteps,indx,iInterval_InMinutes,iDataUnitTypes(2)
    REAL(8)                           :: rDeltaT   
    REAL(8),ALLOCATABLE               :: rValues(:,:)
    CHARACTER(LEN=f_iTimeStampLength) :: cTimeZero
    TYPE(TimeStepType)                :: TimeStep
    
    !Initialize
    iStat = 0
    
    !Get number of time steps stored in the ZBudget file
    CALL ZBudget%GetTimeStepRelatedData(iNTimeSteps,TimeStep)
    
    !Allocate array to read data
    ALLOCATE (rValues(3,iNTimeSteps))  !Add 1 to the first domension for Time column
    
    !Read gw storage inflow and outflow data
    CALL ZBudget%ReadData(ZoneList,iZoneID,iColList,cOutputInterval,cBeginDate,cEndDate,1d0,rFactVL,iDataUnitTypes,iDimActual,rValues,iStat)
    
    !Store values in return argument
    ALLOCATE (rOutDates(iDimActual+1) , rCumStorChange(iDimActual+1))
    rOutDates(2:iDimActual+1) = rValues(1,1:iDimActual)
    rCumStorChange(1)         = 0.0
    DO indx=1,iDimActual
        rCumStorChange(indx+1) = rCumStorChange(indx) + rValues(3,indx) - rValues(2,indx) 
    END DO
    
    !Calculate first date as t=0
    CALL CTimeStep_To_RTimeStep(cOutputInterval,rDeltaT,iInterval_InMinutes,iStat)  ;  IF (iStat .NE. 0) RETURN
    cTimeZero    = IncrementTimeStamp(cBeginDate,iInterval_InMinutes,-1)
    rOutDates(1) = TimeStampToJulian(cTimeZero)
    
  END SUBROUTINE GetCumGWStorChange_GivenFile
  
  
  ! -------------------------------------------------------------
  ! --- GET TIME SERIES DATA FROM ZBUDGET FILE FOR A SELECTED ZONE AND SELECTED COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE GetTSData(GWZBudget,iZoneID,iCols,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,cInterval,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(GWZBudgetType),INTENT(IN) :: GWZBudget
    INTEGER,INTENT(IN)              :: iZoneID,iCols(:),iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)     :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)              :: rFactAR,rFactVL
    REAL(8),INTENT(OUT)             :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)             :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+9) :: ThisProcedure = ModName // 'GetTSData'
    INTEGER                     :: indx,iZonesWithNames(0)
    REAL(8)                     :: rValues(SIZE(iCols)+1,SIZE(rOutputDates))
    CHARACTER(LEN=0)            :: cZoneNames(0)
    TYPE(ZoneListType)          :: ZoneList

    IF (.NOT. GWZBudget%IsOutfileDefined()) THEN
        CALL SetLastMessage('Groundwater zone budget is not part of the model output to retrieve data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
            
    !Generate zone list
    CALL ZoneList%New(GWZBudget%Header%iNData,GWZBudget%Header%lFaceFlows_Defined,GWZBudget%SystemData,iZExtent,iElems,iLayers,iZoneIDs,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read data
    CALL GWZBudget%ReadData(ZoneList,iZoneID,iCols,cInterval,cBeginDate,cEndDate,rFactAR,rFactVL,iDataTypes,inActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indx=1,inActualOutput
        rOutputDates(indx)    = rValues(1,indx)
        rOutputValues(indx,:) = rValues(2:,indx)
    END DO
    
    !Delete zone list
    CALL ZoneList%Kill()
    
  END SUBROUTINE GetTSData
  
  
  ! -------------------------------------------------------------
  ! --- GET Z-BUDGET FILENAME
  ! -------------------------------------------------------------
  SUBROUTINE GetOutFileName(ZBudget,cFileName)
    CLASS(GWZBudgetType),INTENT(IN) :: ZBudget
    CHARACTER(:),ALLOCATABLE        :: cFileName
    
    CALL ZBudget%File%GetName(cFileName)
    
  END SUBROUTINE GetOutFileName
  
  
    
  
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
  ! --- COMPILE HEADER AND SYSTEM DATA
  ! -------------------------------------------------------------
  SUBROUTINE CompileHeaderSystemData(TimeStep,lDeepPerc,lRootZone_Defined,AppGW,AppStream,AppLake,AppSWShed,StrmGWConnector,AppGrid,Stratigraphy,Header,SystemData,NModelFlowTypes,ModelFlowTypes,iStat)
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    LOGICAL,INTENT(IN)                     :: lDeepPerc,lRootZone_Defined
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppLakeType),INTENT(IN)           :: AppLake
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy
    TYPE(ZBudgetHeaderType),INTENT(OUT)    :: Header
    TYPE(SystemDataType),INTENT(OUT)       :: SystemData
    INTEGER,INTENT(OUT)                    :: NModelFlowTypes
    INTEGER,ALLOCATABLE,INTENT(INOUT)      :: ModelFlowTypes(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    INTEGER                  :: indxElem,indxFace,indxVertex,indxFlow,i
    CHARACTER(:),ALLOCATABLE :: cIWFMVersion
    
    !Initialize
    iStat = 0
    
    !Descriptor
    Header%cDescriptor = 'Groundwater zone budget'
    
    !System data
    SystemData%NNodes    = AppGrid%NNodes
    SystemData%NElements = AppGrid%NElements
    SystemData%NLayers   = Stratigraphy%NLayers
    SystemData%NFaces    = AppGrid%NFaces
    ALLOCATE (SystemData%iElementIDs(AppGrid%NElements)                   , &
              SystemData%iElementNNodes(AppGrid%NElements)                , &
              SystemData%iElementNodes(4,AppGrid%NElements)               , &
              SystemData%iFaceElems(2,AppGrid%NFaces)                     , &
              SystemData%lBoundaryFace(AppGrid%NFaces)                    , &
              SystemData%lActiveNode(AppGrid%NNodes,Stratigraphy%NLayers) , &
              SystemData%rNodeAreas(AppGrid%NNodes)                       , &
              SystemData%rElementAreas(AppGrid%NElements)                 , &
              SystemData%rElementNodeAreas(4,AppGrid%NElements)           , &
              SystemData%rElementNodeAreaFractions(4,AppGrid%NElements)   )
    SystemData%rNodeAreas     = AppGrid%AppNode%Area
    SystemData%rElementAreas  = AppGrid%AppElement%Area
    SystemData%iElementNNodes = AppGrid%NVertex
    DO indxElem=1,AppGrid%NElements
        SystemData%iElementIDs(indxElem)     = AppGrid%AppElement(indxElem)%ID
        SystemData%iElementNodes(:,indxElem) = AppGrid%Vertex(:,indxElem)
        DO indxVertex=1,AppGrid%NVertex(indxElem)
            SystemData%rElementNodeAreas(indxVertex,indxElem)         = AppGrid%AppElement(indxElem)%VertexArea(indxVertex)
            SystemData%rElementNodeAreaFractions(indxVertex,indxElem) = AppGrid%AppElement(indxElem)%VertexAreaFraction(indxVertex)
        END DO
        IF (AppGrid%NVertex(indxElem) .EQ. 3) THEN
            SystemData%rElementNodeAreas(4,indxElem)         = 0.0
            SystemData%rElementNodeAreaFractions(4,indxElem) = 0.0
        END IF
    END DO
    DO indxFace=1,AppGrid%NFaces
        SystemData%iFaceElems(:,indxFace) = AppGrid%AppFace%Element(:,indxFace)
    END DO
    SystemData%lBoundaryFace = AppGrid%AppFace%BoundaryFace
    SystemData%lActiveNode   = Stratigraphy%ActiveNode
    
    !Vertical flows are defined at nodes
    Header%lVertFlows_DefinedAtNode = .TRUE.
    
    !Face flows
    Header%lFaceFlows_Defined = .TRUE.
    
    !Storages
    Header%lStorages_Defined = .TRUE.
    
    !Computation of mass balance error
    Header%lComputeError = .TRUE.
    
    !Compile the flow types
    CALL ProcessFlowTypes(TimeStep%DeltaT,lDeepPerc,lRootZone_Defined,AppGW,AppStream,AppLake,AppSWShed,StrmGWConnector,AppGrid,Stratigraphy,Header,ModelFlowTypes,iStat)
    IF (iStat .EQ. -1) RETURN
        
    !Number of all flow types and flow names
    NModelFlowTypes = SIZE(ModelFlowTypes)
    Header%iNData   = 2 * NModelFlowTypes
    ALLOCATE (Header%iDataTypes(Header%iNData) , Header%cFullDataNames(Header%iNData) , Header%cDataHDFPaths(Header%iNData))
    Header%iDataTypes = f_iVR
    DO indxFlow=1,NModelFlowTypes
        Header%cFullDataNames(2*indxFlow-1) = TRIM(FlowNames(ModelFlowTypes(indxFlow))) // '_Inflow (+)'
        Header%cFullDataNames(2*indxFlow)   = TRIM(FlowNames(ModelFlowTypes(indxFlow))) // '_Outflow (-)'
        Header%cDataHDFPaths(2*indxFlow-1)  = TRIM(FlowNames(ModelFlowTypes(indxFlow))) // '_Inflow (+)'
        Header%cDataHDFPaths(2*indxFlow)    = TRIM(FlowNames(ModelFlowTypes(indxFlow))) // '_Outflow (-)'
    END DO
    
    !Column numbers to be used for inflow and outflows for mass balance computation
    ALLOCATE (Header%iErrorInCols(NModelFlowTypes) , Header%iErrorOutCols(NModelFlowTypes))
    Header%iErrorInCols  = [(2*i-1 , i=1,NModelFlowTypes)]
    Header%iErrorOutCols = [(2*i   , i=1,NModelFlowTypes)]
    
    !IWFM version 
    cIWFMVersion            = IWFM_Core%GetVersion()
    Header%cSoftwareVersion = 'IWFM (v' // TRIM(cIWFMVersion) // ')'
    
    !ASCII output data
    Header%ASCIIOutput%iLenTitles = 160
    Header%ASCIIOutput%iNTitles   = 3
    ALLOCATE (Header%ASCIIOutput%cColumnTitles(3))
    Header%ASCIIOutput%cColumnTitles(1) = '                '
    Header%ASCIIOutput%cColumnTitles(2) = '      Time      '
    Header%ASCIIOutput%cColumnTitles(3) = '                '
    i = 18
    DO indxFlow=1,NModelFlowTypes
        Header%ASCIIOutput%cColumnTitles(1)(i+1:i+30) = ArrangeText(TRIM(FlowNames(ModelFlowTypes(indxFlow))),30)
        Header%ASCIIOutput%cColumnTitles(2)(i+1:i+30) = '            IN             OUT'
        Header%ASCIIOutput%cColumnTitles(3)(i+1:i+30) = '           (+)             (-)'
        i                                             = i + 32
    END DO
    Header%ASCIIOutput%iLenColumnTitles = i - 2
    Header%ASCIIOutput%cNumberFormat    = '(A16,' // TRIM(IntToText(Header%iNData)) // '(2X,F14.2))'
    
    !DSS pathname F parts
    ALLOCATE (Header%cDSSFParts(Header%iNData))
    DO indxFlow=1,NModelFlowTypes
        Header%cDSSFParts(2*indxFlow-1) = TRIM(DSSFParts(ModelFlowTypes(indxFlow))) // '_IN'
        Header%cDSSFParts(2*indxFlow)   = TRIM(DSSFParts(ModelFlowTypes(indxFlow))) // '_OUT'
    END DO
    
  END SUBROUTINE CompileHeaderSystemData
  
  
  ! -------------------------------------------------------------
  ! --- FIGURE OUT WHICH FLOW PROCESSES (W.R.T. GW) ARE BEING MODELED 
  ! -------------------------------------------------------------
  SUBROUTINE ProcessFlowTypes(DeltaT,lDeepPerc,lRootZone_Defined,AppGW,AppStream,AppLake,AppSWShed,StrmGWConnector,AppGrid,Stratigraphy,Header,ModelFlowTypes,iStat)
    REAL(8),INTENT(IN)                     :: DeltaT
    LOGICAL,INTENT(IN)                     :: lDeepPerc,lRootZone_Defined
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppLakeType),INTENT(IN)           :: AppLake
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy
    TYPE(ZBudgetHeaderType)                :: Header
    INTEGER,ALLOCATABLE                    :: ModelFlowTypes(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'ProcessFlowTypes'
    INTEGER                      :: NFlowTypes,TempFlowTypes(NFlowID),ErrorCode,indxLayer, &
                                    NLayers,NElements,NNodes,NStrmNodes,NDrain,NSubIrig
    
    !Initialize
    iStat         = 0
    NFlowTypes    = 0
    TempFlowTypes = 0
    NLayers       = Stratigraphy%NLayers
    NElements     = AppGrid%NElements
    NNodes        = AppGrid%NNodes
    NStrmNodes    = AppStream%GetNStrmNodes()
    NDrain        = AppGW%GetNDrain()
    NSubIrig      = AppGW%GetNSubIrig()
    
    !Storage change
    IF (DeltaT .GT. 0.0) TempFlowTypes(StorageID) = StorageID
      
    !Flow ID for stream-gw interaction  
    IF (NStrmNodes .GT. 0) TempFlowTypes(StrmGWXID) = StrmGWXID

    !Flow ID for tile drains  
    IF (NDrain .GT. 0) TempFlowTypes(TileDrainID) = TileDrainID

    !Flow ID for subsurface irrigation  
    IF (NSubIrig .GT. 0) TempFlowTypes(SubIrigID) = SubIrigID

    !Flow ID for subsidence  
    IF (AppGW%IsSubsidenceDefined()) TempFlowTypes(SubsidenceID) = SubsidenceID

    !Flow ID for deep perc  
    IF (lDeepPerc) TempFlowTypes(DeepPercID) = DeepPercID
      
    !Flow ID for specified flow bc
    DO indxLayer=1,NLayers
      IF (AppGW%GetNNodesWithBCType(indxLayer,f_iSpFlowBCID) .GT. 0) THEN
        TempFlowTypes(FlowBCID) = FlowBCID
        EXIT
      END IF
    END DO
      
    !Flow ID for specified head bc
    DO indxLayer=1,NLayers
      IF (AppGW%GetNNodesWithBCType(indxLayer,f_iSpHeadBCID) .GT. 0) THEN
        TempFlowTypes(HeadBCID) = HeadBCID
        EXIT
      END IF
    END DO
      
    !Flow ID for general head bc
    DO indxLayer=1,NLayers
      IF (AppGW%GetNNodesWithBCType(indxLayer,f_iGHBCID) .GT. 0) THEN
        TempFlowTypes(GenHeadBCID) = GenHeadBCID
        EXIT
      END IF
    END DO
      
    !Flow ID for constrained general head bc
    DO indxLayer=1,NLayers
      IF (AppGW%GetNNodesWithBCType(indxLayer,f_iConstrainedGHBCID) .GT. 0) THEN
        TempFlowTypes(ConstGenHeadBCID) = ConstGenHeadBCID
        EXIT
      END IF
    END DO
      
    !Flow ID for baseflow from small watershed bc
    IF (AppSWShed%IsBaseFlowSimulated()) TempFlowTypes(SmallWShedBaseFlowID) = SmallWShedBaseFlowID

    !Flow ID for percolation from small watershed bc
    IF (AppSWShed%IsPercFlowSimulated()) TempFlowTypes(SmallWShedPercID) = SmallWShedPercID

    !Flow ID for recoverable loss from diversions  
    IF (AppStream%GetNDiver() .GT. 0) TempFlowTypes(DivRecoverLossID) = DivRecoverLossID
      
    !Flow ID for recoverable loss from bypasses  
    IF (AppStream%GetNBypass() .GT. 0) TempFlowTypes(BypassRecoverLossID) = BypassRecoverLossID

    !Flow ID for lake-gw interaction  
    IF (AppLake%GetNLakes() .GT. 0) TempFlowTypes(LakeGWXID) = LakeGWXID

    !Flow ID for element pumping
    IF (AppGW%GetNElemPumps() .GT. 0) TempFlowTypes(ElemPumpID) = ElemPumpID

    !Flow ID for well pumping
    IF (AppGW%GetNWells() .GT. 0) TempFlowTypes(WellPumpID) = WellPumpID
    
    !Flow ID for outflow to root zone
    IF (lRootZone_Defined) TempFlowTypes(FlowToRootZoneID) = FlowToRootZoneID

    !Number of simulated flow types
    NFlowTypes = COUNT(TempFlowTypes .GT. 0)
    
    !Compile flow id numbers simulated in the model
    ALLOCATE (ModelFlowTypes(NFlowTypes) , STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL SetLastMessage('Error in allocating memory for groundwater Z-Budget flow types',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    ModelFlowTypes = PACK(TempFlowTypes , MASK=TempFlowTypes.GT.0)
    CALL ShellSort(ModelFlowTypes)
    
    !Compile list of elements for each flow type
    CALL CompileFlowTypeElements(NFlowTypes,ModelFlowTypes,Header,iStat)
    
       
  CONTAINS
  
  
    ! ############################################
    ! --- COMPILE ELEMENTS FOR EACH FLOW TYPE 
    ! ############################################
    SUBROUTINE CompileFlowTypeElements(NFlowTypes,ModelFlowTypes,Header,iStat)
      INTEGER,INTENT(IN)      :: NFlowTypes,ModelFlowTypes(NFlowTypes)
      TYPE(ZBudgetHeaderType) :: Header
      INTEGER,INTENT(OUT)     :: iStat
      
      !Local variables
      CHARACTER(LEN=ModNameLen+23) :: ThisProcedure = ModName // 'CompileFlowTypeElements'
      INTEGER                      :: NFlowTypeElems(NLayers,NFlowID),iElemList(NElements),indxFlow,indxLayer, &
                                      FlowTypeElems(NElements,NLayers,NFlowID),indxElem,indxFlowType,indxNode, &
                                      iNode,iLayer,NElemPumps,NWells,iElem,N,indxDiver,indxSWShed,indxLake,    &
                                      indx,indxPump,indxElem1,indxLayer1,iFlowID
      REAL(8)                      :: rPumpLayerFactors(NLayers)
      INTEGER,ALLOCATABLE          :: Nodes(:),Layers(:),ElemsWithDiverRecvLoss(:),ElemsWithBypassRecvLoss(:), &
                                      LakeElems(:),iBCNodes(:),TopNodes(:),TopLayers(:),iStrmGWNodes(:)
      
      !Initialize
      iStat          = 0
      NFlowTypeElems = 0
      NElemPumps     = AppGW%GetNElemPumps()
      NWells         = AppGW%GetNWells()
      CALL AppStream%GetElemsWithRecvLoss(f_iDiverRecvLoss,ElemsWithDiverRecvLoss)
      CALL AppStream%GetElemsWithRecvLoss(f_iBypassRecvLoss,ElemsWithBypassRecvLoss)
      
      ASSOCIATE (pAppNode        => AppGrid%AppNode             , &
                 pTopActiveLayer => Stratigraphy%TopActiveLayer )
      
        !Identify top nodes and layers
        N = COUNT(pTopActiveLayer .GT. 0)
        CALL AllocArray(TopNodes,N,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
        TopNodes = PACK((/(indxNode,indxNode=1,NNodes)/) , MASK=pTopActiveLayer.GT.0)
        CALL AllocArray(TopLayers,N,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
        TopLayers = PACK(pTopActiveLayer , MASK=pTopActiveLayer.GT.0)
      
        DO indxFlowType=1,NFlowTypes
          SELECT CASE (ModelFlowTypes(indxFlowType))
            !Change in storage
            CASE (StorageID)
              DO indxLayer=1,Stratigraphy%NLayers
                  DO indxNode=1,AppGrid%NNodes
                      IF (Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                          CALL AddFlowTypeToElements_ByNode(indxNode,indxLayer,StorageID,NFlowTypeElems,FlowTypeElems)
                      END IF
                  END DO
              END DO
               
            !Stream-gw interaction
            CASE (StrmGWXID)
              CALL StrmGWConnector%GetAllGWNodes(iStrmGWNodes)
              CALL StrmGWConnector%GetAllLayers(Layers)
              DO indxNode=1,SIZE(iStrmGWNodes)
                  CALL AddFlowTypeToElements_ByNode(iStrmGWNodes(indxNode),Layers(indxNode),StrmGWXID,NFlowTypeElems,FlowTypeElems)
              END DO
              
            !Tile drains
            CASE (TileDrainID)
              CALL AppGW%GetTileDrainNodesLayers(f_iTileDrain,Nodes,Layers)
              DO indxNode=1,NDrain
                CALL AddFlowTypeToElements_ByNode(Nodes(indxNode),Layers(indxNode),TileDrainID,NFlowTypeElems,FlowTypeElems)
              END DO
              
            !Subsurface irrigation
            CASE (SubIrigID)
              CALL AppGW%GetTileDrainNodesLayers(f_iSubIrig,Nodes,Layers)
              DO indxNode=1,NSubIrig
                CALL AddFlowTypeToElements_ByNode(Nodes(indxNode),Layers(indxNode),SubIrigID,NFlowTypeElems,FlowTypeElems)
              END DO
          
            !Subsidence; all elements have this
            CASE (SubsidenceID)
              NFlowTypeElems(:,SubsidenceID) = NElements
              DO indxLayer1=1,NLayers
                  DO indxElem1=1,NElements
                      FlowTypeElems(indxElem1,indxLayer1,SubsidenceID) = indxElem1
                  END DO
              END DO
            
            !Deep perc; only top active elements have this
            CASE (DeepPercID)
              DO indxNode=1,SIZE(TopNodes)
                iNode  = TopNodes(indxNode)
                iLayer = TopLayers(indxNode)
                CALL AddFlowTypeToElements_ByNode(iNode,iLayer,DeepPercID,NFlowTypeElems,FlowTypeElems)
              END DO
              
            !Specified flow BC
            CASE (FlowBCID)
              DO indxLayer=1,NLayers
                CALL AppGW%GetNodesWithBCType(indxLayer,f_iSpFlowBCID,iBCNodes)
                DO indxNode=1,SIZE(iBCNodes)
                  CALL AddFlowTypeToElements_ByNode(iBCNodes(indxNode),indxLayer,FlowBCID,NFlowTypeElems,FlowTypeElems)
                END DO
              END DO
                 
            !Specified head BC
            CASE (HeadBCID)
              DO indxLayer=1,NLayers
                CALL AppGW%GetNodesWithBCType(indxLayer,f_iSpHeadBCID,iBCNodes)
                DO indxNode=1,SIZE(iBCNodes)
                  CALL AddFlowTypeToElements_ByNode(iBCNodes(indxNode),indxLayer,HeadBCID,NFlowTypeElems,FlowTypeElems)
                END DO
              END DO
                 
            !General head BC
            CASE (GenHeadBCID)
              DO indxLayer=1,NLayers
                CALL AppGW%GetNodesWithBCType(indxLayer,f_iGHBCID,iBCNodes)
                DO indxNode=1,SIZE(iBCNodes)
                    CALL AddFlowTypeToElements_ByNode(iBCNodes(indxNode),indxLayer,GenHeadBCID,NFlowTypeElems,FlowTypeElems)
                END DO
              END DO
              
            !Constrained general head BC
            CASE (ConstGenHeadBCID)
              DO indxLayer=1,NLayers
                CALL AppGW%GetNodesWithBCType(indxLayer,f_iConstrainedGHBCID,iBCNodes)
                DO indxNode=1,SIZE(iBCNodes)
                    CALL AddFlowTypeToElements_ByNode(iBCNodes(indxNode),indxLayer,ConstGenHeadBCID,NFlowTypeElems,FlowTypeElems)
                END DO
              END DO
              
            !Base flow from small watersheds
            CASE (SmallWShedBaseFlowID)
              DO indxLayer=1,NLayers
                  CALL AppSWShed%GetNodesWithBCType(indxLayer,SWShedBaseFlowBCID,iBCNodes)
                  DO indxNode=1,SIZE(iBCNodes)
                      CALL AddFlowTypeToElements_ByNode(iBCNodes(indxNode),indxLayer,SmallWShedBaseFlowID,NFlowTypeElems,FlowTypeElems)                      
                  END DO
              END DO
                 
            !Percolation from small watersheds
            CASE (SmallWShedPercID)
              DO indxLayer=1,NLayers
                  CALL AppSWShed%GetNodesWithBCType(indxLayer,SWShedPercFlowBCID,iBCNodes)
                  DO indxNode=1,SIZE(iBCNodes)
                      CALL AddFlowTypeToElements_ByNode(iBCNodes(indxNode),indxLayer,SmallWShedPercID,NFlowTypeElems,FlowTypeElems)                      
                  END DO
              END DO
              
            !Recoverable loss from diversions
            CASE (DivRecoverLossID)
              DO indx=1,SIZE(ElemsWithDiverRecvLoss)
                CALL AddFlowTypeToElements_ByElement(ElemsWithDiverRecvLoss(indx),0,DivRecoverLossID,NFlowTypeElems,FlowTypeElems)
              END DO
                 
            !Recoverable loss from bypasses
            CASE (BypassRecoverLossID)
              DO indx=1,SIZE(ElemsWithBypassRecvLoss)
                CALL AddFlowTypeToElements_ByElement(ElemsWithBypassRecvLoss(indx),0,BypassRecoverLossID,NFlowTypeElems,FlowTypeElems)
              END DO
                 
            !Lake-gw interaction
            CASE (LakeGWXID)
              DO indxLake=1,AppLake%GetNLakes()
                CALL AppLake%GetLakeElements(indxLake,LakeElems)
                DO indxElem=1,SIZE(LakeElems)
                  CALL AddFlowTypeToElements_ByElement(LakeElems(indxElem),0,LakeGWXID,NFlowTypeElems,FlowTypeElems)
                END DO
              END DO
            
            !Element pumping
            CASE (ElemPumpID)
              DO indxPump=1,NElemPumps
                iElem = AppGW%GetPumpElement(indxPump,f_iPump_ElemPump)
                CALL AppGW%GetLayerPumpFactors(indxPump,f_iPump_ElemPump,rPumpLayerFactors)
                DO iLayer=1,NLayers
                  IF (rPumpLayerFactors(iLayer) .GT. 0.0) &
                    CALL AddFlowTypeToElements_ByElement(iElem,iLayer,ElemPumpID,NFlowTypeElems,FlowTypeElems)
                END DO
              END DO
                
            !Well pumping
            CASE (WellPumpID)
              DO indxPump=1,NWells
                iElem = AppGW%GetPumpElement(indxPump,f_iPump_Well)
                CALL AppGW%GetLayerPumpFactors(indxPump,f_iPump_Well,rPumpLayerFactors)
                DO iLayer=1,NLayers
                  IF (rPumpLayerFactors(iLayer) .GT. 0.0) &
                    CALL AddFlowTypeToElements_ByElement(iElem,iLayer,WellPumpID,NFlowTypeElems,FlowTypeElems)
                END DO
              END DO
              
            !Outflow to root zone
            CASE (FlowToRootZoneID)
               DO indxNode=1,SIZE(TopNodes)
                iNode  = TopNodes(indxNode)
                iLayer = TopLayers(indxNode)
                CALL AddFlowTypeToElements_ByNode(iNode,iLayer,FlowToRootZoneID,NFlowTypeElems,FlowTypeElems)
              END DO
               
          END SELECT
        END DO
      
      END ASSOCIATE
      
      !Store data at persistent arrays
      ALLOCATE (Header%iElemDataColumns(NElements,2*NFlowTypes,NLayers) , Header%iNDataElems(2*NFlowTypes,NLayers))
      Header%iElemDataColumns = 0
      DO indxFlow=1,NFlowTypes
          iFlowID = ModelFlowTypes(indxFlow)
          DO indxLayer=1,NLayers
              CALL ShellSort(FlowTypeElems(1:NFlowTypeElems(indxLayer,iFlowID),indxLayer,iFlowID))
              DO indxElem=1,NFlowTypeElems(indxLayer,iFlowID)
                  iElem                                                 = FlowTypeElems(indxElem,indxLayer,iFlowID)
                  Header%iElemDataColumns(iElem,2*indxFlow-1,indxLayer) = indxElem
                  Header%iElemDataColumns(iElem,2*indxFlow,indxLayer)   = indxElem
              END DO
              Header%iNDataElems(2*indxFlow-1,indxLayer) = NFlowTypeElems(indxLayer,iFlowID)
              Header%iNDataElems(2*indxFlow,indxLayer)   = NFlowTypeElems(indxLayer,iFlowID)
          END DO
      END DO
      
      !Clear memory
      DEALLOCATE (Nodes , Layers , ElemsWithDiverRecvLoss , ElemsWithBypassRecvLoss , LakeElems , TopNodes , TopLayers , iStrmGWNodes , STAT=ErrorCode)
      
    END SUBROUTINE CompileFlowTypeElements
    
    
    ! ############################################
    ! --- ADD ELEMENTS SURROUNDING A NODE TO A FLOW TYPE
    ! ############################################
    SUBROUTINE AddFlowTypeToElements_ByNode(iNode,iLayer,FlowID,NFlowTypeElems,FlowTypeElems)
      INTEGER,INTENT(IN) :: iNode,iLayer,FlowID
      INTEGER            :: NFlowTypeElems(:,:),FlowTypeElems(:,:,:)
      
      !Local variables
      INTEGER :: indxElem,iElem,N
      
      ASSOCIATE (pSurroundingElement => AppGrid%AppNode(iNode)%SurroundingElement)
        
        DO indxElem=1,SIZE(pSurroundingElement)
            iElem = pSurroundingElement(indxElem)
            N     = NFlowTypeElems(iLayer,FlowID)
            IF (LocateInList(iElem,FlowTypeElems(1:N,iLayer,FlowID)) .GT. 0) CYCLE
            NFlowTypeElems(iLayer,FlowID)                              = NFlowTypeElems(iLayer,FlowID) + 1
            FlowTypeElems(NFlowTypeElems(iLayer,FlowID),iLayer,FlowID) = iElem
        END DO
        
      END ASSOCIATE
      
    END SUBROUTINE AddFlowTypeToElements_ByNode
    
    
    ! ############################################
    ! --- ADD A FLOW TYPE TO ELEMENT
    ! ############################################
    SUBROUTINE AddFlowTypeToElements_ByElement(iElem,iLayer,FlowID,NFlowTypeElems,FlowTypeElems)
      INTEGER,INTENT(IN) :: iElem,iLayer,FlowID
      INTEGER            :: NFlowTypeElems(:,:),FlowTypeElems(:,:,:)
      
      !Local variable
      INTEGER :: N,indxNode,iNode,iLayerLocal(4),NVertex,Vertex(4),iL
      
      !Initialize
      NVertex = AppGrid%NVertex(iElem)
      Vertex  = AppGrid%Vertex(:,iElem)
      
      !Layer numbers to be used in computations
      IF (iLayer .EQ. 0) THEN
          iLayerLocal(1:NVertex) = Stratigraphy%TopActiveLayer(Vertex(1:NVertex))
      ELSE
          iLayerLocal = iLayer
      END IF
      
      DO indxNode=1,NVertex
          iNode = Vertex(indxNode)
          iL    = iLayerLocal(indxNode)
          N     = NFlowTypeElems(iL,FlowID)
          IF (LocateInList(iElem,FlowTypeElems(1:N,iL,FlowID)) .GT. 0) CYCLE
          NFlowTypeElems(iL,FlowID)                          = NFlowTypeElems(iL,FlowID) + 1
          FlowTypeElems(NFlowTypeElems(iL,FlowID),iL,FlowID) = iElem
      END DO
              
    END SUBROUTINE AddFlowTypeToElements_ByElement
 

  END SUBROUTINE ProcessFlowTypes
  
  
  ! -------------------------------------------------------------
  ! --- IS Z-BUDGET RELATED TERMS COMPUTED? 
  ! -------------------------------------------------------------
  PURE FUNCTION IsComputed(ZBudget) RESULT(lComputed)
    CLASS(GWZBudgetType),INTENT(IN) :: ZBudget
    LOGICAL                         :: lComputed
    
    IF (ZBudget%lComputeFaceFlows .OR. ZBudget%lComputeZBudgetFlows) THEN
        lComputed = .TRUE.
    ELSE
        lComputed = .FALSE.
    END IF
    
  END FUNCTION IsComputed
  
  
  ! -------------------------------------------------------------
  ! --- IS Z-BUDGET FILE DEFINED? 
  ! -------------------------------------------------------------
  PURE FUNCTION IsOutFileDefined(ZBudget) RESULT(lDefined)
    CLASS(GWZBudgetType),INTENT(IN) :: ZBudget
    LOGICAL                         :: lDefined
    
    IF (ZBudget%File%iGetFileType() .EQ. f_iUNKNOWN) THEN
        lDefined = .FALSE.
    ELSE
        lDefined = .TRUE.
    END IF
    
  END FUNCTION IsOutFileDefined
  
  
  ! -------------------------------------------------------------
  ! --- PRINT Z-BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(GWZBudget,AppGrid,Stratigraphy,AppStream,AppGW,AppSWShed,StrmGWConnector,LakeGWConnector,QDEEPPERC,GWToRZFlows,TimeStep,FaceFlows)
    CLASS(GWZBudgetType),INTENT(IN)        :: GWZBudget
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy
    TYPE(AppStreamType),INTENT(IN)         :: AppStream
    TYPE(AppGWType),INTENT(IN)             :: AppGW
    TYPE(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(StrmGWConnectorType),INTENT(IN)   :: StrmGWConnector
    TYPE(LakeGWConnectorType),INTENT(IN)   :: LakeGWConnector
    REAL(8),INTENT(IN)                     :: QDEEPPERC(:),GWToRZFlows(:)
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    REAL(8),INTENT(OUT)                    :: FaceFlows(:,:)
    
    !Local variables
    INTEGER             :: indxLayer,indxNode,indxElem,iNode,indxFlowID,NVertex,iRHSRow,indx,I,J,iFlowID,iDataCol,      &
                           NNodes,NElements,NLayers,NEqns,indxFace,iFace,iElem,NFaces,iOffsetNode,idli,NStrmNodes,      &
                           indxVertex,Vertex(4),iOffsetElem,iLayerB,jNode,iMaxDataCol
    REAL(8)             :: rValue,AreaFrac(4),LocalFaceFlows(50),VertexArea(4),VertexAreaFrac(4),rBottomElev_I,         &
                           FaceFlowsPass(AppGrid%NFaces,1),Vx(AppGrid%NNodes,Stratigraphy%NLayers),rBottomElev_J,       &
                           Vy(AppGrid%NNodes,Stratigraphy%NLayers),Vz(AppGrid%NNodes,Stratigraphy%NLayers),             &
                           VzTemp(AppGrid%NNodes),VerticalFlow(AppGrid%NNodes,1),Storativity(AppGrid%NNodes),           &
                           ChangeInStorage(AppGrid%NNodes),GWHeads(AppGrid%NNodes,Stratigraphy%NLayers),                &
                           ElemDiverRecvLosses(AppGrid%NElements),ElemBypassRecvLosses(AppGrid%NElements),              &
                           ElemStorages(AppGrid%NElements,1),Subsidence(AppGrid%NNodes),FlowPass(AppGrid%NElements,1),  &
                           FlowCollect_IN(AppGrid%NElements,GWZBudget%NModelFlowTypes,Stratigraphy%NLayers),            &
                           FlowCollect_OUT(AppGrid%NElements,GWZBudget%NModelFlowTypes,Stratigraphy%NLayers)
    LOGICAL             :: lAddToRHS,lComputeZBudgetFlows,lSubsidence_Defined,lBoundaryNode,lBoundaryFlowNode,          &
                           lComputeNodalVelocities
    INTEGER,ALLOCATABLE :: iTileDrainNodes(:),iSubIrigNodes(:),iTileDrainLayers(:),iSubIrigLayers(:)
    REAL(8),ALLOCATABLE :: rTileDrainFlows(:),rSubIrigFlows(:)
    TYPE(RHSVectorType) :: NodeRHS(AppGrid%NNodes,Stratigraphy%NLayers)
    
    !Report progress
    CALL EchoProgress('Computing element face flows')
    
    !Initialize
    FlowCollect_IN          = 0.0
    FlowCollect_OUT         = 0.0
    FaceFlows               = 0.0
    Subsidence              = 0.0
    Vz                      = 0.0
    NNodes                  = AppGrid%NNodes
    NElements               = AppGrid%NElements
    NFaces                  = AppGrid%NFaces
    NLayers                 = Stratigraphy%NLayers
    NStrmNodes              = AppStream%GetNStrmNodes()
    lComputeZBudgetFlows    = GWZBudget%lComputeZBudgetFlows
    lSubsidence_Defined     = AppGW%IsSubsidenceDefined()
    lComputeNodalVelocities = GWZBudget%lComputeNodalVelocities
    IF (lComputeNodalVelocities) Vz = 0.0
    DO indxLayer=1,NLayers
      DO indxNode=1,NNodes
        NodeRHS(indxNode,indxLayer)%RHS = 0.0
      END DO
    END DO
    ElemDiverRecvLosses  = AppStream%GetElemRecvLosses(NElements,f_iDiverRecvLoss)
    ElemBypassRecvLosses = AppStream%GetElemRecvLosses(NElements,f_iBypassRecvLoss)
    CALL AppGW%GetTileDrainNodesLayers(f_iTileDrain,iTileDrainNodes,iTileDrainLayers)  ;  iTileDrainNodes = (iTileDrainLayers-1)*NNodes + iTileDrainNodes
    CALL AppGW%GetTileDrainFlows(f_iTileDrain,rTileDrainFlows)
    CALL AppGW%GetTileDrainNodesLayers(f_iSubIrig,iSubIrigNodes,iSubIrigLayers)  ;  iSubIrigNodes = (iSubIrigLayers-1)*NNodes + iSubIrigNodes
    CALL AppGW%GetTileDrainFlows(f_iSubIrig,rSubIrigFlows)
    CALL AppGW%GetHeads_All(.FALSE.,GWHeads)
 
    !Compute
    Layer_Loop :DO indxLayer=1,NLayers
                    iOffsetNode = (indxLayer-1) * NNodes
                    iOffsetElem = (indxLayer-1) * NElements
                    IF (NLayers .GT. 1) THEN
                        IF (indxLayer .LT. NLayers) THEN
                            CALL AppGW%GetVerticalFlowAtNodesLayer(indxLayer,NNodes,Stratigraphy,VerticalFlow(:,1))
                            IF (lComputeNodalVelocities) THEN
                                VzTemp            = VerticalFlow(:,1) / AppGrid%AppNode%Area
                                Vz(:,indxLayer)   = Vz(:,indxLayer)   + VzTemp
                                Vz(:,indxLayer+1) = Vz(:,indxLayer+1) + VzTemp
                            END IF
                        ELSE
                            VerticalFlow = 0.0
                        END IF
                    END IF
                    CALL AppGW%GetChangeInStorageAtLayer(indxLayer,NNodes,Stratigraphy,ChangeInStorage,Storativity)
                    IF (lSubsidence_Defined) CALL AppGW%GetSubsidenceAtLayer(indxLayer,Subsidence)
    FlowID_Loop:    DO indxFlowID=1,GWZBudget%NModelFlowTypes
                        iFlowID = GWZBudget%ModelFlowTypes(indxFlowID)
                        IF (GWZBudget%Header%iNDataElems(2*indxFlowID,indxLayer) .EQ. 0) CYCLE
                    
    Elem_Loop  :        DO indxElem=1,NElements
                            iDataCol = GWZBudget%Header%iElemDataColumns(indxElem,2*indxFlowID,indxLayer) !Header keeps info for both inflow and outflow components of the flow term; we pulling the element data column from outflow info
                            !Cycle if the element does not have the flow type
                            IF (iDataCol .EQ. 0) CYCLE
                        
                            !Initialize relevant variables
                            NVertex                   = AppGrid%NVertex(indxElem)
                            Vertex                    = AppGrid%Vertex(:,indxElem)
                            VertexArea(1:NVertex)     = AppGrid%AppElement(indxElem)%VertexArea
                            VertexAreaFrac(1:NVertex) = AppGrid%AppElement(indxElem)%VertexAreaFraction
                            
                            !Nodal area fractions
                            AreaFrac(1:NVertex) = VertexArea(1:NVertex) / AppGrid%AppNode(Vertex(1:NVertex))%Area
                            
                            !Process horizontal flows (this is valid for all nodes; don't compute RHS for last eqn) and vertical flows first for the RHS vector 
                            IF (indxFlowID .EQ. 1) THEN
                                DO I=1,NVertex
                                    iNode = Vertex(I)
                                    IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) CYCLE
                                    iRHSRow       = LocateInList(indxElem,AppGrid%AppNode(iNode)%ElemID_OnCCWSide)
                                    lBoundaryNode = AppGrid%AppNode(iNode)%BoundaryNode
                                    NEqns         = AppGrid%AppNode(iNode)%NFaceID
                                    rBottomElev_I = Stratigraphy%BottomElev(iNode,indxLayer)
                                    !Horizontal flows
                                    DO J=1,NVertex
                                        jNode = Vertex(J)
                                        IF (J .EQ. I) CYCLE
                                        IF (.NOT. Stratigraphy%ActiveNode(jNode,indxLayer)) CYCLE
                                        rBottomElev_J = Stratigraphy%BottomElev(jNode,indxLayer)
                                        NodeRHS(iNode,indxLayer)%RHS(iRHSRow) = NodeRHS(iNode,indxLayer)%RHS(iRHSRow) - AppGW%GetHorizontalFlow(I,J,indxElem,indxLayer,rBottomElev_I,rBottomElev_J,AppGrid)
                                        IF (lBoundaryNode) NodeRHS(iNode,indxLayer)%RHS(NEqns) = NodeRHS(iNode,indxLayer)%RHS(NEqns) - AppGW%GetRotation(I,J,indxElem,indxLayer,AppGrid)
                                    END DO
                                    !Vertical flows
                                    IF (indxLayer .LT. NLayers) THEN
                                        iLayerB = GWZBudget%ActiveLayerBelow(iNode,indxLayer)
                                        IF (iLayerB .GT. 0) THEN
                                            rValue                                = VerticalFlow(iNode,1) * AreaFrac(I)
                                            NodeRHS(iNode,iLayerB)%RHS(iRHSRow)   = NodeRHS(iNode,iLayerB)%RHS(iRHSRow) + rValue 
                                            NodeRHS(iNode,indxLayer)%RHS(iRHSRow) = NodeRHS(iNode,indxLayer)%RHS(iRHSRow) - rValue
                                        END IF
                                    END IF                    
                                END DO
                            END IF
                            
    Vertex_Loop:            DO indxVertex=1,NVertex
                                iNode     = Vertex(indxVertex)
                                IF (.NOT. Stratigraphy%ActiveNode(iNode,indxLayer)) CYCLE
                                idli      = iOffsetNode + iNode
                                rValue    = 0.0
                                lAddToRHS = .TRUE.
                                 
                                !Locate the row number for node RHS that correspond to element
                                iRHSRow = LocateInList(indxElem,AppGrid%AppNode(iNode)%ElemID_OnCCWSide)
                                
                                SELECT CASE(iFlowID)
                                    !Change in storage
                                    CASE (StorageID)
                                      rValue = -ChangeInStorage(iNode) * AreaFrac(indxVertex) 
                                    
                                    !Stream-gw interaction
                                    CASE (StrmGWXID)
                                      indx = LocateInList(idli,GWZBudget%IDR)
                                      IF (indx .GT. 0) rValue = StrmGWConnector%GetFlowAtGWNode(iNode,indxLayer) * AreaFrac(indxVertex)
                                      
                                    !Tile drains
                                    CASE (TileDrainID)
                                      indx = LocateInList(idli,iTileDrainNodes)
                                      IF (indx .GT. 0) rValue = rTileDrainFlows(indx) * AreaFrac(indxVertex)
                                       
                                    !Sub irig
                                    CASE (SubIrigID)
                                      indx = LocateInList(idli,iSubIrigNodes)
                                      IF (indx .GT. 0) rValue = rSubIrigFlows(indx) * AreaFrac(indxVertex)
                                      
                                    !Subsidence
                                    CASE (SubsidenceID)
                                      rValue= Subsidence(iNode) * VertexArea(indxVertex)            
                                      
                                    !Deep percolation
                                    CASE (DeepPercID)
                                      IF (Stratigraphy%TopActiveLayer(iNode) .EQ. indxLayer)  &
                                        rValue = QDEEPPERC(indxElem) * VertexAreaFrac(indxVertex)
                                      
                                    !Specified flow b.c.
                                    CASE (FlowBCID)
                                      CALL AppGW%GetBoundaryFlowAtElementNodeLayer(f_iSpFlowBCID,indxElem,indxVertex,indxLayer,AppGrid,rValue,lAddToRHS)
                                      
                                    !Specified head b.c.
                                    CASE (HeadBCID)
                                      CALL AppGW%GetBoundaryFlowAtElementNodeLayer(f_iSpHeadBCID,indxElem,indxVertex,indxLayer,AppGrid,rValue,lAddToRHS)
                                      
                                    !General head b.c.
                                    CASE (GenHeadBCID)
                                      CALL AppGW%GetBoundaryFlowAtElementNodeLayer(f_iGHBCID,indxElem,indxVertex,indxLayer,AppGrid,rValue,lAddToRHS)
                                    
                                    !Constrained general head b.c.
                                    CASE (ConstGenHeadBCID)
                                      CALL AppGW%GetBoundaryFlowAtElementNodeLayer(f_iConstrainedGHBCID,indxElem,indxVertex,indxLayer,AppGrid,rValue,lAddToRHS)
                                    
                                    !Small watershed baseflow b.c.
                                    CASE (SmallWShedBaseFlowID)
                                      CALL AppSWShed%GetBoundaryFlowAtElementNodeLayer(SWShedBaseFlowBCID,indxElem,indxVertex,indxLayer,AppGrid,rValue,lAddToRHS)
                                    
                                    !Small watershed percolation
                                    CASE (SmallWShedPercID)
                                      CALL AppSWShed%GetBoundaryFlowAtElementNodeLayer(SWShedPercFlowBCID,indxElem,indxVertex,indxLayer,AppGrid,rValue,lAddToRHS)
                                      
                                    !Recoverable loss from diversions
                                    CASE (DivRecoverLossID)
                                      rValue = ElemDiverRecvLosses(indxElem) * VertexAreaFrac(indxVertex) 
                                      
                                    !Recoverable loss from by-passes
                                    CASE (BypassRecoverLossID)
                                      rValue = ElemBypassRecvLosses(indxElem) * VertexAreaFrac(indxVertex) 
                                      
                                    !Lake-aquifer interaction
                                    CASE (LakeGWXID)
                                      rValue = LakeGWConnector%GetFlowAtGWNode(indxElem,iNode,indxLayer)
                                    
                                    !Element pumping
                                    CASE (ElemPumpID)
                                      rValue = AppGW%GetActualPumpingAtElementLayerNode(indxElem,indxLayer,indxVertex,f_iPump_ElemPump) * TimeStep%DeltaT
                                    
                                    !Well pumping
                                    CASE (WellPumpID)
                                      rValue = AppGW%GetActualPumpingAtElementLayerNode(indxElem,indxLayer,indxVertex,f_iPump_Well) * TimeStep%DeltaT
                                    
                                    !Outflow to root zone
                                    CASE (FlowToRootZoneID)
                                      IF (Stratigraphy%TopActiveLayer(iNode) .EQ. indxLayer)  &
                                        rValue = -GWToRZFlows(indxElem) * VertexAreaFrac(indxVertex)
                                                                          
                                END SELECT
                                
                                !Add flow terms to element flows and RHS vector for node
                                IF (lAddToRHS) NodeRHS(iNode,indxLayer)%RHS(iRHSRow) = NodeRHS(iNode,indxLayer)%RHS(iRHSRow) - rValue
                                IF (lComputeZBudgetFlows) THEN
                                    IF (rValue .NE. 0.0) CALL AddElementFlow(rValue)
                                END IF
                                
                            END DO Vertex_Loop
                            
                        END DO Elem_Loop
                        
                        !Print element inflows and outflows at the layer (face flows and vertical flows will be printed later)
                        IF (lComputeZBudgetFlows) THEN
                            iMaxDataCol               = GWZBudget%Header%iNDataElems(2*indxFlowID,indxLayer) !Header keeps info for both inflow and outflow components of the flow term; we are pulling the number of elements from outflow info
                            !Inflow
                            FlowPass(1:iMaxDataCol,1) = FlowCollect_IN(1:iMaxDataCol,indxFlowID,indxLayer)
                            CALL GWZBudget%WriteData(NLayers,f_iElemDataType,2*indxFlowID-1,indxLayer,FlowPass(1:iMaxDataCol,:))
                            !Outflow
                            FlowPass(1:iMaxDataCol,1) = FlowCollect_OUT(1:iMaxDataCol,indxFlowID,indxLayer)
                            CALL GWZBudget%WriteData(NLayers,f_iElemDataType,2*indxFlowID,indxLayer,FlowPass(1:iMaxDataCol,:))     
                        END IF
                        
                    END DO FlowID_Loop
    
                    !Print out vertical flows
                    IF (lComputeZBudgetFlows) THEN
                        IF (indxLayer .LT. NLayers) CALL GWZBudget%WriteData(NLayers,f_iVerticalFlowType,0,indxLayer,VerticalFlow)
                    END IF
                    
                END DO Layer_Loop
    
    !Print out storages
    IF (lComputeZBudgetFlows) THEN       
        !Storages
        DO indxLayer=1,NLayers
            CALL AppGW%GetElementStorageAtLayer(indxLayer,AppGrid,Stratigraphy,ElemStorages(:,1))
            CALL GWZBudget%WriteData(NLayers,f_iStorageType,0,indxLayer,ElemStorages)        
        END DO
    END IF
    
    !Now compute face flows around each node and print them
    DO indxLayer=1,NLayers
        DO indxNode=1,NNodes
            NEqns = AppGrid%AppNode(indxNode)%NFaceID
            IF (AppGrid%AppNode(indxNode)%BoundaryNode) THEN
                IF (AppSWShed%GetNetBCFlowWithBCType(indxNode,indxLayer,SWShedBaseFlowBCID) .EQ. 0.0) THEN
                    lBoundaryFlowNode = .FALSE.
                ELSE
                    lBoundaryFlowNode = .TRUE.
                END IF
                lBoundaryFlowNode = (lBoundaryFlowNode .OR. AppGW%IsBoundaryFlowNode(indxNode,indxLayer))           
            ELSE
                lBoundaryFlowNode = .FALSE.
            END IF
            CALL ComputeFaceFlowAtNode(NEqns,AppGrid%AppNode(indxNode)%BoundaryNode,lBoundaryFlowNode,AppGrid%AppNode(indxNode)%IrrotationalCoeff,NodeRHS(indxNode,indxLayer)%RHS,LocalFaceFlows(1:NEqns))
            
            !Compute nodal velocity
            IF (lComputeNodalVelocities)    &
               CALL ComputeVelocity(AppGrid,Stratigraphy,indxLayer,indxNode,LocalFaceFlows,GWHeads(indxNode,indxLayer),Vx(indxNode,indxLayer),Vy(indxNode,indxLayer))
            
            !Add nodal face flows to actual face flows
            DO indxFace=1,NEqns
                iFace = AppGrid%AppNode(indxNode)%FaceID(indxFace)
                IF (AppGrid%AppFace%BoundaryFace(iFace)) CYCLE
                iElem = AppGrid%AppNode(indxNode)%ElemID_OnCCWSide(indxFace)
                IF (iElem .EQ. AppGrid%AppFace%Element(1,iFace)) THEN
                    FaceFlows(iFace,indxLayer) = FaceFlows(iFace,indxLayer) + LocalFaceFlows(indxFace)
                ELSE
                    FaceFlows(iFace,indxLayer) = FaceFlows(iFace,indxLayer) - LocalFaceFlows(indxFace)
                END IF
            END DO
        END DO
        
        !Print face flows for the layer
        IF (lComputeZBudgetFlows) THEN
            FaceFlowsPass(:,1) = FaceFlows(:,indxLayer)
            CALL GWZBudget%WriteData(NLayers,f_iFaceFlowType,0,indxLayer,FaceFlowsPass)
        END IF
    END DO
    
    !Store groundwater velocities
    IF (lComputeNodalVelocities) CALL AppGW%SetVelocities(Vx,Vy,Vz)
        
        
  CONTAINS
  
  
    ! ############################################
    ! --- ADD FLOW TERM TO ELEMENT INFLOW/OUTFLOW
    ! ############################################
    SUBROUTINE AddElementFlow(rValue)
      REAL(8),INTENT(IN) :: rValue
      
      IF (rValue .GE. 0.0) THEN
          FlowCollect_IN(iDataCol,indxFlowID,indxLayer) = FlowCollect_IN(iDataCol,indxFlowID,indxLayer) + rValue
      ELSE
          FlowCollect_OUT(iDataCol,indxFlowID,indxLayer)   = FlowCollect_OUT(iDataCol,indxFlowID,indxLayer) - rValue
      END IF
      
    END SUBROUTINE AddElementFlow
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- SOLVE FOR FACE FLOWS AT A NODE 
  ! -------------------------------------------------------------
  SUBROUTINE ComputeFaceFlowAtNode(NEqns,lBoundaryNode,lBoundaryFlowNode,LastRowCoeff,RHS,FaceFlows)
    INTEGER,INTENT(IN)  :: NEqns
    LOGICAL,INTENT(IN)  :: lBoundaryNode,lBoundaryFlowNode
    REAL(8),INTENT(IN)  :: LastRowCoeff(NEqns),RHS(:)
    REAL(8),INTENT(OUT) :: FaceFlows(NEqns)
    
    !Local variables
    INTEGER,PARAMETER :: NMax = 50
    INTEGER           :: indxCol,indx_LU(NEqns)
    REAL(8)           :: CoeffMatrix(NEqns,NEqns),D,RHS_Local(1:NEqns)
    
    !Initialize
    CoeffMatrix = 0.0
    RHS_Local   = RHS(1:NEqns)
    
    !Set the CoeffMatrix
    CoeffMatrix(1,1) = 1.0
    DO indxCol=2,NEqns
      CoeffMatrix(indxCol-1,indxCol) = -1.0
      CoeffMatrix(indxCol,indxCol)   =  1.0
    END DO
    
    !Update last equation
    IF (lBoundaryNode) THEN
        IF (lBoundaryFlowNode) THEN
            CoeffMatrix(NEqns,:) = LastRowCoeff
        ELSE
            CoeffMatrix(NEqns,:)     = 0.0
            CoeffMatrix(NEqns,NEqns) = 1.0
            RHS_Local(NEqns)         = 0.0
        END IF
    ELSE
        CoeffMatrix(NEqns,:) = LastRowCoeff
        RHS_Local(NEqns)     = 0.0
    END IF
    
    !Find the lu decomposition of coeff matrix
    CALL LUDCMP(CoeffMatrix,NEqns,NEqns,indx_LU,D)

    !Compute the element face flows
    CALL LUBKSB(CoeffMatrix,NEqns,NEqns,indx_LU,RHS_Local)
    FaceFlows = RHS_Local

  END SUBROUTINE ComputeFaceFlowAtNode
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE VELOCITY
  ! -------------------------------------------------------------
  SUBROUTINE ComputeVelocity(AppGrid,Stratigraphy,iLayer,iNode0,FaceFlows,GWHead,Vx,Vy)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: iLayer,iNode0
    REAL(8),INTENT(IN)                :: FaceFlows(:),GWHead
    REAL(8),INTENT(OUT)               :: Vx,Vy
    
    !Local variables
    INTEGER :: iNode1,indxFace,iFace
    REAL(8) :: x0,y0,x1,y1,rLength,rVelocity,rDepth,Coeff
    
    !Initialize
    Vx = 0.0
    Vy = 0.0
    
    !Flow depth
    rDepth = MIN(MAX(0.0 , GWHead-Stratigraphy%BottomElev(iNode0,iLayer)) , Stratigraphy%TopElev(iNode0,iLayer)-Stratigraphy%BottomElev(iNode0,iLayer))
    
    !Return if flow depth is zero
    IF (rDepth .EQ. 0.0) RETURN
    
    !Corrdinates of the node
    x0 = AppGrid%X(iNode0)
    y0 = AppGrid%Y(iNode0)
    
    !Compute x- and y- component of velocity vector at node
    DO indxFace=1,AppGrid%AppNode(iNode0)%NFaceID
        !Face ID
        iFace = AppGrid%AppNode(iNode0)%FaceID(indxFace)
        
        !Face length
        rLength = AppGrid%AppFace%Length(iFace)
        
        !Connecting node 
        IF (AppGrid%AppFace%Node(1,iFace) .EQ. iNode0) THEN
            iNode1 = AppGrid%AppFace%Node(2,iFace)
        ELSE
            iNode1 = AppGrid%AppFace%Node(1,iFace)
        END IF
        
        !Coordinates of connecting node
        x1 = AppGrid%X(iNode1)
        y1 = AppGrid%Y(iNode1)
        
        !Magnitude of flow velocity
        rVelocity = 2D0 * FaceFlows(indxFace) / rLength / rDepth
        
        !Velocity vector coefficients
        Coeff      = rVelocity / rLength
        Vx = Vx + Coeff * (y0-y1) 
        Vy = Vy + Coeff * (x1-x0)
    END DO
           
  END SUBROUTINE ComputeVelocity


END MODULE