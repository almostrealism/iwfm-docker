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
MODULE Package_AppUnsatZone
  USE MessageLogger          , ONLY: SetLastMessage           , &
                                     EchoProgress             , &
                                     MessageArray             , &
                                     iFatal
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE IOInterface 
  USE Package_Misc           , ONLY: SolverDataType           , &
                                     iLocationType_Subregion  , &
                                     iLocationType_Zone       , &
                                     iAllLocationIDsListed
  USE Package_Discretization
  USE Package_Budget         , ONLY: BudgetType               , &
                                     BudgetHeaderType         , &
                                     VolumeUnitMarker         , &
                                     AreaUnitMarker           , &
                                     LocationNameMarker       , &
                                     AreaMarker               , &
                                     VLB                      , &
                                     VLE                      , &
                                     VR                       , &
                                     PER_CUM
  USE Package_ZBudget        , ONLY: ZBudgetType              , &
                                     ZBudgetHeaderType        , &
                                     ZoneListType             , &
                                     SystemDataType           , &
                                     iElemDataType            , &
                                     iVerticalFlowType
  USE Package_UnsatZone      , ONLY: SoilType                 , &
                                     KunsatMethodList         , &
                                     VadoseZoneMoistureRouter
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
  PUBLIC :: AppUnsatZoneType       
  
  
  ! -------------------------------------------------------------
  ! --- UNSATURATED ZONE ELEMENT DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(SoilType) :: UnsatElemType
      REAL(8) :: ThicknessMax = 0.0     !Maximum thickness of the unsat zone
      REAL(8) :: Thickness    = 0.0     !Thickness of the unsat zone at the current time step
      REAL(8) :: Thickness_P  = 0.0     !Thickness of the unsat zone at the previous time step
      REAL(8) :: SoilM        = 0.0     !Soil moisture at the current time step
      REAL(8) :: SoilM_P      = 0.0     !Soil moisture at the previous time step
      REAL(8) :: Outflow      = 0.0     !Flow that leaves an unsaturated element in a layer and goes into the next layer
      REAL(8) :: SoilM_To_GW  = 0.0     !Soil moisture (volumetric rate) that is added to GW due to GW rising 
  END TYPE UnsatElemType
  
  
  ! -------------------------------------------------------------
  ! --- UNSATURATED ZONE DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE AppUnsatZoneType
      PRIVATE
      CHARACTER(LEN=6)                  :: VarTimeUnit         = ''        !Time unit of variables used in the component
      LOGICAL                           :: lDefined            = .FALSE.   !Flag to check if unsat zone is simulated
      LOGICAL                           :: lThicknessUpdated   = .FALSE.   !Flag to check if the unsat zone thicknesses are updated based on previous time gw heads
      INTEGER                           :: NUnsatLayers        = 0         !Number of simulated unsat zone layers (below root zone)
      TYPE(SolverDataType)              :: SolverData                      !Data to check the convergence of iterative solver
      TYPE(UnsatElemType),ALLOCATABLE   :: UnsatElems(:,:)                 !Unsaturated soil data for each element for (layer,element) combination
      REAL(8),ALLOCATABLE               :: DeepPerc(:)                     !Deep percolation (volumetric rate) at each (element)
      REAL(8),ALLOCATABLE               :: RegionalStorage(:)              !Regional storage at current time step
      REAL(8),ALLOCATABLE               :: RegionalStorage_P(:)            !Regional storage at the previous time step
      TYPE(BudgetType),ALLOCATABLE      :: BudRawFile                      !Budget HDF5 output file
      TYPE(ZBudgetType),ALLOCATABLE     :: ZBudgetRawFile                  !Z-Budget HDF5 output file
      TYPE(GenericFileType),ALLOCATABLE :: FinSimResultsFile               !File to print final simulation results
  CONTAINS
      PROCEDURE,PASS :: New              
      PROCEDURE,PASS :: Kill  
      PROCEDURE,PASS :: GetNLayers
      PROCEDURE,PASS :: GetNDataList_AtLocationType
      PROCEDURE,PASS :: GetDataList_AtLocationType
      PROCEDURE,PASS :: GetLocationsWithData
      PROCEDURE,PASS :: GetSubDataList_AtLocation
      PROCEDURE,PASS :: GetModelData_AtLocation
      PROCEDURE,PASS :: GetDeepPerc   
      PROCEDURE,PASS :: IsDefined        
      PROCEDURE,PASS :: AdvanceState     
      PROCEDURE,PASS :: ConvertTimeUnit 
      PROCEDURE,PASS :: ReadRestartData
      PROCEDURE,PASS :: PrintResults 
      PROCEDURE,PASS :: PrintRestartData
      PROCEDURE,PASS :: Simulate         
      PROCEDURE,PASS :: UpdateStorage
  END TYPE AppUnsatZoneType
      

  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  CHARACTER(LEN=23),PARAMETER :: cDataList_AtSubregion = 'Unsaturated zone budget'
  CHARACTER(LEN=28),PARAMETER :: cDataList_AtZone = 'Unsaturated zone zone budget'

  
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: NBudColumns  = 5  , &
                                 NZBudColumns = 4
  CHARACTER(LEN=24),PARAMETER :: cBudgetColumnTitles(NBudColumns)   = ['Beginning Storage (+)'      , &
                                                                       'Ending Storage (-)'         , &
                                                                       'Percolation (+)'            , &
                                                                       'Deep Percolation (-)'       , &
                                                                       'Discrepancy (=)'            ]
  CHARACTER(LEN=24),PARAMETER :: cZBudgetColumnTitles(NZBudColumns) = ['Beginning Storage (+)'      , &
                                                                       'Ending Storage (-)'         , &
                                                                       'Percolation (+)'            , &
                                                                       'Deep Percolation (-)'       ]

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen  = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'Package_AppUnsatZone::'


  
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
  ! --- INSTANTIATE AppUnsatZone OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE New(AppUnsatZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,cIWFMVersion,DepthToGW,iStat) 
    CLASS(AppUnsatZoneType),INTENT(OUT) :: AppUnsatZone
    LOGICAL,INTENT(IN)                  :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)   :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)       :: TimeStep
    INTEGER,INTENT(IN)                  :: NTIME
    CHARACTER(LEN=*),INTENT(IN)         :: cIWFMVersion
    REAL(8),INTENT(IN)                  :: DepthToGW(:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: NUZLayers,ErrorCode,NElements,NSubregions
    TYPE(GenericFileType)       :: UnsatZoneFile
    CHARACTER                   :: ALine*2000,cErrorMsg*500
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    TYPE(BudgetHeaderType)      :: BudHeader
    
    !Initialize
    iStat = 0
    
    !Return if cFileName is empty
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('Instantiating unsaturated zone component...')
    
    !Initialize
    NElements   = AppGrid%NElements
    NSubregions = AppGrid%NSubregions
    
    !Open unsat zone parameter file
    CALL UnsatZoneFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='unsaturated zone main parameter',iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read away the version number
    CALL UnsatZoneFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Number of unsat zone layers
    CALL UnsatZoneFile%ReadData(NUZLayers,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NUZLayers .EQ. 0) THEN
        CALL UnsatZoneFile%Kill()
        RETURN
    END IF
    AppUnsatZone%NUnsatLayers = NUZLayers
    
    !Iterative solver data
    CALL UnsatZoneFile%ReadData(AppUnsatZone%SolverData%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL UnsatZoneFile%ReadData(AppUnsatZone%SolverData%IterMax,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !HDF5 budget file
    CALL UnsatZoneFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (AppUnsatZone%BudRawFile)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL AppUnsatZone%BudRawFile%New(cAbsPathFileName,iStat)  
            IF (iStat .EQ. -1) RETURN
        ELSE
            BudHeader = PrepareBudgetHeader(AppGrid,NTIME,TimeStep,cIWFMVersion)
            CALL AppUnsatZone%BudRawFile%New(cAbsPathFileName,BudHeader,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL BudHeader%Kill()
        END IF
    END IF
    
    !HDF5 Z-Budget file
    CALL UnsatZoneFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (AppUnsatZone%ZBudgetRawFile)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL UZZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTIME,NUZLayers,cIWFMVersion,AppGrid,AppUnsatZone%ZBudgetRawFile,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Final simulation results output file
    CALL UnsatZoneFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (AppUnsatZone%FinSimResultsFile)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL AppUnsatZone%FinSimResultsFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,Descriptor='final unsaturated zone moisture output',iStat=iStat)  
        ELSE
            CALL AppUnsatZone%FinSimResultsFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,Descriptor='final unsaturated zone moisture output',iStat=iStat)  
        END IF
        IF (iStat .EQ. -1) RETURN
        IF (AppUnsatZone%FinSimResultsFile%iGetFileType() .NE. TXT) THEN
            CALL SetLastMessage('End-of-simulation unsaturated zone moisture output file must be a text file!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Allocate memory for array attributes
    ALLOCATE (AppUnsatZone%UnsatElems(NUZLayers,NElements) ,  &
              AppUnsatZone%DeepPerc(NElements)             ,  &
              AppUnsatZone%RegionalStorage(NSubregions+1)  ,  &
              AppUnsatZone%RegionalStorage_P(NSubregions+1),  &
              STAT=ErrorCode ,ERRMSG=cErrorMsg             )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for unsaturated zone component!'//NEW_LINE('x')//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read unsat zone parameters
    CALL ReadUnsatZoneParameters(NUZLayers,AppGrid,Stratigraphy,TimeStep,UnsatZoneFile,AppUnsatZone%VarTimeUnit,AppUnsatZone%UnsatElems,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compute dynamic unsat zone thickness based on the location of gw head and set the flag
    CALL ComputeLayerThickness(AppGrid,DepthToGW,AppUnsatZone%UnsatElems)
    AppUnsatZone%UnsatElems%Thickness_P = AppUnsatZone%UnsatElems%Thickness
    AppUnsatZone%lThicknessUpdated      = .TRUE.
    AppUnsatZone%UnsatElems%SoilM_To_GW = 0.0  !Over-write the soil moisture contribution to rising groundwater for the initial case
    
    !Read initial conditions
    CALL ReadInitialConditions(UnsatZoneFile,NUZLayers,AppUnsatZone%UnsatElems,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Calculate initial regional storage
    AppUnsatZone%RegionalStorage   = RegionalStorage(AppGrid,AppUnsatZone%UnsatElems)
    AppUnsatZone%RegionalStorage_P = AppUnsatZone%RegionalStorage
    
    !If made it this far, set the flag
    AppUnsatZone%lDefined = .TRUE.
    
    !Close unsat zone parameter file
    CALL UnsatZoneFile%Kill()
    
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- NEW HDF5 UNSATURATED ZONE ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE UZZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NUZLayers,cIWFMVersion,AppGrid,ZBudFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NUZLayers
    CHARACTER(LEN=*),INTENT(IN)   :: cIWFMVersion
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ZBudgetType)             :: ZBudFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20),PARAMETER :: ThisProcedure = ModName // 'UZZoneBudRawFile_New'
    INTEGER                                :: indxElem,indxVertex,indxLayer,ErrorCode,indxFace
    TYPE(TimeStepType)                     :: TimeStepLocal
    TYPE(ZBudgetHeaderType)                :: Header
    TYPE(SystemDataType)                   :: SystemData
    
    !Initialize
    iStat = 0
    
    !If this is for inquiry, open file for reading and return
    IF (IsForInquiry) THEN
        IF (cFileName .NE. '') CALL ZBudFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Time step received shows the timestamp at t=0; advance time to show that Z-Budget output is at t = 1
    TimeStepLocal                    = TimeStep
    TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes,1)
    TimeStepLocal%CurrentTimeStep    = 1
    
    !Compile system data
    SystemData%NNodes    = AppGrid%NNodes
    SystemData%NElements = AppGrid%NElements
    SystemData%NLayers   = NUZLayers
    SystemData%NFaces    = AppGrid%NFaces
    ALLOCATE (SystemData%iElementNNodes(AppGrid%NElements)                , &
              SystemData%iElementNodes(4,AppGrid%NElements)               , &
              SystemData%iFaceElems(2,AppGrid%NFaces)                     , &
              SystemData%lBoundaryFace(AppGrid%NFaces)                    , &
              SystemData%lActiveNode(AppGrid%NNodes,1)                    , &
              SystemData%rNodeAreas(AppGrid%NNodes)                       , &
              SystemData%rElementAreas(AppGrid%NElements)                 , &
              SystemData%rElementNodeAreas(4,AppGrid%NElements)           , &
              SystemData%rElementNodeAreaFractions(4,AppGrid%NElements)   )
    SystemData%rNodeAreas     = AppGrid%AppNode%Area
    SystemData%rElementAreas  = AppGrid%AppElement%Area
    SystemData%iElementNNodes = AppGrid%Element%NVertex
    DO indxElem=1,AppGrid%NElements
        SystemData%iElementNodes(:,indxElem) = AppGrid%Element(indxElem)%Vertex
        DO indxVertex=1,AppGrid%Element(indxElem)%NVertex
            SystemData%rElementNodeAreas(indxVertex,indxElem)         = AppGrid%AppElement(indxElem)%VertexArea(indxVertex)
            SystemData%rElementNodeAreaFractions(indxVertex,indxElem) = AppGrid%AppElement(indxElem)%VertexAreaFraction(indxVertex)
        END DO
        IF (AppGrid%Element(indxElem)%NVertex .EQ. 3) THEN
            SystemData%rElementNodeAreas(4,indxElem)         = 0.0
            SystemData%rElementNodeAreaFractions(4,indxElem) = 0.0
        END IF
    END DO
    DO indxFace=1,AppGrid%NFaces
        SystemData%iFaceElems(:,indxFace) = AppGrid%AppFace(indxFace)%Element
    END DO
    SystemData%lBoundaryFace = AppGrid%AppFace%BoundaryFace
    SystemData%lActiveNode   = .TRUE.
    
    !Compile Header data
    Header%cSoftwareVersion         = 'IWFM UNSATURATED ZONE PACKAGE (' // TRIM(cIWFMVersion) // ')'
    Header%cDescriptor              = 'Unsaturated zone zone budget'
    Header%lVertFlows_DefinedAtNode = .FALSE.
    Header%lFaceFlows_Defined       = .FALSE.
    Header%lStorages_Defined        = .FALSE.
    Header%lComputeError            = .TRUE.
    Header%iNData                   = NZBudColumns
    ALLOCATE (Header%iDataTypes(NZBudColumns)                                   , &
              Header%cFullDataNames(NZBudColumns)                               , &
              Header%cDataHDFPaths(NZBudColumns)                                , &
              Header%iNDataElems(NZBudColumns,NUZLayers)                        , &
              Header%iElemDataColumns(AppGrid%NElements,NZBudColumns,NUZLayers) , &
              Header%iErrorInCols(NZBudColumns/2)                               , &  
              Header%iErrorOutCols(NZBudColumns/2)                              , &  
              Header%cDSSFParts(NZBudColumns)                                   , &
              Header%ASCIIOutput%cColumnTitles(3)                               , &
              STAT = ErrorCode                                                  )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for unsaturated zone Z-Budget file!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    Header%iDataTypes = [VLB,&  !Beginning storage
                         VLE,&  !Ending storage
                         VR ,&  !Deep perc
                         VR ]   !Net deep perc
    Header%cFullDataNames     = cZBudgetColumnTitles
    Header%cDataHDFPaths      = cZBudgetColumnTitles
    
    !Number of elements associated for each z-budget column in each layer
    Header%iNDataElems(1:2,:) = AppGrid%NElements
    Header%iNDataElems(3,1)   = AppGrid%NElements
    Header%iNDataElems(4,:)   = AppGrid%NElements
    IF (NUZLayers .GT. 1) THEN
        Header%iNDataElems(3,2:) = 0
    END IF        
    
    !Data column number for each element in each z-budget column in each layer
    DO indxElem=1,AppGrid%NElements
        Header%iElemDataColumns(indxElem,1:2,:) = indxElem
        Header%iElemDataColumns(indxElem,3,1)   = indxElem
        Header%iElemDataColumns(indxElem,4,:)   = indxElem
    END DO
    DO indxLayer=2,NUZLayers
        DO indxElem=1,AppGrid%NElements
            Header%iElemDataColumns(indxElem,3,indxLayer) = 0
        END DO
    END DO
    
    !Error in and out columns
    Header%iErrorInCols  = [1,3]
    Header%iErrorOutCols = [2,4]
    
    !ASCII output titles
    Header%ASCIIOutput%iLenTitles       = 90
    Header%ASCIIOutput%iNTitles         = 3
    Header%ASCIIOutput%iLenColumnTitles = 79
    Header%ASCIIOutput%cColumnTitles(1) = '                      Beginning         Ending                         Deep    '
    Header%ASCIIOutput%cColumnTitles(2) = '      Time             Storage          Storage     Percolation     Percolation'
    Header%ASCIIOutput%cColumnTitles(3) = '                         (+)              (-)           (+)             (-)    '
    Header%ASCIIOutput%cNumberFormat    = '(A16,1X,100(F14.2,2X))'
    
    !DSS pathnames
    Header%cDSSFParts = ['BEGIN_STORAGE'      ,& 
                         'END_STORAGE'        ,& 
                         'PERC'               ,&                                                           
                         'DEEP_PERC'          ]
                             
    !Instantiate Z-Budget file
    CALL ZBudFile%New(cFileName,NTIME,TimeStepLocal,Header,SystemData,iStat)
    
  END SUBROUTINE UZZoneBudRawFile_New

  
  
  
  
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
  ! --- KILL UNSAT ZONE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppUnsatZone)
    CLASS(AppUnsatZoneType) :: AppUnsatZone
    
    !Local variables
    INTEGER                :: ErrorCode
    TYPE(AppUnsatZoneType) :: Dummy
    
    IF (.NOT. AppUnsatZone%lDefined) RETURN
    
    IF (ALLOCATED(AppUnsatZone%BudRawFile))        CALL AppUnsatZone%BudRawFile%Kill()
    IF (ALLOCATED(AppUnsatZone%ZBudgetRawFile))    CALL AppUnsatZone%ZBudgetRawFile%Kill()
    IF (ALLOCATED(AppUnsatZone%FinSimResultsFile)) CALL AppUnsatZone%FinSimResultsFile%Kill()
    
    DEALLOCATE (AppUnsatZone%UnsatElems         ,  &
                AppUnsatZone%DeepPerc           ,  &
                AppUnsatZone%RegionalStorage    ,  &
                AppUnsatZone%RegionalStorage_P  ,  &
                AppUnsatZone%BudRawFile         ,  &
                AppUnsatZone%ZBudgetRawFile     ,  &
                AppUnsatZone%FinSimResultsFile  ,  &
                STAT=ErrorCode                  )
    
    SELECT TYPE (AppUnsatZone)
        TYPE IS (AppUnsatZoneType)
            AppUnsatZone = Dummy
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
  ! --- GET NUMBER OF LAYERS
  ! -------------------------------------------------------------
  FUNCTION GetNLayers(AppUnsatZone) RESULT(NLayers)
    CLASS(AppUnsatZoneType),INTENT(IN) :: AppUnsatZone
    INTEGER                            :: NLayers
    
    NLayers = AppUnsatZone%NUnsatLayers
    
  END FUNCTION GetNLayers
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(AppUnsatZone,iLocationType) RESULT(NData) 
    CLASS(AppUnsatZoneType),INTENT(IN) :: AppUnsatZone
    INTEGER,INTENT(IN)                 :: iLocationType
    INTEGER                            :: NData
    
    !Initialize
    NData = 0
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_Subregion)
            !Is unsaturated zone budget defined?
            IF (ALLOCATED(AppUnsatZone%BudRawFile)) NData = 1
            
        CASE (iLocationType_Zone)
            !Is unsaturated zone zone budget defined?
            IF (ALLOCATED(AppUnsatZone%ZBudgetRawFile)) NData = NData + 1

        END SELECT
    
  END FUNCTION GetNDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType(AppUnsatZone,iLocationType,cDataList,cFileList,lBudgetType) 
    CLASS(AppUnsatZoneType),INTENT(IN) :: AppUnsatZone
    INTEGER,INTENT(IN)                 :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE       :: cDataList(:),cFileList(:)
    LOGICAL,ALLOCATABLE                :: lBudgetType(:)
    
    !Local variables
    INTEGER                  :: ErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (cDataList , cFileList , lBudgetType , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_Subregion)
            !Is unsaturated zone budget defined?
            IF (ALLOCATED(AppUnsatZone%BudRawFile)) THEN
                ALLOCATE (cDataList(1) , cFileList(1) , lBudgetType(1))
                cDataList   = cDataList_AtSubregion
                lBudgetType = .TRUE.
                CALL AppUnsatZone%BudRawFile%GetFileName(cFileName)
                cFileList = ''
                cFileList = cFileName
            END IF
            
        CASE (iLocationType_Zone)
            !Is unsaturated zone zone budget defined?
            IF (ALLOCATED(AppUnsatZone%ZBudgetRawFile)) THEN
                ALLOCATE (cDataList(1) , cFileList(1) , lBudgetType(1))
                cDataList   = cDataList_AtZone
                lBudgetType = .TRUE.
                CALL AppUnsatZone%ZBudgetRawFile%GetFileName(cFileName)
                cFileList = ''
                cFileList = cFileName
            END IF

    END SELECT
    
  END SUBROUTINE GetDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET THE LIST OF LOCATIONS THAT HAVE A DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetLocationsWithData(AppUnsatZone,iLocationType,cDataType,iLocations)
     CLASS(AppUnsatZoneType),INTENT(IN) :: AppUnsatZone
     INTEGER,INTENT(IN)                 :: iLocationType
     CHARACTER(LEN=*),INTENT(IN)        :: cDataType    
     INTEGER,ALLOCATABLE,INTENT(OUT)    :: iLocations(:)
     
     SELECT CASE (iLocationType)
         CASE (iLocationType_Subregion)
             IF (ALLOCATED(AppUnsatZone%BudRawFile)) THEN
                 IF (TRIM(cDataType) .EQ. cDataList_AtSubregion) THEN
                     ALLOCATE (iLocations(1))
                     iLocations = iAllLocationIDsListed
                 END IF
             END IF

        CASE (iLocationType_Zone)
             IF (ALLOCATED(AppUnsatZone%ZBudgetRawFile)) THEN
                 IF (TRIM(cDataType) .EQ. cDataList_AtZone) THEN
                     ALLOCATE (iLocations(1))
                     iLocations = iAllLocationIDsListed
                 END IF
             END IF
     END SELECT
     
  END SUBROUTINE GetLocationsWithData
  
  
  ! -------------------------------------------------------------
  ! --- GET SUB-COMPONENTS OF A DATA TYPE FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_AtLocation(AppUnsatZone,iLocationType,cDataType,cSubDataList)
    CLASS(AppUnsatZoneType),INTENT(IN)       :: AppUnsatZone
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cSubDataList , STAT=ErrorCode)
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_Subregion) 
            IF (TRIM(cDataType) .EQ. cDataList_AtSubregion) THEN
                IF (ALLOCATED(AppUnsatZone%BudRawFile)) THEN
                    ALLOCATE (cSubDataList(NBudColumns))
                    cSubDataList = cBudgetColumnTitles
                END IF
            END IF

        CASE (iLocationType_Zone) 
            IF (TRIM(cDataType) .EQ. cDataList_AtZone) THEN
                IF (ALLOCATED(AppUnsatZone%ZBudgetRawFile)) THEN
                    ALLOCATE (cSubDataList(NZBudColumns))
                    cSubDataList = cZBudgetColumnTitles
                END IF
            END IF
    END SELECT
    
  END SUBROUTINE GetSubDataList_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation(AppUnsatZone,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(AppUnsatZoneType)     :: AppUnsatZone
    INTEGER,INTENT(IN)          :: iZExtent,iElems(:),iLayers(:),iZones(:),iZonesWithNames(:),iLocationType,iLocationID,iCol
    CHARACTER(LEN=*),INTENT(IN) :: cZoneNames(:),cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)          :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)         :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER            :: iReadCols(1),iDataUnitTypeArray(1)
    REAL(8)            :: rValues(2,SIZE(rOutputDates))
    TYPE(ZoneListType) :: ZoneList
    
    !Initialize
    iStat         = 0
    nActualOutput = 0
    
    SELECT CASE (iLocationType)
        CASE (iLocationType_Subregion)
            IF (TRIM(cDataType) .EQ. cDataList_AtSubregion) THEN
                IF (ALLOCATED(AppUnsatZone%BudRawFile)) THEN
                    CALL AppUnsatZone%BudRawFile%ReadData(iLocationID,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
                END IF
            END IF

        CASE (iLocationType_Zone)
            IF (TRIM(cDataType) .EQ. cDataList_AtZone) THEN
                IF (ALLOCATED(AppUnsatZone%ZBudgetRawFile)) THEN
                    iReadCols = iCol
                    !Generate zone list
                    CALL ZoneList%New(AppUnsatZone%ZBudgetRawFile%Header%iNData,AppUnsatZone%ZBudgetRawFile%Header%lFaceFlows_Defined,AppUnsatZone%ZBudgetRawFile%SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Read data
                    CALL AppUnsatZone%ZBudgetRawFile%ReadData(ZoneList,iLocationID,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_AR,rFact_VL,iDataUnitTypeArray,nActualOutput,rValues,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    !Populate return variables
                    rOutputDates(1:nActualOutput)  = rValues(1,1:nActualOutput)
                    rOutputValues(1:nActualOutput) = rValues(2,1:nActualOutput)
                    iDataUnitType                  = iDataUnitTypeArray(1)
                END IF
            END IF
    END SELECT
    
    !Only unsaturated zone budget data at subregional level can be returned
    IF (.NOT. (iLocationType.EQ.iLocationType_Subregion  .AND.  TRIM(cDataType).EQ.cDataList_AtSubregion)) RETURN
    
    !If unsaturated zone budget output is not defined return
    IF (.NOT. ALLOCATED(AppUnsatZone%BudRawFile)) RETURN
    
    !Get the data for the specified budget column for the specified period with specified interval
    CALL AppUnsatZone%BudRawFile%ReadData(iLocationID,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,1d0,0d0,0d0,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    
  END SUBROUTINE GetModelData_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET DEEP PERC
  ! -------------------------------------------------------------
  PURE FUNCTION GetDeepPerc(AppUnsatZone,NElements) RESULT(DeepPerc)
    CLASS(AppUnsatZoneType),INTENT(IN) :: AppUnsatZone
    INTEGER,INTENT(IN)                 :: NElements
    REAL(8)                            :: DeepPerc(NElements)
    
    IF (AppUnsatZone%lDefined) THEN
        DeepPerc = AppUnsatZone%DeepPerc
    ELSE
        DeepPerc = 0.0
    END IF
    
  END FUNCTION GetDeepPerc
  
  
  
  
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
  ! --- IS UNSAT ZONE SIMULATED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AppUnsatZone) RESULT(lDefined)
    CLASS(AppUnsatZoneType),INTENT(IN) :: AppUnsatZone
    LOGICAL                            :: lDefined
    
    lDefined = AppUnsatZone%lDefined
    
  END FUNCTION IsDefined
  
  
  
  
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
  ! --- READ RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRestartData(AppUnsatZone,InFile,iStat)
    CLASS(AppUnsatZoneType) :: AppUnsatZone
    TYPE(GenericFileType)   :: InFile
    INTEGER,INTENT(OUT)     :: iStat
    
    CALL InFile%ReadData(AppUnsatZone%UnsatElems%Thickness,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppUnsatZone%UnsatElems%Thickness_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppUnsatZone%UnsatElems%SoilM,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppUnsatZone%UnsatElems%SoilM_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppUnsatZone%RegionalStorage,iStat)         ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppUnsatZone%RegionalStorage_P,iStat)  
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ UNSAT ZONE INITIAL MOISTURE CONDITIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialConditions(InFile,NUZLayers,UnsatElems,iStat) 
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(IN)    :: NUZLayers
    TYPE(UnsatElemType)   :: UnsatElems(:,:)
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'ReadInitialConditions'
    INTEGER                      :: indxElem,ID,indxLayer
    REAL(8)                      :: rDummyArray(1+NUZLayers)
    
    !Read for first element
    CALL InFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    UnsatElems(:,1)%SoilM   = rDummyArray(2:)
    UnsatElems(:,1)%SoilM_P = rDummyArray(2:)
    
    !Make sure data is entered for the first element
    ID  = INT(rDummyArray(1))
    IF (ID.NE.0   .AND.  ID.NE.1) THEN
        MessageArray(1) = 'Initial conditions for unsaturated zone must be entered sequentially for each element!'
        MessageArray(2) = 'Expected element ID = '//TRIM(IntToText(1))
        MessageArray(3) = 'Entered element ID  = '//TRIM(IntToText(ID))
        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !If same values are to be used for all elements, process and exit
    IF (ID .EQ. 0) THEN
        DO indxElem=2,SIZE(UnsatElems,DIM=2)
           UnsatElems(:,indxElem)%SoilM   = rDummyArray(2:)
           UnsatElems(:,indxElem)%SoilM_P = rDummyArray(2:)
        END DO
    
    !Otherwise read the rest of the initial conditions data
    ELSE
        DO indxElem=2,SIZE(UnsatElems,DIM=2)
            CALL InFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
            !Make sure data is entered sequentially
            ID  = INT(rDummyArray(1))
            IF (ID .NE. indxElem) THEN
                MessageArray(1) = 'Initial conditions for unsaturated zone must be entered sequentially for each element!'
                MessageArray(2) = 'Expected element ID = '//TRIM(IntToText(indxElem))
                MessageArray(3) = 'Entered element ID  = '//TRIM(IntToText(ID))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        
            !Assign initial conditions
            UnsatElems(:,indxElem)%SoilM   = rDummyArray(2:)
            UnsatElems(:,indxElem)%SoilM_P = rDummyArray(2:)        
        END DO
    END IF

    !Make sure initial moisture is not larger than total porosity
    DO indxElem=1,SIZE(UnsatElems,DIM=2)
        DO indxLayer=1,SIZE(UnsatElems,DIM=1)
            IF (UnsatElems(indxLayer,indxElem)%SoilM .GT. UnsatElems(indxLayer,indxElem)%TotalPorosity) THEN
                MessageArray(1) = 'Initial unsaturated zone moisture content at element '//TRIM(IntToText(indxElem))// ' and layer '//TRIM(IntToText(indxLayer))
                MessageArray(2) = ' is larger than the total porosity!'
                WRITE(MessageArray(3),'(A27,F8.6)') 'Total porosity           = ',UnsatElems(indxLayer,indxElem)%TotalPorosity
                WRITE(MessageArray(4),'(A27,F8.6)') 'Initial moisture content = ',UnsatElems(indxLayer,indxElem)%SoilM
                CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END DO
    
  END SUBROUTINE ReadInitialConditions
  
  
  ! -------------------------------------------------------------
  ! --- READ UNSAT ZONE PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE ReadUnsatZoneParameters(NUZLayers,AppGrid,Stratigraphy,TimeStep,InFile,VarTimeUnit,UnsatElems,iStat)
    INTEGER,INTENT(IN)                :: NUZLayers
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyTYpe),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    TYPE(GenericFileType)             :: InFile
    CHARACTER(LEN=*),INTENT(OUT)      :: VarTimeUnit           
    TYPE(UnsatElemType),INTENT(OUT)   :: UnsatElems(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+23) :: ThisProcedure = ModName // 'ReadUnsatZoneParameters'
    INTEGER                      :: NGroup,NElements,ID,indxElem,indxLayer,indxNode,iLayer
    REAL(8)                      :: rFactors(3),rDummyArray(1+5*NUZLayers),rDummy3DArray(AppGrid%NElements,NUZLayers,5),  &
                                    ElemDepthToBottom(AppGrid%NElements),NodeDepthToBottom(AppGrid%NNodes)
    CHARACTER                    :: ALine*500
    
    !Initialize
    NElements = AppGrid%NElements
    
    !Read number of parameteric grids
    CALL InFile%ReadData(NGroup,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Conversion factors
    CALL InFile%ReadData(rFactors,iStat)  ;  IF (iStat .EQ. -1) RETURN
    !rFactors(1): for x-y coordinates
    !rFactors(2): for layer maximum thickness
    !rFactors(3): for saturated hydraulic conductivity

    !Time unit
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;   ALine = StripTextUntilCharacter(ALine,'/')
    VarTimeUnit = ADJUSTL(TRIM(ALine))
    
    !Make sure time units are valid if time tracking simulation
    IF (TimeStep%TrackTime) THEN
        IF (IsTimeIntervalValid(VarTimeUnit) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for unsaturated zone hydraulic conductivity is not valid!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF

    !Non-parametric data input
    IF (NGroup .EQ. 0) THEN
        DO indxElem=1,NElements
            CALL InFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
            ID = INT(rDummyArray(1))
            IF (ID .NE. indxElem) THEN 
                MessageArray(1) = 'Unsaturated zone parameters should be entered sequentially for each element.'
                MessageArray(2) = 'Expected element ID = '//TRIM(IntToText(indxElem))
                MessageArray(3) = 'Entered element ID  = '//TRIM(IntToText(ID))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            UnsatElems(:,indxElem)%ThicknessMax  = rDummyArray(2::5) * rFactors(2)
            UnsatElems(:,indxElem)%TotalPorosity = rDummyArray(3::5) 
            UnsatElems(:,indxElem)%Lambda        = rDummyArray(4::5) 
            UnsatElems(:,indxElem)%HydCond       = rDummyArray(5::5) * rFactors(3)
            UnsatElems(:,indxElem)%KunsatMethod  = rDummyArray(6::5) 
        END DO
    END IF
    
    !Parametric data input
    IF (NGroup .GT. 0) THEN

        !Read the parameter values at parametric nodes and compute the interpolation coefficients for grid elements
        CALL GetValuesFromParametricGrid(InFile,AppGrid%GridType,NGroup,rFactors,.TRUE.,rDummy3DArray,iStat)
        IF (iStat .EQ. -1) RETURN

        !Initialize parameter values
        DO indxLayer=1,NUZLayers
            DO indxElem=1,NElements
                UnsatElems(indxLayer,indxElem)%ThicknessMax  = rDummy3DArray(indxElem,indxLayer,1)
                UnsatElems(indxLayer,indxElem)%TotalPorosity = rDummy3DArray(indxElem,indxLayer,2)
                UnsatElems(indxLayer,indxElem)%Lambda        = rDummy3DArray(indxElem,indxLayer,3)
                UnsatElems(indxLayer,indxElem)%HydCond       = rDummy3DArray(indxElem,indxLayer,4)
                UnsatElems(indxLayer,indxElem)%KunsatMethod  = rDummy3DArray(indxElem,indxLayer,5)
            END DO
        END DO
    END IF
    
    !Redefine the maximum layer thickness based on aquifer thickness
    DO indxNode=1,AppGrid%NNodes
        iLayer = Stratigraphy%TopActiveLayer(indxNode)
        IF (iLayer .GT. 0) THEN
            NodeDepthToBottom(indxNode) = Stratigraphy%GSElev(indxNode) - Stratigraphy%BottomElev(indxNode,iLayer)
        ELSE
            NodeDepthToBottom(indxNode) = 0.0
        END IF
    END DO
    CALL AppGrid%AreaAverage_ElemData_From_NodeData(NodeDepthToBottom,ElemDepthToBottom)
    CALL ComputeLayerThickness(AppGrid,ElemDepthToBottom,UnsatElems)
    UnsatElems%ThicknessMax = UnsatElems%Thickness
    
    !Check for errors
    DO indxElem=1,NElements
        DO indxLayer=1,NUZLayers
            
            !Porosity cannot be less than zero
            IF (UnsatElems(indxLayer,indxElem)%TotalPorosity .LE. 0.0) THEN
                MessageArray(1) = 'Unsaturated zone porosity becomes less than zero'
                WRITE (MessageArray(2),'(A,F9.3,A)') 'at element '//TRIM(IntToText(indxElem))//', layer '//TRIM(IntToText(indxLayer))//' (',UnsatElems(indxLayer,indxElem)%TotalPorosity,')'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !KunsatMethod must be recognized
            IF (.NOT. ANY(UnsatElems(indxLayer,indxElem)%KunsatMethod.EQ.KunsatMethodList)) THEN
                MessageArray(1) = 'Method to compute unsaturated hydraulic conductivity for unsaturated'
                MessageArray(2) = 'zone at element '//TRIM(IntToText(indxElem))//' and layer '//TRIM(IntToText(indxLayer))//' is not recognized!'
                CALL SetLastMessage(MessageArray(1:2) ,iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END DO
    
  END SUBROUTINE ReadUnsatZoneParameters
  
  
  
  
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
  ! --- PRINT OUT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppUnsatZone,OutFile)
    CLASS(AppUnsatZoneType),INTENT(IN) :: AppUnsatZone
    TYPE(GenericFileType)              :: OutFile
    
    CALL OutFile%WriteData(AppUnsatZone%UnsatElems%Thickness)
    CALL OutFile%WriteData(AppUnsatZone%UnsatElems%Thickness_P)
    CALL OutFile%WriteData(AppUnsatZone%UnsatElems%SoilM)
    CALL OutFile%WriteData(AppUnsatZone%UnsatElems%SoilM_P)
    CALL OutFile%WriteData(AppUnsatZone%RegionalStorage)
    CALL OutFile%WriteData(AppUnsatZone%RegionalStorage_P)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT UNSATURATED ZONE SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppUnsatZone,AppGrid,TimeStep,lEndOfSimulation,Perc)
    CLASS(AppUnsatZoneType)       :: AppUnsatZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    REAL(8),INTENT(IN)            :: Perc(:)
    
    !Return if unsat zone is not simulated
    IF (.NOT. AppUnsatZone%lDefined) RETURN
    
    !Print budget output, if required
    IF (ALLOCATED(AppUnsatZone%BudRawFile))  &
        CALL PrintResultsToBudFile(AppGrid,Perc,AppUnsatZone)
    
    !Print Z-Budget output, if required
    IF (ALLOCATED(AppUnsatZone%ZBudgetRawFile))  &
        CALL PrintResultsToZBudgetFile(AppGrid,Perc,AppUnsatZone)
    
    !Check if end-of-simulation 
    IF (lEndOfSimulation) THEN
        IF (ALLOCATED(AppUnsatZone%FinSimResultsFile))   &
            CALL PrintFinalResults(TimeStep,AppUnsatZone%NUnsatLayers,AppUnsatZone%UnsatElems,AppUnsatZone%FinSimResultsFile)
    END IF
        
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT UNSAT ZONE RESULTS TO BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintResultsToZBudgetFile(AppGrid,Perc,AppUnsatZone)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    REAL(8),INTENT(IN)            :: Perc(:)
    TYPE(AppUnsatZoneType)        :: AppUnsatZone
    
    !Local variables
    INTEGER                                                        :: indxLayer,indxElem,NUZLayers
    REAL(8),DIMENSION(AppGrid%NElements,1)                         :: rData_Pass
    REAL(8),DIMENSION(AppUnsatZone%NUnsatLayers,AppGrid%NElements) :: rDeepPerc,rVertFlow
    
    !Initialize
    NUZLayers = AppUnsatZone%NUnsatLayers
    
    !Prepare data and print
    DO indxLayer=1,NUZLayers
        !Previous storage
        rData_Pass(:,1) = AppUnsatZone%UnsatElems(indxLayer,:)%SoilM_P * AppUnsatZone%UnsatElems(indxLayer,:)%Thickness_P * AppGrid%AppElement%Area
        CALL AppUnsatZone%ZBudgetRawFile%WriteData(AppUnsatZone%NUnsatLayers,iElemDataType,1,indxLayer,rData_Pass)
        
        !Current storage
        rData_Pass(:,1)   = AppUnsatZone%UnsatElems(indxLayer,:)%SoilM * AppUnsatZone%UnsatElems(indxLayer,:)%Thickness * AppGrid%AppElement%Area
        CALL AppUnsatZone%ZBudgetRawFile%WriteData(AppUnsatZone%NUnsatLayers,iElemDataType,2,indxLayer,rData_Pass)
        
        !Percolation
        IF (indxLayer .EQ. 1) THEN
            rData_Pass(:,1) = Perc
            CALL AppUnsatZone%ZBudgetRawFile%WriteData(AppUnsatZone%NUnsatLayers,iElemDataType,3,indxLayer,rData_Pass)
        END IF
        
    END DO
    
    !Deep perc and vertical flows between layers depending on how many unsaturated layers are active
    IF (NUZLayers .GT. 1) THEN
        DO indxElem=1,AppGrid%NElements
            rVertFlow(:,indxElem) = 0.0
            rDeepPerc(:,indxElem) = 0.0
            IF (AppUnsatZone%UnsatElems(1,indxElem)%Thickness .EQ. 0.0) THEN
                rDeepPerc(1,indxElem) = Perc(indxElem) + AppUnsatZone%UnsatElems(1,indxElem)%SoilM_To_GW
            ELSE
                DO indxLayer=1,NUZLayers-1
                    IF (AppUnsatZone%UnsatElems(indxLayer+1,indxElem)%Thickness .EQ. 0.0) THEN
                        rDeepPerc(indxLayer,indxElem)   = AppUnsatZone%UnsatElems(indxLayer,indxElem)%Outflow + AppUnsatZone%UnsatElems(indxLayer,indxElem)%SoilM_To_GW
                        rDeepPerc(indxLayer+1,indxElem) = AppUnsatZone%UnsatElems(indxLayer+1,indxElem)%SoilM_To_GW
                        CYCLE
                    ELSE
                        rVertFlow(indxLayer,indxElem)      = -AppUnsatZone%UnsatElems(indxLayer,indxElem)%Outflow  !Downward flow must be assigned as negative flow 
                    END IF
                END DO
                IF (AppUnsatZone%UnsatElems(NUZLayers,indxElem)%Thickness .GT. 0.0) THEN
                    rDeepPerc(NUZLayers,indxElem) = AppUnsatZone%UnsatElems(NUZLayers,indxElem)%Outflow + AppUnsatZone%UnsatElems(NUZLayers,indxElem)%SoilM_To_GW
                END IF
            END IF
        END DO
    ELSE
        rDeepPerc(1,:) = AppUnsatZone%DeepPerc
    END IF   
    
    !Write out deep perc and vertical flows
    DO indxLayer=1,NUZLayers-1
        rData_Pass(:,1) = rDeepPerc(indxLayer,:)
        CALL AppUnsatZone%ZBudgetRawFile%WriteData(NUZLayers,iElemDataType,4,indxLayer,rData_Pass)
        rData_Pass(:,1) = rVertFlow(indxLayer,:)
        CALL AppUnsatZone%ZBudgetRawFile%WriteData(NUZLayers,iVerticalFlowType,0,indxLayer,rData_Pass)
    END DO 
    rData_Pass(:,1) = rDeepPerc(NUZLayers,:)
    CALL AppUnsatZone%ZBudgetRawFile%WriteData(NUZLayers,iElemDataType,4,NUZLayers,rData_Pass)
    
  END SUBROUTINE PrintResultsToZBudgetFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT UNSAT ZONE RESULTS TO BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintResultsToBudFile(AppGrid,Perc,AppUnsatZone)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    REAL(8),INTENT(IN)            :: Perc(:)
    TYPE(AppUnsatZoneType)        :: AppUnsatZone
    
    !Local variables
    INTEGER                                  :: NRegions
    REAL(8)                                  :: DummyArray(NBudColumns,(AppGrid%NSubregions+1))
    REAL(8),DIMENSION(AppGrid%NSubregions+1) :: RPerc,RDeepPerc,RError
    
    !Initialize
    NRegions = AppGrid%NSubregions
    
    !Regional perc
    RPerc(1:NRegions) = AppGrid%AccumElemValuesToSubregions(Perc)
    RPerc(NRegions+1) = SUM(RPerc(1:NRegions))
    
    !Regional deep percolation
    RDeepPerc(1:NRegions) = AppGrid%AccumElemValuesToSubregions(AppUnsatZone%DeepPerc)
    RDeepPerc(NRegions+1) = SUM(RDeepPerc(1:NRegions))
    
    !Mass balance error
    RError = AppUnsatZone%RegionalStorage_P  &
           + RPerc                           &
           - RDeepPerc                       &
           - AppUnsatZone%RegionalStorage
    
    !Store budget data in array
    DummyArray(1,:) = AppUnsatZone%RegionalStorage_P
    DummyArray(2,:) = AppUnsatZone%RegionalStorage
    DummyArray(3,:) = RPerc    
    DummyArray(4,:) = RDeepPerc 
    DummyArray(5,:) = RError
   
    !Write data
    CALL AppUnsatZone%BudRawFile%WriteData(DummyArray)
  
  END SUBROUTINE PrintResultsToBudFile

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT END-OF-SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalResults(TimeStep,NUZLayers,UnsatElems,OutFile)
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    INTEGER,INTENT(IN)             :: NUZLayers
    TYPE(UnsatElemType),INTENT(IN) :: UnsatElems(:,:)
    TYPE(GenericFileType)          :: OutFile
    
    !Local variables
    INTEGER   :: indxLayer,indxElem
    CHARACTER :: SimulationTime*21,Text*500
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print header
    CALL OutFile%WriteData('C'//REPEAT('*',100))
    CALL OutFile%WriteData('C ***** UNSATURATED ZONE MOISTURE AT '//TRIM(SimulationTime))
    CALL OutFile%WriteData('C'//REPEAT('*',100))
    CALL OutFile%WriteData('C')    
    CALL OutFile%WriteData('C'//REPEAT('-',100))
    CALL OutFile%WriteData('C                             Unsaturated Layers')
    CALL OutFile%WriteData('C         --------------------------------------------------------------------')
    Text = 'C    ID            1'
    DO indxLayer=2,NUZLayers
        WRITE(Text,'(A,I12)') TRIM(Text),indxLayer
    END DO
    CALL OutFile%WriteData(TRIM(Text))
    CALL OutFile%WriteData('C'//REPEAT('-',100))
    
    !Print end-of-simulation values
    DO indxElem=1,SIZE(UnsatElems,DIM=2)
        WRITE (Text,'(I7,1X,500F12.4)') indxElem,UnsatElems(:,indxElem)%SoilM
        CALL OutFile%WriteData(TRIM(Text))
    END DO
    
    !Close file
    CALL OutFile%Kill()
  
  END SUBROUTINE PrintFinalResults
  
  
  
  
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
  ! --- UPDATE REGIONAL STORAGE
  ! -------------------------------------------------------------
  SUBROUTINE UpdateStorage(AppUnsatZone,AppGrid)
    CLASS(AppUnsatZoneType)      :: AppUnsatZone
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    
    IF (.NOT. AppUnsatZone%lDefined) RETURN
    
    AppUnsatZone%RegionalStorage = RegionalStorage(AppGrid,AppUnsatZone%UnsatElems)
    
  END SUBROUTINE UpdateStorage
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE REGIONAL STORAGE
  ! -------------------------------------------------------------
  FUNCTION RegionalStorage(AppGrid,UnsatElems) RESULT(RStor)
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    TYPE(UnsatElemType),INTENT(IN) :: UnsatElems(:,:)
    REAL(8)                        :: RStor(AppGrid%NSubregions+1)
    
    !Local variables
    INTEGER :: indxLayer,NSubregions
    REAL(8) :: SoilM_Volume(AppGrid%NElements)
    
    !Initialize
    NSubregions = AppGrid%NSubregions
    RStor       = 0.0

    DO indxLayer=1,SIZE(UnsatElems,DIM=1)
        SoilM_Volume         = UnsatElems(indxLayer,:)%SoilM * UnsatElems(indxLayer,:)%Thickness * AppGrid%AppElement%Area
        RStor(1:NSubregions) = RStor(1:NSubregions) + AppGrid%AccumElemValuesToSubregions(SoilM_Volume)
    END DO
    RStor(NSubregions+1) = SUM(RStor(1:NSubregions))
    
  END FUNCTION RegionalStorage
  
  
  ! -------------------------------------------------------------
  ! --- PREPARE UNSAT ZONE BUDGET BINARY FILE HEADER DATA
  ! -------------------------------------------------------------
  FUNCTION PrepareBudgetHeader(AppGrid,NTIME,TimeStep,cIWFMVersion) RESULT(Header)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cIWFMVersion
    TYPE(BudgetHeaderType)        :: Header
    
    !Local variables
    INTEGER,PARAMETER           :: TitleLen           = 88   , &
                                   NTitles            = 4    , &
                                   NColumnHeaderLines = 4    
    TYPE(TimeStepType)          :: TimeStepLocal
    CHARACTER                   :: UnitT*10,TextTime*17
    INTEGER                     :: iCount,indxLocation,indxCol,NRegions,I
    CHARACTER(LEN=18),PARAMETER :: FParts(NBudColumns) = ['BEGIN_STORAGE'      ,& 
                                                          'END_STORAGE'        ,& 
                                                          'PERC'               ,&                                                           
                                                          'DEEP_PERC'          ,& 
                                                          'DISCREPANCY'        ]
    
    !Initialize
    NRegions = AppGrid%NSubregions
                                                      
    !Increment the initial simulation time to represent the data begin date for budget binary output files  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    TextTime = ArrangeText(TRIM(UnitT),17)
    
    !Budget descriptor
    Header%cBudgetDescriptor = 'unsaturated zone budget'

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    ALLOCATE (Header%Areas(NRegions+1))
    Header%NAreas            = NRegions + 1
    Header%Areas(1:NRegions) = AppGrid%AppSubregion%Area
    Header%Areas(NRegions+1) = SUM(AppGrid%AppSubregion%Area)

    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles) , pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM (v'//TRIM(cIWFMVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('UNSATURATED ZONE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,50(F13.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    Header%NLocations = NRegions + 1
    ALLOCATE (Header%cLocationNames(NRegions+1))
    Header%cLocationNames(1:NRegions) = AppGrid%AppSubregion%Name 
    Header%cLocationNames(NRegions+1) = 'ENTIRE MODEL AREA'

    !Locations
    ALLOCATE (Header%Locations(1)                                                        , &
              Header%Locations(1)%cFullColumnHeaders(NBudColumns+1)                      , &
              Header%Locations(1)%iDataColumnTypes(NBudColumns)                          , &
              Header%Locations(1)%iColWidth(NBudColumns+1)                               , &
              Header%Locations(1)%cColumnHeaders(NBudColumns+1,NColumnHeaderLines)       , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns           = NBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                       
      pLocation%cFullColumnHeaders(2:) = cBudgetColumnTitles                               
      pLocation%iDataColumnTypes       = [VLB,&  !Beginning storage
                                          VLE,&  !Ending storage
                                          VR ,&  !Percolation
                                          VR ,&  !Deep perc
                                          VR ]   !Discrepancy
      pLocation%iColWidth              = [17,(13,I=1,NBudColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        pColumnHeaders(:,1) = ['                 ','     Beginning','       Ending ','              ','      Deep    ','              ']
        pColumnHeaders(:,2) = ['      Time       ','      Storage ','       Storage','   Percolation','   Percolation','   Discrepancy']
        pColumnHeaders(:,3) = [      TextTime     ,'        (+)   ','         (-)  ','       (+)    ','       (-)    ','       (=)    ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,15A14)'
        pFormatSpecs(2)     = '(A17,15A14)'
        pFormatSpecs(3)     = '(A17,15A14)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NBudColumns*(NRegions+1)) , pDSSOutput%iDataTypes(1))
      iCount = 1
      DO indxLocation=1,NRegions+1
        DO indxCol=1,NBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_UNSATZONE_BUD/'                                         //  &  !A part
                                          TRIM(UpperCase(Header%cLocationNames(indxLocation)))//'/'      //  &  !B part
                                          'VOLUME/'                                                      //  &  !C part
                                          '/'                                                            //  &  !D part
                                          TRIM(TimeStep%Unit)//'/'                                       //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = PER_CUM
    END ASSOCIATE

  END FUNCTION PrepareBudgetHeader
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF THE UNSAT ZONE IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppUnsatZone)
    CLASS(AppUnsatZoneType) :: AppUnsatZone
    
    AppUnsatZone%UnsatElems%SoilM_P     = AppUnsatZone%UnsatElems%SoilM
    AppUnsatZone%RegionalStorage_P      = AppUnsatZone%RegionalStorage  
    AppUnsatZone%UnsatElems%Thickness_P = AppUnsatZone%UnsatElems%Thickness    
    AppUnsatZone%lThicknessUpdated      = .FALSE.
    
  END SUBROUTINE AdvanceState


  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF UNSATURATED ZONE RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppUnsatZone,NewUnit)
    CLASS(AppUnsatZoneType)     :: AppUnsatZone
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
    
    !Make sure unsarturated zone is simulated
    IF (.NOT. AppUnsatZone%lDefined) RETURN
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert time unit of small watershed parameters
    Factor                          = TimeIntervalConversion(NewUnit,AppUnsatZone%VarTimeUnit)
    AppUnsatZone%VarTimeUnit        = NewUnit
    AppUnsatZone%UnsatElems%HydCond = AppUnsatZone%UnsatElems%HydCond * Factor
          
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppUnsatZone,Perc,DepthToGW,AppGrid,iStat)
    CLASS(AppUnsatZoneType)      :: AppUnsatZone
    REAL(8),INTENT(IN)           :: Perc(:),DepthToGW(:)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    INTEGER                     :: indxElem,indxLayer,NUZLayers
    REAL(8)                     :: Inflow,D,TotalPorosity,Excess,Outflow,AchievedConv,Area,D_P,SoilM_P,Toler
    
    !Initialize
    iStat = 0
        
    !Return if unsat zone is not simulated
    IF (.NOT. AppUnsatZone%lDefined) RETURN
    
    !Inform user
    CALL EchoProgress('Simulating unsaturated zone...')
    
    !Initailize
    NUZLayers = AppUnsatZone%NUnsatLayers
    
    !Update layer thicknesses, if necessary
    IF (.NOT. AppUnsatZone%lThicknessUpdated) THEN
        CALL ComputeLayerThickness(AppGrid,DepthToGW,AppUnsatZone%UnsatElems)
        AppUnsatZone%lThicknessUpdated = .TRUE.
    END IF
    
    !Route moisture through unsat zone
    ASSOCIATE (pUnsatElems => AppUnsatZone%UnsatElems)
        DO indxElem=1,AppGrid%NElements
            Area   = AppGrid%AppElement(indxElem)%Area
            Inflow = Perc(indxElem)/Area
            DO indxLayer=1,NUZLayers
                D       = pUnsatElems(indxLayer,indxElem)%Thickness
                D_P     = pUnsatElems(indxLayer,indxElem)%Thickness_P 
                Excess  = 0.0
                Outflow = 0.0
                IF (D .GT. 0.0) THEN
                    IF (D .LT. D_P) THEN
                        SoilM_P = pUnsatElems(indxLayer,indxElem)%SoilM_P * D
                    ELSE
                        SoilM_P = pUnsatElems(indxLayer,indxElem)%SoilM_P * D_P
                    END IF
                    TotalPorosity = pUnsatElems(indxLayer,indxElem)%TotalPorosity * D
                    Toler         = MAX(TotalPorosity * AppUnsatZone%SolverData%Tolerance / Area , 1D-12)
                    CALL VadoseZoneMoistureRouter(Inflow                                       ,  &
                                                  pUnsatElems(indxLayer,indxElem)%HydCond      ,  &
                                                  TotalPorosity                                ,  &
                                                  pUnsatElems(indxLayer,indxElem)%Lambda       ,  &
                                                  SoilM_P                                      ,  &
                                                  Toler                                        ,  &
                                                  pUnsatElems(indxLayer,indxElem)%KunsatMethod ,  &
                                                  AppUnsatZone%SolverData%IterMax              ,  &
                                                  pUnsatElems(indxLayer,indxElem)%SoilM        ,  &
                                                  Excess                                       ,  &
                                                  Outflow                                      ,  &
                                                  AchievedConv                                 )
                    !Generate error if convergence is not achieved
                    IF (AchievedConv .NE. 0.0) THEN
                        MessageArray(1) = 'Convergence error in routing moisture through unsaturated zone at element '//TRIM(IntToText(indxElem))//', layer '//TRIM(IntToText(indxLayer))//'!'
                        WRITE (MessageArray(2),'(A,F11.8)') 'Desired convergence  = ',Toler
                        WRITE (MessageArray(3),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    pUnsatElems(indxLayer,indxElem)%SoilM   = pUnsatElems(indxLayer,indxElem)%SoilM / D  !Convert moisture depth back to moisture content
                ELSE
                    pUnsatElems(indxLayer,indxElem)%SoilM   = 0.0
                    pUnsatElems(indxLayer,indxElem)%Outflow = 0.0
                    CYCLE
                END IF
                Inflow                                  = Outflow + Excess
                pUnsatElems(indxLayer,indxElem)%Outflow = Inflow * Area
            END DO
            AppUnsatZone%DeepPerc(indxElem) = Inflow * Area + SUM(pUnsatElems(:,indxElem)%SoilM_To_GW)
        END DO
    END ASSOCIATE

  END SUBROUTINE Simulate
  

  ! -------------------------------------------------------------
  ! --- COMPUTE UNSAT ZONE LAYER THICKNESS GIVEN GW HEAD
  ! -------------------------------------------------------------
  SUBROUTINE ComputeLayerThickness(AppGrid,DepthToGW,UnsatElems)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)           :: DepthToGW(:)
    TYPE(UnsatElemType)          :: UnsatElems(:,:)
    
    !Local variables
    INTEGER :: indxLayer,indxElem,NUZLayers
    REAL(8) :: ThickMax,DT,ThickOri,Area
    
    !Initialize
    NUZLayers = SIZE(UnsatElems , DIM=1)
    
    !Compute thicknesses
    DO indxElem=1,AppGrid%NElements
        Area = AppGrid%AppElement(indxElem)%Area
        DT   = 0.0
        DO indxLayer=1,NUZLayers
            !Compute thickness
            ThickMax = UnsatElems(indxLayer,indxElem)%ThicknessMax
            ThickOri = UnsatElems(indxLayer,indxElem)%Thickness
            DT       = DT + ThickMax
            IF (DT .LE. DepthToGW(indxElem)) THEN
                IF (indxLayer .EQ. NUZLayers) THEN
                    UnsatElems(indxLayer,indxElem)%Thickness = DepthToGW(indxElem) - (DT-ThickMax)
                ELSE
                    UnsatElems(indxLayer,indxElem)%Thickness = DT
                END IF
            ELSE
                UnsatElems(indxLayer,indxElem)%Thickness = MAX(DepthToGW(indxElem) - (DT-ThickMax)  ,  0.0)
            END IF
            IF (UnsatElems(indxLayer,indxElem)%Thickness .LT. 1D-10) UnsatElems(indxLayer,indxElem)%Thickness = 0.0 
            
            !Calculate soil moisture to gw when gw is rising
            IF (UnsatElems(indxLayer,indxElem)%Thickness .LT. ThickOri) THEN
                UnsatElems(indxLayer,indxElem)%SoilM_To_GW = (ThickOri-UnsatElems(indxLayer,indxElem)%Thickness) * UnsatElems(indxLayer,indxElem)%SoilM * Area
            ELSE
                UnsatElems(indxLayer,indxElem)%SoilM_To_GW = 0.0
            END IF            
        END DO
    END DO
    
  END SUBROUTINE ComputeLayerThickness

END MODULE