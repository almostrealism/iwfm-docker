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
!  This program is distributed in the hope that it will be useful
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
MODULE Package_AppSmallWatershed
  USE GeneralUtilities       , ONLY: StripTextUntilCharacter       , &
                                     ArrangeText                   , &
                                     IntToText                     , &
                                     CleanSpecialCharacters        , &
                                     EstablishAbsolutePathFileName , &
                                     LocateInList                  , &
                                     ShellSort                     , &
                                     GetUniqueArrayComponents      , &
                                     ConvertID_To_Index
  USE TimeSeriesUtilities    , ONLY: TimeStepType                  , &
                                     IncrementTimeStamp            , &
                                     IsTimeIntervalValid           , &
                                     TimeIntervalConversion        
  USE IOInterface            , ONLY: GenericFileType               , &
                                     f_iUNKNOWN                       
  USE MessageLogger          , ONLY: SetLastMessage                , &
                                     EchoProgress                  , &
                                     MessageArray                  , &
                                     f_iFatal                        
  USE Package_Discretization , ONLY: AppGridType                   , &
                                     StratigraphyType              
  USE Package_Misc           , ONLY: SolverDataType                , &
                                     f_iGWComp                     , &
                                     f_iSWShedComp                 , & 
                                     f_iLocationType_SmallWatershed, &
                                     f_iAllLocationIDsListed         
  USE Package_UnsatZone      , ONLY: RootZoneSoilType              , &
                                     f_iKunsatMethodList           , &
                                     NonPondedLUMoistureRouter     
  USE Package_PrecipitationET, ONLY: PrecipitationType             , &
                                     ETType                        
  USE Package_Budget         , ONLY: BudgetType                    , &
                                     BudgetHeaderType              , &
                                     f_cVolumeUnitMarker           , &
                                     f_cLocationNameMarker         , &
                                     f_cAreaMarker                 , &
                                     f_cAreaUnitMarker             , &
                                     f_iColumnHeaderLen            , &
                                     f_iVR                         , &
                                     f_iVLB                        , &
                                     f_iVLE                        , &
                                     f_iPER_CUM                       
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
  PUBLIC :: AppSmallWatershedType  , &
            SWShedBaseFlowBCID     , &
            SWShedPercFlowBCID     , &
            f_iBudgetType_SWShed   , & 
            f_iSWShedBudComp_RZ    , &
            f_iSWShedBudComp_GW 


  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR GW NODE RECEIVING BASEFLOW FROM SMALL WATERSHED
  ! -------------------------------------------------------------
  TYPE BaseFlowGWNodeType
      PRIVATE
      INTEGER :: Node   = 0     !Node that receives baseflow
      INTEGER :: Layer  = 0     !Aquifer layer for receiving node
      REAL(8) :: Frac   = 0.0   !Fraction of total baseflow that flow into node
      REAL(8) :: Flow   = 0.0   !Flow into receiving node
  END TYPE BaseFlowGWNodeType

  
  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR GW NODE RECEIVING PERCOLATION FROM SMALL WATERSHED
  ! -------------------------------------------------------------
  TYPE PercFlowGWNodeType
      PRIVATE
      INTEGER :: Node     = 0      !Node that receives percolation
      INTEGER :: Layer    = 0      !Aquifer layer for receving node (this is the top active node)
      REAL(8) :: MaxFlow  = 0.0    !Maximum percolation rate
      REAL(8) :: Flow     = 0.0    !Flow into receiving node
  END TYPE PercFlowGWNodeType
  
  
  ! -------------------------------------------------------------
  ! --- NODE LIST DATA TYPE
  ! -------------------------------------------------------------
  TYPE NodeListType
      PRIVATE
      INTEGER             :: NNodes = 0    !Number of small watershed related nodes in a given aquifer layer
      INTEGER,ALLOCATABLE :: Nodes(:)      !List of nodes
  END TYPE NodeListType

  
  ! -------------------------------------------------------------
  ! --- SMALL WATERSHED DATA TYPE
  ! -------------------------------------------------------------
  TYPE SmallWatershedType
      PRIVATE
      INTEGER                              :: ID               = 0      !Small watershed ID number
      REAL(8)                              :: Area             = 0.0    !Area of small watershed
      TYPE(RootZoneSoilType)               :: Soil                      !Small watershed root zone soil parameters
      REAL(8)                              :: RootDepth        = 0.0    !Rooting depth; [L]
      REAL(8)                              :: SMax             = 0.0    !Maximum soil retention parameter
      INTEGER                              :: iColPrecip       = 0      !Column number in the Precipitation Data File 
      REAL(8)                              :: PrecipFactor     = 0.0    !Factor to multiply the precipitation data
      REAL(8)                              :: Precip           = 0.0    !Precipitation; [L/T]
      REAL(8)                              :: Runoff           = 0.0    !Rainfall runoff out of small watershed; [L^3/T]
      REAL(8)                              :: PrecipInfilt     = 0.0    !Infiltration due to precipitation; [L^3/T]
      INTEGER                              :: iColET           = 0      !Column number in the ET Data File
      REAL(8)                              :: ETc              = 0.0    !Potential ET; [L/T]
      REAL(8)                              :: ETa              = 0.0    !Actual ET; [L^3/T]
      REAL(8)                              :: RootZonePerc     = 0.0    !Root zone percolation (=deep perc, also); [L^3/T]
      REAL(8)                              :: GWThreshold      = 0.0    !Threshold GW storage depth beyond which surface runoff starts; [L]
      REAL(8)                              :: MaxGWStor        = 0.0    !Maximum GW storage depth; [L]
      REAL(8)                              :: SurfaceFlowCoeff = 0.0    !Coefficient to convert GW storage to surface flow; [1/T]
      REAL(8)                              :: BaseFlowCoeff    = 0.0    !Coefficient to convert GW storage to base flow; [1/T]
      INTEGER                              :: NBaseFlowNodes   = 0      !Number of groundwater nodes that receive baseflow
      TYPE(BaseFlowGWNodeType),ALLOCATABLE :: BaseFlowNodes(:)          !List and data of the baseflow receiving gw nodes
      INTEGER                              :: NPercFlowNodes   = 0      !Number of groundwater nodes receiving percolation flow
      TYPE(PercFlowGWNodeType),ALLOCATABLE :: PercFlowNodes(:)          !List and data of the percolation receiving gw nodes
      REAL(8)                              :: ExcessGWRunoff   = 0.0    !Surface runoff out of watershed due to groundwater storage exeeding GW threshold; [L^3/T]
      REAL(8)                              :: NetStreamInflow  = 0.0    !Net inflow into destination stream node; [L^3/T]
      INTEGER                              :: StrmNode         = 0      !Stream node that receives surface flow from small watershed
      REAL(8)                              :: SoilMoist        = 0.0    !Soil moisture at the current time step; [L]
      REAL(8)                              :: SoilMoist_P      = 0.0    !Soil moisture at the previous time step; [L]
      REAL(8)                              :: GWStor_P         = 0.0    !GW storage at the previous time step; [L]
      REAL(8)                              :: GWStor           = 0.0    !Groundwater storage at current time step; [L]
  END TYPE SmallWatershedType
  
  
  ! -------------------------------------------------------------
  ! --- SMALL WATERSHED DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE AppSmallWatershedType
      PRIVATE
      CHARACTER(LEN=6)                     :: VarTimeUnit         = ''      !Time unit of releavnt parameters for the small watershdes
      INTEGER                              :: NSWShed             = 0       !Number of small watersheds modeled
      LOGICAL                              :: lDefined            = .FALSE. !Flag to check if small watersheds are simulated
      TYPE(SmallWatershedType),ALLOCATABLE :: SmallWatersheds(:)            !List of small watersheds and their parameters
      TYPE(NodeListType),ALLOCATABLE       :: BaseFlowNodeList(:)           !List of groundwater nodes receiving base flow at each (layer)
      TYPE(NodeListType),ALLOCATABLE       :: PercFlowNodeList(:)           !List of groundwater nodes receiving percolation flow at each (layer)
      TYPE(SolverDataType)                 :: RZSolverData                  !Tolerance and maximum iteration to be used in the simulation of root zone of watersheds
      LOGICAL                              :: lBudRawFile_Defined = .FALSE. !Flag to check if budget binary file will be printed
      TYPE(BudgetType)                     :: BudRawFile                    !Budget binray file
      TYPE(GenericFileType)                :: FinResultsFile                !Optional file to print out the end-of-simulation results
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetBudget_List
      PROCEDURE,PASS   :: GetBudget_NColumns
      PROCEDURE,PASS   :: GetBudget_ColumnTitles
      PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenAppSmallWatershed
      PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetBudget_TSData
      PROCEDURE,PASS   :: GetNSmallWatersheds
      PROCEDURE,PASS   :: GetSmallWatershedIDs
      PROCEDURE,PASS   :: GetStreamInflows
      PROCEDURE,PASS   :: GetSubregionalGWInflows
      PROCEDURE,PASS   :: GetNodesWithBCType 
      PROCEDURE,PASS   :: GetNetBCFlowWithBCType
      PROCEDURE,PASS   :: GetBoundaryFlowAtElementNodeLayer
      PROCEDURE,PASS   :: GetBoundaryFlowAtFaceLayer_AtOneFace    
      PROCEDURE,PASS   :: GetBoundaryFlowAtFaceLayer_AtSomeFaces
      PROCEDURE,PASS   :: GetPercFlow_ForAllSmallwatersheds
      PROCEDURE,PASS   :: GetRootZonePerc_ForAllSmallwatersheds
      PROCEDURE,PASS   :: GetRootZonePerc_ForOneSmallWatershed
      PROCEDURE,PASS   :: ReadTSData                         
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: IsDefined
      PROCEDURE,PASS   :: IsBaseFlowSimulated                
      PROCEDURE,PASS   :: IsPercFlowSimulated               
      PROCEDURE,PASS   :: Simulate
      PROCEDURE,PASS   :: AdvanceState                      
      PROCEDURE,PASS   :: UpdateRHS                          
      PROCEDURE,PASS   :: ConvertTimeUnit                   
      GENERIC          :: GetBoundaryFlowAtFaceLayer => GetBoundaryFlowAtFaceLayer_AtOneFace          , &
                                                        GetBoundaryFlowAtFaceLayer_AtSomeFaces
      GENERIC          :: GetBudget_MonthlyFlows     => GetBudget_MonthlyFlows_GivenFile              , &
                                                        GetBudget_MonthlyFlows_GivenAppSmallWatershed
  END TYPE AppSmallWatershedType
  
  
  ! -------------------------------------------------------------
  ! --- BOUNDARY FLOW FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: SWShedBaseFlowBCID = 1  , &
                       SWShedPercFlowBCID = 2

  
  ! -------------------------------------------------------------
  ! --- BUDGET COMPONENT FLAGS FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iBudgetType_SWShed = f_iSWShedComp*1000 + 1 , &           
                                 f_iSWShedBudComp_RZ  = 1                      , &
                                 f_iSWShedBudComp_GW  = 2
  CHARACTER(LEN=22),PARAMETER :: f_cDescription_SWShedBudget ='Small watershed budget'
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iNBudgetCols = 18
  CHARACTER(LEN=31),PARAMETER :: f_cBudgetColumnTitles(f_iNBudgetCols) = ['Precipitation'                   , &
                                                                          'Runoff'                          , &
                                                                          'Root Zone Beginning Storage (+)' , &
                                                                          'Infiltration (+)'                , &
                                                                          'Actual ET (-)'                   , &
                                                                          'Deep Percolation (-)'            , &
                                                                          'Root Zone Ending Storage (-)'    , &
                                                                          'Root Zone Discrepancy (=)'       , &
                                                                          'GW Beginning Storage (+)'        , &
                                                                          'Recharge (+)'                    , &
                                                                          'Base Flow (-)'                   , &
                                                                          'GW Return Flow (-)'              , &
                                                                          'GW Ending Storage (-)'           , &
                                                                          'GW Discrepancy (=)'              , &
                                                                          'Total Surface Flow (+)'          , &
                                                                          'Percolation to GW (-)'           , &
                                                                          'Net Stream Inflow (=)'           , &
                                                                          'Total GW Inflow'                 ]
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen  = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'Package_AppSmallWatershed::'


   
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
  ! --- NEW SMALL WATERSHED DATABASE
  ! -------------------------------------------------------------
  SUBROUTINE New(AppSWShed,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,NStrmNodes,iStrmNodeIDs,AppGrid,Stratigraphy,cIWFMVersion,iStat) 
    CLASS(AppSmallWatershedType),INTENT(OUT) :: AppSWShed
    LOGICAL,INTENT(IN)                       :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)              :: cFileName,cWorkingDirectory,cIWFMVersion
    TYPE(TimeStepType),INTENT(IN)            :: TimeStep
    INTEGER,INTENT(IN)                       :: NStrmNodes,iStrmNodeIDs(NStrmNodes),NTIME
    TYPE(AppGridType),INTENT(IN)             :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)        :: Stratigraphy
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    TYPE(GenericFileType)    :: MainFile
    CHARACTER                :: ALine*1200,cBudFileName*1200,cFinResultsFileName*1200
    CHARACTER(:),ALLOCATABLE :: cAbsPathFileName
    INTEGER                  :: NSWShed,indxLayer,iGWNodeIDs(AppGrid%NNodes)
    INTEGER,ALLOCATABLE      :: iSWShedIDs(:)
    
    !Initialize
    iStat      = 0
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Return if no filename is specified
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('Instantiating small watershed component...')
    
    !Open main input file
    CALL MainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='small watershed parameters',iStat=iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !Read away the version number line
    CALL MainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Budget output file name
    CALL MainFile%ReadData(cBudFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cBudFileName = StripTextUntilCharacter(cBudFileName,'/')
    CALL CleanSpecialCharacters(cBudFileName)  
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cBudFileName)),cWorkingDirectory,cAbsPathFileName)
    cBudFileName = cAbsPathFileName
    
    !Final results output name
    CALL MainFile%ReadData(cFinResultsFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    cFinResultsFileName = StripTextUntilCharacter(cFinResultsFileName,'/')  
    CALL CleanSpecialCharacters(cFinResultsFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cFinResultsFileName)),cWorkingDirectory,cAbsPathFileName)
    cFinResultsFileName = cAbsPathFileName

    !Number of small watersheds; if zero return
    CALL MainFile%ReadData(NSWShed,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NSWShed .EQ. 0) RETURN
    AppSWShed%NSWShed = NSWShed
    
    !Allocate memory
    ALLOCATE (AppSWShed%SmallWatersheds(NSWShed)               , &
              AppSWShed%BaseFlowNodeList(Stratigraphy%NLayers) , &
              AppSWShed%PercFlowNodeList(Stratigraphy%NLayers) )

    !Read geospatial parameters
    CALL ReadGeospatialData(MainFile,TimeStep,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,AppSWShed,iStat)
    IF (iStat .EQ. -1) RETURN
    ALLOCATE (iSWShedIDs(NSWShed))
    iSWShedIDs = AppSWShed%SmallWatersheds%ID
    
    !Read root zone parameters
    CALL ReadRootZoneData(MainFile,TimeStep,iSWShedIDs,AppSWShed,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read aquifer related parameters
    CALL ReadAquiferData(MainFile,TimeStep,iSWShedIDs,AppSWShed,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read initial conditions
    CALL ReadInitialConditions(MainFile,iSWShedIDs,AppSWShed,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the small watershed budget file
    IF (cBudFileName .NE. '')  THEN
        CALL InitSmallWatershedBudRawFile(IsForInquiry,TRIM(cBudFileName),cIWFMVersion,iSWShedIDs,NTIME,TimeStep,AppSWShed,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Instantiate the file to print final results
    IF (cFinResultsFileName .NE. '') THEN
        IF (IsForInquiry) THEN
            CALL AppSWShed%FinResultsFile%New(FileName=cFinResultsFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='small watersheds final simulation results',iStat=iStat) 
        ELSE
            CALL AppSWShed%FinResultsFile%New(FileName=cFinResultsFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='small watersheds final simulation results',iStat=iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Compile list of nodes recieving baseflow and percolation at each layer
    DO indxLayer=1,Stratigraphy%NLayers
        !Baseflow nodes
        CALL CompileNodesWithBCType(AppSWShed,indxLayer,SWShedBaseFlowBCID,AppSWShed%BaseFlowNodeList(indxLayer)%Nodes)
        AppSWShed%BaseFlowNodeList(indxLayer)%NNodes = SIZE(AppSWShed%BaseFlowNodeList(indxLayer)%Nodes)
        
        !Percolation nodes
        CALL CompileNodesWithBCType(AppSWShed,indxLayer,SWShedPercFlowBCID,AppSWShed%PercFlowNodeList(indxLayer)%Nodes)
        AppSWShed%PercFlowNodeList(indxLayer)%NNodes = SIZE(AppSWShed%PercFlowNodeList(indxLayer)%Nodes)        
    END DO
    
    !Set the flag
    IF (AppSWShed%NSWShed .GT. 0) AppSWShed%lDefined = .TRUE.
    
    !Close main input file
    CALL MainFile%Kill()
      
  END SUBROUTINE New 
  
  
  ! -------------------------------------------------------------
  ! --- NEW SMALL WATERSHED BUDGET OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE InitSmallWatershedBudRawFile(IsForInquiry,cFileName,cIWFMVersion,iSWShedIDs,NTIME,TimeStep,AppSWShed,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cIWFMVersion
    INTEGER,INTENT(IN)            :: iSWShedIDs(:),NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppSmallWatershedType)   :: AppSWShed
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    TYPE(BudgetHeaderType)      :: Header
    TYPE(TimeStepType)          :: TimeStepLocal
    INTEGER,PARAMETER           :: TitleLen           = 296        , &
                                   NTitles            = 6          , &
                                   NColumnHeaderLines = 4          , &
                                   NColumns           = f_iNBudgetCols
    INTEGER                     :: iCount,indxLocation,indxCol,indx,NSWShed,I
    CHARACTER                   :: UnitT*10,TextTime*17
    CHARACTER(LEN=15),PARAMETER :: FParts(NColumns) = ['PRECIP'         , &
                                                       'RUNOFF'         , & 
                                                       'RZ_BEGIN_STOR'  , & 
                                                       'INFILTR'        , &
                                                       'ET'             , &
                                                       'RZ_PERC'        , &
                                                       'RZ_END_STOR'    , &
                                                       'RZ_DISCREPANCY' , &
                                                       'GW_BEGIN_STOR'  , &
                                                       'RECHARGE'       , &
                                                       'BASEFLOW'       , &
                                                       'GW_RTRN_FLOW'   , &
                                                       'GW_END_STOR'    , &
                                                       'GW_DISCREPANCY' , &
                                                       'TOTAL_SRFC_FLOW', &
                                                       'PERC_TO_GW'     , &
                                                       'NET_STRM_INFLOW', &
                                                       'TOTAL_GW_INFLOW']
    
    !Initialize
    iStat = 0

    !Instantiate the small stream budget raw file and set the flag for when the file is opened for inquiry purposes
    IF (IsForInquiry) THEN
        CALL AppSWShed%BudRawFile%New(TRIM(CFileName),iStat)  ;  IF (iStat .EQ. -1) RETURN
        AppSWShed%lBudRawFile_Defined = .TRUE.
        RETURN
    END IF

    !Initialize
    NSWShed = AppSWShed%NSWShed
                                                      
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
    Header%cBudgetDescriptor = 'small watersheds budget'

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    ALLOCATE (Header%Areas(NSWShed))
    Header%NAreas = NSWShed
    Header%Areas  = AppSWShed%SmallWatersheds%Area

    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1) = ArrangeText('IWFM (v'//TRIM(cIWFMVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2) = ArrangeText('SMALL WATERSHED FLOW COMPONENTS IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3) = ArrangeText('WATERSHED AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4) = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(5) = REPEAT(' ',73)//'Root Zone'//REPEAT(' ',97)//'Groundwater'//REPEAT(' ',58)//'Surface Outflow'
        pASCIIOutput%cTitles(6) = REPEAT(' ',17)//REPEAT('-',120)//REPEAT(' ',3)//REPEAT('-',90)//REPEAT(' ',3)//REPEAT('-',45)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,8(F14.1,1X),3X,6(F14.1,1X),3X,3(F14.1,1X),3X,1(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    Header%NLocations = NSWShed
    ALLOCATE (Header%cLocationNames(NSWShed))
    DO indx=1,NSWShed
        Header%cLocationNames(indx) = 'WATERSHED '//TRIM(IntToText(iSWShedIDs(indx))) 
    END DO
    
    !Locations
    ALLOCATE (Header%Locations(1)                                                          , &
              Header%Locations(1)%cFullColumnHeaders(NColumns+1)                           , &
              Header%Locations(1)%iDataColumnTypes(NColumns)                               , &
              Header%Locations(1)%iColWidth(NColumns+1)                                    , &
              Header%Locations(1)%cColumnHeaders(NColumns+1,NColumnHeaderLines)            , &
              Header%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )  
    ASSOCIATE (pLocation => Header%Locations(1))
      pLocation%NDataColumns           = NColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                            
      pLocation%cFullColumnHeaders(2:) = f_cBudgetColumnTitles                            
      pLocation%iDataColumnTypes       = [f_iVR  , &  !Precip
                                          f_iVR  , &  !Runoff
                                          f_iVLB , &  !Root zone beginning storage
                                          f_iVR  , &  !Infiltration
                                          f_iVR  , &  !ET
                                          f_iVR  , &  !Root zone percolation
                                          f_iVLE , &  !Ending storage
                                          f_iVR  , &  !Root zone discrepancy
                                          f_iVLB , &  !GW beginning storage
                                          f_iVR  , &  !Deep perc as recharge to GW
                                          f_iVR  , &  !Base flow
                                          f_iVR  , &  !GW return flow
                                          f_iVLE , &  !GW ending storage
                                          f_iVR  , &  !GW discrepancy
                                          f_iVR  , &  !Total surface outflow
                                          f_iVR  , &  !Percolation to GW
                                          f_iVR  , &  !Net surface outflow
                                          f_iVR  ]   !Total inflow to GW
      pLocation%iColWidth              = [17,(15,I=1,NColumns)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        pColumnHeaders(:,1) = (/'                 ','               ','               ','     Beginning ','               ','         Actual','               ','        Ending ','               ','     Beginning ','               ','         Base  ','       GW      ','       Ending  ','               ','  Total Surface','    Percolation','     Net Stream','      Total GW '/)
        pColumnHeaders(:,2) = (/'      Time       ','  Precipitation','         Runoff','      Storage  ','   Infiltration','           ET  ','    Percolation','        Storage','    Discrepancy','      Storage  ','     Recharge  ','         Flow  ','   Return Flow ','       Storage ','    Discrepancy','     Outflow   ','       to GW   ','       Inflow  ','       Inflow  '/)
        pColumnHeaders(:,3) = (/      TextTime     ,'               ','               ','        (+)    ','        (+)    ','           (-) ','       (-)     ','          (-)  ','        (=)    ','        (+)    ','        (+)    ','         (-)   ','       (-)     ','         (-)   ','        (=)    ','       (+)     ','        (-)    ','         (=)   ','               '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,8A15,3X,6A15,3X,3A15,3X,1A15)'
        pFormatSpecs(2)     = '(A17,8A15,3X,6A15,3X,3A15,3X,1A15)'
        pFormatSpecs(3)     = '(A17,8A15,3X,6A15,3X,3A15,3X,1A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NColumns*NSWShed) , pDSSOutput%iDataTypes(1))
      iCount = 1
      DO indxLocation=1,NSWShed
        DO indxCol=1,NColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_SWSHED_BUD/'                                              //  &  !A part
                                          'WSHED_'//TRIM(IntToText(iSWShedIDs(indxLocation)))//'/'         //  &  !B part
                                          'VOLUME/'                                                        //  &  !C part
                                          '/'                                                              //  &  !D part
                                          TRIM(TimeStep%Unit)//'/'                                         //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                              !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = f_iPER_CUM
    END ASSOCIATE
    
    !Instantiate the small stream budget raw file and set the flag
    CALL AppSWShed%BudRawFile%New(TRIM(CFileName),Header,iStat)
    IF (iStat .EQ. -1) RETURN
    AppSWShed%lBudRawFile_Defined = .TRUE.
    
    !Clear memory
    CALL Header%Kill()

  END SUBROUTINE InitSmallWatershedBudRawFile
  
  
  
  
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
  ! --- KILL AppSmallWatershed OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppSWShed)
    CLASS(AppSmallWatershedType) :: AppSWShed
    
    !Local variables
    INTEGER              :: ErrorCode
    TYPE(SolverDataType) :: DummySolverData
    
    !Deallocate arrays
    DEALLOCATE (AppSWShed%SmallWatersheds ,STAT=ErrorCode)
    
    !Close files
    IF (AppSWShed%lBudRawFile_Defined) CALL AppSWShed%BudRawFile%Kill()
    IF (AppSWShed%FinResultsFile%iGetFileType() .NE. f_iUNKNOWN) CALL AppSWShed%FinResultsFile%Kill()
    
    !Set the attributes to their defaults
    AppSWShed%VarTimeUnit         = ''
    AppSWShed%NSWShed             = 0
    APpSWShed%lDefined            = .FALSE.
    AppSWShed%RZSolverData        = DummySolverData
    AppSWShed%lBudRawFile_Defined = .FALSE.
    
    
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
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(AppSWShed,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(AppSmallWatershedType),INTENT(IN)  :: AppSWShed
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    !Local variables
    INTEGER                  :: iErrorCode
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    !Initialize
    DEALLOCATE (iBudgetTypeList , iBudgetLocationTypeList , cBudgetDescriptions , cBudgetFiles , STAT=iErrorCode)
         
    !Get the list if there is a Budget generated
    IF (AppSWShed%lBudRawFile_Defined) THEN
        ALLOCATE (iBudgetTypeList(1) , iBudgetLocationTypeList(1) , cBudgetDescriptions(1) , cBudgetFiles(1))
        CALL AppSWShed%BudRawFile%GetFileName(cFileName)
        cBudgetFiles(1)            = cFileName
        iBudgetTypeList(1)         = f_iBudgetType_SWShed
        iBudgetLocationTypeList(1) = f_iLocationType_SmallWatershed
        cBudgetDescriptions(1)     = f_cDescription_SWShedBudget
    ELSE
        ALLOCATE (iBudgetTypeList(0) , iBudgetLocationTypeList(0) , cBudgetDescriptions(0) , cBudgetFiles(0))
    END IF
     
  END SUBROUTINE GetBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BUDGET FILE COLUMNS (EXCLUDES TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(AppSWShed,iLocationIndex,iNCols,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iLocationIndex          
    INTEGER,INTENT(OUT)                     :: iNCols,iStat       
    
    IF (AppSWShed%lBudRawFile_Defined) THEN
        CALL AppSWShed%BudRawFile%GetNDataColumns(iLocationIndex,iNCols,iStat)    !Includes Time column
        iNCols = iNCols - 1  !Exclude Time column
    ELSE
        iStat  = 0
        iNCols = 0
    END IF
        
  END SUBROUTINE GetBudget_NColumns
     
     
  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(AppSWShed,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN)  :: AppSWShed
    INTEGER,INTENT(IN)                       :: iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER                                       :: iNCols,iErrorCode
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    IF (AppSWShed%lBudRawFile_Defined) THEN
        !Get number of columns (includes Time column)
        CALL AppSWShed%BudRawFile%GetNDataColumns(iLocationIndex,iNCols,iStat)  
        IF (iStat .NE. 0) RETURN
        
        !Get column titles (includes (Time column)
        ALLOCATE (cColTitles_Local(iNCols))
        cColTitles_Local = AppSWShed%BudRawFile%GetFullColumnHeaders(iLocationIndex,iNCols)
        
        !Insert units
        CALL AppSWShed%BudRawFile%ModifyFullColumnHeaders(cUnitLT,cUnitAR,cUnitVL,cColTitles_Local)
        
        !Remove time column
        iNCols = iNCols - 1
        ALLOCATE (cColTitles(iNCols))
        cColTitles = ADJUSTL(cColTitles_Local(2:))
        
        !Clear memory
        DEALLOCATE (cColTitles_Local , STAT=iErrorCode)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    END IF
     
  END SUBROUTINE GetBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM AppSmallWaterhded OBJECT FOR A SPECIFED WATERSHED
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenAppSmallWatershed(AppSWShed,iBudCompType,iSWShedIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN)  :: AppSWShed
    INTEGER,INTENT(IN)                       :: iBudCompType,iSWShedIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER :: ID
    
    IF (AppSWShed%lBudRawFile_Defined) THEN
        ID = AppSWShed%SmallWatersheds(iSWShedIndex)%ID
        CALL GetBudget_MonthlyFlows_GivenFile(AppSWShed%BudRawFile,iBudCompType,ID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)  
    ELSE
        iStat = 0
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
    END IF
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenAppSmallWatershed

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE FOR A SPECIFED WATERSHED
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iBudCompType,iSWShedID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    INTEGER,INTENT(IN)                       :: iBudCompType,iSWShedID
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER,PARAMETER   :: f_iReadColsRZ(5) = [3,4,5,6,7]     , &
                           f_iReadColsGW(5) = [9,10,11,12,13]
    INTEGER             :: iDimActual,iNTimeSteps,iReadCols(5)
    REAL(8),ALLOCATABLE :: rValues(:,:)
    
    !Get simulation time steps and allocate array to read data
    iNTimeSteps = Budget%GetNTimeSteps()
    ALLOCATE (rValues(6,iNTimeSteps)) !Adding 1 to the first dimension for Time column; it will be removed later
    
    !Columns to read
    IF (iBudCompType .EQ. f_iSWShedBudComp_RZ) THEN
        iReadCols = f_iReadColsRZ
    ELSEIF (iBudCompType .EQ. f_iSWShedBudComp_GW) THEN
        iReadCols = f_iReadColsGW
    END IF
    
    !Read data
    CALL Budget%ReadData(iSWShedID,iReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Store values in return argument
    IF (iBudCompType .EQ. f_iSWShedBudComp_RZ) THEN
        ALLOCATE (rFlows(4,iDimActual) , cFlowNames(4))
        rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(6,1:iDimActual)   !Change in storage
        rFlows(2,:)  = rValues(3,1:iDimActual)                             !Infiltration       
        rFlows(3,:)  = -rValues(4,1:iDimActual)                            !Actual ET       
        rFlows(4,:)  = -rValues(5,1:iDimActual)                            !Deep Percolation               
        !Flow names
        cFlowNames     = ''
        cFlowNames(1)  = 'Change in Storage'      
        cFlowNames(2)  = 'Infiltration'       
        cFlowNames(3)  = 'Actual ET'       
        cFlowNames(4)  = 'Deep Percolation'       
    ELSE  
        ALLOCATE (rFlows(4,iDimActual) , cFlowNames(4))
        rFlows(1,:)  = rValues(2,1:iDimActual) - rValues(6,1:iDimActual)   !Change in storage
        rFlows(2,:)  = rValues(3,1:iDimActual)                             !Recharge       
        rFlows(3,:)  = -rValues(4,1:iDimActual)                            !Base Flow       
        rFlows(4,:)  = -rValues(5,1:iDimActual)                            !GW Return Flow               
        !Flow names
        cFlowNames     = ''
        cFlowNames(1)  = 'Change in Storage'      
        cFlowNames(2)  = 'Recharge'       
        cFlowNames(3)  = 'Base Flow'       
        cFlowNames(4)  = 'GW Return Flow'       
    END IF    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AppSWShed,iSWShedIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iSWShedIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)             :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                      :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                     :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                     :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER :: indx,ID
    
    IF (AppSWShed%lBudRawFile_Defined) THEN
        !Read data
        ID = AppSWShed%Smallwatersheds(iSWShedIndex)%ID
        DO indx=1,SIZE(iCols)
            CALL AppSWShed%BudRawFile%ReadData(ID,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
        END DO
    ELSE
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
    END IF
    
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SMALL WATERSHEDS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNSmallWatersheds(AppSWShed) RESULT(NSWShed)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER                                 :: NSWShed
    
    NSWShed = AppSWShed%NSWShed

  END FUNCTION GetNSmallWatersheds
  
  
  ! -------------------------------------------------------------
  ! --- GET SMALL WATERSHED IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSmallWatershedIDs(AppSWShed,IDs)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(OUT)                     :: IDs(:)
    
    IDs = AppSWShed%SmallWatersheds%ID

  END SUBROUTINE GetSmallWatershedIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FOR ONE SMALL WATERSHED (PERCOLATION WITHIN THE SMALL WATERSHED)
  ! -------------------------------------------------------------
  PURE FUNCTION GetRootZonePerc_ForOneSmallWatershed(AppSWShed,iSWShed) RESULT(Perc)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iSWShed
    REAL(8)                                 :: Perc
    
    Perc = AppSWShed%SmallWatersheds(iSWShed)%RootZonePerc
    
  END FUNCTION GetRootZonePerc_ForOneSmallWatershed
  
    
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FOR ALL SMALL WATERSHEDS (PERCOLATION WITHIN THE SMALL WATERSHED)
  ! -------------------------------------------------------------
  SUBROUTINE GetRootZonePerc_ForAllSmallWatersheds(AppSWShed,Perc)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    REAL(8),INTENT(OUT)                     :: Perc(AppSWShed%NSWShed)
    
    Perc = AppSWShed%SmallWatersheds%RootZonePerc
    
  END SUBROUTINE GetRootZonePerc_ForAllSmallWatersheds
  
    
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION FLOWS FOR ALL SMALL WATERSHEDS
  ! -------------------------------------------------------------
  SUBROUTINE GetPercFlow_ForAllSmallWatersheds(AppSWShed,PercFlows)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    REAL(8),INTENT(OUT)                     :: PercFlows(AppSWShed%NSWShed)
    
    !Local variables
    INTEGER :: indxSWShed
    
    DO indxSWShed=1,AppSWShed%NSWShed
        PercFlows(indxSWShed) = SUM(AppSWShed%SmallWatersheds(indxSWShed)%PercFlowNodes%Flow)
    END DO
    
  END SUBROUTINE GetPercFlow_ForAllSmallWatersheds
  
    
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL INFLOWS
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalGWInflows(AppSWShed,AppGrid) RESULT(RInflows)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    REAL(8)                                 :: RInflows(AppGrid%NSubregions)
    
    !Local variables
    INTEGER :: iNode,indxElem,iElem,indxVertex,iSubregion,indxLayer,indxNode
    REAL(8) :: rFlow
    LOGICAL :: lDummy
    
    !Initialize
    RInflows = 0.0
    
    !Return if small watersheds are not modeled
    IF (AppSWShed%NSWShed .EQ. 0) RETURN
    
    ASSOCIATE (pBaseFlowNodeList => AppSWShed%BaseFlowNodeList , &
               pPercFlowNodeList => AppSWShed%PercFlowNodeList , &
               pAppNodes         => AppGrid%AppNode            )
        DO indxLayer=1,SIZE(pBaseFlowNodeList)
            !Accumulate base flows
            DO indxNode=1,pBaseFlowNodeList(indxLayer)%NNodes
                iNode = pBaseFlowNodeList(indxLayer)%Nodes(indxNode)
                DO indxElem=1,SIZE(pAppNodes(iNode)%SurroundingElement)
                    iElem      = pAppNodes(iNode)%SurroundingElement(indxElem)
                    indxVertex = LocateInList(iNode,AppGrid%Vertex(:,iElem))
                    iSubregion = AppGrid%AppElement(iElem)%Subregion
                    CALL AppSWShed%GetBoundaryFlowAtElementNodeLayer(SWShedBaseFlowBCID,iElem,indxVertex,indxLayer,AppGrid,rFlow,lDummy)
                    RInflows(iSubregion) = RInflows(iSubregion) + rFlow
                END DO
            END DO
            
            !Accumulate percolation
            DO indxNode=1,pPercFlowNodeList(indxLayer)%NNodes
                iNode = pPercFlowNodeList(indxLayer)%Nodes(indxNode)
                DO indxElem=1,SIZE(pAppNodes(iNode)%SurroundingElement)
                    iElem      = pAppNodes(iNode)%SurroundingElement(indxElem)
                    indxVertex = LocateInList(iNode,AppGrid%Vertex(:,iElem))
                    iSubregion = AppGrid%AppElement(iElem)%Subregion
                    CALL AppSWShed%GetBoundaryFlowAtElementNodeLayer(SWShedPercFlowBCID,iElem,indxVertex,indxLayer,AppGrid,rFlow,lDummy)
                    IF (rFlow .EQ. 0.0) EXIT
                    RInflows(iSubregion) = RInflows(iSubregion) + rFlow
                END DO
            END DO

        END DO
    END ASSOCIATE
                       
  END FUNCTION GetSubregionalGWInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET NET INFLOW INTO STREAM NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetStreamInflows(AppSWShed,QTRIB) 
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    REAL(8),INTENT(OUT)                     :: QTRIB(:)
    
    !Local variables
    INTEGER :: indxSWShed,indx
    
    !Initialize
    QTRIB = 0.0
    
    !Process
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%NSWShed
            indx = pSWSheds(indxSWShed)%StrmNode
            IF (indx .EQ. 0) CYCLE
            QTRIB(indx) = QTRIB(indx) + pSWSheds(indxSWShed)%NetStreamInflow
        END DO
    END ASSOCIATE
    
  END SUBROUTINE GetStreamInflows 
  
  
  ! -------------------------------------------------------------
  ! --- GET NODES WITH BASE FLOW AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetNodesWithBCType(AppSWShed,iLayer,iBCType,iBCNodes)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iLayer,iBCType
    INTEGER,ALLOCATABLE,INTENT(OUT)         :: iBCNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (iBCNodes , STAT=ErrorCode)
    
    SELECT CASE (iBCType)
        CASE (SWShedBaseFlowBCID)
            ALLOCATE (iBCNodes(AppSWShed%BaseFlowNodeList(iLayer)%NNodes))
            iBCNodes = AppSWShed%BaseFlowNodeList(iLayer)%Nodes
            
        CASE (SWShedPercFlowBCID)
            ALLOCATE (iBCNodes(AppSWShed%PercFlowNodeList(iLayer)%NNodes))
            iBCNodes = AppSWShed%PercFlowNodeList(iLayer)%Nodes
    END SELECT
        
  END SUBROUTINE GetNodesWithBCType
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FACE FLOW AT A LAYER
  ! --- NOte: It is assumed that iFace is a boundary face
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtFaceLayer_AtOneFace(AppSWShed,AppGrid,iFace,iLayer,rFlow)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    INTEGER,INTENT(IN)                      :: iFace,iLayer
    REAL(8),INTENT(OUT)                     :: rFlow
    
    !Local variables
    INTEGER :: iNodes(2),indxNode,iNode,iElem,iVertex,Vertex(4)
    REAL(8) :: rFlowTemp
    LOGICAL :: lDummy
    
    !Initialize
    rFlow  = 0.0
    iNodes = AppGrid%AppFace%Node(:,iFace)
    iElem  = MAXVAL(AppGrid%AppFace%Element(:,iFace))
    Vertex = AppGrid%Vertex(:,iElem)
    
    !Return if baseflow from small watersheds is not simulated
    IF (.NOT. AppSWShed%IsBaseFlowSimulated()) RETURN
    
    !Find flows for each node
    DO indxNode=1,2
        iNode = iNodes(indxNode)
        
        !Vertex index for the element
        iVertex = LocateInList(iNode,Vertex)
        
        !Base flow
        CALL GetBoundaryFlowAtElementNodeLayer(AppSWShed,SWShedBaseFlowBCID,iElem,iVertex,iLayer,AppGrid,rFlowTemp,lDummy)
        rFlow = rFlow - rFlowTemp
        
        !Percolation
        !This type of b.c. is assumed to flow into system vertically so it won't have a horizontal flow component
        
    END DO

  END SUBROUTINE GetBoundaryFlowAtFaceLayer_AtOneFace
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FACE FLOW AT A LAYER
  ! --- NOte: It is assumed that iFace is a boundary face
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtFaceLayer_AtSomeFaces(AppSWShed,AppGrid,iFaces,iLayers,rFlows)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    INTEGER,INTENT(IN)                      :: iFaces(:),iLayers(:)
    REAL(8),INTENT(OUT)                     :: rFlows(:)
    
    !Local variables
    INTEGER :: iNodes(2),indxNode,iNode,iElem,iVertex,indxFace,Vertex(4),iFace
    REAL(8) :: rFlowTemp
    LOGICAL :: lDummy
    
    !Initialize
    rFlows = 0.0
    
    !Return if baseflow from small watersheds is not simulated
    IF (.NOT. AppSWShed%IsBaseFlowSimulated()) RETURN
    
    DO indxFace=1,SIZE(iFaces)
        iFace  = iFaces(indxFace)
        iNodes = AppGrid%AppFace%Node(:,iFace)
        iElem  = MAXVAL(AppGrid%AppFace%Element(:,iFace))
        Vertex = AppGrid%Vertex(:,iElem)
    
        !Find flows for each node
        DO indxNode=1,2
            iNode = iNodes(indxNode)
        
            !Vertex index for the element
            iVertex = LocateInList(iNode,Vertex)
        
            !Base flow
            CALL GetBoundaryFlowAtElementNodeLayer(AppSWShed,SWShedBaseFlowBCID,iElem,iVertex,iLayers(indxFace),AppGrid,rFlowTemp,lDummy)
            rFlows(indxFace) = rFlows(indxFace) - rFlowTemp
        
            !Percolation
            !This type of b.c. is assumed to flow into system vertically so it won't have a horizontal flow component
        
        END DO
    END DO
    
  END SUBROUTINE GetBoundaryFlowAtFaceLayer_AtSomeFaces
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FLOW TO ELEMENT AT ITS VERTEX WITH DEFINED FLOW TYPE AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtElementNodeLayer(AppSWShed,iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iBCType,iElem,indxVertex,iLayer
    TYPE(AppGridType),INTENT(IN)            :: AppGrid
    REAL(8),INTENT(OUT)                     :: rFlow
    LOGICAL,INTENT(OUT)                     :: lAddToRHS
    
    !Local variables
    INTEGER :: iNode,NFaceID,iRHSRow
    REAL(8) :: rL1,rLn
    
    !Initialize
    iNode     = AppGrid%Vertex(indxVertex,iElem)
    iRHSRow   = LocateInList(iElem,AppGrid%AppNode(iNode)%ElemID_OnCCWSide)  !Locate the row number for node RHS that correspond to element
    NFaceID   = AppGrid%AppNode(iNode)%NFaceID
    rFlow     = AppSWShed%GetNetBCFlowWithBCType(iNode,iLayer,iBCType)
    lAddToRHS = .TRUE.

    !If it is percolation from small watersheds it is always distributed among elements
    IF (iBCType .EQ. SWShedPercFlowBCID) THEN
      rFlow = rFlow * AppGrid%AppElement(iElem)%VertexArea(indxVertex) / AppGrid%AppNode(iNode)%Area
      RETURN
    END IF

    IF (rFlow .NE. 0.0) THEN
        IF (AppGrid%AppNode(iNode)%BoundaryNode) THEN
            lAddToRHS = .FALSE.
            
            !If NFaceID is 2 (i.e. a boundary node that has only 1 element surrounding it, then the boundary flow to element is the full boundary flow
            IF (NFaceID .EQ. 2) RETURN
            
            !Initialize
            rL1 = AppGrid%AppNode(iNode)%IrrotationalCoeff(NFaceID)  !This is the length of the first element face that is associtaed with the boundary flow
            rLn = AppGrid%AppNode(iNode)%IrrotationalCoeff(1)        !This is the length of the last element face that is associated with the boundary flow
            
            !If indx is 1 then this is one of the boundary faces with the unknown boundary flow as inflow
            IF (iRHSRow .EQ. 1) THEN
                rFlow = rFlow * rL1 / (rL1+rLn)
            
            !If indx is NFaceID-1 then this is one of the boundary faces with the unknown boundary flow as outflow
            ELSEIF (iRHSRow .EQ. NFaceID-1) THEN
                rFlow = rFlow * rLn / (rL1+rLn)
           
            !Otherwise, this is not a boundary face and the boundary flow does not cross it
            ELSE
                rFlow = 0.0
            END IF            
        ELSE        
            rFlow = rFlow * AppGrid%AppElement(iElem)%VertexArea(indxVertex) / AppGrid%AppNode(iNode)%Area
        END IF
    ELSE
        lAddToRHS = .FALSE.
    END IF

  END SUBROUTINE GetBoundaryFlowAtElementNodeLayer
  

  ! -------------------------------------------------------------
  ! --- GET NET B.C. FLOW AT A NODE WITH A SPECIFIED FLOW TYPE
  ! -------------------------------------------------------------
  FUNCTION GetNetBCFlowWithBCType(AppSWShed,Node,Layer,iBCType) RESULT(Flow)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: Node,Layer,iBCType
    REAL(8)                                 :: Flow
    
    !Local variables
    INTEGER :: indx,indxNode
    
    !Initialize
    Flow = 0.0
    
    ASSOCIATE (pSWSheds => AppSWSHed%SmallWatersheds)       
        SELECT CASE (iBCType)
            
          !Small watershed base flow
          CASE (SWShedBaseFlowBCID)
            DO indx=1,AppSWShed%NSWShed
                DO indxNode=1,pSWSheds(indx)%NBaseFlowNodes
                    IF (pSWSheds(indx)%BaseFlowNodes(indxNode)%Node .EQ. Node) THEN
                        IF (pSWSheds(indx)%BaseFlowNodes(indxNode)%Layer .EQ. Layer)   &
                            Flow = Flow + pSWSheds(indx)%BaseFlowNodes(indxNode)%Flow
                    END IF
                END DO
            END DO
            
          !Small watershed percolation
          CASE (SWShedPercFlowBCID)
            DO indx=1,AppSWShed%NSWShed
                DO indxNode=1,pSWSheds(indx)%NPercFlowNodes
                    IF (pSWSheds(indx)%PercFlowNodes(indxNode)%Node .EQ. Node) THEN
                      IF (pSWSheds(indx)%PercFlowNodes(indxNode)%Layer .EQ. Layer)   &
                          Flow = Flow + pSWSheds(indx)%PercFlowNodes(indxNode)%Flow
                    END IF
                END DO
            END DO

        END SELECT
    END ASSOCIATE  
    
  END FUNCTION GetNetBCFlowWithBCType

  
  
  
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
  SUBROUTINE ReadRestartData(AppSWShed,InFile,iStat)
    CLASS(AppSmallWatershedType) :: AppSWShed
    TYPE(GenericFileType)        :: InFile
    INTEGER,INTENT(OUT)          :: iStat
    
    CALL InFile%ReadData(AppSWShed%SmallWatersheds%SoilMoist,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSWShed%SmallWatersheds%SoilMoist_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSWShed%SmallWatersheds%GWStor,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSWShed%SmallWatersheds%GWStor_P,iStat)  
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AppSWShed,Precip,ET)
    CLASS(AppSmallWatershedType)       :: AppSWShed
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ET
    
    AppSWShed%SmallWatersheds%Precip = Precip%GetValues(AppSWShed%SmallWatersheds%iColPrecip) * AppSWShed%SmallWatersheds%PrecipFactor
    AppSWShed%SmallWatersheds%ETc    = ET%GetValues(AppSWShed%SmallWatersheds%iColET)

  END SUBROUTINE ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- READ GEOSPATIAL DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadGeospatialData(InFile,TimeStep,AppGrid,Stratigraphy,NStrmNodes,iStrmNodeIDs,AppSWShed,iStat)
    TYPE(GenericFileType)             :: InFile
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes)
    TYPE(AppSmallWatershedType)       :: AppSWShed
    INTEGER,INTENT(OUT)               :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'ReadGeospatialData'
    REAL(8)                      :: FactArea,FactQ,DummyArray(6),QSUM
    CHARACTER                    :: ALine*500
    INTEGER                      :: indxSWShed,NGWNodes,ErrorCode,iCount,NNode,indx,iBaseFlowNode,iBaseFlowLayer,  &
                                    iPercNode,iPercLayer,iSWShedID,iGWNodeIDs(AppGrid%NNodes),iStrmNodeID
    REAL(8),ALLOCATABLE          :: QMaxPerc(:),Dummy2DArray(:,:)
    INTEGER,ALLOCATABLE          :: iGWNodes(:),iGWNodeIDs_SWShed(:)
    
    !Initialize
    iGWNodeIDs = AppGrid%AppNode%ID
    
    !Conversion factors
    CALL InFile%ReadData(FactArea,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactQ,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)     ;  IF (iStat .EQ. -1) RETURN  ;  Aline = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    AppSWShed%VarTimeUnit = TRIM(ADJUSTL(ALine))
    
    !Make sure time unit is recognized
    IF (TimeStep%TrackTime) THEN
        IF (IsTimeIntervalValid(AppSWShed%VarTimeUnit) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for maximum recharge rate for small watersheds is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Read geospatial parameters
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)        
        DO indxSWShed=1,AppSWShed%NSWShed
            CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
            
            iSWShedID                 = INT(DummyArray(1))
            pSWSheds(indxSWShed)%ID   = iSWShedID
            pSWSheds(indxSWShed)%Area = DummyArray(2) * FactArea
            
            !Make sure destination stream node is modeled
            iStrmNodeID = INT(DummyArray(3))
            IF (iStrmNodeID .GT. 0) THEN
                CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,pSWSheds(indxSWShed)%StrmNode)
                IF (pSWSheds(indxSWShed)%StrmNode .EQ. 0) THEN
                    MessageArray(1) = 'Stream node number '//TRIM(IntToText(iStrmNodeID))//' where the surface flow from small'
                    MessageArray(2) = 'watershed '//TRIM(IntToText(iSWShedID))//' flows into is not in the model!'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            ELSE
                pSWSheds(indxSWShed)%StrmNode = iStrmNodeID
            END IF
            
            !Make sure same ID number is not used
            DO indx=1,indxSWShed-1
                IF (iSWShedID .EQ. pSWSheds(indx)%ID) THEN
                    CALL SetLastMessage('Small watershed ID number '//TRIM(IntToText(iSWShedID))//' is used more than once!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
            
            !Destination groundwater nodes
            NGWNodes = DummyArray(4)
            DEALLOCATE (iGWNodes , iGWNodeIDs_SWShed , QMaxPerc , Dummy2DArray , STAT=ErrorCode)
            ALLOCATE (iGWNodes(NGWNodes) , iGWNodeIDs_SWShed(NGWNodes) , QMaxPerc(NGWNodes) , Dummy2DArray(NGWNodes-1,2))
            iGWNodeIDs_SWShed(1) = DummyArray(5)
            QMaxPerc(1)          = DummyArray(6)
            CALL InFile%ReadData(Dummy2DArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
            iGWNodeIDs_SWShed(2:) = Dummy2DArray(:,1)
            QMaxPerc(2:)          = Dummy2DArray(:,2)
            QSUM                  = 0.0
            CALL ConvertID_To_Index(IGWNodeIDs_SWShed,iGWNodeIDs,iGWNodes)
            IF (ANY(iGWNodes.EQ.0)) THEN
                CALL SetLastMessage('One or more groundwater nodes listed for small watershed '//TRIM(IntToText(iSWShedID))//' are not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            DO iCount=1,NGWNodes
               IF (QMaxPerc(iCount) .GE. 0.0) THEN
                   QMaxPerc(iCount) = QMaxPerc(iCount) * FactQ
               ELSE
                   QSUM             = QSUM+1.0
               END IF
           END DO

           !Compile information for the gw nodes that receive the baseflow from small watershed
           IF (QSUM .GT. 0.0) THEN
             NNode                               = INT(QSUM)
             pSWSheds(indxSWShed)%NBaseFlowNodes = NNode
             ALLOCATE (pSWSheds(indxSWShed)%BaseFlowNodes(NNode))
             iCount = 0
             DO indx=1,NGWNodes
               IF (QMaxPerc(indx) .GE. 0.0) CYCLE
               iCount                                           = iCount + 1
               iBaseFlowNode                                    = iGWNodes(indx)
               iBaseFlowLayer                                   = ABS(QMaxPerc(indx))
               pSWSheds(indxSWShed)%BaseFlowNodes(iCount)%Node  = iBaseFlowNode
               pSWSheds(indxSWShed)%BaseFlowNodes(iCount)%Layer = iBaseFlowLayer
               pSWSheds(indxSWShed)%BaseFlowNodes(iCount)%Frac  = 1d0/QSUM
               !Make sure that the node is active
               IF (Stratigraphy%ActiveNode(iBaseFlowNode,iBaseFlowLayer) .EQ. .FALSE.) THEN
                   MessageArray(1) = 'GW node '//TRIM(IntToText(iGWNodeIDs_SWShed(indx)))//' that receives baseflow from '
                   MessageArray(2) = 'small watershed '//TRIM(IntToText(iSWShedID))      //  &
                                     ' at layer '//TRIM(IntToText(iBaseFlowLayer))       //  &
                                     ' is an inactive node!'
                   CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                   iStat = -1
                   RETURN
               END IF
               !Make sure that the node is on the boundary
               IF (.NOT. AppGrid%AppNode(iBaseFlowNode)%BoundaryNode) THEN
                   MessageArray(1) = 'GW node '//TRIM(IntToText(iGWNodeIDs_SWShed(indx)))//' that receives baseflow from '
                   MessageArray(2) = 'small watershed '//TRIM(IntToText(iSWShedID))      //  &
                                     ' at layer '//TRIM(IntToText(iBaseFlowLayer))       //  &
                                     ' is not on the model boundary!'
                   CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                   iStat = -1
                   RETURN
               END IF
             END DO
           END IF

           !Compile information for the gw nodes that receive percolation from small watershed
           NNode                               = COUNT(QMaxPerc .GE. 0.0)
           pSWSheds(indxSWShed)%NPercFlowNodes = NNode
           ALLOCATE (pSWSheds(indxSWShed)%PercFlowNodes(NNode))
           iCount = 0
           DO indx=1,NGWNodes
             IF (QMAxPerc(indx) .LT. 0.0) CYCLE
             iCount                                             = iCount+1
             iPercNode                                          = iGWNodes(indx)
             iPercLayer                                         = Stratigraphy%TopActiveLayer(iPercNode)
             pSWSheds(indxSWShed)%PercFlowNodes(iCount)%Node    = iPercNode
             pSWSheds(indxSWShed)%PercFlowNodes(iCount)%Layer   = iPercLayer
             pSWSheds(indxSWShed)%PercFlowNodes(iCount)%MaxFlow = QMaxPerc(indx)
           END DO

         END DO

    END ASSOCIATE
    
  END SUBROUTINE ReadGeospatialData
  
  
  ! -------------------------------------------------------------
  ! --- READ ROOT ZONE DATA
  ! --- * Note: Assumes the VarTimeUnit attribute is set previously
  ! -------------------------------------------------------------
  SUBROUTINE ReadRootZoneData(InFile,TimeStep,iSWShedIDs,AppSWShed,iStat)
    TYPE(GenericFileType)         :: InFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: iSWShedIDs(:)
    TYPE(AppSmallWaterShedType)   :: AppSWShed
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'ReadRootZoneData'
    INTEGER                      :: indxSWShed,iSWShedID,iSWShed
    REAL(8)                      :: FactLength,FactCN,FactK,DummyArray(12)
    CHARACTER                    :: ALine*500,TimeUnitK*6
    LOGICAL                      :: lProcessed(AppSWShed%NSWShed)
    
    !Initialize
    lProcessed = .FALSE.
    
    !Read solver data and conversion factors
    CALL InFile%ReadData(AppSWShed%RZSolverData%Tolerance,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSWShed%RZSolverData%IterMax,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactLength,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactCN,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactK,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  Aline = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    TimeUnitK = TRIM(ADJUSTL(ALine))
    
    !Make sure time unit is recognized
    IF (TimeStep%TrackTime) THEN
        IF (IsTimeIntervalValid(TimeUnitK) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for small watershed root zone hydraulic conductivity is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Convert time unit of soil hydraulic conductivity to the time unit stored in AppSWShed%VarTimeUnit
    FactK = FactK * TimeIntervalConversion(AppSWShed%VarTimeUnit,TimeUnitK) 
    
    !Read and process root zone parameters
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%NSWShed
            CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
            iSWShedID = INT(DummyArray(1))
            
            !Make sure small watershed ID is legit and has not been already processed
            CALL ConvertID_To_Index(iSWShedID,iSWShedIDs,iSWShed)
            IF (iSWShed .EQ. 0) THEN
                CALL SetLastMessage('Small watershed ID '//TRIM(IntToText(iSWShedID))//' listed for root zone parameter definition is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (lProcessed(iSWShed)) THEN
                CALL SetLastMessage('Small watershed ID '//TRIM(IntToText(iSWShedID))//' is listed more than once for root zone parameter definitions!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iSWShed) = .TRUE.

            pSWSheds(iSWShed)%iColPrecip         = DummyArray(2)
            pSWSheds(iSWShed)%PrecipFactor       = DummyArray(3)
            pSWSheds(iSWShed)%iColET             = DummyArray(4)
            pSWSheds(iSWShed)%Soil%WiltingPoint  = DummyArray(5)
            pSWSheds(iSWShed)%Soil%FieldCapacity = DummyArray(6)
            pSWSheds(iSWShed)%Soil%TotalPorosity = DummyArray(7)
            pSWSheds(iSWShed)%Soil%Lambda        = DummyArray(8)
            pSWSheds(iSWShed)%RootDepth          = DummyArray(9) * FactLength
            pSWSheds(iSWShed)%Soil%HydCond       = DummyArray(10) * FactK * TimeStep%DeltaT
            pSWSheds(iSWShed)%Soil%KunsatMethod  = DummyArray(11)
            pSWSheds(iSWShed)%SMax               = (1000.0/DummyArray(12)-10.0) * FactCN
            
            !Total porosity should be less than 1.0
            IF (pSWSheds(iSWShed)%Soil%TotalPorosity .GT. 1.0) THEN
                CALL SetLastMessage('At small watershed ' // TRIM(IntToText(iSWShedID)) // ' total porosity is greater than 1.0!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Wilting point should be less than field capacity
            IF (pSWSheds(iSWShed)%Soil%WiltingPoint .GE. pSWSheds(iSWShed)%Soil%FieldCapacity) THEN
                CALL SetLastMessage('At small watershed ' // TRIM(IntToText(iSWShedID)) // ' wilting point is greater than or equal to field capacity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Field capacity should be less than or equal to total porosity
            IF (pSWSheds(iSWShed)%Soil%FieldCapacity .GT. pSWSheds(iSWShed)%Soil%TotalPorosity) THEN
                CALL SetLastMessage('At small watershed ' // TRIM(IntToText(iSWShedID)) // ' field capacity is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Convert L/L soil parameters to depth
            pSWSheds(iSWShed)%Soil%WiltingPoint  = pSWSheds(iSWShed)%Soil%WiltingPoint  * pSWSheds(iSWShed)%RootDepth
            pSWSheds(iSWShed)%Soil%FieldCapacity = pSWSheds(iSWShed)%Soil%FieldCapacity * pSWSheds(iSWShed)%RootDepth
            pSWSheds(iSWShed)%Soil%TotalPorosity = pSWSheds(iSWShed)%Soil%TotalPorosity * pSWSheds(iSWShed)%RootDepth
            
            !KunsatMethod must be recognized
            IF (.NOT. ANY(pSWSheds(iSWShed)%Soil%KunsatMethod .EQ. f_iKunsatMethodList)) THEN
                CALL SetLastMessage('Method to compute unsaturated hydraulic conductivity at small watershed '//TRIM(IntToText(iSWShedID))//' is not recognized!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO       
    END ASSOCIATE
    
  END SUBROUTINE ReadRootZoneData
  
  
  ! -------------------------------------------------------------
  ! --- READ AQUIFER DATA
  ! --- * Note: Assumes the VarTimeUnit attribute is set previously
  ! -------------------------------------------------------------
  SUBROUTINE ReadAquiferData(InFile,TimeStep,iSWShedIDs,AppSWShed,iStat)
    TYPE(GenericFileType)         :: InFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: iSWShedIDs(:)
    TYPE(AppSmallWaterShedType)   :: AppSWShed
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+15) :: ThisProcedure = ModName // 'ReadAquiferData'
    INTEGER                      :: indxSWShed,iSWShedID,iSWShed
    REAL(8)                      :: FactGW,FactT,DummyArray(5)
    CHARACTER                    :: ALine*1200,TimeUnitT*6
    LOGICAL                      :: lProcessed(AppSWShed%NSWShed)
    
    !Initialize
    lProcessed = .FALSE.
    
    !Read conversion factors
    CALL InFile%ReadData(FactGW,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  Aline = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    TimeUnitT = TRIM(ADJUSTL(ALine))
    
    !Make sure time unit is recognized
    IF (TimeStep%TrackTime) THEN
        IF (IsTimeIntervalValid(TimeUnitT) .EQ. 0) THEN
            CALL SetLastMessage('Time unit for small watershed recession coefficients is not valid!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Convert time unit of recession coefficients to the time unit stored in AppSWShed%VarTimeUnit
    FactT = FactT * TimeIntervalConversion(AppSWShed%VarTimeUnit,TimeUnitT)
    
    !Read and process data
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%NSWShed
            CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN

            !Make sure small watershed ID is legit and has not beed processed before
            iSWShedID = INT(DummyArray(1))
            CALL ConvertID_To_Index(iSWShedID,iSWShedIDs,iSWShed)
            IF (iSWShed .EQ. 0) THEN
                CALL SetLastMessage('Small watershed ID '//TRIM(IntToText(iSWShedID))//' listed for aquifer parameter definition is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (lProcessed(iSWShed)) THEN
                CALL SetLastMessage('Small watershed '//TRIM(IntToText(iSWShedID))//' is listed more than once for aquifer parameter definitions!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iSWShed) = .TRUE.
       
            pSWSheds(iSWShed)%GWThreshold      = DummyArray(2) * FactGW
            pSWSheds(iSWShed)%MaxGWStor        = DummyArray(3) * FactGW
            pSWSheds(iSWShed)%SurfaceFlowCoeff = DummyArray(4) * FactT
            pSWSheds(iSWShed)%BaseFlowCoeff    = DummyArray(5) * FactT

        END DO
    END ASSOCIATE
    
  END SUBROUTINE ReadAquiferData
  
  
  ! -------------------------------------------------------------
  ! --- READ INITIAL CONDITIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialConditions(InFile,iSWShedIDs,AppSWShed,iStat)
    TYPE(GenericFileType)       :: InFile
    INTEGER,INTENT(IN)          :: iSWShedIDs(:)
    TYPE(AppSmallWaterShedType) :: AppSWShed
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'ReadInitialConditions'
    REAL(8)                      :: Factor,DummyArray(3)
    INTEGER                      :: indxSWShed,iSWShedID,iSWShed
    LOGICAL                      :: lProcessed(AppSWShed%NSWShed)
    
    !Initialize
    lProcessed = .FALSE.
    
    !Conversion factor
    CALL InFile%ReadData(Factor,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read and process data
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%NSWShed           
            CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
            !Make sure small watershed ID is legit and has not yet been processed
            iSWShedID = INT(DummyArray(1))
            CALL ConvertID_To_Index(iSWShedID,iSWSHedIDs,iSWShed)
            IF (iSWShed .EQ. 0) THEN
                CALL SetLastMessage('Small watershed ID '//TRIM(IntToText(iSWShedID))//' listed for initial conditions is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (lProcessed(iSWShed)) THEN
                CALL SetLastMessage('Small watershed ID '//TRIM(IntToText(iSWShedID))//' is listed more than once for initial conditions definition!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iSWShed) = .TRUE.
            
            !Make sure that initail moistuire content is less than or equal to 1.0
            IF (DummyArray(2) .GT. 1.0   .OR.  DummyArray(2) .LT. 0.0) THEN
                CALL SetLastMessage('The initail soil moisture content at small watershed ID '//TRIM(IntToText(iSWShedID))// ' must be between 0.0 and 1.0!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            pSWSheds(iSWShed)%SoilMoist = DummyArray(2) * pSWSheds(iSWShed)%RootDepth   ;   pSWSheds(iSWShed)%SoilMoist_P = pSWSheds(iSWShed)%SoilMoist
            pSWSheds(iSWShed)%GWStor    = DummyArray(3) * Factor                        ;   pSWSheds(iSWShed)%GWStor_P    = pSWSheds(iSWShed)%GWStor
           
        END DO
    END ASSOCIATE
    
  END SUBROUTINE ReadInitialConditions



  
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
  ! --- PRINT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppSWShed,OutFile)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    TYPE(GenericFileType)                   :: OutFile
    
    CALL OutFile%WriteData(AppSWShed%SmallWatersheds%SoilMoist)
    CALL OutFile%WriteData(AppSWShed%SmallWatersheds%SoilMoist_P)
    CALL OutFile%WriteData(AppSWShed%SmallWatersheds%GWStor)
    CALL OutFile%WriteData(AppSWShed%SmallWatersheds%GWStor_P)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO PRINT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppSWShed,TimeStep,lEndOfSimulation)
    CLASS(AppSmallWatershedType)   :: AppSWShed
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    !return if no small watersheds are modeled
    IF (AppSWShed%NSWShed .EQ. 0) RETURN
    
    !Raw budget output
    IF (AppSWShed%lBudRawFile_Defined) CALL PrintBudRawFile(AppSWShed)
    
    !Final results output
    IF (lEndOfSimulation) THEN
        IF (.NOT. AppSWShed%FinResultsFile%iGetFileType() .EQ. f_iUNKNOWN)    &
            CALL PrintFinalResults(TimeStep,AppSWShed%SmallWatersheds%ID,AppSWShed%SmallWatersheds%RootDepth,AppSWShed%SmallWatersheds%SoilMoist,AppSWShed%SmallWatersheds%GWStor,AppSWShed%FinResultsFile)
    END IF
       
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT-OUT BUDGET RAW DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintBudRawFile(AppSWShed)
    TYPE(AppSmallWatershedType) :: AppSWShed
    
    !Local variables
    INTEGER                              :: indxSWShed
    REAL(8)                              :: DummyArray(f_iNBudgetCols,AppSWShed%NSWShed)
    REAL(8),DIMENSION(AppSWShed%NSWShed) :: RZBeginStor,RZEndStor,RZError,            &
                                            GWBeginStor,GWEndStor,GWError,BaseFlow,   &
                                            TotalSurfaceFlow,PercToGW,TotalGWInflow
    
    !Compile
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        
        !Beginning storage
        RZBeginStor = pSWSheds%SoilMoist_P * pSWSheds%Area
        GWBeginStor = pSWSheds%GWStor_P * pSWSheds%Area
    
        !Ending storage
        RZEndStor = pSWSheds%SoilMoist * pSWSheds%Area
        GWEndStor = pSWSheds%GWStor * pSWSheds%Area
        
        !Groundwater baseflow and percolation to groundwater
        DO indxSWShed=1,AppSWShed%NSWShed
            BaseFlow(indxSWShed) = SUM(pSWSheds(indxSWShed)%BaseFlowNodes%Flow)
            PercToGW(indxSWShed) = SUM(pSWSheds(indxSWShed)%PercFlowNodes%Flow)
        END DO
        
        !Errors
        RZError = RZBeginStor + pSWSheds%PrecipInfilt - pSWSheds%ETa - pSWSheds%RootZonePerc - RZEndStor
        GWError = GWBeginStor + pSWSheds%RootZonePerc - BaseFlow - pSWSheds%ExcessGWRunoff - GWEndStor
        
        !Total surface flow
        TotalSurfaceFlow = pSWSheds%Runoff + pSWSheds%ExcessGWRunoff
        
        !Total GW flow
        TotalGWInflow = BaseFlow + PercToGW
        
        !Output temp array
        DummyArray(1,:)  = pSWSheds%Precip * pSWSheds%Area   !Precip
        DummyArray(2,:)  = pSWSheds%Runoff                   !Runoff
        DummyArray(3,:)  = RZBeginStor                       !Root zone beginning stoarge
        DummyArray(4,:)  = pSWSheds%PrecipInfilt             !Infiltration
        DummyArray(5,:)  = pSWSheds%ETa                      !Actual ET
        DummyArray(6,:)  = pSWSheds%RootZonePerc             !Percolation
        DummyArray(7,:)  = RZEndStor                         !Root zone ending storage
        DummyArray(8,:)  = RZError                           !Root zone discrepancy
        DummyArray(9,:)  = GWBeginStor                       !GW beginning storage
        DummyArray(10,:) = pSWSheds%RootZonePerc             !Deep perc as recharge to GW
        DummyArray(11,:) = BaseFlow                          !GW base flow output to model area
        DummyArray(12,:) = pSWSheds%ExcessGWRunoff           !GW return flow
        DummyArray(13,:) = GWEndStor                         !GW ending storage
        DummyArray(14,:) = GWError                           !GW discrepancy
        DummyArray(15,:) = TotalSurfaceFlow                  !Total surface flow
        DummyArray(16,:) = PercToGW                          !Percolation to GW
        DummyArray(17,:) = pSWSheds%NetStreamInflow          !Net inflow to modeled streams
        DummyArray(18,:) = TotalGWInflow                     !Total inflow to GW in model domain
    END ASSOCIATE
    
    !Print out data
    CALL AppSWShed%BudRawFile%WriteData(DummyArray)

  END SUBROUTINE PrintBudRawFile 
  
  
  ! -------------------------------------------------------------
  ! --- PRINT END-OF-SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalResults(TimeStep,iSWShedIDs,RootDepth,SoilMoist,GWStor,OutFile)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: iSWShedIDs(:)
    REAL(8),INTENT(IN)            :: RootDepth(:),SoilMoist(:),GWStor(:)
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER   :: indxSWShed
    CHARACTER :: SimulationTime*21,Text*40
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print header
    CALL OutFile%WriteData('C'//REPEAT('*',100))
    CALL OutFile%WriteData('C ***** SMALL WATERSHED ROOT ZONE MOISTURE AND GROUNDWATER STORAGE AT '//TRIM(SimulationTime))
    CALL OutFile%WriteData('C'//REPEAT('*',100))
    CALL OutFile%WriteData('C')    
    CALL OutFile%WriteData('C'//REPEAT('-',100))
    CALL OutFile%WriteData('     1.0                           / FACT')
    CALL OutFile%WriteData('C'//REPEAT('-',100))
    Text = 'C    ISW    SOILS      GWSTS'
    CALL OutFile%WriteData(TRIM(Text))
    CALL OutFile%WriteData('C'//REPEAT('-',100))
    
    !Print end-of-simulation values
    DO indxSWShed=1,SIZE(RootDepth)
        WRITE (Text,'(I8,2X,F7.4,2X,F9.4)') iSWShedIDs(indxSWShed),SoilMoist(indxSWShed)/RootDepth(indxSWShed),GWStor(indxSWShed)
        CALL OutFile%WriteData(TRIM(Text))
    END DO
    
    !Close file
    CALL OutFile%Kill()

  END SUBROUTINE PrintFinalResults

  


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
  ! --- ARE SMALL WATERSHEDS SIMULATED?
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AppSWShed) RESULT(lDefined)
    CLASS(AppSmallwatershedType),INTENT(IN) :: AppSWShed
    LOGICAL                                 :: lDefined
    
    lDefined = AppSWSHed%lDefined
    
  END FUNCTION IsDefined
  

  ! -------------------------------------------------------------
  ! --- IS SMALL WATERSHED BASE FLOW SIMULATED
  ! -------------------------------------------------------------
  FUNCTION IsBaseFlowSimulated(AppSWShed) RESULT(lSimulated)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    LOGICAL                                 :: lSimulated
    
    !Local variables
    INTEGER :: indxSWShed
    
    !Initialize
    lSimulated = .FALSE.
    
    !Check
    DO indxSWShed=1,AppSWShed%NSWShed
        IF (AppSWShed%SmallWatersheds(indxSWShed)%NBaseFlowNodes .GT. 0) THEN
            lSimulated = .TRUE.
            EXIT
        END IF
    END DO
    
  END FUNCTION IsBaseFlowSimulated

  
  ! -------------------------------------------------------------
  ! --- IS SMALL WATERSHED PERCOLATION FLOW SIMULATED
  ! -------------------------------------------------------------
  FUNCTION IsPercFlowSimulated(AppSWShed) RESULT(lSimulated)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    LOGICAL                                 :: lSimulated
    
    !Local variables
    INTEGER :: indxSWShed
    
    !Initialize
    lSimulated = .FALSE.
    
    !Check
    DO indxSWShed=1,AppSWShed%NSWShed
        IF (AppSWShed%SmallWatersheds(indxSWShed)%NPercFlowNodes .GT. 0) THEN
            lSimulated = .TRUE.
            EXIT
        END IF
    END DO
    
  END FUNCTION IsPercFlowSimulated

  
  

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
  ! --- ADVANCE THE STATE OF SMALL WATERSHEDS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppSWShed)
    CLASS(AppSmallWatershedType) :: AppSWShed
    
    AppSWShed%SmallWatersheds%SoilMoist_P = AppSWShed%SmallWatersheds%SoilMoist
    AppSWShed%SmallWatersheds%GWStor_P    = AppSWShed%SmallWatersheds%GWStor
    
  END SUBROUTINE AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE R.H.S. VECTOR OF MATRIX EQUATION
  ! -------------------------------------------------------------
  SUBROUTINE UpdateRHS(AppSWShed,NNodes,Matrix)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: NNodes
    TYPE(MatrixType)                        :: Matrix
    
    !Local variables
    !*** Assumption: Maximum of 500 GW nodes can be affected by small watersheds
    INTEGER           :: indxSWShed,N,iGWNodes(500)
    REAL(8)           :: Flow(500)
    INTEGER,PARAMETER :: iCompIDs(500) = f_iGWComp
    
    !Return if small watersheds are not simulated
    IF (AppSWShed%NSWShed .EQ. 0) RETURN
    
    !Loop through small watersheds
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%NSWShed
           !Process base flow
           N = pSWSheds(indxSWShed)%NBaseFlowNodes 
           Flow(1:N)     = -pSWSheds(indxSWShed)%BaseFlowNodes%Flow
           iGWNodes(1:N) = (pSWSheds(indxSWShed)%BaseFlowNodes%Layer-1)*NNodes + pSWSheds(indxSWShed)%BaseFlowNodes%Node
           CALL Matrix%UpdateRHS(iCompIDs(1:N),iGWNodes(1:N),Flow(1:N))
           
           !Process percolation
           N = pSWSheds(indxSWShed)%NPercFlowNodes 
           Flow(1:N)     = -pSWSheds(indxSWShed)%PercFlowNodes%Flow
           iGWNodes(1:N) = (pSWSheds(indxSWShed)%PercFlowNodes%Layer-1)*NNodes + pSWSheds(indxSWShed)%PercFlowNodes%Node
           CALL Matrix%UpdateRHS(iCompIDs(1:N),iGWNodes(1:N),Flow(1:N))
        END DO
    END ASSOCIATE
    
  END SUBROUTINE UpdateRHS
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SMALL WATERSHEDS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppSWShed,TimeStep,iStat)
    CLASS(AppSmallWatershedType)  :: AppSWShed
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    INTEGER                     :: indxSWShed,IterMax,indxNode
    REAL(8)                     :: Toler,DeltaT,Area,BaseFlow,ExcessGWRunoff,SurfaceFlow,GWStor,     &
                                   DeepPerc,AchievedConv,GWThreshold,MaxGWStor,Excess,BaseFlowCoeff, &
                                   SurfaceFlowCoeff,TotalPorosity,SoilMoist_P,Tolerance
    REAL(8),PARAMETER           :: Inflow = 0.0
    
    !Initialize
    iStat = 0
   
    !Return if no small watersheds
    IF (AppSWShed%NSWShed .EQ. 0) RETURN
    
    !Inform user
    CALL EchoProgress('Simulating small watershed b.c.')
    
    !Initialize
    DeltaT  = TimeStep%DeltaT
    Toler   = AppSWShed%RZSolverData%Tolerance
    IterMax = AppSWShed%RZSolverData%IterMax
    
    !Simulate
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%NSWShed
            Area             = pSWSheds(indxSWShed)%Area
            GWStor           = pSWSheds(indxSWShed)%GWStor_P
            GWThreshold      = pSWSheds(indxSWShed)%GWThreshold
            MaxGWStor        = pSWSheds(indxSWShed)%MaxGWStor
            BaseFlowCoeff    = pSWSheds(indxSWShed)%BaseFlowCoeff
            SurfaceFlowCoeff = pSWSheds(indxSWShed)%SurfaceFlowCoeff
            TotalPorosity    = pSWSheds(indxSWShed)%Soil%TotalPorosity
            SoilMoist_P      = pSWSheds(indxSWShed)%SoilMoist_P
            IF (SoilMoist_P .EQ. 0.0) THEN
                IF (TotalPorosity .EQ. 0.0) THEN
                    Tolerance = Toler
                ELSE
                    Tolerance = TotalPorosity * Toler
                END IF
            ELSE
                Tolerance = SoilMoist_P * Toler
            END IF
            CALL NonPondedLUMoistureRouter(pSWSheds(indxSWShed)%Precip                     , &
                                           pSWSheds(indxSWShed)%SMax                       , &
                                           SoilMoist_P                                     , &
                                           pSWSheds(indxSWShed)%ETc * DeltaT               , &
                                           pSWSheds(indxSWShed)%Soil%HydCond               , &
                                           TotalPorosity                                   , &
                                           pSWSheds(indxSWShed)%Soil%FieldCapacity         , &
                                           pSWSheds(indxSWShed)%Soil%WiltingPoint          , &
                                           pSWSheds(indxSWShed)%Soil%Lambda                , &
                                           Inflow                                          , &
                                           Tolerance                                       , &
                                           pSWSheds(indxSWShed)%Soil%KunsatMethod          , &
                                           IterMax                                         , &
                                           pSWSheds(indxSWShed)%SoilMoist                  , &
                                           pSWSheds(indxSWShed)%Runoff                     , &
                                           pSWSheds(indxSWShed)%PrecipInfilt               , &
                                           pSWSheds(indxSWShed)%ETa                        , &
                                           pSWSheds(indxSWShed)%RootZonePerc               , &
                                           Excess                                          , &
                                           AchievedConv                                    )
           
            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
                MessageArray(1) = 'Convergence error in soil moisture routing for small watersheds!'
                MessageArray(2) =                   'Small watershed ID   = '//TRIM(IntToText(AppSWShed%SmallWatersheds(indxSWShed)%ID))
                WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',Toler
                WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF

            !Reduce total infiltration based on correction for total porosity
            IF (Excess .NE. 0.0) THEN
                pSWSheds(indxSWShed)%Runoff       = pSWSheds(indxSWShed)%Runoff + Excess 
                pSWSheds(indxSWShed)%PrecipInfilt = pSWSheds(indxSWShed)%Precip - pSWSheds(indxSWShed)%Runoff
            END IF
            
            !Flows out of small watershed and simulation of groundwater
            pSWSheds(indxSWShed)%RootZonePerc = pSWSheds(indxSWShed)%RootZonePerc * DeltaT
            DeepPerc                          = pSWSheds(indxSWShed)%RootZonePerc
            BaseFlow                          = (GWStor + DeepPerc) * BaseFlowCoeff
            ExcessGWRunoff                    = MAX(0.0 , GWStor + DeepPerc - GWThreshold) * SurfaceFlowCoeff
            GWStor                            = GWStor + DeepPerc - (BaseFlow+ExcessGWRunoff)*DeltaT
            IF (GWStor .LT. 0.0) THEN
                BaseFlow       = BaseFlow       + (GWStor/DeltaT) * BaseFlow/(BaseFlow+ExcessGWRunoff)
                ExcessGWRunoff = ExcessGWRunoff + (GWStor/DeltaT) * ExcessGWRunoff/(BaseFlow+ExcessGWRunoff)
                GWStor         = 0.0
            ELSEIF (GWStor .GT. MaxGWStor) THEN
                ExcessGWRunoff = ExcessGWRunoff + GWStor - MaxGWStor
                GWStor         = MaxGWStor
            END IF
            
            !Convert unit rates to volumetric rates and finalize values
            pSWSheds(indxSWShed)%Precip             = pSWSheds(indxSWShed)%Precip 
            pSWSheds(indxSWShed)%PrecipInfilt       = pSWSheds(indxSWShed)%PrecipInfilt * Area
            pSWSheds(indxSWShed)%Runoff             = pSWSheds(indxSWShed)%Runoff * Area
            pSWSheds(indxSWShed)%ETa                = pSWSheds(indxSWShed)%ETa  * Area
            pSWSheds(indxSWShed)%ExcessGWRunoff     = ExcessGWRunoff * Area
            pSWSheds(indxSWShed)%RootZonePerc       = pSWSheds(indxSWShed)%RootZonePerc * Area
            pSWSheds(indxSWShed)%BaseFlowNodes%Flow = BaseFlow * pSWSheds(indxSWShed)%BaseFlowNodes%Frac * Area
            pSWSheds(indxSWShed)%GWStor             = GWStor
            
            !Percolation into groundwater
            SurfaceFlow = pSWSheds(indxSWShed)%Runoff + pSWSheds(indxSWShed)%ExcessGWRunoff
            DO indxNode=1,pSWSheds(indxSWShed)%NPercFlowNodes
                pSWSheds(indxSWShed)%PercFlowNodes(indxNode)%Flow = MAX(0.0 , MIN(SurfaceFlow , pSWSheds(indxSWShed)%PercFlowNodes(indxNode)%MaxFlow))
                SurfaceFlow                                       = SurfaceFlow - pSWSheds(indxSWShed)%PercFlowNodes(indxNode)%Flow
            END DO
            
            !Net inflow into streams (or outside of model area)
            pSWSheds(indxSWShed)%NetStreamInflow = SurfaceFlow

        END DO
    END ASSOCIATE
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF SMALL WATERSHED RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppSWShed,NewUnit)
    CLASS(AppSmallWatershedType) :: AppSWShed
    CHARACTER(LEN=*),INTENT(IN)  :: NewUnit
    
    !Local variables
    INTEGER :: indxSWShed
    REAL(8) :: Factor
    
    !Make sure small watersheds are simulated
    IF (AppSWShed%NSWShed .EQ. 0) RETURN
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert time unit of small watershed parameters
    Factor                = TimeIntervalConversion(NewUnit,AppSWShed%VarTimeUnit)
    AppSWShed%VarTimeUnit = NewUnit
    
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        DO indxSWShed=1,AppSWShed%NSWShed
            pSWSheds(indxSWShed)%PercFlowNodes%MaxFlow = pSWSheds(indxSWShed)%PercFlowNodes%MaxFlow * Factor
            pSWSheds(indxSWShed)%Soil%HydCond          = pSWSheds(indxSWShed)%Soil%HydCond * Factor
            pSWSheds(indxSWShed)%SurfaceFlowCoeff      = pSWSheds(indxSWShed)%SurfaceFlowCoeff * Factor
            pSWSheds(indxSWShed)%BaseFlowCoeff         = pSWSheds(indxSWShed)%BaseFlowCoeff * Factor
        END DO
    END ASSOCIATE
      
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE LIST OF NODES THAT RECEIVE BASE FLOW OR PERCOLATION AT EACH LAYER
  ! -------------------------------------------------------------
  SUBROUTINE CompileNodesWithBCType(AppSWShed,iLayer,iBCType,iBCNodes)
    CLASS(AppSmallWatershedType),INTENT(IN) :: AppSWShed
    INTEGER,INTENT(IN)                      :: iLayer,iBCType
    INTEGER,ALLOCATABLE,INTENT(OUT)         :: iBCNodes(:)
    
    !Local variables
    INTEGER             :: ErrorCode,indxNode,indxSWShed,NTotal,iCount
    INTEGER,ALLOCATABLE :: iWorkArray(:)
    
    !Initialize
    DEALLOCATE (iBCNodes , STAT=ErrorCode)
    iCount = 0
    
    ASSOCIATE (pSWSheds => AppSWShed%SmallWatersheds)
        SELECT CASE (iBCType)
            CASE (SWShedBaseFlowBCID)
                NTotal = SUM(pSWSheds%NBaseFlowNodes)
                ALLOCATE (iWorkArray(NTotal))  ;  iWorkArray = 0
                DO indxSWShed=1,AppSWShed%NSWShed
                    DO indxNode=1,pSWSheds(indxSWShed)%NBaseFlowNodes
                        IF (pSWSheds(indxSWShed)%BaseFlowNodes(indxNode)%Layer .EQ. iLayer) THEN
                            iCount             = iCount + 1
                            iWorkArray(iCount) = pSWSheds(indxSWShed)%BaseFlowNodes(indxNode)%Node
                        END IF
                    END DO
                END DO
                
            CASE (SWShedPercFlowBCID)
                NTotal = SUM(pSWSheds%NPercFlowNodes)
                ALLOCATE (iWorkArray(NTotal))  ;  iWorkArray = 0
                DO indxSWShed=1,AppSWShed%NSWShed
                    DO indxNode=1,pSWSheds(indxSWShed)%NPercFlowNodes
                        IF (pSWSheds(indxSWShed)%PercFlowNodes(indxNode)%Layer .EQ. iLayer) THEN
                            iCount             = iCount + 1
                            iWorkArray(iCount) = pSWSheds(indxSWShed)%PercFlowNodes(indxNode)%Node
                        END IF
                    END DO
                END DO
        
        END SELECT
    END ASSOCIATE
    
    !Store unique node numbers in return variable and sort it
    IF (iCount .GT. 0) THEN
        CALL GetUniqueArrayComponents(iWorkArray(1:iCount),iBCNodes)
        CALL ShellSort(iBCNodes)
    ELSE
        ALLOCATE (iBCNodes(0))
    END IF
    
    !Clear memory
    DEALLOCATE (iWorkArray , STAT=ErrorCode)
    
  END SUBROUTINE CompileNodesWithBCType

END MODULE