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
MODULE Class_AppSubsidence
  USE MessageLogger           , ONLY: SetLastMessage              , &
                                      LogMessage                  , &
                                      EchoProgress                , &
                                      FILE                        , &
                                      MessageArray                , &
                                      iFatal                      , &
                                      iMessage
  USE IOInterface
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE Package_Discretization  , ONLY: NodeType                    , &
                                      AppGridType                 , &
                                      StratigraphyType            , &
                                      GetValuesFromParametricGrid
  USE Class_TecplotOutput     , ONLY: TecplotOutputType
  USE Class_BaseHydrograph
  USE Package_Misc            , ONLY: iGWComp                     , &
                                      iLocationType_SubsidenceObs
  USE Package_Matrix          , ONLY: MatrixType
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
  PUBLIC :: AppSubsidenceType          

 
  ! -------------------------------------------------------------
  ! --- SUBSIDENCE DATA AT A NODE
  ! -------------------------------------------------------------
  TYPE SubsidenceType
      REAL(8) :: ElasticSC          = 0.0        !Elastic storage coefficent
      REAL(8) :: InelasticSC        = 0.0        !Inelastic storage coefficient 
      REAL(8) :: InterbedThick      = 0.0        !Interbed thickness
      REAL(8) :: InterbedThickMin   = 0.0        !Minimum interbed thickness
      REAL(8) :: Subsidence         = 0.0        !Curent subsidence
      REAL(8) :: CumSubsidence      = 0.0        !Cumulative change in the interbed thickness; the InterbedThick above is after applying this change
      REAL(8) :: PreCompactHead     = HUGE(0d0)  !Pre-compaction head
  END TYPE SubsidenceType
  
  
  ! -------------------------------------------------------------
  ! --- SUBSIDENCE DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE AppSubsidenceType
      PRIVATE
      REAL(8)                             :: FactorLen                = 1.0      !Factor for output unit conversion
      CHARACTER(LEN=6)                    :: UnitLen                  = ''       !Output unit
      TYPE(SubsidenceType),ALLOCATABLE    :: Subsidence(:,:)                     !Subsidence data at each (node,layer) combination
      REAL(8),ALLOCATABLE                 :: RegionalCumSubsidence(:)            !Subregional volumetric cumulative subsidence at the current time step
      REAL(8),ALLOCATABLE                 :: RegionalCumSubsidence_P(:)          !Subregional volumetric cumulative subsidence at the previous time step
      TYPE(HydOutputType),ALLOCATABLE     :: SubsHydOutput                       !Subsidence hydrograph output dataset
      TYPE(TecplotOutputType),ALLOCATABLE :: TecplotFile                         !Tecplot output file for subsidence
      TYPE(GenericFileType),ALLOCATABLE   :: FinalSubsFile                       !File that stores the en-of-simulation interbed thicknesses and pre-compaction heads
      LOGICAL                             :: lSubsHydOutput_Defined   = .FALSE.  !Flag to check if this output is defined
      LOGICAL                             :: lTecplotFile_Defined     = .FALSE.  !Flag to check if this output file is defined
      LOGICAL                             :: lFinalSubsFile_Defined   = .FALSE.  !Flag to check if this output is defined
  CONTAINS
      PROCEDURE,PASS :: New                          
      PROCEDURE,PASS :: Kill                         
      PROCEDURE,PASS :: GetInterbedThickAll 
      PROCEDURE,PASS :: GetSubsidence_All
      PROCEDURE,PASS :: GetSubsidenceAtLayer         
      PROCEDURE,PASS :: GetSubregionalCumSubsidence  
      PROCEDURE,PASS :: GetNDataList_AtLocationType
      PROCEDURE,PASS :: GetDataList_AtLocationType
      PROCEDURE,PASS :: GetModelData_AtLocation
      PROCEDURE,PASS :: GetNHydrographs
      PROCEDURE,PASS :: GetHydrographCoordinates
      PROCEDURE,PASS :: GetHydrographNames
      PROCEDURE,PASS :: AdvanceState                 
      PROCEDURE,PASS :: PrintParameters              
      PROCEDURE,PASS :: IsDefined    
      PROCEDURE,PASS :: OverwriteParameters          
      PROCEDURE,PASS :: ProcessSubsidenceParameters  
      PROCEDURE,PASS :: UpdateSubsidence             
      PROCEDURE,PASS :: PrintResults
      PROCEDURE,PASS :: PrintRestartData
      PROCEDURE,PASS :: ReadRestartData
      PROCEDURE,PASS :: Simulate
      PROCEDURE,PASS :: TransferOutputToHDF
  END TYPE AppSubsidenceType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  CHARACTER(LEN=21),PARAMETER :: cDataList_AtSubsObsLocation = 'Subsidence hydrograph'
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppSubsidence::'

  
  
  
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
  ! --- INSTANTIATE SUBSIDENCE COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE New(AppSubsidence,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat) 
    CLASS(AppSubsidenceType),INTENT(OUT) :: AppSubsidence
    LOGICAL,INTENT(IN)                   :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)          :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)    :: Stratigraphy
    COMPLEX,INTENT(IN)                   :: StrmConnectivity(:)
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: ErrorCode,NNodes,NLayers,NRegn,indxLayer
    REAL(8)                     :: rIniSubs(AppGrid%NNodes,Stratigraphy%NLayers),Factors(Stratigraphy%NLayers)
    CHARACTER                   :: cErrorMsg*300,cICFileName*1200,ALine*1200,cVarNames(Stratigraphy%NLayers)*15
    TYPE(GenericFileType)       :: SubsMainFile
    CHARACTER(LEN=14),PARAMETER :: cFormat = '(50(F12.3,2X))'
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('   Instantiating subsidence component ...')
    
    !Initialize
    NNodes   = AppGrid%NNodes
    NRegn    = AppGrid%NSubregions
    NLayers  = Stratigraphy%NLayers
    rIniSubs = 0.0
    
    !Open file
    CALL SubsMainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='subsidence data main input',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (AppSubsidence%Subsidence(NNodes,NLayers)     ,  &
              AppSubsidence%RegionalCumSubsidence(NRegn)   ,  &
              AppSubsidence%RegionalCumSubsidence_P(NRegn) ,  &
              STAT=ErrorCode , ERRMSG=cErrorMsg            )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for subsidence parameters!'//NEW_LINE('x')//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Zero out the initial regional subsidence
    AppSubsidence%RegionalCumSubsidence_P = 0.0
    
    !Read away the version line
    CALL SubsMainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Initial conditions file
    CALL SubsMainFile%ReadData(cICFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    cICFileName = StripTextUntilCharacter(cICFileName,'/')  
    CALL CleanSpecialCharacters(cICFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cICFileName)),cWorkingDirectory,cAbsPathFileName)
    cICFileName = cAbsPathFileName

    !Tecplot output file
    CALL SubsMainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (AppSubsidence%TecplotFile , STAT=ErrorCode , ERRMSG=cErrorMsg)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error allocating memory for subsidence Tecplot file output.'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL AppSubsidence%TecplotFile%New(IsForInquiry,cAbsPathFileName,'subsidence print-out for Tecplot',iStat)  ;  IF (iStat .EQ. -1) RETURN
        AppSubsidence%lTecplotFile_Defined = .TRUE.
        
        !Print zero subsidence as initial values
        IF (.NOT. IsForInquiry) THEN
            cVarNames = [('SUBSIDENCE'//TRIM(IntToText(indxLayer)) , indxLayer=1,NLayers)]
            Factors   = 1.0
            CALL AppSubsidence%TecplotFile%PrintInitialValues(AppGrid,StrmConnectivity,rIniSubs,Factors,cFormat,cVarNames,TimeStep)
        END IF
    END IF
    
    !Final results output file
    CALL SubsMainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (AppSubsidence%FinalSubsFile , STAT=ErrorCode , ERRMSG=cErrorMsg)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error allocating memory for end-of-simulation subsidence output file.'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL AppSubsidence%FinalSubsFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,Descriptor='final subsidence data output',iStat=iStat)
        ELSE
            CALL AppSubsidence%FinalSubsFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,Descriptor='final subsidence data output',iStat=iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
        IF (AppSubsidence%FinalSubsFile%iGetFileType() .NE. TXT) THEN
            CALL SetLastMessage('End-of-simulation subsidence output file must be a text file!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        AppSubsidence%lFinalSubsFile_Defined = .TRUE.
    END IF
    
    !Output unit and conversion factor
    CALL SubsMainFile%ReadData(AppSubsidence%FactorLen,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL SubsMainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;  ALine = StripTextUntilCharacter(ALine,'/')
    AppSubsidence%UnitLen = ADJUSTL(TRIM(ALine))
    
    !Subsidence hydrograph output data
    ALLOCATE (AppSubsidence%SubsHydOutput ,STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for subsidence printing at user-specified locations!'//LineFeed//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL HydOutput_New(IsForInquiry,SubsMainFile,cWorkingDirectory,AppGrid,Stratigraphy,iHyd_Subsidence,AppSubsidence%UnitLen,'TOTAL_CHANGE_THICK',TimeStep,AppSubsidence%SubsHydOutput,iStat)
    IF (iStat .EQ. -1) RETURN
    AppSubsidence%lSubsHydOutput_Defined = HydOutput_IsDefined(AppSubsidence%SubsHydOutput)
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        IF (.NOT. IsForInquiry) CALL HydOutput_PrintResults(Stratigraphy,iHyd_Subsidence,rIniSubs,AppSubsidence%FactorLen,TimeStep,.FALSE.,AppSubsidence%SubsHydOutput)
    ELSE
        DEALLOCATE (AppSubsidence%SubsHydOutput , STAT=ErrorCode)
    END IF
    
    !Read subsidence parameters
    CALL ReadSubsidenceParameters(NLayers,AppGrid,SubsMainFile,AppSubsidence%Subsidence,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read initial interbed thickness and pre-compaction head to overwrite the previous values
    CALL ReadSubsidenceICData(cICFileName,AppSubsidence%Subsidence%InterbedThick,AppSubsidence%Subsidence%PreCompactHead,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL SubsMainFile%Kill()

  END SUBROUTINE New
  
  
 
  
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
  ! --- KILL SUBSIDENCE COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppSubsidence)
    CLASS(AppSubsidenceType) :: AppSubsidence
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (AppSubsidence%Subsidence              , &
                AppSubsidence%RegionalCumSubsidence   , &
                AppSubsidence%RegionalCumSubsidence_P , &
                STAT = ErrorCode                      )
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        CALL HydOutput_Kill(AppSubsidence%SubsHydOutput)
        DEALLOCATE (AppSubsidence%SubsHydOutput , STAT=ErrorCode)
    END IF
    
    IF (AppSubsidence%lTecplotFile_Defined) THEN
        CALL AppSubsidence%TecplotFile%Kill()
        DEALLOCATE (AppSubsidence%TecplotFile , STAT=ErrorCode)
    END IF
        
    IF (AppSubsidence%lFinalSubsFile_Defined) CALL AppSubsidence%FinalSubsFile%Kill()
            
    !Set attributes to defaults
    AppSubsidence%FactorLen              = 0.0
    AppSubsidence%UnitLen                = ''
    AppSubsidence%lSubsHydOutput_Defined = .FALSE.
    AppSubsidence%lTecplotFile_Defined   = .FALSE.
    AppSubsidence%lFinalSubsFile_Defined = .FALSE.
    
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
  ! --- GET NUMBER OF HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(AppSubsidence) RESULT(NHydrographs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER                             :: NHydrographs
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        NHydrographs = AppSubsidence%SubsHydOutput%GetNHydrographs()
    ELSE
        NHydrographs = 0
    END IF
    
  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(AppSubsidence,GridNodeCoordinates,X,Y)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    TYPE(NodeType),INTENT(IN)           :: GridNodeCoordinates(:)
    REAL(8),INTENT(OUT)                 :: X(:),Y(:)
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        CALL AppSubsidence%SubsHydOutput%GetHydrographCoordinates(GridNodeCoordinates,X,Y)
    END IF
    
 END SUBROUTINE GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographNames(AppSubsidence,cNamesList)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    CHARACTER(LEN=*),INTENT(OUT)        :: cNamesList(:)  !Assumes array is previously dimensioned based on the number of hydrographs
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        CALL AppSubsidence%SubsHydOutput%GetHydrographNames(cNamesList)
    END IF
    
 END SUBROUTINE GetHydrographNames
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE AT (node,layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidence_All(AppSubsidence,Subs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    REAL(8),INTENT(OUT)                 :: Subs(:,:)
    
    Subs = AppSubsidence%Subsidence%Subsidence
    
  END SUBROUTINE GetSubsidence_All
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBSIDENCE AT ALL NODES OF A LAYER
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidenceAtLayer(AppSubsidence,iLayer,Subs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(IN)                  :: iLayer
    REAL(8),INTENT(OUT)                 :: Subs(:)
    
    Subs = AppSubsidence%Subsidence(:,iLayer)%Subsidence
    
  END SUBROUTINE GetSubsidenceAtLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET INTERBED THICKNESS AT ALL NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInterbedThickAll(AppSubsidence,InterbedThick)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    REAL(8),INTENT(OUT)                 :: InterbedThick(:,:)
    
    InterbedThick = AppSubsidence%Subsidence%InterbedThick
    
  END SUBROUTINE GetInterbedThickAll
  

  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL CUMULATIVE SUBSIDENCE
  ! -------------------------------------------------------------
  PURE FUNCTION GetSubregionalCumSubsidence(AppSubsidence,NRegions,lPreviousTS) RESULT(Subs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(IN)                  :: NRegions
    LOGICAL,INTENT(IN)                  :: lPreviousTS
    REAL(8)                             :: Subs(NRegions)
    
    IF (lPreviousTS) THEN
        Subs = AppSubsidence%RegionalCumSubsidence_P
    ELSE
        Subs = AppSubsidence%RegionalCumSubsidence
    END IF
    
  END FUNCTION GetSubregionalCumSubsidence
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF DATA TYPES FOR POST-PROCESSING AT A SUBSIDENCE OBSERVATION LOCATION
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(AppSubsidence) RESULT(NData)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER                             :: NData
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        NData = 1
    ELSE
        NData = 0
    END IF
    
  END FUNCTION GetNDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF DATA TYPES FOR POST-PROCESSING AT A SUBSIDENCE OBSERVATION LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType(AppSubsidence,cDataList,cFileList,lBudgetType) 
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    CHARACTER(LEN=*),ALLOCATABLE        :: cDataList(:),cFileList(:)
    LOGICAL,ALLOCATABLE                 :: lBudgetType(:)
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        ALLOCATE (cDataList(1) , cFileList(1) , lBudgetType(1))
        cDataList   = cDataList_AtSubsObsLocation
        lBudgetType = .FALSE.
        CALL AppSubsidence%SubsHydOutput%GetFileName(cFileName)
        cFileList = ''
        cFileList = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have copied into an HDF file
    END IF
    
  END SUBROUTINE GetDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBSIDENCE HYDROGRAPH FOR POST-PROCESSING AT A SUBSIDENCE HYDROGRAPH LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation(AppSubsidence,iSubsHydID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(AppSubsidenceType)    :: AppSubsidence
    INTEGER,INTENT(IN)          :: iSubsHydID
    CHARACTER(LEN=*),INTENT(IN) :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)          :: rFact_LT
    INTEGER,INTENT(OUT)         :: nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    CALL AppSubsidence%SubsHydOutput%ReadHydrograph_AtLocation(iSubsHydID,cOutputBeginDateAndTime,cOutputEndDateAndTime,AppSubsidence%FactorLen,rFact_LT,nActualOutput,rOutputDates,rOutputValues,iStat)

  END SUBROUTINE GetModelData_AtLocation
  
  
  
  

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
  SUBROUTINE ReadRestartData(AppSubsidence,InFile,iStat)
    CLASS(AppSubsidenceType) :: AppSubsidence
    TYPE(GenericFileType)    :: InFile
    INTEGER,INTENT(OUT)      :: iStat
    
    CALL InFile%ReadData(AppSubsidence%Subsidence%InterbedThick,iStat)   ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%Subsidence%CumSubsidence,iStat)   ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%Subsidence%PreCompactHead,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%RegionalCumSubsidence,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%RegionalCumSubsidence_P,iStat)  
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ INITIAL INTERBED THICKNESS AND PRE-COMPACTION HEAD
  ! -------------------------------------------------------------
  SUBROUTINE ReadSubsidenceICData(cICFileName,InterbedThick,PreCompactHead,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cICFileName
    REAL(8)                     :: InterbedThick(:,:),PreCompactHead(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNAmeLen+20) :: ThisProcedure = ModName // 'ReadSubsidenceICData'
    TYPE(GenericFileType)        :: ICFile
    INTEGER                      :: ID,indxNode,NLayers,NNodes
    REAL(8)                      :: rFact,rDummyArray(1+2*SIZE(InterbedThick,DIM=2))
    
    !Initialize
    iStat = 0
    
    !If IC filename is empty return
    IF (cICFileName .EQ. '') RETURN
    
    !Initialize
    NNodes  = SIZE(InterbedThick , DIM=1)
    NLayers = SIZE(InterbedThick , DIM=2)
    
    !Open IC file
    CALL ICFile%New(FileName=cICFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='subsidence initial conditions data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Conversion factor
    CALL ICFile%ReadData(rFact,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read data
    DO indxNode=1,NNodes
        CALL ICFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Make sure data is entered sequentially
        ID = rDummyArray(1)
        IF (ID .NE. indxNode) THEN
            MessageArray(1) = 'Subsidence initial conditions must be entered sequentially for each node.'
            MessageArray(2) = 'Expected node ID = ' // TRIM(IntToText(indxNode))
            MessageArray(3) = 'Entered node ID  = ' // TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Interbed thickness
        InterbedThick(indxNode,:) = rDummyArray(2:1+NLayers) * rFact
        
        !Pre-compaction head
        PreCompactHead(indxNode,:) = rDummyArray(NLayers+2:) * rFact
        
    END DO
    
    !Close file
    CALL ICFile%Kill()
    
  END SUBROUTINE ReadSubsidenceICData
  
  
  ! -------------------------------------------------------------
  ! --- READ SUBSIDENCE PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE ReadSubsidenceParameters(NLayers,AppGrid,InFile,Subs,iStat)
    INTEGER,INTENT(IN)               :: NLayers
    TYPE(AppGRidType),INTENT(IN)     :: AppGrid
    TYPE(GenericFileType)            :: InFile
    TYPE(SubsidenceType),INTENT(OUT) :: Subs(:,:)
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ReadSubsidenceParameters'
    INTEGER                      :: NGroup,NNodes,indxNode,indxLayer,ID
    REAL(8)                      :: rFactors(6),rDummyArray(6),rDummy3DArray(AppGrid%NNodes,NLayers,5)
    
    !Initaialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('   Reading subsidence parameters...')
    
    !Initialize
    NNodes = AppGrid%NNodes
    
    !Read number of parameteric grids
    CALL InFile%ReadData(NGroup,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Conversion factors
    CALL InFile%ReadData(rFactors,iStat)  ;  IF (iStat .EQ. -1) RETURN
    !rFactors(1): for x-y coordinates
    !rFactors(2): for ElasticSC
    !rFactors(3): for InelasticSC
    !rFactors(4): for InterbedThick
    !rFactors(5): for InterbedThickMin
    !rFActors(6): for PreCompactHead
    
    !Non-parametric data input
    IF (NGroup .EQ. 0) THEN
        DO indxNode=1,NNodes
          DO indxLayer=1,NLayers
            IF (indxLayer .EQ. 1) THEN
                CALL InFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
                ID = INT(rDummyArray(1))
                IF (ID .NE. indxNode) THEN 
                  MessageArray(1) = 'Subsidence parameters should be entered sequentially for each node.'
                  MessageArray(2) = 'Expected GW node = '//TRIM(IntToText(indxNode))
                  MessageArray(3) = 'Entered GW node  = '//TRIM(IntToText(ID))
                  CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
                END IF
            ELSE
                CALL InFile%ReadData(rDummyArray(2:),iStat)  ;  IF (iStat .EQ. -1) RETURN
            END IF
            Subs(indxNode,indxLayer)%ElasticSC        = rDummyArray(2) * rFactors(2)
            Subs(indxNode,indxLayer)%InelasticSC      = rDummyArray(3) * rFactors(3)
            Subs(indxNode,indxLayer)%InterbedThick    = rDummyArray(4) * rFactors(4)
            Subs(indxNode,indxLayer)%InterbedThickMin = rDummyArray(5) * rFactors(5)
            Subs(indxNode,indxLayer)%PreCompactHead   = rDummyArray(6) * rFactors(6)
          END DO
        END DO
    END IF

    !Parametric data input
    IF (NGroup .GT. 0) THEN

        !Read the parameter values at parametric nodes and compute the interpolation coefficients for finite element nodes
        CALL GetValuesFromParametricGrid(InFile,AppGrid%GridType,NGroup,rFactors,.FALSE.,rDummy3DArray,iStat)
        IF (iStat .EQ. -1) RETURN

        !Initialize parameter values
        DO indxLayer=1,NLayers
          DO indxNode=1,NNodes
            Subs(indxNode,indxLayer)%ElasticSC        = rDummy3DArray(indxNode,indxLayer,1)
            Subs(indxNode,indxLayer)%InelasticSC      = rDummy3DArray(indxNode,indxLayer,2)
            Subs(indxNode,indxLayer)%InterbedThick    = rDummy3DArray(indxNode,indxLayer,3)
            Subs(indxNode,indxLayer)%InterbedThickMin = rDummy3DArray(indxNode,indxLayer,4)
            Subs(indxNode,indxLayer)%PreCompactHead   = rDummy3DArray(indxNode,indxLayer,5)
          END DO
        END DO
    END IF
    
  END SUBROUTINE ReadSubsidenceParameters

  
  
  
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
  SUBROUTINE PrintRestartData(AppSubsidence,OutFile)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    TYPE(GenericFileType)               :: OutFile
    
    CALL OutFile%WriteData(AppSubsidence%Subsidence%InterbedThick)
    CALL OutFile%WriteData(AppSubsidence%Subsidence%CumSubsidence)
    CALL OutFile%WriteData(AppSubsidence%Subsidence%PreCompactHead)
    CALL OutFile%WriteData(AppSubsidence%RegionalCumSubsidence)
    CALL OutFile%WriteData(AppSubsidence%RegionalCumSubsidence_P)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE FOR SUBSIDENCE-RELATED PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppSubsidence,Stratigraphy,TimeStep,lEndOfSimulation)
    CLASS(AppSubsidenceType)          :: AppSubsidence
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TImeStepType),INTENT(IN)     :: TimeStep
    LOGICAL,INTENT(IN)                :: lEndOfSimulation
    
    !Subsidence hydrographs
    IF (AppSubsidence%lSubsHydOutput_Defined)   &
        CALL PrintSubsidenceHydrographs(Stratigraphy,AppSubsidence%Subsidence%CumSubsidence,AppSubsidence%FactorLen,TimeStep,lEndOfSimulation,AppSubsidence%SubsHydOutput)

    !Tecplot print-out
    IF (AppSubsidence%lTecplotFile_Defined)  &
        CALL AppSubsidence%TecplotFile%PrintResults(AppSubsidence%Subsidence%CumSubsidence,AppSubsidence%FactorLen,TimeStep)
    
    !Final results
    IF (lEndOfSimulation) THEN
        IF (AppSubsidence%lFinalSubsFile_Defined) CALL PrintFinalSubs(AppSubsidence%Subsidence,TimeStep,AppSubsidence%FinalSubsFile)
    END IF
    
  END SUBROUTINE PrintResults


  ! -------------------------------------------------------------
  ! --- PRINT SUBSIDENCE HYDROGRAPHS
  ! -------------------------------------------------------------
  SUBROUTINE PrintSubsidenceHydrographs(Stratigraphy,CumSubsidence,rFactor,TimeStep,lEndOfSimulation,SubsHydOutput)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: CumSubsidence(:,:),rFactor
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    LOGICAL,INTENT(IN)                :: lEndOfSimulation
    TYPE(HydOutputType)               :: SubsHydOutput
  
    CALL HydOutput_PrintResults(Stratigraphy,iHyd_Subsidence,CumSubsidence,rFactor,TimeStep,lEndOfSimulation,SubsHydOutput) 
    
  END SUBROUTINE PrintSubsidenceHydrographs
  

  ! -------------------------------------------------------------
  ! --- PRINT FINAL SUBSIDENCE PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE PrintParameters(AppSubs)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubs
    
    !Local variables
    INTEGER :: indxLayer,indxNode,NNodes,NLayers
    CHARACTER :: Text*500
    
    !Initialize
    NNodes  = SIZE(AppSubs%Subsidence , DIM=1)
    NLayers = SIZE(AppSubs%Subsidence , DIM=2)
    
    !Print parameters
    CALL LogMessage('',iMessage,'',FILE)
    CALL LogMessage(REPEAT('-',100),iMessage,'',FILE)
    CALL LogMessage(REPEAT(' ',30)//'SUBSIDENCE PARAMETER VALUES FOR EACH NODE',iMessage,'',FILE)
    CALL LogMessage(REPEAT(' ',12)//'*** Note: Values Below are After Multiplication by Conversion Factors ***',iMessage,'',FILE)
    CALL LogMessage(REPEAT('-',100),iMessage,'',FILE)
    WRITE (Text,'(A,2X,5(A,2X))')            &
      '   NODE','        SCE             '   &
               ,'        SCI             '   &
               ,'        DC              '   &
               ,'        DCMIN           '   &
               ,'        HC              '
    CALL LogMessage(TRIM(Text),iMessage,'',FILE)

    DO indxNode=1,NNodes
      DO indxLayer=1,NLayers                                                                                     
        IF (indxLayer .EQ. 1) THEN                                                                               
          WRITE (Text,'(I7,2X,10(1PG24.15E3,2X))')                                                                                           & 
               indxNode ,AppSubs%Subsidence(indxNode,indxLayer)%ElasticSC      ,AppSubs%Subsidence(indxNode,indxLayer)%InelasticSC       ,   & 
                         AppSubs%Subsidence(indxNode,indxLayer)%InterbedThick  ,AppSubs%Subsidence(indxNode,indxLayer)%interbedThickMin  ,   &
                         AppSubs%Subsidence(indxNode,indxLayer)%PreCompactHead     
        ELSE                                                                                          
          WRITE (Text,'(9X,10(1PG24.15E3,2X))')                                                                                              &  
                         AppSubs%Subsidence(indxNode,indxLayer)%ElasticSC      ,AppSubs%Subsidence(indxNode,indxLayer)%InelasticSC       ,   & 
                         AppSubs%Subsidence(indxNode,indxLayer)%InterbedThick  ,AppSubs%Subsidence(indxNode,indxLayer)%interbedThickMin  ,   &
                         AppSubs%Subsidence(indxNode,indxLayer)%PreCompactHead     
        END IF                                                                                          
        CALL LogMessage(TRIM(Text),iMessage,'',FILE)                                                                       
      END DO                                                                                          
    END DO  
    CALL LogMessage('',iMessage,'',FILE)

  END SUBROUTINE PrintParameters
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT END-OF-SIMULATION VALUES
  ! -------------------------------------------------------------
  SUBROUTINE PrintFinalSubs(Subs,TimeStep,OutFile)
    Type(SubsidenceType),INTENT(IN) :: Subs(:,:)
    TYPE(TimeStepType),INTENT(IN)   :: TimeStep
    TYPE(GenericFileType)           :: OutFile
    
    !Local variables
    INTEGER   :: indxLayer,indxNode,NLayers,NNodes
    REAL(8)   :: rWorkArray(2*SIZE(Subs,DIM=2))
    CHARACTER :: SimulationTime*21,Text*1000,cLayer*7
    
    !Initialize
    NNodes  = SIZE(Subs,DIM=1)
    NLayers = SIZE(Subs,DIM=2)
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF
    
    !Print header
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C ***** SUBSIDENCE DATA AT '//TRIM(SimulationTime))
    CALL OutFile%WriteData('C'//REPEAT('*',79))
    CALL OutFile%WriteData('C')    
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    CALL OutFile%WriteData('     1.0                           / FACT')
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    Text = 'C      ID           DC[1]'
    DO indxLayer=2,NLayers
        cLayer = ADJUSTL('DC['//TRIM(IntToText(indxLayer))//']')
        WRITE (Text,'(3A)') TRIM(Text),REPEAT(' ',18-LEN_TRIM(cLayer)),TRIM(cLayer)
    END DO
    DO indxLayer=1,NLayers
        cLayer = ADJUSTL('HC['//TRIM(IntToText(indxLayer))//']')
        WRITE (Text,'(3A)') TRIM(Text),REPEAT(' ',18-LEN_TRIM(cLayer)),TRIM(cLayer)
    END DO
    CALL OutFile%WriteData(TRIM(Text))
    CALL OutFile%WriteData('C'//REPEAT('-',79))
    
    !Print final interbed thickness and pre-compaction heads
    DO indxNode=1,NNodes
        rWorkArray(1:NLayers)  = Subs(indxNode,:)%InterbedThick
        rWorkArray(NLayers+1:) = Subs(indxNode,:)%PreCompactHead
        WRITE (Text,'(I8,100F18.6)') indxNode,rWorkArray
        CALL OutFile%WriteData(Text)
    END DO
    
  END SUBROUTINE PrintFinalSubs

  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PRADICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- IS SUBSIDENCE DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AppSubs) RESULT(lDefined)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubs
    LOGICAL                             :: lDefined
    
    IF (ALLOCATED(AppSubs%Subsidence)) THEN
        lDefined = .TRUE.
    ELSE
        lDefined = .FALSE.
    END IF
    
  END FUNCTION IsDefined
    
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. ENTITIES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- TRANSFER HEADS AT ALL NODES FROM TEXT/DSS FILE TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(AppSubs,NTIME,TimeStep,iStat)
    CLASS(AppSubsidenceType)      :: AppSubs
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Initialize
    iStat = 0
    
    !Return if no output
    IF (.NOT. AppSubs%lSubsHydOutput_Defined) RETURN
    
    CALL AppSubs%SubsHydOutput%Transfer_To_HDF('subsidence output','/Subsidence',NTIME,TimeStep,AppSubs%FactorLen,iStat)
    
  END SUBROUTINE TransferOutputToHDF


  ! -------------------------------------------------------------
  ! --- COMPUTE SUBREGIONAL VOLUMETRIC SUBSIDENCE
  ! -------------------------------------------------------------
  SUBROUTINE ComputeRegionalCumSubsidence(AppGrid,CumSubsidence,RegionalCumSubsidence)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)           :: CumSubsidence(:,:)
    REAL(8),INTENT(OUT)          :: RegionalCumSubsidence(:)
  
    !Local variable
    INTEGER :: indxLayer,NLayers
    REAL(8) :: CumSubsVolume(AppGrid%NNodes)
    
    !Initialize
    NLayers               = SIZE(CumSubsidence , DIM=2)
    RegionalCumSubsidence = 0.0
    
    !Compute regional cumulative volumetric subsidence
    DO indxLayer=1,NLayers
        CumSubsVolume         = CumSubsidence(:,indxLayer) * AppGrid%AppNode%Area
        RegionalCumSubsidence = RegionalCumSubsidence + AppGrid%AccumNodeValuesToSubregions(CumSubsVolume)
    END DO

  END SUBROUTINE ComputeRegionalCumSubsidence
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE THE STATE OF THE SUBSIDENCE SYSTEM IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppSubsidence)
    CLASS(AppSubsidenceType) :: AppSubsidence
    
    AppSubsidence%RegionalCumSubsidence_P = AppSubsidence%RegionalCumSubsidence
        
  END SUBROUTINE AdvanceState


  ! -------------------------------------------------------------
  ! --- SIMULATE THE EFFECT OF SUBSIDENCE ON R.H.S. VECTOR AND COEFF MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppSubsidence,AppGrid,Stratigraphy,GWHead,GWHead_P,Matrix)
    CLASS(AppSubsidenceType),INTENT(IN) :: AppSubsidence
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)   :: Stratigraphy
    REAL(8),INTENT(IN)                  :: GWHead(:,:),GWHead_P(:,:)
    TYPE(MatrixType)                    :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iBase,NNodes,NLayers,iGWNode,iNodeIDs(1)
    REAL(8)           :: Area,rGWHead,rGWHead_P,Storativ,InterbedThick,PreCompactHead,  &
                         ElasticSC,InelasticSC,InterbedThickMin,rUpdateValues(1),rUpdateRHS(SIZE(GWHead))
    INTEGER,PARAMETER :: iCompIDs(1) = [iGWComp]
    
    !Initialize
    NNodes  = AppGrid%NNodes
    NLayers = Stratigraphy%NLayers
    
    DO indxLayer=1,NLayers
        iBase = (indxLayer-1) * NNodes
        DO indxNode=1,NNodes           
            iGWNode = iBase + indxNode
            
            !Cycle if node is inactive
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                rUpdateRHS(iGWNode) = 0.0
                CYCLE
            END IF
            
            !Values for computation
            Area             = AppGrid%AppNode(indxNode)%Area
            InterbedThick    = AppSubsidence%Subsidence(indxNode,indxLayer)%InterbedThick * Area
            InterbedThickMin = AppSubsidence%Subsidence(indxNode,indxLayer)%InterbedThickMin * Area
            !Cycle if minimum interbed thickness is attained
            IF (InterbedThick .LE. InterbedThickMin) THEN
                rUpdateRHS(iGWNode) = 0.0
                CYCLE
            END IF
            PreCompactHead   = AppSubsidence%Subsidence(indxNode,indxLayer)%PreCompactHead
            ElasticSC        = AppSubsidence%Subsidence(indxNode,indxLayer)%ElasticSC
            InElasticSC      = AppSubsidence%Subsidence(indxNode,indxLayer)%InelasticSC
            rGWHead          = GWHead(indxNode,indxLayer)
            rGWHead_P        = GWHead_P(indxNode,indxLayer)
            IF (rGWHead .GT. PreCompactHead) THEN
                Storativ = ElasticSC * InterbedThick 
            ELSE
                Storativ = InelasticSC * InterbedThick 
            END IF
            
            !R.H.S. function
            rUpdateRHS(iGWNode) = Storativ * (rGWHead - PreCompactHead)                    &
                                + ElasticSC * InterbedThick * (PreCompactHead - rGWHead_P)
            
            !Coeff. matrix
            iNodeIDs(1)      = iGWNode 
            rUpdateValues(1) = Storativ
            CALL Matrix%UpdateCOEFF(iGWComp,iGWNode,iCompIDs,iNodeIDs,rUpdateValues)
            
       END DO
    END DO
    
    !Update R.H.S. vector
    CALL Matrix%UpdateRHS(iGWComp,1,rUpdateRHS)
  
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- OVERWRITE ELASTIC AND INELASTIC STORAGE COEFFICIENTS
  ! -------------------------------------------------------------
  SUBROUTINE OverwriteParameters(AppSubsidence,ElasticSC,InelasticSC)
    CLASS(AppSubsidenceType) :: AppSubsidence
    REAL(8),INTENT(IN)       :: ElasticSC(:,:),InelasticSC(:,:)
    
    !Local variables
    INTEGER :: NNodes,NLayers,indxNode,indxLayer
    
    !Initialize
    NNodes  = SIZE(ElasticSC , DIM=1)
    NLayers = SIZE(ElasticSC , DIM=2)
    
    !Overwrite parameters
    DO indxLayer=1,NLayers
        DO indxNode=1,NNodes
            IF (ElasticSC(indxNode,indxLayer)   .GE. 0.0) AppSubsidence%Subsidence(indxNode,indxLayer)%ElasticSC   = ElasticSC(indxNode,indxLayer)
            IF (InelasticSC(indxNode,indxLayer) .GE. 0.0) AppSubsidence%Subsidence(indxNode,indxLayer)%InelasticSC = InelasticSC(indxNode,indxLayer)
        END DO
    END DO
    
  END SUBROUTINE OverwriteParameters
  
  
  ! -------------------------------------------------------------
  ! --- PROCESS SUBSIDENCE PARAMETERS TO BE USED IN SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE ProcessSubsidenceParameters(AppSubsidence,GWHead)
    CLASS(AppSubsidenceType) :: AppSubsidence
    REAL(8),INTENT(IN)       :: GWHead(:,:)
    
    !Local variables
    INTEGER :: indxNode,indxLayer,NNodes,NLayers
    
    !Initialize
    NNodes  = SIZE(AppSubsidence%Subsidence , DIM=1)
    NLayers = SIZE(AppSubsidence%Subsidence , DIM=2)
    
    !Process
    ASSOCIATE (pSubsidence => AppSubsidence%Subsidence)
        DO indxLayer=1,NLayers
            DO indxNode=1,NNodes
                pSubsidence(indxNode,indxLayer)%ElasticSC      = pSubsidence(indxNode,indxLayer)%ElasticSC 
                pSubsidence(indxNode,indxLayer)%InelasticSC    = pSubsidence(indxNode,indxLayer)%InelasticSC 
                pSubsidence(indxNode,indxLayer)%PreCompactHead = MIN(pSubsidence(indxNode,indxLayer)%PreCompactHead , GWHead(indxNOde,indxLayer))
            END DO
        END DO
    END ASSOCIATE
    
  END SUBROUTINE ProcessSubsidenceParameters
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE SUBSIDENCE RELATED TERMS
  ! -------------------------------------------------------------
  SUBROUTINE UpdateSubsidence(AppSubsidence,AppGrid,Stratigraphy,GWHead,GWHead_P)
    CLASS(AppSubsidenceType),TARGET   :: AppSubsidence
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:)
    
    !Local variables
    INTEGER         :: indxNode,indxLayer,NNodes
    REAL(8)         :: rGWHead,rGWHead_P,ElasticSC,InelasticSC,InterbedThickMin
    REAL(8),POINTER :: pPreCompactHead,pInterbedThick,pCumSubsidence,pSubsidence
    
    !Initialize
    NNodes = SIZE(GWHead , DIM=1)
    
    DO indxLayer=1,Stratigraphy%NLayers
      DO indxNode=1,NNodes
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) CYCLE
        InterbedThickMin =  AppSubsidence%Subsidence(indxNode,indxLayer)%InterbedThickMin
        pInterbedThick   => AppSubsidence%Subsidence(indxNode,indxLayer)%InterbedThick
        !Cycle if the minimum interbed thickness is attained
        IF (pInterbedThick .LE. InterbedThickMin) THEN
            AppSubsidence%Subsidence(indxNode,indxLayer)%Subsidence = 0.0
            CYCLE
        END IF
        rGWHead          =  GWHead(indxNode,indxLayer)
        rGWHead_P        =  GWHead_P(indxNode,indxLayer)
        ElasticSC        =  AppSubsidence%Subsidence(indxNode,indxLayer)%ElasticSC
        InelasticSC      =  AppSubsidence%Subsidence(indxNode,indxLayer)%InelasticSC
        pSubsidence      => AppSubsidence%Subsidence(indxNode,indxLayer)%Subsidence
        pCumSubsidence   => AppSubsidence%Subsidence(indxNode,indxLayer)%CumSubsidence
        pPreCompactHead  => AppSubsidence%Subsidence(indxNode,indxLayer)%PreCompactHead
        
        IF (rGWHead .GT. pPreCompactHead) THEN
            pSubsidence     = -ElasticSC * pInterbedThick * (rGWHead-rGWHead_P)
        ELSE
            pSubsidence     = -InelasticSC * pInterbedThick * (rGWHead-pPreCompactHead)    &
                              -ElasticSC * pInterbedThick * (pPreCompactHead-rGWHead_P)
            pPreCompactHead = rGWHead
        END IF
        
        pInterbedThick = pInterbedThick - pSubsidence
        pCumSubsidence = pCumSubsidence + pSubsidence
      END DO
    END DO
    
    !Update regional volumetric cumulative subsidence
    CALL ComputeRegionalCumSubsidence(AppGrid,AppSubsidence%Subsidence%CumSubsidence,AppSubsidence%RegionalCumSubsidence)
    
    !Clear pointers
    pInterbedThick  => NULL()
    pSubsidence     => NULL()
    pCumSubsidence  => NULL()
    pPreCompactHead => NULL()

  END SUBROUTINE UpdateSubsidence
  
  
END MODULE