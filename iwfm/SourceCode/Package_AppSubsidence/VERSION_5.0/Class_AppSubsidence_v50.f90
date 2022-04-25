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
MODULE Class_AppSubsidence_v50
  USE MessageLogger           , ONLY: SetLastMessage                , &
                                      LogMessage                    , &
                                      EchoProgress                  , &
                                      f_iFILE                       , &
                                      MessageArray                  , &
                                      f_iFatal                      , &
                                      f_iMessage                      
  USE IOInterface             , ONLY: GenericFileType               , &
                                      f_iTXT                           
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter       , &
                                      EstablishAbsolutePathFilename , &
                                      CleanSpecialCharacters        , &
                                      ConvertID_To_Index            , &
                                      LocateInList                  , &
                                      f_cLineFeed                   , &
                                      IntToText                     , &
                                      FEXP
  USE TimeSeriesUtilities     , ONLY: TimeStepType                  
  USE Package_Discretization  , ONLY: AppGridType                   , &
                                      StratigraphyType              , &
                                      GetValuesFromParametricGrid   
  USE Class_TecplotOutput     , ONLY: TecplotOutputType             
  USE Class_BaseHydrograph    , ONLY: HydOutputType                 , &
                                      f_iHyd_Subsidence             
  USE Package_Misc            , ONLY: f_iGWComp                     , &
                                      f_iLocationType_SubsidenceObs , &
                                      f_rSmoothMaxP 
  USE Package_Matrix          , ONLY: MatrixType
  USE Class_BaseAppSubsidence , ONLY: BaseAppSubsidenceType         , &
                                      ComputeRegionalCumSubsidence
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
  PUBLIC :: AppSubsidence_v50_Type          

 
  ! -------------------------------------------------------------
  ! --- SUBSIDENCE DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseAppSubsidenceType) :: AppSubsidence_v50_Type
      PRIVATE
      LOGICAL             :: lInitHeadsFromGW = .TRUE.      !Are initial interbed heads defined explicitly or gw heads are used instead?
      INTEGER             :: iNSubLayerMax    = 0           !Maximum number of layers for the interbeds
      REAL(8),ALLOCATABLE :: Kvsub(:,:)                     !Vertical conductivity of interbeds at each (node,layer)
      REAL(8),ALLOCATABLE :: rNEQ(:,:)                      !Number of equivalent delay interbeds at each (node,layer)
      INTEGER,ALLOCATABLE :: iDataIndex_S(:,:)              !Beginning index within arrays for the start of interbed data at each (node,layer)
      INTEGER,ALLOCATABLE :: iDataIndex_L(:,:)              !Ending index within arrays for the start of interbed data at each (node,layer)
      REAL(8),ALLOCATABLE :: PreCompactHead(:)              !Preconsolidation head array
      REAL(8),ALLOCATABLE :: HSUB(:)                        !Interbed head array
      REAL(8),ALLOCATABLE :: HSUBOLD(:)                     !Interbed head array from previous time step
  CONTAINS
      PROCEDURE,PASS :: New                             => AppSubsidence_v50_New                        
      PROCEDURE,PASS :: KillImplementation              => AppSubsidence_v50_KillImplementation
      PROCEDURE,PASS :: GetVersion                      => AppSubsidence_v50_GetVersion
      PROCEDURE,PASS :: PrintParameters                 => AppSubsidence_v50_PrintParameters              
      PROCEDURE,PASS :: PrintFinalSubs                  => AppSubsidence_v50_PrintFinalSubs
      PROCEDURE,PASS :: PrintRestartData_Implementation => AppSubsidence_v50_PrintRestartData
      PROCEDURE,PASS :: ReadRestartData_Implementation  => AppSubsidence_v50_ReadRestartData
      PROCEDURE,PASS :: ProcessSubsidenceParameters     => AppSubsidence_v50_ProcessSubsidenceParameters 
      PROCEDURE,PASS :: Simulate                        => AppSubsidence_v50_Simulate
      PROCEDURE,PASS :: UpdateSubsidence                => AppSubsidence_v50_UpdateSubsidence  
      PROCEDURE,PASS :: AdvanceState                    => AppSubsidence_v50_AdvanceState
  END TYPE AppSubsidence_v50_Type
  
  
  ! -------------------------------------------------------------
  ! --- LAKE PACKAGE VERSION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iVersion    = 50
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    ='5.0.0000'
  INCLUDE 'AppSubsidence_v50_Revision.fi'
 
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 25
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppSubsidence_v50::'

  
  
  
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
  SUBROUTINE AppSubsidence_v50_New(AppSubsidence,IsForInquiry,cFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat) 
    CLASS(AppSubsidence_v50_Type),INTENT(OUT) :: AppSubsidence
    LOGICAL,INTENT(IN)                        :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)               :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)                        :: iGWNodeIDs(:)
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy
    COMPLEX,INTENT(IN)                        :: StrmConnectivity(:)
    TYPE(TimeStepType),INTENT(IN)             :: TimeStep
    INTEGER,INTENT(OUT)                       :: iStat
    
    !Local data type for data compilation
    TYPE SubLayerDataType
        REAL(8),ALLOCATABLE :: PreCompactHead(:)  !Preconsolidation head array
        REAL(8),ALLOCATABLE :: HSUB(:)            !Interbed head array
        REAL(8),ALLOCATABLE :: HSUBOLD(:)         !Interbed head array from previous time step
    END TYPE SubLayerDataType

    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'AppSubsidence_v50_New'
    INTEGER                     :: ErrorCode,NNodes,NLayers,NRegn,indxLayer,NN,indxNode,iCount,indx_L
    REAL(8)                     :: Factors(Stratigraphy%NLayers),THK,NN1,THKMIN,rInterbedDZ, &
                                   rInitHeads(AppGrid%NNodes,Stratigraphy%NLayers),          &
                                   rPreCompactHead(AppGrid%NNodes,Stratigraphy%NLayers)
    CHARACTER                   :: cErrorMsg*300,cICFileName*1200,ALine*1200,cVarNames(Stratigraphy%NLayers)*15
    TYPE(GenericFileType)       :: SubsMainFile
    CHARACTER(LEN=14),PARAMETER :: cFormat = '(50(F12.3,2X))'
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    TYPE(SubLayerDataType)      :: LayerData(AppGrid%NNodes,Stratigraphy%NLayers)
    
    !Initialize
    iStat = 0
    
    !Return if no filename is given
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('   Instantiating subsidence component ...')
    
    !Initialize
    NNodes  = AppGrid%NNodes
    NRegn   = AppGrid%NSubregions
    NLayers = Stratigraphy%NLayers
    
    !Open file
    CALL SubsMainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='subsidence data main input',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (AppSubsidence%ElasticSC(NNodes,NLayers)        ,  &
              AppSubsidence%InelasticSC(NNodes,NLayers)      ,  &
              AppSubsidence%InterbedThick_P(NNodes,NLayers)  ,  &
              AppSubsidence%InterbedThick(NNodes,NLayers)    ,  &
              AppSubsidence%InterbedThickMin(NNodes,NLayers) ,  &
              AppSubsidence%Subsidence(NNodes,NLayers)       ,  &
              AppSubsidence%CumSubsidence_P(NNodes,NLayers)  ,  &
              AppSubsidence%CumSubsidence(NNodes,NLayers)    ,  &
              AppSubsidence%Kvsub(NNodes,NLayers)            ,  &
              AppSubsidence%rNEQ(NNodes,NLayers)             ,  &
              AppSubsidence%iDataIndex_S(NNodes,NLayers)     ,  &
              AppSubsidence%iDataIndex_L(NNodes,NLayers)     ,  &
              AppSubsidence%RegionalCumSubsidence(NRegn)     ,  &
              AppSubsidence%RegionalCumSubsidence_P(NRegn)   ,  &
              STAT=ErrorCode , ERRMSG=cErrorMsg              )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error allocating memory for subsidence parameters!'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize values
    AppSubsidence%InterbedThick_P         = 0d0 
    AppSubsidence%InterbedThick           = 0d0 
    AppSubsidence%InterbedThickMin        = 0d0 
    AppSubsidence%Subsidence              = 0d0 
    AppSubsidence%CumSubsidence_P         = 0d0
    AppSubsidence%CumSubsidence           = 0d0
    AppSubsidence%RegionalCumSubsidence_P = 0d0
    rPreCompactHead                       = HUGE(0d0)
    
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
            CALL SetLastMessage('Error allocating memory for subsidence Tecplot file output.'//f_cLineFeed//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
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
            CALL AppSubsidence%TecplotFile%PrintInitialValues(AppGrid,StrmConnectivity,AppSubsidence%Subsidence,Factors,cFormat,cVarNames,TimeStep)
        END IF
    END IF
    
    !Final results output file
    CALL SubsMainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (AppSubsidence%FinalSubsFile , STAT=ErrorCode , ERRMSG=cErrorMsg)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error allocating memory for end-of-simulation subsidence output file.'//f_cLineFeed//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
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
        IF (AppSubsidence%FinalSubsFile%iGetFileType() .NE. f_iTXT) THEN
            CALL SetLastMessage('End-of-simulation subsidence output file must be a text file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        AppSubsidence%lFinalSubsFile_Defined = .TRUE.
    END IF
    
    !Output unit and conversion factor
    CALL SubsMainFile%ReadData(AppSubsidence%FactorLen,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL SubsMainFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;  ALine = StripTextUntilCharacter(ALine,'/')
    AppSubsidence%cUnitLen = ADJUSTL(TRIM(ALine))
    
    !Preferred initial discretization
    CALL SubsMainFile%ReadData(rInterbedDZ,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    
    !Subsidence hydrograph output data
    ALLOCATE (AppSubsidence%SubsHydOutput ,STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for subsidence printing at user-specified locations!'//f_cLineFeed//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL AppSubsidence%SubsHydOutput%New(IsForInquiry,SubsMainFile,cWorkingDirectory,AppGrid,Stratigraphy,iGWNodeIDs,f_iHyd_Subsidence,AppSubsidence%cUnitLen,'TOTAL_CHANGE_THICK',TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    AppSubsidence%lSubsHydOutput_Defined = AppSubsidence%SubsHydOutput%IsDefined()
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        IF (.NOT. IsForInquiry) CALL AppSubsidence%SubsHydOutput%PrintResults(Stratigraphy,f_iHyd_Subsidence,AppSubsidence%Subsidence,AppSubsidence%FactorLen,TimeStep,.FALSE.)
    ELSE
        DEALLOCATE (AppSubsidence%SubsHydOutput , STAT=ErrorCode)
    END IF
    
    !Read subsidence parameters
    CALL ReadSubsidenceParameters(NLayers                        , &
                                  iGWNodeIDs                     , &
                                  AppGrid                        , &
                                  SubsMainFile                   , &
                                  AppSubsidence%ElasticSC        , &
                                  AppSubsidence%InelasticSC      , &
                                  AppSubsidence%InterbedThick    , &
                                  AppSubsidence%InterbedThickMin , &
                                  rPreCompactHead                , &
                                  AppSubsidence%Kvsub            , &
                                  AppSubsidence%rNEQ             , &
                                  iStat                          )
    IF (iStat .EQ. -1) RETURN
    
    !Read initial interbed heads, if defined
    CALL ReadInitialHeads(SubsMainFile,iGWNodeIDs,AppSubsidence%lInitHeadsFromGW,rInitHeads,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read initial interbed thickness, pre-compaction head and initial interbed heads to overwrite the previous values
    CALL ReadOverwriteICData(cICFileName,iGWNodeIDs,AppSubsidence%InterbedThick,rPreCompactHead,AppSubsidence%lInitHeadsFromGW,rInitHeads,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AppSubsidence%InterbedThick_P = AppSubsidence%InterbedThick
    
    !Process and organize interbed data arrays
    iCount                      = 0
    AppSubsidence%iNSubLayerMax = 0
    DO indxLayer=1,NLayers
        DO indxNode=1,NNodes
            THK    = AppSubsidence%InterbedThick(indxNode,indxLayer)
            THKMIN = AppSubsidence%InterbedThickMin(indxNode,indxLayer)
            IF (THK .LE. THKMIN) THEN
                NN = 1
            ELSE
                NN1 = (THK/rInterbedDZ + 1.0)/2.0
                NN  = NINT(NN1)
                IF (NN .LT. 2) NN = 2
            END IF
            iCount = iCount + NN 
            ALLOCATE (LayerData(indxNode,indxLayer)%PreCompactHead(NN) , &
                      LayerData(indxNode,indxLayer)%HSUB(NN)           , &
                      LayerData(indxNode,indxLayer)%HSUBOLD(NN)        )
            
            !Max number of layers that a given interbed is divided into
            IF (NN .GT. AppSubsidence%iNSubLayerMax) AppSubsidence%iNSubLayerMax = NN
            
            !Initial data
            LayerData(indxNode,indxLayer)%PreCompactHead = rPreCompactHead(indxNode,indxLayer)
            LayerData(indxNode,indxLayer)%HSUB           = rInitHeads(indxNode,indxLayer)
            LayerData(indxNode,indxLayer)%HSUBOLD        = rInitHeads(indxNode,indxLayer)
        END DO
    END DO
    
    !Copy layer data into 1-D arrays
    ALLOCATE (AppSubsidence%PreCompactHead(iCount) , AppSubsidence%HSUB(iCount) , AppSubsidence%HSUBOLD(iCount))
    indx_L = 0
    DO indxLayer=1,NLayers
        DO indxNode=1,NNodes
            NN                                             = SIZE(LayerData(indxNode,indxLayer)%PreCompactHead)
            iCount                                         = indx_L + 1
            AppSubsidence%iDataIndex_S(indxNode,indxLayer) = iCount
            indx_L                                         = iCount + NN - 1
            AppSubsidence%iDataIndex_L(indxNode,indxLayer) = indx_L
            AppSubsidence%PreCompactHead(iCount:indx_L)    = LayerData(indxNode,indxLayer)%PreCompactHead
            AppSubsidence%HSUB(iCount:indx_L)              = LayerData(indxNode,indxLayer)%HSUB
            AppSubsidence%HSUBOLD(iCount:indx_L)           = LayerData(indxNode,indxLayer)%HSUBOLD
        END DO
    END DO
    
    !Close file
    CALL SubsMainFile%Kill()

  END SUBROUTINE AppSubsidence_v50_New
  
  
 
  
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
  SUBROUTINE AppSubsidence_v50_KillImplementation(AppSubsidence)
    CLASS(AppSubsidence_v50_Type) :: AppSubsidence
    
    !Local variables
    INTEGER                      :: ErrorCode
    TYPE(AppSubsidence_v50_Type) :: Dummy
    
    DEALLOCATE (AppSubsidence%KvSub          , &
                AppSubsidence%rNEQ           , &
                AppSubsidence%iDataIndex_S   , &
                AppSubsidence%iDataIndex_L   , &
                AppSubsidence%PreCompactHead , &
                AppSubsidence%HSUB           , &
                AppSubsidence%HSUBOLD        , &
                STAT = ErrorCode             )
    
    !Set attributes to defaults
    SELECT TYPE (AppSubsidence)
        TYPE IS (AppSubsidence_v50_Type)
            AppSubsidence = Dummy
    END SELECT
    
  END SUBROUTINE AppSubsidence_v50_KillImplementation
  
  
  
  
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
  ! --- GET VERSION NUMBER 
  ! -------------------------------------------------------------
  FUNCTION AppSubsidence_v50_GetVersion(AppSubsidence) RESULT(cVrs)
    CLASS(AppSubsidence_v50_Type) :: AppSubsidence
    CHARACTER(:),ALLOCATABLE      :: cVrs
    
    IF (.NOT. AppSubsidence%Version%IsDefined())   &
        AppSubsidence%Version = AppSubsidence%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = AppSubsidence%Version%GetVersion()
    
  END FUNCTION AppSubsidence_v50_GetVersion

  
  
  
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
  SUBROUTINE AppSubsidence_v50_ReadRestartData(AppSubs,InFile,iStat)
    CLASS(AppSubsidence_v50_Type) :: AppSubs
    TYPE(GenericFileType)         :: InFile
    INTEGER,INTENT(OUT)           :: iStat
       
    CALL InFile%ReadData(AppSubs%PreCompactHead,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubs%HSUB,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubs%HSUBOLD,iStat)         
    
  END SUBROUTINE AppSubsidence_v50_ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ INITIAL INTERBED HEADS
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialHeads(InFile,iGWNodeIDs,lInitHeadsFromGW,rInitHeads,iStat)
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(IN)    :: iGWNodeIDs(:)
    LOGICAL,INTENT(OUT)   :: lInitHeadsFromGW
    REAL(8)               :: rInitHeads(:,:)
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'ReadInitialHeads'
    INTEGER                      :: ID,indxNode,iNLayers,iNNodes,iNode,iStat_Local
    REAL(8)                      :: rFact,rDummyArray(1+SIZE(rInitHeads,DIM=2))
    LOGICAL                      :: lProcessed(SIZE(iGWNodeIDs))
    
    !Initialize
    iStat            = 0
    lInitHeadsFromGW = .TRUE.
    iNNodes          = SIZE(rInitHeads , DIM=1)
    iNLayers         = SIZE(rInitHeads , DIM=2)
    
    !Are initial heads even defined?
    CALL InFile%ReadData(rFact,iStat_Local)
    IF (iStat_Local .NE. 0) RETURN
    
    !Read initial heads
    lProcessed = .FALSE.
    DO indxNode=1,iNNodes
        CALL InFile%ReadData(rDummyArray,iStat_Local)
        IF (iStat_Local .NE. 0) RETURN
        ID    = INT(rDummyArray(1))
        iNode = LocateInList(ID,iGWNodeIDs)
        IF (iNode .EQ. 0) THEN 
            CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for initial interbed heads (for subsidence) is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        !Make sure node is not entered more than once
        IF (lProcessed(iNode)) THEN
            CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for initial interbed heads (for subsidence)!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iNode)   = .TRUE.
        rInitHeads(iNode,:) = rDummyArray(2:) * rFact
    END DO
    
    !If made this far, it means initial heads are assigned
    lInitHeadsFromGW = .FALSE.
    
  END SUBROUTINE ReadInitialHeads
  
  
  ! -------------------------------------------------------------
  ! --- READ OVERWRITING INITIAL INTERBED THICKNESS AND PRE-COMPACTION HEAD
  ! -------------------------------------------------------------
  SUBROUTINE ReadOverWriteICData(cICFileName,iGWNodeIDs,InterbedThick,PreCompactHead,lInitHeadsFromGW,rInitHeads,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cICFileName
    INTEGER,INTENT(IN)          :: iGWNodeIDs(:)
    REAL(8)                     :: InterbedThick(:,:),PreCompactHead(:,:),rInitHeads(:,:)
    LOGICAL                     :: lInitHeadsFromGW
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'ReadOverWriteICData'
    TYPE(GenericFileType)        :: ICFile
    INTEGER                      :: ID,indxNode,iNLayers,iNNodes,iNode
    REAL(8)                      :: rFact,rDummyArray(1+2*SIZE(InterbedThick,DIM=2))
    LOGICAL                      :: lProcessed(SIZE(iGWNodeIDs))
    
    !If IC filename is empty return
    IF (cICFileName .EQ. '') THEN
        iStat = 0
        RETURN
    END IF
    
    !Initialize
    lProcessed = .FALSE.
    iNNodes    = SIZE(InterbedThick , DIM=1)
    iNLayers   = SIZE(InterbedThick , DIM=2)
    
    !Open IC file
    CALL ICFile%New(FileName=cICFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='subsidence initial conditions data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Conversion factor
    CALL ICFile%ReadData(rFact,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read initial interbed thickness and pre-compaction heads
    DO indxNode=1,iNNodes
        CALL ICFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Make gw node ID is legit and not defined more than once
        ID = rDummyArray(1)
        CALL ConvertID_To_Index(ID,iGWNodeIDs,iNode)
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for subsidence initial conditions is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (lProcessed(iNode)) THEN
            CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for subsidence initial conditions definitions!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iNode) = .TRUE.
        
        !Interbed thickness
        InterbedThick(iNode,:) = rDummyArray(2:1+iNLayers) * rFact
        
        !Pre-compaction head
        PreCompactHead(iNode,:) = rDummyArray(iNLayers+2:) * rFact
        
    END DO
    
    !Read initial heads
    CALL ReadInitialHeads(ICFile,iGWNodeIDs,lInitHeadsFromGW,rInitHeads,iStat)
    
    !Close file
    CALL ICFile%Kill()
    
  END SUBROUTINE ReadOverWriteICData
  
  
  ! -------------------------------------------------------------
  ! --- READ SUBSIDENCE PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE ReadSubsidenceParameters(NLayers,iGWNodeIDs,AppGrid,InFile,ElasticSC,InelasticSC,InterbedThick,InterbedThickMin,PreCompactHead,Kvsub,rNEQ,iStat)
    INTEGER,INTENT(IN)           :: NLayers
    INTEGER,INTENT(IN)           :: iGWNodeIDs(:)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(GenericFileType)        :: InFile
    REAL(8),INTENT(OUT)          :: ElasticSC(:,:),InelasticSC(:,:),InterbedThick(:,:),InterbedThickMin(:,:),PreCompactHead(:,:),Kvsub(:,:),rNEQ(:,:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ReadSubsidenceParameters'
    INTEGER                      :: NGroup,NNodes,indxNode,indxLayer,ID,iNode
    REAL(8)                      :: rFactors(7),rDummyArray(8),rDummy3DArray(AppGrid%NNodes,NLayers,7),rArea
    LOGICAL                      :: lProcessed(AppGrid%NNodes)
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    
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
    !rFActors(7): for Vertical Conductivity
    
    !Non-parametric data input
    IF (NGroup .EQ. 0) THEN
        DO indxNode=1,NNodes
            DO indxLayer=1,NLayers
                IF (indxLayer .EQ. 1) THEN
                    CALL InFile%ReadData(rDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
                    ID = INT(rDummyArray(1))
                    CALL ConvertID_To_Index(ID,iGWNodeIDs,iNode)
                    IF (iNode .EQ. 0) THEN 
                        CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' listed for subsidence parameters is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    !Make sure node is not entered more than once
                    IF (lProcessed(iNode)) THEN
                        CALL SetLastMessage('Groundwater node ID '//TRIM(IntToText(ID))//' is listed more than once for subsidence parameter definition!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    lProcessed(iNode) = .TRUE.
                ELSE
                    CALL InFile%ReadData(rDummyArray(2:),iStat)  ;  IF (iStat .EQ. -1) RETURN
                END IF
                
                rArea                             = AppGrid%AppNode(iNode)%Area
                ElasticSC(iNode,indxLayer)        = rDummyArray(2) * rFactors(2) * rArea
                InelasticSC(iNode,indxLayer)      = rDummyArray(3) * rFactors(3) * rArea
                InterbedThick(iNode,indxLayer)    = rDummyArray(4) * rFactors(4)
                InterbedThickMin(iNode,indxLayer) = rDummyArray(5) * rFactors(5)
                PreCompactHead(iNode,indxLayer)   = rDummyArray(6) * rFactors(6)
                Kvsub(iNode,indxLayer)            = rDummyArray(7) * rFactors(7) * rArea
                rNEQ(iNode,indxLayer)             = rDummyArray(8)
            END DO
        END DO
    END IF

    !Parametric data input
    IF (NGroup .GT. 0) THEN

        !Read the parameter values at parametric nodes and compute the interpolation coefficients for finite element nodes
        CALL GetValuesFromParametricGrid(InFile,AppGrid%GridType,iGWNodeIDs,NGroup,rFactors,.FALSE.,'subsidence',rDummy3DArray,iStat)
        IF (iStat .EQ. -1) RETURN

        !Initialize parameter values
        DO indxLayer=1,NLayers
            DO indxNode=1,NNodes
                rArea                                = AppGrid%AppNode(indxNode)%Area
                ElasticSC(indxNode,indxLayer)        = rDummy3DArray(indxNode,indxLayer,1) * rArea
                InelasticSC(indxNode,indxLayer)      = rDummy3DArray(indxNode,indxLayer,2) * rArea
                InterbedThick(indxNode,indxLayer)    = rDummy3DArray(indxNode,indxLayer,3)
                InterbedThickMin(indxNode,indxLayer) = rDummy3DArray(indxNode,indxLayer,4)
                PreCompactHead(indxNode,indxLayer)   = rDummy3DArray(indxNode,indxLayer,5)
                Kvsub(indxNode,indxLayer)            = rDummy3DArray(indxNode,indxLayer,6) * rArea
                rNEQ(indxNode,indxLayer)             = rDummy3DArray(indxNode,indxLayer,7)
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
  SUBROUTINE AppSubsidence_v50_PrintRestartData(AppSubs,OutFile)
    CLASS(AppSubsidence_v50_Type),INTENT(IN) :: AppSubs
    TYPE(GenericFileType)                    :: OutFile
    
    CALL OutFile%WriteData(AppSubs%PreCompactHead)
    CALL OutFile%WriteData(AppSubs%HSUB)
    CALL OutFile%WriteData(AppSubs%HSUBOLD)
    
  END SUBROUTINE AppSubsidence_v50_PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT FINAL SUBSIDENCE PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v50_PrintParameters(AppSubs,iGWNodeIDs,NodeAreas)
    CLASS(AppSubsidence_v50_Type),INTENT(IN) :: AppSubs
    INTEGER,INTENT(IN)                       :: iGWNodeIDs(:)
    REAL(8),INTENT(IN)                       :: NodeAreas(:)
    
    !Local variables
    INTEGER :: indxLayer,indxNode,NNodes,NLayers,indx_S
    CHARACTER :: Text*500
    
    !Initialize
    NNodes  = SIZE(AppSubs%Subsidence , DIM=1)
    NLayers = SIZE(AppSubs%Subsidence , DIM=2)
    
    !Print parameters
    CALL LogMessage('',f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT('-',100),f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT(' ',30)//'SUBSIDENCE PARAMETER VALUES FOR EACH NODE',f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT(' ',12)//'*** Note: Values Below are After Multiplication by Conversion Factors ***',f_iMessage,'',f_iFILE)
    CALL LogMessage(REPEAT('-',100),f_iMessage,'',f_iFILE)
    WRITE (Text,'(A,2X,7(A,2X))')            &
      '   NODE','        SCE             '   &
               ,'        SCI             '   &
               ,'        DC              '   &
               ,'        DCMIN           '   &
               ,'        HC              '   &
               ,'        KV              '   &
               ,'        NEQ             '
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)

    DO indxNode=1,NNodes
      DO indxLayer=1,NLayers                                                                                     
        indx_S = AppSubs%iDataIndex_S(indxNode,indxLayer)
        IF (indxLayer .EQ. 1) THEN  
          WRITE (Text,'(I7,2X,10(1PG24.15E3,2X))')                                                                                                                   & 
               iGWNodeIDs(indxNode) ,AppSubs%ElasticSC(indxNode,indxLayer) / NodeAreas(indxNode) , AppSubs%InelasticSC(indxNode,indxLayer) / NodeAreas(indxNode) ,   & 
                                     AppSubs%InterbedThick(indxNode,indxLayer)                   , AppSubs%interbedThickMin(indxNode,indxLayer)                  ,   &
                                     AppSubs%PreCompactHead(indx_S)                              , AppSubs%Kvsub(indxNode,indxLayer)                             ,   &
                                     AppSubs%rNEQ(indxNode,indxLayer)
        ELSE                                                                                          
          WRITE (Text,'(9X,10(1PG24.15E3,2X))')                                                                                                          &  
                         AppSubs%ElasticSC(indxNode,indxLayer) / NodeAreas(indxNode) , AppSubs%InelasticSC(indxNode,indxLayer) / NodeAreas(indxNode) ,   & 
                         AppSubs%InterbedThick(indxNode,indxLayer)                   , AppSubs%interbedThickMin(indxNode,indxLayer)                  ,   &
                         AppSubs%PreCompactHead(indx_S)                              , AppSubs%Kvsub(indxNode,indxLayer)                             ,   &
                         AppSubs%rNEQ(indxNode,indxLayer)
        END IF                                                                                          
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)                                                                       
      END DO                                                                                          
    END DO  
    CALL LogMessage('',f_iMessage,'',f_iFILE)

  END SUBROUTINE AppSubsidence_v50_PrintParameters

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT END-OF-SIMULATION VALUES
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v50_PrintFinalSubs(AppSubs,AppGrid,TimeStep)
    CLASS(AppSubsidence_v50_Type) :: AppSubs
    TYPE(AppGridType),INTENT(IN)  :: AppGrid 
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    
    !Local variables
    INTEGER   :: indxLayer,indxNode,iNLayers,iNNodes,indx_L
    REAL(8)   :: rWorkArray(2*SIZE(AppSubs%InterbedThick,DIM=2))
    CHARACTER :: SimulationTime*21,Text*1000,cLayer*7
    
    !Initialize
    iNNodes  = SIZE(AppSubs%InterbedThick , DIM=1)
    iNLayers = SIZE(AppSubs%InterbedThick , DIM=2)
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF
    
    !Print header
    CALL AppSubs%FinalSubsFile%WriteData('C'//REPEAT('*',79))
    CALL AppSubs%FinalSubsFile%WriteData('C ***** SUBSIDENCE DATA AT '//TRIM(SimulationTime))
    CALL AppSubs%FinalSubsFile%WriteData('C'//REPEAT('*',79))
    CALL AppSubs%FinalSubsFile%WriteData('C')    
    CALL AppSubs%FinalSubsFile%WriteData('C'//REPEAT('-',79))
    CALL AppSubs%FinalSubsFile%WriteData('     1.0                           / FACT')
    CALL AppSubs%FinalSubsFile%WriteData('C'//REPEAT('-',79))
    Text = 'C      ID           DC[1]'
    DO indxLayer=2,iNLayers
        cLayer = ADJUSTL('DC['//TRIM(IntToText(indxLayer))//']')
        WRITE (Text,'(3A)') TRIM(Text),REPEAT(' ',18-LEN_TRIM(cLayer)),TRIM(cLayer)
    END DO
    DO indxLayer=1,iNLayers
        cLayer = ADJUSTL('HC['//TRIM(IntToText(indxLayer))//']')
        WRITE (Text,'(3A)') TRIM(Text),REPEAT(' ',18-LEN_TRIM(cLayer)),TRIM(cLayer)
    END DO
    CALL AppSubs%FinalSubsFile%WriteData(TRIM(Text))
    CALL AppSubs%FinalSubsFile%WriteData('C'//REPEAT('-',79))
    
    !Print final interbed thickness and pre-compaction heads of the last interbed layer
    DO indxNode=1,iNNodes
        rWorkArray(1:iNLayers) = AppSubs%InterbedThick(indxNode,:)
        DO indxLayer=1,iNLayers
            indx_L                         = AppSubs%iDataIndex_L(indxNode,indxLayer)
            rWorkArray(iNLayers+indxLayer) = AppSubs%PreCompactHead(indx_L)
        END DO
        WRITE (Text,'(I8,100F18.6)') AppGrid%AppNode(indxNode)%ID,rWorkArray
        CALL AppSubs%FinalSubsFile%WriteData(Text)
    END DO
    
  END SUBROUTINE AppSubsidence_v50_PrintFinalSubs    
  

  
  
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
  ! --- SIMULATE THE EFFECT OF SUBSIDENCE ON R.H.S. VECTOR AND COEFF MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v50_Simulate(AppSubsidence,Stratigraphy,GWHead,GWHead_P,rStorage,rdStorage,Matrix)
    CLASS(AppSubsidence_v50_Type)     :: AppSubsidence
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:),rStorage(:,:),rdStorage(:,:)  !GWHead_P is not used in this version
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER,PARAMETER                              :: iCompIDs(1) = [f_iGWComp]
    INTEGER                                        :: indxLayer,indxNode,iBase,NNodes,NLayers,iGWNode,iNodeIDs(1),NN,I,indx_S,indx_L, &
                                                      indx
    REAL(8)                                        :: rGWHead,rInterbedThick,PreCompactHead,rTopElev,rBottomElev,KVA,DZ,KVADZ,        &
                                                      DZDT,h1,SKMA,h1OLD,rNEQ,ElasticSC,InelasticSC,rUpdateValues(1),rStor,rStorPos,  &
                                                      rUpdateRHS(SIZE(GWHead)),rScaleFactor,rFlow,rConductance,rdStor,rdStorPos,rDiff,&
                                                      rGradient,rdFlow
    REAL(8),DIMENSION(AppSubsidence%iNSubLayerMax) :: A1,B1,C1,R1
    
    !Initialize
    NNodes  = SIZE(GWHead,DIM=1)
    NLayers = SIZE(GWHead,DIM=2)
    
    DO indxLayer=1,NLayers
        iBase = (indxLayer-1) * NNodes
        DO indxNode=1,NNodes           
            iGWNode = iBase + indxNode
            
            !Cycle if node is inactive
            IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) THEN
                rUpdateRHS(iGWNode) = 0.0
                CYCLE
            END IF
            
            !Interbed thickness, cycle if minimum interbed thickness is attained
            rInterbedThick = AppSubsidence%InterbedThick_P(indxNode,indxLayer) 
            IF (rInterbedThick .LE. AppSubsidence%InterbedThickMin(indxNode,indxLayer)) THEN
                rUpdateRHS(iGWNode) = 0.0
                CYCLE
            END IF
            
            !Stratigraphy details
            rTopElev    = Stratigraphy%TopElev(indxNode,indxLayer)
            rBottomElev = Stratigraphy%BottomElev(indxNode,indxLayer)
            
            !Groundwater head
            rGWHead = GWHead(indxNode,indxLayer)
            
            !Scale interbed thickness based on saturated portion of the aquifer
            IF (rGWHead .LT. rTopElev) THEN
                IF (rGWHead .GT. rBottomElev) THEN
                    rScaleFactor   = (rGWHead-rBottomElev) / (rTopElev-rBottomElev)
                    rInterbedThick = rInterbedThick * rScaleFactor
                ELSE
                    rUpdateRHS(iGWNode) = 0.0
                    CYCLE
                END IF
            END IF
            
            !Values for computation
            ElasticSC   = AppSubsidence%ElasticSC(indxNode,indxLayer)    !ALREADY MULTIPLIED BY AREA [L2/L]
            InelasticSC = AppSubsidence%InelasticSC(indxNode,indxLayer)  !ALREADY MULTIPLIED BY AREA [L2/L]
            
            !Set and solve time-delayed diffusion equation
            indx_S = AppSubsidence%iDataIndex_S(indxNode,indxLayer)
            indx_L = AppSubsidence%iDataIndex_L(indxNode,indxLayer)
            NN     = indx_L - indx_S + 1
            KVA    = AppSubsidence%Kvsub(indxNode,indxLayer) !ALREADY MULTIPLIED BY AREA [L3/T]
            DZ     = rInterbedThick / (2d0*REAL(NN,8)-1d0)
            KVADZ  = KVA / DZ
            DZDT   = DZ
            A1     = 0.0
            B1     = 0.0
            C1     = 0.0
            R1     = 0.0
!
            PreCompactHead = AppSubsidence%PreCompactHead(indx_S)
            h1             = AppSubsidence%HSUB(indx_S)  
            h1OLD          = AppSubsidence%HSUBOLD(indx_S)
            IF (h1 .GT. PreCompactHead) THEN
                SKMA = ElasticSC
            ELSE
                SKMA = InelasticSC
            END IF
            B1(1) = -3d0*KVADZ - DZDT*SKMA
            C1(1) = KVADZ
            R1(1) = DZDT*( -SKMA*PreCompactHead + ElasticSC*(PreCompactHead - h1OLD)) - 2d0*KVADZ*rGWHead
            indx  = 1
            DO I=indx_S+1,indx_L-1
                PreCompactHead = AppSubsidence%PreCompactHead(I)
                h1             = AppSubsidence%HSUB(I)
                h1OLD          = AppSubsidence%HSUBOLD(I)
                IF (h1 .GT. PreCompactHead) THEN
                    SKMA = ElasticSC
                ELSE
                    SKMA = InelasticSC
                END IF
                indx     = indx + 1
                A1(indx) = KVADZ
                B1(indx) = -2d0*KVADZ - DZDT*SKMA
                C1(indx) = KVADZ
                R1(indx) = DZDT*( -SKMA*PreCompactHead + ElasticSC*(PreCompactHead - h1OLD))
            END DO
            PreCompactHead = AppSubsidence%PreCompactHead(indx_L)
            h1             = AppSubsidence%HSUB(indx_L)
            h1OLD          = AppSubsidence%HSUBOLD(indx_L)
            IF (h1 .GT. PreCompactHead) THEN
                SKMA = ElasticSC
            ELSE
                SKMA = InelasticSC
            END IF
            A1(NN) = 2d0 * KVADZ
            B1(NN) = -2d0*KVADZ - DZDT*SKMA
            R1(NN) = DZDT*( -SKMA*PreCompactHead + ElasticSC*(PreCompactHead - h1OLD))
!
            CALL TRIDIAGONAL(A1,B1,C1,R1,AppSubsidence%HSUB(indx_S:indx_L),NN)
            
            !Calculate Newton terms
            rNEQ         = AppSubsidence%rNEQ(indxNode,indxLayer)
            h1           = AppSubsidence%HSUB(indx_S)
            rConductance = 4d0 * KVADZ * rNEQ
            rFlow        = rConductance * (rGWHead-h1)  !ALREADY MULTIPLIED BY 2 TO ACCOUNT FOR BOTH HALVES OF INTERBED
            !Limit outflow from aquifer with storage (+ flow means outflow from gw)
            IF (rFlow .GT. 0.0) THEN
                rStor = rStorage(indxNode,indxLayer)
                IF (rFlow .GT. rStor) THEN
                    rdStor    = rdStorage(indxNode,indxLayer)
                    rStorPos  = MAX(rStor , 0.0)
                    rDiff     = rStorPos - rFlow
                    rFlow     = MIN(rStorPos , rFlow)
                    rdStorPos = 0.5d0 * rdStor * (1d0+rStor/SQRT(rStor*rStor+f_rSmoothMaxP))
                    rdFlow    = rConductance
                    rGradient = rdStorPos - 0.5d0 * (1d0 + rDiff/SQRT(rDiff*rDiff+f_rSmoothMaxP)) * (rdStorPos-rdFlow)
                ELSE
                    rGradient = rConductance
                END IF
            ELSE
                rGradient = rConductance
            END IF
            
            !R.H.S. function
            rUpdateRHS(iGWNode) = rFlow 
            
            !Coeff. matrix
            iNodeIDs(1)      = iGWNode 
            rUpdateValues(1) = rGradient
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,1,iCompIDs,iNodeIDs,rUpdateValues)
            
       END DO
    END DO
    
    !Update R.H.S. vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)
  
  END SUBROUTINE AppSubsidence_v50_Simulate

  
  ! -------------------------------------------------------------
  ! --- PROCESS SUBSIDENCE PARAMETERS TO BE USED IN SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v50_ProcessSubsidenceParameters(AppSubsidence,GWHead)
    CLASS(AppSubsidence_v50_Type) :: AppSubsidence
    REAL(8),INTENT(IN)            :: GWHead(:,:)    !Not used in this version
    
    !Local variables
    INTEGER :: indxLayer,indxNode,iNNodes,iNLayers,indx_S,indx_L
    
    !Initilaize
    iNNodes  = SIZE(AppSubsidence%iDataIndex_S , DIM=1)
    iNLayers = SIZE(AppSubsidence%iDataIndex_S , DIM=2)
    
    !Set initial heads to gw heads, if needed
    IF (AppSubsidence%lInitHeadsFromGW) THEN
        DO indxLayer=1,iNLayers
            DO indxNode=1,iNNodes
                indx_S                               = AppSubsidence%iDataIndex_S(indxNode,indxLayer)
                indx_L                               = AppSubsidence%iDataIndex_L(indxNode,indxLayer) 
                AppSubsidence%HSUB(indx_S:indx_L)    = GWHead(indxNode,indxLayer)
                AppSubsidence%HSUBOLD(indx_S:indx_L) = GWHead(indxNode,indxLayer)
            END DO
        END DO
    END IF
    
    !Process
    AppSubsidence%PreCompactHead = MIN(AppSubsidence%PreCompactHead , AppSubsidence%HSUB)
    
  END SUBROUTINE AppSubsidence_v50_ProcessSubsidenceParameters
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE SUBSIDENCE RELATED TERMS
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v50_UpdateSubsidence(AppSubsidence,AppGrid,Stratigraphy,GWHead,GWHead_P)
    CLASS(AppSubsidence_v50_Type)     :: AppSubsidence
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:)
    
    !Local variables
    INTEGER         :: indxNode,indxLayer,iNNodes,iNLayers,NN,I,indx_S,indx_L
    REAL(8)         :: rGWHead,rGWHead_P,ElasticSC,InelasticSC,rInterbedThick,Area,rTopElev,rBottomElev,rScaleFactor, &
                       PreCompactHead,Subsidence,DS,DZ,h1,h1OLD
    
    !Initialize
    iNNodes  = SIZE(GWHead , DIM=1)
    iNLayers = SIZE(GWHead , DIM=2)
    
    DO indxLayer=1,iNLayers
        DO indxNode=1,iNNodes
          IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) CYCLE
          
          !Cycle if the minimum interbed thickness is attained
          rInterbedThick = AppSubsidence%InterbedThick_P(indxNode,indxLayer)
          IF (rInterbedThick .LE. AppSubsidence%InterbedThickMin(indxNode,indxLayer)) THEN
              AppSubsidence%Subsidence(indxNode,indxLayer) = 0.0
              CYCLE
          END IF
          rGWHead   = GWHead(indxNode,indxLayer)
          rGWHead_P = GWHead_P(indxNode,indxLayer)
          
          !Scale interbed thickness for the staurated thickness of the aquifer
          rTopElev    = Stratigraphy%TopElev(indxNode,indxLayer)
          rBottomElev = Stratigraphy%BottomElev(indxNode,indxLayer)
          IF (rGWHead .LT. rTopElev) THEN
              IF (rGWHead .GT. rBottomElev) THEN
                  rScaleFactor   = (rGWHead-rBottomElev) / (rTopElev-rBottomElev)
                  rInterbedThick = rInterbedThick * rScaleFactor
              ELSE
                  AppSubsidence%Subsidence(indxNode,indxLayer) = 0.0
                  CYCLE
              END IF
          END IF
          
          Area        = AppGrid%AppNode(indxNode)%Area
          ElasticSC   = AppSubsidence%ElasticSC(indxNode,indxLayer) / Area
          InelasticSC = AppSubsidence%InelasticSC(indxNode,indxLayer) / Area
        
          Subsidence = 0.0
          indx_S     = AppSubsidence%iDataIndex_S(indxNode,indxLayer)
          indx_L     = AppSubsidence%iDataIndex_L(indxNode,indxLayer)
          NN         = indx_L - indx_S + 1
          DZ         = rInterbedThick/(2d0*REAL(NN,8)-1d0)
          DO I=indx_S,indx_L
              PreCompactHead = AppSubsidence%PreCompactHead(I)
              h1             = AppSubsidence%HSUB(I)
              h1OLD          = AppSubsidence%HSUBOLD(I)
              IF (h1 .GT. PreCompactHead) THEN
                  DS = -ElasticSC * DZ * (h1-h1OLD)
              ELSE            
                  DS = -InelasticSC * DZ * (h1-PreCompactHead)    &
                       -ElasticSC * DZ * (PreCompactHead-h1OLD)
                  AppSubsidence%PreCompactHead(I) = h1
              END IF
              IF (I .EQ. indx_L) THEN
                  Subsidence = Subsidence + DS        !NOT MULTIPLIED BY 2 BECAUSE THERE IS ONLY ONE NN LAYER 
              ELSE
                  Subsidence = Subsidence + 2d0 * DS  !MULTIPLIED BY 2 TO ACCOUNT FOR BOTH HALVES OF INTERBED
              END IF  
          END DO
          Subsidence = Subsidence * AppSubsidence%rNEQ(indxNode,indxLayer)
          
          AppSubsidence%Subsidence(indxNode,indxLayer)    = Subsidence
          AppSubsidence%InterbedThick(indxNode,indxLayer) = AppSubsidence%InterbedThick_P(indxNode,indxLayer) - Subsidence
          AppSubsidence%CumSubsidence(indxNode,indxLayer) = AppSubsidence%CumSubsidence_P(indxNode,indxLayer) + Subsidence
        END DO
    END DO
    
    !Update regional volumetric cumulative subsidence
    CALL ComputeRegionalCumSubsidence(AppGrid,AppSubsidence%CumSubsidence,AppSubsidence%RegionalCumSubsidence)
    
  END SUBROUTINE AppSubsidence_v50_UpdateSubsidence
    

  ! -------------------------------------------------------------
  ! --- ADVANCE THE STATE OF THE SUBSIDENCE SYSTEM IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v50_AdvanceState(AppSubsidence)
    CLASS(AppSubsidence_v50_Type) :: AppSubsidence
    
    AppSubsidence%InterbedThick_P         = AppSubsidence%InterbedThick
    AppSubsidence%CumSubsidence_P         = AppSubsidence%CumSubsidence
    AppSubsidence%RegionalCumSubsidence_P = AppSubsidence%RegionalCumSubsidence
    AppSubsidence%HSUBOLD                 = AppSubsidence%HSUB
        
  END SUBROUTINE AppSubsidence_v50_AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- TIDIAGONAL SOLVER
  ! -------------------------------------------------------------
  SUBROUTINE TRIDIAGONAL(A,B,C,R,H,NN)
    INTEGER :: NN
    REAL(8) :: A(NN),B(NN),C(NN),R(NN),H(NN)
    
    !Local variables
    INTEGER :: I
    REAL(8) :: W
 
    DO I=2,NN
        W    = A(I) / B(I-1)
        B(I) = B(I) - W*C(I-1)
        R(I) = R(I) - W*R(I-1)
    END DO
    H(NN) = R(NN) / B(NN)
    DO I=NN-1,1,-1
        H(I) = (R(I)-C(I)*H(I+1)) / B(I)
    END DO
 
  END SUBROUTINE TRIDIAGONAL

END MODULE