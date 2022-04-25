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
MODULE Class_AppSubsidence_v40
  USE MessageLogger           , ONLY: SetLastMessage                , &
                                      EchoProgress                  , &
                                      LogMessage                    , &
                                      f_iMessage                    , &
                                      f_iFatal                      , &
                                      f_iFILE
  USE TimeSeriesUtilities     , ONLY: TimeStepType                       
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter       , & 
                                      CleanSpecialCharacters        , &
                                      EstablishAbsolutePathFilename , &
                                      IntToText                     , &
                                      ConvertID_To_Index            , &
                                      f_cLineFeed                             
  USE IOInterface             , ONLY: GenericFileType               , &
                                      f_iTXT                             
  USE Package_Discretization  , ONLY: AppGridType                   , &
                                      StratigraphyType              , &
                                      GetValuesFromParametricGrid   
  USE Package_Misc            , ONLY: f_iHyd_Subsidence             , &
                                      f_iGWComp                     , &
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
  PUBLIC :: AppSubsidence_v40_Type          

 
  ! -------------------------------------------------------------
  ! --- SUBSIDENCE DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseAppSubsidenceType) :: AppSubsidence_v40_Type
      REAL(8),ALLOCATABLE :: PreCompactHead (:,:) !Pre-compaction head at (node,layer)
  CONTAINS
      PROCEDURE,PASS :: New                             => AppSubsidence_v40_New
      PROCEDURE,PASS :: KillImplementation              => AppSubsidence_v40_KillImplementation
      PROCEDURE,PASS :: GetVersion                      => AppSubsidence_v40_GetVersion
      PROCEDURE,PASS :: PrintParameters                 => AppSubsidence_v40_PrintParameters
      PROCEDURE,PASS :: PrintFinalSubs                  => AppSubsidence_v40_PrintFinalSubs
      PROCEDURE,PASS :: PrintRestartData_Implementation => AppSubsidence_v40_PrintRestartData
      PROCEDURE,PASS :: ReadRestartData_Implementation  => AppSubsidence_v40_ReadRestartData
      PROCEDURE,PASS :: Simulate                        => AppSubsidence_v40_Simulate
      PROCEDURE,PASS :: ProcessSubsidenceParameters     => AppSubsidence_v40_ProcessSubsidenceParameters
      PROCEDURE,PASS :: UpdateSubsidence                => AppSubsidence_v40_UpdateSubsidence
      PROCEDURE,PASS :: AdvanceState                    => AppSubsidence_v40_AdvanceState
  END TYPE AppSubsidence_v40_Type
  
  
  ! -------------------------------------------------------------
  ! --- LAKE PACKAGE VERSION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iVersion    = 40
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    ='4.0.0000'
  INCLUDE 'AppSubsidence_v40_Revision.fi'
 
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 25
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppSubsidence_v40::'

  
  
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
  SUBROUTINE AppSubsidence_v40_New(AppSubsidence,IsForInquiry,cFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat) 
    CLASS(AppSubsidence_v40_Type),INTENT(OUT) :: AppSubsidence
    LOGICAL,INTENT(IN)                        :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)               :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)                        :: iGWNodeIDs(:)
    TYPE(AppGridType),INTENT(IN)              :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)         :: Stratigraphy
    COMPLEX,INTENT(IN)                        :: StrmConnectivity(:)
    TYPE(TimeStepType),INTENT(IN)             :: TimeStep
    INTEGER,INTENT(OUT)                       :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'AppSubsidence_v40_New'
    INTEGER                     :: ErrorCode,NNodes,NLayers,NRegn,indxLayer
    REAL(8)                     :: Factors(Stratigraphy%NLayers)
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
              AppSubsidence%PreCompactHead(NNodes,NLayers)   ,  &
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
    AppSubsidence%PreCompactHead          = HUGE(0d0)
    AppSubsidence%RegionalCumSubsidence_P = 0d0
    
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
    CALL ReadSubsidenceParameters(NLayers,iGWNodeIDs,AppGrid,SubsMainFile,AppSubsidence%ElasticSC,AppSubsidence%InelasticSC,AppSubsidence%InterbedThick,AppSubsidence%InterbedThickMin,AppSubsidence%PreCompactHead,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read initial interbed thickness and pre-compaction head to overwrite the previous values
    CALL ReadSubsidenceICData(cICFileName,iGWNodeIDs,AppSubsidence%InterbedThick,AppSubsidence%PreCompactHead,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AppSubsidence%InterbedThick_P = AppSubsidence%InterbedThick
    
    !Close file
    CALL SubsMainFile%Kill()

  END SUBROUTINE AppSubsidence_v40_New
  
  
  
   
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
  ! --- KILL IMPLEMENTATION
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v40_KillImplementation(AppSubsidence)
    CLASS(AppSubsidence_v40_Type) :: AppSubsidence
    
    !Local variables
    INTEGER                      :: ErrorCode
    TYPE(AppSubsidence_v40_Type) :: Dummy
    
    DEALLOCATE (AppSubsidence%PreCompactHead   , &
                STAT = ErrorCode               )
    
    !Set attributes to defaults
    SELECT TYPE (AppSubsidence)
        TYPE IS (AppSubsidence_v40_Type)
            AppSubsidence = Dummy
    END SELECT    
  END SUBROUTINE AppSubsidence_v40_KillImplementation

  
  

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
  FUNCTION AppSubsidence_v40_GetVersion(AppSubsidence) RESULT(cVrs)
    CLASS(AppSubsidence_v40_Type) :: AppSubsidence
    CHARACTER(:),ALLOCATABLE      :: cVrs
    
    IF (.NOT. AppSubsidence%Version%IsDefined())   &
        AppSubsidence%Version = AppSubsidence%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = AppSubsidence%Version%GetVersion()
    
  END FUNCTION AppSubsidence_v40_GetVersion

  
  
  
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
  SUBROUTINE AppSubsidence_v40_ReadRestartData(AppSubs,InFile,iStat)
    CLASS(AppSubsidence_v40_Type) :: AppSubs
    TYPE(GenericFileType)         :: InFile
    INTEGER,INTENT(OUT)           :: iStat
    
    CALL InFile%ReadData(AppSubs%PreCompactHead,iStat)     ;  IF (iStat .EQ. -1) RETURN
    
  END SUBROUTINE AppSubsidence_v40_ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ INITIAL INTERBED THICKNESS AND PRE-COMPACTION HEAD
  ! -------------------------------------------------------------
  SUBROUTINE ReadSubsidenceICData(cICFileName,iGWNodeIDs,InterbedThick,PreCompactHead,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cICFileName
    INTEGER,INTENT(IN)          :: iGWNodeIDs(:)
    REAL(8)                     :: InterbedThick(:,:),PreCompactHead(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadSubsidenceICData'
    TYPE(GenericFileType)        :: ICFile
    INTEGER                      :: ID,indxNode,NLayers,NNodes,iNode
    REAL(8)                      :: rFact,rDummyArray(1+2*SIZE(InterbedThick,DIM=2))
    LOGICAL                      :: lProcessed(SIZE(iGWNodeIDs))
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    
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
        InterbedThick(iNode,:) = rDummyArray(2:1+NLayers) * rFact
        
        !Pre-compaction head
        PreCompactHead(iNode,:) = rDummyArray(NLayers+2:) * rFact
        
    END DO
    
    !Close file
    CALL ICFile%Kill()
    
  END SUBROUTINE ReadSubsidenceICData
  
  
  ! -------------------------------------------------------------
  ! --- READ SUBSIDENCE PARAMETERS
  ! -------------------------------------------------------------
  SUBROUTINE ReadSubsidenceParameters(NLayers,iGWNodeIDs,AppGrid,InFile,ElasticSC,InelasticSC,InterbedThick,InterbedThickMin,PreCompactHead,iStat)
    INTEGER,INTENT(IN)           :: NLayers
    INTEGER,INTENT(IN)           :: iGWNodeIDs(:)
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(GenericFileType)        :: InFile
    REAL(8),INTENT(OUT)          :: ElasticSC(:,:),InelasticSC(:,:),InterbedThick(:,:),InterbedThickMin(:,:),PreCompactHead(:,:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'ReadSubsidenceParameters'
    INTEGER                      :: NGroup,NNodes,indxNode,indxLayer,ID,iNode
    REAL(8)                      :: rFactors(6),rDummyArray(6),rDummy3DArray(AppGrid%NNodes,NLayers,5)
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
                
                ElasticSC(iNode,indxLayer)        = rDummyArray(2) * rFactors(2) * AppGrid%AppNode(iNode)%Area
                InelasticSC(iNode,indxLayer)      = rDummyArray(3) * rFactors(3) * AppGrid%AppNode(iNode)%Area
                InterbedThick(iNode,indxLayer)    = rDummyArray(4) * rFactors(4)
                InterbedThickMin(iNode,indxLayer) = rDummyArray(5) * rFactors(5)
                PreCompactHead(iNode,indxLayer)   = rDummyArray(6) * rFactors(6)
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
                ElasticSC(indxNode,indxLayer)        = rDummy3DArray(indxNode,indxLayer,1) * AppGrid%AppNode(indxNode)%Area
                InelasticSC(indxNode,indxLayer)      = rDummy3DArray(indxNode,indxLayer,2) * AppGrid%AppNode(indxNode)%Area
                InterbedThick(indxNode,indxLayer)    = rDummy3DArray(indxNode,indxLayer,3)
                InterbedThickMin(indxNode,indxLayer) = rDummy3DArray(indxNode,indxLayer,4)
                PreCompactHead(indxNode,indxLayer)   = rDummy3DArray(indxNode,indxLayer,5)
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
  SUBROUTINE AppSubsidence_v40_PrintRestartData(AppSubs,OutFile)
    CLASS(AppSubsidence_v40_Type),INTENT(IN) :: AppSubs
    TYPE(GenericFileType)                    :: OutFile
    
    CALL OutFile%WriteData(AppSubs%PreCompactHead)
    
  END SUBROUTINE AppSubsidence_v40_PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT FINAL SUBSIDENCE PARAMETERS TO STANDARD OUTPUT FILE THAT IS ALREADY DEFINED
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v40_PrintParameters(AppSubs,iGWNodeIDs,NodeAreas)
    CLASS(AppSubsidence_v40_Type),INTENT(IN) :: AppSubs
    INTEGER,INTENT(IN)                       :: iGWNodeIDs(:)
    REAL(8),INTENT(IN)                       :: NodeAreas(:)
    
    !Local variables
    INTEGER :: indxLayer,indxNode,NNodes,NLayers
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
    WRITE (Text,'(A,2X,5(A,2X))')            &
      '   NODE','        SCE             '   &
               ,'        SCI             '   &
               ,'        DC              '   &
               ,'        DCMIN           '   &
               ,'        HC              '
    CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)

    DO indxNode=1,NNodes
      DO indxLayer=1,NLayers                                                                                     
        IF (indxLayer .EQ. 1) THEN                                                                               
          WRITE (Text,'(I7,2X,10(1PG24.15E3,2X))')                                                                                                                   & 
               iGWNodeIDs(indxNode) ,AppSubs%ElasticSC(indxNode,indxLayer) / NodeAreas(indxNode) , AppSubs%InelasticSC(indxNode,indxLayer) / NodeAreas(indxNode) ,   & 
                                     AppSubs%InterbedThick(indxNode,indxLayer)                   , AppSubs%interbedThickMin(indxNode,indxLayer)                  ,   &
                                     AppSubs%PreCompactHead(indxNode,indxLayer)     
        ELSE                                                                                          
          WRITE (Text,'(9X,10(1PG24.15E3,2X))')                                                                                                          &  
                         AppSubs%ElasticSC(indxNode,indxLayer) / NodeAreas(indxNode) , AppSubs%InelasticSC(indxNode,indxLayer) / NodeAreas(indxNode) ,   & 
                         AppSubs%InterbedThick(indxNode,indxLayer)                   , AppSubs%interbedThickMin(indxNode,indxLayer)                  ,   &
                         AppSubs%PreCompactHead(indxNode,indxLayer)     
        END IF                                                                                          
        CALL LogMessage(TRIM(Text),f_iMessage,'',f_iFILE)                                                                       
      END DO                                                                                          
    END DO  
    CALL LogMessage('',f_iMessage,'',f_iFILE)

  END SUBROUTINE AppSubsidence_v40_PrintParameters
  

  ! -------------------------------------------------------------
  ! --- PRINT OUT END-OF-SIMULATION VALUES
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v40_PrintFinalSubs(AppSubs,AppGrid,TimeStep)
    CLASS(AppSubsidence_v40_Type) :: AppSubs
    TYPE(AppGridType),INTENT(IN)  :: AppGrid 
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    
    !Local variables
    INTEGER   :: indxLayer,indxNode,iNLayers,iNNodes
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
    
    !Print final interbed thickness and pre-compaction heads
    DO indxNode=1,iNNodes
        rWorkArray(1:iNLayers)  = AppSubs%InterbedThick(indxNode,:)
        rWorkArray(iNLayers+1:) = AppSubs%PreCompactHead(indxNode,:)
        WRITE (Text,'(I8,100F18.6)') AppGrid%AppNode(indxNode)%ID,rWorkArray
        CALL AppSubs%FinalSubsFile%WriteData(Text)
    END DO
    
  END SUBROUTINE AppSubsidence_v40_PrintFinalSubs    
  
  
  
  
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
  ! --- SIMULATE THE EFFECT OF SUBSIDENCE ON R.H.S. VECTOR AND COEFF MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v40_Simulate(AppSubsidence,Stratigraphy,GWHead,GWHead_P,rStorage,rdStorage,Matrix)
    CLASS(AppSubsidence_v40_Type)     :: AppSubsidence
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:),rStorage(:,:),rdStorage(:,:)
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxNode,iBase,NNodes,NLayers,iGWNode,iNodeIDs(1)
    REAL(8)           :: rGWHead,rGWHead_P,Storativ,rInterbedThick,PreCompactHead,rTopElev,rBottomElev,  &
                         ElasticSC,InelasticSC,rUpdateValues(1),rUpdateRHS(SIZE(GWHead)),rScaleFactor,   &
                         rFlow,rdFlow,rStor,rdStor,rStorPos,rdStorPos,rDiff,rGradient 
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
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
            
            !Current and previous head
            rGWHead   = GWHead(indxNode,indxLayer)
            rGWHead_P = GWHead_P(indxNode,indxLayer)
            
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
            PreCompactHead   = AppSubsidence%PreCompactHead(indxNode,indxLayer)
            ElasticSC        = AppSubsidence%ElasticSC(indxNode,indxLayer)
            InElasticSC      = AppSubsidence%InelasticSC(indxNode,indxLayer)
            IF (rGWHead .GT. PreCompactHead) THEN
                Storativ = ElasticSC * rInterbedThick 
            ELSE
                Storativ = InelasticSC * rInterbedThick 
            END IF
            
            !Limit outflow from aquifer to storage at the aquifer (+ flow is outflow)
            rFlow = Storativ * (rGWHead - PreCompactHead)                    &
                  + ElasticSC * rInterbedThick * (PreCompactHead - rGWHead_P)
            IF (rFlow .GT. 0.0) THEN
                rStor = rStorage(indxNode,indxLayer)
                IF (rFlow .GT. rStor) THEN
                    rdStor    = rdStorage(indxNode,indxLayer)
                    rStorPos  = MAX(rStor , 0.0)
                    rDiff     = rStorPos - rFlow
                    rFlow     = MIN(rStorPos , rFlow)
                    rdStorPos = 0.5d0 * rdStor * (1d0+rStor/SQRT(rStor*rStor+f_rSmoothMaxP))
                    rdFlow    = Storativ
                    rGradient = rdStorPos - 0.5d0 * (1d0 + rDiff/SQRT(rDiff*rDiff+f_rSmoothMaxP)) * (rdStorPos-rdFlow)
                ELSE
                    rGradient = Storativ
                END IF
            ELSE
                rGradient = Storativ
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
  
  END SUBROUTINE AppSubsidence_v40_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- PROCESS SUBSIDENCE PARAMETERS TO BE USED IN SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v40_ProcessSubsidenceParameters(AppSubsidence,GWHead)
    CLASS(AppSubsidence_v40_Type) :: AppSubsidence
    REAL(8),INTENT(IN)            :: GWHead(:,:)
    
    !Local variables
    INTEGER :: indxNode,indxLayer,NNodes,NLayers
    
    !Initialize
    NNodes  = SIZE(AppSubsidence%Subsidence , DIM=1)
    NLayers = SIZE(AppSubsidence%Subsidence , DIM=2)
    
    !Process
    DO indxLayer=1,NLayers
        DO indxNode=1,NNodes
            AppSubsidence%PreCompactHead (indxNode,indxLayer)  = MIN(AppSubsidence%PreCompactHead(indxNode,indxLayer) , GWHead(indxNode,indxLayer))
        END DO
    END DO
    
  END SUBROUTINE AppSubsidence_v40_ProcessSubsidenceParameters
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE SUBSIDENCE RELATED TERMS
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v40_UpdateSubsidence(AppSubsidence,AppGrid,Stratigraphy,GWHead,GWHead_P)
    CLASS(AppSubsidence_v40_Type)     :: AppSubsidence
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:)
    
    !Local variables
    INTEGER         :: indxNode,indxLayer,NNodes
    REAL(8)         :: rGWHead,rGWHead_P,ElasticSC,InelasticSC,rInterbedThick,Area,rTopElev,rBottomElev,rScaleFactor, &
                       PreCompactHead,Subsidence 
    
    !Initialize
    NNodes = SIZE(GWHead , DIM=1)
    
    DO indxLayer=1,Stratigraphy%NLayers
      DO indxNode=1,NNodes
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,indxLayer)) CYCLE
        rInterbedThick    =  AppSubsidence%InterbedThick_P(indxNode,indxLayer)
        !Cycle if the minimum interbed thickness is attained
        IF (rInterbedThick .LE. AppSubsidence%InterbedThickMin(indxNode,indxLayer)) THEN
            AppSubsidence%Subsidence(indxNode,indxLayer) = 0.0
            CYCLE
        END IF
        rGWHead   =  GWHead(indxNode,indxLayer)
        rGWHead_P =  GWHead_P(indxNode,indxLayer)
        
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
        Area             =  AppGrid%AppNode(indxNode)%Area
        ElasticSC        =  AppSubsidence%ElasticSC(indxNode,indxLayer) / Area
        InelasticSC      =  AppSubsidence%InelasticSC(indxNode,indxLayer) / Area
        PreCompactHead   =  AppSubsidence%PreCompactHead(indxNode,indxLayer)
        
        IF (rGWHead .GT. PreCompactHead) THEN
            Subsidence = -ElasticSC * rInterbedThick * (rGWHead-rGWHead_P)
        ELSE            
            Subsidence                                       = -InelasticSC * rInterbedThick * (rGWHead-PreCompactHead)    &
                                                               -ElasticSC * rInterbedThick * (PreCompactHead-rGWHead_P)
            AppSubsidence%PreCompactHead(indxNode,indxLayer) = rGWHead
        END IF
        
        AppSubsidence%Subsidence(indxNode,indxLayer)    = Subsidence
        AppSubsidence%InterbedThick(indxNode,indxLayer) = AppSubsidence%InterbedThick_P(indxNode,indxLayer) - Subsidence
        AppSubsidence%CumSubsidence(indxNode,indxLayer) = AppSubsidence%CumSubsidence_P(indxNode,indxLayer) + Subsidence
      END DO
    END DO
    
    !Update regional volumetric cumulative subsidence
    CALL ComputeRegionalCumSubsidence(AppGrid,AppSubsidence%CumSubsidence,AppSubsidence%RegionalCumSubsidence)
    
  END SUBROUTINE AppSubsidence_v40_UpdateSubsidence
    

  ! -------------------------------------------------------------
  ! --- ADVANCE THE STATE OF THE SUBSIDENCE SYSTEM IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AppSubsidence_v40_AdvanceState(AppSubsidence)
    CLASS(AppSubsidence_v40_Type) :: AppSubsidence
    
    AppSubsidence%InterbedThick_P         = AppSubsidence%InterbedThick
    AppSubsidence%CumSubsidence_P         = AppSubsidence%CumSubsidence
    AppSubsidence%RegionalCumSubsidence_P = AppSubsidence%RegionalCumSubsidence
        
  END SUBROUTINE AppSubsidence_v40_AdvanceState
    
END MODULE