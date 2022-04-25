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
MODULE Class_BaseAppSubsidence
  USE Class_Version           , ONLY: VersionType             
  USE MessageLogger           , ONLY: LogMessage          , &
                                      f_iFILE             , &
                                      f_iMessage            
  USE IOInterface             , ONLY: GenericFileType         
  USE GeneralUtilities        , ONLY: IntToText           
  USE TimeSeriesUtilities     , ONLY: TimeStepType        
  USE Package_Discretization  , ONLY: AppGridType         , &
                                      StratigraphyType     
  USE Package_Misc            , ONLY: TecplotOutputType   , &
                                      HydOutputType       , &
                                      f_iHyd_Subsidence             
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
  PUBLIC :: BaseAppSubsidenceType        , &
            ComputeRegionalCumSubsidence , &
            f_cDescription_SubsHyd

  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  CHARACTER(LEN=21),PARAMETER :: f_cDescription_SubsHyd = 'Subsidence hydrograph'
  
  
  ! -------------------------------------------------------------
  ! --- BASE APPLICATION LAKES DATA TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: BaseAppSubsidenceType
      TYPE(VersionType)                   :: Version                             !Subsidence component version number
      REAL(8)                             :: FactorLen                = 1.0      !Factor for output unit conversion
      CHARACTER(LEN=6)                    :: cUnitLen                 = ''       !Output unit
      REAL(8),ALLOCATABLE                 :: ElasticSC(:,:)                      !Elastic storage coefficent at (node,layer)
      REAL(8),ALLOCATABLE                 :: InelasticSC(:,:)                    !Inelastic storage coefficient at (node,layer) 
      REAL(8),ALLOCATABLE                 :: InterbedThick_P(:,:)                !Interbed thickness at the beginning of timestep at (node,layer)
      REAL(8),ALLOCATABLE                 :: InterbedThick(:,:)                  !Interbed thickness at the end of timestep at (node,layer)
      REAL(8),ALLOCATABLE                 :: InterbedThickMin(:,:)               !Minimum interbed thickness at (node,layer)
      REAL(8),ALLOCATABLE                 :: Subsidence(:,:)                     !Curent subsidence at (node,layer)
      REAL(8),ALLOCATABLE                 :: CumSubsidence_P(:,:)                !Cumulative change in the interbed thickness at the beginning of timestep at (node,layer)
      REAL(8),ALLOCATABLE                 :: CumSubsidence(:,:)                  !Cumulative change in the interbed thickness at the end of time step  at (node,layer) (the InterbedThick above is after applying this change)
      REAL(8),ALLOCATABLE                 :: RegionalCumSubsidence(:)            !Subregional volumetric cumulative subsidence at the current time step
      REAL(8),ALLOCATABLE                 :: RegionalCumSubsidence_P(:)          !Subregional volumetric cumulative subsidence at the previous time step
      TYPE(HydOutputType),ALLOCATABLE     :: SubsHydOutput                       !Subsidence hydrograph output dataset
      TYPE(TecplotOutputType),ALLOCATABLE :: TecplotFile                         !Tecplot output file for subsidence
      TYPE(GenericFileType),ALLOCATABLE   :: FinalSubsFile                       !File that stores the en-of-simulation interbed thicknesses and pre-compaction heads
      LOGICAL                             :: lSubsHydOutput_Defined   = .FALSE.  !Flag to check if this output is defined
      LOGICAL                             :: lTecplotFile_Defined     = .FALSE.  !Flag to check if this output file is defined
      LOGICAL                             :: lFinalSubsFile_Defined   = .FALSE.  !Flag to check if this output is defined
  CONTAINS
      PROCEDURE(Abstract_New),PASS,DEFERRED                         :: New                          
      PROCEDURE,PASS                                                :: Kill                         
      PROCEDURE(Abstract_KillImplementation),PASS,DEFERRED          :: KillImplementation
      PROCEDURE,PASS                                                :: GetInterbedThickAll 
      PROCEDURE,PASS                                                :: GetSubsidence_All
      PROCEDURE,PASS                                                :: GetSubsidenceAtLayer         
      PROCEDURE,PASS                                                :: GetSubregionalCumSubsidence  
      PROCEDURE,PASS                                                :: GetNHydrographs
      PROCEDURE,PASS                                                :: GetHydrographIDs
      PROCEDURE,PASS                                                :: GetHydrographCoordinates
      PROCEDURE,PASS                                                :: GetHydrographNames
      PROCEDURE,PASS                                                :: GetHydOutputFileName
      PROCEDURE(Abstract_GetVersion),PASS,DEFERRED                  :: GetVersion
      PROCEDURE(Abstract_Simulate),PASS,DEFERRED                    :: Simulate                          
      PROCEDURE(Abstract_AdvanceState),PASS,DEFERRED                :: AdvanceState                 
      PROCEDURE,PASS                                                :: PrintResults
      PROCEDURE,PASS                                                :: PrintRestartData
      PROCEDURE(Abstract_PrintRestartData),PASS,DEFERRED            :: PrintRestartData_Implementation              
      PROCEDURE(Abstract_PrintParameters),PASS,DEFERRED             :: PrintParameters              
      PROCEDURE(Abstract_PrintFinalSubs),PASS,DEFERRED              :: PrintFinalSubs              
      PROCEDURE,PASS                                                :: OverwriteParameters          
      PROCEDURE,PASS                                                :: ReadRestartData
      PROCEDURE(Abstract_ReadRestartData),PASS,DEFERRED             :: ReadRestartData_Implementation
      PROCEDURE(Abstract_ProcessSubsidenceParameters),PASS,DEFERRED :: ProcessSubsidenceParameters  
      PROCEDURE(Abstract_UpdateSubsidence),PASS,DEFERRED            :: UpdateSubsidence             
      PROCEDURE,PASS                                                :: TransferOutputToHDF
  END TYPE BaseAppSubsidenceType 
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 25
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_BaseAppSubsidence::'

  
  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE

    SUBROUTINE Abstract_New(AppSubsidence,IsForInquiry,cFileName,cWorkingDirectory,iGWNodeIDs,AppGrid,Stratigraphy,StrmConnectivity,TimeStep,iStat) 
        IMPORT                                   :: BaseAppSubsidenceType,AppGridType,StratigraphyType,TimeStepType
        CLASS(BaseAppSubsidenceType),INTENT(OUT) :: AppSubsidence
        LOGICAL,INTENT(IN)                       :: IsForInquiry
        CHARACTER(LEN=*),INTENT(IN)              :: cFileName,cWorkingDirectory
        INTEGER,INTENT(IN)                       :: iGWNodeIDs(:)
        TYPE(AppGridType),INTENT(IN)             :: AppGrid
        TYPE(StratigraphyType),INTENT(IN)        :: Stratigraphy
        COMPLEX,INTENT(IN)                       :: StrmConnectivity(:)
        TYPE(TimeStepType),INTENT(IN)            :: TimeStep
        INTEGER,INTENT(OUT)                      :: iStat
    END SUBROUTINE Abstract_New
    
    
    SUBROUTINE Abstract_KillImplementation(AppSubsidence)
        IMPORT                       :: BaseAppSubsidenceType
        CLASS(BaseAppSubsidenceType) :: AppSubsidence
    END SUBROUTINE Abstract_KillImplementation
    
    
    FUNCTION Abstract_GetVersion(AppSubsidence) RESULT(cVrs)
       IMPORT                       :: BaseAppSubsidenceType
       CLASS(BaseAppSubsidenceType) :: AppSubsidence
       CHARACTER(:),ALLOCATABLE     :: cVrs
    END FUNCTION Abstract_GetVersion
     
     
    SUBROUTINE Abstract_PrintParameters(AppSubs,iGWNodeIDs,NodeAreas)
        IMPORT                                  :: BaseAppSubsidenceType
        CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubs
        INTEGER,INTENT(IN)                      :: iGWNodeIDs(:)
        REAL(8),INTENT(IN)                      :: NodeAreas(:)
    END SUBROUTINE Abstract_PrintParameters

     
    SUBROUTINE Abstract_PrintFinalSubs(AppSubs,AppGrid,TimeStep)
      IMPORT                        :: BaseAppSubsidenceType,AppGridType,TimeStepType
      CLASS(BaseAppSubsidenceType)  :: AppSubs
      TYPE(AppGridType),INTENT(IN)  :: AppGrid 
      TYPE(TimeStepType),INTENT(IN) :: TimeStep
    END SUBROUTINE Abstract_PrintFinalSubs
      
    
    SUBROUTINE Abstract_PrintRestartData(AppSubs,OutFile)
      IMPORT                                  :: BaseAppSubsidenceType,GenericFileType
      CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubs
      TYPE(GenericFileType)                   :: OutFile 
    END SUBROUTINE Abstract_PrintRestartData
      
    
    SUBROUTINE Abstract_ReadRestartData(AppSubs,InFile,iStat)
      IMPORT                       :: BaseAppSubsidenceType,GenericFileType
      CLASS(BaseAppSubsidenceType) :: AppSubs
      TYPE(GenericFileType)        :: InFile 
      INTEGER,INTENT(OUT)          :: iStat 
    END SUBROUTINE Abstract_ReadRestartData
      
    
    SUBROUTINE Abstract_Simulate(AppSubsidence,Stratigraphy,GWHead,GWHead_P,rStorage,rdStorage,Matrix)
        IMPORT                            :: BaseAppSubsidenceType,StratigraphyType,MatrixType
        CLASS(BaseAppSubsidenceType)      :: AppSubsidence
        TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
        REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:),rStorage(:,:),rdStorage(:,:)
        TYPE(MatrixType)                  :: Matrix
    END SUBROUTINE Abstract_Simulate
    
    
    SUBROUTINE Abstract_ProcessSubsidenceParameters(AppSubsidence,GWHead)
        IMPORT                       :: BaseAppSubsidenceType
        CLASS(BaseAppSubsidenceType) :: AppSubsidence
        REAL(8),INTENT(IN)           :: GWHead(:,:)
    END SUBROUTINE Abstract_ProcessSubsidenceParameters
    
    
    SUBROUTINE Abstract_UpdateSubsidence(AppSubsidence,AppGrid,Stratigraphy,GWHead,GWHead_P)
        IMPORT                            :: BaseAppSubsidenceType,AppGridType,StratigraphyType
        CLASS(BaseAppSubsidenceType)      :: AppSubsidence
        TYPE(AppGridType),INTENT(IN)      :: AppGrid
        TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
        REAL(8),INTENT(IN)                :: GWHead(:,:),GWHead_P(:,:)
    END SUBROUTINE Abstract_UpdateSubsidence
    
    
    SUBROUTINE Abstract_AdvanceState(AppSubsidence)
        IMPORT                       :: BaseAppSubsidenceType
        CLASS(BaseAppSubsidenceType) :: AppSubsidence
    END SUBROUTINE Abstract_AdvanceState

    
  END INTERFACE   
  
  
  
  
CONTAINS
    
    
    
    
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
    CLASS(BaseAppSubsidenceType) :: AppSubsidence
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (AppSubsidence%ElasticSC               ,  &
                AppSubsidence%InelasticSC             ,  &
                AppSubsidence%InterbedThick_P         ,  &
                AppSubsidence%InterbedThick           ,  &
                AppSubsidence%InterbedThickMin        ,  &
                AppSubsidence%Subsidence              ,  &
                AppSubsidence%CumSubsidence_P         ,  &
                AppSubsidence%CumSubsidence           ,  &
                AppSubsidence%RegionalCumSubsidence   , &
                AppSubsidence%RegionalCumSubsidence_P , &
                STAT = ErrorCode                      )
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        CALL AppSubsidence%SubsHydOutput%Kill()
        DEALLOCATE (AppSubsidence%SubsHydOutput , STAT=ErrorCode)
    END IF
    
    IF (AppSubsidence%lTecplotFile_Defined) THEN
        CALL AppSubsidence%TecplotFile%Kill()
        DEALLOCATE (AppSubsidence%TecplotFile , STAT=ErrorCode)
    END IF
        
    IF (AppSubsidence%lFinalSubsFile_Defined) CALL AppSubsidence%FinalSubsFile%Kill()
    
    !Default values for arguments
    AppSubsidence%FactorLen              = 1.0
    AppSubsidence%cUnitLen               = ''
    AppSubsidence%lSubsHydOutput_Defined = .FALSE.  
    AppSubsidence%lTecplotFile_Defined   = .FALSE.  
    AppSubsidence%lFinalSubsFile_Defined = .FALSE.
      

    !Clean memory from any other implementation-specific variables
    CALL AppSubsidence%KillImplementation()
            
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
  ! --- GET HYDROGRAPH OUTPUT FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetHydOutputFileName(AppSubsidence,cFileName)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    CHARACTER(:),ALLOCATABLE,INTENT(OUT)    :: cFileName
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (cFileName , STAT=ErrorCode)
    CALL AppSubsidence%SubsHydOutput%GetFileName(cFileName)
    
  END SUBROUTINE GetHydOutputFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(AppSubsidence) RESULT(NHydrographs)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER                                 :: NHydrographs
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        NHydrographs = AppSubsidence%SubsHydOutput%GetNHydrographs()
    ELSE
        NHydrographs = 0
    END IF
    
  END FUNCTION GetNHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(AppSubsidence,IDs)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(OUT)                     :: IDs(:)
    
    IF (AppSubsidence%lSubsHydOutput_Defined) CALL AppSubsidence%SubsHydOutput%GetHydrographIDs(IDs)
    
  END SUBROUTINE GetHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographCoordinates(AppSubsidence,GridX,GridY,XHyd,YHyd)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    REAL(8),INTENT(IN)                      :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)                     :: XHyd(:),YHyd(:)
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        CALL AppSubsidence%SubsHydOutput%GetHydrographCoordinates(GridX,GridY,XHyd,YHyd)
    END IF
    
 END SUBROUTINE GetHydrographCoordinates
  
  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetHydrographNames(AppSubsidence,cNamesList)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    CHARACTER(LEN=*),INTENT(OUT)            :: cNamesList(:)  !Assumes array is previously dimensioned based on the number of hydrographs
    
    IF (AppSubsidence%lSubsHydOutput_Defined) THEN
        CALL AppSubsidence%SubsHydOutput%GetHydrographNames(cNamesList)
    END IF
    
 END SUBROUTINE GetHydrographNames
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL SUBSIDENCE AT (node,layer) COMBINATION
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidence_All(AppSubsidence,Subs)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    REAL(8),INTENT(OUT)                     :: Subs(:,:)
    
    Subs = AppSubsidence%Subsidence
    
  END SUBROUTINE GetSubsidence_All
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBSIDENCE AT ALL NODES OF A LAYER
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSubsidenceAtLayer(AppSubsidence,iLayer,Subs)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(IN)                      :: iLayer
    REAL(8),INTENT(OUT)                     :: Subs(:)
    
    Subs = AppSubsidence%Subsidence(:,iLayer)
    
  END SUBROUTINE GetSubsidenceAtLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET INTERBED THICKNESS AT ALL NODES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInterbedThickAll(AppSubsidence,InterbedThick)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    REAL(8),INTENT(OUT)                     :: InterbedThick(:,:)
    
    InterbedThick = AppSubsidence%InterbedThick
    
  END SUBROUTINE GetInterbedThickAll
  

  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL CUMULATIVE SUBSIDENCE
  ! -------------------------------------------------------------
  PURE FUNCTION GetSubregionalCumSubsidence(AppSubsidence,NRegions,lPreviousTS) RESULT(Subs)
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    INTEGER,INTENT(IN)                      :: NRegions
    LOGICAL,INTENT(IN)                      :: lPreviousTS
    REAL(8)                                 :: Subs(NRegions)
    
    IF (lPreviousTS) THEN
        Subs = AppSubsidence%RegionalCumSubsidence_P
    ELSE
        Subs = AppSubsidence%RegionalCumSubsidence
    END IF
    
  END FUNCTION GetSubregionalCumSubsidence  
  
  
  

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
    CLASS(BaseAppSubsidenceType) :: AppSubsidence
    TYPE(GenericFileType)        :: InFile
    INTEGER,INTENT(OUT)          :: iStat
    
    CALL InFile%ReadData(AppSubsidence%InterbedThick,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%InterbedThick_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%CumSubsidence,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%CumSubsidence_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%RegionalCumSubsidence,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppSubsidence%RegionalCumSubsidence_P,iStat)  ;  IF (iStat .EQ. -1) RETURN

    CALL AppSubsidence%ReadRestartData_Implementation(InFile,iStat)
    
  END SUBROUTINE ReadRestartData

  
  
  
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
    CLASS(BaseAppSubsidenceType),INTENT(IN) :: AppSubsidence
    TYPE(GenericFileType)                   :: OutFile
    
    CALL OutFile%WriteData(AppSubsidence%InterbedThick)
    CALL OutFile%WriteData(AppSubsidence%InterbedThick_P)
    CALL OutFile%WriteData(AppSubsidence%CumSubsidence)
    CALL OutFile%WriteData(AppSubsidence%CumSubsidence_P)
    CALL OutFile%WriteData(AppSubsidence%RegionalCumSubsidence)
    CALL OutFile%WriteData(AppSubsidence%RegionalCumSubsidence_P)
    
    CALL AppSubsidence%PrintRestartData_Implementation(OutFile)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE FOR SUBSIDENCE-RELATED PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppSubsidence,AppGrid,Stratigraphy,TimeStep,lEndOfSimulation)
    CLASS(BaseAppSubsidenceType)      :: AppSubsidence
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TImeStepType),INTENT(IN)     :: TimeStep
    LOGICAL,INTENT(IN)                :: lEndOfSimulation
    
    !Subsidence hydrographs
    IF (AppSubsidence%lSubsHydOutput_Defined)   &
        CALL PrintSubsidenceHydrographs(Stratigraphy,AppSubsidence%CumSubsidence,AppSubsidence%FactorLen,TimeStep,lEndOfSimulation,AppSubsidence%SubsHydOutput)

    !Tecplot print-out
    IF (AppSubsidence%lTecplotFile_Defined)  &
        CALL AppSubsidence%TecplotFile%PrintResults(AppSubsidence%CumSubsidence,AppSubsidence%FactorLen,TimeStep)
    
    !Final results
    IF (lEndOfSimulation) THEN
        IF (AppSubsidence%lFinalSubsFile_Defined) CALL AppSubsidence%PrintFinalSubs(AppGrid,TimeStep)
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
  
    CALL SubsHydOutput%PrintResults(Stratigraphy,f_iHyd_Subsidence,CumSubsidence,rFactor,TimeStep,lEndOfSimulation) 
    
  END SUBROUTINE PrintSubsidenceHydrographs
  
  
  
  
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
    CLASS(BaseAppSubsidenceType)  :: AppSubs
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
  ! --- OVERWRITE ELASTIC AND INELASTIC STORAGE COEFFICIENTS
  ! -------------------------------------------------------------
  SUBROUTINE OverwriteParameters(AppSubsidence,ElasticSC,InelasticSC)
    CLASS(BaseAppSubsidenceType) :: AppSubsidence
    REAL(8),INTENT(IN)           :: ElasticSC(:,:),InelasticSC(:,:)
    
    !Local variables
    INTEGER :: NNodes,NLayers,indxNode,indxLayer
    
    !Initialize
    NNodes  = SIZE(ElasticSC , DIM=1)
    NLayers = SIZE(ElasticSC , DIM=2)
    
    !Overwrite parameters
    DO indxLayer=1,NLayers
        DO indxNode=1,NNodes
            IF (ElasticSC(indxNode,indxLayer)   .GE. 0.0) AppSubsidence%ElasticSC(indxNode,indxLayer)   = ElasticSC(indxNode,indxLayer)
            IF (InelasticSC(indxNode,indxLayer) .GE. 0.0) AppSubsidence%InelasticSC(indxNode,indxLayer) = InelasticSC(indxNode,indxLayer)
        END DO
    END DO
    
  END SUBROUTINE OverwriteParameters
  
  
END MODULE
