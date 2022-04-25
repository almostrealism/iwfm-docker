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
MODULE Package_AppLake
  USE Class_Version               , ONLY: VersionType             , &
                                          ReadVersion             
  USE MessageLogger               , ONLY: SetLastMessage          , &
                                          EchoProgress            , &
                                          MessageArray            , &
                                          f_iFatal                  
  USE IOInterface                 , ONLY: GenericFileType         , &
                                          f_iUNKNOWN
  USE GeneralUtilities            , ONLY: IntToText               , &
                                          FindSubstringInString
  USE TimeSeriesUtilities         , ONLY: TimeStepType
  USE Package_Discretization      , ONLY: AppGridType             , &
                                          StratigraphyType
  USE Package_ComponentConnectors , ONLY: StrmLakeConnectorType   , &
                                          LakeGWConnectorType
  USE Package_Matrix              , ONLY: MatrixType
  USE Package_PrecipitationET     , ONLY: ETType                  , &
                                          PrecipitationType
  USE Class_BaseAppLake           , ONLY: BaseAppLakeType         , &
                                          f_iBudgetType_Lake
  USE Class_AppLake_v40           , ONLY: AppLake_v40_Type
  USE Class_AppLake_v50           , ONLY: AppLake_v50_Type
  USE Package_Budget              , ONLY: BudgetType
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
  PUBLIC :: AppLakeType        , &
            f_iBudgetType_Lake
  
  
  ! -------------------------------------------------------------
  ! --- APPLICATION LAKES DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppLakeType
      PRIVATE
      INTEGER                            :: iVersion = 0
      LOGICAL                            :: lDefined = .FALSE.
      CLASS(BaseAppLakeType),ALLOCATABLE :: Me
  CONTAINS
      PROCEDURE,PASS   :: SetStaticComponent
      PROCEDURE,PASS   :: SetStaticComponentFromBinFile
      PROCEDURE,PASS   :: SetDynamicComponent
      PROCEDURE,PASS   :: SetAllComponents
      PROCEDURE,PASS   :: SetAllComponentsWithoutBinFile
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetBudget_List
      PROCEDURE,PASS   :: GetBudget_NColumns
      PROCEDURE,PASS   :: GetBudget_ColumnTitles
      PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenAppLake
      PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetBudget_TSData
      PROCEDURE,PASS   :: GetNLakes
      PROCEDURE,PASS   :: GetNames
      PROCEDURE,PASS   :: GetLakeIDs
      PROCEDURE,PASS   :: GetLakeID
      PROCEDURE,PASS   :: GetLakeIndex
      PROCEDURE,PASS   :: GetMaxElevs
      PROCEDURE,PASS   :: GetNTotalLakeNodes
      PROCEDURE,PASS   :: GetNElementsInLake
      PROCEDURE,PASS   :: GetNodes
      PROCEDURE,PASS   :: GetLakeElements
      PROCEDURE,PASS   :: GetAllLakeElements
      PROCEDURE,NOPASS :: GetAllLakeElements_FromFile
      PROCEDURE,PASS   :: GetElevs
      PROCEDURE,NOPASS :: GetVersion
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: ReadTSData
      PROCEDURE,PASS   :: WritePreprocessedData
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: IsDefined
      PROCEDURE,PASS   :: RegisterWithMatrix
      PROCEDURE,PASS   :: Simulate
      PROCEDURE,PASS   :: ConvertTimeUnit
      PROCEDURE,PASS   :: ResetElevations
      PROCEDURE,PASS   :: CheckExternalTSDataPointers
      PROCEDURE,PASS   :: AdvanceState
      PROCEDURE,PASS   :: UpdateHeads
      PROCEDURE,PASS   :: DestinationIDs_To_Indices
      GENERIC          :: New                            => SetStaticComponent                 , &
                                                            SetStaticComponentFromBinFile      , &
                                                            SetDynamicComponent                , &
                                                            SetAllComponents                   , &
                                                            SetAllComponentsWithoutBinFile
      GENERIC          :: GetBudget_MonthlyFlows         => GetBudget_MonthlyFlows_GivenFile   , &
                                                            GetBudget_MonthlyFlows_GivenAppLake
  END TYPE AppLakeType
  
  
  ! -------------------------------------------------------------
  ! --- LAKE PACKAGE FACADE VERSION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    ='4.0.0000'
  INCLUDE 'Package_AppLake_Revision.fi'
 
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 17
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_AppLake::'
  
  
  
  
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
  ! --- NEW APPLICATION LAKES FROM RAW DATA (GENERALLY CALLED IN PRE-PROCESSOR)
  ! -------------------------------------------------------------
  SUBROUTINE SetStaticComponent(AppLake,cFileName,Stratigraphy,AppGrid,StrmLakeConnector,LakeGWConnector,iStat)
    CLASS(AppLakeType),INTENT(OUT)        :: AppLake
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(LakeGWConnectorType),INTENT(OUT) :: LakeGWConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'SetStaticComponent'
    INTEGER                      :: ErrorCode
    TYPE(GenericFileType)        :: AppLakeMainFile
    CHARACTER(:),ALLOCATABLE     :: cVersionLocal
    
    !Initialize
    iStat = 0

    !Return if no filename is defined
    IF (cFileName .EQ. '') RETURN

    !Open main control file and retrieve version number
    CALL AppLakeMainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(AppLakeMainFile,'LAKE',cVersionLocal,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file to reset it
    CALL AppLakeMainFile%Kill()
    
    !Instantiate lake component based on version
    SELECT CASE (TRIM(cVersionLocal))
        CASE ('4.0')
            ALLOCATE(AppLake_v40_Type :: AppLake%Me)
            CALL AppLake%Me%New(cFileName,Stratigraphy,AppGrid,StrmLakeConnector,LakeGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 40
            AppLake%lDefined = .TRUE.
            
        CASE ('5.0')
            ALLOCATE(AppLake_v50_Type :: AppLake%Me)
            CALL AppLake%Me%New(cFileName,Stratigraphy,AppGrid,StrmLakeConnector,LakeGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 50
            AppLake%lDefined = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Lake Component version number is not recognized ('//TRIM(cVersionLocal)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END SELECT

  END SUBROUTINE SetStaticComponent
  
  
  ! -------------------------------------------------------------
  ! --- NEW STATIC LAKE DATA FROM PRE-PROCESSOR BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetStaticComponentFromBinFile(AppLake,BinFile,iStat)
    CLASS(AppLakeType),INTENT(OUT) :: AppLake
    TYPE(GenericFileType)          :: BinFile
    INTEGER,INTENT(OUT)            :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+29) :: ThisProcedure = ModName // 'SetStaticComponentFromBinFile'
    INTEGER                      :: iVersion,ErrorCode
    
    !Initialize
    iStat = 0
    
    !Read version number from binary file
    CALL BinFile%ReadData(iVersion,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Return if version number is zero (streams are not simulated)
    IF (iVersion .EQ. 0) RETURN
  
    !Instantiate stream component based on version
    SELECT CASE (iVersion)
        CASE (40)
            ALLOCATE(AppLake_v40_Type :: AppLake%Me)
            CALL AppLake%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 40
            AppLake%lDefined = .TRUE.
            
        CASE (50)
            ALLOCATE(AppLake_v50_Type :: AppLake%Me)
            CALL AppLake%Me%New(BinFile,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 50
            AppLake%lDefined = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Lake Component version number is not recognized ('//TRIM(IntToText(iVersion))//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
  END SUBROUTINE SetStaticComponentFromBinFile
  

  ! -------------------------------------------------------------
  ! --- INSTANTIATE DYNAMIC COMPONENT LAKE DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE SetDynamicComponent(AppLake,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,LakeGWConnector,iStat)
    CLASS(AppLakeType)            :: AppLake
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(LakeGWConnectorType)     :: LakeGWConnector
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'SetDynamicComponent'
    TYPE(GenericFileType)        :: AppLakeMainFile
    INTEGER                      :: ErrorCode
    REAL(8)                      :: rVersionPre
    CHARACTER(:),ALLOCATABLE     :: cVersionSim
    
    !Initialize
    iStat = 0
    
    !Return if no filename is defined
    IF (cFileName .EQ. '') THEN
        !If static component of lakes are defined, dynamic component must be defined as well
        IF (AppLake%iVersion .GT. 0) THEN
            MessageArray(1) = 'For proper simulation of lakes, relevant lake data files must'
            MessageArray(2) = 'be specified when lakes are defined in Pre-Processor.'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        ELSE
            RETURN
        END IF
    END IF

    !Open main control file and retrieve version number
    CALL AppLakeMainFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(AppLakeMainFile,'LAKE',cVersionSim,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file to reset it
    CALL AppLakeMainFile%Kill()
    
    !Make sure versions from static and dynamic components are the same
    ErrorCode   = 0
    rVersionPre = REAL(AppLake%iVersion)/10.0
    SELECT CASE (TRIM(cVersionSim))
        CASE ('4.0')
            IF (AppLake%iVersion .NE. 40) ErrorCode = 1
            
        CASE ('5.0')
            IF (AppLake%iVersion .NE. 50) ErrorCode = 1
            
        CASE DEFAULT
            CALL SetLastMessage('Lake Component version number is not recognized ('//TRIM(cVersionSim)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    IF (ErrorCode .EQ. 1) THEN
        MessageArray(1) = 'Lake Component versions used in Pre-Processor and Simulation must match!'
        WRITE(MessageArray(2),'(A,F3.1)') 'Version number in Pre-Processor = ',rVersionPre
        MessageArray(3) = 'Version number in Simulation    = ' // TRIM(cVersionSim)
        CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Instantiate the dynamic component
    CALL AppLake%Me%New(IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,LakeGWConnector,iStat)
        
  END SUBROUTINE SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE LAKE DATA
  ! -------------------------------------------------------------
  SUBROUTINE SetAllComponents(AppLake,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,BinFile,LakeGWConnector,iStat)
    CLASS(AppLakeType),INTENT(OUT) :: AppLake
    LOGICAL,INTENT(IN)             :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)    :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    INTEGER,INTENT(IN)             :: NTIME
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    TYPE(GenericFileType)          :: BinFile
    TYPE(LakeGWConnectorType)      :: LakeGWConnector
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'SetAllComponents'
    INTEGER                      :: iVersion,ErrorCode
    
    !If a binary file is supplied, read the flag to see if lake are simulated 
    IF (BinFile%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL BinFile%ReadData(iVersion,iStat)  
        IF (iStat .EQ. -1) RETURN
        IF (iVersion .EQ. 0) RETURN
    END IF

    !Return if a Simulation filename is not specified
    IF (cFileName .EQ. ''  .OR.  BinFile%iGetFileType() .EQ. f_iUNKNOWN) RETURN
    
    !Instantiate lake component based on version
    SELECT CASE (iVersion)
        CASE (40)
            ALLOCATE(AppLake_v40_Type :: AppLake%Me)
            CALL AppLake%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,BinFile,LakeGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 40
            AppLake%lDefined = .TRUE.
            
        CASE (50)
            ALLOCATE(AppLake_v50_Type :: AppLake%Me)
            CALL AppLake%Me%New(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,BinFile,LakeGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 50
            AppLake%lDefined = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Lake Component version number is not recognized ('//TRIM(IntToText(iVersion))//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE LAKE DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE SetAllComponentsWithoutBinFile(AppLake,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,LakeGWConnector,iStat)
    CLASS(AppLakeType),INTENT(OUT)        :: AppLake
    LOGICAL,INTENT(IN)                    :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSimFileName,cSimWorkingDirectory
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(LakeGWConnectorType),INTENT(OUT) :: LakeGWConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'SetAllComponentsWithoutBinFile'
    INTEGER                      :: ErrorCode
    TYPE(GenericFileType)        :: MainFile
    CHARACTER(:),ALLOCATABLE     :: cVersionPre
    
    !Initialzie
    iStat = 0
    
    !Return if a Simulation filename is not specified
    IF (cSimFileName .EQ. ''  .OR.  cPPFileName .EQ. '') RETURN
    
    !Open file and read the version number line to decide which component to instantiate
    CALL MainFile%New(FileName=cPPFileName,InputFile=.TRUE.,Descriptor='main stream data file',iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(MainFile,'LAKE',cVersionPre,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close main input file to reset it
    CALL MainFile%Kill()
    
    !Instantiate lake component based on version
    SELECT CASE (TRIM(cVersionPre))
        CASE ('4.0')
            ALLOCATE(AppLake_v40_Type :: AppLake%Me)
            CALL AppLake%Me%New(IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,LakeGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 40
            AppLake%lDefined = .TRUE.
            
        CASE ('5.0')
            ALLOCATE(AppLake_v50_Type :: AppLake%Me)
            CALL AppLake%Me%New(IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,StrmLakeConnector,LakeGWConnector,iStat)
            IF (iStat .EQ. -1) RETURN
            IF (AppLake%Me%NLakes .EQ. 0) THEN
                CALL AppLake%Me%Kill()
                DEALLOCATE (AppLake%Me ,STAT=ErrorCode)
                RETURN
            END IF
            AppLake%iVersion = 50
            AppLake%lDefined = .TRUE.
            
        CASE DEFAULT
            CALL SetLastMessage('Lake Component version number is not recognized ('//TRIM(cVersionPre)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE SetAllComponentsWithoutBinFile
  
  
  
  
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
  ! --- KILL APPLICATION LAKE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppLake)
    CLASS(AppLakeType) :: AppLake
    
    !Local variables
    INTEGER :: ErrorCode
    
    IF (AppLake%iVersion .GT. 0) THEN
        CALL AppLake%Me%Kill()
        DEALLOCATE (AppLake%Me , STAT=ErrorCode)
        AppLake%iVersion = 0
        AppLake%lDefined = .FALSE.
    END IF
    
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
  SUBROUTINE GetBudget_List(AppLake,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(AppLakeType),INTENT(IN)            :: AppLake
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%GetBudget_List(iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    ELSE
        ALLOCATE (iBudgetTypeList(0) , iBudgetLocationTypeList(0) , cBudgetDescriptions(0) , cBudgetFiles(0))
    END IF
  
  END SUBROUTINE GetBudget_List
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BUDGET COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(AppLake,iLocationIndex,iNCols,iStat)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)            :: iLocationIndex          
    INTEGER,INTENT(OUT)           :: iNCols,iStat          
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%GetBudget_NColumns(iLocationIndex,iNCols,iStat)
    ELSE
        iStat  = 0
        iNCols = 0
    END IF
     
  END SUBROUTINE GetBudget_NColumns


  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN TITLES (EXCLUDING Time COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(AppLake,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(AppLakeType),INTENT(IN)            :: AppLake
    INTEGER,INTENT(IN)                       :: iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat         
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%GetBudget_ColumnTitles(iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    END IF
     
  END SUBROUTINE GetBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM AppLake OBJECT FOR A SPECIFED LAKE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenAppLake(AppLake,iLakeIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(AppLakeType),INTENT(IN)            :: AppLake
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iLakeIndex
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%GetBudget_MonthlyFlows_GivenAppLake(iLakeIndex,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)  
    ELSE
        iStat = 0
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
    END IF
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenAppLake


  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE FOR A SPECIFED LAKE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iLakeID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iLakeID
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'GetBudget_MonthlyFlows_GivenFile'
    INTEGER                      :: iErrorCode  
    CHARACTER(:),ALLOCATABLE     :: cVersion
    TYPE(AppLakeType)            :: AppLake
    
    !Get version number
    CALL GetPackageVersion(Budget,cVersion)
    
    !Based on component version, allocate base stream type
    SELECT CASE (cVersion)
        CASE ('4.0')
            ALLOCATE(AppLake_v40_Type :: AppLake%Me)
        CASE ('5.0')
            ALLOCATE(AppLake_v50_Type :: AppLake%Me)
        CASE DEFAULT
            CALL SetLastMessage('Lake Component version number is not recognized ('//TRIM(cVersion)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Get monthly data    
    CALL AppLake%Me%GetBudget_MonthlyFlows_GivenFile(Budget,iLakeID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    
    !Clear memory
    DEALLOCATE (AppLake%Me , cVersion , STAT=iErrorCode)

  CONTAINS
   
    !#######################################################
    !### FIND THE VERSION NUMBER OF THE LAKE PACKAGE USED
    !#######################################################
    SUBROUTINE GetPackageVersion(Budget,cVersion)
      TYPE(BudgetType),INTENT(IN) :: Budget
      CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cVersion
      
      !Local variables
      INTEGER                      :: iNTitles,iLenTitles,indx,iLoc
      CHARACTER(LEN=:),ALLOCATABLE :: cTitles(:)
      CHARACTER(:),ALLOCATABLE     :: cTitlesConc
     
      !Get the ASCII titles from the Budget file; one of these titles include component version number
      iNTitles   = Budget%GetNPersistentTitles()
      iLenTitles = Budget%GetTitleLen()
      ALLOCATE (CHARACTER(iLenTitles) :: cTitles(iNTitles))
      cTitles = Budget%GetPersistentTitles(iNTitles)
      
      !Concotonate titles
      ALLOCATE (CHARACTER(LEN=iNTitles*iLenTitles) :: cTitlesConc)
      cTitlesConc = ''
      DO indx=1,iNTitles
          cTitlesConc = TRIM(cTitlesConc) // TRIM(ADJUSTL(cTitles(indx)))
      END DO  
      
      !Check for version 4.0
      CALL FindSubStringInString('v4.0.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '4.0'
          RETURN
      END IF 
      
      !Check for version 5.0
      CALL FindSubStringInString('v5.0.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '5.0'
          RETURN
      END IF 
      
      !If made to this point, something is wrong
      ALLOCATE (CHARACTER(3) :: cVersion)
      cVersion = '0.0'

    END SUBROUTINE GetPackageVersion
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile


  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AppLake,iLakeIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)            :: iLakeIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)   :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)            :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)           :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)           :: iDataTypes(:),inActualOutput,iStat
    
    IF (AppLake%lDefined) THEN    
        CALL AppLake%Me%GetBudget_TSData(iLakeIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    ELSE
        iStat          = 0
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
    END IF
           
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetLakeIDs(AppLake,iLakeIDs)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(OUT)           :: iLakeIDs(:)
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%GetLakeIDs(iLakeIDs)
    END IF
    
  END SUBROUTINE GetLakeIDs
  
    
  ! -------------------------------------------------------------
  ! --- GET LAKE INDEX GIVEN ID
  ! -------------------------------------------------------------
  PURE FUNCTION GetLakeIndex(AppLake,iLakeID) RESULT(iIndex)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)            :: iLakeID
    INTEGER                       :: iIndex
    
    IF (AppLake%lDefined) THEN
        iIndex = AppLake%Me%GetLakeIndex(iLakeID)
    ELSE
        iIndex = 0
    END IF
    
  END FUNCTION GetLakeIndex
  
      
  ! -------------------------------------------------------------
  ! --- GET LAKE ID GIVEN INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION GetLakeID(AppLake,indx) RESULT(ID)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)            :: indx
    INTEGER                       :: ID
    
    IF (AppLake%lDefined) THEN
        ID = AppLake%Me%GetLakeID(indx)
    ELSE
        ID = 0
    END IF
    
  END FUNCTION GetLakeID
  
      
  ! -------------------------------------------------------------
  ! --- GET LAKE ELEVS
  ! -------------------------------------------------------------
  FUNCTION GetElevs(AppLake,NLakes) RESULT(Elevs)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)            :: NLakes
    REAL(8)                       :: Elevs(NLakes)
    
    IF (AppLake%lDefined) CALL AppLake%Me%GetElevs(Elevs)
    
  END FUNCTION GetElevs
  
    
  ! -------------------------------------------------------------
  ! --- GET ALL LAKE ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE GetAllLakeElements(AppLake,iLakeElems)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,ALLOCATABLE           :: iLakeElems(:)
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%GetAllLakeElements(iLakeElems)
    ELSE
        ALLOCATE (iLakeElems(0))
    END IF
    
  END SUBROUTINE GetAllLakeElements
  
    
  ! -------------------------------------------------------------
  ! --- GET ALL LAKE ELEMENTS FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetAllLakeElements_FromFile(cFileName,iLakeElems,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    INTEGER,ALLOCATABLE         :: iLakeElems(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    TYPE(AppLakeType) :: AppLake
    
    !Initialize
    iStat = 0
    
    !Allocate base lake type as v40 type just to allocate
    ALLOCATE(AppLake_v40_Type :: AppLake%Me)
    
    !Get the lake elements
    CALL AppLake%Me%GetLakeElements_FromFile(cFileName,iLakeElems,iStat)
    
    !Kill AppLake
    CALL AppLake%Kill()
    
  END SUBROUTINE GetAllLakeElements_FromFile
  
    
  ! -------------------------------------------------------------
  ! --- GET ELEMENTS FOR A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetLakeElements(AppLake,iLake,Elements)
    CLASS(AppLakeType),INTENT(IN)   :: AppLake
    INTEGER,INTENT(IN)              :: iLake
    INTEGER,ALLOCATABLE,INTENT(OUT) :: Elements(:)
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%GetLakeElements(iLake,Elements)
    ELSE
        ALLOCATE (Elements(0))
    END IF
    
  END SUBROUTINE GetLakeElements
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NODES OF A LAKE
  ! -------------------------------------------------------------
  SUBROUTINE GetNodes(AppLake,iLakeNo,Nodes)
    CLASS(AppLakeType),INTENT(IN)   :: AppLake
    INTEGER,INTENT(IN)              :: iLakeNo                      
    INTEGER,ALLOCATABLE,INTENT(OUT) :: Nodes(:)
    
    IF (AppLake%lDefined) CALL AppLake%Me%GetNodes(iLakeNo,Nodes)
    
  END SUBROUTINE GetNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENTS IN A LAKE
  ! -------------------------------------------------------------
  PURE FUNCTION GetNElementsInLake(AppLake,iLake) RESULT(NElems)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)            :: iLake
    INTEGER                       :: NElems
    
    IF (AppLake%lDefined) THEN
        NElems = AppLake%Me%GetNElementsInLake(iLake)
    ELSE
        NElems = 0
    END IF
    
  END FUNCTION GetNElementsInLake

  
  ! -------------------------------------------------------------
  ! --- GET THE TOTAL NUMBER OF UNIQUE LAKE NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNTotalLakeNodes(AppLake) RESULT(NNodes)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER                       :: NNodes
    
    IF (AppLake%lDefined) THEN
        NNodes = AppLake%Me%GetNTotalLakeNodes()
    ELSE
        NNodes = 0
    END IF
    
  END FUNCTION GetNTotalLakeNodes


  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF LAKES
  ! -------------------------------------------------------------
  PURE FUNCTION GetNLakes(AppLake) RESULT(NLakes)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER                       :: NLakes
    
    IF (AppLake%lDefined) THEN
        NLakes = AppLake%Me%NLakes
    ELSE
        NLakes = 0
    END IF
    
  END FUNCTION GetNLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET LAKE NAMES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNames(AppLake,cNamesList)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    CHARACTER(LEN=*),INTENT(OUT)  :: cNamesList(:)  !Assumes array is previously dimensioned with respect to number of lakes
    
    IF (AppLake%lDefined) CALL AppLake%Me%GetNames(cNamesList)
    
  END SUBROUTINE GetNames
  
  
  ! -------------------------------------------------------------
  ! --- GET MAX LAKE ELEVS
  ! -------------------------------------------------------------
  FUNCTION GetMaxElevs(AppLake,NLakes) RESULT(MaxElevs)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    INTEGER,INTENT(IN)            :: NLakes
    REAL(8)                       :: MaxElevs(NLakes)
    
    IF (AppLake%lDefined) MaxElevs = AppLake%Me%GetMaxElevs()
    
  END FUNCTION GetMaxElevs
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(AppLake_v40_Type) :: v40
    TYPE(AppLake_v50_Type) :: v50
    TYPE(VersionType)      :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) // ' (Interface) ; ' // TRIM(v40%GetVersion()) // ', ' // TRIM(v50%GetVersion()) // ' (Components)'
    
  END FUNCTION GetVersion
  
  
  
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
  SUBROUTINE ReadRestartData(AppLake,InFile,iStat)
    CLASS(AppLakeType)    :: AppLake
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(OUT)   :: iStat
    
    iStat = 0
    IF (AppLake%lDefined) CALL AppLake%Me%ReadRestartData(InFile,iStat)
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES  DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AppLake,TimeStep,ET,Precip,iStat)
    CLASS(AppLakeType)                 :: AppLake
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    
    iStat = 0
    IF (AppLake%lDefined) CALL AppLake%Me%ReadTSData(TimeStep,ET,Precip,iStat)
    
  END SUBROUTINE ReadTSData
    
  
  
  
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
  ! --- WRITE PREPROCESSED APPLICATION LAKES DATA
  ! -------------------------------------------------------------
  SUBROUTINE WritePreprocessedData(AppLake,OutFile)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    TYPE(GenericFileType)         :: OutFile
    
    IF (AppLake%lDefined) THEN
        CALL OutFile%WriteData(AppLake%iVersion)
        CALL AppLake%Me%WritePreprocessedData(OutFile)
    ELSE
        CALL OutFile%WriteData(0)
    END IF
    
  END SUBROUTINE WritePreprocessedData
  
    
  ! -------------------------------------------------------------
  ! --- PRINT OUT APPLICATION LAKES SIMULATION RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppLake,TimeStep,lEndOfSimulation,Runoff,ReturnFlow,LakeGWConnector,StrmLakeConnector)
    CLASS(AppLakeType)                     :: AppLake
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    LOGICAL,INTENT(IN)                     :: lEndOfSimulation
    REAL(8),INTENT(IN)                     :: Runoff(:),ReturnFlow(:)
    TYPE(LakeGWConnectorType),INTENT(IN)   :: LakeGWConnector
    TYPE(StrmLakeConnectorType),INTENT(IN) :: StrmLakeConnector
    
    IF (AppLake%lDefined) CALL AppLake%Me%PrintResults(TimeStep,lEndOfSimulation,Runoff,ReturnFlow,LakeGWConnector,StrmLakeConnector)
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(AppLake,OutFile)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    TYPE(GenericFileType)         :: OutFile
    
    IF (AppLake%lDefined) CALL AppLake%Me%PrintRestartData(OutFile)

  END SUBROUTINE PrintRestartData
  
  

    
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
  ! --- CONVERT IDs (MAINLY STREAM NODE IDs) TO INDICES
  ! -------------------------------------------------------------
  SUBROUTINE DestinationIDs_To_Indices(AppLake,iStrmNodeIDs,iStat)
    CLASS(AppLakeType)  :: AppLake
    INTEGER,INTENT(IN)  :: iStrmNodeIDs(:)
    INTEGER,INTENT(OUT) :: iStat
    
    IF (AppLake%lDefined) THEN
        CALL AppLake%Me%DestinationIDs_To_Indices(iStrmNodeIDs,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE DestinationIDs_To_Indices
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE LAKES
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppLake,GSElevs,GWHeads,Runoff,ReturnFlow,LakeGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppLakeType)                   :: AppLake
    REAL(8),INTENT(IN)                   :: GSElevs(:),GWHeads(:,:),Runoff(:),ReturnFlow(:)
    TYPE(LakeGWConnectorType),INTENT(IN) :: LakeGWConnector
    TYPE(StrmLakeConnectorType)          :: StrmLakeConnector
    TYPE(MatrixType)                     :: Matrix
    
    IF (AppLake%lDefined) THEN
        !Echo progress
        CALL EchoProgress('Simulating lakes')
    
        !Simulate
        CALL AppLake%Me%Simulate(GSElevs,GWHeads,Runoff,ReturnFlow,LakeGWConnector,StrmLakeConnector,Matrix)
    END IF
    
  END SUBROUTINE Simulate
  

  ! -------------------------------------------------------------
  ! --- ADD LAKE COMPONENT AND ITS CONNECTIVITY TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(AppLake,Matrix,iStat)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    TYPE(MatrixType)              :: Matrix
    INTEGER,INTENT(OUT)           :: iStat
    
    !Initialize
    iStat = 0
    
    IF (AppLake%lDefined) THEN
        CALL EchoProgress('Registering lake component with matrix...')
        CALL AppLake%Me%RegisterWithMatrix(Matrix,iStat)
    END IF
    
  END SUBROUTINE RegisterWithMatrix
  
  
  ! -------------------------------------------------------------
  ! --- ARE LAKES SIMULATED?
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AppLake) RESULT(lDefined)
    CLASS(AppLakeType),INTENT(IN) :: AppLake
    LOGICAL                       :: lDefined
    
    lDefined = AppLake%lDefined

  END FUNCTION IsDefined
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF LAKE RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppLake,NewUnit)
    CLASS(AppLakeType)      :: AppLake
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit

    IF (AppLake%lDefined) CALL AppLake%Me%ConvertTimeUnit(NewUnit)
    
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- RESET LAKE ELEVATIONS AND STORAGES TO PREVIOUS LEVELS
  ! -------------------------------------------------------------
  SUBROUTINE ResetElevations(AppLake)
    CLASS(AppLakeType) :: AppLake
    
    IF (AppLake%lDefined) CALL AppLake%Me%ResetElevations()

  END SUBROUTINE ResetElevations
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE THAT POINTED TIME-SERIES DATA HAVE ENOUGH COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE CheckExternalTSDataPointers(AppLake,Precip,ET,iStat)
    CLASS(AppLakeType),INTENT(IN)      :: AppLake
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ET
    INTEGER,INTENT(OUT)                :: iStat
    
    iStat = 0
    IF (AppLake%lDefined) CALL AppLake%Me%CheckExternalTSDataPointers(Precip,ET,iStat)
  
  END SUBROUTINE CheckExternalTSDataPointers
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF THE LAKES
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(AppLake)
    CLASS(AppLakeType) :: AppLake
    
    IF (AppLake%lDefined) CALL AppLake%Me%AdvanceState()
    
  END SUBROUTINE AdvanceState

  
  ! -------------------------------------------------------------
  ! --- MODIFY LAKE ELEVS USING DELTA_ELEV
  ! -------------------------------------------------------------
  SUBROUTINE UpdateHeads(AppLake,HDelta)
    CLASS(AppLakeType) :: AppLake
    REAL(8),INTENT(IN) :: HDelta(:)
    
    IF (AppLake%lDefined) CALL AppLake%Me%UpdateHeads(HDelta)
    
  END SUBROUTINE UpdateHeads
    
END MODULE