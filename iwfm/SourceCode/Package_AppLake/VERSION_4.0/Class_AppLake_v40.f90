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
MODULE Class_AppLake_v40                                          
  USE Class_Version                , ONLY: ReadVersion            
  USE MessageLogger                , ONLY: SetLastMessage                , &
                                           EchoProgress                  , &
                                           MessageArray                  , &
                                           iFatal                        
  USE GeneralUtilities             , ONLY: StripTextUntilCharacter       , &
                                           IntToText                     , &
                                           FirstLocation                 , &
                                           ShellSort                     , &
                                           EstablishAbsolutePathFilename , &
                                           CleanSpecialCharacters
  USE TimeSeriesUtilities          , ONLY: TimeStepType                              
  USE IOInterface                  , ONLY: GenericFileType                               
  USE Package_Misc                 , ONLY: PairedDataType                , &
                                           FlowDest_Outside              , &
                                           FlowDest_StrmNode             , &
                                           FlowDest_Lake                 , &
                                           iLakeComp                      
  USE Package_Discretization       , ONLY: AppGridType                   , &
                                           StratigraphyType
  USE Package_Budget               , ONLY: BudgetHeaderType              
  USE Package_ComponentConnectors  , ONLY: LakeGWConnectorType           , &
                                           StrmLakeConnectorType         , &
                                           iStrmToLakeType               , &
                                           iBypassToLakeType             , &
                                           iLakeToStrmType              
  USE Class_Lake                   , ONLY: LakeType                      , &
                                           ReadInitialLakeElevs          
  USE Class_MaxLakeElevFile                                              
  USE Package_PrecipitationET      , ONLY: ETType                        , &
                                           PrecipitationType             
  USE Package_Matrix               , ONLY: MatrixType                    
  USE Class_BaseAppLake            , ONLY: BaseAppLakeType               , &
                                           PrepareLakeBudgetHeader       , &
                                           GenerateRatingTable
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
  PUBLIC :: AppLake_v40_Type                         


  ! -------------------------------------------------------------
  ! --- APPLICATION LAKES DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseAppLakeType) :: AppLake_v40_Type
      PRIVATE
      INTEGER,ALLOCATABLE                   :: iColMaxElev(:)        !Pointer to data column in the maximum lake elevation data file
      TYPE(MaxLakeElevFileType),ALLOCATABLE :: MaxLakeElevFile
  CONTAINS
    PROCEDURE,PASS :: SetStaticComponent              => AppLake_v40_SetStaticComponent
    PROCEDURE,PASS :: SetStaticComponentFromBinFile   => AppLake_v40_SetStaticComponentFromBinFile
    PROCEDURE,PASS :: SetDynamicComponent             => AppLake_v40_SetDynamicComponent
    PROCEDURE,PASS :: SetAllComponents                => AppLake_v40_SetAllComponents
    PROCEDURE,PASS :: SetAllComponentsWithoutBinFile  => AppLake_v40_SetAllComponentsWithoutBinFile
    PROCEDURE,PASS :: KillImplementation              => AppLake_v40_Kill  
    PROCEDURE,PASS :: GetVersion                      => AppLake_v40_GetVersion
    PROCEDURE,PASS :: Simulate                        => AppLake_v40_Simulate
    PROCEDURE,PASS :: ReadTSData                      => AppLake_v40_ReadTSData
    PROCEDURE,PASS :: CheckExternalTSDataPointers     => AppLake_v40_CheckExternalTSDataPointers
    PROCEDURE,PASS :: ConvertTimeUnit                 => AppLake_v40_ConvertTimeUnit
  END TYPE AppLake_v40_Type  
      
  
  ! -------------------------------------------------------------
  ! --- LAKE PACKAGE VERSION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iVersion    = 40
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    ='4.0.0000'
  INCLUDE 'AppLake_v40_Revision.fi'
 
  
  ! -------------------------------------------------------------
  ! --- MISC. DATA 
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 19
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppLake_v40::'




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
  ! --- NEW LAKE FROM RAW DATA (GENERALLY CALLED IN PRE-PROCESSOR)
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_SetStaticComponent(AppLake,cFileName,Stratigraphy,AppGrid,NStrmNodes,StrmLakeConnector,LakeGWConnector,iStat)
    CLASS(AppLake_v40_Type),INTENT(OUT)   :: AppLake
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    INTEGER,INTENT(IN)                    :: NStrmNodes
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(LakeGWConnectorType),INTENT(OUT) :: LakeGWConnector
    INTEGER,INTENT(OUT)                   :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'AppLake_v40_SetStaticComponent'
    INTEGER                      :: NLakes,ErrorCode,indxLake,DummyArray(5),ID,iDestType,NElements, &
                                    indxElem,iDest,iElem,indxLake1
    INTEGER,PARAMETER            :: iDestTypes(3) = [FlowDest_Outside,FlowDest_StrmNode,FlowDest_Lake]
    CHARACTER(:),ALLOCATABLE     :: cVersion
    TYPE(GenericFileType)        :: InFile
    
    !Initialize
    iStat = 0

    !Return if filename is empty
    IF (cFileName .EQ. '') RETURN
    
    !Echo progress
    CALL EchoProgress('Instantiating static component of application lakes')
    
    !Open file
    CALL InFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='pre-processor lake data file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read the first line that holds version number
    CALL ReadVersion(InFile,'LAKE',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN

    !Number of lakes
    CALL InFile%ReadData(NLakes,iStat)  ;  IF(iStat .EQ. -1) RETURN
    AppLake%NLakes = NLakes
    
    !Allocate memory
    ALLOCATE (AppLake%Lakes(NLakes) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for lakes!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (NLakes .EQ. 0) RETURN
    
    !Read lake elements and outflow destination
    DO indxLake=1,NLakes
        CALL InFile%ReadData(DummyArray,iStat)  ;  IF(iStat .EQ. -1) RETURN
       
        !Lake ID
        ID = DummyArray(1)
        IF (ID .NE. indxLake) THEN
            MessageArray(1) = 'Lake data should be entered sequentially!'
            MessageArray(2) = 'Lake number expected = '//TRIM(IntToText(indxLake))
            MessageArray(3) = 'Lake number entered  = '//TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
       
        !Outflow destination type
        iDestType = DummyArray(2)
        IF (.NOT. ANY(iDestType.EQ.iDestTypes)) THEN
            CALL SetLastMessage('Outflow destination type for lake '//TRIM(IntToText(indxLake))//' is not recognized!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        AppLake%Lakes(indxLake)%OutflowDestType = iDestType
            
        !Outflow destination
        iDest                               = DummyArray(3)
        AppLake%Lakes(indxLake)%OutflowDest = iDest
        
        !Process outflow destination
        SELECT CASE (iDestType)
          !If flow to stream, make sure that stream node is modeled
          CASE (FlowDest_StrmNode)
            IF (iDest .GT. NStrmNodes) THEN
                CALL SetLastMessage('Stream node '//TRIM(IntToText(iDest))//' as outflow destination for lake '//TRIM(IntToText(indxLake))//' is not modeled!',iFatal,ThisProcedure) 
                iStat = -1
                RETURN
            END IF
            CALL StrmLakeConnector%AddData(iLakeToStrmType,indxLake,iDest)
          
          !If flow to lake, make sure it is a downstream lake and it is modeled
          CASE (FlowDest_Lake)
            IF (iDest .LE. indxLake) THEN
                MessageArray(1) = 'Outflow from a lake can only flow into a lake with a higher ID number.'
                MessageArray(2) = ' Outflow lake = '//TRIM(IntToText(indxLake))
                MessageArray(3) = ' Inflow lake  = '//TRIM(IntToText(iDest))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF (iDest .GT. NLakes) THEN
                CALL SetLastMessage('Lake ID '//TRIM(IntToText(iDest))//' as outflow destination for lake '//TRIM(IntToText(indxLake))//' is not modeled!',iFatal,ThisProcedure) 
                iStat = -1
                RETURN
            END IF
        END SELECT
                    
        !Lake elements
        NElements                         = DummyArray(4)
        AppLake%Lakes(indxLake)%NElements = NElements  
        ALLOCATE (AppLake%Lakes(indxLake)%Elements(NElements))
        AppLake%Lakes(indxLake)%Elements(1) = DummyArray(5)
        DO indxElem=2,NElements
            CALL InFile%ReadData(AppLake%Lakes(indxLake)%Elements(indxElem),iStat)  
            IF(iStat .EQ. -1) RETURN
        END DO
        CALL ShellSort(AppLake%Lakes(indxLake)%Elements)
        
        !Make sure lake elements are not listed for more than one lake
        DO indxElem=1,NElements
            iElem = AppLake%Lakes(indxLake)%Elements(indxElem)
            DO indxLake1=1,indxLake-1
                IF (ANY(iElem .EQ. AppLake%Lakes(indxLake1)%Elements)) THEN
                    CALL SetLastMessage('Element '//TRIM(IntToText(iElem))//' listed for lake '//TRIM(IntToText(indxLake))//' is also listed for lake '//TRIM(IntToText(indxLake1))//'!',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
        END DO
       
        !Lake nodes
        CALL AppLake%Lakes(indxLake)%CompileLakeNodes(AppGrid,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Rating table
        CALL GenerateRatingTable(AppGrid,AppLake%Lakes(indxLake)%Elements,AppLake%Lakes(indxLake)%Nodes,Stratigraphy%GSElev,AppLake%Lakes(indxLake)%RatingTable,iStat)
        IF (iStat .EQ. -1) RETURN
       
        !Lake node areas
        CALL AppLake%Lakes(indxLake)%ComputeLakeNodeAreas(AppGrid)
        
        !Add GW nodes to LakeGWConnector
        CALL LakeGWConnector%AddGWNodes(indxLake,AppLake%Lakes(indxLake)%Elements,AppGrid,Stratigraphy)
        
    END DO
    
    !Close lake data file
    CALL InFile%Kill()

  END SUBROUTINE AppLake_v40_SetStaticComponent
  

  ! -------------------------------------------------------------
  ! --- NEW STATIC LAKE DATA FROM PRE-PROCESSOR BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_SetStaticComponentFromBinFile(AppLake,BinFile,iStat)
    CLASS(AppLake_v40_Type),INTENT(OUT) :: AppLake
    TYPE(GenericFileType)               :: BinFile
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Initialize
    iStat = 0
  
    !Read the preprocessed data for lakes
    CALL AppLake%ReadPreprocessedData(BinFile,iStat)
    
  END SUBROUTINE AppLake_v40_SetStaticComponentFromBinFile
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE THE DYNAMIC COMPONENT OF LAKE DATA (GENERALLY CALLED IN SIMULATION)
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_SetDynamicComponent(AppLake,IsForInquiry,cFileName,cWorkingDirectory,TimeStep,NTIME,AppGrid,LakeGWConnector,iStat)
    CLASS(AppLake_v40_Type)       :: AppLake
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(LakeGWConnectorType)     :: LakeGWConnector
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31) :: ThisProcedure = ModName // 'AppLake_v40_SetDynamicComponent'
    TYPE(GenericFileType)        :: LakeDataFile
    CHARACTER                    :: ALine*2000,TimeUnitConductance*6,cLakeBudgetFileName*2000
    TYPE(BudgetHeaderType)       :: BudHeader
    REAL(8)                      :: FactK,DummyArray(6),FactL,CLAKE,DLAKE,FactC
    INTEGER                      :: indxLake,ID,indx,iLoc
    CHARACTER(:),ALLOCATABLE     :: cVersion,cAbsPathFileName
    
    !Initialize
    iStat = 0
   
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Echo progress
    CALL EchoProgress('Instantiating dynamic component of application lakes')
    
    !Open lake data file
    CALL LakeDataFile%New(cFileName,InputFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read first line that stores the version number
    CALL ReadVersion(LakeDataFile,'LAKE',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Maximum lake elevation data file
    CALL LakeDataFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .EQ. '') THEN
        CALL SetLastMessage('Maximum lake elevations file must be specified when lakes are simulated!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    ALLOCATE (AppLake%MaxLakeElevFile , AppLake%iColMaxElev(AppLake%NLakes))
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AppLake%MaxLakeElevFile%New(cAbsPathFileName,TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
        
    !Lake budget raw file name
    CALL LakeDataFile%ReadData(cLakeBudgetFileName,iStat)  ;  IF(iStat .EQ. -1) RETURN
    cLakeBudgetFileName = StripTextUntilCharacter(cLakeBudgetFileName,'/') 
    CALL CleanSpecialCharacters(cLakeBudgetFileName)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cLakeBudgetFileName)),cWorkingDirectory,cAbsPathFileName)
    cLakeBudgetFileName = cAbsPathFileName 
    
    !Final lake elevations output file
    CALL LakeDataFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN  ;  ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/'))  ;  CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        ALLOCATE (AppLake%FinalElevFile)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL AppLake%FinalElevFile%New(FileName=cAbsPathFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='end-of-simulation lake elevations data',iStat=iStat)
        ELSE
            CALL AppLake%FinalElevFile%New(FileName=cAbsPathFileName,InputFile=.FALSE.,IsTSFile=.FALSE.,Descriptor='end-of-simulation lake elevations data',iStat=iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
        AppLake%lFinalElevFile_Defined = .TRUE.
    END IF
    
    !Read factors
    CALL LakeDataFile%ReadData(FactK,iStat)  ;  IF(iStat .EQ. -1) RETURN
    CALL LakeDataFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    TimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL LakeDataFile%ReadData(FactL,iStat)  ;  IF(iStat .EQ. -1) RETURN

    !Read lake parameters
    DO indxLake=1,AppLake%NLakes
      CALL LakeDataFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN
      READ (ALine,*) DummyArray
      
      !Make sure that lake data is entered sequentially
      ID = INT(DummyArray(1))
      IF (ID .NE. indxLake) THEN 
          MessageArray(1) = 'Lake data should be entered sequentialy.'
          MessageArray(2) = 'Expected lake = '//TRIM(IntToText(indxLake))
          MessageArray(3) = 'Entered lake  = '//TRIM(IntToText(ID))
          CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      CLAKE                               = DummyArray(2)*FactK
      DLAKE                               = DummyArray(3)*FactL
      AppLake%iColMaxElev(indxLake)       = INT(DummyArray(4))
      AppLake%Lakes(indxLake)%iColET      = INT(DummyArray(5))
      AppLake%Lakes(indxLake)%iColPrecip  = INT(DummyArray(6))
      FactC                               = CLAKE / DLAKE
      
      !Compile lake-gw connector
      CALL LakeGWConnector%SetConductance(AppGrid,indxLake,TimeUnitConductance,FactC,iStat)
      IF (iStat .EQ. -1) RETURN
 
      !Make sure that iColMaxElev is consistent with the data columns in the file
      IF (AppLake%iColMaxElev(indxLake) .GT. AppLake%MaxLakeElevFile%iSize) THEN
          MessageArray(1) = 'Maximum lake elevation data column for lake '//TRIM(IntToText(indxLake))//' is greater than the'
          MessageArray(2) = 'available data columns in the Maximum Lake Elevations Data File!'
          CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Extract the name of the lake from ALine
      CALL CleanSpecialCharacters(ALine)
      ALine = ADJUSTL(ALine)
      DO indx=1,SIZE(DummyArray)
        iLoc  = FirstLocation(' ',ALine)
        ALine = ADJUSTL(ALine(iLoc+1:))
      END DO
      AppLake%Lakes(indxLake)%cName = StripTextUntilCharacter(ADJUSTL(ALine),'/')
      
    END DO
    
    !Instantiate the lake budget file
    IF (cLakeBudgetFileName .NE. '') THEN
        ALLOCATE (AppLake%LakeBudRawFile)
        IF (IsForInquiry) THEN
            CALL AppLake%LakeBudRawFile%New(TRIM(cLakeBudgetFileName),iStat)
            IF (iStat .EQ. -1) RETURN
        ELSE
            BudHeader = PrepareLakeBudgetHeader(AppLake%Lakes,NTIME,TimeStep,AppLake%GetVersion())
            CALL AppLake%LakeBudRawFile%New(TRIM(cLakeBudgetFileName),BudHeader,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL BudHeader%Kill()
        END IF
        AppLake%LakeBudRawFile_Defined = .TRUE.
    END IF

    !Initial lake elevations
    CALL ReadInitialLakeElevs(LakeDataFile,AppLake%Lakes,iStat)
    
    !Close file
    CALL LakeDataFile%Kill()
    
  END SUBROUTINE AppLake_v40_SetDynamicComponent
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE LAKE DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_SetAllComponents(AppLake,IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,BinFile,LakeGWConnector,iStat)
    CLASS(AppLake_v40_Type),INTENT(OUT) :: AppLake
    LOGICAL,INTENT(IN)                  :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName,cSimWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)       :: TimeStep
    INTEGER,INTENT(IN)                  :: NTIME
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    TYPE(GenericFileType)               :: BinFile
    TYPE(LakeGWConnectorType)           :: LakeGWConnector
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+28) :: ThisProcedure = ModName // 'AppLake_v40_SetAllComponents'
    
    !Initialize
    iStat = 0
    
    !Read the preprocessed data for lakes
    CALL AppLake%ReadPreprocessedData(BinFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Return if there are no lakes defined in Pre-Processor
    IF (AppLake%NLakes .EQ. 0) RETURN
    
    !Set the dynamic part of AppLake
    CALL AppLake%SetDynamicComponent(IsForInquiry,cFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that if static part is defined, so is the dynamic part
    IF (AppLake%NLakes .GT. 0) THEN
        IF (cFileName .EQ. '') THEN
            MessageArray(1) = 'For proper simulation of lakes, relevant lake data files must'
            MessageArray(2) = 'be specified when lakes are defined in Pre-Processor.'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF 
    
  END SUBROUTINE AppLake_v40_SetAllComponents
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE COMPLETE LAKE DATA WITHOUT INTERMEDIATE BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_SetAllComponentsWithoutBinFile(AppLake,IsForInquiry,cPPFileName,cSimFileName,cSimWorkingDirectory,AppGrid,Stratigraphy,TimeStep,NTIME,NStrmNodes,StrmLakeConnector,LakeGWConnector,iStat)
    CLASS(AppLake_v40_Type),INTENT(OUT)   :: AppLake
    LOGICAL,INTENT(IN)                    :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cPPFileName,cSimFileName,cSimWorkingDirectory
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)     :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    INTEGER,INTENT(IN)                    :: NTIME,NStrmNodes
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    TYPE(LakeGWConnectorType),INTENT(OUT) :: LakeGWConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+43) :: ThisProcedure = ModName // 'AppLake_v40_SetAllComponentsWithoutBinFile'
    
    !Initialize
    iStat = 0
    
    !Instantiate the static components of the AppLake data
    CALL AppLake%SetStaticComponent(cPPFileName,Stratigraphy,AppGrid,NStrmNodes,StrmLakeConnector,LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the dynamic component of the AppLake data
    CALL AppLake%SetDynamicComponent(IsForInquiry,cSimFileName,cSimWorkingDirectory,TimeStep,NTIME,AppGrid,LakeGWConnector,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that if static part is defined, so is the dynamic part
    IF (AppLake%NLakes .GT. 0) THEN
        IF (AppLake%MaxLakeElevFile%iSize .EQ. 0) THEN
            MessageArray(1) = 'For proper simulation of lakes, relevant lake data files must'
            MessageArray(2) = 'be specified when lakes are defined in Pre-Processor.'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF 
  
  END SUBROUTINE AppLake_v40_SetAllComponentsWithoutBinFile

  
  
  
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
  ! --- KILL LAKE DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_Kill(AppLake)
    CLASS(AppLake_v40_Type) :: AppLake
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (AppLake%iColMaxElev , STAT=ErrorCode)
    
    !Kill maximum lake elevation data file object
    IF (ALLOCATED(AppLake%MaxLakeElevFile)) THEN
        CALL AppLake%MaxLakeElevFile%Kill()
        DEALLOCATE (AppLake%MaxLakeElevFile , STAT=ErrorCode)
    END IF
    
  END SUBROUTINE AppLake_v40_Kill
  
  
  
  
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
  FUNCTION AppLake_v40_GetVersion(AppLake) RESULT(cVrs)
    CLASS(AppLake_v40_Type)  :: AppLake
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    IF (.NOT. AppLake%Version%IsDefined())   &
        AppLake%Version = AppLAke%Version%New(iLenVersion,cVersion,cRevision)

    cVrs = AppLake%Version%GetVersion()
    
  END FUNCTION AppLake_v40_GetVersion
  

  
  
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
  ! --- READ TIME SERIES  DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_ReadTSData(AppLake,TimeStep,ET,Precip,iStat)
    CLASS(AppLake_v40_Type)            :: AppLake
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'AppLake_v40_ReadTSData'
    INTEGER                      :: indxLake
    REAL(8)                      :: rLowGSElev,rMaxElev
    
    !Initialzie
    iStat = 0
    
    !Lake precipitation
    IF (Precip%IsUpdated()) AppLake%Lakes%PrecipRate = Precip%GetValues(AppLake%Lakes%iColPrecip)
    
    !Lake potential evapotranspiration
    IF (ET%IsUpdated()) AppLake%Lakes%ETp_Rate = ET%GetValues(AppLake%Lakes%iColET)
    
    !Maximum lake elevation
    CALL AppLake%MaxLakeElevFile%ReadTSData(TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that maximum lake elevation is greater than the minimum ground surface elevation
    IF (AppLake%MaxLakeElevFile%lUpdated) THEN
        AppLake%Lakes%MaxElev = AppLake%MaxLakeElevFile%rValues(AppLake%iColMaxElev)
        DO indxLake=1,AppLake%NLakes
            rLowGSElev = AppLake%Lakes(indxLake)%RatingTable%XPoint(1)
            rMaxElev   = AppLake%Lakes(indxLake)%MaxElev
            IF (rLowGSElev .GT. rMaxElev) THEN
                MessageArray(1) = 'Maximum lake elevation at lake '//TRIM(IntToText(indxLake))//' is lower than lake bottom!'
                WRITE (MessageArray(2),'(A,F6.2)') 'Maximum lake elevation = ',rMaxElev
                WRITE (MessageArray(3),'(A,F6.2)') 'Lake bottom elevation  = ',rLowGSElev
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    
  END SUBROUTINE AppLake_v40_ReadTSData
  
  
 
  
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
  ! --- SIMULATE LAKES
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_Simulate(AppLake,GSElevs,GWHeads,Runoff,ReturnFlow,LakeGWConnector,StrmLakeConnector,Matrix)
    CLASS(AppLake_v40_Type)              :: AppLake
    REAL(8),INTENT(IN)                   :: GSElevs(:),GWHeads(:,:),Runoff(:),ReturnFlow(:)
    TYPE(LakeGWConnectorType),INTENT(IN) :: LakeGWConnector
    TYPE(StrmLakeConnectorType)          :: StrmLakeConnector
    TYPE(MatrixType)                     :: Matrix
    
    !Local variables
    INTEGER                           :: indxLake,NLakes,iDest,iNodeIDs(1)
    REAL(8),DIMENSION(AppLake%NLakes) :: StrmInflows,LakeGWFlow_AtMaxLakeElev
    REAL(8)                           :: Elev,rOutflow,rUpdateValues(1),rUpdateRHS(AppLake%NLakes),OtherInflow
    INTEGER,PARAMETER                 :: iCompIDs(1) = [iLakeComp]
    
    !Initialize
    NLakes                     =  AppLake%NLakes
    AppLake%Lakes%InflowUplake =  0.0
    AppLake%Lakes%Outflow      =  0.0
    CALL StrmLakeConnector%ResetLakeToStrmFlows()
    
    !Compute inflows from streams
    DO indxLake=1,NLakes
      StrmInflows(indxLake) = StrmLakeConnector%GetFlow(iStrmToLakeType,indxLake)  &
                            + StrmLakeConnector%GetFlow(iBypassToLakeType,indxLake)
    END DO
    
    !Lake-gw interaction at maximum lake elevations
    LakeGWFlow_AtMaxLakeElev= LakeGWConnector%ComputeLakeGWFlow(NLakes,AppLake%Lakes%MaxElev,GWHeads,GSElevs)
   
    !Actual ET
    CALL AppLake%ComputeLakeETa(GSElevs,StrmInflows)
    
    !Compile lake equations
    DO indxLake=1,NLakes
      !Initialize
      Elev        = AppLake%Lakes(indxLake)%Elev                                    !Lake elevation
      iNodeIDs(1) = indxLake                                                        !Lake to lake connectivity node
      OtherInflow = StrmInflows(indxLake) + Runoff(indxLake) + ReturnFlow(indxLake) !Inflows into lake
  
      !Lake storage
      AppLake%Lakes(indxLake)%Storage = MAX(AppLake%Lakes(indxLake)%RatingTable%Evaluate(Elev) , 0.0)
  
      !RHS vector (compute lake outflow if necessary)
      rOutflow                        = RHSLake(AppLake%Lakes(indxLake),AppLake%Lakes(indxLake)%MaxElev,OtherInflow) + LakeGWFlow_AtMaxLakeElev(indxLake)
      rOutflow                        = -MIN(rOutflow , 0.0)
      rUpdateRHS(indxLake)            = RHSLake(AppLake%Lakes(indxLake),Elev,OtherInflow) + rOutflow
      AppLake%Lakes(indxLake)%Outflow = rOutflow
      
      !Send outflow to destination
      IF (rOutflow .GT. 0.0) THEN
          iDest = AppLake%Lakes(indxLake)%OutflowDest
          SELECT CASE (AppLake%Lakes(indxLake)%OutflowDestType)
              CASE (FlowDest_Lake)
                  AppLake%Lakes(iDest)%InflowUpLake = AppLake%Lakes(iDest)%InflowUpLake + rOutflow
              CASE (FlowDest_StrmNode)
                  CALL StrmLakeConnector%SetFlow(iLakeToStrmType,indxLake,iDest,rOutflow)
          END SELECT
      END IF

      !Update COEFF matrix
      rUpdateValues(1) = AppLake%Lakes(indxLake)%RatingTable%Derivative(Elev)
      CALL Matrix%UpdateCOEFF(iLakeComp,indxLake,iCompIDs,iNodeIDs,rUpdateValues)
        
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(iLakeComp,1,rUpdateRHS)

  END SUBROUTINE AppLake_v40_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE RIGHT-HAND-SIDE OF A LAKE EQUATION
  ! -------------------------------------------------------------
  FUNCTION RHSLake(Lake,HLake,OtherInflow) RESULT(RHS)
    TYPE(LakeType),INTENT(IN) :: Lake
    REAL(8),INTENT(IN)        :: HLake,OtherInflow
    REAL(8)                   :: RHS

    !Local variables
    REAL(8) :: Storage

    !Compute storage at HLake
    Storage = MAX(Lake%RatingTable%Evaluate(HLake) , 0.0)

    RHS =  Storage                      & !Storage 
         - Lake%Storage_P               & !Storage at the previous time step
         - Lake%PrecipRate * Lake%Area  & !Precipitation
         + Lake%ETa                     & !Evaporation
         - OtherInflow                  & !All other inflows to lake
         - Lake%InflowUpLake              !Inflow from upstream lake

  END FUNCTION RHSLake


  ! -------------------------------------------------------------
  ! --- MAKE SURE THAT POINTED TIME-SERIES DATA HAVE ENOUGH COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_CheckExternalTSDataPointers(AppLake,Precip,ET,iStat)
    CLASS(AppLake_v40_Type),INTENT(IN) :: AppLake
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ET
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(Len=ModNameLen+39) :: ThisProcedure = ModName // 'AppLake_v40_CheckExternalTSDataPointers'
    INTEGER                      :: iLake(1)
    
    !Initialize
    iStat = 0
    
    !Check precip columns
    IF (Precip%GetNDataColumns() .LT. MAXVAL(AppLake%Lakes%iColPrecip)) THEN
        iLake = MAXLOC(AppLake%Lakes%iColPrecip)
        MessageArray(1) = 'Precipitation data column for lake '//TRIM(IntToText(iLake(1)))//' is greater than the'
        MessageArray(2) = 'available data columns in the Precipitation Data file!'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Check ET columns
    IF (ET%GetNDataColumns() .LT. MAXVAL(AppLake%Lakes%iColET)) THEN
        iLake = MAXLOC(AppLake%Lakes%iColET)
        MessageArray(1) = 'Evapotranspiration data column for lake '//TRIM(IntToText(iLake(1)))//' is greater than the'
        MessageArray(2) = 'available data columns in the Evapotranspiration Data file!'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
  END SUBROUTINE AppLake_v40_CheckExternalTSDataPointers
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF LAKE RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE AppLake_v40_ConvertTimeUnit(AppLake,NewUnit)
    CLASS(AppLake_v40_Type)     :: AppLake
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !No time unit to be converted in this version
    
  END SUBROUTINE AppLake_v40_ConvertTimeUnit
  
  
END MODULE