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
MODULE Class_UrbanLandUseGW
  !$ USE OMP_LIB
  USE MessageLogger            , ONLY: SetLastMessage                   , &
                                       EchoProgress                     , &
                                       MessageArray                     , &
                                       f_iFatal                           
  USE GeneralUtilities         , ONLY: StripTextUntilCharacter          , &
                                       EstablishAbsolutePathFileName    , &
                                       CleanSpecialCharacters           , &
                                       IntToText                        , &
                                       NormalizeArray                   , &
                                       LocateInList                     
  USE TimeSeriesUtilities      , ONLY: TimeStepType                     , &
                                       IncrementTimeStamp               , &
                                       OPERATOR(.TSGT.)
  USE IOInterface              , ONLY: GenericFileType                  
  USE Package_Misc             , ONLY: RealTSDataInFileType             , &
                                       IntTSDataInFileType              , &
                                       SolverDataType                   , & 
                                       ReadTimeSeriesData => ReadTSData , &
                                       f_iFlowDest_GWElement         
  USE Class_BaseRootZone       , ONLY: TrackMoistureDueToSource         , &
                                       CalculateUrbanFracDemand         
  USE Class_GenericLandUseGW   , ONLY: GenericLandUseGWType             , &
                                       ComputeETFromGW_Max              
  USE Class_LandUseDataFile    , ONLY: LandUseDataFileType              
  USE Package_Discretization   , ONLY: AppGridType                      
  USE Package_PrecipitationET  , ONLY: ETType   
  USE Util_Package_RootZone    , ONLY: ReadRealData
  USE Util_RootZone_v41        , ONLY: RootZoneSoil_v41_Type 
  USE Package_UnsatZone       , ONLY: NonPondedLUMoistureRouter               
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
  PUBLIC :: UrbanDatabaseType
 
  
  ! -------------------------------------------------------------
  ! --- URBAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseGWType) :: UrbanType
    INTEGER :: iColReturnFrac       = 0     !Column number in the return flow fraction data file
    INTEGER :: iColReuseFrac        = 0     !Column number in the re-use fraction data file
    INTEGER :: iColPopulation       = 0     !Column number in the population data file
    INTEGER :: iColPerCapitaWaterUse= 0     !Column number in the per capita water use data file
    INTEGER :: iColWaterUseSpec     = 0     !Column number in the urban water use specs data file
    REAL(8) :: FracDemand           = 1.0   !Relative proportion of demand computed by multiplying popultaion with per capita water use to be applied to an element
    REAL(8) :: Demand               = 0.0   !Urban water demand
    REAL(8) :: PerviousFrac         = 0.0   !Fraction of pervious area to total urban area
    REAL(8) :: IrigInfilt           = 0.0   !Infiltration due to irrigation
    REAL(8) :: Reuse                = 0.0   !Reused return flow 
    REAL(8) :: ReturnFlow           = 0.0   !Return flow
    REAL(8) :: ElemDemandFrac       = 0.0   !Ratio of urban demand to the total demand at the element it is located at
  END TYPE UrbanType
  
  
  ! -------------------------------------------------------------
  ! --- URBAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE UrbanDatabaseType
    TYPE(UrbanType),ALLOCATABLE :: UrbData(:)                      !Urban data for each element
    REAL(8)                     :: RootDepth               = 0.0   !Urban root depth
    REAL(8)                     :: PerCapitaWaterUseFactor = 1.0   !Conversion factor for water demand
    LOGICAL                     :: lFracDemand_wrt_Area = .FALSE.  !Flag to see if FracDemand is specified by the user as a constant or with respect to urban area in each element
    REAL(8),ALLOCATABLE         :: RegionETPot(:)                  !Regional urban potential ET
    TYPE(LandUseDataFileType)   :: LandUseDataFile                 !Land use data file
    TYPE(RealTSDataInFileType)  :: PerCapitaWaterUseFile           !Urban per capita water use data file
    TYPE(IntTSDataInFileType)   :: PopulationFile                  !Population data file
    TYPE(RealTSDataInFileType)  :: WaterUseSpecsFile               !Urban water use specs data file
  CONTAINS
    PROCEDURE,PASS :: New                          
    PROCEDURE,PASS :: Kill 
    PROCEDURE,PASS :: GetMaxAndMinNetReturnFlowFrac
    PROCEDURE,PASS :: SetAreas                     
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadTSData                   
    PROCEDURE,PASS :: AdvanceAreas                 
    PROCEDURE,PASS :: SoilMContent_To_Depth        
    PROCEDURE,PASS :: Simulate                     
    PROCEDURE,PASS :: ComputeWaterDemand           
    PROCEDURE,PASS :: ComputeETFromGW_Max => UrbanLandUse_ComputeETFromGW_Max         
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp          
  END TYPE UrbanDatabaseType

  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_UrbanLandUseGW::'

  


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
  ! --- NEW URBAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(UrbLand,cFileName,cWorkingDirectory,FactCN,NElements,NSubregions,iElemIDs,TrackTime,iStat)
    CLASS(UrbanDatabaseType)    :: UrbLand
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)          :: FACTCN
    INTEGER,INTENT(IN)          :: NElements,NSubregions,iElemIDs(NElements)
    LOGICAL,INTENT(IN)          :: TrackTime
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)  :: ThisProcedure = ModName // 'New'
    CHARACTER                    :: ALine*1000
    INTEGER                      :: ErrorCode,indxElem,iElem,ID
    REAL(8)                      :: FACT,Factor(1)
    LOGICAL                      :: lError,lProcessed(NElements)
    REAL(8),ALLOCATABLE          :: DummyArray(:,:)
    TYPE(GenericFileType)        :: UrbanDataFile
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    
    !Initialize
    iStat = 0
   
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Open file
    CALL UrbanDataFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (UrbLand%UrbData(NElements)       ,  &
              UrbLand%RegionETPot(NSubregions) ,  &
              STAT=ErrorCode                   )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for urban data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Land use data file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Urban area file',NElements,1,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Rooting depths
    CALL UrbanDataFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL UrbanDataFile%ReadData(UrbLand%RootDepth,iStat)  ;  IF (iStat .EQ. -1) RETURN
    UrbLand%RootDepth = UrbLand%RootDepth * FACT
    
    !Urban population file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%PopulationFile%Init(cAbsPathFileName,cWorkingDirectory,'Population data file',TrackTime,1,iStat)
    IF (iStat .EQ. -1) RETURN
      
    !Urban per capita water use file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%PerCapitawaterUseFile%Init(cAbsPathFileName,cWorkingDirectory,'Urban per capita water use data file',TrackTime,1,.TRUE.,Factor,(/.TRUE./),iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    UrbLand%PerCapitawaterUseFactor = Factor(1)
      
    !Urban water use specifications data file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%WaterUseSpecsFile%Init(cAbsPathFileName,cWorkingDirectory,'Urban water use specifications',TrackTime,1,.FALSE.,Factor,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN

    !Read other data
    CALL ReadRealData(UrbanDataFile,'data for urban lands','elements',NElements,10,iElemIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ASSOCIATE (pUrbData  => UrbLand%UrbData  )
        lProcessed = .FALSE.
        DO indxElem=1,NElements
            iElem = INT(DummyArray(indxElem,1))
            IF (lProcessed(iElem)) THEN
                ID = iElemIDs(iElem)
                CALL SetLastMessage('Data for urban lands at element '//TRIM(IntToText(ID))//' is defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iElem) = .TRUE.
            
            pUrbData(iElem)%PerviousFrac          =     DummyArray(indxElem,2)
            pUrbData(iElem)%SMax                  = (1000.0/DummyArray(indxElem,3)-10.0) * FACTCN
            pUrbData(iElem)%iColPopulation        = INT(DummyArray(indxElem,4))
            pUrbData(iElem)%iColPerCapitaWaterUse = INT(DummyArray(indxElem,5))
            pUrbData(iElem)%FracDemand            =     DummyArray(indxElem,6)
            pUrbData(iElem)%iColETc               = INT(DummyArray(indxElem,7))
            pUrbData(iElem)%iColReturnFrac        = INT(DummyArray(indxElem,8))
            pUrbData(iElem)%iColReuseFrac         = INT(DummyArray(indxElem,9))
            pUrbData(iElem)%iColWaterUseSpec      = INT(DummyArray(indxElem,10))
        END DO
      
        !Make sure FRACDM is either entered all negative or all greater than (or equal to) zero
        lError = .FALSE.
        IF (pUrbData(1)%FracDemand .LT. 0.0) THEN
            UrbLand%lFracDemand_wrt_Area = .TRUE.
            IF (ANY(pUrbData(2:)%FracDemand.GE.0.0)) lError = .TRUE.
        ELSE
            UrbLand%lFracDemand_wrt_Area = .FALSE.
            IF (ANY(pUrbData(2:)%FracDemand.LT.0.0)) lError = .TRUE.
        END IF
        IF (lError) THEN
            MessageArray(1) = 'FRACDM variable in urban main data file must be all specified '
            MessageArray(2) = 'as either -1.0 or greater than (or equal to) zero!'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
                  
        !Normalize the relative fractions to distribute the demands, if fractions are specified by the user as constant
        IF (.NOT. UrbLand%lFracDemand_wrt_Area)  &
            CALL CalculateUrbanFracDemand(NElements,pUrbData%iColPopulation,pUrbData%iColPerCapitaWaterUse,pUrbData%FracDemand)
        
        !Check for timeseries column pointer errors
        DO indxElem=1,NElements
            ID = iElemIDs(indxElem)
            CALL UrbLand%PopulationFile%CheckColNum('Urban population file as referenced by element '//TRIM(IntToText(ID)),[pUrbData(indxElem)%iColPopulation],.TRUE.,iStat)                          ;  IF (iStat .EQ. -1) RETURN
            CALL UrbLand%PerCapitaWaterUseFile%CheckColNum('Urban per capita water use file as referenced by element '//TRIM(IntToText(ID)),[pUrbData(indxElem)%iColPerCapitaWaterUse],.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL UrbLand%WaterUseSpecsFile%CheckColNum('Urban water use specifications file as referenced by element '//TRIM(IntToText(ID)),[pUrbData(indxElem)%iColWaterUseSpec],.TRUE.,iStat)       ;  IF (iStat .EQ. -1) RETURN
        END DO

    END ASSOCIATE

    !Initial conditions
    CALL ReadRealData(UrbanDataFile,'initial conditions for urban lands','elements',NElements,3,iElemIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(DummyArray(:,2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,2)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some fractions of initial soil moisture due to precipitation is less '
        MessageArray(2) = 'than 0.0 or greater than 1.0 for urban areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    IF (MINVAL(DummyArray(:,3:)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,3:)) .GT. 1.0          ) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 or greater than 1.0 for urban areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('initial conditions for urban lands at element '//TRIM(IntToText(ID))//' is defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                                  = .TRUE.
        UrbLand%UrbData(iElem)%SoilM_Precip                = DummyArray(indxElem,2) * DummyArray(indxElem,3)
        UrbLand%UrbData(iElem)%SoilM_AW                    = DummyArray(indxElem,3) - UrbLand%UrbData(iElem)%SoilM_Precip
        UrbLand%UrbData(iElem)%SoilM_Precip_P_BeforeUpdate = UrbLand%UrbData(iElem)%SoilM_Precip 
        UrbLand%UrbData(iElem)%SoilM_Precip_P              = UrbLand%UrbData(iElem)%SoilM_Precip 
        UrbLand%UrbData(iElem)%SoilM_AW_P_BeforeUpdate     = UrbLand%UrbData(iElem)%SoilM_AW 
        UrbLand%UrbData(iElem)%SoilM_AW_P                  = UrbLand%UrbData(iElem)%SoilM_AW 
    END DO
    
    !Close file
    CALL UrbanDataFile%Kill()
    
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
  ! --- KILL URBAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(UrbLand)
    CLASS(UrbanDatabaseType) :: UrbLand

    !Local variables
    INTEGER                 :: ErrorCode
    TYPE(UrbanDatabaseType) :: Dummy
    
    !Deallocate arrays
    DEALLOCATE (UrbLand%UrbData  , &
                STAT = ErrorCode )
    
    !Close files
    CALL UrbLand%LandUseDataFile%Kill()
    CALL UrbLand%PerCapitaWaterUseFile%Close()
    CALL UrbLand%PopulationFile%Close()
    CALL UrbLand%WaterUseSpecsFile%Close()
    
    !Assign default values to components
    SELECT TYPE (UrbLand)
        TYPE IS (UrbanDatabaseType)
            UrbLand = Dummy
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
  ! --- GET MIN AND MAX NET RETURN FLOW FRACTIONS THROUGH THE ENTITE SIMULATION PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE GetMaxAndMinNetReturnFlowFrac(UrbLand,ReturnFracFile,ReuseFracFile,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    CLASS(UrbanDatabaseType),INTENT(IN) :: UrbLand
    TYPE(RealTSDataInFileType)          :: ReturnFracFile,ReuseFracFile
    TYPE(TimeStepType),INTENT(IN)       :: FirstTimeStep
    REAL(8),INTENT(OUT)                 :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    INTEGER            :: FileReadCode_Return,FileReadCode_Reuse,indxElem
    REAL(8)            :: rRT,rRU
    
    !Initialize
    TimeStep = FirstTimeStep
    rMaxFrac = 0.0
    rMinFrac = 1.0
    
    !Loop through timesteps and read return flow fractions
    DO
        !Read data
        CALL ReadTimeSeriesData(TimeStep,'Return flow fractions data',ReturnFracFile,FileReadCode_Return,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ReadTimeSeriesData(TimeStep,'Reuse fractions data',ReuseFracFile,FileReadCode_Reuse,iStat)          ;  IF (iStat .EQ. -1) RETURN
        
        !If new data is read, find min and max
        IF (FileReadCode_Return.EQ.0  .OR.  FileReadCode_Reuse.EQ.0) THEN
            DO indxElem=1,SIZE(UrbLand%UrbData)
                rRT      = ReturnFracFile%rValues(UrbLand%UrbData(indxElem)%iColReturnFrac)
                rRU      = ReuseFracFile%rValues(UrbLand%UrbData(indxElem)%iColReuseFrac)
                rMaxFrac = MAX(rMaxFrac , rRT-rRU)
                rMinFrac = MIN(rMinFrac , rRT-rRU)
            END DO
        END IF
        
        !Advance time
        TimeStep%CurrentDateAndTime = IncrementTimeStamp(TimeStep%CurrentDateAndTime,TimeStep%DELTAT_InMinutes)
        
        !Exit if past the simulation end date
        IF (TimeStep%CurrentDateAndTime .TSGT. TimeStep%EndDateAndTime) EXIT

    END DO
      
  END SUBROUTINE GetMaxAndMinNetReturnFlowFrac
  
  

  
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
  ! --- SET THE URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE SetAreas(UrbLand,Area)
    CLASS(UrbanDatabaseType) :: UrbLand
    REAL(8),INTENT(IN)       :: Area(:)
   
    UrbLand%UrbData%Area = Area
    
  END SUBROUTINE SetAreas
  
  
  
  
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
  SUBROUTINE ReadRestartData(UrbanLand,InFile,iStat)
    CLASS(UrbanDatabaseType) :: UrbanLand
    TYPE(GenericFileType)    :: InFile
    INTEGER,INTENT(OUT)      :: iStat
    
    CALL InFile%ReadData(UrbanLand%UrbData%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%ReturnFlow,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Oth,iStat)  
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(UrbanLand,TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
    CLASS(UrbanDataBaseType)      :: UrbanLand
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(IN)            :: iElemIDs(AppGrid%NElements)
    REAL(8),INTENT(IN)            :: rElemAreas(AppGrid%NElements)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode,FileReadCode1
    
    !Initialize
    iStat = 0

    !Echo progress
    CALL EchoProgress('Reading time series data for urban lands')
    
    !Land use areas
    CALL UrbanLand%LandUseDataFile%ReadTSData('Urban areas',TimeStep,rElemAreas,iElemIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (UrbanLand%LandUseDataFile%lUpdated) UrbanLand%UrbData%Area = UrbanLand%LandUseDataFile%rValues(:,2)
    
    !Population
    CALL ReadTimeSeriesData(TimeStep,'Population data',UrbanLand%PopulationFile,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Water use
    CALL ReadTimeSeriesData(TimeStep,'Per capita water use data',UrbanLand%PerCapitaWaterUseFile,FileReadCode1,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode1 .EQ. 0) UrbanLand%PerCapitaWaterUseFile%rValues = UrbanLand%PerCapitaWaterUseFile%rValues * UrbanLand%PerCapitaWaterUseFactor
    
    !Water use specifications
    CALL ReadTimeSeriesData(TimeStep,'Urban water use specifications',UrbanLand%WaterUseSpecsFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
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
  ! --- PRINT RESTART DATA
  ! -------------------------------------------------------------
  SUBROUTINE PrintRestartData(UrbanLand,OutFile)
    CLASS(UrbanDatabaseType),INTENT(IN) :: UrbanLand
    TYPE(GenericFileType)               :: OutFile
    
    CALL OutFile%WriteData(UrbanLand%UrbData%Runoff)
    CALL OutFile%WriteData(UrbanLand%UrbData%ReturnFlow)
    CALL OutFile%WriteData(UrbanLand%UrbData%Area_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%Area)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Precip_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Precip)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_AW_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_AW)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Oth_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Oth)
    
  END SUBROUTINE PrintRestartData
  
  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MOISTURE ROUTING
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SIMULATE FLOW PROCESSES AT URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(UrbanLand,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,ElemSupply,ReuseFrac,ReturnFrac,ElemsToGW,SolverData,lLakeElem,iStat)
    CLASS(UrbanDatabaseType),TARGET        :: UrbanLand
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(ETType),INTENT(IN)                :: ETData
    TYPE(RootZoneSoil_v41_Type),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                     :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                     :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)        :: SolverData
    LOGICAL,INTENT(IN)                     :: lLakeElem(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'Simulate'
    !$ INTEGER                            :: iChunk
    INTEGER                               :: indxElem,iColETc(1),KunsatMethod,iElemID
    REAL(8)                               :: AchievedConv,Area,ETc(1),HydCond,TotalPorosity,Area_Indoors,Area_Outdoors,  &
                                             FieldCapacity,TotalPorosityUrban,FieldCapacityUrban,RootDepth,Lambda,       &
                                             AW_Outdoors,AW_Indoors,WiltingPoint,WiltingPointUrban,SoilM_P,SoilM,Supply, &
                                             GM,PrecipD,rMultip,Excess,Inflow,fRU,fRF,ratio(3),SoilM_P_Array(3),         &
                                             SoilM_Array(3),ETPartition(3),Infilt(3),ETc_effect,rIndoorFrac
    LOGICAL                               :: lNegativeMoist
    TYPE(UrbanType),POINTER               :: pUrban
    
    !Initialize
    iStat     = 0
    !$ iChunk = MAX(1 , AppGrid%NElements/(OMP_GET_MAX_THREADS()-1)/10)
  
    !Inform user
    CALL EchoProgress('Simulating flows at urban lands')
    
    !Initialize
    UrbanLand%UrbData%Runoff          = 0.0
    UrbanLand%UrbData%PrecipInfilt    = 0.0                     
    UrbanLand%UrbData%ETa             = 0.0                     
    UrbanLand%UrbData%Perc            = 0.0                    
    UrbanLand%UrbData%ReturnFlow      = 0.0
    UrbanLand%UrbData%IrigInfilt      = 0.0
    UrbanLand%UrbData%Reuse           = 0.0
    UrbanLand%UrbData%GMExcess        = 0.0
    UrbanLand%UrbData%ETFromGW_Actual = 0.0
    RootDepth                         = UrbanLand%RootDepth   
    
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,UrbanLand,lLakeElem,SoilsData,ETData,Precip,DeltaT,GenericMoisture, &
    !$OMP                                  ReturnFrac,ReuseFrac,SolverData,ElemSupply,ElemsToGW,iStat,RootDepth)       &
    !$OMP          NUM_THREADS(OMP_GET_MAX_THREADS()-1)
    !$OMP DO SCHEDULE(DYNAMIC,iChunk)
    DO indxElem=1,AppGrid%NElements
        IF (iStat .EQ. -1) CYCLE
        !Cycle if Area is zero
        Area = UrbanLand%UrbData(indxElem)%Area
        IF (Area .EQ. 0.0) CYCLE
          
        !Cycle if it is a lake element
        IF (lLakeElem(indxElem)) CYCLE
        
        !Pointer to urban land data
        pUrban => UrbanLand%UrbData(indxElem)
        
        !Initialize
        WiltingPoint       = SoilsData(indxElem)%WiltingPoint
        FieldCapacity      = SoilsData(indxElem)%FieldCapacity
        TotalPorosity      = SoilsData(indxElem)%TotalPorosity
        HydCond            = SoilsData(indxElem)%HydCond
        Lambda             = SoilsData(indxElem)%Lambda
        KunsatMethod       = SoilsData(indxElem)%KunsatMethod
        iColETc(1)         = pUrban%iColETc
        ETc                = ETData%GetValues(iColETc)
        PrecipD            = Precip(indxElem) * DeltaT
        GM                 = GenericMoisture(1,indxElem) * RootDepth * DeltaT
        Area_Outdoors      = Area * pUrban%PerviousFrac
        Area_Indoors       = Area - Area_Outdoors 
        WiltingPointUrban  = WiltingPoint  * RootDepth
        FieldCapacityUrban = FieldCapacity * RootDepth
        TotalPorosityUrban = TotalPorosity * RootDepth
        SoilM_P            = pUrban%SoilM_Precip_P + pUrban%SoilM_AW_P + pUrban%SoilM_Oth_P
        IF (Area_Indoors .EQ. 0.0) THEN
            AW_Indoors  = 0.0
            AW_Outdoors = ElemSupply(indxElem) / Area_Outdoors
        ELSEIF (Area_Outdoors .EQ. 0.0) THEN
            AW_Indoors  = ElemSupply(indxElem) / Area_Indoors
            AW_Outdoors = 0.0
        ELSE
            rIndoorFrac = UrbanLand%WaterUseSpecsFile%rValues(pUrban%iColWaterUseSpec)
            AW_Indoors  = ElemSupply(indxElem)*rIndoorFrac       / Area_Indoors
            AW_Outdoors = ElemSupply(indxElem)*(1d0-rIndoorFrac) / Area_Outdoors
        END IF
        
        !Infiltration and return flow due to applied water
        Supply            = AW_Outdoors * DeltaT
        fRF               = ReturnFrac(pUrban%iColReturnFrac)
        fRU               = ReuseFrac(pUrban%iColReuseFrac)
        pUrban%IrigInfilt = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
        pUrban%ReturnFlow = Supply - pUrban%IrigInfilt
        
        !Total inflow to the root zone
        Inflow = GM + pUrban%IrigInfilt
        
        !ET from GW
        pUrban%ETFromGW_Actual = MIN(ETc(1)*DeltaT , pUrban%ETFromGW_Max)
        ETc_effect             = ETc(1)*DeltaT - pUrban%ETFromGW_Actual
        
        !Simulate
        CALL NonPondedLUMoistureRouter(PrecipD                                 ,  &
                                       pUrban%SMax                             ,  &
                                       SoilM_P                                 ,  &
                                       ETc_effect                              ,  & 
                                       HydCond                                 ,  & 
                                       TotalPorosityUrban                      ,  & 
                                       FieldCapacityUrban                      ,  & 
                                       WiltingPointUrban                       ,  &
                                       Lambda                                  ,  & 
                                       Inflow                                  ,  &
                                       SolverData%Tolerance*TotalPorosityUrban ,  &
                                       KunsatMethod                            ,  &
                                       SolverData%IterMax                      ,  &
                                       SoilM                                   ,  & 
                                       pUrban%Runoff                           ,  & 
                                       pUrban%PrecipInfilt                     ,  & 
                                       pUrban%ETa                              ,  & 
                                       pUrban%Perc                             ,  & 
                                       Excess                                  ,  &
                                       AchievedConv                            ) 
        
        !Generate error if convergence is not achieved
        IF (AchievedConv .NE. 0.0) THEN
            !$OMP CRITICAL
            iElemID         = AppGrid%AppElement(indxElem)%ID
            MessageArray(1) = 'Convergence error in soil moisture routing for urban lands!'
            MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
            WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityUrban
            WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
            CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
            iStat = -1
            !$OMP END CRITICAL
            CYCLE
        END IF
        
        !Reduce inflows based on correction for total porosity
        IF (Excess .NE. 0.0) THEN
            ratio = [pUrban%PrecipInfilt , pUrban%IrigInfilt , GM]
            CALL NormalizeArray(ratio)
            pUrban%Runoff         = pUrban%Runoff     + Excess * ratio(1) 
            pUrban%ReturnFlow     = pUrban%ReturnFlow + Excess * ratio(2)
            pUrban%GMExcess       = Excess * ratio(3)
            pUrban%PrecipInfilt   = PrecipD - pUrban%Runoff
            pUrban%IrigInfilt     = Supply  - pUrban%ReturnFlow
        END IF
        
        !Compute re-use based on return flow
        IF (ReturnFrac(pUrban%iColReturnFrac) .GT. 0.0)   &
            pUrban%Reuse = pUrban%ReturnFlow * fRU / fRF
        
        !Compute moisture from precip and irrigation
        SoilM_P_Array = [pUrban%SoilM_Precip_P , pUrban%SoilM_AW_P , pUrban%SoilM_Oth_P]
        Infilt        = [pUrban%PrecipInfilt   , pUrban%IrigInfilt , GM - pUrban%GMExcess]
        CALL TrackMoistureDueToSource(SoilM_P_Array      ,  &
                                      Infilt             ,  &
                                      pUrban%Perc        ,  &
                                      pUrban%ETa         ,  &
                                      0d0                ,  &
                                      SoilM_Array        ,  &
                                      ETPartition        )
        pUrban%SoilM_Precip = SoilM_Array(1)
        pUrban%SoilM_AW     = SoilM_Array(2)
        pUrban%SoilM_Oth    = SoilM_Array(3)
        
        !Make sure soil moisture is not less than zero
        lNegativeMoist = .FALSE.
        IF (pUrban%SoilM_Precip .LT. 0.0) lNegativeMoist = .TRUE.
        IF (pUrban%SoilM_AW     .LT. 0.0) lNegativeMoist = .TRUE.
        IF (pUrban%SoilM_Oth    .LT. 0.0) lNegativeMoist = .TRUE.
        IF (lNegativeMoist) THEN
            !$OMP CRITICAL
            iElemID         = AppGrid%AppElement(indxElem)%ID
            MessageArray(1) = 'Soil moisture content becomes negative at element '//TRIM(IntToText(iElemID))//'.'
            MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
            MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
            MessageArray(4) = 'in the Root Zone Main Input File.'
            CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
            iStat = -1
            !$OMP END CRITICAL
            CYCLE
        END IF
        
        !Convert depths to volumetric rates
        rMultip                = Area_Outdoors / DeltaT
        pUrban%Runoff          = pUrban%Runoff          * rMultip + Precip(indxElem) * Area_Indoors
        pUrban%ReturnFlow      = pUrban%ReturnFlow      * rMultip + AW_Indoors       * Area_Indoors
        pUrban%PrecipInfilt    = pUrban%PrecipInfilt    * rMultip
        pUrban%IrigInfilt      = pUrban%IrigInfilt      * rMultip
        pUrban%Perc            = pUrban%Perc            * rMultip
        pUrban%Reuse           = pUrban%Reuse           * rMultip
        pUrban%ETFromGW_Actual = pUrban%ETFromGW_Actual * rMultip
        pUrban%ETa             = pUrban%ETa             * rMultip + pUrban%ETFromGW_Actual  !Includes ET from groundwater
        
        !If surface flow goes to groundwater, update the runoff processes
        IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
            pUrban%Perc         = pUrban%Perc + pUrban%Runoff + pUrban%ReturnFlow
            pUrban%PrecipInfilt = pUrban%PrecipInfilt + pUrban%Runoff        !Runoff and 
            pUrban%IrigInfilt   = pUrban%IrigInfilt + pUrban%ReturnFlow      ! return flow are assumed to bypass root zone for proper mass balance       
            pUrban%Runoff       = 0.0
            pUrban%ReturnFlow   = 0.0
        END IF
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    !Nullify pointer
    NULLIFY(pUrban)
    
  END SUBROUTINE Simulate
  
  
  
  
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
  ! --- COMPUTE GW INFLOW INTO ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE UrbanLandUse_ComputeETFromGW_Max(UrbanLand,DepthToGW,Sy,CapillaryRise)
    CLASS(UrbanDatabaseType) :: UrbanLand
    REAL(8),INTENT(IN)       :: DepthToGW(:),Sy(:),CapillaryRise(:)
    
    !Local variables
    REAL(8) :: RootDepth(1),Area(1,SIZE(Sy)),GWInflow(1,SIZE(Sy))
    
    !Initialize
    RootDepth(1) = UrbanLand%RootDepth
    Area(1,:)    = UrbanLand%UrbData%Area * UrbanLand%UrbData%PerviousFrac
    
    CALL ComputeETFromGW_Max(DepthToGW,Sy,RootDepth,CapillaryRise,Area,GWInflow)
    
    UrbanLand%UrbData%ETFromGW_Max = GWInflow(1,:)
    
  END SUBROUTINE UrbanLandUse_ComputeETFromGW_Max
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT SOIL INITIAL MOISTURE CONTENTS TO DEPTHS
  ! ---  Note: Called only once at the beginning of simulation
  ! -------------------------------------------------------------
  SUBROUTINE SoilMContent_To_Depth(UrbanLand,NElements,iElemIDs,TotalPorosity,iStat)
    CLASS(UrbanDatabaseType) :: UrbanLand
    INTEGER,INTENT(IN)       :: NElements,iElemIDs(NElements)
    REAL(8),INTENT(IN)       :: TotalPorosity(:)
    INTEGER,INTENT(OUT)      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxElem
    REAL(8)                      :: RootDepth
    
    !Initialize
    iStat = 0
    
    !Return if urban lands are not simulated
    IF (SIZE(UrbanLand%UrbData) .EQ. 0) RETURN
    
    !Initialize
    RootDepth = UrbanLand%RootDepth
    
    !Check if initial conditions are greater than total porosity, if not convert conetnts to depths and equate SoilM_P to SoilM
    DO indxElem=1,NElements
      ASSOCIATE (pUrbData => UrbanLand%UrbData(indxElem)) 
                 
        IF ((pUrbData%SoilM_Precip + pUrbData%SoilM_AW + pUrbData%SoilM_Oth) .GT. TotalPorosity(indxElem)) THEN
            CALL SetLastMessage('Initial moisture content for urban land at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        pUrbData%SoilM_Precip   = pUrbData%SoilM_Precip * RootDepth
        pUrbData%SoilM_AW       = pUrbData%SoilM_AW * RootDepth
        pUrbData%SoilM_Oth      = pUrbData%SoilM_Oth * RootDepth
        pUrbData%SoilM_Precip_P = pUrbData%SoilM_Precip
        pUrbData%SoilM_AW_P     = pUrbData%SoilM_AW
        pUrbData%SoilM_Oth_P    = pUrbData%SoilM_Oth
   
      END ASSOCIATE
    END DO 
    
  END SUBROUTINE SoilMContent_To_Depth
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(UrbanLand) 
    CLASS(UrbanDatabaseType) :: UrbanLand
    
    UrbanLand%UrbData%Area_P = UrbanLand%UrbData%Area
    
  END SUBROUTINE AdvanceAreas

  
  ! -------------------------------------------------------------
  ! --- COMPUTE URBAN WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(UrbanLand)
    CLASS(UrbanDatabaseType) :: UrbanLand

    ASSOCIATE (pUrbData => UrbanLand%UrbData)
        !Element demand is calculated based on urban area fractions
        IF (UrbanLand%lFracDemand_wrt_Area) THEN
            IF (UrbanLand%PopulationFile%lUpdated  .OR.  UrbanLand%PerCapitaWaterUseFile%lUpdated) THEN
                !Update element demand fractions if needed
                IF (UrbanLand%LandUseDataFile%lUpdated) THEN
                    !Set FracDemand to urban area in each element
                    pUrbData%FracDemand = pUrbData%Area    
                    !Normalize FracDemand
                    CALL CalculateUrbanFracDemand(SIZE(pUrbData%iColPopulation),pUrbData%iColPopulation,pUrbData%iColPerCapitaWaterUse,pUrbData%FracDemand)
                END IF
                
                !Calculate demand
                pUrbData%Demand = UrbanLand%PopulationFile%iValues(pUrbData%iColPopulation) * UrbanLand%PerCapitaWaterUseFile%rValues(pUrbData%iColPerCapitaWaterUse)* pUrbData%FracDemand                
            END IF
            
        !Element demand is calculated based on user-specified static fractions
        ELSE
            IF (UrbanLand%PopulationFile%lUpdated  .OR.  UrbanLand%PerCapitaWaterUseFile%lUpdated) THEN
                pUrbData%Demand = UrbanLand%PopulationFile%iValues(pUrbData%iColPopulation) * UrbanLand%PerCapitaWaterUseFile%rValues(pUrbData%iColPerCapitaWaterUse)* pUrbData%FracDemand
            END IF
            
        END IF
    END ASSOCIATE
       
  END SUBROUTINE ComputeWaterDemand

  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(UrbanLand,iElemIDs,rElemAreas,TimeStep,iStat)
    CLASS(UrbanDatabaseType)      :: UrbanLand
    INTEGER,INTENT(IN)            :: iElemIDs(:)
    REAL(8),INTENT(IN)            :: rElemAreas(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep 
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    CALL UrbanLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                   ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%LandUseDataFile%ReadTSData('Urban areas',TimeStep,rElemAreas,iElemIDs,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL UrbanLand%PopulationFile%File%RewindFile_To_BeginningOfTSData(iStat)                         ;  IF (iStat .NE. 0) RETURN
    CALL ReadTimeSeriesData(TimeStep,'Population data',UrbanLand%PopulationFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL UrbanLand%PerCapitaWaterUseFile%File%RewindFile_To_BeginningOfTSData(iStat)                                   ;  IF (iStat .NE. 0) RETURN
    CALL ReadTimeSeriesData(TimeStep,'Per capita water use data',UrbanLand%PerCapitaWaterUseFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iFileReadCode .EQ. 0) UrbanLand%PerCapitaWaterUseFile%rValues = UrbanLand%PerCapitaWaterUseFile%rValues * UrbanLand%PerCapitaWaterUseFactor

    CALL UrbanLand%WaterUseSpecsFile%File%RewindFile_To_BeginningOfTSData(iStat)                                       ;  IF (iStat .NE. 0) RETURN
    CALL ReadTimeSeriesData(TimeStep,'Urban water use specifications',UrbanLand%WaterUseSpecsFile,iFileReadCode,iStat)
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp

END MODULE