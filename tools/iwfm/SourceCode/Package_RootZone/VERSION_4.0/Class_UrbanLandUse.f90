!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2022  
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
MODULE Class_UrbanLandUse
  !$ USE OMP_LIB
  USE MessageLogger           , ONLY: SetLastMessage                  , &
                                      MessageArray                    , &
                                      EchoProgress                    , &
                                      f_iFatal
  USE IOInterface             , ONLY: GenericFileType                 , &
                                      RealTSDataInFileType            , &
                                      IntTSDataInFileType  
  USE TimeSeriesUtilities     , ONLY: TimeStepType                    , &
                                      IncrementTimeStamp              , &
                                      CTimeStep_To_RTimeStep          , &
                                      OPERATOR(.TSGT.) 
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter         , &
                                      IntToText                       , &
                                      CleanSpecialCharacters          , &
                                      NormalizeArray                  , &
                                      LocateInList                    , &
                                      EstablishAbsolutePathFileName   , &
                                      UpperCase
  USE Package_Misc            , ONLY: SolverDataType                                       
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource        , &
                                      CalculateUrbanFracDemand
  USE Class_GenericLandUse    , ONLY: GenericLandUseType
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType
  USE Package_Discretization  , ONLY: AppGridType
  USE Package_PrecipitationET , ONLY: ETType
  USE Package_UnsatZone       , ONLY: RootZoneSoilType                , &
                                      NonPondedLUMoistureRouter               
  USE Util_Package_RootZone   , ONLY: ReadRealData                   
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
  TYPE,EXTENDS(GenericLandUseType) :: UrbanType
    INTEGER,ALLOCATABLE :: iColReturnFrac(:,:)           !Column number in the return flow fraction data file
    INTEGER,ALLOCATABLE :: iColReuseFrac(:,:)            !Column number in the re-use fraction data file
    INTEGER,ALLOCATABLE :: iColPopulation(:,:)           !Column number in the population data file
    INTEGER,ALLOCATABLE :: iColPerCapitaWaterUse(:,:)    !Column number in the per capita water use data file
    INTEGER,ALLOCATABLE :: iColWaterUseSpec(:,:)         !Column number in the urban water use specs data file
    REAL(8),ALLOCATABLE :: FracDemand(:,:)               !Relative proportion of demand computed by multiplying popultaion with per capita water use to be applied to an element
    REAL(8),ALLOCATABLE :: Demand(:,:)                   !Urban water demand
    REAL(8),ALLOCATABLE :: PerviousFrac(:,:)             !Fraction of pervious area to total urban area
    REAL(8),ALLOCATABLE :: IrigInfilt(:,:)               !Infiltration due to irrigation
    REAL(8),ALLOCATABLE :: Reuse(:,:)                    !Reused return flow 
    REAL(8),ALLOCATABLE :: ReturnFlow(:,:)               !Return flow
    REAL(8),ALLOCATABLE :: ElemDemandFrac(:,:)           !Ratio of urban demand to the total demand at the element it is located at
  END TYPE UrbanType
  
  
  ! -------------------------------------------------------------
  ! --- URBAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE UrbanDatabaseType
    TYPE(UrbanType)             :: UrbData                         !Urban data for each element
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
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadElemArea
    PROCEDURE,PASS :: ReadTSData                  
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: AdvanceAreas                
    PROCEDURE,PASS :: SoilMContent_To_Depth       
    PROCEDURE,PASS :: Simulate                    
    PROCEDURE,PASS :: ComputeWaterDemand          
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp          
  END TYPE UrbanDatabaseType

  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_UrbanLandUse::'

  


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
    CALL UrbLand%UrbData%New(NElements,1,iStat) 
    ALLOCATE (UrbLand%UrbData%iColReturnFrac(NElements,1)           , &
              UrbLand%UrbData%iColReuseFrac(NElements,1)            , &
              UrbLand%UrbData%iColPopulation(NElements,1)           , &
              UrbLand%UrbData%iColPerCapitaWaterUse(NElements,1)    , &
              UrbLand%UrbData%iColWaterUseSpec(NElements,1)         , &
              UrbLand%UrbData%FracDemand(NElements,1)               , &
              UrbLand%UrbData%Demand(NElements,1)                   , &
              UrbLand%UrbData%PerviousFrac(NElements,1)             , &
              UrbLand%UrbData%IrigInfilt(NElements,1)               , &
              UrbLand%UrbData%Reuse(NElements,1)                    , &
              UrbLand%UrbData%ReturnFlow(NElements,1)               , &
              UrbLand%UrbData%ElemDemandFrac(NElements,1)           , &
              UrbLand%RegionETPot(NSubregions)                      , &
              STAT=ErrorCode                                        )
    IF (ErrorCode+iStat .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for urban data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize variables
    UrbLand%UrbData%iColReturnFrac        = 0
    UrbLand%UrbData%iColReuseFrac         = 0
    UrbLand%UrbData%iColPopulation        = 0
    UrbLand%UrbData%iColPerCapitaWaterUse = 0
    UrbLand%UrbData%iColWaterUseSpec      = 0
    UrbLand%UrbData%FracDemand            = 1.0
    UrbLand%UrbData%Demand                = 0.0
    UrbLand%UrbData%PerviousFrac          = 0.0
    UrbLand%UrbData%IrigInfilt            = 0.0
    UrbLand%UrbData%Reuse                 = 0.0
    UrbLand%UrbData%ReturnFlow            = 0.0
    UrbLand%UrbData%ElemDemandFrac        = 0.0
    UrbLand%RegionETPot                   = 0.0
    
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
            
            pUrbData%PerviousFrac(iElem,1)          =     DummyArray(indxElem,2)
            pUrbData%SMax(iElem,1)                  = (1000.0/DummyArray(indxElem,3)-10.0) * FACTCN
            pUrbData%iColPopulation(iElem,1)        = INT(DummyArray(indxElem,4))
            pUrbData%iColPerCapitaWaterUse(iElem,1) = INT(DummyArray(indxElem,5))
            pUrbData%FracDemand(iElem,1)            =     DummyArray(indxElem,6)
            pUrbData%iColETc(iElem,1)               = INT(DummyArray(indxElem,7))
            pUrbData%iColReturnFrac(iElem,1)        = INT(DummyArray(indxElem,8))
            pUrbData%iColReuseFrac(iElem,1)         = INT(DummyArray(indxElem,9))
            pUrbData%iColWaterUseSpec(iElem,1)      = INT(DummyArray(indxElem,10))
        END DO
      
        !Make sure FRACDM is either entered all negative or all greater than (or equal to) zero
        lError = .FALSE.
        IF (pUrbData%FracDemand(1,1) .LT. 0.0) THEN
            UrbLand%lFracDemand_wrt_Area = .TRUE.
            IF (ANY(pUrbData%FracDemand(2:,1).GE.0.0)) lError = .TRUE.
        ELSE
            UrbLand%lFracDemand_wrt_Area = .FALSE.
            IF (ANY(pUrbData%FracDemand(2:,1).LT.0.0)) lError = .TRUE.
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
            CALL UrbLand%PopulationFile%CheckColNum('Urban population file as referenced by element '//TRIM(IntToText(ID)),[pUrbData%iColPopulation(indxElem,1)],.TRUE.,iStat)                          ;  IF (iStat .EQ. -1) RETURN
            CALL UrbLand%PerCapitaWaterUseFile%CheckColNum('Urban per capita water use file as referenced by element '//TRIM(IntToText(ID)),[pUrbData%iColPerCapitaWaterUse(indxElem,1)],.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL UrbLand%WaterUseSpecsFile%CheckColNum('Urban water use specifications file as referenced by element '//TRIM(IntToText(ID)),[pUrbData%iColWaterUseSpec(indxElem,1)],.TRUE.,iStat)       ;  IF (iStat .EQ. -1) RETURN
        END DO

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
            lProcessed(iElem)                             = .TRUE.
            pUrbData%SoilM_Precip(iElem,1)                = DummyArray(indxElem,2) * DummyArray(indxElem,3)
            pUrbData%SoilM_AW(iElem,1)                    = DummyArray(indxElem,3) - pUrbData%SoilM_Precip(iElem,1)
            pUrbData%SoilM_Precip_P_BeforeUpdate(iElem,1) = pUrbData%SoilM_Precip(iElem,1) 
            pUrbData%SoilM_Precip_P(iElem,1)              = pUrbData%SoilM_Precip(iElem,1) 
            pUrbData%SoilM_AW_P_BeforeUpdate(iElem,1)     = pUrbData%SoilM_AW(iElem,1) 
            pUrbData%SoilM_AW_P(iElem,1)                  = pUrbData%SoilM_AW(iElem,1) 
        END DO
    END ASSOCIATE
    
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
    CALL UrbLand%UrbData%Kill()
    DEALLOCATE (UrbLand%UrbData%iColReturnFrac            , &
                UrbLand%UrbData%iColReuseFrac             , &
                UrbLand%UrbData%iColPopulation            , &
                UrbLand%UrbData%iColPerCapitaWaterUse     , &
                UrbLand%UrbData%iColWaterUseSpec          , &
                UrbLand%UrbData%FracDemand                , &
                UrbLand%UrbData%Demand                    , &
                UrbLand%UrbData%PerviousFrac              , &
                UrbLand%UrbData%IrigInfilt                , &
                UrbLand%UrbData%Reuse                     , &
                UrbLand%UrbData%ReturnFlow                , &
                UrbLand%UrbData%ElemDemandFrac            , &
                UrbLand%RegionETPot                       , & 
                STAT = ErrorCode                          )
    
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
        CALL ReturnFracFile%ReadTSData(TimeStep,'Return flow fractions data',FileReadCode_Return,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ReuseFracFile%ReadTSData(TimeStep,'Reuse fractions data',FileReadCode_Reuse,iStat)          ;  IF (iStat .EQ. -1) RETURN
        
        !If new data is read, find min and max
        IF (FileReadCode_Return.EQ.0  .OR.  FileReadCode_Reuse.EQ.0) THEN
            DO indxElem=1,SIZE(UrbLand%UrbData%SMax , DIM=1)
                rRT      = ReturnFracFile%rValues(UrbLand%UrbData%iColReturnFrac(indxElem,1))
                rRU      = ReuseFracFile%rValues(UrbLand%UrbData%iColReuseFrac(indxElem,1))
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
   
    UrbLand%UrbData%Area(:,1) = Area
    
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
    IF (UrbanLand%LandUseDataFile%lUpdated) UrbanLand%UrbData%Area(:,1) = UrbanLand%LandUseDataFile%rValues(:,2)
    
    !Population
    CALL UrbanLand%PopulationFile%ReadTSData(TimeStep,'Population data',FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Water use
    CALL UrbanLand%PerCapitaWaterUseFile%ReadTSData(TimeStep,'Per capita water use data',FileReadCode1,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode1 .EQ. 0) UrbanLand%PerCapitaWaterUseFile%rValues = UrbanLand%PerCapitaWaterUseFile%rValues * UrbanLand%PerCapitaWaterUseFactor
    
    !Water use specifications
    CALL UrbanLand%WaterUseSpecsFile%ReadTSData(TimeStep,'Urban water use specifications',FileReadCode,iStat)
    
  END SUBROUTINE ReadTSData
  

  ! -------------------------------------------------------------
  ! --- READ URBAN LAND AREA AT AN ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE ReadElemArea(UrbanLand,iElem,lForInquiry,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,iStat)
    CLASS(UrbanDataBaseType)    :: UrbanLand
    INTEGER,INTENT(IN)          :: iElem
    LOGICAL,INTENT(IN)          :: lForInquiry
    CHARACTER(LEN=*),INTENT(IN) :: cReadBeginDateAndTime,cReadEndDateAndTime
    INTEGER,INTENT(OUT)         :: nActualOutput
    REAL(8),INTENT(OUT)         :: ElemLandUse(:),rOutputDates(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER :: ErrorCode
    
    !ReadData
    CALL UrbanLand%LandUseDataFile%ReadTSData(iElem,2,iElem,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,ErrorCode,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Unit conversion
    ElemLandUse(1:nActualOutput) = ElemLandUse(1:nActualOutput) * UrbanLand%LandUseDataFile%Fact
    
    !Rewind land use file if it was opened for querying
    IF (lForInquiry) CALL UrbanLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)     
        
  END SUBROUTINE ReadElemArea


  
  
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
    CLASS(UrbanDatabaseType)          :: UrbanLand
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(ETType),INTENT(IN)           :: ETData
    TYPE(RootZoneSoilType),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    LOGICAL,INTENT(IN)                :: lLakeElem(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'Simulate'
    INTEGER                               :: indxElem,iColETc(1),KunsatMethod,iElemID
    REAL(8)                               :: AchievedConv,Area,ETc(1),HydCond,TotalPorosity,Area_Indoors,Area_Outdoors, &
                                             FieldCapacity,TotalPorosityUrban,FieldCapacityUrban,RootDepth,Lambda,      &
                                             AW_Outdoors,AW_Indoors,WiltingPoint,WiltingPointUrban,SoilM_P,SoilM,       &
                                             GM,PrecipD,rMultip,Excess,Inflow,Supply,fRU,fRF,ratio(3),SoilM_P_Array(3), &
                                             SoilM_Array(3),Infilt(3),ETPartition(3),rIndoorFrac
    
    !Initialize
    iStat = 0
  
    !Inform user
    CALL EchoProgress('Simulating flows at urban lands')
    
    ASSOCIATE (pUrban => UrbanLand%UrbData)
        !Initialize
        pUrban%Runoff          = 0.0
        pUrban%PrecipInfilt    = 0.0                     
        pUrban%ETa             = 0.0                     
        pUrban%Perc            = 0.0                    
        pUrban%ReturnFlow      = 0.0
        pUrban%IrigInfilt      = 0.0
        pUrban%GMExcess        = 0.0
        pUrban%Reuse           = 0.0
        RootDepth              = UrbanLand%RootDepth   
        
        !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,UrbanLand,lLakeElem,SoilsData,ETData,Precip,DeltaT,GenericMoisture, &
        !$OMP                                  pUrban,ReturnFrac,ReuseFrac,SolverData,ElemSupply,ElemsToGW,iStat,RootDepth)       
        !$OMP DO SCHEDULE(NONMONOTONIC:DYNAMIC,96)
        DO indxElem=1,AppGrid%NElements
            
            !Cycle if Area is zero
            Area = pUrban%Area(indxElem,1)
            IF (Area .EQ. 0.0) CYCLE
              
            !Cycle is it is a lake element
            IF (lLakeElem(indxElem)) CYCLE
            
            !Initialize
            WiltingPoint       = SoilsData(indxElem)%WiltingPoint
            FieldCapacity      = SoilsData(indxElem)%FieldCapacity
            TotalPorosity      = SoilsData(indxElem)%TotalPorosity
            HydCond            = SoilsData(indxElem)%HydCond
            Lambda             = SoilsData(indxElem)%Lambda
            KunsatMethod       = SoilsData(indxElem)%KunsatMethod
            iColETc(1)         = pUrban%iColETc(indxElem,1)
            ETc                = ETData%GetValues(iColETc)
            PrecipD            = Precip(indxElem) * DeltaT
            GM                 = GenericMoisture(1,indxElem) * RootDepth * DeltaT
            Area_Outdoors      = Area * pUrban%PerviousFrac(indxElem,1)
            Area_Indoors       = Area - Area_Outdoors 
            WiltingPointUrban  = WiltingPoint  * RootDepth
            FieldCapacityUrban = FieldCapacity * RootDepth
            TotalPorosityUrban = TotalPorosity * RootDepth
            SoilM_P            = pUrban%SoilM_Precip_P(indxElem,1) + pUrban%SoilM_AW_P(indxElem,1) + pUrban%SoilM_Oth_P(indxElem,1)
            IF (Area_Indoors .EQ. 0.0) THEN
                AW_Indoors  = 0.0
                AW_Outdoors = ElemSupply(indxElem) / Area_Outdoors
            ELSEIF (Area_Outdoors .EQ. 0.0) THEN
                AW_Indoors  = ElemSupply(indxElem) / Area_Indoors
                AW_Outdoors = 0.0
            ELSE
                rIndoorFrac = UrbanLand%WaterUseSpecsFile%rValues(pUrban%iColWaterUseSpec(indxElem,1))
                AW_Indoors  = ElemSupply(indxElem)*rIndoorFrac        / Area_Indoors
                AW_Outdoors = ElemSupply(indxElem)*(1d0-rIndoorFrac ) / Area_Outdoors
            END IF
            
            !Infiltration and return flow due to applied water
            Supply                        = AW_Outdoors * DeltaT
            fRF                           = ReturnFrac(pUrban%iColReturnFrac(indxElem,1))
            fRU                           = ReuseFrac(pUrban%iColReuseFrac(indxElem,1))
            pUrban%IrigInfilt(indxElem,1) = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
            pUrban%ReturnFlow(indxElem,1) = Supply - pUrban%IrigInfilt(indxElem,1)
              
            !Total inflow to the root zone
            Inflow = GM + pUrban%IrigInfilt(indxElem,1)
              
            !Simulate
            CALL NonPondedLUMoistureRouter(PrecipD                                 ,  &
                                           pUrban%SMax(indxElem,1)                 ,  &
                                           SoilM_P                                 ,  &
                                           ETc(1)*DeltaT                           ,  & 
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
                                           pUrban%Runoff(indxElem,1)               ,  & 
                                           pUrban%PrecipInfilt(indxElem,1)         ,  & 
                                           pUrban%ETa(indxElem,1)                  ,  & 
                                           pUrban%Perc(indxElem,1)                 ,  & 
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
                ratio = [pUrban%PrecipInfilt(indxElem,1) , pUrban%IrigInfilt(indxElem,1) , GM]
                CALL NormalizeArray(ratio)
                pUrban%Runoff(indxElem,1)       = pUrban%Runoff(indxElem,1)     + Excess * ratio(1) 
                pUrban%ReturnFlow(indxElem,1)   = pUrban%ReturnFlow(indxElem,1) + Excess * ratio(2)
                pUrban%GMExcess(indxElem,1)     = Excess * ratio(3)
                pUrban%PrecipInfilt(indxElem,1) = PrecipD - pUrban%Runoff(indxElem,1)
                pUrban%IrigInfilt(indxElem,1)   = Supply  - pUrban%ReturnFlow(indxElem,1)
            END IF
            
            !Compute re-use based on return flow
            IF (fRF .GT. 0.0) pUrban%Reuse(indxElem,1) = pUrban%ReturnFlow(indxElem,1) * fRU / fRF
              
            !Compute moisture from precip and irrigation
            SoilM_P_Array = [pUrban%SoilM_Precip_P(indxElem,1) , pUrban%SoilM_AW_P(indxElem,1) , pUrban%SoilM_Oth_P(indxElem,1)  ]
            Infilt        = [pUrban%PrecipInfilt(indxElem,1)   , pUrban%IrigInfilt(indxElem,1) , GM - pUrban%GMExcess(indxElem,1)]
            CALL TrackMoistureDueToSource(SoilM_P_Array           ,  &
                                          Infilt                  ,  &
                                          pUrban%Perc(indxElem,1) ,  &
                                          pUrban%ETa(indxElem,1)  ,  &
                                          0d0                     ,  &
                                          SoilM_Array             ,  &
                                          ETPartition             )
            pUrban%SoilM_Precip(indxElem,1) = SoilM_Array(1)
            pUrban%SoilM_AW(indxElem,1)     = SoilM_Array(2)
            pUrban%SoilM_Oth(indxElem,1)    = SoilM_Array(3)
        
            !Make sure soil moisture is not less than zero
            IF (ANY(SoilM_Array.LT.0.0)) THEN
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
            rMultip                         = Area_Outdoors / DeltaT
            pUrban%Runoff(indxElem,1)       = pUrban%Runoff(indxElem,1)       * rMultip + Precip(indxElem) * Area_Indoors
            pUrban%ReturnFlow(indxElem,1)   = pUrban%ReturnFlow(indxElem,1)   * rMultip + AW_Indoors       * Area_Indoors
            pUrban%PrecipInfilt(indxElem,1) = pUrban%PrecipInfilt(indxElem,1) * rMultip
            pUrban%IrigInfilt(indxElem,1)   = pUrban%IrigInfilt(indxElem,1)   * rMultip
            pUrban%ETa(indxElem,1)          = pUrban%ETa(indxElem,1)          * rMultip
            pUrban%Perc(indxElem,1)         = pUrban%Perc(indxElem,1)         * rMultip
            pUrban%Reuse(indxElem,1)        = pUrban%Reuse(indxElem,1)        * rMultip
              
            !If surface flow goes to groundwater, update the runoff processes
            IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
                pUrban%Perc(indxElem,1)         = pUrban%Perc(indxElem,1) + pUrban%Runoff(indxElem,1) + pUrban%ReturnFlow(indxElem,1)
                pUrban%PrecipInfilt(indxElem,1) = pUrban%PrecipInfilt(indxElem,1) + pUrban%Runoff(indxElem,1)        !Runoff and 
                pUrban%IrigInfilt(indxElem,1)   = pUrban%IrigInfilt(indxElem,1) + pUrban%ReturnFlow(indxElem,1)      ! return flow are assumed to bypass root zone for proper mass balance       
                pUrban%Runoff(indxElem,1)       = 0.0
                pUrban%ReturnFlow(indxElem,1)   = 0.0
            END IF
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
    END ASSOCIATE
    
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
    IF (.NOT.ALLOCATED(UrbanLand%UrbData%SMax) .EQ. 0) RETURN
    
    !Initialize
    RootDepth = UrbanLand%RootDepth
    
    !Check if initial conditions are greater than total porosity, if not convert conetnts to depths and equate SoilM_P to SoilM
    ASSOCIATE (pUrbData => UrbanLand%UrbData) 
        DO indxElem=1,NElements
            IF ((pUrbData%SoilM_Precip(indxElem,1) + pUrbData%SoilM_AW(indxElem,1) + pUrbData%SoilM_Oth(indxElem,1)) .GT. TotalPorosity(indxElem)) THEN
                CALL SetLastMessage('Initial moisture content for urban land at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            pUrbData%SoilM_Precip(indxElem,1)   = pUrbData%SoilM_Precip(indxElem,1) * RootDepth
            pUrbData%SoilM_AW(indxElem,1)       = pUrbData%SoilM_AW(indxElem,1) * RootDepth
            pUrbData%SoilM_Oth(indxElem,1)      = pUrbData%SoilM_Oth(indxElem,1) * RootDepth
            pUrbData%SoilM_Precip_P(indxElem,1) = pUrbData%SoilM_Precip(indxElem,1)
            pUrbData%SoilM_AW_P(indxElem,1)     = pUrbData%SoilM_AW(indxElem,1)
            pUrbData%SoilM_Oth_P(indxElem,1)    = pUrbData%SoilM_Oth(indxElem,1)
        END DO 
    END ASSOCIATE
    
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
                pUrbData%Demand(:,1) = UrbanLand%PopulationFile%iValues(pUrbData%iColPopulation(:,1)) * UrbanLand%PerCapitaWaterUseFile%rValues(pUrbData%iColPerCapitaWaterUse(:,1))* pUrbData%FracDemand(:,1)                
            END IF
            
        !Element demand is calculated based on user-specified static fractions
        ELSE
            IF (UrbanLand%PopulationFile%lUpdated  .OR.  UrbanLand%PerCapitaWaterUseFile%lUpdated) THEN
                pUrbData%Demand(:,1) = UrbanLand%PopulationFile%iValues(pUrbData%iColPopulation(:,1)) * UrbanLand%PerCapitaWaterUseFile%rValues(pUrbData%iColPerCapitaWaterUse(:,1))* pUrbData%FracDemand(:,1)
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
    CALL UrbanLand%PopulationFile%ReadTSData(TimeStep,'Population data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL UrbanLand%PerCapitaWaterUseFile%File%RewindFile_To_BeginningOfTSData(iStat)                                   ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%PerCapitaWaterUseFile%ReadTSData(TimeStep,'Per capita water use data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iFileReadCode .EQ. 0) UrbanLand%PerCapitaWaterUseFile%rValues = UrbanLand%PerCapitaWaterUseFile%rValues * UrbanLand%PerCapitaWaterUseFactor

    CALL UrbanLand%WaterUseSpecsFile%File%RewindFile_To_BeginningOfTSData(iStat)                                       ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%WaterUseSpecsFile%ReadTSData(TimeStep,'Urban water use specifications',iFileReadCode,iStat)
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp

END MODULE