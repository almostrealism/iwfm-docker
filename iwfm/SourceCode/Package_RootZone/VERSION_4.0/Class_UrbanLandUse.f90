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
MODULE Class_UrbanLandUse
  USE MessageLogger           , ONLY: SetLastMessage                  , &
                                      MessageArray                    , &
                                      EchoProgress                    , &
                                      iFatal
  USE IOInterface             , ONLY: GenericFileType                 
  USE TimeSeriesUtilities     , ONLY: TimeStepType                    , &
                                      IncrementTimeStamp              , &
                                      CTimeStep_To_RTimeStep
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter         , &
                                      IntToText                       , &
                                      CleanSpecialCharacters          , &
                                      NormalizeArray                  , &
                                      LocateInList                    , &
                                      EstablishAbsolutePathFileName   , &
                                      UpperCase
  USE Package_Misc            , ONLY: RealTSDataInFileType            , &
                                      IntTSDataInFileType             , &
                                      ReadTSData                      , &
                                      FlowDest_GWElement
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource        , &
                                      CalculateUrbanFracDemand
  USE Class_GenericLandUse
  USE Class_LandUseDataFile
  USE Class_SolverData
  USE Class_AppGrid
  USE Package_PrecipitationET , ONLY: ETType
  USE Util_RootZone_v40
  USE Package_UnsatZone
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
  PUBLIC :: UrbanDatabaseType                        ,  &
            UrbanLandUse_New                         ,  &
            UrbanLandUse_Kill                        ,  &
            UrbanLandUse_SetAreas                    ,  &
            UrbanLandUse_AdvanceAreas                ,  &
            UrbanLandUse_SoilMContent_To_Depth       ,  &
            UrbanLandUse_ReadTSData                  ,  &
            UrbanLandUse_Simulate                    ,  &
            UrbanLandUse_ComputeWaterDemand          
  
  
  ! -------------------------------------------------------------
  ! --- URBAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: UrbanType
    INTEGER :: iColReturnFrac       = 0       !Column number in the return flow fraction data file
    INTEGER :: iColReuseFrac        = 0       !Column number in the re-use fraction data file
    INTEGER :: iColPopulation       = 0       !Column number in the population data file
    INTEGER :: iColPerCapitaWaterUse= 0       !Column number in the per capita water use data file
    INTEGER :: iColWaterUseSpec     = 0       !Column number in the urban water use specs data file
    REAL(8) :: FracDemand           = 1.0     !Relative proportion of demand computed by multiplying popultaion with per capita water use to be applied to an element
    REAL(8) :: Demand               = 0.0     !Urban water demand
    REAL(8) :: PerviousFrac         = 0.0     !Fraction of pervious area to total urban area
    REAL(8) :: IrigInfilt           = 0.0     !Infiltration due to irrigation
    REAL(8) :: Reuse                = 0.0     !Reused return flow 
    REAL(8) :: ReturnFlow           = 0.0     !Return flow
    REAL(8) :: ElemDemandFrac       = 0.0     !Ratio of urban demand to the total demand at the element it is located at
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
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadElemArea
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
  SUBROUTINE UrbanLandUse_New(cFileName,cWorkingDirectory,FactCN,NElements,NSubregions,TrackTime,UrbLand,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)          :: FACTCN
    INTEGER,INTENT(IN)          :: NElements,NSubregions
    LOGICAL,INTENT(IN)          :: TrackTime
    TYPE(UrbanDatabaseType)     :: UrbLand
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'UrbanLandUse_New'
    CHARACTER                    :: ALine*1000
    INTEGER                      :: ErrorCode,indxElem
    REAL(8)                      :: FACT,Factor(1)
    LOGICAL                      :: lError
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
    ALLOCATE (UrbLand%UrbData(NElements)       , &
              UrbLand%RegionETPot(NSubregions) , &
              STAT=ErrorCode                   )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for urban data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Land use data file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%LandUseDataFile%New(cAbsPathFileName,'Urban area file',NElements,1,TrackTime,iStat)
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
    CALL UrbLand%PopulationFile%Init(cAbsPathFileName,'Population data file',TrackTime,1,iStat)
    IF (iStat .EQ. -1) RETURN
      
    !Urban per capita water use file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%PerCapitawaterUseFile%Init(cAbsPathFileName,'Urban per capita water use data file',TrackTime,1,.TRUE.,Factor,(/.TRUE./),iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    UrbLand%PerCapitawaterUseFactor = Factor(1)
      
    !Urban water use specifications data file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%WaterUseSpecsFile%Init(cAbsPathFileName,'Urban water use specifications',TrackTime,1,.FALSE.,Factor,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Read other data
    CALL ReadRealData(UrbanDataFile,NElements,10,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ASSOCIATE (pUrbData  => UrbLand%UrbData  )
      pUrbData%PerviousFrac          =     DummyArray(:,2)
      pUrbData%SMax                  = (1000.0/DummyArray(:,3)-10.0) * FACTCN
      pUrbData%iColPopulation        = INT(DummyArray(:,4))
      pUrbData%iColPerCapitaWaterUse = INT(DummyArray(:,5))
      pUrbData%FracDemand            =     DummyArray(:,6)
      pUrbData%iColETc               = INT(DummyArray(:,7))
      pUrbData%iColReturnFrac        = INT(DummyArray(:,8))
      pUrbData%iColReuseFrac         = INT(DummyArray(:,9))
      pUrbData%iColWaterUseSpec      = INT(DummyArray(:,10))
      
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
          CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
                
      !Normalize the relative fractions to distribute the demands, if fractions are specified by the user as constant
      IF (.NOT. UrbLand%lFracDemand_wrt_Area)  &
          CALL CalculateUrbanFracDemand(NElements,pUrbData%iColPopulation,pUrbData%iColPerCapitaWaterUse,pUrbData%FracDemand)

      !Check for timeseries column pointer errors
      DO indxElem=1,NElements
          CALL UrbLand%PopulationFile%CheckColNum('Urban population file as referenced by element '//TRIM(IntToText(indxElem)),[pUrbData(indxElem)%iColPopulation],.TRUE.,iStat)                          ;  IF (iStat .EQ. -1) RETURN
          CALL UrbLand%PerCapitaWaterUseFile%CheckColNum('Urban per capita water use file as referenced by element '//TRIM(IntToText(indxElem)),[pUrbData(indxElem)%iColPerCapitaWaterUse],.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
          CALL UrbLand%WaterUseSpecsFile%CheckColNum('Urban water use specifications file as referenced by element '//TRIM(IntToText(indxElem)),[pUrbData(indxElem)%iColWaterUseSpec],.TRUE.,iStat)       ;  IF (iStat .EQ. -1) RETURN
      END DO

    END ASSOCIATE

    !Initial conditions
    CALL ReadRealData(UrbanDataFile,NElements,3,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(DummyArray(:,2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,2)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some fractions of initial soil moisture due to precipitation is less '
        MessageArray(2) = 'than 0.0 or greater than 1.0 for urban areas!'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    IF (MINVAL(DummyArray(:,3:)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,3:)) .GT. 1.0          ) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 or greater than 1.0 for urban areas!'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    UrbLand%UrbData%SoilM_Precip                = DummyArray(:,2)         * DummyArray(:,3)
    UrbLand%UrbData%SoilM_AW                    = (1d0 - DummyArray(:,2)) * DummyArray(:,3)
    UrbLand%UrbData%SoilM_Precip_P_BeforeUpdate = UrbLand%UrbData%SoilM_Precip 
    UrbLand%UrbData%SoilM_Precip_P              = UrbLand%UrbData%SoilM_Precip 
    UrbLand%UrbData%SoilM_AW_P_BeforeUpdate     = UrbLand%UrbData%SoilM_AW 
    UrbLand%UrbData%SoilM_AW_P                  = UrbLand%UrbData%SoilM_AW 
    
    !Close file
    CALL UrbanDataFile%Kill()
    
  END SUBROUTINE UrbanLandUse_New
  
  
  
  
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
  SUBROUTINE UrbanLandUse_Kill(UrbLand)
    TYPE(UrbanDatabaseType) :: UrbLand

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
    UrbLand = Dummy

  END SUBROUTINE UrbanLandUse_Kill
  
  
  
  
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
  SUBROUTINE UrbanLandUse_SetAreas(Area,UrbLand)
    REAL(8),INTENT(IN)      :: Area(:)
    TYPE(UrbanDatabaseType) :: UrbLand
   
    UrbLand%UrbData%Area = Area
    
  END SUBROUTINE UrbanLandUse_SetAreas
  
  
  
  
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
  SUBROUTINE UrbanLandUse_ReadTSData(TimeStep,AppGrid,UrbanLand,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(UrbanDataBaseType)       :: UrbanLand
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode,FileReadCode1
    
    !Initialize
    iStat = 0

    !Echo progress
    CALL EchoProgress('Reading time series data for urban lands')
    
    !Land use areas
    CALL UrbanLand%LandUseDataFile%ReadTSData('Urban areas',TimeStep,AppGrid%AppElement%Area,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (UrbanLand%LandUseDataFile%lUpdated) UrbanLand%UrbData%Area = UrbanLand%LandUseDataFile%rValues(:,2)
    
    !Population
    CALL ReadTSData(TimeStep,'Population data',UrbanLand%PopulationFile,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Water use
    CALL ReadTSData(TimeStep,'Per capita water use data',UrbanLand%PerCapitaWaterUseFile,FileReadCode1,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode1 .EQ. 0) UrbanLand%PerCapitaWaterUseFile%rValues = UrbanLand%PerCapitaWaterUseFile%rValues * UrbanLand%PerCapitaWaterUseFactor
    
    !Water use specifications
    CALL ReadTSData(TimeStep,'Urban water use specifications',UrbanLand%WaterUseSpecsFile,FileReadCode,iStat)
    
  END SUBROUTINE UrbanLandUse_ReadTSData
  

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
    CALL UrbanLand%LandUseDataFile%ReadData(iElem,2,iElem,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,ErrorCode,iStat)  
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
  SUBROUTINE UrbanLandUse_Simulate(AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,ElemSupply,ReuseFrac,ReturnFrac,ElemsToGW,SolverData,lLakeElem,UrbanLand,iStat)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(ETType)                      :: ETData
    TYPE(RootZoneSoilType),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    LOGICAL,INTENT(IN)                :: lLakeElem(:)
    TYPE(UrbanDatabaseType)           :: UrbanLand
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'UrbanLandUse_Simulate'
    INTEGER                      :: indxElem,iColETc(1),KunsatMethod
    REAL(8)                      :: AchievedConv,Area,ETc(1),HydCond,TotalPorosity,Area_Indoors,Area_Outdoors, &
                                    FieldCapacity,TotalPorosityUrban,FieldCapacityUrban,RootDepth,Lambda,      &
                                    AW_Outdoors,AW_Indoors,WiltingPoint,WiltingPointUrban,SoilM_P,SoilM,       &
                                    GM,PrecipD,rMultip,Excess,Inflow,Supply,fRU,fRF,ratio(3),SoilM_P_Array(3), &
                                    SoilM_Array(3),Infilt(3),ETPartition(3)
    LOGICAL                      :: lNegativeMoist
    
    !Initialize
    iStat = 0
  
    !Inform user
    CALL EchoProgress('Simulating flows at urban lands')
    
    ASSOCIATE (pUrbData               => UrbanLand%UrbData                   , &
               pUrbanIndoorsWaterFrac => UrbanLand%WaterUseSpecsFile%rValues )

      !Initialize
      pUrbData%Runoff       = 0.0
      pUrbData%PrecipInfilt = 0.0                     
      pUrbData%ETa          = 0.0                     
      pUrbData%Perc         = 0.0                    
      pUrbData%ReturnFlow   = 0.0
      pUrbData%IrigInfilt   = 0.0  
      pUrbData%GMExcess     = 0.0
      pUrbData%Reuse        = 0.0
      RootDepth             = UrbanLand%RootDepth   
      
      DO indxElem=1,AppGrid%NElements
        !Cycle if Area is zero
        Area = pUrbData(indxElem)%Area
        IF (Area .EQ. 0.0) CYCLE
          
        !Cycle is it is a lake element
        IF (lLakeElem(indxElem)) CYCLE
        
        WiltingPoint  = SoilsData(indxElem)%WiltingPoint
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        HydCond       = SoilsData(indxElem)%HydCond
        Lambda        = SoilsData(indxElem)%Lambda
        KunsatMethod  = SoilsData(indxElem)%KunsatMethod
        iColETc(1)    = pUrbData(indxElem)%iColETc
        ETc           = ETData%GetValues(iColETc)
        PrecipD       = Precip(indxElem) * DeltaT
        GM            = GenericMoisture(1,indxElem) * RootDepth * DeltaT
        ASSOCIATE (pUrban => pUrbData(indxElem))
          
          !Initialize
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
            AW_Indoors  = ElemSupply(indxElem)*pUrbanIndoorsWaterFrac(pUrban%iColWaterUseSpec)       / Area_Indoors
            AW_Outdoors = ElemSupply(indxElem)*(1d0-pUrbanIndoorsWaterFrac(pUrban%iColWaterUseSpec)) / Area_Outdoors
          END IF
        
          !Infiltration and return flow due to applied water
          Supply            = AW_Outdoors * DeltaT
          fRF               = ReturnFrac(pUrban%iColReturnFrac)
          fRU               = ReuseFrac(pUrban%iColReuseFrac)
          pUrban%IrigInfilt = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
          pUrban%ReturnFlow = Supply - pUrban%IrigInfilt
          
          !Total inflow to the root zone
          Inflow = GM + pUrban%IrigInfilt
          
          !Simulate
          CALL NonPondedLUMoistureRouter(PrecipD                                 ,  &
                                         pUrban%SMax                             ,  &
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
                                         pUrban%Runoff                           ,  & 
                                         pUrban%PrecipInfilt                     ,  & 
                                         pUrban%ETa                              ,  & 
                                         pUrban%Perc                             ,  & 
                                         Excess                                  ,  &
                                         AchievedConv                            ) 
 
          !Generate error if convergence is not achieved
          IF (AchievedConv .NE. 0.0) THEN
              MessageArray(1) = 'Convergence error in soil moisture routing for urban lands!'
              MessageArray(2) = 'Element              = '//TRIM(IntToText(indxElem))
              WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityUrban
              WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
              CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
 
          !Reduce inflows based on correction for total porosity
          IF (Excess .NE. 0.0) THEN
              ratio = [pUrban%PrecipInfilt , pUrban%IrigInfilt , GM]
              CALL NormalizeArray(ratio)
              pUrban%Runoff       = pUrban%Runoff     + Excess * ratio(1) 
              pUrban%ReturnFlow   = pUrban%ReturnFlow + Excess * ratio(2)
              pUrban%GMExcess     = Excess * ratio(3)
              pUrban%PrecipInfilt = PrecipD - pUrban%Runoff
              pUrban%IrigInfilt   = Supply  - pUrban%ReturnFlow
          END IF
        
          !Compute re-use based on return flow
          IF (fRF .GT. 0.0) pUrban%Reuse = pUrban%ReturnFlow * fRU / fRF
          
          !Compute moisture from precip and irrigation
          SoilM_P_Array = [pUrban%SoilM_Precip_P , pUrban%SoilM_AW_P , pUrban%SoilM_Oth_P  ]
          Infilt        = [pUrban%PrecipInfilt   , pUrban%IrigInfilt , GM - pUrban%GMExcess]
          CALL TrackMoistureDueToSource(SoilM_P_Array   ,  &
                                        Infilt          ,  &
                                        pUrban%Perc     ,  &
                                        pUrban%ETa      ,  &
                                        0d0             ,  &
                                        SoilM_Array     ,  &
                                        ETPartition     )
          pUrban%SoilM_Precip = SoilM_Array(1)
          pUrban%SoilM_AW     = SoilM_Array(2)
          pUrban%SoilM_Oth    = SoilM_Array(3)
 
          !Make sure soil moisture is not less than zero
          lNegativeMoist = .FALSE.
          IF (pUrban%SoilM_Precip .LT. 0.0) lNegativeMoist = .TRUE.
          IF (pUrban%SoilM_AW     .LT. 0.0) lNegativeMoist = .TRUE.
          IF (pUrban%SoilM_Oth    .LT. 0.0) lNegativeMoist = .TRUE.
          IF (lNegativeMoist) THEN
              MessageArray(1) = 'Soil moisture content becomes negative at element '//TRIM(IntToText(indxElem))//'.'
              MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
              MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
              MessageArray(4) = 'in the Root Zone Main Input File.'
              CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
 
          !Convert depths to volumetric rates
          rMultip             = Area_Outdoors / DeltaT
          pUrban%Runoff       = pUrban%Runoff       * rMultip + Precip(indxElem) * Area_Indoors
          pUrban%ReturnFlow   = pUrban%ReturnFlow   * rMultip + AW_Indoors       * Area_Indoors
          pUrban%PrecipInfilt = pUrban%PrecipInfilt * rMultip
          pUrban%IrigInfilt   = pUrban%IrigInfilt   * rMultip
          pUrban%ETa          = pUrban%ETa          * rMultip
          pUrban%Perc         = pUrban%Perc     * rMultip
          pUrban%Reuse        = pUrban%Reuse        * rMultip
          
          !If surface flow goes to groundwater, update the runoff processes
          IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
            pUrban%Perc         = pUrban%Perc + pUrban%Runoff + pUrban%ReturnFlow
            pUrban%PrecipInfilt = pUrban%PrecipInfilt + pUrban%Runoff        !Runoff and 
            pUrban%IrigInfilt   = pUrban%IrigInfilt + pUrban%ReturnFlow      ! return flow are assumed to bypass root zone for proper mass balance       
            pUrban%Runoff       = 0.0
            pUrban%ReturnFlow   = 0.0
          END IF
        END ASSOCIATE
      END DO
    END ASSOCIATE                
    
  END SUBROUTINE UrbanLandUse_Simulate
  
  
  
  
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
  SUBROUTINE UrbanLandUse_SoilMContent_To_Depth(NElements,TotalPorosity,UrbanLand,iStat)
    INTEGER,INTENT(IN)      :: NElements
    REAL(8),INTENT(IN)      :: TotalPorosity(:)
    TYPE(UrbanDatabaseType) :: UrbanLand
    INTEGER,INTENT(OUT)     :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+34) :: ThisProcedure = ModName // 'UrbanLandUse_SoilMContent_To_Depth'
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
            CALL SetLastMessage('Initial moisture content for urban land at element ' // TRIM(IntToText(indxElem)) // ' is greater than total porosity!',iFatal,ThisProcedure)
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
    
  END SUBROUTINE UrbanLandUse_SoilMContent_To_Depth
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE UrbanLandUse_AdvanceAreas(UrbanLand) 
    TYPE(UrbanDatabaseType) :: UrbanLand
    
    UrbanLand%UrbData%Area_P = UrbanLand%UrbData%Area
    
  END SUBROUTINE UrbanLandUse_AdvanceAreas

  
  ! -------------------------------------------------------------
  ! --- COMPUTE URBAN WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE UrbanLandUse_ComputeWaterDemand(UrbanLand)
    TYPE(UrbanDatabaseType) :: UrbanLand

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
       
  END SUBROUTINE UrbanLandUse_ComputeWaterDemand


END MODULE