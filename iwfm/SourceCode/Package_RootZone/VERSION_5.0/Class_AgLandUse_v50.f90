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
MODULE Class_AgLandUse_v50
  USE TimeSeriesUtilities     , ONLY: TimeStepType                            , &
                                      IncrementTimeStamp                      , &
                                      OPERATOR(.TSGT.)
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter                 , &
                                      CleanSpecialCharacters                  , &
                                      IntTotext                               , &
                                      ArrangeText                             , &
                                      PrepareTitle                            , &
                                      AllocArray                              , &
                                      NormalizeArray                          , &
                                      LocateInList                            , &
                                      EstablishAbsolutePathFileName
  USE MessageLogger           , ONLY: EchoProgress                            , &
                                      SetLastMessage                          , &
                                      LogMessage                              , &
                                      MessageArray                            , &
                                      f_iFatal                                , &
                                      f_iInfo
  USE IOInterface             , ONLY: GenericFileType                         , &
                                      f_iUNKNOWN 
  USE Package_Discretization  , ONLY: AppGridType
  USE Class_GenericLandUse    , ONLY: GenericLandUseType
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType  
  USE Package_UnsatZone       , ONLY: RootZoneSoilType                        , &
                                      NonPondedLUMoistureRouter               , &
                                      NonPondedCropDemand
  USE Package_Misc            , ONLY: SolverDataType                          , &
                                      RealTSDataInFileType                    , &
                                      IntTSDataInFileType                     , &
                                      PrepareTSDOutputFile                    , &
                                      TSDataFile_ReadData => ReadTSData 
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource
  USE Util_Package_RootZone   , ONLY: ReadRealData                            , &
                                      ReadPointerData                         , &
                                      f_iDemandFromMoistAtBegin               , &
                                      f_iDemandFromMoistAtEnd                 , &
                                      f_iNoIrigPeriod                         , &
                                      f_iIrigPeriod                           , &
                                      f_iIrigPeriodFlags
  USE Package_PrecipitationET , ONLY: ETType
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
  PUBLIC :: AgDatabase_v50_Type


  ! -------------------------------------------------------------
  ! --- AG LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: AgType
      REAL(8) :: IrigInfilt              = 0.0     !Infiltration due to irrigation
      REAL(8) :: Reuse                   = 0.0     !Reused return flow 
      REAL(8) :: ReturnFlow              = 0.0     !Return flow
      REAL(8) :: DemandRaw               = 0.0     !Ag demand before net return flow is computed
      REAL(8) :: Demand                  = 0.0     !Ag demand after net return flow is included
      REAL(8) :: ETAW                    = 0.0     !ET of applied water
      REAL(8) :: ETP                     = 0.0     !ET of precipitation
      REAL(8) :: ETOth                   = 0.0     !ET from sources other than irrigtaion and precipitation
  END TYPE AgType
  
  
  ! -------------------------------------------------------------
  ! --- AVERAGE CROP CHARACTERISTICS DATA TYPE
  ! -------------------------------------------------------------
  TYPE AvgCropType
      REAL(8) :: RootDepth   = 0.0     
      REAL(8) :: ETc         = 0.0 
      INTEGER :: IrigPeriod  = f_iNoIrigPeriod
      REAL(8) :: MinSoilM    = 0.0
      REAL(8) :: TargetSoilM = 0.0
  END TYPE AvgCropType
  
  
  ! -------------------------------------------------------------
  ! --- AG LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE AgDatabase_v50_Type
      TYPE(AgType),ALLOCATABLE      :: AgData(:,:)                                         !Ag data for each (soil,subregion) combination
      INTEGER                       :: NCrops                 = 0                          !Number of simulated crops
      TYPE(AvgCropType),ALLOCATABLE :: AvgCrop(:)                                          !Average crop parameters at each (subregion)
      INTEGER                       :: iDemandFromMoist       = f_iDemandFromMoistAtBegin  !Flag to check if demand is computed based on moisture at the beginning or at the end of timestep
      REAL(8),ALLOCATABLE           :: RootDepth(:)                                        !Rooting depth for each ag crop
      INTEGER,ALLOCATABLE           :: iColReturnFrac(:)                                   !Column number in the return flow fraction data file defined for each (subregion)
      INTEGER,ALLOCATABLE           :: iColReuseFrac(:)                                    !Column number in the re-use fraction data file defined for each (subregion)
      INTEGER,ALLOCATABLE           :: iColETcCrop(:,:)                                    !Column number in the ET data file for each (crop,subregion) combination; overrides the iColETc parameter in AgType data type
      INTEGER,ALLOCATABLE           :: iColWaterDemand(:)                                  !Column number in the ag water demand data file (if specified) defined for each (subregion)
      INTEGER,ALLOCATABLE           :: iColIrigPeriod(:,:)                                 !Column number in irrigation period data file for each (crop,subregion) combination
      INTEGER,ALLOCATABLE           :: iColMinSoilM(:,:)                                   !Column number in min. soil moisture file for each (crop,subregion) combination
      INTEGER,ALLOCATABLE           :: iColTargetSoilM(:,:)                                !Column number in irrgation target soil moisture file for each (crop,subregion) combination
      REAL(8),ALLOCATABLE           :: ElementalArea(:)                                    !Ag area at each (element) at current time step
      REAL(8),ALLOCATABLE           :: ElementalArea_P(:)                                  !Ag area at each (element) at previous time step
      REAL(8),ALLOCATABLE           :: SubregionalArea(:)                                  !Total ag area for each (subregion)             
      REAL(8),ALLOCATABLE           :: SubregionalCropAreaFrac(:,:)                        !Subregional crop area fractions for each (crop,subregion) combination
      REAL(8),ALLOCATABLE           :: SubregionalDemand(:)                                !Ag water demand for each (subregion)
      REAL(8),ALLOCATABLE           :: RegionETPot(:)                                      !Regional AG potential ET
      TYPE(LandUseDataFileType)     :: SubregionCropAreaDataFile                           !Subregional crop area data file
      TYPE(LandUseDataFileType)     :: ElemAgAreaDataFile                                  !Elemental ag area data file
      TYPE(RealTSDataInFileType)    :: WaterDemandFile                                     !Ag water demand data file (optional)
      REAL(8)                       :: WaterDemandFactor      = 1.0                        !Conversion factor for specified ag water demand
      TYPE(IntTSDataInFileType)     :: IrigPeriodFile                                      !Irrigation period file
      TYPE(RealTSDataInFileType)    :: MinSoilMFile                                        !Irrigation trigger minimum soil moisture data file
      TYPE(RealTSDataInFileType)    :: TargetSoilMFile                                     !Irrigation target soil moisture data file
      TYPE(GenericFileType)         :: AvgCropOutput                                       !Output file for average crop characteristics
      REAL(8)                       :: FactorAvgCropLen       = 1.0                        !Conversion factor for the length unit of average crop characteristics output
      LOGICAL                       :: lWaterDemand_Defined   = .FALSE.                    !Flag to check if ag water demand is pre-defined or computed dynamically
      LOGICAL                       :: lAvgCropOutput_Defined = .FALSE.                    !Flag to check if output file for average crop characteristics is specified
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: GetMaxAndMinNetReturnFlowFrac
      PROCEDURE,PASS :: SetAreas
      PROCEDURE,PASS :: ReadTSData
      PROCEDURE,PASS :: ReadRestartData
      PROCEDURE,PASS :: PrintResults
      PROCEDURE,PASS :: PrintRestartData
      PROCEDURE,PASS :: SoilMContent_To_Depth
      PROCEDURE,PASS :: AdvanceAreas
      PROCEDURE,PASS :: Simulate
      PROCEDURE,PASS :: ComputeWaterDemand
      PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp
  END TYPE AgDatabase_v50_Type


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AgLandUse_v50::'
  
  
  
  
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
  ! --- NEW AG LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(AgLand,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,FactCN,NSoils,iSubregionIDs,TimeStep,iStat)
    CLASS(AgDatabase_v50_Type)    :: AgLand
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    REAL(8),INTENT(IN)            :: FACTCN
    INTEGER,INTENT(IN)            :: NSoils,iSubregionIDs(AppGrid%NSubregions)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: NElements,NSubregions,ErrorCode,NCrops,indxCrop,indxRegion,iRegion,ID
    INTEGER,ALLOCATABLE         :: DummyIntArray(:,:)
    REAL(8)                     :: Factor(1)
    REAL(8),ALLOCATABLE         :: DummyArray(:,:)
    CHARACTER                   :: ALine*1000,cUnitAvgCropOutput*10
    LOGICAL                     :: lProcessed(AppGrid%NSubregions)
    TYPE(GenericFileType)       :: AgDataFile
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Initialize
    NElements   = AppGrid%NElements
    NSubregions = AppGrid%NSubregions
    
    !Open file
    CALL AgDataFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Number of crops
    CALL AgDataFile%ReadData(NCrops,iStat)  ;  IF (iStat .EQ. -1) RETURN    
    AgLand%NCrops = NCrops
    
    !Allocate memory
    ALLOCATE (AgLand%RootDepth(NCrops)                            , &
              AgLand%AgData(NSoils,NSubregions)                   , &
              AgLand%AvgCrop(NSubregions)                         , &
              AgLand%iColReturnFrac(NSubregions)                  , &
              AgLand%iColReuseFrac(NSubregions)                   , &
              AgLand%iColETcCrop(NCrops,NSubregions)              , &
              AgLand%iColIrigPeriod(NCrops,NSubregions)           , &
              AgLand%iColMinSoilM(NCrops,NSubregions)             , &
              AgLand%iColTargetSoilM(NCrops,NSubregions)          , &
              AgLand%ElementalArea(NElements)                     , &
              AgLand%ElementalArea_P(NElements)                   , &
              AgLand%SubregionalArea(NSubregions)                 , &
              AgLand%SubregionalCropAreaFrac(NCrops,NSubregions)  , &
              AgLand%SubregionalDemand(NSubregions)               , &
              AgLand%RegionETPot(NSubregions)                     , &
              STAT=ErrorCode                                      )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for agricultural data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Initialize elemental area to zero
    AgLand%ElementalArea = 0.0
    
    !Subregional crop area data file
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AgLand%SubregionCropAreaDataFile%New(cAbsPathFileName,cWorkingDirectory,'Subregional crop area file',NSubregions,NCrops,TimeStep%TrackTime,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Elemental ag area data file
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AgLand%ElemAgAreaDataFile%New(cAbsPathFileName,cWorkingDirectory,'Elemental agricultural area file',NElements,1,TimeStep%TrackTime,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Average crop output file and related data
    !-----------------------------------------    
    !Output conversion factor
    CALL AgDataFile%ReadData(AgLand%FactorAvgCropLen,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Output unit
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    cUnitAvgCropOutput = ADJUSTL(TRIM(ALine))
    
    !Output filename
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    ALine = ADJUSTL(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL PrepareAvgCropOutputFile(IsForInquiry,cAbsPathFileName,cUnitAvgCropOutput,TimeStep%Unit,NSubregions,AgLand%AvgCropOutput,iStat)  ;  IF (iStat .EQ. -1) RETURN 
        AgLand%lAvgCropOutput_Defined = .TRUE.
    END IF
    
    !Rooting depths
    CALL AgDataFile%ReadData(Factor(1),iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indxCrop=1,NCrops
        CALL AgDataFile%ReadData(AgLand%RootDepth(indxCrop),iStat)  ;  IF (iStat .EQ. -1) RETURN 
        AgLand%RootDepth(indxCrop) = AgLand%RootDepth(indxCrop) * Factor(1)
    END DO
    
    !Read curve numbers
    CALL ReadRealData(AgDataFile,'curve numbers for agricultural crops','subregions',NSubregions,NSoils+1,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = INT(DummyArray(indxRegion,1))
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Curve numbers for agricultural crops at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)           = .TRUE.
        AgLand%AgData(:,iRegion)%SMax = (1000.0/DummyArray(indxRegion,2:)-10.0) * FACTCN
    END DO
    
    !Read ETc column pointers
    CALL ReadPointerData(AgDataFile,'evapotranspiration column pointers for agricultural crops','subregions',NSubregions,NCrops+1,iSubregionIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = DummyIntArray(indxRegion,1)
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Evapotranspration column pointers for agricultural crops at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)           = .TRUE.
        AgLand%iColETcCrop(:,iRegion) = DummyIntArray(indxRegion,2:)
    END DO
    
    !Irrigation period data
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/')
    CALL CleanSpecialCharacters(ALine)
    ALine = ADJUSTL(ALine)
    IF (ALine .EQ. '') THEN
        CALL SetLastMessage('Irrigation Period Data File in Agricultural Lands Main Data File must be specified!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AgLand%IrigPeriodFile%Init(cAbsPathFileName,cWorkingDirectory,'agricultural crops irrigation period data file',TimeStep%TrackTime,BlocksToSkip=1,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadPointerData(AgDataFile,'irrigation period column pointers for agricultural crops','subregions',NSubregions,NCrops+1,iSubregionIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = DummyIntArray(indxRegion,1)
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Irrigation period column pointers for agricultural crops at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)              = .TRUE.
        AgLand%iColIrigPeriod(:,iRegion) = DummyIntArray(indxRegion,2:)
    END DO
    CALL AgLand%IrigPeriodFile%CheckColNum('irrigation period data file',PACK(AgLand%iColIrigPeriod,.TRUE.),.TRUE.,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Irrigation trigger minimum soil moisture 
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/')
    CALL CleanSpecialCharacters(ALine)
    ALine = ADJUSTL(ALine)
    IF (ALine .EQ. '') THEN
        CALL SetLastMessage('Minimum Soil Moisture Data File in Agricultural Lands Main Data File must be specified!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AgLand%MinSoilMFile%Init(cAbsPathFileName,cWorkingDirectory,'agricultural crops minimum soil moisture data file',TimeStep%TrackTime,BlocksToSkip=1,lFactorDefined=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadPointerData(AgDataFile,'minimum soil moisture column pointers for agricultural crops','subregions',NSubregions,NCrops+1,iSubregionIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = DummyIntArray(indxRegion,1)
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Irrigation period column pointers for agricultural crops at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)            = .TRUE.
        AgLand%iColMinSoilM(:,iRegion) = DummyIntArray(indxRegion,2:)
    END DO
    CALL AgLand%MinSoilMFile%CheckColNum('minimum soil moisture data file',PACK(AgLand%iColMinSoilM,.TRUE.),.TRUE.,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Irrigation target soil moisture
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL AgLand%TargetSoilMFile%Init(cAbsPathFileName,cWorkingDirectory,'irrigation target soil moisture data file',TimeStep%TrackTime,BlocksToSkip=1,lFactorDefined=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ReadPointerData(AgDataFile,'irrigation target soil moisture column pointers for agricultural crops','subregions',NSubregions,NCrops+1,iSubregionIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        lProcessed = .FALSE.
        DO indxRegion=1,NSubregions
            iRegion = DummyIntArray(indxRegion,1)
            IF (lProcessed(iRegion)) THEN
                ID = iSubregionIDs(iRegion)
                CALL SetLastMessage('Irrigation target soil moisture pointers for agricultural crops at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iRegion)               = .TRUE.
            AgLand%iColTargetSoilM(:,iRegion) = DummyIntArray(indxRegion,2:)
        END DO
        CALL AgLand%TargetSoilMFile%CheckColNum('irrigation target soil moisture data file',PACK(AgLand%iColTargetSoilM,.TRUE.),.TRUE.,iStat)  
        IF (iStat .EQ. -1) RETURN
    ELSE
        ALLOCATE (AgLand%TargetSoilMFile%rValues(1))
        AgLand%TargetSoilMFile%rValues(1) = 1.0
        AgLand%TargetSoilMFile%iSize      = 1
        AgLand%iColTargetSoilM            = 1
    END IF    
    
    !Specified water demand, return flow and reuse factors
    !-----------------------------------------------------
    !Specified water demand file
    CALL AgDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/')
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL AgLand%WaterDemandFile%Init(cAbsPathFileName,cWorkingDirectory,'agricultural supply requirement data file',TimeStep%TrackTime,BlocksToSkip=1,lFactorDefined=.TRUE.,Factor=Factor,RateTypeData=[.TRUE.],iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        AgLand%WaterDemandFactor    = Factor(1)
        AgLand%lWaterDemand_Defined = .TRUE.
        ALLOCATE (AgLand%iColWaterDemand(NSubregions))
    END IF
    
    !Flag to see if moisture at the beginning or at the end of time step will be used to compute water demand
    CALL AgDataFile%ReadData(AgLand%iDemandFromMoist,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AgLand%iDemandFromMoist .NE. f_iDemandFromMoistAtBegin  .AND.  &
        AgLand%iDemandFromMoist .NE. f_iDemandFromMoistAtEnd         ) THEN
        MessageArray(1) = 'Flag for soil moisture to be used in the computation of agricultural '
        MessageArray(2) = 'crop water demand and irrigation timing is not recognized!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
        
    !Water supply requirement, return flow and reuse factor pointers
    CALL ReadRealData(AgDataFile,'water supply, return flow and re-use factors for agricultural crops','subregions',NSubregions,4,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = INT(DummyArray(indxRegion,1))
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Water supply requirements, return flow and re-use fractions for agricultural crops at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)            = .TRUE.
        AgLand%iColReturnFrac(iRegion) = INT(DummyArray(indxRegion,3))
        AgLand%iColReuseFrac(iRegion)  = INT(DummyArray(indxRegion,4))
        IF (AgLand%lWaterDemand_Defined) &
            AgLand%iColWaterDemand(iRegion) = INT(DummyArray(indxRegion,2))
    END DO
    IF (AgLand%lWaterDemand_Defined) THEN
        CALL AgLand%WaterDemandFile%CheckColNum('agricultural water supply requirement data file',AgLand%iColWaterDemand,.FALSE.,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Initial conditions
    !------------------
    CALL ReadRealData(AgDataFile,'initial conditions for agricultural crops','subregions',NSubregions,2*NSoils+1,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Make sure fractions due to precipitation are between 0 and 1
    IF (MINVAL(DummyArray(:,2::2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,2::2)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some fractions of initial soil moisture due to precipitation is less '
        MessageArray(2) = 'than 0.0 or greater than 1.0 for agricultural areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
        
    !Make sure initial moisture contents are between 0 and 1 
    IF (MINVAL(DummyArray(:,3::2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,3::2)) .GT. 1.0          ) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 or greater than 1.0 for agricultrural areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    
    !Store data in persistent arrays
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = INT(DummyArray(indxRegion,1))
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Initial conditions for agricultural crops at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)                                  = .TRUE.
        AgLand%AgData(:,iRegion)%SoilM_Precip                = DummyArray(indxRegion,2::2) * DummyArray(indxRegion,3::2)
        AgLand%AgData(:,iRegion)%SoilM_AW                    = DummyArray(indxRegion,3::2) - AgLand%AgData(:,iRegion)%SoilM_Precip
        AgLand%AgData(:,iRegion)%SoilM_Precip_P              = AgLand%AgData(:,iRegion)%SoilM_Precip 
        AgLand%AgData(:,iRegion)%SoilM_AW_P                  = AgLand%AgData(:,iRegion)%SoilM_AW
        AgLand%AgData(:,iRegion)%SoilM_Precip_P_BeforeUpdate = AgLand%AgData(:,iRegion)%SoilM_Precip 
        AgLand%AgData(:,iRegion)%SoilM_AW_P_BeforeUpdate     = AgLand%AgData(:,iRegion)%SoilM_AW
    END DO
    
    !Close ag data file
    CALL AgDataFile%Kill()
    
    !Clear memory
    DEALLOCATE (DummyArray , DummyIntArray ,  STAT=ErrorCode)
    
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
  ! --- KILL AG LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AgLand)
    CLASS(AgDatabase_v50_Type) :: AgLand
    
    !Local variables
    INTEGER                    :: ErrorCode
    TYPE(AgDatabase_v50_Type) :: DefaultAgLand
    
    !Clear memory
    DEALLOCATE (AgLand%AgData                  , &
                AgLand%AvgCrop                 , &
                AgLand%RootDepth               , &
                AgLand%iColReturnFrac          , &
                AgLand%iColReuseFrac           , &
                AgLand%iColETcCrop             , &
                AgLand%iColWaterDemand         , &
                AgLand%iColIrigPeriod          , &
                AgLand%iColMinSoilM            , &
                AgLand%iColTargetSoilM         , &
                AgLand%ElementalArea           , &
                AgLand%ElementalArea_P         , &
                AgLand%SubregionalArea         , &
                AgLand%SubregionalCropAreaFrac , &
                AgLand%SubregionalDemand       , &
                AgLand%RegionETPot             , &
                STAT = ErrorCode               )
    
    !Close files
    CALL AgLand%SubregionCropAreaDataFile%Kill()
    CALL AgLand%ElemAgAreaDataFile%Kill()
    CALL AgLand%WaterDemandFile%Close()
    CALL AgLand%IrigPeriodFile%Close()
    CALL AgLand%MinSoilMFile%Close()
    CALL AgLand%TargetSoilMFile%Close()
    CALL AgLand%AvgCropOutput%Kill()
    
    !Initialize variables to their defaults
    SELECT TYPE (AgLand)
        TYPE IS (AgDatabase_v50_Type)
            AgLand = DefaultAgLand
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
  SUBROUTINE GetMaxAndMinNetReturnFlowFrac(AgLand,ReturnFracFile,ReuseFracFile,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    CLASS(AgDatabase_v50_Type),INTENT(IN) :: AgLand
    TYPE(RealTSDataInFileType)            :: ReturnFracFile,ReuseFracFile
    TYPE(TimeStepType),INTENT(IN)         :: FirstTimeStep
    REAL(8),INTENT(OUT)                   :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    INTEGER            :: FileReadCode_Return,FileReadCode_Reuse,indx
    REAL(8)            :: rRT,rRU
    
    !Initialize
    TimeStep = FirstTimeStep
    rMaxFrac = 0.0
    rMinFrac = 1.0
    
    !Loop through timesteps and read return flow fractions
    DO
        !Read data
        CALL TSDataFile_ReadData(TimeStep,'Return flow fractions data',ReturnFracFile,FileReadCode_Return,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL TSDataFile_ReadData(TimeStep,'Reuse fractions data',ReuseFracFile,FileReadCode_Reuse,iStat)          ;  IF (iStat .EQ. -1) RETURN
        
        !If new data is read, find min and max
        IF (FileReadCode_Return.EQ.0  .OR.  FileReadCode_Reuse.EQ.0) THEN
            DO indx=1,SIZE(AgLand%iColReturnFrac)
                rRT      = ReturnFracFile%rValues(AgLand%iColReturnFrac(indx))
                rRU      = ReuseFracFile%rValues(AgLand%iColReuseFrac(indx))
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
  SUBROUTINE SetAreas(AgLand,AppGrid,iSoilType,AreaElem)
    CLASS(AgDatabase_v50_Type)   :: AgLand
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: iSoilType(:)
    REAL(8),INTENT(IN)           :: AreaElem(:)
   
    !Local variables
    INTEGER :: indxElem,iSoil,iRegion
    
    !Initialize
    AgLand%agData%Area = 0.0
    
    !Set elemental areas
    AgLand%ElementalArea = AreaElem
    
    !Calulcuate areas for (soil,region) combinations
    DO indxElem=1,AppGrid%NElements
        iRegion                           = AppGrid%AppElement(indxElem)%Subregion
        iSoil                             = iSoilType(indxElem)
        AgLand%AgData(iSoil,iRegion)%Area = AgLand%AgData(iSoil,iRegion)%Area + AreaElem(indxElem)
    END DO
    
    !Update subregional areas
    AgLand%SubregionalArea = SUM(AgLand%AgData%Area , DIM=1)
    
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
  SUBROUTINE ReadRestartData(AgLand,InFile,iStat)
    CLASS(AgDatabase_v50_Type) :: AgLand
    TYPE(GenericFileType)      :: InFile
    INTEGER,INTENT(OUT)        :: iStat
    
    CALL InFile%ReadData(AgLand%AgData%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%ReturnFlow,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%AgData%SoilM_Oth,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%ElementalArea,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AgLand%ElementalArea_P,iStat)        ;  IF (iStat .EQ. -1) RETURN
    
  END SUBROUTINE ReadRestartData

    
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AgLand,NSoils,ElemSoilTypes,iSubregionIDs,rRegionAreas,SoilRegionArea,WiltingPoint,FieldCapacity,lLakeElem,ETData,TimeStep,AppGrid,RegionCropAreas,iStat)
    CLASS(AgDatabase_v50_Type)    :: AgLand
    INTEGER,INTENT(IN)            :: NSoils,ElemSoilTypes(:),iSubregionIDs(:)
    REAL(8),INTENT(IN)            :: rRegionAreas(:),SoilRegionArea(:,:),WiltingPoint(:,:),FieldCapacity(:,:)
    LOGICAL,INTENT(IN)            :: lLakeElem(:)
    TYPE(ETType),INTENT(IN)       :: ETData
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    REAL(8),OPTIONAL,INTENT(IN)   :: RegionCropAreas(:,:)  !If provided, will overwrite the subregional crop area read from file
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReadTSData'
    INTEGER                      :: iSoil,iRegion,indxElem,indxRegion,indxCol,indxCrop,indxSoil,FileReadCode
    REAL(8)                      :: TAW,WP,MinSoilM,TargetSoilM,rFrac,FC
    LOGICAL                      :: lMinSoilM_Updated,lTargetSoilM_Updated
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Reading time series data for agricultural lands')
    
    !Elemental ag areas
    CALL AgLand%ElemAgAreaDataFile%ReadTSData('Elemental agricultural areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (AgLand%ElemAgAreaDataFile%lUpdated) THEN
        ASSOCIATE (pArea     => AgLand%AgData%Area   , &
                   pElemArea => AgLand%ElementalArea )
            pArea = 0.0
            DO indxElem=1,AppGrid%NElements
                IF (lLakeElem(indxElem)) CYCLE
                iSoil                = ElemSoilTypes(indxElem)
                iRegion              = AppGrid%AppElement(indxElem)%Subregion
                pElemArea(indxElem)  = AgLand%ElemAgAreaDataFile%rValues(indxElem,2)
                pArea(iSoil,iRegion) = pArea(iSoil,iRegion) + pElemArea(indxElem)
            END DO
            
            !Update subregional areas
            AgLand%SubregionalArea = SUM(pArea , DIM=1)
        END ASSOCIATE        
    END IF
    
    !Irrigation period
    CALL TSDataFile_ReadData(TimeStep,'Crop irrigation period data',AgLand%IrigPeriodFile,FileReadCode,iStat)  
    IF (iStat .EQ. -1) RETURN
    IF (AgLand%IrigPeriodFile%lUpdated) THEN
        DO indxCol=1,AgLand%IrigPeriodFile%iSize
            IF (LocateInList(AgLand%IrigPeriodFile%iValues(indxCol) , f_iIrigPeriodFlags) .EQ. 0) THEN
                CALL SetLastMessage('One or more irrigation period flags are not recognized!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END IF
    
    !Min. soil moisture data as irrigation trigger
    CALL TSDataFile_ReadData(TimeStep,'Minimum soil moisture requirement data',AgLand%MinSoilMFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lMinSoilM_Updated = AgLand%MinSoilMFile%lUpdated
    
    !Irrigation target soil moisture data
    CALL TSDataFile_ReadData(TimeStep,'Irrigation target soil moisture data',AgLand%TargetSoilMFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lTargetSoilM_Updated = AgLand%TargetSoilMFile%lUpdated
    
    !Make sure that irrigation target soil moisture is not less than minimum soil moisture
    IF (lMinSoilM_Updated .OR. lTargetSoilM_Updated) THEN
        DO indxRegion=1,AppGrid%NSubregions
            DO indxSoil=1,NSoils
                IF (SoilRegionArea(indxSoil,indxRegion) .EQ. 0.0) CYCLE
                WP  = WiltingPoint(indxSoil,indxRegion)
                TAW = FieldCapacity(indxSoil,indxRegion) - WP
                DO indxCrop=1,AgLand%NCrops
                    TargetSoilM = AgLand%TargetSoilMFile%rValues(AgLand%iColTargetSoilM(indxCrop,indxRegion))
                    rFrac       = AgLand%MinSoilMFile%rValues(AgLand%iColMinSoilM(indxCrop,indxRegion))
                    MinSoilM    = WP + rFrac * TAW
                    IF (TargetSoilM .LT. MinSoilM) THEN
                        MessageArray(1) = 'Irrigation target soil moisture for agricultural crop' // TRIM(IntToText(indxCrop)) // ' is less than minimum '
                        MessageArray(2) = 'soil moisture at subregion ' // TRIM(IntToText(iSubregionIDs(indxRegion))) // '!'
                        CALL SetLastMessage(MessageArray(:2),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF 
                END DO
            END DO
        END DO
    END IF
    
    !Warn user if min. soil moisture is less than half of field capacity (deficit irrigation)
    IF (lMinSoilM_Updated) THEN
        DO indxRegion=1,AppGrid%NSubregions
            DO indxSoil=1,NSoils
                IF (SoilRegionArea(indxSoil,indxRegion) .EQ. 0.0) CYCLE
                WP    = WiltingPoint(indxSoil,indxRegion)
                FC    = FieldCapacity(indxSoil,indxRegion)
                DO indxCrop=1,AgLand%NCrops
                    rFrac = AgLand%MinSoilMFile%rValues(AgLand%iColMinSoilM(indxCrop,indxRegion))
                    IF (WP+rFrac*(FC-WP) .LT. 0.5d0*FC) THEN
                        MessageArray(1) = 'Deficit irrigation is being simulated for crop ID '//TRIM(IntToText(indxCrop))//' at soil type '//TRIM(IntToText(indxSoil))//' in subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//'!'
                        WRITE (MessageArray(2),'(A,F6.3)') 'Irrigation trigger minimum moisture = ' , WP + rFrac*(FC-WP)
                        WRITE (MessageArray(3),'(A,F6.3)') 'Half of field capacity              = ' , 0.5D0 * FC
                        CALL LogMessage(MessageArray(1:3),f_iInfo,ThisProcedure)
                    END IF
                END DO
            END DO
        END DO
    END IF
    
    !Specified water demand
    IF (AgLand%lWaterDemand_Defined) THEN
        CALL TSDataFile_ReadData(TimeStep,'Agricultural water supply requirement data',AgLand%WaterDemandFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (AgLand%WaterDemandFile%lUpdated) THEN
            DO indxRegion=1,AppGrid%NSubregions
                IF (AgLand%iColWaterDemand(indxRegion) .EQ. 0) CYCLE
                AgLand%SubregionalDemand(indxRegion) = AgLand%WaterDemandFile%rValues(AgLand%iColWaterDemand(indxRegion)) * AgLand%WaterDemandFactor
                !Make sure that ag area is non-zero when there is non-zero demand
                IF (AgLand%SubregionalDemand(indxRegion) .GT. 0.0) THEN
                    IF (AgLand%SubregionalArea(indxRegion) .EQ. 0.0) THEN
                        CALL SetLastMessage('Agricultural water supply requirement at subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//' is greater than zero when agricultural area is zero!',f_iFatal,ThisProcedure) 
                        iStat = -1
                        RETURN
                    END IF
                END IF
            END DO
        END IF
    END IF
    
    !Subregional crop areas
    IF (PRESENT(RegionCropAreas)) THEN
        !Read out the data in file so that the pointer will be positioned properly
        CALL AgLand%SubregionCropAreaDataFile%ReadTSData('Subregional agricultural crop areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)
        IF (iStat .EQ. -1) RETURN
        !Compute subregional crop area fractions
        DO indxRegion=1,AppGrid%NSubregions
           AgLand%SubregionalCropAreaFrac(:,indxRegion) = RegionCropAreas(:,indxRegion)
           CALL NormalizeArray(AgLand%SubregionalCropAreaFrac(:,indxRegion))
           
           !Make sure crop fractions are non-zero if there is specified elemental ag 
           IF (SUM(AgLand%SubregionalCropAreaFrac(:,indxRegion)) .EQ. 0.0) THEN
               IF (AgLand%SubregionalArea(indxRegion) .GT. 0.0) THEN
                   CALL SetLastMessage('Subregional crop areas cannot be all zero in subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//' when elemental agricultural areas are non-zero!',f_iFatal,ThisProcedure)
                   iStat = -1
                   RETURN
               END IF
           END IF 
        END DO
    ELSE
        CALL AgLand%SubregionCropAreaDataFile%ReadTSData('Subregional agricultural crop areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)
        IF (iStat .EQ. -1) RETURN
        IF (AgLand%SubregionCropAreaDataFile%lUpdated) THEN
            
            !Compute subregional crop area fractions
            DO indxRegion=1,AppGrid%NSubregions
               AgLand%SubregionalCropAreaFrac(:,indxRegion) = AgLand%SubregionCropAreaDataFile%rValues(indxRegion,2:)
               CALL NormalizeArray(AgLand%SubregionalCropAreaFrac(:,indxRegion))
               
               !Make sure crop fractions are non-zero if there is specified elemental ag 
               IF (SUM(AgLand%SubregionalCropAreaFrac(:,indxRegion)) .EQ. 0.0) THEN
                   IF (AgLand%SubregionalArea(indxRegion) .GT. 0.0) THEN
                       CALL SetLastMessage('Subregional crop areas cannot be all zero in subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//' when elemental agricultural areas are non-zero!',f_iFatal,ThisProcedure)
                       iStat = -1
                       RETURN
                   END IF
               END IF 
            END DO
            
        END IF
    END IF
    
    !Update average crop characteristics
    IF (lMinSoilM_Updated                         .OR. &
        lTargetSoilM_Updated                      .OR. &
        AgLand%IrigPeriodFile%lUpdated            .OR. &
        AgLand%SubregionCropAreaDataFile%lUpdated .OR. &
        PRESENT(RegionCropAreas)                       ) CALL ComputeAvgCropCharacteristics(AppGrid%NSubregions,ETData,AgLand)
        
    !Compute subregional potential ET
    IF (ETData%IsUpdated()                        .OR. &
        AgLand%SubregionCropAreaDataFile%lUpdated .OR. &
        AgLand%ElemAgAreaDataFile%lUpdated        .OR. &
        PRESENT(RegionCropAreas)                       ) AgLand%RegionETPot = AgLand%AvgCrop%ETc * AgLand%SubregionalArea
    
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
  SUBROUTINE PrintRestartData(AgLand,OutFile)
    CLASS(AgDatabase_v50_Type),INTENT(IN) :: AgLand
    TYPE(GenericFileType)                 :: OutFile
    
    CALL OutFile%WriteData(AgLand%AgData%Runoff)
    CALL OutFile%WriteData(AgLand%AgData%ReturnFlow)
    CALL OutFile%WriteData(AgLand%AgData%Area)
    CALL OutFile%WriteData(AgLand%AgData%Area_P)
    CALL OutFile%WriteData(AgLand%AgData%SoilM_Precip_P)
    CALL OutFile%WriteData(AgLand%AgData%SoilM_Precip)
    CALL OutFile%WriteData(AgLand%AgData%SoilM_AW_P)
    CALL OutFile%WriteData(AgLand%AgData%SoilM_AW)
    CALL OutFile%WriteData(AgLand%AgData%SoilM_Oth_P)
    CALL OutFile%WriteData(AgLand%AgData%SoilM_Oth)
    CALL OutFile%WriteData(AgLand%ElementalArea)
    CALL OutFile%WriteData(AgLand%ElementalArea_P)
    
  END SUBROUTINE PrintRestartData

    
  ! -------------------------------------------------------------
  ! --- PRINT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AgLand,NSubregions,TimeStep,lEndOfSimulation)
    CLASS(AgDatabase_v50_Type)    :: AgLand
    INTEGER,INTENT(IN)            :: NSubregions
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndofSimulation
    
    !Local variables
    INTEGER           :: indxRegion,indx
    REAL(8)           :: rValues(4*NSubregions),Factor
    CHARACTER(LEN=21) :: SimulationTime
    
    !Return if no print out is required
    IF (.NOT. AgLand%lAvgCropOutput_Defined) RETURN
    
    !Initialize
    Factor = AgLand%FactorAvgCropLen
   
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime=ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Prepare output
    DO indxRegion=1,NSubregions
        indx            = (indxRegion-1) * 4
        rValues(indx+1) = AgLand%AvgCrop(indxRegion)%RootDepth * Factor
        rValues(indx+2) = AgLand%AvgCrop(indxRegion)%MinSoilM
        rValues(indx+3) = AgLand%AvgCrop(indxRegion)%TargetSoilM
        rValues(indx+4) = AgLand%AvgCrop(indxRegion)%ETc * Factor
    END DO

    !Print
    CALL AgLand%AvgCropOutput%WriteData(SimulationTime,rValues,FinalPrint=lEndOfSimulation)      
    
  END SUBROUTINE PrintResults
  
  
  
  
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
  ! --- SIMULATE FLOW PROCESSES AT AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AgLand,iSubregionIDs,DeltaT,Precip,GenericMoisture,SubregionSoilsData,WaterSupply,ReuseFrac,ReturnFrac,SolverData,iStat)
    CLASS(AgDatabase_v50_Type)        :: AgLand
    INTEGER,INTENT(IN)                :: iSubregionIDs(:)
    TYPE(RootZoneSoilType),INTENT(IN) :: SubregionSoilsData(:,:)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:,:),GenericMoisture(:,:),WaterSupply(:),ReuseFrac(:),ReturnFrac(:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    INTEGER                     :: indxRegion,indxSoil,KunsatMethod
    REAL(8)                     :: RootDepth,ETc,WiltingPoint,FieldCapacity,TotalPorosity,HydCond,Lambda,GM,PrecipD,  &
                                   Supply,SoilM_P,fRF,fRU,Inflow,SoilM,Excess,AchievedConv,ratio(3),SoilM_P_Array(3), &
                                   Infilt(3),SoilM_Array(3),ETPartition(3)
    LOGICAL                     :: lNegativeMoist
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('Simulating flows at agricultural lands...')
    
    ASSOCIATE (pAgData  => AgLand%AgData   , &
               pAvgCrop => AgLand%AvgCrop  )
        
        DO indxRegion=1,SIZE(pAgData , DIM=2)
            
            !Initialize subregion-level values
            RootDepth = pAvgCrop(indxRegion)%RootDepth
            ETc       = pAvgCrop(indxRegion)%ETc * DeltaT
            Supply    = WaterSupply(indxRegion) * DeltaT
            
            !Infiltration and return flow due to applied water
            fRF                              = ReturnFrac(AgLand%iColReturnFrac(indxRegion))
            fRU                              = ReuseFrac(AgLand%iColReuseFrac(indxRegion))
            pAgData(:,indxRegion)%IrigInfilt = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
            pAgData(:,indxRegion)%ReturnFlow = Supply - pAgData(1,indxRegion)%IrigInfilt   !IrigInfilt is the same for all soils in the same subregion
                      
            DO indxSoil=1,SIZE(pAgData , DIM=1)
                ASSOCIATE (pAg => pAgData(indxSoil,indxRegion))
                    !Cycle if area is zero
                    IF (pAg%Area .EQ. 0.0) CYCLE
                    
                    !Soil parameters
                    WiltingPoint  = SubregionSoilsData(indxSoil,indxRegion)%WiltingPoint  * RootDepth
                    FieldCapacity = SubregionSoilsData(indxSoil,indxRegion)%FieldCapacity * RootDepth
                    TotalPorosity = SubregionSoilsData(indxSoil,indxRegion)%TotalPorosity * RootDepth
                    HydCond       = SubregionSoilsData(indxSoil,indxRegion)%HydCond
                    Lambda        = SubregionSoilsData(indxSoil,indxRegion)%Lambda
                    KunsatMethod  = SubregionSoilsData(indxSoil,indxRegion)%KunsatMethod
                    
                    !Moisture related values
                    GM            = GenericMoisture(indxSoil,indxRegion) * RootDepth * DeltaT
                    PrecipD       = Precip(indxSoil,indxRegion) * DeltaT
                    SoilM_P       = pAg%SoilM_Precip_P + pAg%SoilM_AW_P + pAg%SoilM_Oth_P
            
                    !Total inflow to the root zone
                    Inflow = GM + pAg%IrigInfilt
                    
                    !Simulate
                    CALL NonPondedLUMoistureRouter(PrecipD                                 ,  &
                                                   pAg%SMax                                ,  &
                                                   SoilM_P                                 ,  &
                                                   ETc                                     ,  & 
                                                   HydCond                                 ,  & 
                                                   TotalPorosity                           ,  & 
                                                   FieldCapacity                           ,  & 
                                                   WiltingPoint                            ,  &
                                                   Lambda                                  ,  & 
                                                   Inflow                                  ,  &
                                                   SolverData%Tolerance*TotalPorosity      ,  &
                                                   KunsatMethod                            ,  &
                                                   SolverData%IterMax                      ,  &
                                                   SoilM                                   ,  & 
                                                   pAg%Runoff                              ,  & 
                                                   pAg%PrecipInfilt                        ,  & 
                                                   pAg%ETa                                 ,  & 
                                                   pAg%Perc                                ,  & 
                                                   Excess                                  ,  &
                                                   AchievedConv                            ) 
                    
                    !Generate error if convergence is not achieved
                    IF (AchievedConv .NE. 0.0) THEN
                        MessageArray(1) = 'Convergence error in soil moisture routing for agricultural lands!'
                        MessageArray(2) =                   'Soil type            = '//TRIM(IntToText(indxSoil))
                        MessageArray(3) =                   'Subregion            = '//TRIM(IntToText(iSubregionIDs(indxRegion)))
                        WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosity
                        WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                        CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
 
                    !Reduce inflows based on correction for total porosity
                    IF (Excess .NE. 0.0) THEN
                        ratio = [pAg%PrecipInfilt , pAg%IrigInfilt , GM]
                        CALL NormalizeArray(ratio)
                        pAg%Runoff       = pAg%Runoff     + Excess * ratio(1) 
                        pAg%ReturnFlow   = pAg%ReturnFlow + Excess * ratio(2)
                        pAg%GMExcess     = Excess * ratio(3)
                        pAg%PrecipInfilt = PrecipD - pAg%Runoff
                        pAg%IrigInfilt   = Supply  - pAg%ReturnFlow
                    END IF

                    !Compute re-use based on return flow
                    IF (fRF .GT. 0.0) pAg%Reuse = pAg%ReturnFlow * fRU / fRF
          
                    !Compute moisture from precip and irrigation
                    SoilM_P_Array = [pAg%SoilM_Precip_P , pAg%SoilM_AW_P , pAg%SoilM_Oth_P  ]
                    Infilt        = [pAg%PrecipInfilt   , pAg%IrigInfilt , GM - pAg%GMExcess]
                    CALL TrackMoistureDueToSource(SoilM_P_Array   ,  &
                                                  Infilt          ,  &
                                                  pAg%Perc        ,  &
                                                  pAg%ETa         ,  &
                                                  0d0             ,  &
                                                  SoilM_Array     ,  &
                                                  ETPartition     )
                    pAg%SoilM_Precip = SoilM_Array(1)
                    pAg%SoilM_AW     = SoilM_Array(2)
                    pAg%SoilM_Oth    = SoilM_Array(3)
                    pAg%ETP          = ETPartition(1)                
                    pAg%ETAW         = ETPartition(2)
                    pAg%ETOth        = ETPartition(3)                    

                    !Make sure soil moisture is not less than zero
                    lNegativeMoist = .FALSE.
                    IF (pAg%SoilM_Precip .LT. 0.0) lNegativeMoist = .TRUE.
                    IF (pAg%SoilM_AW     .LT. 0.0) lNegativeMoist = .TRUE.
                    IF (pAg%SoilM_Oth    .LT. 0.0) lNegativeMoist = .TRUE.
                    IF (lNegativeMoist) THEN
                        MessageArray(1) = 'Soil moisture content becomes negative at subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//'.'
                        MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
                        MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
                        MessageArray(4) = 'in the Root Zone Main Input File.'
                        CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
 
                END ASSOCIATE
            END DO
        END DO
    
    END ASSOCIATE

 END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE  AG WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(AgLand,iSubregionIDs,DeltaT,Precip,GenericMoisture,SoilsData,ReuseFrac,ReturnFrac,SolverData,iStat)
    CLASS(AgDatabase_v50_Type)        :: AgLand
    INTEGER,INTENT(IN)                :: iSubregionIDs(:)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:,:),GenericMoisture(:,:),ReuseFrac(:),ReturnFrac(:)
    TYPE(RootZoneSoilType),INTENT(IN) :: SoilsData(:,:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'ComputeWaterDemand'
    INTEGER                      :: indxRegion,indxSoil,KunsatMethod
    REAL(8)                      :: RootDepth,ETc,fRF,fRU,WiltingPoint,FieldCapacity,TotalPorosity,Lambda, &
                                    HydCond,GM,PrecipD,SoilM_P,TAW,MinSoilM,TargetSoilM,SoilM,Runoff,      &
                                    PrecipInfilt,ETa,Perc,Excess,AchievedConv,TargetSoilM_Soil
    
    !Initialize
    iStat = 0
        
    ASSOCIATE (pAgData  => AgLand%AgData  , &
               pAvgCrop => AgLand%AvgCrop )
        
        DO indxRegion=1,SIZE(pAgData , DIM=2)
            
            !Cycle if demand is specified
            IF (AgLand%lWaterDemand_Defined) THEN
                IF (AgLand%iColWaterDemand(indxRegion) .GT. 0) CYCLE
            END IF
            
            !Cycle if it is not an irrigation period
            IF (pAvgCrop(indxRegion)%IrigPeriod .EQ. f_iNoIrigPeriod) THEN
                pAgData(:,indxRegion)%DemandRaw = 0.0
                pAgData(:,indxRegion)%Demand    = 0.0
                CYCLE
            END IF
            
            !Initialize subregion-level values
            RootDepth   = pAvgCrop(indxRegion)%RootDepth
            MinSoilM    = pAvgCrop(indxRegion)%MinSoilM
            TargetSoilM = pAvgCrop(indxRegion)%TargetSoilM
            ETc         = pAvgCrop(indxRegion)%ETc * DeltaT
            
            !Return flow and reuse fractions
            fRF = ReturnFrac(AgLand%iColReturnFrac(indxRegion))
            fRU = ReuseFrac(AgLand%iColReuseFrac(indxRegion))
            
            DO indxSoil=1,SIZE(pAgData , DIM=1)
                ASSOCIATE (pAg => pAgData(indxSoil,indxRegion))
                    
                    !Cycle if area is zero
                    IF (pAg%Area .EQ. 0.0) CYCLE
                    
                    !Soil parameters
                    WiltingPoint  = SoilsData(indxSoil,indxRegion)%WiltingPoint  * RootDepth
                    FieldCapacity = SoilsData(indxSoil,indxRegion)%FieldCapacity * RootDepth
                    TAW           = FieldCapacity - WiltingPoint
                    TotalPorosity = SoilsData(indxSoil,indxRegion)%TotalPorosity * RootDepth
                    HydCond       = SoilsData(indxSoil,indxRegion)%HydCond
                    Lambda        = SoilsData(indxSoil,indxRegion)%Lambda
                    KunsatMethod  = SoilsData(indxSoil,indxRegion)%KunsatMethod
                    
                    !Moisture related values
                    GM            = GenericMoisture(indxSoil,indxRegion) * RootDepth * DeltaT
                    PrecipD       = Precip(indxSoil,indxRegion) * DeltaT
                    SoilM_P       = pAg%SoilM_Precip_P + pAg%SoilM_AW_P + pAg%SoilM_Oth_P
                    
                    !Check if there is a need to compute water demand
                    SELECT CASE (AgLand%iDemandFromMoist)
                        CASE (f_iDemandFromMoistAtBegin)
                            IF (SoilM_P  .GE.  MinSoilM*TAW + WiltingPoint) THEN
                                pAg%DemandRaw = 0.0
                                pAg%Demand    = 0.0
                                CYCLE
                            END IF

                        CASE (f_iDemandFromMoistAtEnd)
                            CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                                           pAg%SMax                               ,  &
                                                           SoilM_P                                ,  &
                                                           ETc                                    ,  & 
                                                           HydCond                                ,  & 
                                                           TotalPorosity                          ,  & 
                                                           FieldCapacity                          ,  &
                                                           WiltingPoint                           ,  & 
                                                           Lambda                                 ,  & 
                                                           GM                                     ,  &
                                                           SolverData%Tolerance * TotalPorosity   ,  &
                                                           KunsatMethod                           ,  &
                                                           SolverData%IterMax                     ,  &
                                                           SoilM                                  ,  & 
                                                           Runoff                                 ,  & 
                                                           PrecipInfilt                           ,  & 
                                                           ETa                                    ,  & 
                                                           Perc                                   ,  & 
                                                           Excess                                 ,  &
                                                           AchievedConv                           )
                            !Generate error if convergence is not achieved
                            IF (AchievedConv .NE. 0.0) THEN
                                MessageArray(1) = 'Convergence error in water demand calculations for agricultural crops!'
                                MessageArray(2) =                   'Soil type            = '//TRIM(IntToText(indxSoil))
                                MessageArray(3) =                   'Subregion            = '//TRIM(IntToText(iSubregionIDs(indxRegion)))
                                WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosity
                                WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                                CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                                iStat = -1
                                RETURN
                            END IF
                            !Check if demand calculation is necessary
                            IF (SoilM  .GE.  MinSoilM*TAW + WiltingPoint) THEN
                                pAg%DemandRaw = 0.0
                                pAg%Demand    = 0.0
                                CYCLE
                            END IF
                    END SELECT
                        
                    !Compute demand
                    SoilM            = pAg%SoilM_Precip_P + pAg%SoilM_AW_P + pAg%SoilM_Oth_P
                    TargetSoilM_Soil = MIN(FieldCapacity * TargetSoilM , TotalPorosity)
                    CALL NonPondedCropDemand(PrecipD                               ,  &
                                             pAg%SMax                              ,  &
                                             GM                                    ,  &
                                             fRF                                   ,  &
                                             fRU                                   ,  &
                                             0d0                                   ,  &
                                             ETc                                   ,  &
                                             HydCond                               ,  &
                                             TotalPorosity                         ,  &
                                             FieldCapacity                         ,  &
                                             WiltingPoint                          ,  &      
                                             TargetSoilM_Soil                      ,  & 
                                             Lambda                                ,  & 
                                             SoilM                                 ,  &
                                             SolverData%Tolerance * TotalPorosity  ,  &
                                             KunsatMethod                          ,  &
                                             SolverData%IterMax                    ,  &
                                             pAg%DemandRaw                         ,  &
                                             pAg%Demand                            ,  &
                                             AchievedConv                          )
                        
                    !Generate error if convergence is not achieved
                    IF (AchievedConv .NE. 0.0) THEN
                        MessageArray(1) = 'Convergence error in calculating agricultural water demand '
                        MessageArray(2) = 'for soil type '//TRIM(IntToText(indxSoil))//' in subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//'!'
                        WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosity
                        WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                        CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    
                    !Convert demand related values to volumetric rates and compute subregional values
                    pAg%DemandRaw = pAg%DemandRaw * pAg%Area * DeltaT
                    pAg%Demand    = pAg%Demand * pAg%Area * DeltaT
                    
                END ASSOCIATE
            END DO            
        END DO
    
        !Compute subregional water demands
        AgLand%SubregionalDemand = SUM(pAgData%Demand , DIM=1)
        
    END ASSOCIATE

  END SUBROUTINE ComputeWaterDemand
  
  ! -------------------------------------------------------------
  ! --- CONVERT SOIL INITIAL MOISTURE CONTENTS TO DEPTHS
  ! ---  Note: Called only once at the beginning of simulation
  ! -------------------------------------------------------------
  SUBROUTINE SoilMContent_To_Depth(AgLand,NSoils,NRegions,iSubregionIDs,TotalPorosity,iStat)
    CLASS(AgDatabase_v50_Type) :: AgLand
    INTEGER,INTENT(IN)         :: NSoils,NRegions,iSubregionIDs(NRegions)
    REAL(8),INTENT(IN)         :: TotalPorosity(NSoils,NRegions)
    INTEGER,INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxRegion,indxSoil
    REAL(8)                      :: RootDepth(NRegions)
    
    !Initialize
    iStat = 0
    
    !Return if ag lands are not simulated
    IF (SIZE(AgLand%AgData) .EQ. 0) RETURN
    
    !Initialize
    RootDepth = AgLand%AvgCrop%RootDepth
    
    !Check if initial conditions are greater than total porosity, if not convert contents to depths and equate SoilM_P to SoilM
    ASSOCIATE (pAgData => AgLand%AgData) 
        DO indxRegion=1,NRegions
            DO indxSoil=1,NSoils
                IF ((pAgData(indxSoil,indxRegion)%SoilM_Precip + pAgData(indxSoil,indxRegion)%SoilM_AW + pAgData(indxSoil,indxRegion)%SoilM_Oth) .GT. TotalPorosity(indxSoil,indxRegion)) THEN
                    CALL SetLastMessage('Initial moisture content for agricultural lands with soil type ' // TRIM(IntToText(indxSoil)) // ' at subregion ' // TRIM(IntToText(iSubregionIDs(indxRegion))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                pAgData(indxSoil,indxRegion)%SoilM_Precip   = pAgData(indxSoil,indxRegion)%SoilM_Precip * RootDepth(indxRegion)
                pAgData(indxSoil,indxRegion)%SoilM_AW       = pAgData(indxSoil,indxRegion)%SoilM_AW * RootDepth(indxRegion)
                pAgData(indxSoil,indxRegion)%SoilM_Oth      = pAgData(indxSoil,indxRegion)%SoilM_Oth * RootDepth(indxRegion)
                pAgData(indxSoil,indxRegion)%SoilM_Precip_P = pAgData(indxSoil,indxRegion)%SoilM_Precip
                pAgData(indxSoil,indxRegion)%SoilM_AW_P     = pAgData(indxSoil,indxRegion)%SoilM_AW
                pAgData(indxSoil,indxRegion)%SoilM_Oth_P    = pAgData(indxSoil,indxRegion)%SoilM_Oth
            END DO
        END DO 
    END ASSOCIATE
    
  END SUBROUTINE SoilMContent_To_Depth


  ! -------------------------------------------------------------
  ! --- PREPARE AVERAGE CROP CHARACTERISTICS OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrepareAvgCropOutputFile(IsForInquiry,cFileName,cUnitOutput,UnitT,NRegions,AvgCropOutputFile,iStat)
    LOGICAL,INTENT(IN)          :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cUnitOutput,UnitT
    INTEGER,INTENT(IN)          :: NRegions
    TYPE(GenericFileType)       :: AvgCropOutputFile
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER   :: NColumnsOfData,NRowsOfData,indx,j,indxRegion,indxType,iCol
    CHARACTER :: Text*20,FormatSpec*100,DataUnit(4)*10,DataType(4)*10,CPart(4)*32,FPart(4)*32,TitleLines(1)*350, &
                 Header(3,4*NRegions+1)*23,WorkArray(2)*37,cColUnit(4)*23,HeaderFormat(4)*500
    LOGICAL   :: OverwriteNColumnsOfData,PrintColumnNo
    CHARACTER(LEN=23),PARAMETER :: cColTitle(4) = ['        Root Zone Depth' , '       Min. Soil Moist.' , '   Irrig. Target Moist.' , 'Crop Evapotranspiration']
    
    !Initialize
    iStat = 0
    
    !Open output file
    IF (IsForInquiry) THEN
        CALL AvgCropOutputFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='average crop characteristics output file',iStat=iStat)
        RETURN
    ELSE
        CALL AvgCropOutputFile%New(FileName=cFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='average crop characteristics output file',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !Compile format related data
    NColumnsOfData          = 4 * NRegions
    Text                    = IntToText(NColumnsOfData)
    NRowsOfData             = 1
    OverwriteNColumnsOfData = .FALSE.
    PrintColumnNo           = .FALSE.
    FormatSpec              = '(A21,'//TRIM(Text)//'(2X,F23.3))'
    DataUnit(1)             = ADJUSTL(cUnitOutput)  !Root zone depth
    DataUnit(2)             = ''                    !Minimum moisture 
    DataUnit(3)             = ''                    !Irrigation target moisture
    DataUnit(4)             = ADJUSTL(cUnitOutput)  !ETc in units of length (the rate is implied implicitly since each output will be for a given length of time step)
    !---------------------------------------------------------------------
    DataType(1)             = 'PER-AVER'         !Root zone depth
    DataType(2)             = 'PER-AVER'         !Minimum moisture 
    DataType(3)             = 'PER-AVER'         !Irrigation target moisture
    DataType(4)             = 'PER-CUM'          !ETc in units of length 
    !---------------------------------------------------------------------
    CPart(1)                = ADJUSTL('DEPTH')      !Root zone depth
    CPart(2)                = ADJUSTL('FRACTION')   !Minimum moisture 
    CPart(3)                = ADJUSTL('FRACTION')   !Irrigation target moisture 
    CPart(4)                = ADJUSTL('EVAPOTR')    !ETc in units of length
    !---------------------------------------------------------------------
    FPart(1)                = 'average_crop_root_zone'
    FPart(2)                = 'average_crop_min_soil_moist'
    FPart(3)                = 'average_crop_target_soil_moist'
    FPart(4)                = 'average_crop_ET'

    !Prepare title
    WorkArray(1) = ArrangeText('AVERAGE CROP CHARACTERISTICS',37)
    WorkArray(2) = ArrangeText('FOR EACH SUBREGION',37)
    CALL PrepareTitle(TitleLines(1),WorkArray,39,42)
    
    !Prepare column headers
    Header      = ''
    Header(1,1) = '*              REGION'
    Header(2,1) = '*'
    Header(3,1) = '*        TIME'
    cColUnit(1) = '('//TRIM(cUnitOutput)//')'
    cColUnit(2) = '(dimensionless)'
    cColUnit(3) = '(dimensionless)'
    cColUnit(4) = '('//TRIM(cUnitOutput)//')'
    DO indxRegion=1,NRegions
        DO indxType=1,4
            iCol = 1 + (indxRegion-1)*4 + indxType
            WRITE (Header(1,iCol),'(I23)') indxRegion
            Header(2,iCol) = ADJUSTL(cColTitle(indxType))
            Header(3,iCol) = ADJUSTL(cColUnit(indxType))
        END DO
    END DO
    
    !Prepare column title fomat specs
    HeaderFormat(1) = '(A21,'//TRIM(Text)//'(2X,A23))'
    HeaderFormat(2) = '(A1,20X,'//TRIM(Text)//'(2X,A23))'
    HeaderFormat(3) = '(A13,8X,'//TRIM(Text)//'(2X,A23))'
    
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(AvgCropOutputFile                                , &
                              NColumnsOfData                                   , &
                              NRowsOfData                                      , &
                              OverwriteNColumnsOfData                          , &
                              FormatSpec                                       , &
                              TitleLines                                       , &
                              Header                                           , &
                              HeaderFormat                                     , &
                              PrintColumnNo                                    , &
                              DataUnit                                         , &
                              DataType                                         , &
                              CPart                                            , &
                              FPart                                            , &
                              UnitT                                            , &
                              Subregions=(/((indx,j=1,4),indx=1,NRegions)/)    , &
                              iStat = iStat                                    )

  END SUBROUTINE PrepareAvgCropOutputFile
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(AgLand) 
    CLASS(AgDatabase_v50_Type) :: AgLand
    
    AgLand%ElementalArea_P = AgLand%ElementalArea
    AgLand%AgData%Area_P   = AgLand%AgData%Area
    
  END SUBROUTINE AdvanceAreas
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE AVERAGE CROP CHARACTERISTICS
  ! -------------------------------------------------------------
  SUBROUTINE ComputeAvgCropCharacteristics(NSubregions,ETData,AgLand)
    INTEGER,INTENT(IN)        :: NSubregions
    TYPE(ETType),INTENT(IN)   :: ETData
    TYPE(AgDatabase_v50_Type) :: AgLand
    
    !Local variables
    INTEGER :: indxRegion,indxCrop
    REAL(8) :: ETc(AgLand%NCrops),ModifiedCropFracs(AgLand%NCrops)
    
    ASSOCIATE (pAvgCrop   => AgLand%AvgCrop                 , &
               pCropFracs => AgLand%SubregionalCropAreaFrac )
    
        DO indxRegion=1,NSubregions           
            !Initialize average crop irrigtaion period
            pAvgCrop(indxRegion)%IrigPeriod = f_iNoIrigPeriod
            
            !Update crop area fractions based on only irrigated crops
            DO indxCrop=1,AgLand%NCrops
                IF (AgLand%IrigPeriodFile%iValues(AgLand%iColIrigPeriod(indxCrop,indxRegion)) .EQ. f_iNoIrigPeriod) THEN
                    ModifiedCropFracs(indxCrop) = 0.0
                ELSE
                    ModifiedCropFracs(indxCrop)     = pCropFracs(indxCrop,indxRegion)
                    pAvgCrop(indxRegion)%IrigPeriod = f_iIrigPeriod
                END IF
            END DO
            IF (SUM(ModifiedCropFracs) .GT. 0.0) CALL NormalizeArray(ModifiedCropFracs)

            !Average root depth
            pAvgCrop(indxRegion)%RootDepth = SUM(AgLand%RootDepth * pCropFracs(:,indxRegion))
            
            !Average ETc
            ETc                      = ETData%GetValues(AgLand%iColETcCrop(:,indxRegion))
            pAvgCrop(indxRegion)%ETc = SUM(ETc * pCropFracs(:,indxRegion))
            
            !Average minimum soil moisture (don't include non-irrigated crops in the average)
            pAvgCrop(indxRegion)%MinSoilM = SUM(AgLand%MinSoilMFile%rValues(AgLand%iColMinSoilM(:,indxRegion)) * ModifiedCropFracs)
            
            !Average target soil moisture (don't include non-irrigated crops in the average)
            pAvgCrop(indxRegion)%TargetSoilM = SUM(AgLand%TargetSoilMFile%rValues(AgLand%iColTargetSoilM(:,indxRegion)) * ModifiedCropFracs)
        
        END DO
    END ASSOCIATE
          
  END SUBROUTINE ComputeAvgCropCharacteristics
    
  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFIED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(AgLand,iSubregionIDs,rRegionAreas,TimeStep,iStat)
    CLASS(AgDatabase_v50_Type)    :: AgLand
    INTEGER,INTENT(IN)            :: iSubregionIDs(:)
    REAL(8),INTENT(IN)            :: rRegionAreas(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep 
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    !Rewind files to beginning and read data until specified time stamp
    CALL AgLand%ElemAgAreaDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                                            ;  IF (iStat .NE. 0) RETURN
    CALL AgLand%ElemAgAreaDataFile%ReadTSData('Elemental agricultural areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)   ;  IF (iStat .NE. 0) RETURN

    CALL AgLand%IrigPeriodFile%File%RewindFile_To_BeginningOfTSData(iStat)                                      ;  IF (iStat .NE. 0) RETURN
    CALL TSDataFile_ReadData(TimeStep,'Crop irrigation period data',AgLand%IrigPeriodFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL AgLand%MinSoilMFile%File%RewindFile_To_BeginningOfTSData(iStat)                                                 ;  IF (iStat .NE. 0) RETURN
    CALL TSDataFile_ReadData(TimeStep,'Minimum soil moisture requirement data',AgLand%MinSoilMFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN

    IF (AgLand%TargetSoilMFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL AgLand%TargetSoilMFile%File%RewindFile_To_BeginningOfTSData(iStat)                                               ;  IF (iStat .NE. 0) RETURN
        CALL TSDataFile_ReadData(TimeStep,'Irrigation target soil moisture data',AgLand%TargetSoilMFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    END IF
    
    IF (AgLand%lWaterDemand_Defined) THEN
        CALL AgLand%WaterDemandFile%File%RewindFile_To_BeginningOfTSData(iStat)                                                     ;  IF (iStat .NE. 0) RETURN
        CALL TSDataFile_ReadData(TimeStep,'Agricultural water supply requirement data',AgLand%WaterDemandFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    END IF
    
    CALL AgLand%SubregionCropAreaDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                                                  ;  IF (iStat .NE. 0) RETURN
    CALL AgLand%SubregionCropAreaDataFile%ReadTSData('Subregional agricultural crop areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)  ;  IF (iStat .NE. 0) RETURN
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp

END MODULE