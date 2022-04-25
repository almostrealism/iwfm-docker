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
MODULE Class_NonPondedAgLandUse
  !$ USE OMP_LIB
  USE MessageLogger                , ONLY: SetLastMessage                   , &
                                           LogMessage                       , &
                                           EchoProgress                     , &
                                           MessageArray                     , &
                                           f_iFatal                         , &
                                           f_iInfo    
  USE Class_BaseRootZone           , ONLY: TrackMoistureDueToSource         
  USE Class_GenericLandUse         , ONLY: GenericLandUseType               
  USE Class_LandUseDataFile        , ONLY: LandUseDataFileType              
  USE Class_RootDepthFracDataFile  , ONLY: RootDepthFracDataFileType        , &
                                           RootDepthFracDataFile_New        , &
                                           RootDepthFracDataFile_Kill       , &
                                           RootDepthFracDataFile_ReadTSData 
  USE Package_Discretization       , ONLY: AppGridType
  USE Package_PrecipitationET      , ONLY: ETType
  USE Util_Package_RootZone        , ONLY: WaterSupplyType                  , & 
                                           ReadPointerData                  , &
                                           ReadRealData                     , &
                                           f_iIrigPeriod                    , &
                                           f_iNoIrigPeriod                  , &
                                           f_iDemandFromMoistAtBegin        , &
                                           f_iDemandFromMoistAtEnd          , &
                                           f_iBudgetType_NonPondedCrop_LWU  , & 
                                           f_iBudgetType_NonPondedCrop_RZ 
  USE Util_RootZone_v40            , ONLY: AgRootZoneBudRawFile_New         , &
                                           AgLWUseBudRawFile_New            , &
                                           f_iNAgLWUseBudColumns            , &
                                           f_iNAgRootZoneBudColumns         
  USE Package_UnsatZone            , ONLY: RootZoneSoilType                 , &
                                           NonPondedCropDemand              , &
                                           NonPondedLUMoistureRouter        
  USE Package_Misc                 , ONLY: RealTSDataInFileType             , &
                                           IntTSDataInFileType              , &
                                           SolverDataType                   , & 
                                           ReadTimeSeriesData => ReadTSData , &
                                           f_iFlowDest_GWElement            
  USE Package_Budget               , ONLY: BudgetType                       , &
                                           f_iMaxLocationNameLen               
  USE TimeSeriesUtilities          , ONLY: TimeStepType                     , &
                                           IncrementTimeStamp               , &
                                           CTimeStep_To_RTimeStep           , &
                                           OPERATOR(.TSGT.)                 
  USE GeneralUtilities             , ONLY: StripTextUntilCharacter          , &
                                           IntToText                        , &
                                           UpperCase                        , &
                                           CleanSpecialCharacters           , &
                                           NormalizeArray                   , &
                                           ShellSort                        , &
                                           AllocArray                       , &
                                           LocateInList                     , &
                                           EstablishAbsolutePathFileName    
  USE IOInterface                  , ONLY: GenericFileType                  , &
                                           f_iUNKNOWN
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
  PUBLIC :: NonPondedAgDatabaseType                 


  ! -------------------------------------------------------------
  ! --- NON PONDED AG LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: NonPondedAgType
    INTEGER :: iColIrigPeriod       = 0     !Column number in the irrigation period data file
    INTEGER :: iColMinSoilM         = 0     !Column number in the irrigation trigger minimum soil moisture data file
    INTEGER :: iColTargetSoilM      = 1     !Column number in the irrigtaion target soil moisture data file
    INTEGER :: iColReturnFrac       = 0     !Column number in the return flow fraction data file
    INTEGER :: iColReuseFrac        = 0     !Column number in the re-use fraction data file
    INTEGER :: iColLeachFrac        = 1     !Column number in the minimum perc fraction data file
    REAL(8) :: MinSoilM             = 0.0   !Minimum soil moisture as a fraction of Total Available Water (field capacity - wilting point) to be used for irrigation trigger
    REAL(8) :: ReturnFlow           = 0.0   !Return flow
    REAL(8) :: IrigInfilt           = 0.0   !Infiltration due to irrigation
    REAL(8) :: Reuse                = 0.0   !Reused return flow 
    REAL(8) :: ETAW                 = 0.0   !ET of applied water
    REAL(8) :: ETP                  = 0.0   !ET of precipitation
    REAL(8) :: ETOth                = 0.0   !ET of other sources of moisture
    REAL(8) :: DemandRaw            = 0.0   !Agricultural water demand before the return flow is included
    REAL(8) :: Demand               = 0.0   !Agricultural water demand after the return flow is included 
    REAL(8) :: ElemDemandFrac       = 0.0   !Ratio of crop demand to the total demand at the element it is located at
    REAL(8) :: ElemDemandFrac_Ag    = 0.0   !Ratio of crop demand to the total "ag" demand at the element it is located at
  END TYPE NonPondedAgType
  
  
  ! -------------------------------------------------------------
  ! --- NON PONDED AG LAND DATABASE TYPE
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: LenCropCode = 2  !Length of crop codes
  TYPE NonPondedAgDatabaseType
    INTEGER                                :: NCrops                      = 0                         !Number of non-ponded crops
    INTEGER                                :: iDemandFromMoist            = f_iDemandFromMoistAtBegin !Moisture that will be used to decide when to compute ag water demand
    TYPE(NonPondedAgType),ALLOCATABLE      :: Crops(:,:)                                              !Non-ponded ag land data for each (crop,element) combination
    CHARACTER(LEN=LenCropCode),ALLOCATABLE :: CropCodes(:)                                            !Non-ponded crop codes
    INTEGER                                :: NBudgetCrops                = 0                         !Number of non-ponded crops for budget output
    INTEGER,ALLOCATABLE                    :: iBudgetCrops(:)                                         !Indices of non-ponded crops for budget output
    REAL(8),ALLOCATABLE                    :: MaxRootDepth(:)                                         !Maximum root depth
    INTEGER,ALLOCATABLE                    :: iColRootDepthFrac(:)                                    !Column number in the root depth fractions data file for each (crop)
    REAL(8),ALLOCATABLE                    :: RootDepth_P(:)                                          !Rooting depth used for the previous time step for each (crop)
    REAL(8),ALLOCATABLE                    :: RootDepth(:)                                            !Rooting depth used for the current time step for each (crop)
    INTEGER,ALLOCATABLE                    :: iColAgDemand(:,:)                                       !Pointer to ag water demand file for each (crop,element) combination
    REAL(8),ALLOCATABLE                    :: RegionETPot(:,:)                                        !Regional potential ET for each (crop,region) combination
    TYPE(RootDepthFracDataFileType)        :: RootDepthFracDataFile                                   !Data file for the fraction of max. rooting depth as time-series data 
    TYPE(LandUseDataFileType)              :: LandUseDataFile                                         !Land use data file
    TYPE(RealTSDataInFileType)             :: MinSoilMFile                                            !Irrigation trigger moisture data file
    TYPE(RealTSDataInFileType)             :: TargetSoilMFile                                         !Target soil moisture during irrigation data file 
    TYPE(RealTSDataInFileType)             :: LeachFracFile                                           !Minimum perc fractions data file
    LOGICAL                                :: lLWUseBudRawFile_Defined    = .FALSE.                   !Flag to see if the land and water use file is defined
    TYPE(BudgetType)                       :: LWUseBudRawFile                                         !Raw land and water use budget output file
    LOGICAL                                :: lRootZoneBudRawFile_Defined = .FALSE.                   !Flag to see if the root zone budget file is defined
    TYPE(BudgetType)                       :: RootZoneBudRawFile                                      !Raw root zone budget output file
  CONTAINS
    PROCEDURE,PASS :: New                         
    PROCEDURE,PASS :: Kill 
    PROCEDURE,PASS :: GetMaxAndMinNetReturnFlowFrac
    PROCEDURE,PASS :: GetNCrops                   
    PROCEDURE,PASS :: GetBudget_TSData
    PROCEDURE,PASS :: SetAreas                    
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: PrintResults                
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadTotalElemArea
    PROCEDURE,PASS :: ReadTSData               
    PROCEDURE,PASS :: AdvanceAreas                
    PROCEDURE,PASS :: SoilMContent_To_Depth       
    PROCEDURE,PASS :: ComputeWaterDemand
    PROCEDURE,PASS :: CheckSpecifiedDemandAndArea
    PROCEDURE,PASS :: Simulate 
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp
  END TYPE NonPondedAgDatabaseType
   

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 26
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_NonPondedAgLandUse::'
  
  
  
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
  ! --- NEW NON-PONDED AG LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(AgLand,IsForInquiry,cFileName,cWorkingDirectory,FactCN,AppGrid,iElemIDs,TimeStep,NTimeSteps,cVersion,iStat)
    CLASS(NonPondedAgDatabaseType) :: AgLand
    LOGICAL,INTENT(IN)             :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)    :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)             :: FACTCN
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    INTEGER,INTENT(IN)             :: NTimeSteps,iElemIDs(AppGrid%NElements)
    CHARACTER(LEN=*),INTENT(IN)    :: cVersion
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)                      :: ThisProcedure = ModName // 'New'
    CHARACTER                                        :: ALine*1000,cBudgetCropCode*LenCropCode
    CHARACTER(LEN=f_iMaxLocationNameLen),ALLOCATABLE :: cRegionNames(:)
    CHARACTER(LEN=f_iMaxLocationNameLen)             :: SubRegionNames(AppGrid%NSubregions+1)
    TYPE(GenericFileType)                            :: CropDataFile
    REAL(8)                                          :: FACT,DummyFactor(1),SubRegionArea(AppGrid%NSubregions+1)
    REAL(8),ALLOCATABLE                              :: DummyRealArray(:,:),RegionAreas(:)
    INTEGER                                          :: ErrorCode,indxElem,indxCrop,NCrops,iLen,indxCropP,NBudgetCrops,NElements, &
                                                        NBudgetRegions,NRegions,indxRegion,iElem,ID
    INTEGER,ALLOCATABLE                              :: DummyIntArray(:,:)
    LOGICAL                                          :: lCropFound,TrackTime,lProcessed(AppGrid%NElements)
    CHARACTER(:),ALLOCATABLE                         :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Initialize
    NElements                 = AppGrid%NElements
    NRegions                  = AppGrid%NSubregions
    TrackTime                 = TimeStep%TrackTime
    SubRegionArea(1:NRegions) = AppGrid%GetSubregionArea()
    SubRegionArea(NRegions+1) = SUM(SubRegionArea(1:NRegions))
    SubRegionNames            = ''  ;  SubRegionNames(1:NRegions) = AppGrid%GetSubregionNames()
    SubRegionNames(NRegions+1)= 'ENTIRE MODEL AREA'
    
    !Open file
    CALL CropDataFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Number of crops
    CALL CropDataFile%ReadData(NCrops,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AgLand%NCrops = NCrops
    
    !Moisture to be used for water demand calculations
    CALL CropDataFile%ReadData(AgLand%iDemandFromMoist,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (AgLand%iDemandFromMoist.NE.f_iDemandFromMoistAtBegin  .AND.  &
        AgLand%iDemandFromMoist.NE.f_iDemandFromMoistAtEnd         ) THEN
        MessageArray(1) = 'Flag for soil moisture to be used in the computation of non-ponded '
        MessageArray(2) = 'crop water demand and irrigation timing is not recognized!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Allocate memory
    ALLOCATE (AgLand%Crops(NCrops,NElements)      , &
              AgLand%CropCodes(NCrops)            , &
              AgLand%MaxRootDepth(NCrops)         , &
              AgLand%iColRootDepthFrac(NCrops)    , &
              AgLand%RootDepth_P(NCrops)          , &
              AgLand%RootDepth(NCrops)            , &
              AgLand%RegionETPot(NCrops,NRegions) , &
              STAT=ErrorCode                      )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for non-ponded agricultural data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read crop codes
    AgLand%CropCodes = ''
    DO indxCrop=1,NCrops
      CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
      ALine = StripTextUntilCharacter(ALine,'/') 
      CALL CleanSpecialCharacters(ALine)
      ALine = ADJUSTL(ALine)
      iLen = LEN_TRIM(ALine)
      IF (iLen .EQ. 1) THEN
        AgLand%CropCodes(indxCrop)(2:2) = TRIM(ALine)
      ELSEIF (iLen .EQ. 2) THEN
        AgLand%CropCodes(indxCrop) = TRIM(Aline)
      ELSE
        CALL SetLastMessage('Too many or too few characters for crop code of crop ' // TRIM(IntToText(indxCrop)) // '!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
      !Make sure that crop code is not defined twice
      DO indxCropP=1,indxCrop-1
        IF (AgLand%CropCodes(indxCrop) .EQ. AgLand%CropCodes(indxCropP)) THEN
          CALL SetLastMessage('Crop code '//TRIM(AgLand%CropCodes(indxCrop))//' is defined twice!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
        END IF
      END DO
    END DO
    
    !Land use data file
    CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AgLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Non-ponded ag. area file',NElements,NCrops,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Crops for budget output
    CALL CropDataFile%ReadData(NBudgetCrops,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AgLand%NBudgetCrops = NBudgetCrops
    IF (NBudgetCrops .GT. 0) THEN
      !Number of budget regions
      NBudgetRegions = (AppGrid%NSubregions+1) * NBudgetCrops
      
      !Allocate memory
      ALLOCATE (AgLand%iBudgetCrops(NBudgetCrops)   ,  &
                cRegionNames(NBudgetRegions)        ,  &
                RegionAreas(NBudgetRegions)         ,  &
                STAT=ErrorCode                      )
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for non-ponded crops budget output data!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Find the indices for budget-print-out crops
      DO indxCrop=1,NBudgetCrops
        CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  cBudgetCropCode = ADJUSTL(StripTextUntilCharacter(ALine,'/'))  ;  CALL CleanSpecialCharacters(cBudgetCropCode)
        lCropFound = .FALSE.
        DO indxCropP=1,NCrops
          IF (UpperCase(cBudgetCropCode) .EQ. UpperCase(AgLand%CropCodes(indxCropP))) THEN
            AgLand%iBudgetCrops(indxCrop) = indxCropP
            lCropFound                    = .TRUE.
            EXIT
          END IF
        END DO
        IF (.NOT. lCropFound) THEN
            CALL SetLastMessage (TRIM(cBudgetCropCode)//' for water budget output is not defined as a non-ponded crop!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
      END DO
      
      !Order the crop indicies for budget output
      CALL ShellSort(AgLand%iBudgetCrops)
      
      !Region names and areas
      DO indxCrop=1,NBudgetCrops
        indxCropP = AgLand%iBudgetCrops(indxCrop)
        DO indxRegion=1,NRegions+1
          cRegionNames((indxRegion-1)*NBudgetCrops+indxCrop) = TRIM(SubRegionNames(indxRegion)) // '_' // TRIM(UpperCase(AgLand%CropCodes(indxCropP)))
          RegionAreas((indxRegion-1)*NBudgetCrops+indxCrop)  = SubregionArea(indxRegion)
        END DO
      END DO
      
    END IF
    
    !Land and water use budget
    CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (NBudgetCrops .GT. 0) THEN
      IF (ALine .NE. '') THEN
          CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
          CALL AgLWUseBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'land and water use budget for specific non-ponded crops',cVersion,AgLand%LWUseBudRawFile,iStat)
          IF (iStat .EQ. -1) RETURN
          AgLand%lLWUseBudRawFile_Defined = .TRUE.
      END IF
    END IF
    
    !Root zone budget
    CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (NBudgetCrops .GT. 0) THEN
      IF (ALine .NE. '') THEN
          CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
          CALL AgRootZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'root zone budget for specific non-ponded crops',cVersion,AgLand%RootZoneBudRawFile,iStat)
          IF (iStat .EQ. -1) RETURN
          AgLand%lRootZoneBudRawFile_Defined = .TRUE.
      END IF
    END IF

    !Rooting depths
    CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL RootDepthFracDataFile_New(cAbsPathFileName,cWorkingDirectory,TrackTime,AgLand%RootDepthFracDataFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CropDataFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL AllocArray(DummyRealArray,NCrops,3,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CropDataFile%ReadData(DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    AgLand%MaxRootDepth      = DummyRealArray(:,2)*FACT
    AgLand%iColRootDepthFrac = INT(DummyRealArray(:,3))
    
    !Make sure there are enough data columns in the root depth fractions data file
    CALL AgLand%RootDepthFracDataFile%CheckColNum('root depth fractions data file',AgLand%iColRootDepthFrac,.TRUE.,iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !Curve numbers
    CALL ReadRealData(CropDataFile,'curve numbers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyRealArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Curve numbers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)          = .TRUE.
        AgLand%Crops(:,iElem)%SMax = (1000.0/DummyRealArray(indxElem,2:)-10.0) * FACTCN
    END DO
      
    !ETc pointers
    CALL ReadPointerData(CropDataFile,'evapotranspiration column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Evapotranspration column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)             = .TRUE.
        AgLand%Crops(:,iElem)%iColETc = DummyIntArray(indxElem,2:)    
    END DO

    !Agricultural water demand pointers
    ALLOCATE (AgLand%iColAgDemand(NCrops,NElements))
    CALL ReadPointerData(CropDataFile,'water supply requirement column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Water supply requirement column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)            = .TRUE.
        AgLand%iColAgDemand(:,iElem) = DummyIntArray(indxElem,2:)
    END DO

    !Irrigation period pointers
    CALL ReadPointerData(CropDataFile,'irrigation period column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Irrigation period column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                    = .TRUE.
        AgLand%Crops(:,iElem)%iColIrigPeriod = DummyIntArray(indxElem,2:)
    END DO
    IF (MINVAL(AgLand%Crops%iColIrigPeriod) .LT. 1) THEN
        MessageArray(1) = 'Irrigation period column number for non-ponded crops' 
        MessageArray(2) = 'cannot be less than 1!' 
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
       
    !Minimum soil moisture pointers
    CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL AgLand%MinSoilMFile%Init(cAbsPathFileName,cWorkingDirectory,'Irrigation trigger soil moisture data file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadPointerData(CropDataFile,'minimum soil moisture column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Minimum soil moisture column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                  = .TRUE.
        AgLand%Crops(:,iElem)%iColMinSoilM = DummyIntArray(indxElem,2:) 
    END DO
    CALL AgLand%MinSoilMFile%CheckColNum('minimum soil moisture data file',PACK(AgLand%Crops%iColMinSoilM,MASK=.TRUE.),.TRUE.,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Target soil moisture for irrigation pointers
    CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL AgLand%TargetSoilMFile%Init(cAbsPathFileName,cWorkingDirectory,'irrigation target soil moisture file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ReadPointerData(CropDataFile,'irrigation target soil moisture column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        lProcessed = .FALSE.
        DO indxElem=1,NElements
            iElem = DummyIntArray(indxElem,1)
            IF (lProcessed(iElem)) THEN
                ID = iElemIDs(iElem)
                CALL SetLastMessage('Irrigation target soil moisture column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iElem)                     = .TRUE.
            AgLand%Crops(:,iElem)%iColTargetSoilM = DummyIntArray(indxElem,2:) 
        END DO
        CALL AgLand%TargetSoilMFile%CheckColNum('irrigation target soil moisture file',PACK(AgLand%Crops%iColTargetSoilM,MASK=.TRUE.),.TRUE.,iStat)
        IF (iStat .EQ. -1) RETURN
    ELSE
        ALLOCATE (AgLand%TargetSoilMFile%rValues(1))
        AgLand%TargetSoilMFile%rValues(1) = 1.0
        AgLand%TargetSoilMFile%iSize      = 1
    END IF 

    !Irrigation water return flow fraction pointers
    CALL ReadPointerData(CropDataFile,'return flow fraction column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Return flow fraction column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                    = .TRUE.
        AgLand%Crops(:,iElem)%iColReturnFrac = DummyIntArray(indxElem,2:) 
    END DO
    
    !Irrigation water re-use factor pointers
    CALL ReadPointerData(CropDataFile,'re-use fraction column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Re-use flow fraction column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                   = .TRUE.
        AgLand%Crops(:,iElem)%iColReuseFrac = DummyIntArray(indxElem,2:) 
    END DO
    
    !Minimum perc fractions column pointers
    CALL CropDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL AgLand%LeachFracFile%Init(cAbsPathFileName,cWorkingDirectory,'leaching factors file',TrackTime,1,.FALSE.,DummyFactor,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ReadPointerData(CropDataFile,'minimum percolation column pointers for non-ponded crops','elements',NElements,NCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        lProcessed = .FALSE.
        DO indxElem=1,NElements
            iElem = DummyIntArray(indxElem,1)
            IF (lProcessed(iElem)) THEN
                ID = iElemIDs(iElem)
                CALL SetLastMessage('Minimum percolation column pointers for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iElem)                   = .TRUE.
            AgLand%Crops(:,iElem)%iColLeachFrac = DummyIntArray(indxElem,2:) 
        END DO
    ELSE
        ALLOCATE (AgLand%LeachFracFile%rValues(1))
        AgLand%LeachFracFile%rValues(1) = 0.0
        AgLand%LeachFracFile%iSize      = 1
    END IF 
    IF (MAXVAL(AgLand%Crops%iColLeachFrac) .GT. AgLand%LeachFracFile%iSize) THEN
        MessageArray(1) = 'Minimum percolation column number for a non-ponded crop is larger'
        MessageArray(2) = 'than the available columns in the minimum percolation file!' 
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (MINVAL(AgLand%Crops%iColLeachFrac) .LT. 1) THEN
        MessageArray(1) = 'Minimum percolation column number for non-ponded crops' 
        MessageArray(2) = 'cannot be less than 1!' 
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initial conditions
    CALL ReadRealData(CropDataFile,'initial conditions for non-ponded crops','elements',NElements,NCrops+2,iElemIDs,DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(DummyRealArray(:,2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyRealArray(:,2)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some fractions of initial soil moisture due to precipitation is less '
        MessageArray(2) = 'than 0.0 or greater than 1.0 for non-ponded agricultural crops!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF 
    IF (MINVAL(DummyRealArray(:,3:)) .LT. 0.0   .OR.  &
        MAXVAL(DummyRealArray(:,3:)) .GT. 1.0          ) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 or greater than 1.0 for non-ponded agricultural crops!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyRealArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL setLastMessage('Initial conditions for non-ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                  = .TRUE.
        AgLand%Crops(:,iElem)%SoilM_Precip = DummyRealArray(indxElem,2) * DummyRealArray(indxElem,3:)
        AgLand%Crops(:,iElem)%SoilM_AW     = DummyRealArray(indxElem,3:) - AgLand%Crops(:,iElem)%SoilM_Precip
    END DO
    AgLand%Crops%SoilM_Precip_P_BeforeUpdate = AgLand%Crops%SoilM_Precip 
    AgLand%Crops%SoilM_Precip_P              = AgLand%Crops%SoilM_Precip 
    AgLand%Crops%SoilM_AW_P_BeforeUpdate     = AgLand%Crops%SoilM_AW
    AgLand%Crops%SoilM_AW_P                  = AgLand%Crops%SoilM_AW
    
    !Close file
    CALL CropDataFile%Kill()
    
    !Clear memory
    DEALLOCATE (DummyRealArray , DummyIntArray , cRegionNames , RegionAreas , STAT=ErrorCode)
    
    
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
  ! --- KILL NON-PONDED AG LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AgLand)
    CLASS(NonPondedAgDatabaseType) :: AgLand
    
    !Local variables
    INTEGER                       :: ErrorCode
    TYPE(NonPondedAgDatabaseType) :: Dummy
    
    !Deallocate arrays
    DEALLOCATE (AgLand%Crops             , &
                AgLand%CropCodes         , &
                AgLand%iBudgetCrops      , &
                AgLand%MaxRootDepth      , &
                AgLand%iColRootDepthFrac , &
                AgLand%RootDepth_P       , &
                AgLand%RootDepth         , &
                AgLand%iColAgDemand      , &
                STAT = ErrorCode         )
    
    !Close files
    CALL RootDepthFracDataFile_Kill(AgLand%RootDepthFracDataFile)
    CALL AgLand%LandUseDataFile%Kill()
    CALL AgLand%MinSoilMFile%Close()
    CALL AgLand%TargetSoilMFile%Close()
    CALL AgLand%LeachFracFile%Close()
    CALL AgLand%LWUseBudRawFile%Kill()
    CALL AgLand%RootZoneBudRawFile%Kill()
    
    !Assign default values to components
    SELECT TYPE (AgLand)
        TYPE IS (NonPondedAgDatabaseType)
            AgLand = Dummy
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
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(AgLand,iBudgetType,iLocationIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(NonPondedAgDatabaseType),TARGET,INTENT(IN) :: AgLand
    INTEGER,INTENT(IN)                               :: iBudgetType,iLocationIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)                      :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                               :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                              :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                              :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER :: indx
    TYPE(BudgetTYpe),POINTER :: pBudget
    
    !Initialize
    NULLIFY(pBudget)
    
    !Pointer to budget file
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_NonPondedCrop_LWU)
            IF (AgLand%lLWUseBudRawFile_Defined) pBudget => AgLand%LWUseBudRawFile
            
        CASE (f_iBudgetType_NonPondedCrop_RZ)
            IF (AgLand%lRootZoneBudRawFile_Defined) pBudget => AgLand%RootZoneBudRawFile
            
    END SELECT
        
    !Return if Budget file is not defined
    IF (.NOT. ASSOCIATED(pBudget)) THEN
        iStat          = 0
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
        RETURN
    END IF
    
    !Retrieve data
    DO indx=1,SIZE(iCols)
        CALL pBudget%ReadData(iLocationIndex,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
        IF (iStat .NE. 0) EXIT
    END DO
    
    !Clear memory
    NULLIFY(pBudget)
    
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET MIN AND MAX NET RETURN FLOW FRACTIONS THROUGH THE ENTITE SIMULATION PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE GetMaxAndMinNetReturnFlowFrac(AgLand,ReturnFracFile,ReuseFracFile,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    CLASS(NonPondedAgDatabaseType),INTENT(IN) :: AgLand
    TYPE(RealTSDataInFileType)                :: ReturnFracFile,ReuseFracFile
    TYPE(TimeStepType),INTENT(IN)             :: FirstTimeStep
    REAL(8),INTENT(OUT)                       :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT)                       :: iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    INTEGER            :: FileReadCode_Return,FileReadCode_Reuse,indxElem,indxCrop
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
            DO indxElem=1,SIZE(AgLand%Crops,DIM=2)
                DO indxCrop=1,SIZE(AgLand%Crops,DIM=1)
                    rRT      = ReturnFracFile%rValues(AgLand%Crops(indxCrop,indxElem)%iColReturnFrac)
                    rRU      = ReuseFracFile%rValues(AgLand%Crops(indxCrop,indxElem)%iColReuseFrac)
                    rMaxFrac = MAX(rMaxFrac , rRT-rRU)
                    rMinFrac = MIN(rMinFrac , rRT-rRU)
                END DO
            END DO
        END IF
        
        !Advance time
        TimeStep%CurrentDateAndTime = IncrementTimeStamp(TimeStep%CurrentDateAndTime,TimeStep%DELTAT_InMinutes)
        
        !Exit if past the simulation end date
        IF (TimeStep%CurrentDateAndTime .TSGT. TimeStep%EndDateAndTime) EXIT

    END DO
      
  END SUBROUTINE GetMaxAndMinNetReturnFlowFrac
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF CROPS
  ! -------------------------------------------------------------
  FUNCTION GetNCrops(AgLand) RESULT(NCrops)
    CLASS(NonPondedAgDatabaseType),INTENT(IN) :: AgLand
    INTEGER                                   :: NCrops
    
    NCrops = AgLand%NCrops
    
  END FUNCTION GetNCrops
  
  
  
  
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
  ! --- SET THE CROP AREAS
  ! -------------------------------------------------------------
  SUBROUTINE SetAreas(AgLand,Area)
    CLASS(NonPondedAgDatabaseType) :: AgLand
    REAL(8),INTENT(IN)             :: Area(:,:)
   
    AgLand%Crops%Area = Area
    
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
  SUBROUTINE ReadRestartData(NonPondedAg,InFile,iStat)
    CLASS(NonPondedAgDatabaseType) :: NonPondedAg
    TYPE(GenericFileType)          :: InFile
    INTEGER,INTENT(OUT)            :: iStat
    
    CALL InFile%ReadData(NonPondedAg%RootDepth_P,iStat)           ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%RootDepth,iStat)             ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%ReturnFlow,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NonPondedAg%Crops%SoilM_Oth,iStat)  
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR NON-PONDED AG
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(AgLand,TimeStep,AppGrid,IrigPeriodFile,iElemIDs,rElemAreas,WiltingPoint,FieldCapacity,iStat)
    CLASS(NonPondedAgDataBaseType)       :: AgLand
    TYPE(TimeStepType),INTENT(IN)        :: TimeStep
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(IntTSDataInFileType),INTENT(IN) :: IrigPeriodFile
    INTEGER,INTENT(IN)                   :: iElemIDs(AppGrid%NElements)
    REAL(8),INTENT(IN)                   :: rElemAreas(AppGrid%NElements),WiltingPoint(:),FieldCapacity(:)
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReadTSData'
    INTEGER                      :: indxElem,indxCrop,FileReadCode,NElements,NCrops,iIrigPeriod
    REAL(8)                      :: TargetSoilM,MinSoilM,TAW,rFrac,WP,FC
    LOGICAL                      :: lMinSoilM_Updated,lTargetSoilM_Updated
    
    !Initialzie
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Reading time series data for non-ponded agricultural crops')
    
    !Initialize
    NElements = AppGrid%NElements
    NCrops    = AgLand%NCrops
    !$ CALL OMP_SET_NUM_THREADS(OMP_GET_NUM_PROCS()-1)
    
    !Read root depth fractions data first, since a lot of stuff depends on them
    CALL RootDepthFracDataFile_ReadTSData(AgLand%RootDepthFracDataFile,TimeStep,iStat)  
    IF (iStat .EQ. -1) RETURN
    IF (AgLand%RootDepthFracDataFile%lUpdated) THEN
        AgLand%RootDepth = AgLand%MaxRootDepth * AgLand%RootDepthFracDataFile%rValues(AgLand%iColRootDepthFrac)
        WHERE (AgLand%RootDepth .EQ. 0.0) AgLand%RootDepth = AgLand%MaxRootDepth
        IF (TimeStep%CurrentTimeStep .EQ. 1) AgLand%RootDepth_P = AgLand%RootDepth
    END IF
    
    !Land use areas
    CALL AgLand%LandUseDataFile%ReadTSData('Non-ponded crop areas',TimeStep,rElemAreas,iElemIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (AgLand%LandUseDataFile%lUpdated) THEN
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            AgLand%Crops(:,indxElem)%Area = AgLand%LandUseDataFile%rValues(indxElem,2:)
        END DO
        !$OMP END PARALLEL DO
    END IF
       
    !Min. soil moisture data as irrigation trigger
    CALL ReadTimeSeriesData(TimeStep,'Minimum soil moisture requirement data',AgLand%MinSoilMFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lMinSoilM_Updated = AgLand%MinSoilMFile%lUpdated
    IF (lMinSoilM_Updated) THEN
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            AgLand%Crops(:,indxElem)%MinSoilM = AgLand%MinSoilMFile%rValues(AgLand%Crops(:,indxElem)%iColMinSoilM)
        END DO
        !$OMP END PARALLEL DO
    END IF
    
    !Irrigation target soil moisture data
    CALL ReadTimeSeriesData(TimeStep,'Irrigation target soil moisture data',AgLand%TargetSoilMFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lTargetSoilM_Updated = AgLand%TargetSoilMFile%lUpdated
    
    !Leaching factors
    CALL ReadTimeSeriesData(TimeStep,'Leaching factor data',AgLand%LeachFracFile,FileReadCode,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that irrigation target soil moisture is not less than minimum soil moisture
    IF (lMinSoilM_Updated .OR. lTargetSoilM_Updated) THEN
        DO indxElem=1,NElements
            WP  = WiltingPoint(indxElem)
            TAW = FieldCapacity(indxElem) - WP
            DO indxCrop=1,NCrops
                TargetSoilM = AgLand%TargetSoilMFile%rValues(AgLand%Crops(indxCrop,indxElem)%iColTargetSoilM)
                rFrac       = AgLand%MinSoilMFile%rValues(AgLand%Crops(indxCrop,indxElem)%iColMinSoilM)
                MinSoilM    = WP + rFrac * TAW
                IF (TargetSoilM .LT. MinSoilM) THEN
                    MessageArray(1) = 'Irrigation target soil moisture for ' // TRIM(AgLand%CropCodes(indxCrop)) // ' is less than minimum '
                    MessageArray(2) = 'soil moisture at element ' // TRIM(IntToText(iElemIDs(indxElem))) // '!'
                    CALL SetLastMessage(MessageArray(:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF 
            END DO
        END DO
    END IF
    
    !Warn if deficit irrigation
    IF (lMinSoilM_Updated) THEN
      DO indxElem=1,NElements
          WP  = WiltingPoint(indxElem)
          FC  = FieldCapacity(indxElem)
          TAW = FC - WP
          DO indxCrop=1,NCrops
              rFrac = AgLand%Crops(indxCrop,indxElem)%MinSoilM
              IF (WP+rFrac*TAW .LT. 0.5D0*FC) THEN
                  MessageArray(1) = 'Deficit irrigation is being simulated for crop ' // TRIM(AgLand%CropCodes(indxCrop)) // ' in element '//TRIM(IntToText(iElemIDs(indxElem)))//'!'
                  WRITE (MessageArray(2),'(A,F6.3)') 'Irrigation trigger minimum moisture = ' , WP + rFrac*TAW
                  WRITE (MessageArray(3),'(A,F6.3)') 'Half of field capacity              = ' , 0.5D0 * FC
                  CALL LogMessage(MessageArray(1:3),f_iInfo,ThisProcedure)
              END IF
          END DO
      END DO
    END IF
    
    !Make sure root depth is not zero if it is irrigation period
    IF (IrigPeriodFile%lUpdated) THEN
      DO indxElem=1,NElements
        DO indxCrop=1,NCrops
          iIrigPeriod = IrigPeriodFile%iValues(AgLand%Crops(indxCrop,indxElem)%iColIrigPeriod)
          IF (iIrigPeriod .EQ. f_iIrigPeriod) THEN
            IF (AgLand%RootDepth(indxCrop) .EQ. 0.0) THEN
                CALL SetLastMessage('Rooting depth for ' // TRIM(AgLand%CropCodes(indxCrop)) // ' cannot be zero during irrigation period!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
          END IF
        END DO
      END DO
    END IF
        
  END SUBROUTINE ReadTSData
  

  ! -------------------------------------------------------------
  ! --- READ TOTAL NON-PONDED AG AREA AT AN ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE ReadTotalElemArea(AgLand,iElem,lForInquiry,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemAgLandUse,rOutputDates,iStat)
    CLASS(NonPondedAgDataBaseType) :: AgLand
    INTEGER,INTENT(IN)             :: iElem
    LOGICAL,INTENT(IN)             :: lForInquiry
    CHARACTER(LEN=*),INTENT(IN)    :: cReadBeginDateAndTime,cReadEndDateAndTime
    INTEGER,INTENT(OUT)            :: nActualOutput,iStat
    REAL(8),INTENT(OUT)            :: ElemAgLandUse(:),rOutputDates(:)   
    
    !Local variables
    INTEGER :: iPathNameIndex,indxCrop,iOffset,FileReadCode,iColumn
    REAL(8) :: rData(SIZE(rOutputDates))
    
    !Initialize
    ElemAgLandUse = 0    
    iOffset       = (iElem-1) * AgLand%NCrops
    
    !Read and accumulate total ag lands
    DO indxCrop=1,AgLand%NCrops
        iPathNameIndex = iOffset + indxCrop
        iColumn        = indxCrop + 1 !+1 because first column in ASCII file is reserved for element number
        CALL AgLand%LandUseDataFile%ReadData(iElem,iColumn,iPathNameIndex,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,rData,rOutputDates,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL AgLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .EQ. -1) RETURN
        ElemAgLandUse(1:nActualOutput) = ElemAgLandUse(1:nActualOutput) + rData(1:nActualOutput)
    END DO
    
    !Unit conversion
    ElemAgLandUse(1:nActualOutput) = ElemAgLandUse(1:nActualOutput) * AgLand%LandUseDataFile%Fact
    
    !Rewind land use file if it was opened for querying
    IF (lForInquiry) CALL AgLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)     
    
  END SUBROUTINE ReadTotalElemArea
  
  

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
  SUBROUTINE PrintRestartData(NonPondedAg,OutFile)
    CLASS(NonPondedAgDatabaseType),INTENT(IN) :: NonPondedAg
    TYPE(GenericFileType)                     :: OutFile
    
    CALL OutFile%WriteData(NonPondedAg%RootDepth_P)
    CALL OutFile%WriteData(NonPondedAg%RootDepth)
    CALL OutFile%WriteData(NonPondedAg%Crops%Runoff)
    CALL OutFile%WriteData(NonPondedAg%Crops%ReturnFlow)
    CALL OutFile%WriteData(NonPondedAg%Crops%Area_P)
    CALL OutFile%WriteData(NonPondedAg%Crops%Area)
    CALL OutFile%WriteData(NonPondedAg%Crops%SoilM_Precip_P)
    CALL OutFile%WriteData(NonPondedAg%Crops%SoilM_Precip)
    CALL OutFile%WriteData(NonPondedAg%Crops%SoilM_AW_P)
    CALL OutFile%WriteData(NonPondedAg%Crops%SoilM_AW)
    CALL OutFile%WriteData(NonPondedAg%Crops%SoilM_Oth_P)
    CALL OutFile%WriteData(NonPondedAg%Crops%SoilM_Oth)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE FOR RESULTS PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(NonPondedAg,AppGrid,ElemSupply,ElemPrecip,ElemGenericMoist)
    CLASS(NonPondedAgDatabaseType)   :: NonPondedAg
    TYPE(AppGridType),INTENT(IN)     :: AppGrid
    TYPE(WaterSupplyType),INTENT(IN) :: ElemSupply(AppGrid%NElements)
    REAL(8),INTENT(IN)               :: ElemPrecip(AppGrid%NElements),ElemGenericMoist(AppGrid%NElements)
    
    !Local variables
    INTEGER                                                             :: NBudgetCrops,indxLast,indxCrop,indxElem,iCrop
    REAL(8),DIMENSION(AppGrid%NElements)                                :: ElemValue
    REAL(8),DIMENSION(AppGrid%NElements,NonPondedAg%NBudgetCrops)       :: DemandFrac,Area
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*NonPondedAg%NBudgetCrops) :: RPump,RDeli,RRunoff,RLUArea,RGenMoistInflow
    
    !Return if no output is desired
    IF (NonPondedAg%NBudgetCrops .EQ. 0) RETURN
    
    !Initialize
    NBudgetCrops    = NonPondedAg%NBudgetCrops
    indxLast        = NBudgetCrops * AppGrid%NSubregions

    !Compute variables necessary for both land&water use and root zone budget files
    IF (NonPondedAg%lLWUseBudRawFile_Defined .OR. NonPondedAg%lRootZoneBudRawFile_Defined) THEN
      DO indxElem=1,AppGrid%NElements
        DO indxCrop=1,NBudgetCrops 
          DemandFrac(indxElem,indxCrop) = NonPondedAg%Crops(NonPondedAg%iBudgetCrops(indxCrop),indxElem)%ElemDemandFrac_Ag
          Area(indxElem,indxCrop)       = NonPondedAg%Crops(NonPondedAg%iBudgetCrops(indxCrop),indxElem)%Area
        END DO
      END DO
      DO indxCrop=1,NBudgetCrops
        iCrop                                           = NonPondedAg%iBudgetCrops(indxCrop)
        ElemValue                                       = ElemSupply%Pumping_Ag * DemandFrac(:,indxCrop)
        RPump(indxCrop:indxLast:NBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RPump(indxLast+indxCrop)                        = SUM(RPump(indxCrop:indxLast:NBudgetCrops))
        ElemValue                                       = ElemSupply%Diversion_Ag * DemandFrac(:,indxCrop)
        RDeli(indxCrop:indxLast:NBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RDeli(indxLast+indxCrop)                        = SUM(RDeli(indxCrop:indxLast:NBudgetCrops))
        ElemValue                                       = (ElemGenericMoist * NonPondedAg%RootDepth(iCrop) - NonPondedAg%Crops(indxCrop,:)%GMExcess) * Area(:,indxCrop)
        RGenMoistInflow(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RGenMoistInflow(indxLast+indxCrop)              = SUM(RGenMoistInflow(indxCrop:indxLast:NBudgetCrops))
        ElemValue                                       = ElemSupply%UpstrmRunoff * DemandFrac(:,indxCrop)
        RRunoff(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RRunoff(indxLast+indxCrop)                      = SUM(RRunoff(indxCrop:indxLast:NBudgetCrops))
        RLUArea(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(Area(:,indxCrop))
        RLUArea(indxLast+indxCrop)                      = SUM(RLUArea(indxCrop:indxLast:NBudgetCrops))
      END DO
    END IF

    IF (NonPondedAg%lLWUseBudRawFile_Defined)    CALL WriteLWUseFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RRunoff,NonPondedAg)
    IF (NonPondedAg%lRootZoneBudRawFile_Defined) CALL WriteRootZoneFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RGenMoistInflow,RRunoff,ElemPrecip,Area,NonPondedAg)
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT LAND AND WATER USE BUDGET RAW DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RUpstrmElemRunoff,NonPondedAg)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RLUArea,RPump,RDeli,RUpstrmElemRunoff
    TYPE(NonPondedAgDatabaseType)   :: NonPondedAg
    
    !Local variables
    INTEGER                                                             :: indxCrop,iBudgetCrops(NonPondedAg%NBudgetCrops),indxLast,NBudgetCrops
    REAL(8),DIMENSION(AppGrid%NElements)                                :: ElemValue
    REAL(8)                                                             :: DummyArray(f_iNAgLWUseBudColumns,(AppGrid%NSubregions+1)*NonPondedAg%NBudgetCrops)
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*NonPondedAg%NBudgetCrops) :: RDemandRaw,RDemand,RDemandShort,RETAW,RETP,RETOth

    !Initialize
    NBudgetCrops = NonPondedAg%NBudgetCrops
    iBudgetCrops = NonPondedAg%iBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    
    !Compute budget terms
    DO indxCrop=1,NBudgetCrops
      ElemValue                                  = NonPondedAg%Crops(iBudgetCrops(indxCrop),:)%DemandRaw
      RDemandRaw(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDemandRaw(indxLast+indxCrop)              = SUM(RDemandRaw(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = NonPondedAg%Crops(iBudgetCrops(indxCrop),:)%Demand
      RDemand(indxCrop:indxLast:NBudgetCrops)    = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDemand(indxLast+indxCrop)                 = SUM(RDemand(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = NonPondedAg%Crops(iBudgetCrops(indxCrop),:)%ETAW
      RETAW(indxCrop:indxLast:NBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETAW(indxLast+indxCrop)                   = SUM(RETAW(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = NonPondedAg%Crops(iBudgetCrops(indxCrop),:)%ETP
      RETP(indxCrop:indxLast:NBudgetCrops)       = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETP(indxLast+indxCrop)                    = SUM(RETP(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = NonPondedAg%Crops(iBudgetCrops(indxCrop),:)%ETOth
      RETOth(indxCrop:indxLast:NBudgetCrops)     = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETOth(indxLast+indxCrop)                  = SUM(RETOth(indxCrop:indxLast:NBudgetCrops))
    END DO
    RDemandShort = RDemand - RPump - RDeli - RUpstrmElemRunoff
    
    !Store in temporary array
    DummyArray(1,:)  = RLUArea
    DummyArray(2,:)  = RDemandRaw
    DummyArray(3,:)  = RDemand
    DummyArray(4,:)  = RPump
    DummyArray(5,:)  = RDeli
    DummyArray(6,:)  = RUpstrmElemRunoff
    DummyArray(7,:)  = RDemandShort
    DummyArray(8,:)  = RETAW
    DummyArray(9,:)  = RETP
    DummyArray(10,:) = RETOth

    !Print out values to binary file
    CALL NonPondedAg%LWUseBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteLWUseFlowsToBudRawFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT ROOT ZONE BUDGET RAW DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RGenMoistInflow,RUpstrmElemRunoff,ElemPrecip,Area,NonPondedAg)
   TYPE(AppGridType),INTENT(IN)    :: AppGrid
   REAL(8),DIMENSION(:),INTENT(IN) :: RPump,RDeli,RGenMoistInflow,RUpstrmElemRunoff,RLUArea,ElemPrecip
   TYPE(NonPondedAgDatabaseType)   :: NonPondedAg
   REAL(8),INTENT(IN)              :: Area(AppGrid%NElements,NonPondedAg%NBudgetCrops)
    
    !Local variables
    INTEGER                                                             :: NRegions,NElements,NBudgetCrops,indxCrop,indxLast,  &
                                                                           iBudgetCrops(NonPondedAg%NBudgetCrops),iCrop
    REAL(8)                                                             :: DummyArray(f_iNAgRootZoneBudColumns,(AppGrid%NSubregions+1)*NonPondedAg%NBudgetCrops) 
    REAL(8),DIMENSION(AppGrid%NElements)                                :: ElemValue
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*NonPondedAg%NBudgetCrops) :: RRunoff,RPrecip,RReuse,RReturn,RSoilMCh,RInfilt,   &
                                                                           RETa,RPerc,Error,RSoilM,RSoilM_P,RETPot
    TYPE(NonPondedAgType)                                               :: NPCrops(AppGrid%NElements,NonPondedAg%NCrops)
    
    !Initialize
    NRegions     = AppGrid%NSubregions
    NElements    = AppGrid%NElements
    NBudgetCrops = NonPondedAg%NBudgetCrops
    iBudgetCrops = NonPondedAg%iBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    DummyArray   = 0.0
    NPCrops      = TRANSPOSE(NonPondedAg%Crops)
    
    !Compute subregional values
    DO indxCrop=1,NBudgetCrops
      iCrop                                           = iBudgetCrops(indxCrop)
      !Moisture at the beginning of the time step must be corrected for chnages due to acreage changes
      ElemValue                                       = (NPCrops(:,iCrop)%SoilM_Precip_P + NPCrops(:,iCrop)%SoilM_AW_P + NPCrops(:,iCrop)%SoilM_Oth_P) * Area(:,indxCrop) &
                                                       - NPCrops(:,iCrop)%SoilMCh                                                                                                                                               &
                                                       + NPCrops(:,iCrop)%PercCh
      RSoilM_P(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM_P(indxLast+indxCrop)                     = SUM(RSoilM_P(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = (NPCrops(:,iCrop)%SoilM_Precip + NPCrops(:,iCrop)%SoilM_AW + NPCrops(:,iCrop)%SoilM_Oth) * Area(:,indxCrop)
      RSoilM(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM(indxLast+indxCrop)                       = SUM(RSoilM(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = ElemPrecip * Area(:,indxCrop)
      RPrecip(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RPrecip(indxLast+indxCrop)                      = SUM(RPrecip(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NPCrops(:,iCrop)%Runoff
      RRunoff(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RRunoff(indxLast+indxCrop)                      = SUM(RRunoff(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NPCrops(:,iCrop)%Reuse
      RReuse(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReuse(indxLast+indxCrop)                       = SUM(RReuse(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NPCrops(:,iCrop)%ReturnFlow
      RReturn(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReturn(indxLast+indxCrop)                      = SUM(RReturn(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NPCrops(:,iCrop)%SoilMCh
      RSoilMCh(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilMCh(indxLast+indxCrop)                     = SUM(RSoilMCh(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NPCrops(:,iCrop)%PrecipInfilt + NPCrops(:,iCrop)%IrigInfilt
      RInfilt(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RInfilt(indxLast+indxCrop)                      = SUM(RInfilt(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NPCrops(:,iCrop)%ETa
      RETa(indxCrop:indxLast:NBudgetCrops)            = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETa(indxLast+indxCrop)                         = SUM(RETa(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NPCrops(:,iCrop)%Perc + NPCrops(:,iCrop)%PercCh
      RPerc(indxCrop:indxLast:NBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RPerc(indxLast+indxCrop)                        = SUM(RPerc(indxCrop:indxLast:NBudgetCrops))
      RETPot(indxCrop:indxLast:NBudgetCrops)          = NonPondedAg%RegionETPot(iCrop,:)
      RETPot(indxLast+indxCrop)                       = SUM(RETPot(indxCrop:indxLast:NBudgetCrops))
    END DO
    Error                                             = RSoilM_P + RSoilMCh + RInfilt + RGenMoistInflow - RETa - RPerc - RSoilM  
    
    !Store in temporary array
    DummyArray(1,:)  = RLUArea                                         !Agricultural area
    DummyArray(2,:)  = RETPot                                          !Potential ET on ag lands
    DummyArray(3,:)  = RPrecip                                         !Precipitation on ag lands
    DummyArray(4,:)  = RRunoff                                         !Runoff from ag lands
    DummyArray(5,:)  = RDeli + RPump                                   !Prime applied water on ag lands prior to application of re-use water
    DummyArray(6,:)  = RUpstrmElemRunoff                               !Surface runoff from upstream elements/subregions used on ag lands
    DummyArray(7,:)  = RReuse                                          !Applied recycled water on ag lands 
    DummyArray(8,:)  = RReturn                                         !Return flow from ag lands
    DummyArray(9,:)  = RSoilM_P                                        !Storage at the beginning of the time interval
    DummyArray(10,:) = RSoilMCh                                        !Soil moisture chnage due to expansion/contraction of ag lands
    DummyArray(11,:) = RInfilt                                         !Infiltration on ag lands
    DummyArray(12,:) = RGenMoistInflow                                 !Generic moisture inflow to non-ponded ag lands
    DummyArray(13,:) = 0.0                                             !Rice/refuge pond drainage on ag lands
    DummyArray(14,:) = RETa                                            !ET on ag lands
    DummyArray(15,:) = RPerc                                           !Percolation on ag lands
    DummyArray(16,:) = RSoilM                                          !Storage at the end of the time interval
    DummyArray(17,:) = Error                                           !Mass balance error for ag lands

    !Print out values to binary file
    CALL NonPondedAg%RootZoneBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteRootZoneFlowsToBudRawFile

  



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DEMAND COMPUTATION AND ROUTING
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SIMULATE FLOW PROCESSES AT NON-PONDED AG
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(NonPondedAg,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,ElemSupply,ReuseFrac,ReturnFrac,ElemsToGW,SolverData,lLakeElem,iStat)
    CLASS(NonPondedAgDatabaseType),TARGET :: NonPondedAg
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(ETType),INTENT(IN)               :: ETData
    TYPE(RootZoneSoilType),INTENT(IN)     :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                    :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:,:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                    :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)       :: SolverData
    LOGICAL,INTENT(IN)                    :: lLakeElem(:)
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8)   :: ThisProcedure = ModName // 'Simulate'
    !$ INTEGER                    :: iChunk
    INTEGER                       :: indxElem,indxCrop,KunsatMethod,iElemID
    REAL(8)                       :: AchievedConv,Area,ETc(NonPondedAg%NCrops),HydCond,TotalPorosity,fRU, &
                                     FieldCapacity,TotalPorosityCrop,FieldCapacityCrop,RootDepth,Lambda,  &
                                     Supply,WiltingPoint,WiltingPointCrop,SoilM_P,SoilM,RootDepth_P,fRF,  &
                                     RootDepthFrac,Perc_RDAdj,GM,PrecipD,rMultip,GMElem,Excess,Inflow,&
                                     ratio(3),SoilM_P_Array(3),SoilM_Array(3),Infilt(3),ETPartition(3)
    TYPE(NonPondedAgType),POINTER :: pCrop
    LOGICAL                       :: lNegativeMoist
    
    !Initialize
    iStat     = 0
    !$ iChunk = MAX(1 , AppGrid%NElements/(OMP_GET_MAX_THREADS()-1)/10)
    
    !Inform user
    CALL EchoProgress('Simulating flows at non-ponded agricultural crop lands')
    
    !Simulate
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,ETData,Precip,GenericMoisture,DeltaT,       &
    !$OMP                                  NonPondedAg,ElemSupply,ReturnFrac,ReuseFrac,ElemsToGW,SolverData,iStat) &
    !$OMP          NUM_THREADS(OMP_GET_MAX_THREADS()-1) 
    !$OMP DO SCHEDULE(DYNAMIC,iChunk)
    DO indxElem=1,AppGrid%NElements
        IF (iStat .EQ. -1) CYCLE
        IF (lLakeElem(indxElem)) CYCLE
        WiltingPoint  = SoilsData(indxElem)%WiltingPoint
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        HydCond       = SoilsData(indxElem)%HydCond
        Lambda        = SoilsData(indxElem)%Lambda
        KunsatMethod  = SoilsData(indxElem)%KunsatMethod
        ETc           = ETData%GetValues(NonPondedAg%Crops(:,indxElem)%iColETc)
        GMElem        = GenericMoisture(1,indxElem) * DeltaT
        PrecipD       = Precip(indxElem) * DeltaT
        DO indxCrop=1,NonPondedAg%NCrops
            !Initialize
            pCrop              => NonPondedAg%Crops(indxCrop,indxElem)
            pCrop%Runoff       = 0.0
            pCrop%ReturnFlow   = 0.0
            pCrop%PrecipInfilt = 0.0                     
            pCrop%IrigInfilt   = 0.0                          
            pCrop%ETa          = 0.0
            pCrop%ETAW         = 0.0
            pCrop%ETP          = 0.0 
            pCrop%ETOth        = 0.0
            pCrop%Perc         = 0.0
            pCrop%Reuse        = 0.0
            pCrop%GMExcess     = 0.0
            
            !Cycle if Area is zero
            Area = pCrop%Area
            IF (Area .EQ. 0.0) CYCLE
            
            !Initialize
            RootDepth   = NonPondedAg%RootDepth(indxCrop)
            RootDepth_P = NonPondedAg%RootDepth_P(indxCrop)
            IF (RootDepth .GT. RootDepth_P) THEN
                RootDepthFrac = 1d0
            ELSE
                RootDepthFrac = RootDepth / RootDepth_P
            END IF
            TotalPorosityCrop = TotalPorosity * RootDepth
            FieldCapacityCrop = FieldCapacity * RootDepth
            WiltingPointCrop  = WiltingPoint  * RootDepth
            GM                = GMElem        * RootDepth
            Supply            = ElemSupply(indxCrop,indxElem) * DeltaT/Area
            SoilM_P           = pCrop%SoilM_Precip_P + pCrop%SoilM_AW_P + pCrop%SoilM_Oth_P
            Perc_RDAdj        = SoilM_P * (1d0 - RootDepthFrac)
            SoilM_P           = SoilM_P * RootDepthFrac
            
            !Infiltration and return flow due to applied water
            fRF               = ReturnFrac(pCrop%iColReturnFrac)
            fRU               = ReuseFrac(pCrop%iColReuseFrac)
            pCrop%IrigInfilt  = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
            pCrop%ReturnFlow  = Supply - pCrop%IrigInfilt
            
            !Inflow into root zone
            Inflow            = GM + pCrop%IrigInfilt 
            
            !Simulate
            CALL NonPondedLUMoistureRouter(PrecipD                                     ,  &
                                           pCrop%SMax                                  ,  &
                                           SoilM_P                                     ,  &
                                           ETc(indxCrop)*DeltaT                        ,  & 
                                           HydCond                                     ,  & 
                                           TotalPorosityCrop                           ,  & 
                                           FieldCapacityCrop                           ,  &
                                           WiltingPointCrop                            ,  & 
                                           Lambda                                      ,  & 
                                           Inflow                                      ,  &
                                           SolverData%Tolerance*TotalPorosityCrop      ,  &
                                           KunsatMethod                                ,  &
                                           SolverData%IterMax                          ,  &
                                           SoilM                                       ,  & 
                                           pCrop%Runoff                                ,  & 
                                           pCrop%PrecipInfilt                          ,  & 
                                           pCrop%ETa                                   ,  & 
                                           pCrop%Perc                                  ,  & 
                                           Excess                                      ,  &
                                           AchievedConv                                )
                                          
            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
                !$OMP CRITICAL
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Convergence error in soil moisture routing for non-ponded crops!'
                MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                MessageArray(3) = 'Crop type            = '//TRIM(NonPondedAg%CropCodes(indxCrop))
                WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                iStat = -1
                !$OMP END CRITICAL
                EXIT
            END IF
            
            !Reduce inflows based on correction for total porosity
            IF (Excess .NE. 0.0) THEN
                ratio = [pCrop%PrecipInfilt , pCrop%IrigInfilt , GM]
                CALL NormalizeArray(ratio)
                pCrop%Runoff       = pCrop%Runoff     + Excess * ratio(1) 
                pCrop%ReturnFlow   = pCrop%ReturnFlow + Excess * ratio(2)
                pCrop%GMExcess     = Excess * ratio(3)
                pCrop%PrecipInfilt = PrecipD - pCrop%Runoff
                pCrop%IrigInfilt   = Supply  - pCrop%ReturnFlow
            END IF
            
            !Compute re-use based on return flow
            IF (ReturnFrac(pCrop%iColReturnFrac) .GT. 0.0)   &
                pCrop%Reuse = pCrop%ReturnFlow * fRU / fRF
            
            !Compute moisture from precip and irrigation
            SoilM_P_Array = [pCrop%SoilM_Precip_P * RootDepthFrac , pCrop%SoilM_AW_P * RootDepthFrac , pCrop%SoilM_Oth_P * RootDepthFrac]
            Infilt        = [pCrop%PrecipInfilt                   , pCrop%IrigInfilt                 , GM - pCrop%GMExcess              ]
            CALL TrackMoistureDueToSource(SoilM_P_Array  , &
                                          Infilt         , &
                                          pCrop%Perc     , &
                                          pCrop%ETa      , &
                                          0d0            , &
                                          SoilM_Array    , &
                                          ETPartition    )
            pCrop%SoilM_Precip = SoilM_Array(1)
            pCrop%SoilM_AW     = SoilM_Array(2)
            pCrop%SoilM_Oth    = SoilM_Array(3)
            pCrop%ETP          = ETPartition(1)                
            pCrop%ETAW         = ETPartition(2)
            pCrop%ETOth        = ETPartition(3)
            
            !Make sure soil moisture is not less than zero
            lNegativeMoist = .FALSE.
            IF (pCrop%SoilM_Precip .LT. 0.0) lNegativeMoist = .TRUE.
            IF (pCrop%SoilM_AW     .LT. 0.0) lNegativeMoist = .TRUE.
            IF (pCrop%SoilM_Oth    .LT. 0.0) lNegativeMoist = .TRUE.
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
                EXIT
            END IF
            
            !Convert depths to volumetric rates
            rMultip            = Area / DeltaT 
            pCrop%Runoff       = pCrop%Runoff            * rMultip
            pCrop%ReturnFlow   = pCrop%ReturnFlow        * rMultip
            pCrop%PrecipInfilt = pCrop%PrecipInfilt      * rMultip
            pCrop%IrigInfilt   = pCrop%IrigInfilt        * rMultip
            pCrop%ETa          = pCrop%ETa               * rMultip
            pCrop%Perc         = (pCrop%Perc+Perc_RDAdj) * rMultip  
            pCrop%Reuse        = pCrop%Reuse             * rMultip
            pCrop%ETAW         = pCrop%ETAW              * rMultip
            pCrop%ETP          = pCrop%ETP               * rMultip
            pCrop%ETOth        = pCrop%ETOth             * rMultip
            
            !If surface flow goes to groundwater, update the runoff processes
            IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
                pCrop%Perc         = pCrop%Perc + pCrop%Runoff + pCrop%ReturnFlow
                pCrop%PrecipInfilt = pCrop%PrecipInfilt + pCrop%Runoff        !Runoff and 
                pCrop%IrigInfilt   = pCrop%IrigInfilt + pCrop%ReturnFlow      ! return flow are assumed to bypass root zone for proper mass balance       
                pCrop%Runoff       = 0.0
                pCrop%ReturnFlow   = 0.0
            END IF
         
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE NON-PONDED AG DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(NonPondedAg,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,SpecifiedDemand,ReuseFrac,ReturnFrac,IrigPeriod,SolverData,lLakeElem,lReadAgWaterDemand,iStat)
    CLASS(NonPondedAgDatabaseType),TARGET :: NonPondedAg
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(ETType)                          :: ETData
    TYPE(RootZoneSoilType),INTENT(IN)     :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                    :: DeltaT,Precip(:),GenericMoisture(:,:),SpecifiedDemand(:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                    :: IrigPeriod(:)
    TYPE(SolverDataType),INTENT(IN)       :: SolverData
    LOGICAL,INTENT(IN)                    :: lLakeElem(:),lReadAgWaterDemand
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18)  :: ThisProcedure = ModName // 'ComputeWaterDemand'
    !$ INTEGER                    :: iChunk
    INTEGER                       :: indxElem,indxCrop,KunsatMethod,iElemID
    REAL(8)                       :: Area,ETc(NonPondedAg%NCrops),RootDepth,TotalPorosityCrop,TotalPorosity,  &
                                     FieldCapacity,AchievedConv,HydCond,PrecipDepth,Lambda,SoilM,TargetSoilM, &
                                     WiltingPoint,WiltingPointCrop,FieldCapacityCrop,RootDepth_P,Runoff,ETa,  &
                                     SoilM_P,RootDepthFrac,GMElem,GM,PrecipD,TAW,PrecipInfilt,Perc,Excess
    TYPE(NonPondedAgType),POINTER :: pCrop    
    
    !Initialize
    iStat     = 0
    !$ iChunk = MAX(1 , AppGrid%NElements/(OMP_GET_MAX_THREADS()-1)/10)
    
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,Precip,ETData,GenericMoisture,lReadAgWaterDemand,DeltaT, &
    !$OMP                                  SolverData,NonPondedAg,SpecifiedDemand,ReuseFrac,ReturnFrac,IrigPeriod,iStat)        &
    !$OMP          NUM_THREADS(OMP_GET_MAX_THREADS()-1)
    !$OMP DO SCHEDULE(DYNAMIC,iChunk)
    DO indxElem=1,AppGrid%NElements
        NonPondedAg%Crops(:,indxElem)%DemandRaw = 0.0
        NonPondedAg%Crops(:,indxElem)%Demand    = 0.0
        IF (iStat .EQ. -1) CYCLE
        IF (lLakeElem(indxElem)) CYCLE
        WiltingPoint  = SoilsData(indxElem)%WiltingPoint
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        TAW           = FieldCapacity - WiltingPoint
        HydCond       = SoilsData(indxElem)%HydCond
        Lambda        = SoilsData(indxElem)%Lambda
        KunsatMethod  = SoilsData(indxElem)%KunsatMethod
        PrecipDepth   = Precip(indxElem)*DeltaT
        ETc           = ETData%GetValues(NonPondedAg%Crops(:,indxElem)%iColETc)
        GMElem        = GenericMoisture(1,indxElem) * DeltaT
        PrecipD       = Precip(indxElem) * DeltaT
        DO indxCrop=1,NonPondedAg%NCrops
            pCrop => NonPondedAg%Crops(indxCrop,indxElem)
            
            !Cycle if Area is zero
            Area = pCrop%Area
            IF (Area .EQ. 0.0) CYCLE
          
            !Cycle if demand is specified 
            IF (lReadAgWaterDemand) THEN
                IF (NonPondedAg%iColAgDemand(indxCrop,indxElem) .GT. 0) THEN
                    pCrop%DemandRaw = SpecifiedDemand(NonPondedAg%iColAgDemand(indxCrop,indxElem))
                    pCrop%Demand    = pCrop%DemandRaw
                    CYCLE
                END IF
            END IF
              
            !Cycle if it is not an irrigation period
            IF (IrigPeriod(pCrop%iColIrigPeriod) .EQ. f_iNoIrigPeriod) CYCLE
            
            !Initialize varaibles for crop
            RootDepth   = NonPondedAg%RootDepth(indxCrop)
            RootDepth_P = NonPondedAg%RootDepth_P(indxCrop)
            GM          = GMElem * RootDepth
            IF (RootDepth .GT. RootDepth_P) THEN
                RootDepthFrac = 1d0 
            ELSE
                RootDepthFrac = RootDepth / RootDepth_P
            END IF
            
            !Soil moisture at the beginning time step
            SoilM_P = (pCrop%SoilM_Precip_P + pCrop%SoilM_AW_P + pCrop%SoilM_Oth_P) * RootDepthFrac

            !Cycle if no need for demand computation; i.e. moisture is above required minimum
            SELECT CASE (NonPondedAg%iDemandFromMoist)
                CASE (f_iDemandFromMoistAtBegin)
                    IF (SoilM_P  .GE.  (pCrop%MinSoilM*TAW + WiltingPoint) * RootDepth) CYCLE
                  
                CASE (f_iDemandFromMoistAtEnd)
                    CALL NonPondedLUMoistureRouter(PrecipD                                            ,  &
                                                   pCrop%SMax                                         ,  &
                                                   SoilM_P                                            ,  &
                                                   ETc(indxCrop) * DeltaT                             ,  & 
                                                   HydCond                                            ,  & 
                                                   TotalPorosity * RootDepth                          ,  & 
                                                   FieldCapacity *RootDepth                           ,  &
                                                   WiltingPoint * RootDepth                           ,  & 
                                                   Lambda                                             ,  & 
                                                   GM                                                 ,  &
                                                   SolverData%Tolerance * TotalPorosity * RootDepth   ,  &
                                                   KunsatMethod                                       ,  &
                                                   SolverData%IterMax                                 ,  &
                                                   SoilM                                              ,  & 
                                                   Runoff                                             ,  & 
                                                   PrecipInfilt                                       ,  & 
                                                   ETa                                                ,  & 
                                                   Perc                                               ,  & 
                                                   Excess                                             ,  &
                                                   AchievedConv                                       )
                    !Generate error if convergence is not achieved
                    IF (AchievedConv .NE. 0.0) THEN
                        !$OMP CRITICAL
                        iElemID         = AppGrid%AppElement(indxElem)%ID
                        MessageArray(1) = 'Convergence error in water demand calculations for non-ponded crops!'
                        MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                        MessageArray(3) = 'Crop type            = '//TRIM(NonPondedAg%CropCodes(indxCrop))
                        WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosity*RootDepth
                        WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                        CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                        iStat = -1
                        !$OMP END CRITICAL
                        EXIT
                    END IF
                    
                    !Check if demand calculation is necessary
                    IF (SoilM  .GE.  (pCrop%MinSoilM*TAW + WiltingPoint) * RootDepth) CYCLE                    
            END SELECT  
              
            !Initialize
            SoilM             = (pCrop%SoilM_Precip_P + pCrop%SoilM_AW_P + pCrop%SoilM_Oth_P) * RootDepthFrac
            WiltingPointCrop  = WiltingPoint  * RootDepth
            FieldCapacityCrop = FieldCapacity * RootDepth
            TotalPorosityCrop = TotalPorosity * RootDepth
            TargetSoilM       = MIN(FieldCapacityCrop * NonPondedAg%TargetSoilMFile%rValues(pCrop%iColTargetSoilM) , TotalPorosityCrop)
          
            !Compute demand
            CALL NonPondedCropDemand(PrecipDepth                                            ,  &
                                     pCrop%SMax                                             ,  &
                                     GM                                                     ,  &
                                     ReturnFrac(pCrop%iColReturnFrac)                       ,  &
                                     ReuseFrac(pCrop%iColReuseFrac)                         ,  &
                                     NonPondedAg%LeachFracFile%rValues(pCrop%iColLeachFrac) ,  &
                                     ETc(indxCrop)*DeltaT                                   ,  &
                                     HydCond                                                ,  &
                                     TotalPorosityCrop                                      ,  &
                                     FieldCapacityCrop                                      ,  &
                                     WiltingPointCrop                                       ,  &      
                                     TargetSoilM                                            ,  & 
                                     Lambda                                                 ,  & 
                                     SoilM                                                  ,  &
                                     SolverData%Tolerance*TotalPorosityCrop                 ,  &
                                     KunsatMethod                                           ,  &
                                     SolverData%IterMax                                     ,  &
                                     pCrop%DemandRaw                                        ,  &
                                     pCrop%Demand                                           ,  &
                                     AchievedConv                                           )
                                     
            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
                !$OMP CRITICAL
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Convergence error in calculating agricultural water demand '
                MessageArray(2) = 'for crop '//TRIM(NonPondedAg%CropCodes(indxCrop))//' in element '//TRIM(IntToText(iElemID))//'!'
                WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                !$OMP END CRITICAL
                EXIT
            END IF

            !Convert demand related data to volumetric rate and store in persistent arrays
            pCrop%DemandRaw = pCrop%DemandRaw / DeltaT * Area
            pCrop%Demand    = pCrop%Demand    / DeltaT * Area
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    
    
    ASSOCIATE (pCrops          => NonPondedAg%Crops                  , &
               pRootDepth      => NonPondedAg%RootDepth              , &
               pRootDepth_P    => NonPondedAg%RootDepth_P            , &
               piColAgDemand   => NonPondedAg%iColAgDemand           , &
               pLeachFrac      => NonPondedAg%LeachFracFile%rValues  , &
               pTargetSoilM    => NonPondedAg%TargetSoilMFile%rValues)
    
      !Initialize
      iStat             = 0
      pCrops%DemandRaw  = 0.0
      pCrops%Demand     = 0.0
    
      DO indxElem=1,AppGrid%NElements
        IF (lLakeElem(indxElem)) CYCLE
        WiltingPoint  = SoilsData(indxElem)%WiltingPoint
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        TAW           = FieldCapacity - WiltingPoint
        HydCond       = SoilsData(indxElem)%HydCond
        Lambda        = SoilsData(indxElem)%Lambda
        KunsatMethod  = SoilsData(indxElem)%KunsatMethod
        PrecipDepth   = Precip(indxElem)*DeltaT
        ETc           = ETData%GetValues(pCrops(:,indxElem)%iColETc)
        GMElem        = GenericMoisture(1,indxElem) * DeltaT
        PrecipD       = Precip(indxElem) * DeltaT
        DO indxCrop=1,NonPondedAg%NCrops
          ASSOCIATE (pCrop => pCrops(indxCrop,indxElem))
               
            !Cycle if Area is zero
            Area = pCrop%Area
            IF (Area .EQ. 0.0) CYCLE
          
            !Cycle if demand is specified 
            IF (lReadAgWaterDemand) THEN
              IF (piColAgDemand(indxCrop,indxElem) .GT. 0) THEN
                pCrop%DemandRaw = SpecifiedDemand(piColAgDemand(indxCrop,indxElem))
                pCrop%Demand    = pCrop%DemandRaw
                CYCLE
              END IF
            END IF
              
            !Cycle if it is not an irrigation period
            IF (IrigPeriod(pCrop%iColIrigPeriod) .EQ. f_iNoIrigPeriod) CYCLE
            
            !Initialize varaibles for crop
            RootDepth   = pRootDepth(indxCrop)
            RootDepth_P = pRootDepth_P(indxCrop)
            GM          = GMElem * RootDepth
            IF (RootDepth .GT. RootDepth_P) THEN
              RootDepthFrac = 1d0 
            ELSE
              RootDepthFrac = RootDepth / RootDepth_P
            END IF
            
            !Soil moisture at the beginning time step
            SoilM_P = (pCrop%SoilM_Precip_P + pCrop%SoilM_AW_P + pCrop%SoilM_Oth_P) * RootDepthFrac

            !Cycle if no need for demand computation; i.e. moisture is above required minimum
            SELECT CASE (NonPondedAg%iDemandFromMoist)
              CASE (f_iDemandFromMoistAtBegin)
                IF (SoilM_P  .GE.  (pCrop%MinSoilM*TAW + WiltingPoint) * RootDepth) CYCLE
                
              CASE (f_iDemandFromMoistAtEnd)
                CALL NonPondedLUMoistureRouter(PrecipD                                            ,  &
                                               pCrop%SMax                                         ,  &
                                               SoilM_P                                            ,  &
                                               ETc(indxCrop) * DeltaT                             ,  & 
                                               HydCond                                            ,  & 
                                               TotalPorosity * RootDepth                          ,  & 
                                               FieldCapacity *RootDepth                           ,  &
                                               WiltingPoint * RootDepth                           ,  & 
                                               Lambda                                             ,  & 
                                               GM                                                 ,  &
                                               SolverData%Tolerance * TotalPorosity * RootDepth   ,  &
                                               KunsatMethod                                       ,  &
                                               SolverData%IterMax                                 ,  &
                                               SoilM                                              ,  & 
                                               Runoff                                             ,  & 
                                               PrecipInfilt                                       ,  & 
                                               ETa                                                ,  & 
                                               Perc                                               ,  & 
                                               Excess                                             ,  &
                                               AchievedConv                                       )
                !Generate error if convergence is not achieved
                IF (AchievedConv .NE. 0.0) THEN
                    iElemID         = AppGrid%AppElement(indxElem)%ID
                    MessageArray(1) = 'Convergence error in water demand calculations for non-ponded crops!'
                    MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                    MessageArray(3) = 'Crop type            = '//TRIM(NonPondedAg%CropCodes(indxCrop))
                    WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosity*RootDepth
                    WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                    CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                !Check if demand calculation is necessary
                IF (SoilM  .GE.  (pCrop%MinSoilM*TAW + WiltingPoint) * RootDepth) CYCLE
              
            END SELECT  
              
            !Initialize
            SoilM             = (pCrop%SoilM_Precip_P + pCrop%SoilM_AW_P + pCrop%SoilM_Oth_P) * RootDepthFrac
            WiltingPointCrop  = WiltingPoint  * RootDepth
            FieldCapacityCrop = FieldCapacity * RootDepth
            TotalPorosityCrop = TotalPorosity * RootDepth
            TargetSoilM       = MIN(FieldCapacityCrop * pTargetSoilM(pCrop%iColTargetSoilM) , TotalPorosityCrop)
          
            !Compute demand
            CALL NonPondedCropDemand(PrecipDepth                             ,  &
                                     pCrop%SMax                              ,  &
                                     GM                                      ,  &
                                     ReturnFrac(pCrop%iColReturnFrac)        ,  &
                                     ReuseFrac(pCrop%iColReuseFrac)          ,  &
                                     pLeachFrac(pCrop%iColLeachFrac)         ,  &
                                     ETc(indxCrop)*DeltaT                    ,  &
                                     HydCond                                 ,  &
                                     TotalPorosityCrop                       ,  &
                                     FieldCapacityCrop                       ,  &
                                     WiltingPointCrop                        ,  &      
                                     TargetSoilM                             ,  & 
                                     Lambda                                  ,  & 
                                     SoilM                                   ,  &
                                     SolverData%Tolerance*TotalPorosityCrop  ,  &
                                     KunsatMethod                            ,  &
                                     SolverData%IterMax                      ,  &
                                     pCrop%DemandRaw                         ,  &
                                     pCrop%Demand                            ,  &
                                     AchievedConv                            )
                                     
            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Convergence error in calculating agricultural water demand '
                MessageArray(2) = 'for crop '//TRIM(NonPondedAg%CropCodes(indxCrop))//' in element '//TRIM(IntToText(iElemID))//'!'
                WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF

            !Convert demand related data to volumetric rate and store in persistent arrays
            pCrop%DemandRaw = pCrop%DemandRaw / DeltaT * Area
            pCrop%Demand    = pCrop%Demand    / DeltaT * Area

          END ASSOCIATE 
          
        END DO
      END DO
      
    END ASSOCIATE
    
  END SUBROUTINE ComputeWaterDemand
  
  
  
  
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
  SUBROUTINE SoilMContent_To_Depth(NonPondedAgLand,NElements,iElemIDs,TotalPorosity,iStat)
    CLASS(NonPondedAgDatabaseType) :: NonPondedAgLand
    INTEGER,INTENT(IN)             :: NElements,iElemIDs(NElements)
    REAL(8),INTENT(IN)             :: TotalPorosity(:)
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxElem,NCrops,indxCrop
    REAL(8)                      :: RootDepth(NonPondedAgLand%NCrops),TP
    
    !Initialize
    iStat = 0
    
    !Return if non-ponded crops are not modeled
    IF (NonPondedAgLand%NCrops .EQ. 0) RETURN
    
    !Initialize
    NCrops    = NonPondedAgLand%NCrops
    RootDepth = NonPondedAgLand%RootDepth
    
    !Check if initial conditions are greater than total porosity, if not convert conetnts to depths and equate SoilM_P to SoilM
    DO indxElem=1,NElements
      TP = TotalPorosity(indxElem)
      ASSOCIATE (pCrops => NonPondedAgLand%Crops(:,indxElem)) 
                 
        DO indxCrop=1,NCrops
          IF (pCrops(indxCrop)%SoilM_Precip + pCrops(indxCrop)%SoilM_AW + pCrops(indxCrop)%SoilM_Oth .GT. TP) THEN
              CALL SetLastMessage('Initial moisture content for crop ' // TRIM(NonPondedAgLand%CropCodes(indxCrop)) // ' at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
        END DO
        pCrops%SoilM_Precip   = pCrops%SoilM_Precip * RootDepth
        pCrops%SoilM_Precip_P = pCrops%SoilM_Precip
        pCrops%SoilM_AW       = pCrops%SoilM_AW * RootDepth
        pCrops%SoilM_AW_P     = pCrops%SoilM_AW
        pCrops%SoilM_Oth      = pCrops%SoilM_Oth * RootDepth
        pCrops%SoilM_Oth_P    = pCrops%SoilM_Oth
   
      END ASSOCIATE
    END DO 
    
  END SUBROUTINE SoilMContent_To_Depth
  

  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(NonPondedAgLand) 
    CLASS(NonPondedAgDatabaseType) :: NonPondedAgLand
    
    NonPondedAgLand%Crops%Area_P = NonPondedAgLand%Crops%Area
    
  END SUBROUTINE AdvanceAreas
  
  ! -------------------------------------------------------------
  ! --- COMPARE SPECIFIED AG DEMAND TO AREAS
  ! -------------------------------------------------------------
  SUBROUTINE CheckSpecifiedDemandAndArea(NonPondedAgLand,iElemIDs,rSpecifiedDemand,iStat)
    CLASS(NonPondedAgDatabaseType),INTENT(IN) :: NonPondedAgLand
    INTEGER,INTENT(IN)                        :: iElemIDs(:)
    REAL(8),INTENT(IN)                        :: rSpecifiedDemand(:)
    INTEGER,INTENT(OUT)                       :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+27) :: ThisProcedure = ModName // 'CheckSpecifiedDemandAndArea'
    INTEGER                      :: indxElem,indxCrop,iCol
    
    !Initialize
    iStat = 0
    
    !Compare
    DO indxElem=1,SIZE(NonPondedAgLand%Crops,DIM=2)
        DO indxCrop=1,NonPondedAgLand%NCrops
            iCol = NonPondedAgLand%iColAgDemand(indxCrop,indxElem)
            IF (iCol .GT. 0) THEN
                IF (rSpecifiedDemand(iCol) .GT. 0.0) THEN
                    IF (NonPondedAgLand%Crops(indxCrop,indxElem)%Area .EQ. 0.0) THEN
                        MessageArray(1) = 'Specified water demand for crop ' // TRIM(NonPondedAgLand%CropCodes(indxCrop)) // ' at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater '
                        MessageArray(2) = 'than zero when crop area is zero.'
                        MessageArray(3) = '(This may be due to the element being specified as a lake element)'
                        CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
            END IF
        END DO
    END DO
    
  END SUBROUTINE CheckSpecifiedDemandAndArea
    
  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFIED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(NonPondedAgLand,iElemIDs,rElemAreas,TimeStep,iStat)
    CLASS(NonPondedAgDatabaseType) :: NonPondedAgLand
    INTEGER,INTENT(IN)             :: iElemIDs(:)
    REAL(8),INTENT(IN)             :: rElemAreas(:)
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep 
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    !Rewind files to beginning and read data until specified time stamp
    CALL NonPondedAgLand%RootDepthFracDataFile%File%RewindFile_To_BeginningOfTSData(iStat)       ;  IF (iStat .NE. 0) RETURN
    CALL RootDepthFracDataFile_ReadTSData(NonPondedAgLand%RootDepthFracDataFile,TimeStep,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    CALL NonPondedAgLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                             ;  IF (iStat .NE. 0) RETURN
    CALL NonPondedAgLand%LandUseDataFile%ReadTSData('Non-ponded crop areas',TimeStep,rElemAreas,iElemIDs,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL NonPondedAgLand%MinSoilMFile%File%RewindFile_To_BeginningOfTSData(iStat)                                                ;  IF (iStat .NE. 0) RETURN
    CALL ReadTimeSeriesData(TimeStep,'Minimum soil moisture requirement data',NonPondedAgLand%MinSoilMFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN

    IF (NonPondedAgLand%TargetSoilMFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL NonPondedAgLand%TargetSoilMFile%File%RewindFile_To_BeginningOfTSData(iStat)                                              ;  IF (iStat .NE. 0) RETURN
        CALL ReadTimeSeriesData(TimeStep,'Irrigation target soil moisture data',NonPondedAgLand%TargetSoilMFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    END IF
    
    IF (NonPondedAgLand%LeachFracFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL NonPondedAgLand%LeachFracFile%File%RewindFile_To_BeginningOfTSData(iStat)                              ;  IF (iStat .NE. 0) RETURN
        CALL ReadTimeSeriesData(TimeStep,'Leaching factor data',NonPondedAgLand%LeachFracFile,iFileReadCode,iStat)
    END IF
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp

END MODULE