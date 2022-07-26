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
  USE Class_RootDepthFracDataFile  , ONLY: RootDepthFracDataFileType        
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
  USE Package_Misc                 , ONLY: SolverDataType                   , & 
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
                                           RealTSDataInFileType             , &
                                           IntTSDataInFileType              , &
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
    INTEGER,ALLOCATABLE :: iColIrigPeriod(:,:)          !Column number in the irrigation period data file
    INTEGER,ALLOCATABLE :: iColMinSoilM(:,:)            !Column number in the irrigation trigger minimum soil moisture data file
    INTEGER,ALLOCATABLE :: iColTargetSoilM(:,:)         !Column number in the irrigtaion target soil moisture data file
    INTEGER,ALLOCATABLE :: iColReturnFrac(:,:)          !Column number in the return flow fraction data file
    INTEGER,ALLOCATABLE :: iColReuseFrac(:,:)           !Column number in the re-use fraction data file
    INTEGER,ALLOCATABLE :: iColLeachFrac(:,:)           !Column number in the minimum perc fraction data file
    REAL(8),ALLOCATABLE :: MinSoilM(:,:)                !Minimum soil moisture as a fraction of Total Available Water (field capacity - wilting point) to be used for irrigation trigger
    REAL(8),ALLOCATABLE :: ReturnFlow(:,:)              !Return flow
    REAL(8),ALLOCATABLE :: IrigInfilt(:,:)              !Infiltration due to irrigation
    REAL(8),ALLOCATABLE :: Reuse(:,:)                   !Reused return flow 
    REAL(8),ALLOCATABLE :: ETAW(:,:)                    !ET of applied water
    REAL(8),ALLOCATABLE :: ETP(:,:)                     !ET of precipitation
    REAL(8),ALLOCATABLE :: ETOth(:,:)                   !ET of other sources of moisture
    REAL(8),ALLOCATABLE :: DemandRaw(:,:)               !Agricultural water demand before the return flow is included
    REAL(8),ALLOCATABLE :: Demand(:,:)                  !Agricultural water demand after the return flow is included 
    REAL(8),ALLOCATABLE :: ElemDemandFrac(:,:)          !Ratio of crop demand to the total demand at the element it is located at
    REAL(8),ALLOCATABLE :: ElemDemandFrac_Ag(:,:)       !Ratio of crop demand to the total "ag" demand at the element it is located at
  END TYPE NonPondedAgType
  
  
  ! -------------------------------------------------------------
  ! --- NON PONDED AG LAND DATABASE TYPE
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: LenCropCode = 2  !Length of crop codes
  TYPE NonPondedAgDatabaseType
    INTEGER                                :: NCrops                      = 0                         !Number of non-ponded crops
    INTEGER                                :: iDemandFromMoist            = f_iDemandFromMoistAtBegin !Moisture that will be used to decide when to compute ag water demand
    TYPE(NonPondedAgType)                  :: Crops                                                   !Non-ponded ag land data for each (crop,element) combination
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
    CALL AgLand%Crops%New(NCrops,NElements,iStat)
    ALLOCATE (AgLand%Crops%iColIrigPeriod(NCrops,NElements)    , &      
              AgLand%Crops%iColMinSoilM(NCrops,NElements)      , &      
              AgLand%Crops%iColTargetSoilM(NCrops,NElements)   , &      
              AgLand%Crops%iColReturnFrac(NCrops,NElements)    , &      
              AgLand%Crops%iColReuseFrac(NCrops,NElements)     , &      
              AgLand%Crops%iColLeachFrac(NCrops,NElements)     , &      
              AgLand%Crops%MinSoilM(NCrops,NElements)          , &      
              AgLand%Crops%ReturnFlow(NCrops,NElements)        , &      
              AgLand%Crops%IrigInfilt(NCrops,NElements)        , &      
              AgLand%Crops%Reuse(NCrops,NElements)             , &      
              AgLand%Crops%ETAW(NCrops,NElements)              , &      
              AgLand%Crops%ETP(NCrops,NElements)               , &      
              AgLand%Crops%ETOth(NCrops,NElements)             , &      
              AgLand%Crops%DemandRaw(NCrops,NElements)         , &      
              AgLand%Crops%Demand(NCrops,NElements)            , &      
              AgLand%Crops%ElemDemandFrac(NCrops,NElements)    , &      
              AgLand%Crops%ElemDemandFrac_Ag(NCrops,NElements) , &
              AgLand%CropCodes(NCrops)                         , &
              AgLand%MaxRootDepth(NCrops)                      , &
              AgLand%iColRootDepthFrac(NCrops)                 , &
              AgLand%RootDepth_P(NCrops)                       , &
              AgLand%RootDepth(NCrops)                         , &
              AgLand%RegionETPot(NCrops,NRegions)              , &
              STAT=ErrorCode                                   )
    IF (ErrorCode+iStat .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for non-ponded agricultural data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize arrays
    AgLand%Crops%iColIrigPeriod    = 0      
    AgLand%Crops%iColMinSoilM      = 0     
    AgLand%Crops%iColTargetSoilM   = 1
    AgLand%Crops%iColReturnFrac    = 0     
    AgLand%Crops%iColReuseFrac     = 0      
    AgLand%Crops%iColLeachFrac     = 1
    AgLand%Crops%MinSoilM          = 0.0      
    AgLand%Crops%ReturnFlow        = 0.0     
    AgLand%Crops%IrigInfilt        = 0.0     
    AgLand%Crops%Reuse             = 0.0      
    AgLand%Crops%ETAW              = 0.0      
    AgLand%Crops%ETP               = 0.0      
    AgLand%Crops%ETOth             = 0.0      
    AgLand%Crops%DemandRaw         = 0.0      
    AgLand%Crops%Demand            = 0.0      
    AgLand%Crops%ElemDemandFrac    = 0.0     
    AgLand%Crops%ElemDemandFrac_Ag = 0.0    
        
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
    CALL AgLand%RootDepthFracDataFile%New(cAbsPathFileName,cWorkingDirectory,TrackTime,iStat)  ;  IF (iStat .EQ. -1) RETURN
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
        AgLand%Crops%SMax(:,iElem) = (1000.0/DummyRealArray(indxElem,2:)-10.0) * FACTCN
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
        AgLand%Crops%iColETc(:,iElem) = DummyIntArray(indxElem,2:)    
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
        AgLand%Crops%iColIrigPeriod(:,iElem) = DummyIntArray(indxElem,2:)
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
        AgLand%Crops%iColMinSoilM(:,iElem) = DummyIntArray(indxElem,2:) 
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
            AgLand%Crops%iColTargetSoilM(:,iElem) = DummyIntArray(indxElem,2:) 
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
        AgLand%Crops%iColReturnFrac(:,iElem) = DummyIntArray(indxElem,2:) 
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
        AgLand%Crops%iColReuseFrac(:,iElem) = DummyIntArray(indxElem,2:) 
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
            AgLand%Crops%iColLeachFrac(:,iElem) = DummyIntArray(indxElem,2:) 
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
        AgLand%Crops%SoilM_Precip(:,iElem) = DummyRealArray(indxElem,2) * DummyRealArray(indxElem,3:)
        AgLand%Crops%SoilM_AW(:,iElem)     = DummyRealArray(indxElem,3:) - AgLand%Crops%SoilM_Precip(:,iElem)
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
    CALL AgLand%Crops%Kill()
    DEALLOCATE (AgLand%Crops%iColIrigPeriod     , &      
                AgLand%Crops%iColMinSoilM       , &      
                AgLand%Crops%iColTargetSoilM    , &      
                AgLand%Crops%iColReturnFrac     , &      
                AgLand%Crops%iColReuseFrac      , &      
                AgLand%Crops%iColLeachFrac      , &      
                AgLand%Crops%MinSoilM           , &      
                AgLand%Crops%ReturnFlow         , &      
                AgLand%Crops%IrigInfilt         , &      
                AgLand%Crops%Reuse              , &      
                AgLand%Crops%ETAW               , &      
                AgLand%Crops%ETP                , &      
                AgLand%Crops%ETOth              , &      
                AgLand%Crops%DemandRaw          , &      
                AgLand%Crops%Demand             , &      
                AgLand%Crops%ElemDemandFrac     , &      
                AgLand%Crops%ElemDemandFrac_Ag  , &
                AgLand%CropCodes                , &
                AgLand%iBudgetCrops             , &
                AgLand%MaxRootDepth             , &
                AgLand%iColRootDepthFrac        , &
                AgLand%RootDepth_P              , &
                AgLand%RootDepth                , &
                AgLand%iColAgDemand             , &
                STAT = ErrorCode                )
    
    !Close files
    CALL AgLand%RootDepthFracDataFile%Kill()
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
        CALL ReturnFracFile%ReadTSData(TimeStep,'Return flow fractions data',FileReadCode_Return,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ReuseFracFile%ReadTSData(TimeStep,'Reuse fractions data',FileReadCode_Reuse,iStat)          ;  IF (iStat .EQ. -1) RETURN
        
        !If new data is read, find min and max
        IF (FileReadCode_Return.EQ.0  .OR.  FileReadCode_Reuse.EQ.0) THEN
            DO indxElem=1,SIZE(AgLand%Crops%SMax,DIM=2)
                DO indxCrop=1,SIZE(AgLand%Crops%SMax,DIM=1)
                    rRT      = ReturnFracFile%rValues(AgLand%Crops%iColReturnFrac(indxCrop,indxElem))
                    rRU      = ReuseFracFile%rValues(AgLand%Crops%iColReuseFrac(indxCrop,indxElem))
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
    
    !Read root depth fractions data first, since a lot of stuff depends on them
    CALL AgLand%RootDepthFracDataFile%ReadTSData(TimeStep,iStat)  
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
            AgLand%Crops%Area(:,indxElem) = AgLand%LandUseDataFile%rValues(indxElem,2:)
        END DO
        !$OMP END PARALLEL DO
    END IF
       
    !Min. soil moisture data as irrigation trigger
    CALL AgLand%MinSoilMFile%ReadTSData(TimeStep,'Minimum soil moisture requirement data',FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lMinSoilM_Updated = AgLand%MinSoilMFile%lUpdated
    IF (lMinSoilM_Updated) THEN
        !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(indxElem) SCHEDULE(STATIC,100)
        DO indxElem=1,NElements
            AgLand%Crops%MinSoilM(:,indxElem) = AgLand%MinSoilMFile%rValues(AgLand%Crops%iColMinSoilM(:,indxElem))
        END DO
        !$OMP END PARALLEL DO
    END IF
    
    !Irrigation target soil moisture data
    CALL AgLand%TargetSoilMFile%ReadTSData(TimeStep,'Irrigation target soil moisture data',FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lTargetSoilM_Updated = AgLand%TargetSoilMFile%lUpdated
    
    !Leaching factors
    CALL AgLand%LeachFracFile%ReadTSData(TimeStep,'Leaching factor data',FileReadCode,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Make sure that irrigation target soil moisture is not less than minimum soil moisture
    IF (lMinSoilM_Updated .OR. lTargetSoilM_Updated) THEN
        DO indxElem=1,NElements
            WP  = WiltingPoint(indxElem)
            TAW = FieldCapacity(indxElem) - WP
            DO indxCrop=1,NCrops
                TargetSoilM = AgLand%TargetSoilMFile%rValues(AgLand%Crops%iColTargetSoilM(indxCrop,indxElem))
                rFrac       = AgLand%MinSoilMFile%rValues(AgLand%Crops%iColMinSoilM(indxCrop,indxElem))
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
              rFrac = AgLand%Crops%MinSoilM(indxCrop,indxElem)
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
          iIrigPeriod = IrigPeriodFile%iValues(AgLand%Crops%iColIrigPeriod(indxCrop,indxElem))
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
        CALL AgLand%LandUseDataFile%ReadTSData(iElem,iColumn,iPathNameIndex,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,rData,rOutputDates,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
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
          DemandFrac(indxElem,indxCrop) = NonPondedAg%Crops%ElemDemandFrac_Ag(NonPondedAg%iBudgetCrops(indxCrop),indxElem)
          Area(indxElem,indxCrop)       = NonPondedAg%Crops%Area(NonPondedAg%iBudgetCrops(indxCrop),indxElem)
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
        ElemValue                                       = (ElemGenericMoist * NonPondedAg%RootDepth(iCrop) - NonPondedAg%Crops%GMExcess(indxCrop,:)) * Area(:,indxCrop)
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
        ElemValue                                  = NonPondedAg%Crops%DemandRaw(iBudgetCrops(indxCrop),:)
        RDemandRaw(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RDemandRaw(indxLast+indxCrop)              = SUM(RDemandRaw(indxCrop:indxLast:NBudgetCrops))
        ElemValue                                  = NonPondedAg%Crops%Demand(iBudgetCrops(indxCrop),:)
        RDemand(indxCrop:indxLast:NBudgetCrops)    = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RDemand(indxLast+indxCrop)                 = SUM(RDemand(indxCrop:indxLast:NBudgetCrops))
        ElemValue                                  = NonPondedAg%Crops%ETAW(iBudgetCrops(indxCrop),:)
        RETAW(indxCrop:indxLast:NBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RETAW(indxLast+indxCrop)                   = SUM(RETAW(indxCrop:indxLast:NBudgetCrops))
        ElemValue                                  = NonPondedAg%Crops%ETP(iBudgetCrops(indxCrop),:)
        RETP(indxCrop:indxLast:NBudgetCrops)       = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RETP(indxLast+indxCrop)                    = SUM(RETP(indxCrop:indxLast:NBudgetCrops))
        ElemValue                                  = NonPondedAg%Crops%ETOth(iBudgetCrops(indxCrop),:)
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
    
    !Initialize
    NRegions     = AppGrid%NSubregions
    NElements    = AppGrid%NElements
    NBudgetCrops = NonPondedAg%NBudgetCrops
    iBudgetCrops = NonPondedAg%iBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    DummyArray   = 0.0
    
    !Compute subregional values
    DO indxCrop=1,NBudgetCrops
      iCrop                                           = iBudgetCrops(indxCrop)
      !Moisture at the beginning of the time step must be corrected for chnages due to acreage changes
      ElemValue                                       = (NonPondedAg%Crops%SoilM_Precip_P(iCrop,:) + NonPondedAg%Crops%SoilM_AW_P(iCrop,:) + NonPondedAg%Crops%SoilM_Oth_P(iCrop,:)) * Area(:,indxCrop) &
                                                       - NonPondedAg%Crops%SoilMCh(iCrop,:)                                                                                                                                               &
                                                       + NonPondedAg%Crops%PercCh(iCrop,:)
      RSoilM_P(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM_P(indxLast+indxCrop)                     = SUM(RSoilM_P(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = (NonPondedAg%Crops%SoilM_Precip(iCrop,:) + NonPondedAg%Crops%SoilM_AW(iCrop,:) + NonPondedAg%Crops%SoilM_Oth(iCrop,:)) * Area(:,indxCrop)
      RSoilM(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM(indxLast+indxCrop)                       = SUM(RSoilM(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = ElemPrecip * Area(:,indxCrop)
      RPrecip(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RPrecip(indxLast+indxCrop)                      = SUM(RPrecip(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NonPondedAg%Crops%Runoff(iCrop,:)
      RRunoff(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RRunoff(indxLast+indxCrop)                      = SUM(RRunoff(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NonPondedAg%Crops%Reuse(iCrop,:)
      RReuse(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReuse(indxLast+indxCrop)                       = SUM(RReuse(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NonPondedAg%Crops%ReturnFlow(iCrop,:)
      RReturn(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReturn(indxLast+indxCrop)                      = SUM(RReturn(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NonPondedAg%Crops%SoilMCh(iCrop,:)
      RSoilMCh(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilMCh(indxLast+indxCrop)                     = SUM(RSoilMCh(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NonPondedAg%Crops%PrecipInfilt(iCrop,:) + NonPondedAg%Crops%IrigInfilt(iCrop,:)
      RInfilt(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RInfilt(indxLast+indxCrop)                      = SUM(RInfilt(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NonPondedAg%Crops%ETa(iCrop,:)
      RETa(indxCrop:indxLast:NBudgetCrops)            = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETa(indxLast+indxCrop)                         = SUM(RETa(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = NonPondedAg%Crops%Perc(iCrop,:) + NonPondedAg%Crops%PercCh(iCrop,:)
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
    CLASS(NonPondedAgDatabaseType)    :: NonPondedAg
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(ETType),INTENT(IN)           :: ETData
    TYPE(RootZoneSoilType),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:,:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    LOGICAL,INTENT(IN)                :: lLakeElem(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    INTEGER                     :: indxElem,indxCrop,KunsatMethod,iElemID
    REAL(8)                     :: AchievedConv,Area,ETc(NonPondedAg%NCrops),HydCond,TotalPorosity,fRU,  &
                                   FieldCapacity,TotalPorosityCrop,FieldCapacityCrop,RootDepth,Lambda,   &
                                   Supply,WiltingPoint,WiltingPointCrop,rSoilM_P,SoilM,RootDepth_P,fRF,  &
                                   RootDepthFrac,Perc_RDAdj,GM,PrecipD,rMultip,GMElem,Excess,Inflow,     &
                                   ratio(3),SoilM_P_Array(3),SoilM_Array(3),Infilt(3),ETPartition(3),    &
                                   rRunoff,rReturnFlow,rPrecipInfilt,rIrigInfilt,rETa,rETAW,rETP,rETOth, &
                                   rPerc,rReuse,rGMExcess,rSoilM_Precip_P,rSoilM_AW_P,rSoilM_Oth_P
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('Simulating flows at non-ponded agricultural crop lands')
    
    !Simulate
    ASSOCIATE (pCrops => NonPondedAg%Crops)
        !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,ETData,Precip,GenericMoisture,DeltaT,pCrops,       &
        !$OMP                                  NonPondedAg,ElemSupply,ReturnFrac,ReuseFrac,ElemsToGW,SolverData,iStat) 
        !$OMP DO SCHEDULE(NONMONOTONIC:DYNAMIC,96)
        DO indxElem=1,AppGrid%NElements
            IF (lLakeElem(indxElem)) CYCLE
            WiltingPoint  = SoilsData(indxElem)%WiltingPoint
            FieldCapacity = SoilsData(indxElem)%FieldCapacity
            TotalPorosity = SoilsData(indxElem)%TotalPorosity
            HydCond       = SoilsData(indxElem)%HydCond
            Lambda        = SoilsData(indxElem)%Lambda
            KunsatMethod  = SoilsData(indxElem)%KunsatMethod
            ETc           = ETData%GetValues(pCrops%iColETc(:,indxElem))
            GMElem        = GenericMoisture(1,indxElem) * DeltaT
            PrecipD       = Precip(indxElem) * DeltaT
            DO indxCrop=1,NonPondedAg%NCrops
                !Cycle if Area is zero
                Area = pCrops%Area(indxCrop,indxElem)
                IF (Area .EQ. 0.0) THEN
                    pCrops%Runoff(indxCrop,indxElem)       = 0.0
                    pCrops%ReturnFlow(indxCrop,indxElem)   = 0.0
                    pCrops%PrecipInfilt(indxCrop,indxElem) = 0.0                     
                    pCrops%IrigInfilt(indxCrop,indxElem)   = 0.0                          
                    pCrops%ETa(indxCrop,indxElem)          = 0.0
                    pCrops%ETAW(indxCrop,indxElem)         = 0.0
                    pCrops%ETP(indxCrop,indxElem)          = 0.0 
                    pCrops%ETOth(indxCrop,indxElem)        = 0.0
                    pCrops%Perc(indxCrop,indxElem)         = 0.0
                    pCrops%Reuse(indxCrop,indxElem)        = 0.0
                    pCrops%GMExcess(indxCrop,indxElem)     = 0.0
                    CYCLE
                END IF
                
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
                rSoilM_Precip_P   = pCrops%SoilM_Precip_P(indxCrop,indxElem)
                rSoilM_AW_P       = pCrops%SoilM_AW_P(indxCrop,indxElem)
                rSoilM_Oth_P      = pCrops%SoilM_Oth_P(indxCrop,indxElem)
                rSoilM_P          = rSoilM_Precip_P + rSoilM_AW_P + rSoilM_Oth_P
                Perc_RDAdj        = rSoilM_P * (1d0 - RootDepthFrac)
                rSoilM_P          = rSoilM_P * RootDepthFrac
                
                !Infiltration and return flow due to applied water
                fRF         = ReturnFrac(pCrops%iColReturnFrac(indxCrop,indxElem))
                fRU         = ReuseFrac(pCrops%iColReuseFrac(indxCrop,indxElem))
                rIrigInfilt = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
                rReturnFlow = Supply - rIrigInfilt
                
                !Inflow into root zone
                Inflow = GM + rIrigInfilt 
                
                !Simulate
                CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                               pCrops%SMax(indxCrop,indxElem)         ,  &
                                               rSoilM_P                               ,  &
                                               ETc(indxCrop)*DeltaT                   ,  & 
                                               HydCond                                ,  & 
                                               TotalPorosityCrop                      ,  & 
                                               FieldCapacityCrop                      ,  &
                                               WiltingPointCrop                       ,  & 
                                               Lambda                                 ,  & 
                                               Inflow                                 ,  &
                                               SolverData%Tolerance*TotalPorosityCrop ,  &
                                               KunsatMethod                           ,  &
                                               SolverData%IterMax                     ,  &
                                               SoilM                                  ,  & 
                                               rRunoff                                ,  & 
                                               rPrecipInfilt                          ,  & 
                                               rETa                                   ,  & 
                                               rPerc                                  ,  & 
                                               Excess                                 ,  &
                                               AchievedConv                           )
                                              
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
                rGMExcess = 0.0
                IF (Excess .NE. 0.0) THEN
                    ratio = [rPrecipInfilt , rIrigInfilt , GM]
                    CALL NormalizeArray(ratio)
                    rRunoff       = rRunoff     + Excess * ratio(1) 
                    rReturnFlow   = rReturnFlow + Excess * ratio(2)
                    rGMExcess     = Excess * ratio(3)
                    rPrecipInfilt = PrecipD - rRunoff
                    rIrigInfilt   = Supply  - rReturnFlow
                END IF
                
                !Compute re-use based on return flow
                rReuse = 0.0
                IF (fRF .GT. 0.0) rReuse = rReturnFlow * fRU / fRF
                
                !Compute moisture from precip and irrigation
                SoilM_P_Array = [rSoilM_Precip_P * RootDepthFrac , rSoilM_AW_P * RootDepthFrac , rSoilM_Oth_P * RootDepthFrac]
                Infilt        = [rPrecipInfilt                   , rIrigInfilt                 , GM - rGMExcess              ]
                CALL TrackMoistureDueToSource(SoilM_P_Array , &
                                              Infilt        , &
                                              rPerc         , &
                                              rETa          , &
                                              0d0           , &
                                              SoilM_Array   , &
                                              ETPartition   )
                pCrops%SoilM_Precip(indxCrop,indxElem) = SoilM_Array(1)
                pCrops%SoilM_AW(indxCrop,indxElem)     = SoilM_Array(2)
                pCrops%SoilM_Oth(indxCrop,indxElem)    = SoilM_Array(3)
                rETP          = ETPartition(1)                
                rETAW         = ETPartition(2)
                rETOth        = ETPartition(3)
                
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
                    EXIT
                END IF
                
                !Convert depths to volumetric rates
                rMultip       = Area / DeltaT 
                rRunoff       = rRunoff            * rMultip
                rReturnFlow   = rReturnFlow        * rMultip
                rPrecipInfilt = rPrecipInfilt      * rMultip
                rIrigInfilt   = rIrigInfilt        * rMultip
                rETa          = rETa               * rMultip
                rPerc         = (rPerc+Perc_RDAdj) * rMultip  
                rReuse        = rReuse             * rMultip
                rETAW         = rETAW              * rMultip
                rETP          = rETP               * rMultip
                rETOth        = rETOth             * rMultip
                
                !If surface flow goes to groundwater, update the runoff processes
                IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
                    rPerc         = rPerc + rRunoff + rReturnFlow
                    rPrecipInfilt = rPrecipInfilt + rRunoff        !Runoff and 
                    rIrigInfilt   = rIrigInfilt + rReturnFlow      ! return flow are assumed to bypass root zone for proper mass balance       
                    rRunoff       = 0.0
                    rReturnFlow   = 0.0
                END IF
             
                !Store results in persistent arrays
                pCrops%Runoff(indxCrop,indxElem)       = rRunoff
                pCrops%ReturnFlow(indxCrop,indxElem)   = rReturnFlow
                pCrops%PrecipInfilt(indxCrop,indxElem) = rPrecipInfilt                     
                pCrops%IrigInfilt(indxCrop,indxElem)   = rIrigInfilt                          
                pCrops%ETa(indxCrop,indxElem)          = rETa
                pCrops%ETAW(indxCrop,indxElem)         = rETAW
                pCrops%ETP(indxCrop,indxElem)          = rETP 
                pCrops%ETOth(indxCrop,indxElem)        = rETOth
                pCrops%Perc(indxCrop,indxElem)         = rPerc
                pCrops%Reuse(indxCrop,indxElem)        = rReuse
                pCrops%GMExcess(indxCrop,indxElem)     = rGMExcess

            END DO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
    END ASSOCIATE
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE NON-PONDED AG DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(NonPondedAg,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,SpecifiedDemand,ReuseFrac,ReturnFrac,IrigPeriod,SolverData,lLakeElem,lReadAgWaterDemand,iStat)
    CLASS(NonPondedAgDatabaseType)    :: NonPondedAg
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(ETType)                      :: ETData
    TYPE(RootZoneSoilType),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:),GenericMoisture(:,:),SpecifiedDemand(:),ReuseFrac(:),ReturnFrac(:)
    INTEGER,INTENT(IN)                :: IrigPeriod(:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    LOGICAL,INTENT(IN)                :: lLakeElem(:),lReadAgWaterDemand
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'ComputeWaterDemand'
    INTEGER                      :: indxElem,indxCrop,KunsatMethod,iElemID
    REAL(8)                      :: Area,ETc(NonPondedAg%NCrops),RootDepth,TotalPorosityCrop,TotalPorosity,  &
                                    FieldCapacity,AchievedConv,HydCond,PrecipDepth,Lambda,SoilM,TargetSoilM, &
                                    WiltingPoint,WiltingPointCrop,FieldCapacityCrop,RootDepth_P,Runoff,ETa,  &
                                    SoilM_P,RootDepthFrac,GMElem,GM,PrecipD,TAW,PrecipInfilt,Perc,Excess
    
    !Initialize
    iStat = 0
    
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,Precip,ETData,GenericMoisture,lReadAgWaterDemand,DeltaT, &
    !$OMP                                  SolverData,NonPondedAg,SpecifiedDemand,ReuseFrac,ReturnFrac,IrigPeriod,iStat)        
    !$OMP DO SCHEDULE(DYNAMIC,200)
    DO indxElem=1,AppGrid%NElements
        NonPondedAg%Crops%DemandRaw(:,indxElem) = 0.0
        NonPondedAg%Crops%Demand(:,indxElem)    = 0.0
        IF (lLakeElem(indxElem)) CYCLE
        WiltingPoint  = SoilsData(indxElem)%WiltingPoint
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        TAW           = FieldCapacity - WiltingPoint
        HydCond       = SoilsData(indxElem)%HydCond
        Lambda        = SoilsData(indxElem)%Lambda
        KunsatMethod  = SoilsData(indxElem)%KunsatMethod
        PrecipDepth   = Precip(indxElem)*DeltaT
        ETc           = ETData%GetValues(NonPondedAg%Crops%iColETc(:,indxElem))
        GMElem        = GenericMoisture(1,indxElem) * DeltaT
        PrecipD       = Precip(indxElem) * DeltaT
        DO indxCrop=1,NonPondedAg%NCrops
            !Cycle if Area is zero
            Area = NonPondedAg%Crops%Area(indxCrop,indxElem)
            IF (Area .EQ. 0.0) CYCLE
          
            !Cycle if demand is specified 
            IF (lReadAgWaterDemand) THEN
                IF (NonPondedAg%iColAgDemand(indxCrop,indxElem) .GT. 0) THEN
                    NonPondedAg%Crops%DemandRaw(indxCrop,indxElem) = SpecifiedDemand(NonPondedAg%iColAgDemand(indxCrop,indxElem))
                    NonPondedAg%Crops%Demand(indxCrop,indxElem)    = NonPondedAg%Crops%DemandRaw(indxCrop,indxElem)
                    CYCLE
                END IF
            END IF
              
            !Cycle if it is not an irrigation period
            IF (IrigPeriod(NonPondedAg%Crops%iColIrigPeriod(indxCrop,indxElem)) .EQ. f_iNoIrigPeriod) CYCLE
            
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
            SoilM_P = (NonPondedAg%Crops%SoilM_Precip_P(indxCrop,indxElem) + NonPondedAg%Crops%SoilM_AW_P(indxCrop,indxElem) + NonPondedAg%Crops%SoilM_Oth_P(indxCrop,indxElem)) * RootDepthFrac

            !Cycle if no need for demand computation; i.e. moisture is above required minimum
            SELECT CASE (NonPondedAg%iDemandFromMoist)
                CASE (f_iDemandFromMoistAtBegin)
                    IF (SoilM_P  .GE.  (NonPondedAg%Crops%MinSoilM(indxCrop,indxElem)*TAW + WiltingPoint) * RootDepth) CYCLE
                  
                CASE (f_iDemandFromMoistAtEnd)
                    CALL NonPondedLUMoistureRouter(PrecipD                                            ,  &
                                                   NonPondedAg%Crops%SMax(indxCrop,indxElem)          ,  &
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
                    IF (SoilM  .GE.  (NonPondedAg%Crops%MinSoilM(indxCrop,indxElem)*TAW + WiltingPoint) * RootDepth) CYCLE                    
            END SELECT  
              
            !Initialize
            SoilM             = (NonPondedAg%Crops%SoilM_Precip_P(indxCrop,indxElem) + NonPondedAg%Crops%SoilM_AW_P(indxCrop,indxElem) + NonPondedAg%Crops%SoilM_Oth_P(indxCrop,indxElem)) * RootDepthFrac
            WiltingPointCrop  = WiltingPoint  * RootDepth
            FieldCapacityCrop = FieldCapacity * RootDepth
            TotalPorosityCrop = TotalPorosity * RootDepth
            TargetSoilM       = MIN(FieldCapacityCrop * NonPondedAg%TargetSoilMFile%rValues(NonPondedAg%Crops%iColTargetSoilM(indxCrop,indxElem)) , TotalPorosityCrop)
          
            !Compute demand
            CALL NonPondedCropDemand(PrecipDepth                                                                           ,  &
                                     NonPondedAg%Crops%SMax(indxCrop,indxElem)                                             ,  &
                                     GM                                                                                    ,  &
                                     ReturnFrac(NonPondedAg%Crops%iColReturnFrac(indxCrop,indxElem))                       ,  &
                                     ReuseFrac(NonPondedAg%Crops%iColReuseFrac(indxCrop,indxElem))                         ,  &
                                     NonPondedAg%LeachFracFile%rValues(NonPondedAg%Crops%iColLeachFrac(indxCrop,indxElem)) ,  &
                                     ETc(indxCrop)*DeltaT                                                                  ,  &
                                     HydCond                                                                               ,  &
                                     TotalPorosityCrop                                                                     ,  &
                                     FieldCapacityCrop                                                                     ,  &
                                     WiltingPointCrop                                                                      ,  &      
                                     TargetSoilM                                                                           ,  & 
                                     Lambda                                                                                ,  & 
                                     SoilM                                                                                 ,  &
                                     SolverData%Tolerance*TotalPorosityCrop                                                ,  &
                                     KunsatMethod                                                                          ,  &
                                     SolverData%IterMax                                                                    ,  &
                                     NonPondedAg%Crops%DemandRaw(indxCrop,indxElem)                                        ,  &
                                     NonPondedAg%Crops%Demand(indxCrop,indxElem)                                           ,  &
                                     AchievedConv                                                                          )
                                     
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
            NonPondedAg%Crops%DemandRaw(indxCrop,indxElem) = NonPondedAg%Crops%DemandRaw(indxCrop,indxElem) / DeltaT * Area
            NonPondedAg%Crops%Demand(indxCrop,indxElem)    = NonPondedAg%Crops%Demand(indxCrop,indxElem)    / DeltaT * Area
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
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
    ASSOCIATE (pCrops => NonPondedAgLand%Crops) 
        DO indxElem=1,NElements
            TP = TotalPorosity(indxElem)
            DO indxCrop=1,NCrops
                IF (pCrops%SoilM_Precip(indxCrop,indxElem) + pCrops%SoilM_AW(indxCrop,indxElem) + pCrops%SoilM_Oth(indxCrop,indxElem) .GT. TP) THEN
                    CALL SetLastMessage('Initial moisture content for crop ' // TRIM(NonPondedAgLand%CropCodes(indxCrop)) // ' at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
            pCrops%SoilM_Precip(:,indxElem)   = pCrops%SoilM_Precip(:,indxElem) * RootDepth
            pCrops%SoilM_Precip_P(:,indxElem) = pCrops%SoilM_Precip(:,indxElem)
            pCrops%SoilM_AW(:,indxElem)       = pCrops%SoilM_AW(:,indxElem) * RootDepth
            pCrops%SoilM_AW_P(:,indxElem)     = pCrops%SoilM_AW(:,indxElem)
            pCrops%SoilM_Oth(:,indxElem)      = pCrops%SoilM_Oth(:,indxElem) * RootDepth
            pCrops%SoilM_Oth_P(:,indxElem)    = pCrops%SoilM_Oth(:,indxElem)
        END DO 
    END ASSOCIATE
    
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
    DO indxElem=1,SIZE(NonPondedAgLand%Crops%SMax,DIM=2)
        DO indxCrop=1,NonPondedAgLand%NCrops
            iCol = NonPondedAgLand%iColAgDemand(indxCrop,indxElem)
            IF (iCol .GT. 0) THEN
                IF (rSpecifiedDemand(iCol) .GT. 0.0) THEN
                    IF (NonPondedAgLand%Crops%Area(indxCrop,indxElem) .EQ. 0.0) THEN
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
    CALL NonPondedAgLand%RootDepthFracDataFile%ReadTSData(TimeStep,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    CALL NonPondedAgLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                             ;  IF (iStat .NE. 0) RETURN
    CALL NonPondedAgLand%LandUseDataFile%ReadTSData('Non-ponded crop areas',TimeStep,rElemAreas,iElemIDs,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL NonPondedAgLand%MinSoilMFile%File%RewindFile_To_BeginningOfTSData(iStat)                                                ;  IF (iStat .NE. 0) RETURN
    CALL NonPondedAgLand%MinSoilMFile%ReadTSData(TimeStep,'Minimum soil moisture requirement data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN

    IF (NonPondedAgLand%TargetSoilMFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL NonPondedAgLand%TargetSoilMFile%File%RewindFile_To_BeginningOfTSData(iStat)                                              ;  IF (iStat .NE. 0) RETURN
        CALL NonPondedAgLand%TargetSoilMFile%ReadTSData(TimeStep,'Irrigation target soil moisture data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    END IF
    
    IF (NonPondedAgLand%LeachFracFile%File%iGetFileType() .NE. f_iUNKNOWN) THEN
        CALL NonPondedAgLand%LeachFracFile%File%RewindFile_To_BeginningOfTSData(iStat)                              ;  IF (iStat .NE. 0) RETURN
        CALL NonPondedAgLand%LeachFracFile%ReadTSData(TimeStep,'Leaching factor data',iFileReadCode,iStat)
    END IF
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp

END MODULE