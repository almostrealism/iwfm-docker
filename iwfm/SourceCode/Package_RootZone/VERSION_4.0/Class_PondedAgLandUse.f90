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
MODULE Class_PondedAgLandUse
  !$ USE OMP_LIB
  USE MessageLogger           , ONLY: SetLastMessage                    , &
                                      EchoProgress                      , &
                                      MessageArray                      , &
                                      f_iFatal                            
  USE TimeSeriesUtilities     , ONLY: TimeStepType                      , &
                                      IncrementTimeStamp                , &
                                      CTimeStep_To_RTimeStep            
  USE IOInterface             , ONLY: GenericFileType                   
  USe GeneralUtilities        , ONLY: StripTextUntilCharacter           , &
                                      UpperCase                         , &
                                      IntToText                         , &
                                      CleanSpecialCharacters            , &
                                      NormalizeArray                    , &
                                      ShellSort                         , &
                                      LocateInList                      , &
                                      EstablishAbsolutePathFileName     
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource          
  USE Class_GenericLandUse    , ONLY: GenericLandUseType                
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType               
  USE Package_Discretization  , ONLY: AppGridType                       
  USE Package_PrecipitationET , ONLY: ETType  
  USE Util_Package_RootZone   , ONLY: WaterSupplyType                   , &
                                      ReadRealData                      , &
                                      ReadPointerData                   , &
                                      f_iNoIrigPeriod                   , &
                                      f_iBudgetType_PondedCrop_LWU      , & 
                                      f_iBudgetType_PondedCrop_RZ 
  USE Util_RootZone_v40       , ONLY: AgRootZoneBudRawFile_New          , &
                                      AgLWUseBudRawFile_New             , &
                                      f_iNAgLWUseBudColumns             , &
                                      f_iNAgRootZoneBudColumns          
  USE Package_UnsatZone       , ONLY: RootZoneSoilType                  , &
                                      PondedCropDemand                  , &
                                      PondedLUMoistureRouter            
  USE Package_Misc            , ONLY: RealTSDataInFileType              , &
                                      IntTSDataInFileType               , &
                                      SolverDataType                    , &
                                      ReadTimeSeriesData  => ReadTSData , &
                                      f_iFlowDest_GWElement
  USE Package_Budget          , ONLY: BudgetType                        , &
                                      f_iMaxLocationNameLen             
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
  PUBLIC :: PondedAgDatabaseType  ,  &                       
            f_iNPondedCrops                              
            
  
  ! -------------------------------------------------------------
  ! --- STATIC PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER          :: f_iNPondedCrops             = 5 , &
                                f_iindxRice_FloodDecomp     = 1 , &
                                f_iindxRice_NonFloodDecomp  = 2 , &
                                f_iindxRice_NoDecomp        = 3 , &
                                f_iindxRefuge_Seasonal      = 4 , &
                                f_iindxRefuge_Permanent     = 5 
  INTEGER,PARAMETER          :: f_iLenCropCode              = 9
  CHARACTER(LEN=f_iLenCropCode),PARAMETER :: f_cCropCodes(f_iNPondedCrops)  = ['RICE_FL','RICE_NFL','RICE_NDC','REFUGE_SL','REFUGE_PR'] 
                       
                       
  ! -------------------------------------------------------------
  ! --- PONDED LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: PondedAgType
    INTEGER :: iColReturn           = 0     !Column number in the rice/refuge operations data file for return flow depth
    INTEGER :: iColReuse            = 0     !Column number in the rice/refuge operations data file for reuse depth
    INTEGER :: iColPondDepth        = 0     !Column number in the rice/refuge operations data file for ponding depth
    INTEGER :: iColIrigPeriod       = 0     !Pointer to irrigation period data file for (crop,element) combination
    REAL(8) :: PondDepth            = 0.0   !Actual ponding depth
    REAL(8) :: Drain                = 0.0   !Drainage flow from ponds
    REAL(8) :: ReturnFlow           = 0.0   !Return flow
    REAL(8) :: IrigInfilt           = 0.0   !Infiltration due to irrigation
    REAL(8) :: Reuse                = 0.0   !Reused return flow 
    REAL(8) :: ETAW                 = 0.0   !ET of applied water
    REAL(8) :: ETP                  = 0.0   !ET of precipitation
    REAL(8) :: ETOth                = 0.0   !ET of other sources of moisture
    REAL(8) :: DemandRaw            = 0.0   !Water demand before the return flow is included
    REAL(8) :: Demand               = 0.0   !Water demand after the return flow is included 
    REAL(8) :: ElemDemandFrac       = 0.0   !Ratio of crop demand to the total demand at the element it is located at
    REAL(8) :: ElemDemandFrac_Ag    = 0.0   !Ratio of crop demand to the total "ag" demand at the element it is located at
  END TYPE PondedAgType
  


  ! -------------------------------------------------------------
  ! --- PONDED LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE PondedAgDatabaseType
    INTEGER                        :: NCrops                      = f_iNPondedCrops  !Number of ponded crops
    TYPE(PondedAgType),ALLOCATABLE :: Crops(:,:)                                     !Ponded crops for each (crop,element) combination
    CHARACTER(LEN=f_iLenCropCode)  :: CropCodes(f_iNPondedCrops)  = f_cCropCodes     !Ponded crop codes
    INTEGER                        :: NBudgetCrops                = 0                !Number of ponded crops for budget output
    INTEGER,ALLOCATABLE            :: iBudgetCrops(:)                                !Indices of ponded crops for budget output
    INTEGER,ALLOCATABLE            :: iColAgDemand(:,:)                              !Pointer to ag water demand file for each (crop,element) combination
    INTEGER,ALLOCATABLE            :: iColNonFloodRiceDecompAW(:)                    !Column number in the rice/refuge operations data file for non-floooded rice decomp water application depth for each (element)
    REAL(8),ALLOCATABLE            :: RegionETPot(:,:)                               !Regional potential ET for each (crop,region) combination
    REAL(8),ALLOCATABLE            :: RootDepth(:)                                   !Rooting depth for each ponded crop
    REAL(8)                        :: PondDepthFactor             = 1.0              !Conversion factor for rice/refuge pond depths
    REAL(8)                        :: OperationFlowsFactor        = 1.0              !Conversion factor for rice/refuge operation flows
    TYPE(LandUseDataFileType)      :: LandUseDataFile                                !Land use data file
    TYPE(RealTSDataInFileType)     :: PondDepthFile                                  !Rice/refuge pond depths data file
    TYPE(RealTSDataInFileType)     :: OperationFlowsFile                             !Rice/refuge operations flow (application for non-flooded rice decomp, return flow and re-use flow) data file
    LOGICAL                        :: lLWUseBudRawFile_Defined    = .FALSE.          !Flag to see if the land and water use file is defined
    TYPE(BudgetType)               :: LWUseBudRawFile                                !Raw binary file for ponded-ag land and water use budget
    LOGICAL                        :: lRootZoneBudRawFile_Defined = .FALSE.          !Flag to see if the root zone budget file is defined
    TYPE(BudgetType)               :: RootZoneBudRawFile                             !Raw binary file for ponded-ag root zone budget
  CONTAINS
    PROCEDURE,PASS :: New                          
    PROCEDURE,PASS :: Kill                         
    PROCEDURE,PASS :: GetBudget_TSData                     
    PROCEDURE,PASS :: SetAreas                     
    PROCEDURE,PASS :: PrintResults                 
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadTotalElemArea
    PROCEDURE,PASS :: ReadTSData                   
    PROCEDURE,PASS :: AdvanceAreas                 
    PROCEDURE,PASS :: SoilMContent_To_Depth        
    PROCEDURE,PASS :: ComputeWaterDemand           
    PROCEDURE,PASS :: CheckSpecifiedDemandAndArea
    PROCEDURE,PASS :: Simulate                     
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp                     
  END TYPE PondedAgDatabaseType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_PondedAgLandUse::'




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
  ! --- NEW PONDED AG LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(PondLand,IsForInquiry,cFileName,cWorkingDirectory,FactCN,AppGrid,iElemIDs,TimeStep,NTimeSteps,cVersion,iStat)
    CLASS(PondedAgDatabaseType)   :: PondLand
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)            :: FACTCN
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTimeSteps,iElemIDs(AppGrid%NElements)
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)                      :: ThisProcedure = ModName // 'New'
    CHARACTER                                        :: ALine*1000,cBudgetCropCode*f_iLenCropCode
    CHARACTER(LEN=f_iMaxLocationNameLen),ALLOCATABLE :: cRegionNames(:)
    CHARACTER(LEN=f_iMaxLocationNameLen)             :: SubRegionNames(AppGrid%NSubregions+1)
    REAL(8)                                          :: FACT,Factor(1),SubRegionArea(AppGrid%NSubregions+1)
    INTEGER                                          :: ErrorCode,indxCrop,indxElem,NBudgetCrops,NBudgetRegions,NElements, &
                                                        NRegions,indxCropP,indxRegion,iElem,ID
    INTEGER,ALLOCATABLE                              :: DummyIntArray(:,:)
    REAL(8),ALLOCATABLE                              :: DummyRealArray(:,:),RegionAreas(:)
    TYPE(GenericFileType)                            :: RiceRefugeDataFile
    LOGICAL                                          :: lCropFound,TrackTime,lProcessed(AppGrid%NElements)
    CHARACTER(:),ALLOCATABLE                         :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no filename is specified
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
    CALL RiceRefugeDataFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (PondLand%Crops(f_iNPondedCrops,NElements)         ,  &
              PondLand%RegionETPot(f_iNPondedCrops,NRegions)    ,  &
              PondLand%RootDepth(f_iNPondedCrops)               ,  &
              PondLand%iColNonFloodRiceDecompAW(NElements)      ,  &
              STAT=ErrorCode                                    )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for ponded agricultural data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Land use data file
    CALL RiceRefugeDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL PondLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Ponded ag. area file',NElements,f_iNPondedCrops,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Crops for budget output
    CALL RiceRefugeDataFile%ReadData(NBudgetCrops,iStat)  ;  IF (iStat .EQ. -1) RETURN
    PondLand%NBudgetCrops = NBudgetCrops
    IF (NBudgetCrops .GT. 0) THEN
      !Number of budget regions
      NBudgetRegions = (AppGrid%NSubregions+1) * NBudgetCrops
      
      !Allocate memory
      ALLOCATE (PondLand%iBudgetCrops(NBudgetCrops)   ,  &
                cRegionNames(NBudgetRegions)          ,  &
                RegionAreas(NBudgetRegions)           ,  &
                STAT=ErrorCode                        )
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for ponded crops budget output data!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Find the indices for budget-print-out crops
      DO indxCrop=1,NBudgetCrops
        CALL RiceRefugeDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  cBudgetCropCode = ADJUSTL(StripTextUntilCharacter(ALine,'/'))  ;  CALL CleanSpecialCharacters(cBudgetCropCode)
        lCropFound = .FALSE.
        DO indxCropP=1,f_iNPondedCrops
          IF (UpperCase(cBudgetCropCode) .EQ. UpperCase(f_cCropCodes(indxCropP))) THEN
            PondLand%iBudgetCrops(indxCrop) = indxCropP
            lCropFound                      = .TRUE.
            EXIT
          END IF
        END DO
        IF (.NOT. lCropFound) THEN
            CALL SetLastMessage (TRIM(cBudgetCropCode)//' for water budget output is not defined as a ponded crop!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
      END DO
      
      !Order the crop indicies for budget output
      CALL ShellSort(PondLand%iBudgetCrops)
      
      !Region names and areas
      DO indxCrop=1,NBudgetCrops
        indxCropP = PondLand%iBudgetCrops(indxCrop)
        DO indxRegion=1,NRegions+1
          cRegionNames((indxRegion-1)*NBudgetCrops+indxCrop) = TRIM(SubRegionNames(indxRegion)) // '_' // TRIM(UpperCase(f_cCropCodes(indxCropP)))
          RegionAreas((indxRegion-1)*NBudgetCrops+indxCrop)  = SubregionArea(indxRegion)
        END DO
      END DO
    END IF
    
    !Land and water use budget
    CALL RiceRefugeDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (NBudgetCrops .GT. 0) THEN
      IF (ALine .NE. '') THEN
          CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
          CALL AgLWUseBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'land and water use budget for specific ponded crops',cVersion,PondLand%LWUseBudRawFile,iStat)
          IF (iStat .EQ. -1) RETURN
          PondLand%lLWUseBudRawFile_Defined = .TRUE.
      END IF
    END IF
    
    !Root zone budget
    CALL RiceRefugeDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (NBudgetCrops .GT. 0) THEN
      IF (ALine .NE. '') THEN
          CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
          CALL AgRootZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'root zone budget for specific ponded crops',cVersion,PondLand%RootZoneBudRawFile,iStat)
          IF (iStat .EQ. -1) RETURN
          PondLand%lRootZoneBudRawFile_Defined = .TRUE.
      END IF
    END IF

    !Root depths
    CALL RiceRefugeDataFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indxCrop=1,f_iNPondedCrops
      CALL RiceRefugeDataFile%ReadData(PondLand%RootDepth(indxCrop),iStat)  ;  IF (iStat .EQ. -1) RETURN
      PondLand%RootDepth(indxCrop) = PondLand%RootDepth(indxCrop) * FACT
    END DO
    
    !Curve numbers
    CALL ReadRealData(RiceRefugeDataFile,'curve numbers for ponded crops','elements',NElements,f_iNPondedCrops+1,iElemIDs,DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyRealArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Curve numbers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)            = .TRUE.
        PondLand%Crops(:,iElem)%SMax = (1000.0/DummyRealArray(indxElem,2:)-10.0) * FACTCN
    END DO
       
    !ETc
    CALL ReadPointerData(RiceRefugeDataFile,'evapotranspiration column pointers for ponded crops','elements',NElements,f_iNPondedCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyRealArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Evapotranspiration column pointers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)               = .TRUE.
        PondLand%Crops(:,iElem)%iColETc = DummyIntArray(indxElem,2:)
    END DO
    
    !Water demand pointers
    ALLOCATE (PondLand%iColAgDemand(f_iNPondedCrops,NElements))
    CALL ReadPointerData(RiceRefugeDataFile,'water supply requirement column pointers for ponded crops','elements',NElements,f_iNPondedCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Water supply requirement column pointers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)              = .TRUE.
        PondLand%iColAgDemand(:,iElem) = DummyIntArray(indxElem,2:)
    END DO
    
    !Irrigation period pointers
    CALL ReadPointerData(RiceRefugeDataFile,'irrigation period column pointers for ponded crops','elements',NElements,f_iNPondedCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Irrigation period column pointers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                      = .TRUE.
        PondLand%Crops(:,iElem)%iColIrigPeriod = DummyIntArray(indxElem,2:)
    END DO

    !Ponding depth data file
    CALL RiceRefugeDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL PondLand%PondDepthFile%Init(cAbsPathFileName,cWorkingDirectory,'rice/refuge ponding depth data file',TrackTime,1,.TRUE.,Factor,[.FALSE.],iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    PondLand%PondDepthFactor = Factor(1)
    
    !Operations flow data file
    CALL RiceRefugeDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL PondLand%OperationFlowsFile%Init(cAbsPathFileName,cWorkingDirectory,'rice/refuge operations flow data file',TrackTime,1,.TRUE.,Factor,[.TRUE.],iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    PondLand%OperationFlowsFactor = Factor(1)
    
    !Ponding depths
    CALL ReadPointerData(RiceRefugeDataFile,'ponding depth column pointers for ponded crops','elements',NElements,6,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Ponding depth column pointers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                                           = .TRUE.
        PondLand%Crops(f_iindxRice_FloodDecomp,iElem)%iColPondDepth    = DummyIntArray(indxElem,2) 
        PondLand%Crops(f_iindxRice_NonFloodDecomp,iElem)%iColPondDepth = DummyIntArray(indxElem,3)
        PondLand%Crops(f_iindxRice_NoDecomp,iElem)%iColPondDepth       = DummyIntArray(indxElem,4)
        PondLand%Crops(f_iindxRefuge_Seasonal,iElem)%iColPondDepth     = DummyIntArray(indxElem,5)
        PondLand%Crops(f_iindxRefuge_Permanent,iElem)%iColPondDepth    = DummyIntArray(indxElem,6)
    END DO

    !Non-flooded decomposition application depths
    CALL ReadPointerData(RiceRefugeDataFile,'non-flooded rice decomposition water column pointers for ponded crops','elements',NElements,2,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Non-flooded rice decomposition water column pointers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                        = .TRUE.
        PondLand%iColNonFloodRiceDecompAW(iElem) = DummyIntArray(indxElem,2)
    END DO
        
    !Return flow depths
    CALL ReadPointerData(RiceRefugeDataFile,'return flow depth column pointers for ponded crops','elements',NElements,f_iNPondedCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Return flow depth column pointers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                  = .TRUE.
        PondLand%Crops(:,iElem)%iColReturn = DummyIntArray(indxElem,2:) 
    END DO
      
    !Re-use water depths
    CALL ReadPointerData(RiceRefugeDataFile,'re-use depth column pointers for ponded crops','elements',NElements,f_iNPondedCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = DummyIntArray(indxElem,1)
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Re-use flow depth column pointers for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                 = .TRUE.
        PondLand%Crops(:,iElem)%iColReuse = DummyIntArray(indxElem,2:) 
    END DO
    
    !Check for time-series column pointer errors
    DO indxElem=1,NElements
        ID = iElemIDs(indxElem)
        CALL PondLand%PondDepthFile%CheckColNum('Pond depth file as referenced by element '//TRIM(IntToText(ID)),PondLand%Crops(:,indxElem)%iColPondDepth,.TRUE.,iStat)                                                                                                ;  IF (iStat .EQ. -1) RETURN
        CALL PondLand%OperationFlowsFile%CheckColNum('Rice/refuge pond operations flow file as referenced by element '//TRIM(IntToText(ID))//' for application depths for non-flooded rice decomposition',[PondLand%iColNonFloodRiceDecompAW(indxElem)],.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL PondLand%OperationFlowsFile%CheckColNum('Rice/refuge pond operations flow file as referenced by element '//TRIM(IntToText(ID))//' for return flow depths',PondLand%Crops(:,indxElem)%iColReturn,.TRUE.,iStat)                                             ;  IF (iStat .EQ. -1) RETURN
        CALL PondLand%OperationFlowsFile%CheckColNum('Rice/refuge pond operations flow file as referenced by element '//TRIM(IntToText(ID))//' for re-use flow depths',PondLand%Crops(:,indxElem)%iColReuse,.TRUE.,iStat)                                              ;  IF (iStat .EQ. -1) RETURN
    END DO
    
    !Initial conditions
    CALL ReadRealData(RiceRefugeDataFile,'initial conditions for ponded crops','elements',NElements,f_iNPondedCrops+2,iElemIDs,DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(DummyRealArray(:,2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyRealArray(:,2)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some fractions of initial soil moisture due to precipitation is less '
        MessageArray(2) = 'than 0.0 or greater than 1.0 for ponded agricultural crops!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    IF (MINVAL(DummyRealArray(:,3:)) .LT. 0.0) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 for ponded crops!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyRealArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Initial conditions for ponded crops at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                    = .TRUE.
        PondLand%Crops(:,iElem)%SoilM_Precip = DummyRealArray(indxElem,2) * DummyRealArray(indxElem,3:)
        PondLand%Crops(:,iElem)%SoilM_AW     = DummyRealArray(indxElem,3:) - PondLand%Crops(:,iElem)%SoilM_Precip
    END DO
    PondLand%Crops%SoilM_Precip_P_BeforeUpdate = PondLand%Crops%SoilM_Precip
    PondLand%Crops%SoilM_Precip_P              = PondLand%Crops%SoilM_Precip
    PondLand%Crops%SoilM_AW_P_BeforeUpdate     = PondLand%Crops%SoilM_AW
    PondLand%Crops%SoilM_AW_P                  = PondLand%Crops%SoilM_AW
    
    !Close file
    CALL RiceRefugeDataFile%Kill()
    
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
  ! --- KILL PONDED AG LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(PondLand)
    CLASS(PondedAgDatabaseType) :: PondLand
    
    !Local variables
    INTEGER                    :: ErrorCode
    TYPE(PondedAgDatabaseType) :: Dummy
    
    !Deallocate arrays
    DEALLOCATE (PondLand%Crops                    , &
                PondLand%iBudgetCrops             , &
                PondLand%iColAgDemand             , &
                PondLand%iColNonFloodRiceDecompAW , &
                PondLand%RootDepth                , &
                STAT = ErrorCode                  )
    
    !Close files
    CALL PondLand%LandUseDataFile%Kill()
    CALL PondLand%PondDepthFile%Close()
    CALL PondLand%OperationFlowsFile%Close()
    CALL PondLand%LWUseBudRawFile%Kill()
    CALL PondLand%RootZoneBudRawFile%Kill()
    
    !Assign default values to components
    SELECT TYPE (PondLand)
        TYPE IS (PondedAgDatabaseType)
            PondLand = Dummy
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
  SUBROUTINE GetBudget_TSData(PondLand,iBudgetType,iLocationIndex,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(PondedAgDatabaseType),TARGET,INTENT(IN) :: PondLand
    INTEGER,INTENT(IN)                            :: iBudgetType,iLocationIndex,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)                   :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                            :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                           :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                           :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER                  :: indx
    TYPE(BudgetTYpe),POINTER :: pBudget
    
    !Initialize
    NULLIFY(pBudget)
    
    !Pointer to budget file
    SELECT CASE (iBudgetType)
        CASE (f_iBudgetType_PondedCrop_LWU)
            IF (PondLand%lLWUseBudRawFile_Defined) pBudget => PondLand%LWUseBudRawFile
            
        CASE (f_iBudgetType_PondedCrop_RZ)
            IF (PondLand%lRootZoneBudRawFile_Defined) pBudget => PondLand%RootZoneBudRawFile
            
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
  SUBROUTINE SetAreas(PondedAgLand,Area)
    CLASS(PondedAgDatabaseType) :: PondedAgLand
    REAL(8),INTENT(IN)          :: Area(:,:)
   
    PondedAgLand%Crops%Area = Area
    
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
  SUBROUTINE ReadRestartData(PondedAg,InFile,iStat)
    CLASS(PondedAgDatabaseType):: PondedAg
    TYPE(GenericFileType)      :: InFile
    INTEGER,INTENT(OUT)        :: iStat
    
    CALL InFile%ReadData(PondedAg%Crops%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%ReturnFlow,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(PondedAg%Crops%SoilM_Oth,iStat)  
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR PONDED AG
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(PondLand,TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
    CLASS(PondedAgDataBaseType)   :: PondLand
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(IN)            :: iElemIDs(AppGrid%NElements)
    REAL(8),INTENT(IN)            :: rElemAreas(AppGrid%NElements)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReadTSData'
    INTEGER                      :: indxCrop,indxElem,FileReadCode
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Reading time series data for ponded agricultural crops')
    
    !Land use areas
    CALL PondLand%LandUseDataFile%ReadTSData('Ponded crop areas',TimeStep,rElemAreas,iElemIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (PondLand%LandUseDataFile%lUpdated) THEN
      DO indxElem=1,AppGrid%NElements
        PondLand%Crops(:,indxElem)%Area = PondLand%LandUseDataFile%rValues(indxElem,2:)
      END DO
    END IF
    
    !Ponding depths
    CALL ReadTimeSeriesData(TimeStep,'Rice/refuge pond depths data',PondLand%PondDepthFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode .EQ. 0) PondLand%PondDepthFile%rValues = PondLand%PondDepthFile%rValues * PondLand%PondDepthFactor
    
    !Operation flow depths
    CALL ReadTimeSeriesData(TimeStep,'Rice/refuge pond operation flows data',PondLand%OperationFlowsFile,FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode .EQ. 0) PondLand%OperationFlowsFile%rValues = PondLand%OperationFlowsFile%rValues * PondLand%OperationFlowsFactor
    
    !Make sure that return flow depth is larger than re-use depth
    ASSOCIATE (pPondOps => PondLand%OperationFlowsFile%rValues  , &
               pCrops   => PondLand%Crops                   )
    
        DO indxElem=1,AppGrid%NElements
            DO indxCrop=1,f_iNPondedCrops
                IF (pPondOps(pCrops(indxCrop,indxElem)%iColReturn) .LT. pPondOps(pCrops(indxCrop,indxElem)%iColReuse)) THEN
                    MessageArray(1) = 'Re-use depth for ' // TRIM(f_cCropCodes(indxCrop)) // ' at element ' // TRIM(IntToText(iElemIDs(indxElem)))//' is greater than return flow depth!'
                    WRITE (MessageArray(2),'(A,F5.3)') 'Re-use depth      = ',pPondOps(pCrops(indxCrop,indxElem)%iColReuse)
                    WRITE (MessageArray(3),'(A,F5.3)') 'Return flow depth = ',pPondOps(pCrops(indxCrop,indxElem)%iColReturn)
                    CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
        END DO
      
    END ASSOCIATE

  END SUBROUTINE ReadTSData
  

  ! -------------------------------------------------------------
  ! --- READ TOTAL PONDED AG LAND AREA AT AN ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE ReadTotalElemArea(PondLand,iElem,lForInquiry,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemAgLandUse,rOutputDates,iStat)
    CLASS(PondedAgDataBaseType) :: PondLand
    INTEGER,INTENT(IN)          :: iElem
    LOGICAL,INTENT(IN)          :: lForInquiry
    CHARACTER(LEN=*),INTENT(IN) :: cReadBeginDateAndTime,cReadEndDateAndTime
    INTEGER,INTENT(OUT)         :: nActualOutput,iStat
    REAL(8),INTENT(OUT)         :: ElemAgLandUse(:),rOutputDates(:)   
    
    !Local variables
    INTEGER :: iPathNameIndex,indxCrop,iOffset,ErrorCode,iColumn
    REAL(8) :: rData(SIZE(rOutputDates))
    
    !Initialize
    ElemAgLandUse = 0    
    iOffset       = (iElem-1) * PondLand%NCrops
    
    !Read and accumulate total ag lands
    DO indxCrop=1,PondLand%NCrops
        iPathNameIndex = iOffset + indxCrop
        iColumn        = indxCrop+ 1 !+1 because first column in ASCII file is reserved for element number
        CALL PondLand%LandUseDataFile%ReadData(iElem,iColumn,iPathNameIndex,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,rData,rOutputDates,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL PondLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .EQ. -1) RETURN
        ElemAgLandUse(1:nActualOutput) = ElemAgLandUse(1:nActualOutput) + rData(1:nActualOutput)
    END DO
    
    !Unit conversion
    ElemAgLandUse(1:nActualOutput) = ElemAgLandUse(1:nActualOutput) * PondLand%LandUseDataFile%Fact
    
    !Rewind land use file if it was opened for querying
    IF (lForInquiry) CALL PondLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)     
        
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
  SUBROUTINE PrintRestartData(PondedAg,OutFile)
    CLASS(PondedAgDatabaseType),INTENT(IN) :: PondedAg
    TYPE(GenericFileType)                  :: OutFile
    
    CALL OutFile%WriteData(PondedAg%Crops%Runoff)
    CALL OutFile%WriteData(PondedAg%Crops%ReturnFlow)
    CALL OutFile%WriteData(PondedAg%Crops%Area_P)
    CALL OutFile%WriteData(PondedAg%Crops%Area)
    CALL OutFile%WriteData(PondedAg%Crops%SoilM_Precip_P)
    CALL OutFile%WriteData(PondedAg%Crops%SoilM_Precip)
    CALL OutFile%WriteData(PondedAg%Crops%SoilM_AW_P)
    CALL OutFile%WriteData(PondedAg%Crops%SoilM_AW)
    CALL OutFile%WriteData(PondedAg%Crops%SoilM_Oth_P)
    CALL OutFile%WriteData(PondedAg%Crops%SoilM_Oth)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE FOR RESULTS PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(PondedAg,AppGrid,ElemSupply,ElemPrecip,ElemGenericMoist)
    CLASS(PondedAgDatabaseType)      :: PondedAg
    TYPE(AppGridType),INTENT(IN)     :: AppGrid
    TYPE(WaterSupplyType),INTENT(IN) :: ElemSupply(AppGrid%NElements)
    REAL(8),INTENT(IN)               :: ElemPrecip(AppGrid%NElements),ElemGenericMoist(AppGrid%NElements)
    
    !Local variables
    INTEGER                                                          :: NBudgetCrops,indxLast,indxCrop,indxElem
    REAL(8),DIMENSION(AppGrid%NElements)                             :: ElemValue
    REAL(8),DIMENSION(AppGrid%NElements,PondedAg%NBudgetCrops)       :: DemandFrac,Area
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*PondedAg%NBudgetCrops) :: RPump,RDeli,RRunoff,RLUArea
    
    !Return if no output is desired
    IF (PondedAg%NBudgetCrops .EQ. 0) RETURN
    
    !Initialize
    NBudgetCrops = PondedAg%NBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    RPump        = 0.0
    RDeli        = 0.0
    RRunoff      = 0.0
    RLUArea      = 0.0

    !Compute variables necessary for both land&water use and root zone budget files
    IF (PondedAg%lLWUseBudRawFile_Defined .OR. PondedAg%lRootZoneBudRawFile_Defined) THEN
      DO indxElem=1,AppGrid%NElements
        DO indxCrop=1,NBudgetCrops 
          DemandFrac(indxElem,indxCrop) = PondedAg%Crops(PondedAg%iBudgetCrops(indxCrop),indxElem)%ElemDemandFrac_Ag 
          Area(indxElem,indxCrop)       = PondedAg%Crops(PondedAg%iBudgetCrops(indxCrop),indxElem)%Area
        END DO
      END DO
      DO indxCrop=1,NBudgetCrops
        ElemValue                               = ElemSupply%Pumping_Ag * DemandFrac(:,indxCrop)
        RPump(indxCrop:indxLast:NBudgetCrops)   = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RPump(indxLast+indxCrop)                = RPump(indxLast+indxCrop) + SUM(RPump(indxCrop:indxLast:NBudgetCrops))
        ElemValue                               = ElemSupply%Diversion_Ag * DemandFrac(:,indxCrop)
        RDeli(indxCrop:indxLast:NBudgetCrops)   = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RDeli(indxLast+indxCrop)                = RDeli(indxLast+indxCrop) + SUM(RDeli(indxCrop:indxLast:NBudgetCrops))
        ElemValue                               = ElemSupply%UpstrmRunoff * DemandFrac(:,indxCrop)
        RRunoff(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
        RRunoff(indxLast+indxCrop)              = RRunoff(indxLast+indxCrop) + SUM(RRunoff(indxCrop:indxLast:NBudgetCrops))
        RLUArea(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(Area(:,indxCrop))
        RLUArea(indxLast+indxCrop)              = RLUArea(indxLast+indxCrop) + SUM(RLUArea(indxCrop:indxLast:NBudgetCrops))
      END DO
    END IF

    IF (PondedAg%lLWUseBudRawFile_Defined)    CALL WriteLWUseFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RRunoff,PondedAg)
    IF (PondedAg%lRootZoneBudRawFile_Defined) CALL WriteRootZoneFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RRunoff,ElemPrecip,ElemGenericMoist,Area,PondedAg)
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT LAND AND WATER USE BUDGET RAW DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteLWUseFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RUpstrmElemRunoff,PondedAg)
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    REAL(8),DIMENSION(:),INTENT(IN) :: RLUArea,RPump,RDeli,RUpstrmElemRunoff
    TYPE(PondedAgDatabaseType)      :: PondedAg
    
    !Local variables
    INTEGER                                                          :: indxCrop,iBudgetCrops(PondedAg%NBudgetCrops),indxLast,NBudgetCrops
    REAL(8),DIMENSION(AppGrid%NElements)                             :: ElemValue
    REAL(8)                                                          :: DummyArray(f_iNAgLWUseBudColumns,(AppGrid%NSubregions+1)*PondedAg%NBudgetCrops)
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*PondedAg%NBudgetCrops) :: RDemandRaw,RDemand,RDemandShort,RETAW,RETP,RETOth

    !Initialize
    NBudgetCrops = PondedAg%NBudgetCrops
    iBudgetCrops = PondedAg%iBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    DummyArray   = 0.0
    
    !Compute budget terms
    DO indxCrop=1,NBudgetCrops
      ElemValue                                  = PondedAg%Crops(iBudgetCrops(indxCrop),:)%DemandRaw
      RDemandRaw(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDemandRaw(indxLast+indxCrop)              = SUM(RDemandRaw(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops(iBudgetCrops(indxCrop),:)%Demand
      RDemand(indxCrop:indxLast:NBudgetCrops)    = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDemand(indxLast+indxCrop)                 = SUM(RDemand(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops(iBudgetCrops(indxCrop),:)%ETAW
      RETAW(indxCrop:indxLast:NBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETAW(indxLast+indxCrop)                   = SUM(RETAW(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops(iBudgetCrops(indxCrop),:)%ETP
      RETP(indxCrop:indxLast:NBudgetCrops)       = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETP(indxLast+indxCrop)                    = SUM(RETP(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops(iBudgetCrops(indxCrop),:)%ETOth
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
    CALL PondedAg%LWUseBudRawFile%WriteData(DummyArray)

  END SUBROUTINE WriteLWUseFlowsToBudRawFile
  

  ! -------------------------------------------------------------
  ! --- PRINT ROOT ZONE BUDGET RAW DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteRootZoneFlowsToBudRawFile(AppGrid,RLUArea,RPump,RDeli,RUpstrmElemRunoff,ElemPrecip,ElemGenericMoist,Area,PondedAg)
   TYPE(AppGridType),INTENT(IN)    :: AppGrid
   REAL(8),DIMENSION(:),INTENT(IN) :: RPump,RDeli,RUpstrmElemRunoff,RLUArea,ElemPrecip,ElemGenericMoist
   TYPE(PondedAgDatabaseType)      :: PondedAg
   REAL(8),INTENT(IN)              :: Area(AppGrid%NElements,PondedAg%NBudgetCrops)
    
    !Local variables
    INTEGER                                                          :: NRegions,NElements,NBudgetCrops,indxCrop,indxLast,  &
                                                                        iBudgetCrops(PondedAg%NBudgetCrops),iCrop
    REAL(8)                                                          :: DummyArray(f_iNAgRootZoneBudColumns,(AppGrid%NSubregions+1)*PondedAg%NBudgetCrops) 
    REAL(8),DIMENSION(AppGrid%NElements)                             :: ElemValue
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*PondedAg%NBudgetCrops) :: RRunoff,RPrecip,RReuse,RReturn,RSoilMCh,RInfilt,RETPot,           &
                                                                        RETa,RPerc,Error,RSoilM,RSoilM_P,RDrain,RGenMoistInflow

    !Initialize
    NRegions     = AppGrid%NSubregions
    NElements    = AppGrid%NElements
    NBudgetCrops = PondedAg%NBudgetCrops
    iBudgetCrops = PondedAg%iBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    DummyArray   = 0.0
    
    !Compute subregional values
    DO indxCrop=1,NBudgetCrops
      iCrop                                           = iBudgetCrops(indxCrop)
      !Moisture at the beginning of the time step must be corrected for chnages due to acreage changes
      ElemValue                                       = (PondedAg%Crops(iCrop,:)%SoilM_Precip_P + PondedAg%Crops(iCrop,:)%SoilM_AW_P + PondedAg%Crops(iCrop,:)%SoilM_Oth_P) * Area(:,indxCrop)    &
                                                       - PondedAg%Crops(iCrop,:)%SoilMCh                                                                                                                                            &
                                                       + PondedAg%Crops(iCrop,:)%PercCh
      RSoilM_P(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM_P(indxLast+indxCrop)                     = SUM(RSoilM_P(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = (PondedAg%Crops(iCrop,:)%SoilM_Precip + PondedAg%Crops(iCrop,:)%SoilM_AW + PondedAg%Crops(iCrop,:)%SoilM_Oth) * Area(:,indxCrop)
      RSoilM(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM(indxLast+indxCrop)                       = SUM(RSoilM(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = (ElemGenericMoist * PondedAg%RootDepth(iCrop) - PondedAg%Crops(iCrop,:)%GMExcess) * Area(:,indxCrop)
      RGenMoistInflow(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RGenMoistInflow(indxLast+indxCrop)              = SUM(RGenMoistInflow(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = ElemPrecip * Area(:,indxCrop)
      RPrecip(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RPrecip(indxLast+indxCrop)                      = SUM(RPrecip(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%Runoff
      RRunoff(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RRunoff(indxLast+indxCrop)                      = SUM(RRunoff(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%Reuse
      RReuse(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReuse(indxLast+indxCrop)                       = SUM(RReuse(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%ReturnFlow
      RReturn(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReturn(indxLast+indxCrop)                      = SUM(RReturn(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%SoilMCh
      RSoilMCh(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilMCh(indxLast+indxCrop)                     = SUM(RSoilMCh(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%PrecipInfilt + PondedAg%Crops(iCrop,:)%IrigInfilt
      RInfilt(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RInfilt(indxLast+indxCrop)                      = SUM(RInfilt(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%Drain
      RDrain(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDrain(indxLast+indxCrop)                       = SUM(RDrain(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%ETa
      RETa(indxCrop:indxLast:NBudgetCrops)            = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETa(indxLast+indxCrop)                         = SUM(RETa(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops(iCrop,:)%Perc + PondedAg%Crops(iCrop,:)%PercCh
      RPerc(indxCrop:indxLast:NBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RPerc(indxLast+indxCrop)                        = SUM(RPerc(indxCrop:indxLast:NBudgetCrops))
      RETPot(indxCrop:indxLast:NBudgetCrops)          = PondedAg%RegionETPot(iCrop,:)
      RETPot(indxLast+indxCrop)                       = SUM(RETPot(indxCrop:indxLast:NBudgetCrops))
    END DO
    Error                                             = RSoilM_P + RSoilMCh + RInfilt +RGenMoistInflow - RDrain - RETa - RPerc - RSoilM
    
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
    DummyArray(12,:) = RGenMoistInflow                                 !Generic moisture inflow to ponded ag lands
    DummyArray(13,:) = RDrain                                          !Rice/refuge pond drainage on ag lands
    DummyArray(14,:) = RETa                                            !ET on ag lands
    DummyArray(15,:) = RPerc                                           !Percolation on ag lands
    DummyArray(16,:) = RSoilM                                          !Storage at the end of the time interval
    DummyArray(17,:) = Error                                           !Mass balance error for ag lands

    !Print out values to binary file
    CALL PondedAg%RootZoneBudRawFile%WriteData(DummyArray)

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
  ! --- SIMULATE FLOW PROCESSES AT PONDED AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(PondedAg,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,HydCondPonded,ElemSupply,ElemsToGW,SolverData,lLakeElem,iStat)
    CLASS(PondedAgDatabaseType),TARGET :: PondedAg
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(ETType),INTENT(IN)            :: ETData
    TYPE(RootZoneSoilType),INTENT(IN)  :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                 :: HydCondPonded(:),DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:,:)
    INTEGER,INTENT(IN)                 :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)    :: SolverData
    LOGICAL,INTENT(IN)                 :: lLakeElem(:)
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    !$ INTEGER                  :: iChunk
    INTEGER                     :: indxElem,indxCrop,KunsatMethod,iElemID
    REAL(8)                     :: AchievedConv,Area,ETc(f_iNPondedCrops),HydCond,TotalPorosity,       &
                                   FieldCapacity,TotalPorosityCrop,FieldCapacityCrop,RootDepth,Lambda, &
                                   Supply,WiltingPoint,WiltingPointCrop,SoilM,SoilM_P,GM,PrecipD,      &
                                   rMultip,GMElem,Inflow,Excess,dRF,dRU,ratio(3),SoilM_P_Array(3),     &
                                   SoilM_Array(3),Infilt(3),ETPartition(3)
    TYPE(PondedAgType),POINTER  :: pCrop
    LOGICAL                     :: lNegativeMoist
    
    !Initialize
    iStat     = 0
    !$ iChunk = MAX(1 , AppGrid%NElements/(OMP_GET_MAX_THREADS()-1)/10)
                                    
    !Inform user
    CALL EchoProgress('Simulating flows at ponded agricultural crop lands')
    
    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,HydCondPonded,ETData,Precip,GenericMoisture,    &
    !$OMP                                  DeltaT,PondedAg,ElemSupply,SolverData,ElemsToGW,iStat)                      &
    !$OMP          NUM_THREADS(OMP_GET_MAX_THREADS()-1) 
    !$OMP DO SCHEDULE(DYNAMIC,iChunk)
    DO indxElem=1,AppGrid%NElements
        !Initialize flows
        PondedAg%Crops(:,indxElem)%Runoff       = 0.0   
        PondedAg%Crops(:,indxElem)%ReturnFlow   = 0.0   
        PondedAg%Crops(:,indxElem)%PrecipInfilt = 0.0                      
        PondedAg%Crops(:,indxElem)%IrigInfilt   = 0.0 
        PondedAg%Crops(:,indxElem)%Drain        = 0.0
        PondedAg%Crops(:,indxElem)%ETa          = 0.0                     
        PondedAg%Crops(:,indxElem)%ETAW         = 0.0   
        PondedAg%Crops(:,indxElem)%ETP          = 0.0 
        PondedAg%Crops(:,indxElem)%ETOth        = 0.0
        PondedAg%Crops(:,indxElem)%Perc         = 0.0 
        PondedAg%Crops(:,indxElem)%Reuse        = 0.0
        PondedAg%Crops(:,indxElem)%GMExcess     = 0.0
        
        !Cycle if necessary
        IF (iStat .EQ. -1) CYCLE
        IF (lLakeElem(indxElem)) CYCLE
        
        !Soil parameters
        WiltingPoint  = SoilsData(indxElem)%WiltingPoint
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        HydCond       = HydCondPonded(indxElem)
        Lambda        = SoilsData(indxElem)%Lambda
        KunsatMethod  = SoilsData(indxElem)%KunsatMethod
        ETc           = ETData%GetValues(PondedAg%Crops(:,indxElem)%iColETc) * DeltaT
        GMElem        = GenericMoisture(1,indxElem) * DeltaT
        PrecipD       = Precip(indxElem) * DeltaT
        
        !Loop over each land use
        DO indxCrop=1,f_iNPondedCrops
            pCrop => PondedAg%Crops(indxCrop,indxElem)
            
            !Cycle if area is zero
            Area = pCrop%Area
            IF (Area .EQ. 0.0) CYCLE
          
            !Initialize
            RootDepth         = PondedAg%RootDepth(indxCrop)
            WiltingPointCrop  = WiltingPoint  * RootDepth
            TotalPorosityCrop = TotalPorosity * RootDepth
            FieldCapacityCrop = FieldCapacity * RootDepth
            GM                = GMElem        * RootDepth
            Supply            = ElemSupply(indxCrop,indxElem)*DeltaT/Area
            SoilM_P           = pCrop%SoilM_Precip_P + pCrop%SoilM_AW_P + pCrop%SoilM_Oth_P
            Inflow            = GM + Supply
            dRF               = PondedAg%OperationFlowsFile%rValues(pCrop%iColReturn)
            dRU               = PondedAg%OperationFlowsFile%rValues(pCrop%iColReuse)
            
            !Simulate
            CALL PondedLUMoistureRouter(PrecipD                                             ,  &
                                        pCrop%SMax                                          ,  &
                                        PondedAg%PondDepthFile%rValues(pCrop%iColPondDepth) ,  &
                                        SoilM_P                                             ,  &
                                        ETc(indxCrop)                                       ,  & 
                                        HydCond                                             ,  & 
                                        TotalPorosityCrop                                   ,  & 
                                        FieldCapacityCrop                                   ,  & 
                                        WiltingPointCrop                                    ,  &
                                        Lambda                                              ,  & 
                                        Inflow                                              ,  &
                                        SolverData%Tolerance*TotalPorosityCrop              ,  &
                                        KunsatMethod                                        ,  &
                                        SolverData%IterMax                                  ,  &
                                        SoilM                                               ,  & 
                                        pCrop%Runoff                                        ,  & 
                                        pCrop%Drain                                         ,  &
                                        pCrop%PrecipInfilt                                  ,  & 
                                        pCrop%ETa                                           ,  & 
                                        pCrop%Perc                                          ,  & 
                                        Excess                                              ,  &
                                        AchievedConv                                        ) 

            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
                !$OMP CRITICAL
                iElemID         = AppGrid%AppElement(indxElem)%ID
                MessageArray(1) = 'Convergence error in soil moisture routing for ponded lands!'
                MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                MessageArray(3) = 'Crop type            = '//TRIM(f_cCropCodes(indxCrop))
                WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                iStat = -1
                !$OMP END CRITICAL
                EXIT
            END IF
            
            !Infiltration due to irrigation
            pCrop%IrigInfilt = Supply

            !Scale down the inflows into root zone if there is non-zero excess
            IF (Excess .NE. 0.0) THEN
                !Compute return flow first before modifying other flow terms
                pCrop%ReturnFlow = MIN(MIN(dRF-dRU , Supply)  ,  Excess)
                pCrop%IrigInfilt = Supply - pCrop%ReturnFlow
                Excess           = Excess - pCrop%ReturnFlow
                !Then compute excess inflows of other terms
                IF (Excess .NE. 0.0) THEN
                    !If we have to further decrease AW infiltration
                    IF (Excess .GT. pCrop%PrecipInfilt+GM) THEN
                        ratio = [pCrop%PrecipInfilt , pCrop%IrigInfilt , GM]
                        CALL NormalizeArray(ratio)
                        pCrop%Runoff         = pCrop%Runoff + Excess * ratio(1)
                        pCrop%ReturnFlow     = pCrop%ReturnFlow + Excess * ratio(2)
                        pCrop%GMExcess       = Excess * ratio(3)
                        pCrop%PrecipInfilt   = PrecipD - pCrop%Runoff
                        pCrop%IrigInfilt     = Supply - pCrop%ReturnFlow
                    ELSE
                        ratio(1:2) = [pCrop%PrecipInfilt , GM]
                        CALL NormalizeArray(ratio(1:2))
                        pCrop%Runoff         = pCrop%Runoff + Excess * ratio(1)
                        pCrop%GMExcess       = Excess * ratio(2)
                        pCrop%PrecipInfilt   = PrecipD - pCrop%Runoff
                    END IF
                END IF
            END IF
            
            !Compute re-use
            IF (dRF-dRU .GT. 0.0)   &
              pCrop%Reuse = MIN(dRU , pCrop%ReturnFlow * dRU/(dRF-dRU))

            !Compute moisture from precip and irrigation
            SoilM_P_Array = [pCrop%SoilM_Precip_P , pCrop%SoilM_AW_P , pCrop%SoilM_Oth_P  ]
            Infilt        = [pCrop%PrecipInfilt   , pCrop%IrigInfilt , GM - pCrop%GMExcess]
            CALL TrackMoistureDueToSource(SoilM_P_Array  ,  &
                                          Infilt         ,  &
                                          pCrop%Perc     ,  &
                                          pCrop%ETa      ,  &
                                          pCrop%Drain    ,  &
                                          SoilM_Array    ,  &
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
            pCrop%Runoff       = pCrop%Runoff       * rMultip
            pCrop%ReturnFlow   = pCrop%ReturnFlow   * rMultip
            pCrop%Drain        = pCrop%Drain        * rMultip
            pCrop%PrecipInfilt = pCrop%PrecipInfilt * rMultip
            pCrop%IrigInfilt   = pCrop%IrigInfilt   * rMultip
            pCrop%ETa          = pCrop%ETa          * rMultip
            pCrop%Perc         = pCrop%Perc         * rMultip  
            pCrop%Reuse        = pCrop%Reuse        * rMultip
            pCrop%ETAW         = pCrop%ETAW         * rMultip
            pCrop%ETP          = pCrop%ETP          * rMultip
            pCrop%ETOth        = pCrop%ETOth        * rMultip

            !If surface flow goes to groundwater, update the runoff processes
            IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
                pCrop%Perc         = pCrop%Perc + pCrop%Runoff + pCrop%ReturnFlow + pCrop%Drain
                pCrop%PrecipInfilt = pCrop%PrecipInfilt + pCrop%Runoff                     !Runoff and 
                pCrop%IrigInfilt   = pCrop%IrigInfilt + pCrop%ReturnFlow + pCrop%Drain     ! return flow are assumed to bypass root zone for proper mass balance       
                pCrop%Runoff       = 0.0
                pCrop%ReturnFlow   = 0.0
                pCrop%Drain        = 0.0
            END IF
        END DO
    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE PONDED AG DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(PondedAg,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,HydCondPonded,SpecifiedDemand,IrigPeriod,lLakeElem,lReadAgWaterDemand)
    CLASS(PondedAgDatabaseType),TARGET :: PondedAg
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(ETType)                       :: ETData
    TYPE(RootZoneSoilType),INTENT(IN)  :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                 :: HydCondPonded(:),DeltaT,Precip(:),GenericMoisture(:,:),SpecifiedDemand(:)
    INTEGER,INTENT(IN)                 :: IrigPeriod(:)
    LOGICAL,INTENT(IN)                 :: lLakeElem(:),lReadAgWaterDemand
    
    !Local variables
    !$ INTEGER                 :: iChunk
    INTEGER                    :: indxElem,indxCrop
    REAL(8)                    :: Area,ETc(f_iNPondedCrops),RootDepth,TotalPorosityCrop,TotalPorosity,GM,  &
                                  FieldCapacity,FieldCapacityCrop,PondDepth,SoilM,NonFloodRiceDecompAW,    &
                                  PrecipD,GMElem,Reuse,ReturnFlow
    TYPE(PondedAgType),POINTER :: pCrop

    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,ETData,Precip,GenericMoisture,PondedAg,DeltaT, &
    !$OMP                                  IrigPeriod,lReadAgWaterDemand,SpecifiedDemand,HydCondPonded) &
    !$OMP          NUM_THREADS(OMP_GET_MAX_THREADS()-1)
    !$OMP DO SCHEDULE(DYNAMIC,iChunk)
    DO indxElem=1,AppGrid%NElements
        PondedAg%Crops(:,indxElem)%DemandRaw = 0.0  
        PondedAg%Crops(:,indxElem)%Demand    = 0.0
        IF (lLakeElem(indxElem)) CYCLE
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        ETc           = ETData%GetValues(PondedAg%Crops(:,indxElem)%iColETc) * DeltaT
        GMElem        = GenericMoisture(1,indxElem) * DeltaT
        PrecipD       = Precip(indxElem) * DeltaT
        DO indxCrop=1,f_iNPondedCrops
            pCrop => PondedAg%Crops(indxCrop,indxElem)
            
            !Cycle if Area is zero
            Area  = pCrop%Area
            IF (Area .EQ. 0.0) CYCLE
          
            !Cycle if demand is specified 
            IF (lReadAgWaterDemand) THEN
                IF (PondedAg%iColAgDemand(indxCrop,indxElem) .GT. 0) THEN
                    pCrop%DemandRaw = SpecifiedDemand(PondedAg%iColAgDemand(indxCrop,indxElem))
                    pCrop%Demand    = pCrop%DemandRaw
                    CYCLE
                END IF
            END IF
            
            !If it is non-flooded rice decomp and it is decomp time, specify demand and cycle
            IF (indxCrop .EQ. f_iindxRice_NonFloodDecomp) THEN
                NonFloodRiceDecompAW = PondedAg%OperationFlowsFile%rValues(PondedAg%iColNonFloodRiceDecompAW(indxElem))
                IF (NonFloodRiceDecompAW .GT. 0.0) THEN
                    pCrop%Demand = NonFloodRiceDecompAW
                    CYCLE
                END IF
            END IF
            
            !Cycle if it is not an irrigation period
            IF (IrigPeriod(pCrop%iColIrigPeriod) .EQ. f_iNoIrigPeriod) CYCLE

            !Initialize
            RootDepth         = PondedAg%RootDepth(indxCrop)
            PondDepth         = PondedAg%PondDepthFile%rValues(pCrop%iColPondDepth)
            ReturnFlow        = PondedAg%OperationFlowsFile%rValues(pCrop%iColReturn) * DeltaT
            Reuse             = PondedAg%OperationFlowsFile%rValues(pCrop%iColReuse) * DeltaT
            TotalPorosityCrop = TotalPorosity * RootDepth
            FieldCapacityCrop = FieldCapacity * RootDepth
            GM                = GMElem * RootDepth
            SoilM             = pCrop%SoilM_Precip_P + pCrop%SoilM_AW_P + pCrop%SoilM_Oth_P

            !Compute demand
            CALL PondedCropDemand(PrecipD                                    ,  &
                                  pCrop%SMax                                 ,  &
                                  GM                                         ,  &
                                  ReturnFlow                                 ,  &
                                  Reuse                                      ,  &
                                  PondDepth                                  ,  &
                                  ETc(indxCrop)                              ,  &
                                  HydCondPonded(indxElem)                    ,  &
                                  TotalPorosityCrop                          ,  & 
                                  FieldCapacityCrop                          ,  & 
                                  SoilM                                      ,  & 
                                  pCrop%DemandRaw                            ,  &
                                  pCrop%Demand                               ) 
 
            !Convert demand related data to volumetric rate and store in persistent arrays
            pCrop%DemandRaw = pCrop%DemandRaw / DeltaT * Area
            pCrop%Demand    = pCrop%Demand    / DeltaT * Area
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
  SUBROUTINE SoilMContent_To_Depth(PondedAgLand,NElements,TotalPorosity)
    CLASS(PondedAgDatabaseType) :: PondedAgLand
    INTEGER,INTENT(IN)          :: NElements
    REAL(8),INTENT(IN)          :: TotalPorosity(:)
    
    !Local variables
    INTEGER :: indxElem,indxCrop
    REAL(8) :: RootDepth(f_iNPondedCrops),TP,SoilM,rValue(3)
    
    !Return if rice/refuge is not modeled
    IF (.NOT. ALLOCATED(PondedAgLand%Crops)) RETURN
    
    !Initialize
    RootDepth = PondedAgLand%RootDepth
    
    !Convert contents to depths, find initial ponding depth and equate SoilM_P to SoilM
    DO indxElem=1,NElements
      TP = TotalPorosity(indxElem) 
      DO indxCrop=1,f_iNPondedCrops
        ASSOCIATE (pCrop => PondedAgLand%Crops(indxCrop,indxElem))
          SoilM              = pCrop%SoilM_Precip + pCrop%SoilM_AW + pCrop%SoilM_Oth
          pCrop%PondDepth    = MAX((SoilM-TP) * RootDepth(indxCrop)  ,   0.0                      )
          SoilM              = MIN(SoilM * RootDepth(indxCrop)       ,   TP * RootDepth(indxCrop) )
          rValue(1)          = pCrop%SoilM_Precip + pCrop%SoilM_AW + pCrop%SoilM_Oth
          IF (rValue(1) .GT. 0.0) THEN
            rValue             = [pCrop%SoilM_Precip , pCrop%SoilM_AW , pCrop%SoilM_Oth]
            CALL NormalizeArray(rValue)
            pCrop%SoilM_Precip = SoilM * rValue(1)
            pCrop%SoilM_AW     = SoilM * rValue(2)
            pCrop%SoilM_Oth    = SoilM * rValue(3)
          END IF
        END ASSOCIATE
      END DO
    END DO 
    PondedAgLand%Crops%SoilM_Precip_P = PondedAgLand%Crops%SoilM_Precip
    PondedAgLand%Crops%SoilM_AW_P     = PondedAgLand%Crops%SoilM_AW
    PondedAgLand%Crops%SoilM_Oth_P    = PondedAgLand%Crops%SoilM_Oth
    
  END SUBROUTINE SoilMContent_To_Depth
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(PondedAgLand) 
    CLASS(PondedAgDatabaseType) :: PondedAgLand
    
    PondedAgLand%Crops%Area_P = PondedAgLand%Crops%Area
    
  END SUBROUTINE AdvanceAreas
  
  
  ! -------------------------------------------------------------
  ! --- COMPARE SPECIFIED AG DEMAND TO AREAS
  ! -------------------------------------------------------------
  SUBROUTINE CheckSpecifiedDemandAndArea(PondedAgLand,iElemIDs,rSpecifiedDemand,iStat)
    CLASS(PondedAgDatabaseType),INTENT(IN) :: PondedAgLand
    INTEGER,INTENT(IN)                     :: iElemIDs(:)
    REAL(8),INTENT(IN)                     :: rSpecifiedDemand(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+27) :: ThisProcedure = ModName // 'CheckSpecifiedDemandAndArea'
    INTEGER                      :: indxElem,indxCrop,iCol
    
    !Initialize
    iStat = 0
    
    !Compare
    DO indxElem=1,SIZE(PondedAgLand%Crops,DIM=2)
        DO indxCrop=1,PondedAgLand%NCrops
            iCol = PondedAgLand%iColAgDemand(indxCrop,indxElem)
            IF (iCol .GT. 0) THEN
                IF (rSpecifiedDemand(iCol) .GT. 0.0) THEN
                    IF (PondedAgLand%Crops(indxCrop,indxElem)%Area .EQ. 0.0) THEN
                        MessageArray(1) = 'Specified water demand for crop ' // TRIM(PondedAgLand%CropCodes(indxCrop)) // ' at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater '
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
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIDED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(PondedAgLand,iElemIDs,rElemAreas,TimeStep,iStat)
    CLASS(PondedAgDatabaseType)   :: PondedAgLand
    INTEGER,INTENT(IN)            :: iElemIDs(:)
    REAL(8),INTENT(IN)            :: rElemAreas(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep 
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    CALL PondedAgLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                         ;  IF (iStat .NE. 0) RETURN
    CALL PondedAgLand%LandUseDataFile%ReadTSData('Ponded crop areas',TimeStep,rElemAreas,iElemIDs,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL PondedAgLand%PondDepthFile%File%RewindFile_To_BeginningOfTSData(iStat)                                     ;  IF (iStat .NE. 0) RETURN
    CALL ReadTimeSeriesData(TimeStep,'Rice/refuge pond depths data',PondedAgLand%PondDepthFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iFileReadCode .EQ. 0) PondedAgLand%PondDepthFile%rValues = PondedAgLand%PondDepthFile%rValues * PondedAgLand%PondDepthFactor

    CALL PondedAgLand%OperationFlowsFile%File%RewindFile_To_BeginningOfTSData(iStat)                                               ;  IF (iStat .NE. 0) RETURN
    CALL ReadTimeSeriesData(TimeStep,'Rice/refuge pond operation flows data',PondedAgLand%OperationFlowsFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN 
    IF (iFileReadCode .EQ. 0) PondedAgLand%OperationFlowsFile%rValues = PondedAgLand%OperationFlowsFile%rValues * PondedAgLand%OperationFlowsFactor
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp
  
END MODULE