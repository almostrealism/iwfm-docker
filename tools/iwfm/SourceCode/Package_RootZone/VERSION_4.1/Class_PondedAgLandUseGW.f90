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
MODULE Class_PondedAgLandUseGW
  !$ USE OMP_LIB
  USE MessageLogger           , ONLY: SetLastMessage                   , &
                                      EchoProgress                     , &
                                      MessageArray                     , &
                                      f_iFatal                           
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter          , &
                                      CleanSpecialCharacters           , &
                                      EstablishAbsolutePathFileName    , & 
                                      UpperCase                        , &
                                      IntToText                        , &
                                      NormalizeArray                   , &
                                      ShellSort                        , &
                                      LocateInList                     
  USE TimeSeriesUtilities     , ONLY: TimeStepType                     
  USE IOInterface             , ONLY: GenericFileType                  , &
                                      RealTSDataInFileType             
  USE Package_Budget          , ONLY: BudgetType                       , &
                                      f_iMaxLocationNameLen               
  USE Package_Misc            , ONLY: SolverDataType                   , & 
                                      f_iFlowDest_GWElement         
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource      
  USE Class_GenericLandUseGW  , ONLY: GenericLandUseGWType             , &
                                      ComputeETFromGW_Max              
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType              
  USE Package_Discretization  , ONLY: AppGridType                      
  USE Package_PrecipitationET , ONLY: ETType 
  USE Util_Package_RootZone   , ONLY: WaterSupplyType                  , &
                                      ReadRealData                     , &
                                      ReadPointerData                  , &
                                      f_iNoIrigPeriod                  , &   
                                      f_iBudgetType_PondedCrop_LWU     , & 
                                      f_iBudgetType_PondedCrop_RZ 
  USE Util_RootZone_v41       , ONLY: RootZoneSoil_v41_Type            , &
                                      AgRootZoneBudRawFile_New         , &
                                      AgLWUseBudRawFile_New            , &
                                      f_iNAgRootZoneBudColumns         , & 
                                      f_iNAgLWUseBudColumns               
  USE Package_UnsatZone       , ONLY: PondedCropDemand                 , & 
                                      PondedLUMoistureRouter           
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
  PUBLIC :: PondedAgDatabaseType ,  &
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
  TYPE,EXTENDS(GenericLandUseGWType) :: PondedAgType
    INTEGER,ALLOCATABLE :: iColReturn(:,:)        !Column number in the rice/refuge operations data file for return flow depth
    INTEGER,ALLOCATABLE :: iColReuse(:,:)         !Column number in the rice/refuge operations data file for reuse depth
    INTEGER,ALLOCATABLE :: iColPondDepth(:,:)     !Column number in the rice/refuge operations data file for ponding depth
    INTEGER,ALLOCATABLE :: iColIrigPeriod(:,:)    !Pointer to irrigation period data file for (crop,element) combination
    REAL(8),ALLOCATABLE :: PondDepth(:,:)         !Actual ponding depth
    REAL(8),ALLOCATABLE :: Drain(:,:)             !Drainage flow from ponds
    REAL(8),ALLOCATABLE :: ReturnFlow(:,:)        !Return flow
    REAL(8),ALLOCATABLE :: IrigInfilt(:,:)        !Infiltration due to irrigation
    REAL(8),ALLOCATABLE :: Reuse(:,:)             !Reused return flow 
    REAL(8),ALLOCATABLE :: ETAW(:,:)              !ET of applied water
    REAL(8),ALLOCATABLE :: ETP(:,:)               !ET of precipitation
    REAL(8),ALLOCATABLE :: ETOth(:,:)             !ET of other sources of moisture
    REAL(8),ALLOCATABLE :: DemandRaw(:,:)         !Water demand before the return flow is included
    REAL(8),ALLOCATABLE :: Demand(:,:)            !Water demand after the return flow is included 
    REAL(8),ALLOCATABLE :: ElemDemandFrac(:,:)    !Ratio of crop demand to the total demand at the element it is located at
    REAL(8),ALLOCATABLE :: ElemDemandFrac_Ag(:,:) !Ratio of crop demand to the total "ag" demand at the element it is located at
  END TYPE PondedAgType
  


  ! -------------------------------------------------------------
  ! --- PONDED LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE PondedAgDatabaseType
    INTEGER                        :: NCrops                      = f_iNPondedCrops  !Number of ponded crops
    TYPE(PondedAgType)             :: Crops                                          !Ponded crops for each (crop,element) combination
    CHARACTER(LEN=f_iLenCropCode)  :: CropCodes(f_iNPondedCrops)  = f_cCropCodes     !Non-ponded crop codes
    INTEGER                        :: NBudgetCrops                = 0                !Number of ponded crops for budget output
    INTEGER,ALLOCATABLE            :: iBudgetCrops(:)                                !Indices of ponded crops for budget output
    INTEGER,ALLOCATABLE            :: iColAgDemand(:,:)                              !Pointer to ag water demand file for each (crop,element) combination
    INTEGER,ALLOCATABLE            :: iColNonFloodRiceDecompAW(:)                    !Column number in the rice/refuge operations data file for non-floooded rice decomp water application depth for each (element)
    REAL(8),ALLOCATABLE            :: RegionETPot(:,:)                               !Regional potential ET for each (crop,subregion) combination
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
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadTSData                   
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: PrintResults                 
    PROCEDURE,PASS :: AdvanceAreas                 
    PROCEDURE,PASS :: SoilMContent_To_Depth        
    PROCEDURE,PASS :: ComputeWaterDemand 
    PROCEDURE,PASS :: CheckSpecifiedDemandAndArea
    PROCEDURE,PASS :: Simulate                     
    PROCEDURE,PASS :: ComputeETFromGW_Max  => PondedAgLandUse_ComputeETFromGW_Max
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp                     
  END TYPE PondedAgDatabaseType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 25
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_PondedAgLandUseGW::'




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
  SUBROUTINE New(PondLand,IsForInquiry,cFileName,cWorkingDirectory,FactCN,AppGrid,iElemIDs,TimeStep,NTimeSteps,cIWFMVersion,iStat,pAgLWUseBudRawFile_New,pAgRootZoneBudRawFile_New)
    CLASS(PondedAgDatabaseType)                                     :: PondLand
    LOGICAL,INTENT(IN)                                              :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                                     :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)                                              :: FACTCN
    TYPE(AppGridType),INTENT(IN)                                    :: AppGrid
    TYPE(TimeStepType),INTENT(IN)                                   :: TimeStep
    INTEGER,INTENT(IN)                                              :: NTimeSteps,iElemIDs(AppGrid%NElements)
    CHARACTER(LEN=*),INTENT(IN)                                     :: cIWFMVersion
    INTEGER,INTENT(OUT)                                             :: iStat
    PROCEDURE(AgLWUseBudRawFile_New),OPTIONAL,POINTER,INTENT(IN)    :: pAgLWUseBudRawFile_New
    PROCEDURE(AgRootZoneBudRawFile_New),OPTIONAL,POINTER,INTENT(IN) :: pAgRootZoneBudRawFile_New
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)                      :: ThisProcedure = ModName // 'New'
    CHARACTER                                        :: ALine*1000,cBudgetCropCode*f_iLenCropCode
    CHARACTER(LEN=f_iMaxLocationNameLen),ALLOCATABLE :: cRegionNames(:)
    CHARACTER(LEN=f_iMaxLocationNameLen)             :: SubRegionNames(AppGrid%NSubregions+1)
    REAL(8)                                          :: FACT,Factor(1),SubRegionArea(AppGrid%NSubregions+1)
    INTEGER                                          :: ErrorCode,indxCrop,indxElem,NBudgetCrops,NBudgetRegions,NElements, &
                                                        NRegions,indxCropP,indxRegion,ID,iElem
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
    CALL PondLand%Crops%New(f_iNPondedCrops,NElements,iStat)
    ALLOCATE (PondLand%Crops%iColReturn(f_iNPondedCrops,NElements)         , &      
              PondLand%Crops%iColReuse(f_iNPondedCrops,NElements)          , &          
              PondLand%Crops%iColPondDepth(f_iNPondedCrops,NElements)      , &      
              PondLand%Crops%iColIrigPeriod(f_iNPondedCrops,NElements)     , &     
              PondLand%Crops%PondDepth(f_iNPondedCrops,NElements)          , &          
              PondLand%Crops%Drain(f_iNPondedCrops,NElements)              , &              
              PondLand%Crops%ReturnFlow(f_iNPondedCrops,NElements)         , &         
              PondLand%Crops%IrigInfilt(f_iNPondedCrops,NElements)         , &         
              PondLand%Crops%Reuse(f_iNPondedCrops,NElements)              , &              
              PondLand%Crops%ETAW(f_iNPondedCrops,NElements)               , &               
              PondLand%Crops%ETP(f_iNPondedCrops,NElements)                , &                
              PondLand%Crops%ETOth(f_iNPondedCrops,NElements)              , &              
              PondLand%Crops%DemandRaw(f_iNPondedCrops,NElements)          , &          
              PondLand%Crops%Demand(f_iNPondedCrops,NElements)             , &             
              PondLand%Crops%ElemDemandFrac(f_iNPondedCrops,NElements)     , &     
              PondLand%Crops%ElemDemandFrac_Ag(f_iNPondedCrops,NElements)  , & 
              PondLand%RegionETPot(f_iNPondedCrops,NRegions)               , &
              PondLand%RootDepth(f_iNPondedCrops)                          , &
              PondLand%iColNonFloodRiceDecompAW(NElements)                 , &
              STAT=ErrorCode                                               )
    IF (ErrorCode+iStat .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for ponded agricultural data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize arrays
    PondLand%Crops%iColReturn         = 0
    PondLand%Crops%iColReuse          = 0
    PondLand%Crops%iColPondDepth      = 0 
    PondLand%Crops%iColIrigPeriod     = 0 
    PondLand%Crops%PondDepth          = 0.0 
    PondLand%Crops%Drain              = 0.0
    PondLand%Crops%ReturnFlow         = 0.0 
    PondLand%Crops%IrigInfilt         = 0.0 
    PondLand%Crops%Reuse              = 0.0 
    PondLand%Crops%ETAW               = 0.0 
    PondLand%Crops%ETP                = 0.0 
    PondLand%Crops%ETOth              = 0.0 
    PondLand%Crops%DemandRaw          = 0.0 
    PondLand%Crops%Demand             = 0.0 
    PondLand%Crops%ElemDemandFrac     = 0.0 
    PondLand%Crops%ElemDemandFrac_Ag  = 0.0
    PondLand%RegionETPot              = 0.0
    PondLand%RootDepth                = 0.0
    PondLand%iColNonFloodRiceDecompAW = 0
    
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
      ALLOCATE (PondLand%iBudgetCrops(NBudgetCrops)         ,  &
                cRegionNames(NBudgetRegions)                ,  &
                RegionAreas(NBudgetRegions)                 ,  &
                STAT=ErrorCode                              )
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
            IF (PRESENT(pAgLWUseBudRawFile_New)) THEN
                CALL pAgLWUseBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'land and water use budget for specific ponded crops',cIWFMVersion,PondLand%LWUseBudRawFile,iStat)
            ELSE
                CALL AgLWUseBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'land and water use budget for specific ponded crops',cIWFMVersion,PondLand%LWUseBudRawFile,iStat)
            END IF
            IF (iStat .EQ. -1) RETURN
            PondLand%lLWUseBudRawFile_Defined = .TRUE.
        END IF
    END IF
    
    !Root zone budget
    CALL RiceRefugeDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)
    IF (NBudgetCrops .GT. 0) THEN
        IF (ALine .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
            IF (PRESENT(pAgRootZoneBudRawFile_New)) THEN
                CALL pAgRootZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'root zone budget for specific ponded crops',cIWFMVersion,PondLand%RootZoneBudRawFile,iStat)
            ELSE
                CALL AgRootZoneBudRawFile_New(IsForInquiry,cAbsPathFileName,TimeStep,NTimeSteps,NBudgetRegions,RegionAreas,cRegionNames,'root zone budget for specific ponded crops',cIWFMVersion,PondLand%RootZoneBudRawFile,iStat)
            END IF
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
        PondLand%Crops%SMax(:,iElem) = (1000.0/DummyRealArray(indxElem,2:)-10.0) * FACTCN
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
        PondLand%Crops%iColETc(:,iElem) = DummyIntArray(indxElem,2:)
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
        PondLand%Crops%iColIrigPeriod(:,iElem) = DummyIntArray(indxElem,2:)
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
    CALL PondLand%OperationFlowsFile%Init(cAbsPathFileName,cWorkingDirectory,'rice/refuge operations flow data file',TrackTime,1,.TRUE.,Factor,[.TRUE.],iStat=iStat)   ;  IF (iStat .EQ. -1) RETURN
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
        lProcessed(iElem)                                              = .TRUE.
        PondLand%Crops%iColPondDepth(f_iindxRice_FloodDecomp,iElem)    = DummyIntArray(indxElem,2) 
        PondLand%Crops%iColPondDepth(f_iindxRice_NonFloodDecomp,iElem) = DummyIntArray(indxElem,3)
        PondLand%Crops%iColPondDepth(f_iindxRice_NoDecomp,iElem)       = DummyIntArray(indxElem,4)
        PondLand%Crops%iColPondDepth(f_iindxRefuge_Seasonal,iElem)     = DummyIntArray(indxElem,5)
        PondLand%Crops%iColPondDepth(f_iindxRefuge_Permanent,iElem)    = DummyIntArray(indxElem,6)
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
        PondLand%Crops%iColReturn(:,iElem) = DummyIntArray(indxElem,2:) 
    END DO
      
    !Re-use water depths
    CALL ReadPointerData(RiceRefugeDataFile,'re-use flow depth column pointers for ponded crops','elements',NElements,f_iNPondedCrops+1,iElemIDs,DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
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
        PondLand%Crops%iColReuse(:,iElem) = DummyIntArray(indxElem,2:)
    END DO
    
    !Check for time-series column pointer errors
    DO indxElem=1,NElements
        ID = iElemIDs(indxElem)
        CALL PondLand%PondDepthFile%CheckColNum('Pond depth file as referenced by element '//TRIM(IntToText(ID)),PondLand%Crops%iColPondDepth(:,indxElem),.TRUE.,iStat)                                                                                                ;  IF (iStat .EQ. -1) RETURN
        CALL PondLand%OperationFlowsFile%CheckColNum('Rice/refuge pond operations flow file as referenced by element '//TRIM(IntToText(ID))//' for application depths for non-flooded rice decomposition',[PondLand%iColNonFloodRiceDecompAW(indxElem)],.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL PondLand%OperationFlowsFile%CheckColNum('Rice/refuge pond operations flow file as referenced by element '//TRIM(IntToText(ID))//' for return flow depths',PondLand%Crops%iColReturn(:,indxElem),.TRUE.,iStat)                                             ;  IF (iStat .EQ. -1) RETURN
        CALL PondLand%OperationFlowsFile%CheckColNum('Rice/refuge pond operations flow file as referenced by element '//TRIM(IntToText(ID))//' for re-use flow depths',PondLand%Crops%iColReuse(:,indxElem),.TRUE.,iStat)                                              ;  IF (iStat .EQ. -1) RETURN
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
        PondLand%Crops%SoilM_Precip(:,iElem) = DummyRealArray(indxElem,2) * DummyRealArray(indxElem,3:)
        PondLand%Crops%SoilM_AW(:,iElem)     = DummyRealArray(indxElem,3:) - PondLand%Crops%SoilM_Precip(:,iElem)
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
    CALL PondLand%Crops%Kill()
    DEALLOCATE (PondLand%Crops%iColReturn          , &      
                PondLand%Crops%iColReuse           , &          
                PondLand%Crops%iColPondDepth       , &      
                PondLand%Crops%iColIrigPeriod      , &     
                PondLand%Crops%PondDepth           , &          
                PondLand%Crops%Drain               , &              
                PondLand%Crops%ReturnFlow          , &         
                PondLand%Crops%IrigInfilt          , &         
                PondLand%Crops%Reuse               , &              
                PondLand%Crops%ETAW                , &               
                PondLand%Crops%ETP                 , &                
                PondLand%Crops%ETOth               , &              
                PondLand%Crops%DemandRaw           , &          
                PondLand%Crops%Demand              , &             
                PondLand%Crops%ElemDemandFrac      , &     
                PondLand%Crops%ElemDemandFrac_Ag   , & 
                PondLand%RegionETPot               , &
                PondLand%RootDepth                 , &
                PondLand%iColNonFloodRiceDecompAW , &
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
  SUBROUTINE ReadTSData(PondLand,TimeStep,AppGrid,ETData,iElemIDs,rElemAreas,iStat)
    CLASS(PondedAgDataBaseType)   :: PondLand
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(IN)            :: iElemIDs(AppGrid%NElements)
    REAL(8),INTENT(IN)            :: rElemAreas(AppGrid%NElements)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReadTSData'
    INTEGER                      :: indxCrop,indxElem,FileReadCode,iRegion
    REAL(8)                      :: ETc(f_iNPondedCrops)
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Reading time series data for ponded agricultural crops')
    
    !Land use areas
    CALL PondLand%LandUseDataFile%ReadTSData('Ponded crop areas',TimeStep,rElemAreas,iElemIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (PondLand%LandUseDataFile%lUpdated) THEN
        DO indxElem=1,AppGrid%NElements
            PondLand%Crops%Area(:,indxElem) = PondLand%LandUseDataFile%rValues(indxElem,2:)
        END DO
    END IF
    
    !Ponding depths
    CALL PondLand%PondDepthFile%ReadTSData(TimeStep,'Rice/refuge pond depths data',FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode .EQ. 0) PondLand%PondDepthFile%rValues = PondLand%PondDepthFile%rValues * PondLand%PondDepthFactor
    
    !Operation flow depths
    CALL PondLand%OperationFlowsFile%ReadTSData(TimeStep,'Rice/refuge pond operation flows data',FileReadCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (FileReadCode .EQ. 0) PondLand%OperationFlowsFile%rValues = PondLand%OperationFlowsFile%rValues * PondLand%OperationFlowsFactor
    
    !Regional potential ET
    IF (ETData%IsUpdated()) THEN
        PondLand%RegionETPot = 0.0
        DO indxElem=1,AppGrid%NElements
            ETc     = ETData%GetValues(PondLand%Crops%iColETc(:,indxElem))
            iRegion = AppGrid%AppElement(indxElem)%Subregion
            DO indxCrop=1,f_iNPondedCrops
                PondLand%RegionETPot(indxCrop,iRegion) = PondLand%RegionETPot(indxCrop,iRegion) + ETc(indxCrop) * PondLand%Crops%Area(indxCrop,indxElem)
            END DO
        END DO
    END IF
    
    !Make sure that return flow depth is larger than re-use depth
    ASSOCIATE (pPondOps => PondLand%OperationFlowsFile%rValues  , &
               pCrops   => PondLand%Crops                   )
    
        DO indxElem=1,AppGrid%NElements
            DO indxCrop=1,f_iNPondedCrops
                IF (pPondOps(pCrops%iColReturn(indxCrop,indxElem)) .LT. pPondOps(pCrops%iColReuse(indxCrop,indxElem))) THEN
                    MessageArray(1) = 'Re-use depth for ' // TRIM(f_cCropCodes(indxCrop)) // ' at element ' // TRIM(IntToText(iElemIDs(indxElem)))//' is greater than return flow depth!'
                    WRITE (MessageArray(2),'(A,F5.3)') 'Re-use depth      = ',pPondOps(pCrops%iColReuse(indxCrop,indxElem))
                    WRITE (MessageArray(3),'(A,F5.3)') 'Return flow depth = ',pPondOps(pCrops%iColReturn(indxCrop,indxElem))
                    CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
        END DO
      
    END ASSOCIATE

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
          DemandFrac(indxElem,indxCrop) = PondedAg%Crops%ElemDemandFrac_Ag(PondedAg%iBudgetCrops(indxCrop),indxElem) 
          Area(indxElem,indxCrop)       = PondedAg%Crops%Area(PondedAg%iBudgetCrops(indxCrop),indxElem)
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
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*PondedAg%NBudgetCrops) :: RDemandRaw,RDemand,RDemandShort,RETAW,RETP,RETOth,RETGW

    !Initialize
    NBudgetCrops = PondedAg%NBudgetCrops
    iBudgetCrops = PondedAg%iBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    DummyArray   = 0.0
    
    !Compute budget terms
    DO indxCrop=1,NBudgetCrops
      ElemValue                                  = PondedAg%Crops%DemandRaw(iBudgetCrops(indxCrop),:)
      RDemandRaw(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDemandRaw(indxLast+indxCrop)              = SUM(RDemandRaw(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops%Demand(iBudgetCrops(indxCrop),:)
      RDemand(indxCrop:indxLast:NBudgetCrops)    = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDemand(indxLast+indxCrop)                 = SUM(RDemand(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops%ETAW(iBudgetCrops(indxCrop),:)
      RETAW(indxCrop:indxLast:NBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETAW(indxLast+indxCrop)                   = SUM(RETAW(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops%ETP(iBudgetCrops(indxCrop),:)
      RETP(indxCrop:indxLast:NBudgetCrops)       = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETP(indxLast+indxCrop)                    = SUM(RETP(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops%ETFromGW_Actual(iBudgetCrops(indxCrop),:)
      RETGW(indxCrop:indxLast:NBudgetCrops)      = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETGW(indxLast+indxCrop)                   = SUM(RETGW(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                  = PondedAg%Crops%ETOth(iBudgetCrops(indxCrop),:)
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
    DummyArray(10,:) = RETGW
    DummyArray(11,:) = RETOth

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
    INTEGER                                                          :: NBudgetCrops,indxCrop,indxLast,iBudgetCrops(PondedAg%NBudgetCrops),iCrop
    REAL(8)                                                          :: DummyArray(f_iNAgRootZoneBudColumns,(AppGrid%NSubregions+1)*PondedAg%NBudgetCrops) 
    REAL(8),DIMENSION(AppGrid%NElements)                             :: ElemValue
    REAL(8),DIMENSION((AppGrid%NSubregions+1)*PondedAg%NBudgetCrops) :: RRunoff,RPrecip,RReuse,RReturn,RSoilMCh,RInfilt,RGWInflow,    &
                                                                        RETa,RPerc,Error,RSoilM,RSoilM_P,RDrain,RGenMoistInflow,RETPot

    !Initialize
    NBudgetCrops = PondedAg%NBudgetCrops
    iBudgetCrops = PondedAg%iBudgetCrops
    indxLast     = NBudgetCrops * AppGrid%NSubregions
    DummyArray   = 0.0
    
    !Compute subregional values
    DO indxCrop=1,NBudgetCrops
      iCrop                                           = iBudgetCrops(indxCrop)
      !Moisture at the beginning of the time step must be corrected for chnages due to acreage changes
      ElemValue                                       = (PondedAg%Crops%SoilM_Precip_P(iCrop,:) + PondedAg%Crops%SoilM_AW_P(iCrop,:) + PondedAg%Crops%SoilM_Oth_P(iCrop,:)) * Area(:,indxCrop) &
                                                       - PondedAg%Crops%SoilMCh(iCrop,:)                                                                                                                                            &
                                                       + PondedAg%Crops%PercCh(iCrop,:)
      RSoilM_P(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM_P(indxLast+indxCrop)                     = SUM(RSoilM_P(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = (PondedAg%Crops%SoilM_Precip(iCrop,:) + PondedAg%Crops%SoilM_AW(iCrop,:) + PondedAg%Crops%SoilM_Oth(iCrop,:)) * Area(:,indxCrop)
      RSoilM(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilM(indxLast+indxCrop)                       = SUM(RSoilM(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = (ElemGenericMoist * PondedAg%RootDepth(iCrop) - PondedAg%Crops%GMExcess(iCrop,:)) * Area(:,indxCrop)
      RGenMoistInflow(indxCrop:indxLast:NBudgetCrops) = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RGenMoistInflow(indxLast+indxCrop)              = SUM(RGenMoistInflow(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = ElemPrecip * Area(:,indxCrop)
      RPrecip(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RPrecip(indxLast+indxCrop)                      = SUM(RPrecip(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%Runoff(iCrop,:)
      RRunoff(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RRunoff(indxLast+indxCrop)                      = SUM(RRunoff(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%Reuse(iCrop,:)
      RReuse(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReuse(indxLast+indxCrop)                       = SUM(RReuse(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%ReturnFlow(iCrop,:)
      RReturn(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RReturn(indxLast+indxCrop)                      = SUM(RReturn(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%SoilMCh(iCrop,:)
      RSoilMCh(indxCrop:indxLast:NBudgetCrops)        = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RSoilMCh(indxLast+indxCrop)                     = SUM(RSoilMCh(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%PrecipInfilt(iCrop,:) + PondedAg%Crops%IrigInfilt(iCrop,:)
      RInfilt(indxCrop:indxLast:NBudgetCrops)         = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RInfilt(indxLast+indxCrop)                      = SUM(RInfilt(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%ETFromGW_Actual(iCrop,:) 
      RGWInflow(indxCrop:indxLast:NBudgetCrops)       = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RGWInflow(indxLast+indxCrop)                    = SUM(RGWInflow(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%Drain(iCrop,:)
      RDrain(indxCrop:indxLast:NBudgetCrops)          = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RDrain(indxLast+indxCrop)                       = SUM(RDrain(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%ETa(iCrop,:)
      RETa(indxCrop:indxLast:NBudgetCrops)            = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RETa(indxLast+indxCrop)                         = SUM(RETa(indxCrop:indxLast:NBudgetCrops))
      ElemValue                                       = PondedAg%Crops%Perc(iCrop,:) + PondedAg%Crops%PercCh(iCrop,:)
      RPerc(indxCrop:indxLast:NBudgetCrops)           = AppGrid%AccumElemValuesToSubregions(ElemValue)
      RPerc(indxLast+indxCrop)                        = SUM(RPerc(indxCrop:indxLast:NBudgetCrops))
      RETPot(indxCrop:indxLast:NBudgetCrops)          = PondedAg%RegionETPot(iCrop,:)
      RETPot(indxLast+indxCrop)                       = SUM(RETPot(indxCrop:indxLast:NBudgetCrops))
    END DO
    Error                                             = RSoilM_P + RSoilMCh + RInfilt + RGWInflow + RGenMoistInflow - RDrain - RETa - RPerc - RSoilM
    
    !Store in temporary array
    DummyArray(1,:)  = RLUArea                                         !Agricultural area
    DummyArray(2,:)  = RETPot                                          !Potential ET
    DummyArray(3,:)  = RPrecip                                         !Precipitation on ag lands
    DummyArray(4,:)  = RRunoff                                         !Runoff from ag lands
    DummyArray(5,:)  = RDeli + RPump                                   !Prime applied water on ag lands prior to application of re-use water
    DummyArray(6,:)  = RUpstrmElemRunoff                               !Surface runoff from upstream elements/subregions used on ag lands
    DummyArray(7,:)  = RReuse                                          !Applied recycled water on ag lands 
    DummyArray(8,:)  = RReturn                                         !Return flow from ag lands
    DummyArray(9,:)  = RSoilM_P                                        !Storage at the beginning of the time interval
    DummyArray(10,:) = RSoilMCh                                        !Soil moisture chnage due to expansion/contraction of ag lands
    DummyArray(11,:) = RInfilt                                         !Infiltration on ag lands
    DummyArray(12,:) = RGWInflow                                       !Groundwater inflow to ponded ag lands
    DummyArray(13,:) = RGenMoistInflow                                 !Generic moisture inflow to ponded ag lands
    DummyArray(14,:) = RDrain                                          !Rice/refuge pond drainage on ag lands
    DummyArray(15,:) = RETa                                            !ET on ag lands
    DummyArray(16,:) = RPerc                                           !Percolation on ag lands
    DummyArray(17,:) = RSoilM                                          !Storage at the end of the time interval
    DummyArray(18,:) = Error                                           !Mass balance error for ag lands

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
    CLASS(PondedAgDatabaseType)            :: PondedAg
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(ETType),INTENT(IN)                :: ETData
    TYPE(RootZoneSoil_v41_Type),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                     :: HydCondPonded(:),DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:,:)
    INTEGER,INTENT(IN)                     :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)        :: SolverData
    LOGICAL,INTENT(IN)                     :: lLakeElem(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'Simulate'
    INTEGER                               :: indxElem,indxCrop,KunsatMethod,iElemID
    REAL(8)                               :: AchievedConv,Area,ETc(f_iNPondedCrops),HydCond,TotalPorosity,             &
                                             FieldCapacity,TotalPorosityCrop,FieldCapacityCrop,RootDepth,Lambda,       &
                                             Supply,WiltingPoint,WiltingPointCrop,rSoilM,rSoilM_P,GM,PrecipD,          &
                                             rMultip,GMElem,Inflow,Excess,dRF,dRU,ratio(3),rSoilM_P_Array(3),          &
                                             Infilt(3),rSoilM_Array(3),ETPartition(3),ETc_effect,rRunoff,rReturnFlow,  &
                                             rPrecipInfilt,rIrigInfilt,rDrain,rETa,rETP,rETOth,rPerc,rReuse,rGMExcess, &
                                             rETFromGW_Actual,rSoilM_Precip_P,rSoilM_AW_P,rSoilM_Oth_P,rETAW
    
    !Initialize
    iStat     = 0
                                    
    !Inform user
    CALL EchoProgress('Simulating flows at ponded agricultural crop lands')
    
    ASSOCIATE (pCrops => PondedAg%Crops)
        !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,HydCondPonded,ETData,Precip,GenericMoisture,    &
        !$OMP                                  pCrops,DeltaT,PondedAg,ElemSupply,SolverData,ElemsToGW,iStat)                      
        !$OMP DO SCHEDULE(NONMONOTONIC:DYNAMIC,96)
        DO indxElem=1,AppGrid%NElements
            !Cycle if necessary
            IF (lLakeElem(indxElem)) CYCLE
            
            !Soil parameters
            WiltingPoint  = SoilsData(indxElem)%WiltingPoint
            FieldCapacity = SoilsData(indxElem)%FieldCapacity
            TotalPorosity = SoilsData(indxElem)%TotalPorosity
            HydCond       = HydCondPonded(indxElem)
            Lambda        = SoilsData(indxElem)%Lambda
            KunsatMethod  = SoilsData(indxElem)%KunsatMethod
            ETc           = ETData%GetValues(pCrops%iColETc(:,indxElem)) * DeltaT
            GMElem        = GenericMoisture(1,indxElem) * DeltaT
            PrecipD       = Precip(indxElem) * DeltaT
            
            !Loop over each land use
            DO indxCrop=1,f_iNPondedCrops
              
                !Cycle if area is zero
                Area = pCrops%Area(indxCrop,indxElem)
                IF (Area .EQ. 0.0) THEN
                    pCrops%Runoff(indxCrop,indxElem)          = 0.0   
                    pCrops%ReturnFlow(indxCrop,indxElem)      = 0.0   
                    pCrops%PrecipInfilt(indxCrop,indxElem)    = 0.0                      
                    pCrops%IrigInfilt(indxCrop,indxElem)      = 0.0 
                    pCrops%Drain(indxCrop,indxElem)           = 0.0
                    pCrops%ETa(indxCrop,indxElem)             = 0.0                     
                    pCrops%ETAW(indxCrop,indxElem)            = 0.0   
                    pCrops%ETP(indxCrop,indxElem)             = 0.0 
                    pCrops%ETOth(indxCrop,indxElem)           = 0.0
                    pCrops%Perc(indxCrop,indxElem)            = 0.0                    
                    pCrops%Reuse(indxCrop,indxElem)           = 0.0   
                    pCrops%GMExcess(indxCrop,indxElem)        = 0.0
                    pCrops%ETFromGW_Actual(indxCrop,indxElem) = 0.0
                    CYCLE
                END IF
              
                !Initialize
                RootDepth         = PondedAg%RootDepth(indxCrop)
                WiltingPointCrop  = WiltingPoint  * RootDepth
                TotalPorosityCrop = TotalPorosity * RootDepth
                FieldCapacityCrop = FieldCapacity * RootDepth
                GM                = GMElem        * RootDepth
                Supply            = ElemSupply(indxCrop,indxElem)*DeltaT/Area
                rSoilM_Precip_P   = pCrops%SoilM_Precip_P(indxCrop,indxElem)
                rSoilM_AW_P       = pCrops%SoilM_AW_P(indxCrop,indxElem)
                rSoilM_Oth_P      = pCrops%SoilM_Oth_P(indxCrop,indxElem)
                rSoilM_P          = rSoilM_Precip_P + rSoilM_AW_P + rSoilM_Oth_P
                Inflow            = GM + Supply
                dRF               = PondedAg%OperationFlowsFile%rValues(pCrops%iColReturn(indxCrop,indxElem))
                dRU               = PondedAg%OperationFlowsFile%rValues(pCrops%iColReuse(indxCrop,indxElem))
                
                !ET from GW
                rETFromGW_Actual = MIN(ETc(indxCrop) , pCrops%ETFromGW_Max(indxCrop,indxElem))
                ETc_effect       = ETc(indxCrop) - rETFromGW_Actual
                
                !Simulate
                CALL PondedLUMoistureRouter(PrecipD                                                                  ,  &
                                            pCrops%SMax(indxCrop,indxElem)                                           ,  &
                                            PondedAg%PondDepthFile%rValues(pCrops%iColPondDepth(indxCrop,indxElem))  ,  &
                                            rSoilM_P                                                                 ,  &
                                            ETc_effect                                                               ,  & 
                                            HydCond                                                                  ,  & 
                                            TotalPorosityCrop                                                        ,  & 
                                            FieldCapacityCrop                                                        ,  & 
                                            WiltingPointCrop                                                         ,  &
                                            Lambda                                                                   ,  &
                                            Inflow                                                                   ,  &
                                            SolverData%Tolerance*TotalPorosityCrop                                   ,  &
                                            KunsatMethod                                                             ,  &
                                            SolverData%IterMax                                                       ,  &
                                            rSoilM                                                                   ,  & 
                                            rRunoff                                                                  ,  & 
                                            rDrain                                                                   ,  &
                                            rPrecipInfilt                                                            ,  & 
                                            rETa                                                                     ,  & 
                                            rPerc                                                                    ,  & 
                                            Excess                                                                   ,  &
                                            AchievedConv                                                             ) 
            
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
                rIrigInfilt = Supply
            
                !Scale down the inflows into root zone if there is non-zero excess
                rReturnFlow = 0.0
                rGMExcess   = 0.0
                IF (Excess .NE. 0.0) THEN
                    !Compute return flow first before modifying other flow terms
                    rReturnFlow = MIN(MIN(dRF-dRU , Supply)  ,  Excess)
                    rIrigInfilt = Supply - rReturnFlow
                    Excess      = Excess - rReturnFlow
                    !Then compute excess inflows of other terms
                    IF (Excess .NE. 0.0) THEN
                        !If we have to further decrease AW infiltration
                        IF (Excess .GT. rPrecipInfilt+GM) THEN
                            ratio = [rPrecipInfilt , rIrigInfilt , GM]
                            CALL NormalizeArray(ratio)
                            rRunoff       = rRunoff + Excess * ratio(1)
                            rReturnFlow   = rReturnFlow + Excess * ratio(2)
                            rGMExcess     = Excess * ratio(3)
                            rPrecipInfilt = PrecipD - rRunoff
                            rIrigInfilt   = Supply - rReturnFlow
                        ELSE
                            ratio(1:2) = [rPrecipInfilt , GM]
                            CALL NormalizeArray(ratio(1:2))
                            rRunoff       = rRunoff + Excess * ratio(1)
                            rGMExcess     = Excess * ratio(2)
                            rPrecipInfilt = PrecipD - rRunoff
                        END IF
                    END IF
                END IF
                
                !Compute re-use
                rReuse = 0.0
                IF (dRF-dRU .GT. 0.0) rReuse = MIN(dRU , rReturnFlow * dRU/(dRF-dRU))
            
                !Compute moisture from precip and irrigation
                rSoilM_P_Array = [rSoilM_Precip_P , rSoilM_AW_P , rSoilM_Oth_P]
                Infilt         = [rPrecipInfilt   , rIrigInfilt , GM -rGMExcess]
                CALL TrackMoistureDueToSource(rSoilM_P_Array ,  &
                                              Infilt         ,  &
                                              rPerc          ,  &
                                              rETa           ,  &
                                              rDrain         ,  &
                                              rSoilM_Array   ,  &
                                              ETPartition    )
                pCrops%SoilM_Precip(indxCrop,indxElem) = rSoilM_Array(1)
                pCrops%SoilM_AW(indxCrop,indxElem)     = rSoilM_Array(2)
                pCrops%SoilM_Oth(indxCrop,indxElem)    = rSoilM_Array(3)
                rETP                                   = ETPartition(1)                    
                rETAW                                  = ETPartition(2)
                rETOth                                 = ETPartition(3)
            
                !Make sure soil moisture is not less than zero
                IF (ANY(rSoilM_Array.LT.0.0)) THEN
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
                rMultip          = Area / DeltaT
                rRunoff          = rRunoff          * rMultip
                rReturnFlow      = rReturnFlow      * rMultip
                rDrain           = rDrain           * rMultip
                rPrecipInfilt    = rPrecipInfilt    * rMultip
                rIrigInfilt      = rIrigInfilt      * rMultip
                rPerc            = rPerc            * rMultip
                rReuse           = rReuse           * rMultip
                rETAW            = rETAW            * rMultip
                rETP             = rETP             * rMultip
                rETOth           = rETOth           * rMultip
                rETFromGW_Actual = rETFromGW_Actual * rMultip
                rETa             = rETa             * rMultip + rETFromGW_Actual    !Includes ET from groundwater
            
                !If surface flow goes to groundwater, update the runoff processes
                IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
                    rPerc         = rPerc + rRunoff + rReturnFlow + rDrain
                    rPrecipInfilt = rPrecipInfilt + rRunoff             !Runoff and 
                    rIrigInfilt   = rIrigInfilt + rReturnFlow + rDrain  ! return flow are assumed to bypass root zone for proper mass balance       
                    rRunoff       = 0.0
                    rReturnFlow   = 0.0
                    rDrain        = 0.0
                END IF
                 
                !Store results in persistent arrays
                pCrops%Runoff(indxCrop,indxElem)          = rRunoff   
                pCrops%ReturnFlow(indxCrop,indxElem)      = rReturnFlow   
                pCrops%PrecipInfilt(indxCrop,indxElem)    = rPrecipInfilt                           
                pCrops%IrigInfilt(indxCrop,indxElem)      = rIrigInfilt        
                pCrops%Drain(indxCrop,indxElem)           = rDrain
                pCrops%ETa(indxCrop,indxElem)             = rETa          
                pCrops%ETAW(indxCrop,indxElem)            = rETAW           
                pCrops%ETP(indxCrop,indxElem)             = rETP    
                pCrops%ETOth(indxCrop,indxElem)           = rETOth 
                pCrops%Perc(indxCrop,indxElem)            = rPerc          
                pCrops%Reuse(indxCrop,indxElem)           = rReuse          
                pCrops%GMExcess(indxCrop,indxElem)        = rGMExcess   
                pCrops%ETFromGW_Actual(indxCrop,indxElem) = rETFromGW_Actual

            END DO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL
    END ASSOCIATE
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE PONDED AG DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(PondedAg,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,HydCondPonded,SpecifiedDemand,IrigPeriod,lLakeElem,lReadAgWaterDemand)
    CLASS(PondedAgDatabaseType)            :: PondedAg
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(ETType),INTENT(IN)                :: ETData
    TYPE(RootZoneSoil_v41_Type),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                     :: HydCondPonded(:),DeltaT,Precip(:),GenericMoisture(:,:),SpecifiedDemand(:)
    INTEGER,INTENT(IN)                     :: IrigPeriod(:)
    LOGICAL,INTENT(IN)                     :: lLakeElem(:),lReadAgWaterDemand
    
    !Local variables
    INTEGER :: indxElem,indxCrop
    REAL(8) :: Area,ETc(f_iNPondedCrops),RootDepth,TotalPorosityCrop,TotalPorosity,GM,  &
               FieldCapacity,FieldCapacityCrop,PondDepth,SoilM,NonFloodRiceDecompAW,    &
               PrecipD,GMElem,ETc_effect,Reuse,ReturnFlow

    !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,lLakeElem,SoilsData,ETData,Precip,GenericMoisture,PondedAg,DeltaT, &
    !$OMP                                  IrigPeriod,lReadAgWaterDemand,SpecifiedDemand,HydCondPonded) 
    !$OMP DO SCHEDULE(DYNAMIC,200)
    DO indxElem=1,AppGrid%NElements
        PondedAg%Crops%DemandRaw(:,indxElem) = 0.0  
        PondedAg%Crops%Demand(:,indxElem)    = 0.0
        IF (lLakeElem(indxElem)) CYCLE
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        ETc           = ETData%GetValues(PondedAg%Crops%iColETc(:,indxElem)) * DeltaT
        GMElem        = GenericMoisture(1,indxElem) * DeltaT
        PrecipD       = Precip(indxElem) * DeltaT
        DO indxCrop=1,f_iNPondedCrops
          
            !Cycle if Area is zero
            Area  = PondedAg%Crops%Area(indxCrop,indxElem)
            IF (Area .EQ. 0.0) CYCLE
          
            !Cycle if demand is specified 
            IF (lReadAgWaterDemand) THEN
                IF (PondedAg%iColAgDemand(indxCrop,indxElem) .GT. 0) THEN
                    PondedAg%Crops%DemandRaw(indxCrop,indxElem) = SpecifiedDemand(PondedAg%iColAgDemand(indxCrop,indxElem))
                    PondedAg%Crops%Demand(indxCrop,indxElem)    = PondedAg%Crops%DemandRaw(indxCrop,indxElem)
                    CYCLE
                END IF
            END IF
            
            !If it is non-flooded rice decomp and it is decomp time, specify demand and cycle
            IF (indxCrop .EQ. f_iindxRice_NonFloodDecomp) THEN
                NonFloodRiceDecompAW = PondedAg%OperationFlowsFile%rValues(PondedAg%iColNonFloodRiceDecompAW(indxElem))
                IF (NonFloodRiceDecompAW .GT. 0.0) THEN
                    PondedAg%Crops%Demand(indxCrop,indxElem) = NonFloodRiceDecompAW
                    CYCLE
                END IF
            END IF
            
            !Cycle if it is not an irrigation period
            IF (IrigPeriod(PondedAg%Crops%iColIrigPeriod(indxCrop,indxElem)) .EQ. f_iNoIrigPeriod) CYCLE
        
            !Initialize
            RootDepth         = PondedAg%RootDepth(indxCrop)
            PondDepth         = PondedAg%PondDepthFile%rValues(PondedAg%Crops%iColPondDepth(indxCrop,indxElem))
            ReturnFlow        = PondedAg%OperationFlowsFile%rValues(PondedAg%Crops%iColReturn(indxCrop,indxElem)) * DeltaT
            Reuse             = PondedAg%OperationFlowsFile%rValues(PondedAg%Crops%iColReuse(indxCrop,indxElem)) * DeltaT
            TotalPorosityCrop = TotalPorosity * RootDepth
            FieldCapacityCrop = FieldCapacity * RootDepth
            GM                = GMElem * RootDepth
            ETc_effect        = ETc(indxCrop) - MIN(ETc(indxCrop) , PondedAg%Crops%ETFromGW_Max(indxCrop,indxElem))
            SoilM             = PondedAg%Crops%SoilM_Precip_P(indxCrop,indxElem) + PondedAg%Crops%SoilM_AW_P(indxCrop,indxElem) + PondedAg%Crops%SoilM_Oth_P(indxCrop,indxElem)
            
            !Compute demand
            CALL PondedCropDemand(PrecipD                                      ,  &
                                  PondedAg%Crops%SMax(indxCrop,indxElem)       ,  &
                                  GM                                           ,  &
                                  ReturnFlow                                   ,  &
                                  Reuse                                        ,  &
                                  PondDepth                                    ,  &
                                  ETc_effect                                   ,  &
                                  HydCondPonded(indxElem)                      ,  &
                                  TotalPorosityCrop                            ,  & 
                                  FieldCapacityCrop                            ,  & 
                                  SoilM                                        ,  & 
                                  PondedAg%Crops%DemandRaw(indxCrop,indxElem)  ,  &
                                  PondedAg%Crops%Demand(indxCrop,indxElem)     ) 
        
            !Convert demand related data to volumetric rate and store in persistent arrays
            PondedAg%Crops%DemandRaw(indxCrop,indxElem) = PondedAg%Crops%DemandRaw(indxCrop,indxElem)/DeltaT * Area  
            PondedAg%Crops%Demand(indxCrop,indxElem)    = PondedAg%Crops%Demand(indxCrop,indxElem)/DeltaT * Area    
        
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
  ! --- COMPUTE GW INFLOW INTO ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE PondedAgLandUse_ComputeETFromGW_Max(PondedAgLand,DepthToGW,Sy,CapillaryRise)
    CLASS(PondedAgDatabaseType) :: PondedAgLand
    REAL(8),INTENT(IN)          :: DepthToGW(:),Sy(:),CapillaryRise(:)
    
    CALL ComputeETFromGW_Max(DepthToGW,Sy,PondedAgLand%RootDepth,CapillaryRise,PondedAgLand%Crops%Area,PondedAgLand%Crops%ETFromGW_Max)
    
  END SUBROUTINE PondedAgLandUse_ComputeETFromGW_Max
  
  
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
    IF (.NOT. ALLOCATED(PondedAgLand%Crops%SMax)) RETURN
    
    !Initialize
    RootDepth = PondedAgLand%RootDepth
    
    !Convert contents to depths, find initial ponding depth and equate SoilM_P to SoilM
    ASSOCIATE (pCrops => PondedAgLand%Crops)
        DO indxElem=1,NElements
            TP = TotalPorosity(indxElem) 
            DO indxCrop=1,f_iNPondedCrops
                SoilM                               = pCrops%SoilM_Precip(indxCrop,indxElem) + pCrops%SoilM_AW(indxCrop,indxElem) + pCrops%SoilM_Oth(indxCrop,indxElem)
                pCrops%PondDepth(indxCrop,indxElem) = MAX((SoilM-TP) * RootDepth(indxCrop)  ,   0.0                      )
                SoilM                               = MIN(SoilM * RootDepth(indxCrop)       ,   TP * RootDepth(indxCrop) )
                rValue(1)                           = pCrops%SoilM_Precip(indxCrop,indxElem) + pCrops%SoilM_AW(indxCrop,indxElem) + pCrops%SoilM_Oth(indxCrop,indxElem)
                IF (rValue(1) .GT. 0.0) THEN
                    rValue             = [pCrops%SoilM_Precip(indxCrop,indxElem) , pCrops%SoilM_AW(indxCrop,indxElem) , pCrops%SoilM_Oth(indxCrop,indxElem)]
                    CALL NormalizeArray(rValue)
                    pCrops%SoilM_Precip(indxCrop,indxElem) = SoilM * rValue(1)
                    pCrops%SoilM_AW(indxCrop,indxElem)     = SoilM * rValue(2)
                    pCrops%SoilM_Oth(indxCrop,indxElem)    = SoilM * rValue(3)
                END IF
            END DO
        END DO 
        pCrops%SoilM_Precip_P = pCrops%SoilM_Precip
        pCrops%SoilM_AW_P     = pCrops%SoilM_AW
        pCrops%SoilM_Oth_P    = pCrops%SoilM_Oth
    END ASSOCIATE
    
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
    DO indxElem=1,SIZE(PondedAgLand%Crops%SMax,DIM=2)
        DO indxCrop=1,PondedAgLand%NCrops
            iCol = PondedAgLand%iColAgDemand(indxCrop,indxElem)
            IF (iCol .GT. 0) THEN
                IF (rSpecifiedDemand(iCol) .GT. 0.0) THEN
                    IF (PondedAgLand%Crops%Area(indxCrop,indxElem) .EQ. 0.0) THEN
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
    CALL PondedAgLand%PondDepthFile%ReadTSData(TimeStep,'Rice/refuge pond depths data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    IF (iFileReadCode .EQ. 0) PondedAgLand%PondDepthFile%rValues = PondedAgLand%PondDepthFile%rValues * PondedAgLand%PondDepthFactor

    CALL PondedAgLand%OperationFlowsFile%File%RewindFile_To_BeginningOfTSData(iStat)                                               ;  IF (iStat .NE. 0) RETURN
    CALL PondedAgLand%OperationFlowsFile%ReadTSData(TimeStep,'Rice/refuge pond operation flows data',iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN 
    IF (iFileReadCode .EQ. 0) PondedAgLand%OperationFlowsFile%rValues = PondedAgLand%OperationFlowsFile%rValues * PondedAgLand%OperationFlowsFactor
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp
  
END MODULE