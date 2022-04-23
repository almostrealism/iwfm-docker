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
MODULE Class_NativeRiparianLandUse
  USE MessageLogger           , ONLY: SetLastMessage            , &
                                      EchoProgress              , &
                                      MessageArray              , &
                                      iFatal
  USE IOInterface
  USE TimeSeriesUtilities
  USE GeneralUtilities
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource
  USE Class_GenericLandUse
  USE Class_LandUseDataFile
  USE Class_SolverData
  USE Class_AppGrid
  USE Package_PrecipitationET , ONLY: ETType
  USE Util_RootZone_v40
  USE Package_UnsatZone
  USE Package_Misc            , ONLY: FlowDest_GWElement
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
  PUBLIC :: NativeRiparianDatabaseType                  ,  &
            NativeRiparianLandUse_New                   ,  &
            NativeRiparianLandUse_Kill                  ,  &
            NativeRiparianLandUse_ReadTSData            ,  &
            NativeRiparianLandUse_SetAreas              ,  &
            NativeRiparianLandUse_AdvanceAreas          ,  &
            NativeRiparianLandUse_SoilMContent_To_Depth ,  &
            NativeRiparianLandUse_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: NativeRiparianType
  END TYPE NativeRiparianType


  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE NativeRiparianDatabaseType
    TYPE(NativeRiparianType),ALLOCATABLE :: NativeVeg(:)
    TYPE(NativeRiparianType),ALLOCATABLE :: RiparianVeg(:)
    REAL(8)                              :: RootDepth_Native         = 0.0     
    REAL(8)                              :: RootDepth_Riparian       = 0.0     
    REAL(8),ALLOCATABLE                  :: RegionETPot_NV(:)                    !Regional potential ET for native vegetation
    REAL(8),ALLOCATABLE                  :: RegionETPot_RV(:)                    !Regional potential ET for riparian vegetation
    TYPE(LandUseDataFileType)            :: LandUseDataFile                      !Land use data file
  CONTAINS
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadElemNVArea
    PROCEDURE,PASS :: ReadElemRVArea
  END TYPE NativeRiparianDatabaseType
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 29
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_NativeRiparianLandUse::'

  


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
  ! --- NEW NATIVE AND RIPARIAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE NativeRiparianLandUse_New(cFileName,cWorkingDirectory,FactCN,NElements,NSubregions,TrackTime,NVRVLand,iStat)
    CHARACTER(LEN=*),INTENT(IN)      :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)               :: FACTCN
    INTEGER,INTENT(IN)               :: NElements,NSubregions
    LOGICAL,INTENT(IN)               :: TrackTime
    TYPE(NativeRiparianDatabaseType) :: NVRVLand
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+25) :: ThisProcedure = ModName // 'NativeRiparianLandUse_New'
    CHARACTER                    :: ALine*1000
    INTEGER                      :: ErrorCode
    REAL(8)                      :: FACT
    REAL(8),ALLOCATABLE          :: DummyArray(:,:)
    TYPE(GenericFileType)        :: NVRVFile
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Open file
    CALL NVRVFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Allocate memory
    ALLOCATE (NVRVLand%NativeVeg(NElements)        , &
              NVRVLand%RiparianVeg(NElements)      , &
              NVRVLand%RegionETPot_NV(NSubregions) , &
              NVRVLand%RegionETPot_RV(NSubregions) , &
              STAT=ErrorCode                       )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for native/riparian vegetation data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Land use data file
    CALL NVRVFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL NVRVLand%LandUseDataFile%New(cAbsPathFileName,'Native and riparian veg. area file',NElements,2,TrackTime,iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !Rooting depths
    CALL NVRVFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL NVRVFile%ReadData(NVRVLand%RootDepth_Native,iStat)    ;  IF (iStat .EQ. -1) RETURN    ;  NVRVLand%RootDepth_Native   = NVRVLand%RootDepth_Native * FACT
    CALL NVRVFile%ReadData(NVRVLand%RootDepth_Riparian,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  NVRVLand%RootDepth_Riparian = NVRVLand%RootDepth_Riparian * FACT
 
    !Read CN and ETc column pointers
    CALL AllocArray(DummyArray,NElements,5,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadRealData(NVRVFile,NElements,5,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    NVRVLand%NativeVeg%SMax      = (1000.0/DummyArray(:,2)-10.0) * FACTCN
    NVRVLand%RiparianVeg%SMax    = (1000.0/DummyArray(:,3)-10.0) * FACTCN
    NVRVLand%NativeVeg%iColETc   = INT(DummyArray(:,4))
    NVRVLand%RiparianVeg%iColETc = INT(DummyArray(:,5))
    
    !Initial conditions    
    CALL ReadRealData(NVRVFile,NElements,3,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(DummyArray(:,2:)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,2:)) .GT. 1.0         ) THEN
      MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
      MessageArray(2) = '0.0 or greater than 1.0 for native and riparian vegetation areas!'
      CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure) 
      iStat = -1
      RETURN
    END IF
    NVRVLand%NativeVeg%SoilM_Precip                  = DummyArray(:,2) 
    NVRVLand%NativeVeg%SoilM_AW                      = 0.0
    NVRVLand%NativeVeg%SoilM_Precip_P                = NVRVLand%NativeVeg%SoilM_Precip
    NVRVLand%NativeVeg%SoilM_AW_P                    = NVRVLand%NativeVeg%SoilM_AW
    NVRVLand%NativeVeg%SoilM_Precip_P_BeforeUpdate   = NVRVLand%NativeVeg%SoilM_Precip
    NVRVLand%NativeVeg%SoilM_AW_P_BeforeUpdate       = NVRVLand%NativeVeg%SoilM_AW
    NVRVLand%RiparianVeg%SoilM_Precip                = DummyArray(:,3) 
    NVRVLand%RiparianVeg%SoilM_AW                    = 0.0 
    NVRVLand%RiparianVeg%SoilM_Precip_P              = NVRVLand%RiparianVeg%SoilM_Precip
    NVRVLand%RiparianVeg%SoilM_AW_P                  = NVRVLand%RiparianVeg%SoilM_AW
    NVRVLand%RiparianVeg%SoilM_Precip_P_BeforeUpdate = NVRVLand%RiparianVeg%SoilM_Precip
    NVRVLand%RiparianVeg%SoilM_AW_P_BeforeUpdate     = NVRVLand%RiparianVeg%SoilM_AW
    
    !Close file
    CALL NVRVFile%Kill()
    
    !Free memory
    DEALLOCATE (DummyArray , STAT=ErrorCode)

  END SUBROUTINE NativeRiparianLandUse_New
  
  
  

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
  ! --- KILL NATIVE AND RIPARIAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE NativeRiparianLandUse_Kill(NVRVLand)
    TYPE(NativeRiparianDatabaseType) :: NVRVLand

    !Local variables
    INTEGER                          :: ErrorCode
    TYPE(NativeRiparianDatabaseType) :: Dummy
    
    !Deallocate arrays
    DEALLOCATE (NVRVLand%NativeVeg    , &
                NVRVLand%RiparianVeg  , &
                STAT = ErrorCode      )
    
    !Close files
    CALL NVRVLand%LandUseDataFile%Kill()
    
    !Assign default values to components
    NVRVLand = Dummy


  END SUBROUTINE NativeRiparianLandUse_Kill  
    
    
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
  ! --- SET THE LAND USE AREAS
  ! -------------------------------------------------------------
  SUBROUTINE NativeRiparianLandUse_SetAreas(Area,NVRVLand)
    REAL(8),INTENT(IN)               :: Area(:,:)
    TYPE(NativeRiparianDatabaseType) :: NVRVLand
   
    NVRVLand%NativeVeg%Area   = Area(1,:)
    NVRVLand%RiparianVeg%Area = Area(2,:)
    
  END SUBROUTINE NativeRiparianLandUse_SetAreas
  
  
  
  
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
  SUBROUTINE ReadRestartData(NVRVLand,InFile,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    TYPE(GenericFileType)             :: InFile
    INTEGER,INTENT(OUT)               :: iStat
    
    CALL InFile%ReadData(NVRVLand%NativeVeg%Runoff,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%Area_P,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%Area,iStat)              ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Precip_P,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Precip,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_AW_P,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_AW,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Oth_P,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Oth,iStat)         ;  IF (iStat .EQ. -1) RETURN
                                                                     
    CALL InFile%ReadData(NVRVLand%RiparianVeg%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Oth,iStat)       ;  IF (iStat .EQ. -1) RETURN

  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR NATIVE AND RIPARIAN VEG
  ! -------------------------------------------------------------
  SUBROUTINE NativeRiparianLandUse_ReadTSData(TimeStep,AppGrid,NVRVLand,iStat)
    TYPE(TimeStepType),INTENT(IN)    :: TimeStep
    TYPE(AppGridType),INTENT(IN)     :: AppGrid
    TYPE(NativeRiparianDatabaseType) :: NVRVLand
    INTEGER,INTENT(OUT)              :: iStat
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Reading time series data for native and riparian vegitation lands')
    
    !Land use areas
    CALL NVRVLand%LandUseDataFile%ReadTSData('Native and riparian veg. areas',TimeStep,AppGrid%AppElement%Area,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (NVRVLAnd%LandUseDataFile%lUpdated) THEN
      NVRVLand%NativeVeg%Area   = NVRVLand%LandUseDataFile%rValues(:,2)
      NVRVLand%RiparianVeg%Area = NVRVLand%LandUseDataFile%rValues(:,3)
    END IF
    
  END SUBROUTINE NativeRiparianLandUse_ReadTSData
  
  
  ! -------------------------------------------------------------
  ! --- READ NATIVE VEG. AREA AT AN ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE ReadElemNVArea(NVRVLand,iElem,lForInquiry,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    INTEGER,INTENT(IN)                :: iElem
    LOGICAL,INTENT(IN)                :: lForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cReadBeginDateAndTime,cReadEndDateAndTime
    INTEGER,INTENT(OUT)               :: nActualOutput,iStat
    REAL(8),INTENT(OUT)               :: ElemLandUse(:),rOutputDates(:)   
    
    !Local variables
    INTEGER :: FileReadCode,iPathNameIndex
    
    !Ptahname index to read data from DSS file
    iPathNameIndex = (iElem-1)*2 + 1
    
    !ReadData
    CALL NVRVLand%LandUseDataFile%ReadData(iElem,2,iPathNameIndex,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,FileReadCode,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Unit conversion
    ElemLandUse(1:nActualOutput) = ElemLandUse(1:nActualOutput) * NVRVLand%LandUseDataFile%Fact
    
    !Rewind land use file if it was opened for querying
    IF (lForInquiry) CALL NVRVLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)     
    
  END SUBROUTINE ReadElemNVArea
  

  ! -------------------------------------------------------------
  ! --- READ RIPARIAN VEG. AREA AT AN ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE ReadElemRVArea(NVRVLand,iElem,lForInquiry,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    INTEGER,INTENT(IN)                :: iElem
    LOGICAL,INTENT(IN)                :: lForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cReadBeginDateAndTime,cReadEndDateAndTime
    INTEGER,INTENT(OUT)               :: nActualOutput,iStat
    REAL(8),INTENT(OUT)               :: ElemLandUse(:),rOutputDates(:)   
    
    !Local variables
    INTEGER :: FileReadCode,iPathNameIndex
    
    !Ptahname index to read data from DSS file
    iPathNameIndex = (iElem-1)*2 + 2
    
    !ReadData
    CALL NVRVLand%LandUseDataFile%ReadData(iElem,3,iPathNameIndex,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN

    !Unit conversion
    ElemLandUse(1:nActualOutput) = ElemLandUse(1:nActualOutput) * NVRVLand%LandUseDataFile%Fact
    
    !Rewind land use file if it was opened for querying
    IF (lForInquiry) CALL NVRVLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat) 
    
  END SUBROUTINE ReadElemRVArea



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
  SUBROUTINE PrintRestartData(NVRVLand,OutFile)
    CLASS(NativeRiparianDatabaseType),INTENT(IN) :: NVRVLand
    TYPE(GenericFileType)                        :: OutFile
    
    CALL OutFile%WriteData(NVRVLand%NativeVeg%Runoff)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%Area_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%Area)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Precip_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Precip)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_AW_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_AW)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Oth_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Oth)
    
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%Runoff)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%Area_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%Area)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Precip_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Precip)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_AW_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_AW)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Oth_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Oth)

  END SUBROUTINE PrintRestartData
  
  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SIMULATION OF ROOT ZONE RUNOFF PROCESSES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SIMULATE FLOW PROCESSES 
  ! -------------------------------------------------------------
  SUBROUTINE NativeRiparianLandUse_Simulate(AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,ElemSupply,ElemsToGW,SolverData,lLakeElem,NVRVLand,iStat)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(ETType)                      :: ETData
    TYPE(RootZoneSoilType),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:)
    INTEGER,INTENT(IN)                :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    LOGICAL,INTENT(IN)                :: lLakeElem(:)
    TYPE(NativeRiparianDatabaseType)  :: NVRVLand
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'NativeRiparianLandUse_Simulate'
    INTEGER                      :: indxElem,iColETc(2),KunsatMethod
    REAL(8)                      :: AchievedConv,ETc(2),HydCond,TotalPorosity,Area_NV,Area_RV, &
                                    FieldCapacity,TotalPorosityCrop,FieldCapacityCrop,Lambda,  &
                                    RootDepthNV,RootDepthRV,Supply,WiltingPoint,GMRV,Excess,   &
                                    WiltingPointCrop,SoilM,SoilM_P,GMNV,rMultip,PrecipD,Inflow,&
                                    ratio(2),SoilM_P_Array(3),SoilM_Array(3),ETPartition(3),   &
                                    Infilt(3)
    LOGICAL                      :: lElemFlowToGW,lNegativeMoist
    
    !Initialize
    iStat = 0
  
    !Inform user
    CALL EchoProgress('Simulating flows at native and riparian vegetation lands')
    
    ASSOCIATE (pNV     => NVRVLand%NativeVeg      , &
               pRV     => NVRVLand%RiparianVeg    )

      !Initialize
      pNV%Runoff       = 0.0  ;  pRV%Runoff       = 0.0
      pNV%PrecipInfilt = 0.0  ;  pRV%PrecipInfilt = 0.0   
      pNV%ETa          = 0.0  ;  pRV%ETa          = 0.0   
      pNV%Perc         = 0.0  ;  pRV%Perc         = 0.0 
      pNV%GMExcess     = 0.0  ;  pRV%GMExcess     = 0.0
      RootDepthNV      = NVRVLand%RootDepth_Native 
      RootDepthRV      = NVRVLand%RootDepth_Riparian
    
      DO indxElem=1,AppGrid%NElements
        !Cycle if native and riparian beg areas are both zero
        Area_NV = pNV(indxElem)%Area
        Area_RV = pRV(indxElem)%Area
        IF (Area_NV .EQ. 0.0   .AND.   Area_RV .EQ. 0.0) CYCLE
        
        !Cycle if it is lake element
        IF (lLakeElem(indxElem)) CYCLE
        
        !Does the surface flow go to groundwater
        IF (LocateInList(indxElem,ElemsToGW) .GT. 0) THEN
            lElemFlowToGW = .TRUE.
        ELSE
            lElemFlowToGW = .FALSE.
        END IF
        
        WiltingPoint  = SoilsData(indxElem)%WiltingPoint
        FieldCapacity = SoilsData(indxElem)%FieldCapacity
        TotalPorosity = SoilsData(indxElem)%TotalPorosity
        HydCond       = SoilsData(indxElem)%HydCond
        Lambda        = SoilsData(indxElem)%Lambda
        KunsatMethod  = SoilsData(indxElem)%KunsatMethod
        iColETc(1)    = pNV(indxElem)%iColETc
        iColETc(2)    = pRV(indxElem)%iColETc
        ETc           = ETData%GetValues(iColETc)
        ASSOCIATE (pNVElem => pNV(indxElem) , &
                   pRVElem => pRV(indxElem) )
        
          !Water supply as runoff from upstream elements plus precip 
          Supply = ElemSupply(indxElem) * DeltaT / (Area_NV+Area_RV) 
          
          !Precipitation
          PrecipD = Precip(indxElem) * DeltaT
          
          !Simulation of native veg
          IF (Area_NV .GT. 0.0) THEN
            !Initialize
            TotalPorosityCrop = TotalPorosity * RootDepthNV
            FieldCapacityCrop = FieldCapacity * RootDepthNV
            WiltingPointCrop  = WiltingPoint  * RootDepthNV
            SoilM_P           = pNVElem%SoilM_Precip_P + pNVElem%SoilM_AW_P + pNVElem%SoilM_Oth_P
            GMNV              = GenericMoisture(1,indxElem) * RootDepthNV * DeltaT
            Inflow            = Supply + GMNV

            !Simulate
            CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                           pNVElem%SMax                           ,  &
                                           SoilM_P                                ,  &
                                           ETc(1)*DeltaT                          ,  & 
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
                                           pNVElem%Runoff                         ,  & 
                                           pNVElem%PrecipInfilt                   ,  & 
                                           pNVElem%ETa                            ,  & 
                                           pNVElem%Perc                           ,  & 
                                           Excess                                 ,  &
                                           AchievedConv                           ) 
                                         
            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
              MessageArray(1) = 'Convergence error in soil moisture routing for native vegetation!'
              MessageArray(2) = 'Element              = '//TRIM(IntToText(indxElem))
              WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
              WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
              CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
              iStat = -1
              RETURN
            END IF

            !Reduce total infiltration based on correction for total porosity
            IF (Excess .NE. 0.0) THEN
                ratio = [pNVElem%PrecipInfilt , GMNV]
                CALL NormalizeArray(ratio)
                pNVElem%Runoff       = pNVElem%Runoff + Excess * ratio(1)
                pNVElem%GMExcess     = Excess * ratio(2)
                pNVElem%PrecipInfilt = PrecipD - pNVElem%Runoff
            END IF
      
            !Compute moisture from precip and irrigation
            SoilM_P_Array = [pNVElem%SoilM_Precip_P , pNVElem%SoilM_AW_P , pNVElem%SoilM_Oth_P    ]
            Infilt        = [pNVElem%PrecipInfilt   , 0d0                , GMNV - pNVElem%GMExcess]
            CALL TrackMoistureDueToSource(SoilM_P_Array    ,  &
                                          Infilt           ,  &
                                          pNVElem%Perc     ,  &
                                          pNVElem%ETa      ,  &
                                          0d0              ,  &
                                          SoilM_Array      ,  &
                                          ETPartition      )
            pNVElem%SoilM_Precip = SoilM_Array(1)
            pNVElem%SoilM_AW     = SoilM_Array(2)
            pNVElem%SoilM_Oth    = SoilM_Array(3)
            
            !Make sure soil moisture is not less than zero
            lNegativeMoist = .FALSE.
            IF (pNVElem%SoilM_Precip .LT. 0.0) lNegativeMoist = .TRUE.
            IF (pNVElem%SoilM_AW     .LT. 0.0) lNegativeMoist = .TRUE.
            IF (pNVElem%SoilM_Oth    .LT. 0.0) lNegativeMoist = .TRUE.
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
            rMultip              = Area_NV / DeltaT
            pNVElem%Runoff       = pNVElem%Runoff       * rMultip
            pNVElem%PrecipInfilt = pNVElem%PrecipInfilt * rMultip
            pNVElem%ETa          = pNVElem%ETa          * rMultip
            pNVElem%Perc         = pNVElem%Perc         * rMultip  

            !If surface flow goes to groundwater, update the runoff processes
            IF (lElemFlowToGW) THEN
              pNVElem%Perc         = pNVElem%Perc + pNVElem%Runoff
              pNVElem%PrecipInfilt = pNVElem%PrecipInfilt + pNVElem%Runoff        !Runoff is assumed to bypass root zone for proper mass balance    
              pNVElem%Runoff       = 0.0
            END IF
          END IF
          
          !Simulation of riparian veg
          IF (Area_RV .GT. 0.0) THEN
            !Initialize
            TotalPorosityCrop = TotalPorosity * RootDepthRV
            FieldCapacityCrop = FieldCapacity * RootDepthRV
            WiltingPointCrop  = WiltingPoint  * RootDepthRV
            SoilM_P           = pRVElem%SoilM_Precip_P + pRVElem%SoilM_AW_P + pRVElem%SoilM_Oth_P
            GMRV              = GenericMoisture(1,indxElem) * RootDepthRV * DeltaT
            Inflow            = Supply + GMRV
          
            !Simulate
            CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                           pRVElem%SMax                           ,  &
                                           SoilM_P                                ,  &
                                           ETc(2)*DeltaT                          ,  & 
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
                                           pRVElem%Runoff                         ,  & 
                                           pRVElem%PrecipInfilt                   ,  & 
                                           pRVElem%ETa                            ,  & 
                                           pRVElem%Perc                           ,  & 
                                           Excess                                 ,  &
                                           AchievedConv                           ) 
                                         
            !Generate error if convergence is not achieved
            IF (AchievedConv .NE. 0.0) THEN
              MessageArray(1) = 'Convergence error in soil moisture routing for riparian vegetation!'
              MessageArray(2) = 'Element              = '//TRIM(IntToText(indxElem))
              WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
              WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
              CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
              iStat = -1
              RETURN
            END IF

            !Reduce total infiltration based on correction for total porosity
            IF (Excess .NE. 0.0) THEN
                ratio = [pRVElem%PrecipInfilt , GMRV]
                CALL NormalizeArray(ratio)
                pRVElem%Runoff       = pRVElem%Runoff + Excess * ratio(1)
                pRVElem%GMExcess     = Excess * ratio(2)
                pRVElem%PrecipInfilt = PrecipD - pRVElem%Runoff
            END IF
      
            !Compute moisture from precip and irrigation
            SoilM_P_Array = [pRVElem%SoilM_Precip_P , pRVElem%SoilM_AW_P , pRVElem%SoilM_Oth_P    ]
            Infilt        = [pRVElem%PrecipInfilt   , 0d0                , GMRV - pRVElem%GMExcess]
            CALL TrackMoistureDueToSource(SoilM_P_Array    ,  &
                                          Infilt           ,  &
                                          pRVElem%Perc     ,  &
                                          pRVElem%ETa      ,  &
                                          0d0              ,  &
                                          SoilM_Array      ,  &
                                          ETPartition      )
            pRVElem%SoilM_Precip = SoilM_Array(1)
            pRVElem%SoilM_AW     = SoilM_Array(2)
            pRVElem%SoilM_Oth    = SoilM_Array(3)
 
            !Make sure soil moisture is not less than zero
            lNegativeMoist = .FALSE.
            IF (pRVElem%SoilM_Precip .LT. 0.0) lNegativeMoist = .TRUE.
            IF (pRVElem%SoilM_AW     .LT. 0.0) lNegativeMoist = .TRUE.
            IF (pRVElem%SoilM_Oth    .LT. 0.0) lNegativeMoist = .TRUE.
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
            rMultip              = Area_RV / DeltaT
            pRVElem%Runoff       = pRVElem%Runoff       * rMultip
            pRVElem%PrecipInfilt = pRVElem%PrecipInfilt * rMultip
            pRVElem%ETa          = pRVElem%ETa          * rMultip
            pRVElem%Perc         = pRVElem%Perc         * rMultip  

            !If surface flow goes to groundwater, update the runoff processes
            IF (lElemFlowToGW) THEN
              pRVElem%Perc         = pRVElem%Perc + pRVElem%Runoff
              pRVElem%PrecipInfilt = pRVElem%PrecipInfilt + pRVElem%Runoff        !Runoff is assumed to bypass root zone for proper mass balance    
              pRVElem%Runoff       = 0.0
            END IF
          END IF
        END ASSOCIATE                                 
      END DO
    END ASSOCIATE  
                                   
  END SUBROUTINE NativeRiparianLandUse_Simulate
  



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
  SUBROUTINE NativeRiparianLandUse_SoilMContent_To_Depth(NElements,TotalPorosity,NVRVLand,iStat)
    INTEGER,INTENT(IN)               :: NElements
    REAL(8),INTENT(IN)               :: TotalPorosity(:)
    TYPE(NativeRiparianDatabaseType) :: NVRVLand
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+43) :: ThisProcedure = ModName // 'NativeRiparianLandUse_SoilMContent_To_Depth'
    INTEGER                      :: indxElem
    REAL(8)                      :: RootDepth_NV,RootDepth_RV
    
    !Initialize
    iStat = 0
    
    !Return if native and riparian lands are not simulated
    IF (SIZE(NVRVLand%NativeVeg).EQ.0  .AND.  SIZE(NVRVLand%RiparianVeg).EQ.0) RETURN
    
    !Initialize
    RootDepth_NV = NVRVLand%RootDepth_Native
    RootDepth_RV = NVRVLand%RootDepth_Riparian
    
    !Check if initial conditions are greater than total porosity, if not convert conetnts to depths and equate SoilM_P to SoilM
    DO indxElem=1,NElements
      ASSOCIATE (pNV => NVRVLand%NativeVeg(indxElem)   , &
                 pRV => NVRVLand%RiparianVeg(indxElem) ) 
                 
        IF ((pNV%SoilM_Precip + pNV%SoilM_AW + pNV%SoilM_Oth) .GT. TotalPorosity(indxElem)) THEN
            CALL SetLastMessage('Initial moisture content for native vegetation at element ' // TRIM(IntToText(indxElem)) // ' is greater than total porosity!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF ((pRV%SoilM_Precip + pRV%SoilM_AW + pRV%SoilM_Oth) .GT. TotalPorosity(indxElem)) THEN
            CALL SetLastMessage('Initial moisture content for riparian vegetation at element ' // TRIM(IntToText(indxElem)) // ' is greater than total porosity!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        pNV%SoilM_Precip   = pNV%SoilM_Precip * RootDepth_NV
        pNV%SoilM_AW       = pNV%SoilM_AW     * RootDepth_NV
        pNV%SoilM_Oth      = pNV%SoilM_Oth    * RootDepth_NV
        pRV%SoilM_Precip   = pRV%SoilM_Precip * RootDepth_RV
        pRV%SoilM_AW       = pRV%SoilM_AW     * RootDepth_RV
        pRV%SoilM_Oth      = pRV%SoilM_Oth    * RootDepth_RV
        pNV%SoilM_Precip_P = pNV%SoilM_Precip
        pNV%SoilM_AW_P     = pNV%SoilM_AW
        pNV%SoilM_Oth_P    = pNV%SoilM_Oth
        pRV%SoilM_Precip_P = pRV%SoilM_Precip
        pRV%SoilM_AW_P     = pRV%SoilM_AW
        pRV%SoilM_Oth_P    = pRV%SoilM_Oth
   
      END ASSOCIATE
    END DO 
    
  END SUBROUTINE NativeRiparianLandUse_SoilMContent_To_Depth
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE NativeRiparianLandUse_AdvanceAreas(NVRVLand) 
    TYPE(NativeRiparianDatabaseType) :: NVRVLand
    
    NVRVLand%NativeVeg%Area_P   = NVRVLand%NativeVeg%Area
    NVRVLand%RiparianVeg%Area_P = NVRVLand%RiparianVeg%Area
    
  END SUBROUTINE NativeRiparianLandUse_AdvanceAreas

  
END MODULE