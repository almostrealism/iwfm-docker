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
MODULE Class_NativeRiparianLandUse
  !$ USE OMP_LIB
  USE MessageLogger           , ONLY: SetLastMessage                , &
                                      EchoProgress                  , &
                                      MessageArray                  , &
                                      f_iFatal                        
  USE IOInterface             , ONLY: GenericFileType               
  USE TimeSeriesUtilities     , ONLY: TimeStepType                  
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter       , &
                                      IntToText                     , &
                                      CleanSpecialCharacters        , &
                                      EstablishAbsolutePathFilename , &
                                      LocateInList                  , &
                                      NormalizeArray
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource
  USE Class_GenericLandUse    , ONLY: GenericLandUseType
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType
  USE Util_Package_RootZone   , ONLY: ReadRealData
  USE Class_AppGrid           , ONLY: AppGridType
  USE Package_PrecipitationET , ONLY: ETType
  USE Package_UnsatZone       , ONLY: RootZoneSoilType              , &
                                      NonPondedLUMoistureRouter     
  USE Package_Misc            , ONLY: SolverDataType                , &
                                      f_iFlowDest_GWElement         
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
  PUBLIC :: NativeRiparianDatabaseType                  
  
  
  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: NativeRiparianType
  END TYPE NativeRiparianType


  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE NativeRiparianDatabaseType
    TYPE(NativeRiparianType)             :: NativeVeg
    TYPE(NativeRiparianType)             :: RiparianVeg
    REAL(8)                              :: RootDepth_Native         = 0.0     
    REAL(8)                              :: RootDepth_Riparian       = 0.0     
    REAL(8),ALLOCATABLE                  :: RegionETPot_NV(:)                    !Regional potential ET for native vegetation
    REAL(8),ALLOCATABLE                  :: RegionETPot_RV(:)                    !Regional potential ET for riparian vegetation
    TYPE(LandUseDataFileType)            :: LandUseDataFile                      !Land use data file
  CONTAINS
    PROCEDURE,PASS :: New                   
    PROCEDURE,PASS :: Kill                  
    PROCEDURE,PASS :: SetAreas              
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: ReadElemNVArea
    PROCEDURE,PASS :: ReadElemRVArea
    PROCEDURE,PASS :: ReadTSData            
    PROCEDURE,PASS :: Simulate
    PROCEDURE,PASS :: AdvanceAreas          
    PROCEDURE,PASS :: SoilMContent_To_Depth 
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp 
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
  SUBROUTINE New(NVRVLand,cFileName,cWorkingDirectory,FactCN,NElements,NSubregions,iElemIDs,TrackTime,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)                :: FACTCN
    INTEGER,INTENT(IN)                :: NElements,NSubregions,iElemIDs(NElements)
    LOGICAL,INTENT(IN)                :: TrackTime
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    CHARACTER                   :: ALine*1000
    INTEGER                     :: ErrorCode,iElem,ID,indxElem,iStat1
    REAL(8)                     :: FACT
    REAL(8),ALLOCATABLE         :: DummyArray(:,:)
    LOGICAL                     :: lProcessed(NElements)
    TYPE(GenericFileType)       :: NVRVFile
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Open file
    CALL NVRVFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Allocate memory
    CALL NVRVLand%NativeVeg%New(Nelements,1,iStat1)
    CALL NVRVLand%RiparianVeg%New(Nelements,1,iStat)
    ALLOCATE (NVRVLand%RegionETPot_NV(NSubregions) , &
              NVRVLand%RegionETPot_RV(NSubregions) , &
              STAT=ErrorCode                       )
    IF (ErrorCode+iStat+iStat1 .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for native/riparian vegetation data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize variables
    NVRVLand%RegionETPot_NV = 0.0
    NVRVLand%RegionETPot_RV = 0.0
    
    !Land use data file
    CALL NVRVFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL NVRVLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Native and riparian veg. area file',NElements,2,TrackTime,iStat) 
    IF (iStat .EQ. -1) RETURN
    
    !Rooting depths
    CALL NVRVFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL NVRVFile%ReadData(NVRVLand%RootDepth_Native,iStat)    ;  IF (iStat .EQ. -1) RETURN    ;  NVRVLand%RootDepth_Native   = NVRVLand%RootDepth_Native * FACT
    CALL NVRVFile%ReadData(NVRVLand%RootDepth_Riparian,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  NVRVLand%RootDepth_Riparian = NVRVLand%RootDepth_Riparian * FACT
 
    !Read CN and ETc column pointers
    CALL ReadRealData(NVRVFile,'curve numbers and evapotranspiration column pointers for native and riparian vegetation','elements',NElements,5,iElemIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('curve numbers and evapotranspiration column pointers for native and riparian vegetation at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                     = .TRUE.
        NVRVLand%NativeVeg%SMax(iElem,1)      = (1000.0/DummyArray(indxElem,2)-10.0) * FACTCN
        NVRVLand%RiparianVeg%SMax(iElem,1)    = (1000.0/DummyArray(indxElem,3)-10.0) * FACTCN
        NVRVLand%NativeVeg%iColETc(iElem,1)   = INT(DummyArray(indxElem,4))
        NVRVLand%RiparianVeg%iColETc(iElem,1) = INT(DummyArray(indxElem,5))
    END DO
    
    !Initial conditions    
    CALL ReadRealData(NVRVFile,'initial conditions for native and riparian vegetation','elements',NElements,3,iElemIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (MINVAL(DummyArray(:,2:)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,2:)) .GT. 1.0         ) THEN
      MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
      MessageArray(2) = '0.0 or greater than 1.0 for native and riparian vegetation areas!'
      CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure) 
      iStat = -1
      RETURN
    END IF
    lProcessed = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyArray(indxElem,1))
        IF (lProcessed(iElem)) THEN
            ID = iElemIDs(iElem)
            CALL SetLastMessage('Initial conditions for native and riparian vegetation at element '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iElem)                                         = .TRUE.
        NVRVLand%NativeVeg%SoilM_Precip(iElem,1)                  = DummyArray(indxElem,2) 
        NVRVLand%NativeVeg%SoilM_AW(iElem,1)                      = 0.0
        NVRVLand%NativeVeg%SoilM_Precip_P(iElem,1)                = NVRVLand%NativeVeg%SoilM_Precip(iElem,1)
        NVRVLand%NativeVeg%SoilM_AW_P(iElem,1)                    = NVRVLand%NativeVeg%SoilM_AW(iElem,1)
        NVRVLand%NativeVeg%SoilM_Precip_P_BeforeUpdate(iElem,1)   = NVRVLand%NativeVeg%SoilM_Precip(iElem,1)
        NVRVLand%NativeVeg%SoilM_AW_P_BeforeUpdate(iElem,1)       = NVRVLand%NativeVeg%SoilM_AW(iElem,1)
        NVRVLand%RiparianVeg%SoilM_Precip(iElem,1)                = DummyArray(indxElem,3) 
        NVRVLand%RiparianVeg%SoilM_AW(iElem,1)                    = 0.0 
        NVRVLand%RiparianVeg%SoilM_Precip_P(iElem,1)              = NVRVLand%RiparianVeg%SoilM_Precip(iElem,1)
        NVRVLand%RiparianVeg%SoilM_AW_P(iElem,1)                  = NVRVLand%RiparianVeg%SoilM_AW(iElem,1)
        NVRVLand%RiparianVeg%SoilM_Precip_P_BeforeUpdate(iElem,1) = NVRVLand%RiparianVeg%SoilM_Precip(iElem,1)
        NVRVLand%RiparianVeg%SoilM_AW_P_BeforeUpdate(iElem,1)     = NVRVLand%RiparianVeg%SoilM_AW(iElem,1)
    END DO
    
    !Close file
    CALL NVRVFile%Kill()
    
    !Free memory
    DEALLOCATE (DummyArray , STAT=ErrorCode)

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
  ! --- KILL NATIVE AND RIPARIAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(NVRVLand)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand

    !Local variables
    INTEGER                          :: ErrorCode
    TYPE(NativeRiparianDatabaseType) :: Dummy
    
    !Deallocate arrays
    CALL NVRVLand%NativeVeg%Kill()
    CALL NVRVLand%RiparianVeg%Kill()
    DEALLOCATE (NVRVLand%RegionETPot_NV  , &
                NVRVLand%RegionETPot_RV  , &
                STAT = ErrorCode         )
    
    !Close files
    CALL NVRVLand%LandUseDataFile%Kill()
    
    !Assign default values to components
    SELECT TYPE (NVRVLand)
        TYPE IS (NativeRiparianDatabaseType)
            NVRVLand = Dummy
    END SELECT

  END SUBROUTINE Kill  
    
    
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
  SUBROUTINE SetAreas(NVRVLand,Area)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    REAL(8),INTENT(IN)                :: Area(:,:)
   
    NVRVLand%NativeVeg%Area(:,1)   = Area(1,:)
    NVRVLand%RiparianVeg%Area(:,1) = Area(2,:)
    
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
  SUBROUTINE ReadTSData(NVRVLand,TimeStep,AppGrid,iElemIDs,rElemAreas,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    INTEGER,INTENT(IN)                :: iElemIDs(AppGrid%NElements)
    REAL(8),INTENT(IN)                :: rElemAreas(AppGrid%NElements)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Reading time series data for native and riparian vegitation lands')
    
    !Land use areas
    CALL NVRVLand%LandUseDataFile%ReadTSData('Native and riparian veg. areas',TimeStep,rElemAreas,iElemIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (NVRVLAnd%LandUseDataFile%lUpdated) THEN
        NVRVLand%NativeVeg%Area(:,1)   = NVRVLand%LandUseDataFile%rValues(:,2)
        NVRVLand%RiparianVeg%Area(:,1) = NVRVLand%LandUseDataFile%rValues(:,3)
    END IF
    
  END SUBROUTINE ReadTSData
  
  
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
    CALL NVRVLand%LandUseDataFile%ReadTSData(iElem,2,iPathNameIndex,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,FileReadCode,iStat)  
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
    CALL NVRVLand%LandUseDataFile%ReadTSData(iElem,3,iPathNameIndex,cReadBeginDateAndTime,cReadEndDateAndTime,nActualOutput,ElemLandUse,rOutputDates,FileReadCode,iStat)
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
  SUBROUTINE Simulate(NVRVLand,AppGrid,ETData,DeltaT,Precip,GenericMoisture,SoilsData,ElemSupply,ElemsToGW,SolverData,lLakeElem,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(ETType),INTENT(IN)           :: ETData
    TYPE(RootZoneSoilType),INTENT(IN) :: SoilsData(AppGrid%NElements)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:),GenericMoisture(:,:),ElemSupply(:)
    INTEGER,INTENT(IN)                :: ElemsToGW(:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    LOGICAL,INTENT(IN)                :: lLakeElem(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    INTEGER                     :: indxElem,iColETc(2),KunsatMethod,iElemID
    REAL(8)                     :: AchievedConv,ETc(2),HydCond,TotalPorosity,Area_NV,Area_RV, &
                                   FieldCapacity,TotalPorosityCrop,FieldCapacityCrop,Lambda,  &
                                   RootDepthNV,RootDepthRV,Supply,WiltingPoint,GMRV,Excess,   &
                                   WiltingPointCrop,SoilM,SoilM_P,GMNV,rMultip,PrecipD,Inflow,&
                                   ratio(2),SoilM_P_Array(3),SoilM_Array(3),ETPartition(3),   &
                                   Infilt(3)
    LOGICAL                     :: lElemFlowToGW
    
    !Initialize
    iStat = 0
  
    !Inform user
    CALL EchoProgress('Simulating flows at native and riparian vegetation lands')
    
    !Root depth 
    RootDepthNV = NVRVLand%RootDepth_Native 
    RootDepthRV = NVRVLand%RootDepth_Riparian
    
    ASSOCIATE (pNV => NVRVLand%NativeVeg   , &
               pRV => NVRVLand%RiparianVeg )
        !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(AppGrid,NVRVLand,lLakeElem,ETData,SoilsData,DeltaT,Precip,iStat,RootDepthNV,  &
        !$OMP                                  pNV,pRV,RootDepthRV,GenericMoisture,ElemSupply,ElemsToGW,SolverData)  
        !$OMP DO SCHEDULE(NONMONOTONIC:DYNAMIC,96)
        DO indxElem=1,AppGrid%NElements
            !Initalize flows
            pNV%Runoff(indxElem,1)          = 0.0  ;  pRV%Runoff(indxElem,1)          = 0.0
            pNV%PrecipInfilt(indxElem,1)    = 0.0  ;  pRV%PrecipInfilt(indxElem,1)    = 0.0   
            pNV%ETa(indxElem,1)             = 0.0  ;  pRV%ETa(indxElem,1)             = 0.0   
            pNV%Perc(indxElem,1)            = 0.0  ;  pRV%Perc(indxElem,1)            = 0.0 
            pNV%GMExcess(indxElem,1)        = 0.0  ;  pRV%GMExcess(indxElem,1)        = 0.0
            IF (iStat .EQ. -1) CYCLE
            
            !Cycle if native and riparian veg areas are both zero
            Area_NV = pNV%Area(indxElem,1)
            Area_RV = pRV%Area(indxElem,1)
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
            iColETc(1)    = pNV%iColETc(indxElem,1)
            iColETc(2)    = pRV%iColETc(indxElem,1)
            ETc           = ETData%GetValues(iColETc)
            
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
                SoilM_P           = pNV%SoilM_Precip_P(indxElem,1) + pNV%SoilM_AW_P(indxElem,1) + pNV%SoilM_Oth_P(indxElem,1)
                GMNV              = GenericMoisture(1,indxElem) * RootDepthNV * DeltaT
                Inflow            = Supply + GMNV
                
                !Simulate
                CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                               pNV%SMax(indxElem,1)                   ,  &
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
                                               pNV%Runoff(indxElem,1)                 ,  & 
                                               pNV%PrecipInfilt(indxElem,1)           ,  & 
                                               pNV%ETa(indxElem,1)                    ,  & 
                                               pNV%Perc(indxElem,1)                   ,  & 
                                               Excess                                 ,  &
                                               AchievedConv                           ) 
                                             
                !Generate error if convergence is not achieved
                IF (AchievedConv .NE. 0.0) THEN
                    !$OMP CRITICAL
                    iElemID         = AppGrid%AppElement(indxElem)%ID
                    MessageArray(1) = 'Convergence error in soil moisture routing for native vegetation!'
                    MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                    WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                    WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                    CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                    iStat = -1
                    !$OMP END CRITICAL
                    CYCLE
                END IF
                
                !Reduce total infiltration based on correction for total porosity
                IF (Excess .NE. 0.0) THEN
                    ratio = [pNV%PrecipInfilt(indxElem,1) , GMNV]
                    CALL NormalizeArray(ratio)
                    pNV%Runoff(indxElem,1)       = pNV%Runoff(indxElem,1) + Excess * ratio(1)
                    pNV%GMExcess(indxElem,1)     = Excess * ratio(2)
                    pNV%PrecipInfilt(indxElem,1) = PrecipD - pNV%Runoff(indxElem,1)
                END IF
                
                !Compute moisture from precip and irrigation
                SoilM_P_Array = [pNV%SoilM_Precip_P(indxElem,1) , pNV%SoilM_AW_P(indxElem,1) , pNV%SoilM_Oth_P(indxElem,1)    ]
                Infilt        = [pNV%PrecipInfilt(indxElem,1)   , 0d0                        , GMNV - pNV%GMExcess(indxElem,1)]
                CALL TrackMoistureDueToSource(SoilM_P_Array        ,  &
                                              Infilt               ,  &
                                              pNV%Perc(indxElem,1) ,  &
                                              pNV%ETa(indxElem,1)  ,  &
                                              0d0                  ,  &
                                              SoilM_Array          ,  &
                                              ETPartition          )
                pNV%SoilM_Precip(indxElem,1) = SoilM_Array(1)
                pNV%SoilM_AW(indxElem,1)     = SoilM_Array(2)
                pNV%SoilM_Oth(indxElem,1)    = SoilM_Array(3)
                
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
                rMultip                      = Area_NV / DeltaT
                pNV%Runoff(indxElem,1)       = pNV%Runoff(indxElem,1)       * rMultip
                pNV%PrecipInfilt(indxElem,1) = pNV%PrecipInfilt(indxElem,1) * rMultip
                pNV%ETa(indxElem,1)          = pNV%ETa(indxElem,1)          * rMultip
                pNV%Perc(indxElem,1)         = pNV%Perc(indxElem,1)         * rMultip  
                
                !If surface flow goes to groundwater, update the runoff processes
                IF (lElemFlowToGW) THEN
                    pNV%Perc(indxElem,1)         = pNV%Perc(indxElem,1) + pNV%Runoff(indxElem,1)
                    pNV%PrecipInfilt(indxElem,1) = pNV%PrecipInfilt(indxElem,1) + pNV%Runoff(indxElem,1)        !Runoff is assumed to bypass root zone for proper mass balance    
                    pNV%Runoff(indxElem,1)       = 0.0
                END IF
            END IF
              
            !Simulation of riparian veg
            IF (Area_RV .GT. 0.0) THEN
                !Initialize
                TotalPorosityCrop = TotalPorosity * RootDepthRV
                FieldCapacityCrop = FieldCapacity * RootDepthRV
                WiltingPointCrop  = WiltingPoint  * RootDepthRV
                SoilM_P           = pRV%SoilM_Precip_P(indxElem,1) + pRV%SoilM_AW_P(indxElem,1) + pRV%SoilM_Oth_P(indxElem,1)
                GMRV              = GenericMoisture(1,indxElem) * RootDepthRV * DeltaT
                Inflow            = Supply + GMRV
                
                !Simulate
                CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                               pRV%SMax(indxElem,1)                   ,  &
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
                                               pRV%Runoff(indxElem,1)                 ,  & 
                                               pRV%PrecipInfilt(indxElem,1)           ,  & 
                                               pRV%ETa(indxElem,1)                    ,  & 
                                               pRV%Perc(indxElem,1)                   ,  & 
                                               Excess                                 ,  &
                                               AchievedConv                           ) 
                                             
                !Generate error if convergence is not achieved
                IF (AchievedConv .NE. 0.0) THEN
                    !$OMP CRITICAL
                    iElemID         = AppGrid%AppElement(indxElem)%ID
                    MessageArray(1) = 'Convergence error in soil moisture routing for riparian vegetation!'
                    MessageArray(2) = 'Element              = '//TRIM(IntToText(iElemID))
                    WRITE (MessageArray(3),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                    WRITE (MessageArray(4),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                    CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                    iStat = -1
                    !$OMP END CRITICAL
                    CYCLE
                END IF
                
                !Reduce total infiltration based on correction for total porosity
                IF (Excess .NE. 0.0) THEN
                    ratio = [pRV%PrecipInfilt(indxElem,1) , GMRV]
                    CALL NormalizeArray(ratio)
                    pRV%Runoff(indxElem,1)       = pRV%Runoff(indxElem,1) + Excess * ratio(1)
                    pRV%GMExcess(indxElem,1)     = Excess * ratio(2)
                    pRV%PrecipInfilt(indxElem,1) = PrecipD - pRV%Runoff(indxElem,1)
                END IF
                
                !Compute moisture from precip and irrigation
                SoilM_P_Array = [pRV%SoilM_Precip_P(indxElem,1) , pRV%SoilM_AW_P(indxElem,1) , pRV%SoilM_Oth_P(indxElem,1)    ]
                Infilt        = [pRV%PrecipInfilt(indxElem,1)   , 0d0                        , GMRV - pRV%GMExcess(indxElem,1)]
                CALL TrackMoistureDueToSource(SoilM_P_Array        ,  &
                                              Infilt               ,  &
                                              pRV%Perc(indxElem,1) ,  &
                                              pRV%ETa(indxElem,1)  ,  &
                                              0d0                  ,  &
                                              SoilM_Array          ,  &
                                              ETPartition          )
                pRV%SoilM_Precip(indxElem,1) = SoilM_Array(1)
                pRV%SoilM_AW(indxElem,1)     = SoilM_Array(2)
                pRV%SoilM_Oth(indxElem,1)    = SoilM_Array(3)
                
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
                rMultip                      = Area_RV / DeltaT
                pRV%Runoff(indxElem,1)       = pRV%Runoff(indxElem,1)       * rMultip
                pRV%PrecipInfilt(indxElem,1) = pRV%PrecipInfilt(indxElem,1) * rMultip
                pRV%ETa(indxElem,1)          = pRV%ETa(indxElem,1)          * rMultip
                pRV%Perc(indxElem,1)         = pRV%Perc(indxElem,1)         * rMultip  
                
                !If surface flow goes to groundwater, update the runoff processes
                IF (lElemFlowToGW) THEN
                    pRV%Perc(indxElem,1)         = pRV%Perc(indxElem,1) + pRV%Runoff(indxElem,1)
                    pRV%PrecipInfilt(indxElem,1) = pRV%PrecipInfilt(indxElem,1) + pRV%Runoff(indxElem,1)        !Runoff is assumed to bypass root zone for proper mass balance    
                    pRV%Runoff(indxElem,1)       = 0.0
                END IF
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
  SUBROUTINE SoilMContent_To_Depth(NVRVLand,NElements,iElemIDs,TotalPorosity,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    INTEGER,INTENT(IN)                :: NElements,iElemIDs(NElements)
    REAL(8),INTENT(IN)                :: TotalPorosity(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxElem
    REAL(8)                      :: RootDepth_NV,RootDepth_RV
    
    !Initialize
    iStat = 0
    
    !Return if native and riparian lands are not simulated
    IF (.NOT.ALLOCATED(NVRVLand%NativeVeg%SMax)  .AND.  .NOT.ALLOCATED(NVRVLand%RiparianVeg%SMax)) RETURN
    
    !Initialize
    RootDepth_NV = NVRVLand%RootDepth_Native
    RootDepth_RV = NVRVLand%RootDepth_Riparian
    
    !Check if initial conditions are greater than total porosity, if not convert conetnts to depths and equate SoilM_P to SoilM
    ASSOCIATE (pNV => NVRVLand%NativeVeg   , &
               pRV => NVRVLand%RiparianVeg ) 
        DO indxElem=1,NElements
                     
            IF ((pNV%SoilM_Precip(indxElem,1) + pNV%SoilM_AW(indxElem,1) + pNV%SoilM_Oth(indxElem,1)) .GT. TotalPorosity(indxElem)) THEN
                CALL SetLastMessage('Initial moisture content for native vegetation at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            IF ((pRV%SoilM_Precip(indxElem,1) + pRV%SoilM_AW(indxElem,1) + pRV%SoilM_Oth(indxElem,1)) .GT. TotalPorosity(indxElem)) THEN
                CALL SetLastMessage('Initial moisture content for riparian vegetation at element ' // TRIM(IntToText(iElemIDs(indxElem))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            pNV%SoilM_Precip(indxElem,1)   = pNV%SoilM_Precip(indxElem,1) * RootDepth_NV
            pNV%SoilM_AW(indxElem,1)       = pNV%SoilM_AW(indxElem,1)     * RootDepth_NV
            pNV%SoilM_Oth(indxElem,1)      = pNV%SoilM_Oth(indxElem,1)    * RootDepth_NV
            pRV%SoilM_Precip(indxElem,1)   = pRV%SoilM_Precip(indxElem,1) * RootDepth_RV
            pRV%SoilM_AW(indxElem,1)       = pRV%SoilM_AW(indxElem,1)     * RootDepth_RV
            pRV%SoilM_Oth(indxElem,1)      = pRV%SoilM_Oth(indxElem,1)    * RootDepth_RV
            pNV%SoilM_Precip_P(indxElem,1) = pNV%SoilM_Precip(indxElem,1)
            pNV%SoilM_AW_P(indxElem,1)     = pNV%SoilM_AW(indxElem,1)
            pNV%SoilM_Oth_P(indxElem,1)    = pNV%SoilM_Oth(indxElem,1)
            pRV%SoilM_Precip_P(indxElem,1) = pRV%SoilM_Precip(indxElem,1)
            pRV%SoilM_AW_P(indxElem,1)     = pRV%SoilM_AW(indxElem,1)
            pRV%SoilM_Oth_P(indxElem,1)    = pRV%SoilM_Oth(indxElem,1)
        
        END DO 
    END ASSOCIATE
    
  END SUBROUTINE SoilMContent_To_Depth
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(NVRVLand) 
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    
    NVRVLand%NativeVeg%Area_P   = NVRVLand%NativeVeg%Area
    NVRVLand%RiparianVeg%Area_P = NVRVLand%RiparianVeg%Area
    
  END SUBROUTINE AdvanceAreas

  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFIED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(NVRVLand,iElemIDs,rElemAreas,TimeStep,iStat)
    CLASS(NativeRiparianDatabaseType) :: NVRVLand
    INTEGER,INTENT(IN)                :: iElemIDs(:)
    REAL(8),INTENT(IN)                :: rElemAreas(:)
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep 
    INTEGER,INTENT(OUT)               :: iStat
    
    CALL NVRVLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL NVRVLand%LandUseDataFile%ReadTSData('Native and riparian veg. areas',TimeStep,rElemAreas,iElemIDs,iStat)
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp
  
END MODULE