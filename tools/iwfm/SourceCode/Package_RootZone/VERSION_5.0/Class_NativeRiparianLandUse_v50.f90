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
MODULE Class_NativeRiparianLandUse_v50
  USE MessageLogger           , ONLY: SetLastMessage                , &
                                      EchoProgress                  , &
                                      MessageArray                  , &
                                      f_iFatal
  USE IOInterface
  USE TimeSeriesUtilities     , ONLY: TimeStepType
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter       , &
                                      CleanSpecialCharacters        , &
                                      IntToText                     , &
                                      NormalizeArray                , &
                                      EstablishAbsolutePathFileName
  USE Package_Discretization  , ONLY: AppGridType
  USE Util_Package_RootZone   , ONLY: ReadRealData                  , &
                                      ReadPointerData
  USE Class_GenericLandUse    , ONLY: GenericLandUseType
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType  
  USE Package_UnsatZone       , ONLY: RootZoneSoilType              , &
                                      NonPondedLUMoistureRouter
  USE Package_PrecipitationET , ONLY: ETType
  USE Package_Misc            , ONLY: SolverDataType                
  USE Class_BaseRootZone      , ONLY: TrackMoistureDueToSource
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
  PUBLIC :: NativeRiparianDatabase_v50_Type
  
  
  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: NativeRiparianType
  END TYPE NativeRiparianType


  ! -------------------------------------------------------------
  ! --- NATIVE/RIPARIAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE NativeRiparianDatabase_v50_Type
    TYPE(NativeRiparianType)             :: NativeVeg                            !Native veg data for each (soil,subregion) combination
    TYPE(NativeRiparianType)             :: RiparianVeg                          !Riparian veg data for each (soil,subregion) combination
    REAL(8)                              :: RootDepth_Native         = 0.0     
    REAL(8)                              :: RootDepth_Riparian       = 0.0     
    REAL(8),ALLOCATABLE                  :: ElementalArea_NV(:)                  !Area of native vegetation at each (element) at current time step
    REAL(8),ALLOCATABLE                  :: ElementalArea_RV(:)                  !Area of riparian vegetation at each (element) at current time step
    REAL(8),ALLOCATABLE                  :: ElementalArea_P_NV(:)                !Area of native vegetation at each (element) at previous time step
    REAL(8),ALLOCATABLE                  :: ElementalArea_P_RV(:)                !Area of riparian vegetation at each (element) at previous time step
    REAL(8),ALLOCATABLE                  :: SubregionalArea_NV(:)                !Area of native vegetation at each (subregion)
    REAL(8),ALLOCATABLE                  :: SubregionalArea_RV(:)                !Area of riparian vegetation at each (subregion)
    REAL(8),ALLOCATABLE                  :: RegionETPot_NV(:)                    !Regional potential ET for native vegetation
    REAL(8),ALLOCATABLE                  :: RegionETPot_RV(:)                    !Regional potential ET for riparian vegetation
    TYPE(LandUseDataFileType)            :: LandUseDataFile                      !Land use data file
  CONTAINS
    PROCEDURE,PASS :: New
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: ReadTSData 
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: SetAreas              
    PROCEDURE,PASS :: AdvanceAreas          
    PROCEDURE,PASS :: SoilMContent_To_Depth 
    PROCEDURE,PASS :: Simulate  
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp 
  END TYPE NativeRiparianDatabase_v50_Type
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 33
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_NativeRiparianLandUse_v50::'



  
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
  SUBROUTINE New(NVRVLand,cFileName,cWorkingDirectory,FactCN,NSoils,NElements,NSubregions,iSubregionIDs,TrackTime,iStat) 
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    CHARACTER(LEN=*),INTENT(IN)            :: cFileName,cWorkingDirectory
    REAL(8),INTENT(IN)                     :: FACTCN
    INTEGER,INTENT(IN)                     :: NSoils,NElements,NSubregions,iSubregionIDs(NSubregions)
    LOGICAL,INTENT(IN)                     :: TrackTime
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    CHARACTER                   :: ALine*1000
    INTEGER                     :: ErrorCode,indxRegion,iRegion,ID,iStat1
    REAL(8)                     :: FACT
    REAL(8),ALLOCATABLE         :: DummyArray(:,:)
    INTEGER,ALLOCATABLE         :: IntDummyArray(:,:)
    LOGICAL                     :: lProcessed(NSubregions)
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
    CALL NVRVLand%NativeVeg%New(NSoils,NSubregions,iStat1)
    CALL NVRVLand%RiparianVeg%New(NSoils,NSubregions,iStat)
    ALLOCATE (NVRVLand%ElementalArea_NV(NElements)     , &
              NVRVLand%ElementalArea_RV(NElements)     , &
              NVRVLand%ElementalArea_P_NV(NElements)   , &
              NVRVLand%ElementalArea_P_RV(NElements)   , &
              NVRVLand%SubregionalArea_NV(NSubregions) , &
              NVRVLand%SubregionalArea_RV(NSubregions) , &
              NVRVLand%RegionETPot_NV(NSubregions)     , &
              NVRVLand%RegionETPot_RV(NSubregions)     , &
              STAT=ErrorCode                           )
    IF (ErrorCode+iStat1+iStat .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for native/riparian vegetation data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize variables
    NVRVLand%ElementalArea_NV   = 0.0   
    NVRVLand%ElementalArea_RV   = 0.0   
    NVRVLand%ElementalArea_P_NV = 0.0   
    NVRVLand%ElementalArea_P_RV = 0.0   
    NVRVLand%SubregionalArea_NV = 0.0 
    NVRVLand%SubregionalArea_RV = 0.0 
    NVRVLand%RegionETPot_NV     = 0.0 
    NVRVLand%RegionETPot_RV     = 0.0 
        
    ASSOCIATE (pNativeVeg   => NVRVLand%NativeVeg   , &
               pRiparianVeg => NVRVLand%RiparianVeg )
        
        !Land use data file
        CALL NVRVFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
        ALine = StripTextUntilCharacter(ALine,'/') 
        CALL CleanSpecialCharacters(ALine)
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL NVRVLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Native and riparian veg. area file',NElements,2,TrackTime,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Rooting depths
        CALL NVRVFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL NVRVFile%ReadData(NVRVLand%RootDepth_Native,iStat)    ;  IF (iStat .EQ. -1) RETURN  ;  NVRVLand%RootDepth_Native   = NVRVLand%RootDepth_Native * FACT
        CALL NVRVFile%ReadData(NVRVLand%RootDepth_Riparian,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  NVRVLand%RootDepth_Riparian = NVRVLand%RootDepth_Riparian * FACT
 
        !Read CN column pointers
        CALL ReadRealData(NVRVFile,'curve numbers for native and riparian vegetation','subregions',NSubregions,2*NSoils+1,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        lProcessed = .FALSE.
        DO indxRegion=1,NSubregions
            iRegion = INT(DummyArray(indxRegion,1))
            IF (lProcessed(iRegion)) THEN
                ID = iSubregionIDs(iRegion)
                CALL SetLastMessage('curve numbers for native and riparian vegetation at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iRegion)          = .TRUE.
            pNativeVeg%SMax(:,iRegion)   = (1000.0/DummyArray(indxRegion,2:NSoils+1)-10.0) * FACTCN
            pRiparianVeg%SMax(:,iRegion) = (1000.0/DummyArray(indxRegion,NSoils+2:)-10.0) * FACTCN
        END DO
        
        !Read ETc column pointers (although iColETc can be different for different soils, it is not)
        CALL ReadPointerData(NVRVFile,'evapotranspiration column pointers for native and riparian vegetation','subregions',NSubregions,3,iSubregionIDs,IntDummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        lProcessed = .FALSE.
        DO indxRegion=1,NSubregions
            iRegion = IntDummyArray(indxRegion,1)
            IF (lProcessed(iRegion)) THEN
                ID = iSubregionIDs(iRegion)
                CALL SetLastMessage('evapotranspiration column pointers for native and riparian vegetation at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iRegion)             = .TRUE.
            pNativeVeg%iColETc(:,iRegion)   = IntDummyArray(indxRegion,2)
            pRiparianVeg%iColETc(:,iRegion) = IntDummyArray(indxRegion,3)
        END DO

        !Initial conditions
        CALL ReadRealData(NVRVFile,'initial conditions for native and riparian vegetation','subregions',NSubregions,2*NSoils+1,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        IF (MINVAL(DummyArray(:,2:)) .LT. 0.0   .OR.  &
            MAXVAL(DummyArray(:,2:)) .GT. 1.0         ) THEN
          MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
          MessageArray(2) = '0.0 or greater than 1.0 for native and riparian vegetation areas!'
          CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)  
          iStat = -1
          RETURN
        END IF
        lProcessed = .FALSE.
        DO indxRegion=1,NSubregions 
            iRegion = INT(DummyArray(indxRegion,1))
            IF (lProcessed(iRegion)) THEN
                ID = iSubregionIDs(iRegion)
                CALL SetLastMessage('initial conditions for native and riparian vegetation at subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            lProcessed(iRegion)                  = .TRUE.
            pNativeVeg%SoilM_Precip(:,iRegion)   = DummyArray(indxRegion,2::2) 
            pRiparianVeg%SoilM_Precip(:,iRegion) = DummyArray(indxRegion,3::2) 
        END DO
        pNativeVeg%SoilM_AW                      = 0.0
        pRiparianVeg%SoilM_AW                    = 0.0 
        pNativeVeg%SoilM_Precip_P                = NVRVLand%NativeVeg%SoilM_Precip
        pRiparianVeg%SoilM_Precip_P              = NVRVLand%RiparianVeg%SoilM_Precip
        pNativeVeg%SoilM_AW_P                    = NVRVLand%NativeVeg%SoilM_AW
        pRiparianVeg%SoilM_AW_P                  = NVRVLand%RiparianVeg%SoilM_AW
        pNativeVeg%SoilM_Precip_P_BeforeUpdate   = pNativeVeg%SoilM_Precip_P
        pNativeVeg%SoilM_AW_P_BeforeUpdate       = pNativeVeg%SoilM_AW_P
        pRiparianVeg%SoilM_Precip_P_BeforeUpdate = pRiparianVeg%SoilM_Precip_P
        pRiparianVeg%SoilM_AW_P_BeforeUpdate     = pRiparianVeg%SoilM_AW_P
        
    END ASSOCIATE

    !Close file
    CALL NVRVFile%Kill()
    
    !Free memory
    DEALLOCATE (DummyArray , IntDummyArray , STAT=ErrorCode)

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
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand

    !Local variables
    INTEGER                                :: ErrorCode
    TYPE(NativeRiparianDatabase_v50_Type) :: Dummy
    
    !Deallocate arrays
    CALL NVRVLand%NativeVeg%Kill()
    CALL NVRVLand%RiparianVeg%Kill()
    DEALLOCATE (NVRVLand%ElementalArea_NV    , &
                NVRVLand%ElementalArea_RV    , &
                NVRVLand%ElementalArea_P_NV  , &
                NVRVLand%ElementalArea_P_RV  , &
                NVRVLand%SubregionalArea_NV  , &
                NVRVLand%SubregionalArea_RV  , &
                NVRVLand%RegionETPot_NV      , &
                NVRVLand%RegionETPot_RV      , &
                STAT = ErrorCode             )
    
    !Close files
    CALL NVRVLand%LandUseDataFile%Kill()
    
    !Assign default values to components
    SELECT TYPE (NVRVLand)
        TYPE IS (NativeRiparianDatabase_v50_Type)
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
  SUBROUTINE SetAreas(NVRVLand,AppGrid,iSoilType,AreaElem_NV,AreaElem_RV)
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    INTEGER,INTENT(IN)                     :: iSoilType(:)
    REAL(8),INTENT(IN)                     :: AreaElem_NV(:),AreaElem_RV(:)
    
    !Local variables
    INTEGER :: indxElem,iSoil,iRegion
    
    !Initialize
    NVRVLand%NativeVeg%Area   = 0.0
    NVRVLand%RiparianVeg%Area = 0.0
    
    !Set elemental areas
    NVRVLand%ElementalArea_NV = AreaElem_NV
    NVRVLand%ElementalArea_RV = AreaElem_RV
   
    !Calculate (soil,region) areas
    DO indxElem=1,AppGrid%NElements
        iRegion                                  = AppGrid%AppElement(indxElem)%Subregion
        iSoil                                    = iSoilType(indxElem)
        NVRVLand%NativeVeg%Area(iSoil,iRegion)   = NVRVLand%NativeVeg%Area(iSoil,iRegion)   + AreaElem_NV(indxElem)
        NVRVLand%RiparianVeg%Area(iSoil,iRegion) = NVRVLand%RiparianVeg%Area(iSoil,iRegion) + AreaElem_RV(indxElem)
    END DO
    
    !Subregional areas
    NVRVLand%SubregionalArea_NV = SUM(NVRVLand%NativeVeg%Area , DIM=1) 
    NVRVLand%SubregionalArea_RV = SUM(NVRVLand%RiparianVeg%Area , DIM=1)
    
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
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    TYPE(GenericFileType)                  :: InFile
    INTEGER,INTENT(OUT)                    :: iStat
    
    CALL InFile%ReadData(NVRVLand%NativeVeg%Runoff,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%Area,iStat)              ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%Area_P,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Precip_P,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Precip,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_AW_P,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_AW,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Oth_P,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%NativeVeg%SoilM_Oth,iStat)         ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%ElementalArea_NV,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%ElementalArea_P_NV,iStat)          ;  IF (iStat .EQ. -1) RETURN
    
    CALL InFile%ReadData(NVRVLand%RiparianVeg%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%RiparianVeg%SoilM_Oth,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%ElementalArea_RV,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(NVRVLand%ElementalArea_P_RV,iStat)          

  END SUBROUTINE ReadRestartData

    
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR NATIVE AND RIPARIAN VEG
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(NVRVLand,ElemSoilTypes,iSubregionIDs,rRegionAreas,lLakeElem,TimeStep,AppGrid,iStat)
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    INTEGER,INTENT(IN)                     :: ElemSoilTypes(:),iSubregionIDs(:)
    REAL(8),INTENT(IN)                     :: rRegionAreas(:)
    LOGICAL,INTENT(IN)                     :: lLakeElem(:)
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    INTEGER :: indxElem,iSoil,iRegion
    
    !Initialize
    iStat = 0
    
    !Echo progress
    CALL EchoProgress('Reading time series data for native and riparian vegitation lands')
    
    !Land use areas
    CALL NVRVLand%LandUseDataFile%ReadTSData('Native and riparian veg. areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (NVRVLand%LandUseDataFile%lUpdated) THEN
        ASSOCIATE (pNVArea      => NVRVLand%NativeVeg%Area          , &
                   pRVArea      => NVRVLand%RiparianVeg%Area        , &
                   pElemArea_NV => NVRVLand%ElementalArea_NV        , &
                   pElemArea_RV => NVRVLand%ElementalArea_RV        , &
                   pReadArea    => NVRVLand%LandUseDataFile%rValues )
            pNVArea = 0.0
            pRVArea = 0.0
            DO indxElem=1,AppGrid%NElements
                IF (lLakeElem(indxElem)) CYCLE
                iSoil                  = ElemSoilTypes(indxElem)
                iRegion                = AppGrid%AppElement(indxElem)%Subregion
                pElemArea_NV(indxElem) = pReadArea(indxElem,2)
                pElemArea_RV(indxElem) = pReadArea(indxElem,3)
                pNVArea(iSoil,iRegion) = pNVArea(iSoil,iRegion) + pElemArea_NV(indxElem)
                pRVArea(iSoil,iRegion) = pRVArea(iSoil,iRegion) + pElemArea_RV(indxElem)
            END DO
            
            !Update subregional total areas
            NVRVLand%SubregionalArea_NV = SUM(pNVArea , DIM=1)
            NVRVLand%SubregionalArea_RV = SUM(pRVArea , DIM=1)
            
        END ASSOCIATE
    END IF
    
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
  SUBROUTINE PrintRestartData(NVRVLand,OutFile)
    CLASS(NativeRiparianDatabase_v50_Type),INTENT(IN) :: NVRVLand
    TYPE(GenericFileType)                             :: OutFile
    
    CALL OutFile%WriteData(NVRVLand%NativeVeg%Runoff)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%Area)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%Area_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Precip_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Precip)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_AW_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_AW)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Oth_P)
    CALL OutFile%WriteData(NVRVLand%NativeVeg%SoilM_Oth)
    CALL OutFile%WriteData(NVRVLand%ElementalArea_NV)
    CALL OutFile%WriteData(NVRVLand%ElementalArea_P_NV)
    
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%Runoff)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%Area)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%Area_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Precip_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Precip)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_AW_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_AW)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Oth_P)
    CALL OutFile%WriteData(NVRVLand%RiparianVeg%SoilM_Oth)
    CALL OutFile%WriteData(NVRVLand%ElementalArea_RV)
    CALL OutFile%WriteData(NVRVLand%ElementalArea_P_RV)

  END SUBROUTINE PrintRestartData

    

  
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
  ! --- SIMULATE FLOW PROCESSES 
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(NVRVLand,ETData,iSubregionIDs,DeltaT,Precip,GenericMoisture,SubregionSoilsData,UpstrmFlow,SolverData,iStat)
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    TYPE(ETType),INTENT(IN)                :: ETData
    INTEGER,INTENT(IN)                     :: iSubregionIDs(:)
    REAL(8),INTENT(IN)                     :: DeltaT,Precip(:,:),GenericMoisture(:,:),UpstrmFlow(:)
    TYPE(RootZoneSoilType),INTENT(IN)      :: SubregionSoilsData(:,:)
    TYPE(SolverDataType),INTENT(IN)        :: SolverData
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    INTEGER                     :: indxRegion,indxSoil,iColETc(2),KunsatMethod
    REAL(8)                     :: AchievedConv,ETc(2),HydCond,TotalPorosity,Area_NV,Area_RV,  &
                                   FieldCapacity,TotalPorosityCrop,FieldCapacityCrop,Lambda,   &
                                   RootDepthNV,RootDepthRV,Supply,WiltingPoint,GMRV,Excess,    &
                                   WiltingPointCrop,SoilM,SoilM_P,GMNV,PrecipD,Inflow,ratio(2),&
                                   SoilM_P_Array(3),SoilM_Array(3),ETPartition(3),Infilt(3)
    LOGICAL                     :: lNegativeMoist
    
    !Initialize
    iStat = 0
  
    !Inform user
    CALL EchoProgress('Simulating flows at native and riparian vegetation lands')
    
    ASSOCIATE (pNV => NVRVLand%NativeVeg      , &
               pRV => NVRVLand%RiparianVeg    )

      !Initialize
      RootDepthNV      = NVRVLand%RootDepth_Native 
      RootDepthRV      = NVRVLand%RootDepth_Riparian
    
      DO indxRegion=1,SIZE(iSubregionIDs)
          iColETc(1) = pNV%iColETc(1,indxRegion)    !ETc column pointers are the same for all soils in a subregion
          iColETc(2) = pRV%iColETc(1,indxRegion)    !ETc column pointers are the same for all soils in a subregion
          ETc        = ETData%GetValues(iColETc)    !ETc is the same for all soils in a subregion
          DO indxSoil=1,SIZE(pNV%SMax , DIM=1)
              !Cycle if native and riparian areas are both zero
              Area_NV = pNV%Area(indxSoil,indxRegion)
              Area_RV = pRV%Area(indxSoil,indxRegion)
              IF (Area_NV .EQ. 0.0) THEN
                  pNV%Runoff(indxSoil,indxRegion)       = 0.0  
                  pNV%PrecipInfilt(indxSoil,indxRegion) = 0.0  
                  pNV%ETa(indxSoil,indxRegion)          = 0.0  
                  pNV%Perc(indxSoil,indxRegion)         = 0.0  
                  pNV%GMExcess(indxSoil,indxRegion)     = 0.0  
              END IF
              IF (Area_RV .EQ. 0.0) THEN
                  pRV%Runoff(indxSoil,indxRegion)       = 0.0
                  pRV%PrecipInfilt(indxSoil,indxRegion) = 0.0
                  pRV%ETa(indxSoil,indxRegion)          = 0.0
                  pRV%Perc(indxSoil,indxRegion)         = 0.0
                  pRV%GMExcess(indxSoil,indxRegion)     = 0.0
              END IF
              IF (Area_NV .EQ. 0.0   .AND.   Area_RV .EQ. 0.0) CYCLE
              
              WiltingPoint  = SubregionSoilsData(indxSoil,indxRegion)%WiltingPoint
              FieldCapacity = SubregionSoilsData(indxSoil,indxRegion)%FieldCapacity
              TotalPorosity = SubregionSoilsData(indxSoil,indxRegion)%TotalPorosity
              HydCond       = SubregionSoilsData(indxSoil,indxRegion)%HydCond
              Lambda        = SubregionSoilsData(indxSoil,indxRegion)%Lambda
              KunsatMethod  = SubregionSoilsData(indxSoil,indxRegion)%KunsatMethod
              
              !Water supply as runoff from upstream regions plus precip 
              Supply = UpstrmFlow(indxRegion) * DeltaT 
              
              !Precipitation
              PrecipD = Precip(indxSoil,indxRegion) * DeltaT
              
              !Simulation of native veg
              IF (Area_NV .GT. 0.0) THEN
                  !Initialize
                  TotalPorosityCrop = TotalPorosity * RootDepthNV
                  FieldCapacityCrop = FieldCapacity * RootDepthNV
                  WiltingPointCrop  = WiltingPoint  * RootDepthNV
                  SoilM_P           = pNV%SoilM_Precip_P(indxSoil,indxRegion) + pNV%SoilM_AW_P(indxSoil,indxRegion) + pNV%SoilM_Oth_P(indxSoil,indxRegion)
                  GMNV              = GenericMoisture(indxSoil,indxRegion) * RootDepthNV * DeltaT
                  Inflow            = Supply + GMNV
                  
                  !Simulate
                  CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                                 pNV%SMax(indxSoil,indxRegion)          ,  &
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
                                                 pNV%Runoff(indxSoil,indxRegion)        ,  & 
                                                 pNV%PrecipInfilt(indxSoil,indxRegion)  ,  & 
                                                 pNV%ETa(indxSoil,indxRegion)           ,  & 
                                                 pNV%Perc(indxSoil,indxRegion)          ,  & 
                                                 Excess                                 ,  &
                                                 AchievedConv                           ) 
                                               
                  !Generate error if convergence is not achieved
                  IF (AchievedConv .NE. 0.0) THEN
                      MessageArray(1) = 'Convergence error in soil moisture routing for native vegetation!'
                      MessageArray(2) =                   'Soil type            = '//TRIM(IntToText(indxSoil))
                      MessageArray(3) =                   'Subregion            = '//TRIM(IntToText(iSubregionIDs(indxRegion)))
                      WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                      WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                      CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                      iStat = -1
                      RETURN
                  END IF
                  
                  !Reduce total infiltration based on correction for total porosity
                  IF (Excess .NE. 0.0) THEN
                      ratio = [pNV%PrecipInfilt(indxSoil,indxRegion) , GMNV]
                      CALL NormalizeArray(ratio)
                      pNV%Runoff(indxSoil,indxRegion)       = pNV%Runoff(indxSoil,indxRegion) + Excess * ratio(1)
                      pNV%GMExcess(indxSoil,indxRegion)     = Excess * ratio(2)
                      pNV%PrecipInfilt(indxSoil,indxRegion) = PrecipD - pNV%Runoff(indxSoil,indxRegion)
                  END IF
                  
                  !Compute moisture from precip and irrigation
                  SoilM_P_Array = [pNV%SoilM_Precip_P(indxSoil,indxRegion) , pNV%SoilM_AW_P(indxSoil,indxRegion) , pNV%SoilM_Oth_P(indxSoil,indxRegion)    ]
                  Infilt        = [pNV%PrecipInfilt(indxSoil,indxRegion)   , 0d0                                 , GMNV - pNV%GMExcess(indxSoil,indxRegion)]
                  CALL TrackMoistureDueToSource(SoilM_P_Array                 ,  &
                                                Infilt                        ,  &
                                                pNV%Perc(indxSoil,indxRegion) ,  &
                                                pNV%ETa(indxSoil,indxRegion)  ,  &
                                                0d0                           ,  &
                                                SoilM_Array                   ,  &
                                                ETPartition                   )
                  pNV%SoilM_Precip(indxSoil,indxRegion) = SoilM_Array(1)
                  pNV%SoilM_AW(indxSoil,indxRegion)     = SoilM_Array(2)
                  pNV%SoilM_Oth(indxSoil,indxRegion)    = SoilM_Array(3)
                  
                  !Make sure soil moisture is not less than zero
                  lNegativeMoist = .FALSE.
                  IF (pNV%SoilM_Precip(indxSoil,indxRegion) .LT. 0.0) lNegativeMoist = .TRUE.
                  IF (pNV%SoilM_AW(indxSoil,indxRegion)     .LT. 0.0) lNegativeMoist = .TRUE.
                  IF (pNV%SoilM_Oth(indxSoil,indxRegion)    .LT. 0.0) lNegativeMoist = .TRUE.
                  IF (lNegativeMoist) THEN
                      MessageArray(1) = 'Soil moisture content becomes negative at subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//'.'
                      MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
                      MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
                      MessageArray(4) = 'in the Root Zone Main Input File.'
                      CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                      iStat = -1
                      RETURN
                  END IF
              END IF
              
              !Simulation of riparian veg
              IF (Area_RV .GT. 0.0) THEN
                  !Initialize
                  TotalPorosityCrop = TotalPorosity * RootDepthRV
                  FieldCapacityCrop = FieldCapacity * RootDepthRV
                  WiltingPointCrop  = WiltingPoint  * RootDepthRV
                  SoilM_P           = pRV%SoilM_Precip_P(indxSoil,indxRegion) + pRV%SoilM_AW_P(indxSoil,indxRegion) + pRV%SoilM_Oth_P(indxSoil,indxRegion)
                  GMRV              = GenericMoisture(indxSoil,indxRegion) * RootDepthRV * DeltaT
                  Inflow            = Supply + GMRV
                  
                  !Simulate
                  CALL NonPondedLUMoistureRouter(PrecipD                                ,  &
                                                 pRV%SMax(indxSoil,indxRegion)          ,  &
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
                                                 pRV%Runoff(indxSoil,indxRegion)        ,  & 
                                                 pRV%PrecipInfilt(indxSoil,indxRegion)  ,  & 
                                                 pRV%ETa(indxSoil,indxRegion)           ,  & 
                                                 pRV%Perc(indxSoil,indxRegion)          ,  & 
                                                 Excess                                 ,  &
                                                 AchievedConv                           ) 
                                               
                  !Generate error if convergence is not achieved
                  IF (AchievedConv .NE. 0.0) THEN
                      MessageArray(1) = 'Convergence error in soil moisture routing for riparian vegetation!'
                      MessageArray(2) =                   'Soil type            = '//TRIM(IntToText(indxSoil))
                      MessageArray(3) =                   'Subregion            = '//TRIM(IntToText(iSubregionIDs(indxRegion)))
                      WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityCrop
                      WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                      CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                      iStat = -1
                      RETURN
                  END IF
                  
                  !Reduce total infiltration based on correction for total porosity
                  IF (Excess .NE. 0.0) THEN
                      ratio = [pRV%PrecipInfilt(indxSoil,indxRegion) , GMRV]
                      CALL NormalizeArray(ratio)
                      pRV%Runoff(indxSoil,indxRegion)       = pRV%Runoff(indxSoil,indxRegion) + Excess * ratio(1)
                      pRV%GMExcess(indxSoil,indxRegion)     = Excess * ratio(2)
                      pRV%PrecipInfilt(indxSoil,indxRegion) = PrecipD - pRV%Runoff(indxSoil,indxRegion)
                  END IF
                  
                  !Compute moisture from precip and irrigation
                  SoilM_P_Array = [pRV%SoilM_Precip_P(indxSoil,indxRegion) , pRV%SoilM_AW_P(indxSoil,indxRegion) , pRV%SoilM_Oth_P(indxSoil,indxRegion)    ]
                  Infilt        = [pRV%PrecipInfilt(indxSoil,indxRegion)   , 0d0                                 , GMRV - pRV%GMExcess(indxSoil,indxRegion)]
                  CALL TrackMoistureDueToSource(SoilM_P_Array                 ,  &
                                                Infilt                        ,  &
                                                pRV%Perc(indxSoil,indxRegion) ,  &
                                                pRV%ETa(indxSoil,indxRegion)  ,  &
                                                0d0                           ,  &
                                                SoilM_Array                   ,  &
                                                ETPartition                   )
                  pRV%SoilM_Precip(indxSoil,indxRegion) = SoilM_Array(1)
                  pRV%SoilM_AW(indxSoil,indxRegion)     = SoilM_Array(2)
                  pRV%SoilM_Oth(indxSoil,indxRegion)    = SoilM_Array(3)
                  
                  !Make sure soil moisture is not less than zero
                  lNegativeMoist = .FALSE.
                  IF (pRV%SoilM_Precip(indxSoil,indxRegion) .LT. 0.0) lNegativeMoist = .TRUE.
                  IF (pRV%SoilM_AW(indxSoil,indxRegion)     .LT. 0.0) lNegativeMoist = .TRUE.
                  IF (pRV%SoilM_Oth(indxSoil,indxRegion)    .LT. 0.0) lNegativeMoist = .TRUE.
                  IF (lNegativeMoist) THEN
                      MessageArray(1) = 'Soil moisture content becomes negative at subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//'.'
                      MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
                      MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
                      MessageArray(4) = 'in the Root Zone Main Input File.'
                      CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                      iStat = -1
                      RETURN
                  END IF
              END IF
          END DO
      END DO
    END ASSOCIATE  
                                   
  END SUBROUTINE Simulate

  
  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(NVRVLand) 
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    
    NVRVLand%ElementalArea_P_NV = NVRVLand%ElementalArea_NV
    NVRVLand%ElementalArea_P_RV = NVRVLand%ElementalArea_RV
    NVRVLand%NativeVeg%Area_P   = NVRVLand%NativeVeg%Area
    NVRVLand%RiparianVeg%Area_P = NVRVLand%RiparianVeg%Area
    
  END SUBROUTINE AdvanceAreas

  
  ! -------------------------------------------------------------
  ! --- CONVERT SOIL INITIAL MOISTURE CONTENTS TO DEPTHS
  ! ---  Note: Called only once at the beginning of simulation
  ! -------------------------------------------------------------
  SUBROUTINE SoilMContent_To_Depth(NVRVLand,NSoils,NRegions,iSubregionIDs,TotalPorosity,iStat)
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    INTEGER,INTENT(IN)                     :: NSoils,NRegions,iSubregionIDs(NRegions)
    REAL(8),INTENT(IN)                     :: TotalPorosity(NSoils,NRegions)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxRegion,indxSoil
    REAL(8)                      :: RootDepth_NV,RootDepth_RV
    
    !Initialize
    iStat = 0
    
    !Return if native and riparian lands are not simulated
    IF (.NOT.ALLOCATED(NVRVLand%NativeVeg%SMax)  .AND.  .NOT.ALLOCATED(NVRVLand%RiparianVeg%SMax)) RETURN
    
    !Initialize
    RootDepth_NV = NVRVLand%RootDepth_Native
    RootDepth_RV = NVRVLand%RootDepth_Riparian
    
    !Check if initial conditions are greater than total porosity, if not convert contents to depths and equate SoilM_P to SoilM
    ASSOCIATE (pNV => NVRVLand%NativeVeg   , &
               pRV => NVRVLand%RiparianVeg )
        DO indxRegion=1,NRegions
            DO indxSoil=1,NSoils
                IF ((pNV%SoilM_Precip(indxSoil,indxRegion) + pNV%SoilM_AW(indxSoil,indxRegion) + pNV%SoilM_Oth(indxSoil,indxRegion)) .GT. TotalPorosity(indxSoil,indxRegion)) THEN
                    CALL SetLastMessage('Initial moisture content for native vegetation with soil type ' // TRIM(IntToText(indxSoil)) // ' at subregion ' // TRIM(IntToText(iSubregionIDs(indxRegion))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                IF ((pRV%SoilM_Precip(indxSoil,indxRegion) + pRV%SoilM_AW(indxSoil,indxRegion) + pRV%SoilM_Oth(indxSoil,indxRegion)) .GT. TotalPorosity(indxSoil,indxRegion)) THEN
                    CALL SetLastMessage('Initial moisture content for riparian vegetation with soil type ' // TRIM(IntToText(indxSoil)) // ' at subregion ' // TRIM(IntToText(iSubregionIDs(indxRegion))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                pNV%SoilM_Precip(indxSoil,indxRegion)   = pNV%SoilM_Precip(indxSoil,indxRegion) * RootDepth_NV
                pNV%SoilM_AW(indxSoil,indxRegion)       = pNV%SoilM_AW(indxSoil,indxRegion)     * RootDepth_NV
                pNV%SoilM_Oth(indxSoil,indxRegion)      = pNV%SoilM_Oth(indxSoil,indxRegion)    * RootDepth_NV
                pRV%SoilM_Precip(indxSoil,indxRegion)   = pRV%SoilM_Precip(indxSoil,indxRegion) * RootDepth_RV
                pRV%SoilM_AW(indxSoil,indxRegion)       = pRV%SoilM_AW(indxSoil,indxRegion)     * RootDepth_RV
                pRV%SoilM_Oth(indxSoil,indxRegion)      = pRV%SoilM_Oth(indxSoil,indxRegion)    * RootDepth_RV
                pNV%SoilM_Precip_P(indxSoil,indxRegion) = pNV%SoilM_Precip(indxSoil,indxRegion)
                pNV%SoilM_AW_P(indxSoil,indxRegion)     = pNV%SoilM_AW(indxSoil,indxRegion)
                pNV%SoilM_Oth_P(indxSoil,indxRegion)    = pNV%SoilM_Oth(indxSoil,indxRegion)
                pRV%SoilM_Precip_P(indxSoil,indxRegion) = pRV%SoilM_Precip(indxSoil,indxRegion)
                pRV%SoilM_AW_P(indxSoil,indxRegion)     = pRV%SoilM_AW(indxSoil,indxRegion)
                pRV%SoilM_Oth_P(indxSoil,indxRegion)    = pRV%SoilM_Oth(indxSoil,indxRegion)   
            END DO
        END DO 
    END ASSOCIATE
    
  END SUBROUTINE SoilMContent_To_Depth

  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFIED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(NVRVLand,iSubregionIDs,rRegionAreas,TimeStep,iStat)
    CLASS(NativeRiparianDatabase_v50_Type) :: NVRVLand
    INTEGER,INTENT(IN)                     :: iSubregionIDs(:)
    REAL(8),INTENT(IN)                     :: rRegionAreas(:)
    TYPE(TimeStepType),INTENT(IN)          :: TimeStep 
    INTEGER,INTENT(OUT)                    :: iStat
    
    CALL NVRVLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)  ;  IF (iStat .NE. 0) RETURN
    CALL NVRVLand%LandUseDataFile%ReadTSData('Native and riparian veg. areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp
  
END MODULE