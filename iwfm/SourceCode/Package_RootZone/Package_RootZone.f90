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
MODULE Package_RootZone
  USE Class_Version              , ONLY : VersionType                        ,  &
                                          ReadVersion                        
  USE MessageLogger              , ONLY : SetLastMessage                     ,  & 
                                          EchoProgress                       ,  &
                                          MessageArray                       ,  &
                                          iFatal                             
  USE TimeSeriesUtilities        , ONLY : TimeStepType                       
  USE GeneralUtilities           , ONLY : FirstLocation                      ,  &
                                          CleanSpecialCharacters             ,  &
                                          NormalizeArray
  USE IOInterface                , ONLY : GenericFileType                    
  USE Package_Misc               , ONLY : FlowDest_Element                   ,  &
                                          FlowDest_Subregion                 ,  &
                                          iSupply_Diversion                  ,  &
                                          iSupply_ElemPump                   ,  &
                                          iSupply_Well
  USE Package_Discretization     , ONLY : AppGridType
  USE Class_BaseRootZone         , ONLY : BaseRootZoneType                   , &
                                          ElementLU_InterpolateExtrapolate   , &
                                          iMeasuredLUDataForSubregion        , &
                                          iMeasuredLUDataForModelDomain 
  USE RootZone_v40               , ONLY : RootZone_v40_Type
  USE RootZone_v401              , ONLY : RootZone_v401_Type
  USE RootZone_v41               , ONLY : RootZone_v41_Type
  USE RootZone_v411              , ONLY : RootZone_v411_Type
  USE RootZone_v50               , ONLY : RootZone_v50_Type 
  USE Package_PrecipitationET    , ONLY : PrecipitationType                  ,  &
                                          ETType
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
  PUBLIC :: RootZoneType                       , &
            ElementLU_InterpolateExtrapolate   , &
            iMeasuredLUDataForSubregion        , &
            iMeasuredLUDataForModelDomain 
  
  
  ! -------------------------------------------------------------
  ! --- ROOT ZONE FACADE DATA TYPE
  ! -------------------------------------------------------------
  TYPE RootZoneType
      PRIVATE
      INTEGER                             :: iVersion   =  0
      CLASS(BaseRootZoneType),ALLOCATABLE :: Me
  CONTAINS
      PROCEDURE,PASS   :: New
      PROCEDURE,PASS   :: Kill
      PROCEDURE,PASS   :: GetNDataList_AtLocationType
      PROCEDURE,PASS   :: GetDataList_AtLocationType
      PROCEDURE,PASS   :: GetLocationsWithData
      PROCEDURE,PASS   :: GetSubDataList_AtLocation
      PROCEDURE,PASS   :: GetModelData_AtLocation
      PROCEDURE,PASS   :: GetNAgCrops
      PROCEDURE,PASS   :: GetNDemandLocations
      PROCEDURE,PASS   :: GetDemandCalcLocation
      PROCEDURE,PASS   :: GetElementPrecipInfilt
      PROCEDURE,PASS   :: GetElementActualET
      PROCEDURE,PASS   :: GetWaterDemand_Ag                     
      PROCEDURE,PASS   :: GetWaterDemand_Urb                    
      PROCEDURE,PASS   :: GetWaterSupply_Ag                      
      PROCEDURE,PASS   :: GetWaterSupply_Urb
      PROCEDURE,PASS   :: GetElementAgAreas                            
      PROCEDURE,PASS   :: GetElementUrbanAreas 
      PROCEDURE,PASS   :: GetElementNativeVegAreas 
      PROCEDURE,PASS   :: GetElementRiparianVegAreas 
      PROCEDURE,PASS   :: GetSubregionAgAreas                            
      PROCEDURE,PASS   :: GetSubregionUrbanAreas 
      PROCEDURE,PASS   :: GetSubregionNativeVegAreas 
      PROCEDURE,PASS   :: GetSubregionRiparianVegAreas 
      PROCEDURE,PASS   :: GetDemandAgAreas                            
      PROCEDURE,PASS   :: GetDemandUrbanAreas 
      PROCEDURE,PASS   :: GetElementSoilMVolume
      PROCEDURE,PASS   :: GetRatio_DestinationSupplyToRegionSupply_Ag  
      PROCEDURE,PASS   :: GetRatio_DestinationSupplyToRegionSupply_Urb 
      PROCEDURE,PASS   :: GetSurfaceFlowDestinations            
      PROCEDURE,PASS   :: GetSurfaceFlowDestinationTypes        
      PROCEDURE,PASS   :: GetPercAll                        
      PROCEDURE,PASS   :: GetPercElement                    
      PROCEDURE,PASS   :: GetFlowsToStreams                     
      PROCEDURE,PASS   :: GetFlowsToLakes                       
      PROCEDURE,PASS   :: GetActiveVersion                      
      PROCEDURE,PASS   :: GetElemGWInflows                      
      PROCEDURE,PASS   :: GetActualRiparianET_AtStrmNodes       
      PROCEDURE,PASS   :: GetRegionalPerc 
      PROCEDURE,PASS   :: GetRegionalReturnFlow_Ag 
      PROCEDURE,PASS   :: GetRegionalReturnFlow_Urb 
      PROCEDURE,NOPASS :: GetVersion 
      PROCEDURE,PASS   :: SetSupply
      PROCEDURE,PASS   :: SetLakeElemFlag                       
      PROCEDURE,PASS   :: SetActualRiparianET_AtStrmNodes       
      PROCEDURE,PASS   :: IsDefined
      PROCEDURE,PASS   :: IsLandUseUpdated
      PROCEDURE,PASS   :: ComputeWaterDemand
      PROCEDURE,PASS   :: Simulate 
      PROCEDURE,PASS   :: ConvertTimeUnit   
      PROCEDURE,PASS   :: ReadTSData                            
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: AdvanceState                          
      PROCEDURE,PASS   :: ZeroSupply                            
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: ComputeGWInflow
  END TYPE RootZoneType
  

  ! -------------------------------------------------------------
  ! --- ROOT ZONE FACADE VERSION RELATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion ='4.0.0000'
  INCLUDE 'Package_RootZone_Revision.fi'
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 18
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_RootZone::'

  
  
  
CONTAINS  

  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW ROOT ZONE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
    CLASS(RootZoneType),INTENT(OUT)    :: RootZone
    LOGICAL,INTENT(IN)                 :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    INTEGER,INTENT(IN)                 :: NStrmNodes,NLakes,NTIME
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)          :: ThisProcedure = ModName // 'New'
    TYPE(GenericFileType)                :: RootZoneParamFile
    CHARACTER(:),ALLOCATABLE             :: cVersion
    
    !Initialize
    iStat = 0
    
    !Return if no filename is defined
    IF (cFileName .EQ. '') RETURN
    
    !Open root zone file and retrieve version number
    CALL RootZoneParamFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL ReadVersion(RootZoneParamFile,'ROOT ZONE',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Close file to reset it
    CALL RootZoneParamFile%Kill()
    
    !If instantaited, kill RootZone object
    IF (ALLOCATED(RootZone%Me)) CALL RootZone%Kill()
    
    !Instantiate root zone data based on version
    SELECT CASE (TRIM(cVersion))
        CASE ('5.0')
            ALLOCATE(RootZone_v50_Type :: RootZone%Me)
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
            IF (iStat .EQ. -1) RETURN
            RootZone%iVersion = 50
        CASE ('4.0')
            ALLOCATE(RootZone_v40_Type :: RootZone%Me)
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
            IF (iStat .EQ. -1) RETURN
            RootZone%iVersion = 40
        CASE ('4.01')
            ALLOCATE(RootZone_v401_Type :: RootZone%Me)
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
            IF (iStat .EQ. -1) RETURN
            RootZone%iVersion = 401
        CASE ('4.1')
            ALLOCATE(RootZone_v41_Type :: RootZone%Me)
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
            IF (iStat .EQ. -1) RETURN
            RootZone%iVersion = 41
        CASE ('4.11')
            ALLOCATE(RootZone_v411_Type :: RootZone%Me)
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,NStrmNodes,NLakes,TimeStep,NTIME,ET,Precip,iStat)
            IF (iStat .EQ. -1) RETURN
            RootZone%iVersion = 411
        CASE DEFAULT
            CALL SetLastMessage('Root Zone Component version number is not recognized ('//TRIM(cVersion)//')!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
    
  END SUBROUTINE New
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL ROOT ZONE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(RootZone)
    CLASS(RootZoneType) :: RootZone
    
    !Local variables
    TYPE(RootZoneType) :: Dummy
    
    IF (RootZone%iVersion .EQ. 0) RETURN
    
    CALL RootZone%Me%Kill()
    DEALLOCATE (RootZone%Me)
    RootZone%iVersion =  Dummy%iVersion
    
  END SUBROUTINE Kill 
  
  
  
        
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF THE ROOT ZONE IS DEFINED
  ! -------------------------------------------------------------
  FUNCTION IsDefined(RootZone) RESULT(lDefined)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    LOGICAL                        :: lDefined
    
    IF (RootZone%iVersion .EQ. 0) THEN
        lDefined = .FALSE.
    ELSE
        lDefined = .TRUE.
    END IF
    
  END FUNCTION IsDefined
  
  
  ! -------------------------------------------------------------
  ! --- IS LAND USE UPDATED
  ! -------------------------------------------------------------
  FUNCTION IsLandUseUpdated(RootZone) RESULT(lUpdated)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    LOGICAL                        :: lUpdated
    
    IF (RootZone%iVersion .EQ. 0) THEN
        lUpdated = .FALSE.
    ELSE
        lUpdated = RootZone%Me%IsLandUseUpdated()
    END IF
      
  END FUNCTION IsLandUseUpdated
  
  

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
  ! --- GET NUMBER OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(RootZone,iLocationType) RESULT(NData)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: iLocationType
    INTEGER                        :: NData
    
    IF (ALLOCATED(RootZone%Me)) THEN
        NData = RootZone%Me%GetNDataList_AtLocationType(iLocationType)
    ELSE
        NData = 0
    END IF
    
  END FUNCTION GetNDataList_AtLocationType

  
  ! -------------------------------------------------------------
  ! --- GET THE LIST OF DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType(RootZone,iLocationType,cDataList,cFileList,lBudgetType)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: iLocationType
    CHARACTER(LEN=*),ALLOCATABLE   :: cDataList(:),cFileList(:)
    LOGICAL,ALLOCATABLE            :: lBudgetType(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetDataList_AtLocationType(iLocationType,cDataList,cFileList,lBudgetType)
    ELSE
        DEALLOCATE (cDataList ,STAT=ErrorCode)
        DEALLOCATE (cFileList ,STAT=ErrorCode)
        DEALLOCATE (lBudgetType ,STAT=ErrorCode)
    END IF
    
  END SUBROUTINE GetDataList_AtLocationType

  
  ! -------------------------------------------------------------
  ! --- GET LOCATIONS THAT HAS A DATA TYPE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetLocationsWithData(RootZone,iLocationType,cDataType,iLocations)
    CLASS(RootZoneType),INTENT(IN)     :: RootZone
    INTEGER,INTENT(IN)                 :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)        :: cDataType
    INTEGER,ALLOCATABLE,INTENT(OUT)    :: iLocations(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetLocationsWithData(iLocationType,cDataType,iLocations)
    ELSE
        DEALLOCATE (iLocations ,STAT=ErrorCode)
    END IF
    
  END SUBROUTINE GetLocationsWithData

  
  ! -------------------------------------------------------------
  ! --- GET THE LIST OF SUB-DATA TYPES FOR POST-PROCESSING AT A LOCATION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetSubDataList_AtLocation(RootZone,iLocationType,cDataType,cSubDataList)
    CLASS(RootZoneType),INTENT(IN)           :: RootZone
    INTEGER,INTENT(IN)                       :: iLocationType
    CHARACTER(LEN=*),INTENT(IN)              :: cDataType
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cSubDataList(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetSubDataList_AtLocation(iLocationType,cDataType,cSubDataList)
    ELSE
        DEALLOCATE (cSubDataList , STAT=ErrorCode)
    END IF
    
  END SUBROUTINE GetSubDataList_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET MODEL DATA AT A LOCATION FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation(RootZone,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(RootZoneType)         :: RootZone
    INTEGER,INTENT(IN)          :: iZExtent,iElems(:),iLayers(:),iZones(:),iZonesWithNames(:),iLocationType,iLocationID,iCol
    CHARACTER(LEN=*),INTENT(IN) :: cZoneNames(:),cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)          :: rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)         :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetModelData_AtLocation(iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iLocationType,iLocationID,cDataType,iCol,cOutputBeginDateAndTime,cOutputEndDateAndTime,cOutputInterval,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE GetModelData_AtLocation
  
    
  ! -------------------------------------------------------------
  ! --- GET WHERE THE DEMAND IS CALCULATED (ELEMENT OR SUBREGION)
  ! -------------------------------------------------------------
  PURE FUNCTION GetDemandCalcLocation(RootZone) RESULT(iCalcLocation)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER                        :: iCalcLocation
    
    IF (RootZone%iVersion .EQ. 0) THEN
        iCalcLocation = -1
    ELSE
        SELECT TYPE (p => RootZone%Me)
            CLASS IS (RootZone_v50_Type)
                iCalcLocation = FlowDest_Subregion
            CLASS DEFAULT
                iCalcLocation = FlowDest_Element
        END SELECT
    END IF    
    
  END FUNCTION GetDemandCalcLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL VOLUMETRIC SOIL MOISTURE
  ! -------------------------------------------------------------
  SUBROUTINE GetElementSoilMVolume(RootZone,AppGrid,SoilM)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid   
    REAL(8),INTENT(OUT)            :: SoilM(:)
    
    CALL RootZone%Me%GetElementSoilMVolume(AppGrid,SoilM)
    
  END SUBROUTINE GetElementSoilMVolume
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL RIPARIAN ET AT STREAM NODES 
  ! -------------------------------------------------------------
  SUBROUTINE GetActualRiparianET_AtStrmNodes(RootZone,QRVET)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)            :: QRVET(:)

    SELECT TYPE (p => RootZone%Me)
        CLASS IS (RootZone_v41_Type)
            CALL p%GetActualRiparianET_AtStrmNodes(QRVET)
        CLASS DEFAULT
            QRVET = 0.0
    END SELECT
        
  END SUBROUTINE GetActualRiparianET_AtStrmNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT LEVEL GW INFLOWS INTO ROOT ZONE 
  ! -------------------------------------------------------------
  FUNCTION GetElemGWInflows(RootZone,NElements) RESULT(GWInflows)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: NElements
    REAL(8)                        :: GWInflows(NElements)
    
    SELECT TYPE (p => RootZone%Me)
        CLASS IS (RootZone_v41_Type)
            CALL p%GetActualETFromGW_AtElems(NElements,GWInflows) 
        CLASS DEFAULT
            GWInflows = 0.0
    END SELECT
        
  END FUNCTION GetElemGWInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL AG SUPPLIES TO DEMAND LOCATIONS 
  ! -------------------------------------------------------------
  SUBROUTINE GetWaterSupply_Ag(RootZone,AppGrid,Supply)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8)                        :: Supply(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        Supply = 0.0
    ELSE
        CALL RootZone%Me%GetWaterSupply_Ag(AppGrid,Supply)
    END IF
        
  END SUBROUTINE GetWaterSupply_Ag      
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL URBAN SUPPLIES TO DEMAND LOCATIONS 
  ! -------------------------------------------------------------
  SUBROUTINE GetWaterSupply_Urb(RootZone,AppGrid,Supply)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8)                        :: Supply(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        Supply = 0.0
    ELSE
        CALL RootZone%Me%GetWaterSupply_Urb(AppGrid,Supply)
    END IF
        
  END SUBROUTINE GetWaterSupply_Urb     
 
    
  ! -------------------------------------------------------------
  ! --- GET RATIO OF DESTINATION SUPPLIES TO REGIONAL SUPLLIES FOR AG 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetRatio_DestinationSupplyToRegionSupply_Ag(RootZone,Ratio)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)            :: Ratio(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        RETURN
    ELSE
        CALL RootZone%Me%GetRatio_DestSupplyToRegionSupply_Ag(Ratio)
    END IF
    
  END SUBROUTINE GetRatio_DestinationSupplyToRegionSupply_Ag


  ! -------------------------------------------------------------
  ! --- GET RATIO OF DESTINATION SUPPLIES TO REGIONAL SUPPLIES FOR URBAN 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetRatio_DestinationSupplyToRegionSupply_Urb(RootZone,Ratio)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)            :: Ratio(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        RETURN
    ELSE
        CALL RootZone%Me%GetRatio_DestSupplyToRegionSupply_Urb(Ratio)
    END IF
    
  END SUBROUTINE GetRatio_DestinationSupplyToRegionSupply_Urb
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DEMAND CALCULATION LOCATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNDemandLocations(RootZone) RESULT(NLocations)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER                        :: NLocations
    
    IF (RootZone%iVersion .EQ. 0) THEN
        NLocations = 0
    ELSE
        NLocations = RootZone%Me%GetNDemandLocations()
    END IF

  END FUNCTION GetNDemandLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET PRECIPITATION INFILTRATION AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementPrecipInfilt(RootZone,ElemRegion,PrecipInfilt)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: ElemRegion(:)
    REAL(8)                        :: PrecipInfilt(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        PrecipInfilt = 0.0
    ELSE
        CALL RootZone%Me%GetElementPrecipInfilt(ElemRegion,PrecipInfilt)
    END IF

  END SUBROUTINE GetElementPrecipInfilt
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL ET AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementActualET(RootZone,ElemRegion,ET)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: ElemRegion(:)
    REAL(8)                        :: ET(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        ET = 0.0
    ELSE
        CALL RootZone%Me%GetElementActualET(ElemRegion,ET)
    END IF

  END SUBROUTINE GetElementActualET
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF AGRICULTURAL CROPS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNAgCrops(RootZone) RESULT(NAgCrops)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER                        :: NAgCrops
    
    IF (RootZone%iVersion .EQ. 0) THEN
        NAgCrops = 0
    ELSE
        NAgCrops = RootZone%Me%GetNAgCrops()
    END IF

  END FUNCTION GetNAgCrops
  
  
  ! -------------------------------------------------------------
  ! --- GET AG WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE GetWaterDemand_Ag(RootZone,Demand)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8)                        :: Demand(:)
    
    CALL RootZone%Me%GetWaterDemand_Ag(Demand)

  END SUBROUTINE GetWaterDemand_Ag
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE GetWaterDemand_Urb(RootZone,Demand)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8)                        :: Demand(:)
    
    CALL RootZone%Me%GetWaterDemand_Urb(Demand)

  END SUBROUTINE GetWaterDemand_Urb
  

  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL AG AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetElementAgAreas(RootZone,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetElementAgAreas(Areas)
    
  END SUBROUTINE GetElementAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetElementUrbanAreas(RootZone,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetElementUrbanAreas(Areas)
    
  END SUBROUTINE GetElementUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetElementNativeVegAreas(RootZone,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetElementNativeVegAreas(Areas)
    
  END SUBROUTINE GetElementNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENTAL RIPARIAN VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetElementRiparianVegAreas(RootZone,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetElementRiparianVegAreas(Areas)
    
  END SUBROUTINE GetElementRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL AG AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionAgAreas(RootZone,AppGrid,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetSubregionAgAreas(AppGrid,Areas)
    
  END SUBROUTINE GetSubregionAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionUrbanAreas(RootZone,AppGrid,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetSubregionUrbanAreas(AppGrid,Areas)
    
  END SUBROUTINE GetSubregionUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL NATIVE VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionNativeVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetSubregionNativeVegAreas(AppGrid,Areas)
    
  END SUBROUTINE GetSubregionNativeVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RIPARIAN VEGETATION AREAS
  ! -------------------------------------------------------------
  SUBROUTINE GetSubregionRiparianVegAreas(RootZone,AppGrid,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: Areas(:)
    
    CALL RootZone%Me%GetSubregionRiparianVegAreas(AppGrid,Areas)
    
  END SUBROUTINE GetSubregionRiparianVegAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET AG AREAS AT DEMAND LOCATIONS (ELEMENTS OR SUBREGIONS)
  ! -------------------------------------------------------------
  SUBROUTINE GetDemandAgAreas(RootZone,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),ALLOCATABLE            :: Areas(:)
    
    CALL RootZone%Me%GetDemandAgAreas(Areas)
    
  END SUBROUTINE GetDemandAgAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET URBAN AREAS AT DEMAND LOCATIONS (ELEMENTS OR SUBREGIONS)
  ! -------------------------------------------------------------
  SUBROUTINE GetDemandUrbanAreas(RootZone,Areas)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),ALLOCATABLE            :: Areas(:)
    
    CALL RootZone%Me%GetDemandUrbanAreas(Areas)
    
  END SUBROUTINE GetDemandUrbanAreas
  
  
  ! -------------------------------------------------------------
  ! --- GET SURFACE FLOW DESTINATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION GetSurfaceFlowDestinations(RootZone,NLocations) RESULT(Dest)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: NLocations
    INTEGER                        :: Dest(NLocations)
    
    Dest = RootZone%Me%GetSurfaceFlowDestinations(NLocations)

  END FUNCTION GetSurfaceFlowDestinations
  
  
  ! -------------------------------------------------------------
  ! --- GET SURFACE FLOW DESTINATION TYPES
  ! -------------------------------------------------------------
  PURE FUNCTION GetSurfaceFlowDestinationTypes(RootZone,NLocations) RESULT(DestTypes)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: NLocations
    INTEGER                        :: DestTypes(NLocations)
    
    DestTypes = RootZone%Me%GetSurfaceFlowDestinationTypes(NLocations)
    
  END FUNCTION GetSurfaceFlowDestinationTypes
  
  
  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT ALL LOCATIONS
  ! -------------------------------------------------------------
  FUNCTION GetPercAll(RootZone,AppGrid) RESULT(Perc)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8)                        :: Perc(AppGrid%NElements)
    
    !Return if root zone is not defined
    IF (RootZone%iVersion .EQ. 0) RETURN
    
    !Print progress
    CALL EchoProgress('Retrieving percolation at all elements')
        
    Perc = RootZone%Me%GetPercAll(AppGrid)
    
  END FUNCTION GetPercAll


  ! -------------------------------------------------------------
  ! --- GET PERCOLATION AT AN INDIVIDUAL LOCATIONS
  ! -------------------------------------------------------------
  FUNCTION GetPercElement(RootZone,iLocation) RESULT(Perc)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: iLocation
    REAL(8)                        :: Perc
  
    !Return if root zone is not defined
    IF (RootZone%iVersion .EQ. 0) RETURN

    !Print progress
    CALL EchoProgress('Retrieving percolation at a specified element')
    
    Perc = RootZone%Me%GetPercElement(iLocation)
    
  END FUNCTION GetPercElement
  
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE GetFlowsToStreams(RootZone,AppGrid,DirectRunoff,ReturnFlow,RiparianET)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: DirectRunoff(:),ReturnFlow(:)
    REAL(8),INTENT(INOUT)          :: RiparianET(:)
    
    IF (RootZone%iVersion .NE. 0) CALL RootZone%Me%GetFlowsToStreams(AppGrid,DirectRunoff,ReturnFlow,RiparianET)
    
  END SUBROUTINE GetFlowsToStreams
  
  
  ! -------------------------------------------------------------
  ! --- GET DIRECT RUNOFF AND RETURN FLOW TO LAKES
  ! -------------------------------------------------------------
  SUBROUTINE GetFlowsToLakes(RootZone,AppGrid,DirectRunoff,ReturnFlow)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: DirectRunoff(:),ReturnFlow(:)
    
    IF (RootZone%iVersion .NE. 0) CALL RootZone%Me%GetFlowsToLakes(AppGrid,DirectRunoff,ReturnFlow)
    
  END SUBROUTINE GetFlowsToLakes
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBERS
  ! -------------------------------------------------------------
  FUNCTION GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(RootZone_v50_Type)  :: v50
    TYPE(RootZone_v40_Type)  :: v40
    TYPE(RootZone_v401_Type) :: v401
    TYPE(RootZone_v41_Type)  :: v41
    TYPE(RootZone_v411_Type) :: v411
    TYPE(VersionType)        :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) // ' (Interface) ; ' // TRIM(v50%GetVersion()) // ', ' // TRIM(v40%GetVersion()) // ', ' // TRIM(v401%GetVersion()) //  ', ' // TRIM(v41%GetVersion()) //  ', ' // TRIM(v411%GetVersion()) // ' (Components)'
    
  END FUNCTION GetVersion

  
  ! -------------------------------------------------------------
  ! --- GET THE VERSION NUMBER OF THE CURRENT ROOT ZONE OBJECT
  ! -------------------------------------------------------------
  FUNCTION GetActiveVersion(RootZone) RESULT(iVersion)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER                        :: iVersion
    
    iVersion = RootZone%iVersion
    
  END FUNCTION GetActiveVersion
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL PERCOLATION
  ! -------------------------------------------------------------
  FUNCTION GetRegionalPerc(RootZone,AppGrid) RESULT(RPERC)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8)                        :: RPERC(AppGrid%NSubregions+1)
    
    IF (RootZone%iVersion .EQ. 0.0) THEN
        RPERC = 0.0
    ELSE
        RPERC = RootZone%Me%RegionalPerc(AppGrid)
    END IF
 
  END FUNCTION GetRegionalPerc

  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RETURN FLOW FROM AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE GetRegionalReturnFlow_Ag(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: RReturnFlow(AppGrid%NSubregions+1)
    
    IF (RootZone%iVersion .EQ. 0.0) THEN
        RReturnFlow = 0.0
    ELSE
        CALL RootZone%Me%RegionalReturnFlow_Ag(AppGrid,RReturnFlow)
    END IF
 
  END SUBROUTINE GetRegionalReturnFlow_Ag

  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RETURN FLOW FROM URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE GetRegionalReturnFlow_Urb(RootZone,AppGrid,RReturnFlow)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    REAL(8),INTENT(OUT)            :: RReturnFlow(AppGrid%NSubregions+1)
    
    IF (RootZone%iVersion .EQ. 0.0) THEN
        RReturnFlow = 0.0
    ELSE
        CALL RootZone%Me%RegionalReturnFlow_Urb(AppGrid,RReturnFlow)
    END IF
 
  END SUBROUTINE GetRegionalReturnFlow_Urb


  
  
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
  ! --- SET THE LAKE ELEMENT FLAG
  ! -------------------------------------------------------------
  SUBROUTINE SetLakeElemFlag(RootZone,iLakeElem)
    CLASS(RootZoneType) :: RootZone
    INTEGER,INTENT(IN)  :: iLakeElem(:)
    
    IF (RootZone%iVersion .NE. 0) CALL RootZone%Me%SetLakeElemFlag(iLakeElem)

  END SUBROUTINE SetLakeElemFlag
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY TO DEMAND LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE SetSupply(RootZone,Supply,SupplyType)
    CLASS(RootZoneType) :: RootZone
    REAL(8),INTENT(IN)  :: Supply(:)
    INTEGER,INTENT(IN)  :: SupplyType
    
    IF (RootZone%iVersion .EQ. 0) RETURN
    
    CALL RootZone%Me%SetSupply(Supply,SupplyType)
    
  END SUBROUTINE SetSupply
  
  
  ! -------------------------------------------------------------
  ! --- SET ACTUAL RIPARIAN ET TAKEN OUT FROM STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE SetActualRiparianET_AtStrmNodes(RootZone,RiparianETFrac)
    CLASS(RootZoneType) :: RootZone
    REAL(8),INTENT(IN)  :: RiparianETFrac(:)
    
    SELECT TYPE (p => RootZone%Me)
        CLASS IS (RootZone_v41_Type)
            CALL p%SetActualRiparianET_AtStrmNodes(RiparianETFrac)
        CLASS DEFAULT 
            RETURN
    END SELECT
  
  END SUBROUTINE SetActualRiparianET_AtStrmNodes
  
  
  

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
  SUBROUTINE ReadRestartData(RootZone,InFile,iStat)
    CLASS(RootZoneType)   :: RootZone
    TYPE(GenericFileType) :: InFile
    INTEGER,INTENT(OUT)   :: iStat
    
    CALL RootZone%Me%ReadRestartData(InFile,iStat)
    
  END SUBROUTINE ReadRestartData
  
  
  ! -------------------------------------------------------------
  ! --- READ ROOT ZONE RELATED TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(RootZone,AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas)
    CLASS(RootZoneType)                :: RootZone
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    TYPE(ETType),INTENT(IN)            :: ETData
    INTEGER,INTENT(OUT)                :: iStat
    REAL(8),OPTIONAL,INTENT(IN)        :: RegionLUAreas(:,:)  !Should come as (region,land use) format. If provided, will overwrite regional land-use areas (for cell-level calculations regional areas will be distributed to cells)
    
    !Local variables
    INTEGER             :: indxRegion
    REAL(8),ALLOCATABLE :: RegionLUAreas_Work(:,:) 
    
    !Initialize
    iStat = 0
    
    IF (RootZone%iVersion .NE. 0) THEN
        IF (PRESENT(RegionLUAreas)) THEN
            !First, compute regional land-use areas based on normalized fractions for subregional areas to avoid any inconsistencies
            ALLOCATE (RegionLUAreas_Work(SIZE(RegionLUAreas,DIM=1),SIZE(RegionLUAreas,DIM=2)))
            RegionLUAreas_Work = RegionLUAreas
            DO indxRegion=1,AppGrid%NSubregions
                CALL NormalizeArray(RegionLUAreas_Work(indxRegion,:))
                RegionLUAreas_Work(indxRegion,:) = RegionLUAreas_Work(indxRegion,:) * AppGrid%AppSubregion(indxRegion)%Area
            END DO
            CALL RootZone%Me%ReadTSData(AppGrid,TimeStep,Precip,ETData,iStat,RegionLUAreas_Work)
        ELSE
            CALL RootZone%Me%ReadTSData(AppGrid,TimeStep,Precip,ETData,iStat)
        END IF
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
  SUBROUTINE PrintRestartData(RootZone,OutFile)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(GenericFileType)          :: OutFile
    
    CALL RootZone%Me%PrintRestartData(OutFile)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE TO PRINT OUT RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(RootZone,AppGrid,ETData,TimeStep,lEndOfSimulation)
    CLASS(RootZoneType)           :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(ETType),INTENT(IN)       :: ETData
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    IF (RootZone%iVersion .NE. 0) CALL RootZone%Me%PrintResults(AppGrid,ETData,TimeStep,lEndOfSimulation)
    
  END SUBROUTINE PrintResults

    
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DEMAND COMPUTATIONS AND ROUTING
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- COMPUTE AGRICULTURAL WATER DEMAND
  ! -------------------------------------------------------------
  SUBROUTINE ComputeWaterDemand(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZoneType)           :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
    
    iStat = 0
    CALL RootZone%Me%ComputeWaterDemand(AppGrid,TimeStep,ETData,iStat)
    
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
  ! --- CONVERT TIME UNIT OF ROOT ZONE RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(RootZone,NewUnit)
    CLASS(RootZoneType)         :: RootZone
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit

    IF (RootZone%iVersion .NE. 0) CALL RootZone%Me%ConvertTimeUnit(NewUnit)
    
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE STATE OF ROOT ZONE IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceState(RootZone)
    CLASS(RootZoneType) :: RootZone
    
    IF (RootZone%iVersion .NE. 0) CALL RootZone%Me%AdvanceState()
  
  END SUBROUTINE AdvanceState
  
  
  ! -------------------------------------------------------------
  ! --- ZERO OUT WATER SUPPLY
  ! -------------------------------------------------------------
  SUBROUTINE ZeroSupply(RootZone)
    CLASS(RootZoneType) :: RootZone
  
    CALL RootZone%Me%ZeroSupply()
    
  END SUBROUTINE ZeroSupply
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SOIL MOISTURE IN ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(RootZone,AppGrid,TimeStep,ETData,iStat)
    CLASS(RootZoneType)           :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    INTEGER,INTENT(OUT)           :: iStat
     
    iStat = 0
    IF (RootZone%iVersion .NE. 0) CALL RootZone%Me%Simulate(AppGrid,TimeStep,ETData,iStat)
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE GW INFLOW TO ROOT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE ComputeGWInflow(RootZone,DepthToGW,Sy)
    CLASS(RootZoneType) :: RootZone
    REAL(8),INTENT(IN)  :: DepthToGW(:),Sy(:)
    
    SELECT TYPE (p => RootZone%Me)
        CLASS IS (RootZone_v41_Type)
            CALL p%ComputeETFromGW_Max(DepthToGW,Sy)
        CLASS DEFAULT
            RETURN
    END SELECT
       
  END SUBROUTINE ComputeGWInflow


END MODULE


