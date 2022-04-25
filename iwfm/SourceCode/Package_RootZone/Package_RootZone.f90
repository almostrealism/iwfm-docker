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
MODULE Package_RootZone
  USE Class_Version              , ONLY : VersionType                        , &
                                          ReadVersion                        
  USE MessageLogger              , ONLY : SetLastMessage                     , & 
                                          EchoProgress                       , &
                                          MessageArray                       , &
                                          f_iFatal                             
  USE TimeSeriesUtilities        , ONLY : TimeStepType                       
  USE GeneralUtilities           , ONLY : FirstLocation                      , &
                                          CleanSpecialCharacters             , &
                                          FindSubStringInString              , &
                                          NormalizeArray                     , &
                                          IntToText                          , &
                                          ConvertID_To_Index
  USE IOInterface                , ONLY : GenericFileType                    
  USE Package_Misc               , ONLY : f_iLocationType_Subregion          , &
                                          f_iFlowDest_Element                , &
                                          f_iFlowDest_Subregion              , &
                                          f_iSupply_Diversion                , &
                                          f_iSupply_ElemPump                 , &
                                          f_iSupply_Well                     
  USE Package_Discretization     , ONLY : AppGridType
  USE Class_BaseRootZone         , ONLY : BaseRootZoneType                   , &
                                          ElementLU_InterpolateExtrapolate   , &
                                          iMeasuredLUDataForSubregion        , &
                                          iMeasuredLUDataForModelDomain 
  USE Util_Package_RootZone      , ONLY : f_iBudgetType_LWU                  , &
                                          f_iBudgetType_RootZone             , &
                                          f_iBudgetType_NonPondedCrop_LWU    , &
                                          f_iBudgetType_NonPondedCrop_RZ     , & 
                                          f_iBudgetType_PondedCrop_LWU       , &
                                          f_iBudgetType_PondedCrop_RZ        , & 
                                          f_iZBudgetType_RootZone            , &
                                          f_iZBudgetType_LWU             
  USE RootZone_v40               , ONLY : RootZone_v40_Type
  USE RootZone_v401              , ONLY : RootZone_v401_Type
  USE RootZone_v41               , ONLY : RootZone_v41_Type
  USE RootZone_v411              , ONLY : RootZone_v411_Type
  USE RootZone_v50               , ONLY : RootZone_v50_Type 
  USE Package_PrecipitationET    , ONLY : PrecipitationType                  ,  &
                                          ETType
  USE Package_ComponentConnectors, ONLY : SupplyDestinationConnectorType
  USE Package_ZBudget            , ONLY : ZBudgetType                        , &
                                          ZoneListType
  USE Package_Budget             , ONLY : BudgetType
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
            iMeasuredLUDataForModelDomain      , &
            f_iBudgetType_LWU                  , &
            f_iBudgetType_RootZone             , &
            f_iBudgetType_NonPondedCrop_LWU    , &
            f_iBudgetType_NonPondedCrop_RZ     , & 
            f_iBudgetType_PondedCrop_LWU       , &
            f_iBudgetType_PondedCrop_RZ        , &
            f_iZBudgetType_RootZone            , &
            f_iZBudgetType_LWU             
  
  
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
      PROCEDURE,PASS   :: GetMaxAndMinNetReturnFlowFrac
      PROCEDURE,PASS   :: GetBudget_NColumns
      PROCEDURE,PASS   :: GetBudget_ColumnTitles
      PROCEDURE,PASS   :: GetBudget_List
      PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenRootZone
      PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetBudget_TSData
      PROCEDURE,PASS   :: GetZBudget_List
      PROCEDURE,PASS   :: GetZBudget_NColumns
      PROCEDURE,PASS   :: GetZBudget_ColumnTitles
      PROCEDURE,PASS   :: GetZBudget_MonthlyFlows_GivenRootZone
      PROCEDURE,NOPASS :: GetZBudget_MonthlyFlows_GivenFile
      PROCEDURE,PASS   :: GetZBudget_TSData
      PROCEDURE,PASS   :: GetNAgCrops
      PROCEDURE,PASS   :: GetNDemandLocations
      PROCEDURE,PASS   :: GetDemandCalcLocation
      PROCEDURE,PASS   :: GetElementPrecip
      PROCEDURE,PASS   :: GetElementPrecipInfilt
      PROCEDURE,PASS   :: GetElementActualET
      PROCEDURE,PASS   :: GetWaterDemandAll                     
      PROCEDURE,PASS   :: GetWaterDemandAtLocations                     
      PROCEDURE,PASS   :: GetWaterSupply                      
      PROCEDURE,PASS   :: GetSupplyShortAtDestination_ForSomeSupplies
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
      PROCEDURE,PASS   :: GetFutureWaterDemands
      PROCEDURE,NOPASS :: GetVersion 
      PROCEDURE,PASS   :: SetSupply
      PROCEDURE,PASS   :: SetLakeElemFlag                       
      PROCEDURE,PASS   :: SetActualRiparianET_AtStrmNodes       
      PROCEDURE,PASS   :: IsDefined
      PROCEDURE,PASS   :: IsLandUseUpdated
      PROCEDURE,PASS   :: ComputeWaterDemand
      PROCEDURE,PASS   :: ComputeFutureWaterDemands
      PROCEDURE,PASS   :: Simulate 
      PROCEDURE,PASS   :: ConvertTimeUnit   
      PROCEDURE,PASS   :: ReadTSData                            
      PROCEDURE,PASS   :: ReadRestartData
      PROCEDURE,PASS   :: AdvanceState                          
      PROCEDURE,PASS   :: ZeroSupply                            
      PROCEDURE,PASS   :: ZeroSurfaceFlows                            
      PROCEDURE,PASS   :: PrintResults
      PROCEDURE,PASS   :: PrintRestartData
      PROCEDURE,PASS   :: ComputeGWInflow
      GENERIC          :: GetWaterDemand          => GetWaterDemandAll                      , &
                                                     GetWaterDemandAtLocations 
      GENERIC          :: GetBudget_MonthlyFlows  => GetBudget_MonthlyFlows_GivenFile       , &
                                                     GetBudget_MonthlyFlows_GivenRootZone 
      GENERIC          :: GetZBudget_MonthlyFlows => GetZBudget_MonthlyFlows_GivenFile      , &
                                                     GetZBudget_MonthlyFlows_GivenRootZone 
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
  SUBROUTINE New(RootZone,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iStrmNodeIDs,iLakeIDs)
    CLASS(RootZoneType),INTENT(OUT)    :: RootZone
    LOGICAL,INTENT(IN)                 :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)        :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    INTEGER,INTENT(IN)                 :: NTIME
    TYPE(ETType),INTENT(IN)            :: ET
    TYPE(PrecipitationType),INTENT(IN) :: Precip
    INTEGER,INTENT(OUT)                :: iStat
    INTEGER,OPTIONAL,INTENT(IN)        :: iStrmNodeIDs(:),iLakeIDs(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    TYPE(GenericFileType)       :: RootZoneParamFile
    CHARACTER(:),ALLOCATABLE    :: cVersion
    
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
    
    !Allocate memory based on version
    SELECT CASE (TRIM(cVersion))
        CASE ('5.0')
            ALLOCATE(RootZone_v50_Type :: RootZone%Me)
            RootZone%iVersion = 50
        CASE ('4.0')
            ALLOCATE(RootZone_v40_Type :: RootZone%Me)
            RootZone%iVersion = 40
        CASE ('4.01')
            ALLOCATE(RootZone_v401_Type :: RootZone%Me)
            RootZone%iVersion = 401
        CASE ('4.1')
            ALLOCATE(RootZone_v41_Type :: RootZone%Me)
            RootZone%iVersion = 41
        CASE ('4.11')
            ALLOCATE(RootZone_v411_Type :: RootZone%Me)
            RootZone%iVersion = 411
        CASE DEFAULT
            CALL SetLastMessage('Root Zone Component version number is not recognized ('//TRIM(cVersion)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Now, instantiate
    IF (PRESENT(iStrmNodeIDs)) THEN
        IF (PRESENT(iLakeIDs)) THEN
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iStrmNodeIDs=iStrmNodeIDs,iLakeIDs=iLakeIDs)
        ELSE
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iStrmNodeIDs=iStrmNodeIDs)
        END IF
    ELSE
        IF (PRESENT(iLakeIDs)) THEN
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat,iLakeIDs=iLakeIDs)
        ELSE
            CALL RootZone%Me%New(IsForInquiry,cFileName,cWorkingDirectory,AppGrid,TimeStep,NTIME,ET,Precip,iStat)
        END IF
    END IF
    
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
    
    IF (.NOT. ALLOCATED(RootZone%Me)) RETURN
    
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
  ! --- GET BUDGET LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_List(RootZone,iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    CLASS(RootZoneType),INTENT(IN)           :: RootZone
    INTEGER,ALLOCATABLE,INTENT(OUT)          :: iBudgetTypeList(:),iBudgetLocationTypeList(:)          
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cBudgetDescriptions(:),cBudgetFiles(:)
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetBudget_List(iBudgetTypeList,iBudgetLocationTypeList,cBudgetDescriptions,cBudgetFiles)
    ELSE
        ALLOCATE (iBudgetTypeList(0) , iBudgetLocationTypeList(0) , cBudgetDescriptions(0) , cBudgetFiles(0))
    END IF
        
  END SUBROUTINE GetBudget_List
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BUDGET FILE COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(RootZone,iBudgetType,iLocationIndex,iNCols,iStat)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: iBudgetType,iLocationIndex
    INTEGER,INTENT(OUT)            :: iNCols,iStat
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetBudget_NColumns(iBudgetType,iLocationIndex,iNCols,iStat)
    ELSE
        iStat  = 0
        iNCols = 0
    END IF
                
  END SUBROUTINE GetBudget_NColumns
     
     
  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN TITLES
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(RootZone,iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(RootZoneType),INTENT(IN)           :: RootZone
    INTEGER,INTENT(IN)                       :: iBudgetType,iLocationIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
    INTEGER,INTENT(OUT)                      :: iStat

    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetBudget_ColumnTitles(iBudgetType,iLocationIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    END IF

  END SUBROUTINE GetBudget_ColumnTitles
     
     
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM RootZone OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenRootZone(RootZone,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(RootZoneType),INTENT(IN)           :: RootZone
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLUType,iSubregionID  
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)     !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetBudget_MonthlyFlows_GivenRootZone(iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    ELSE
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
        iStat = 0
    END IF
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenRootZone
  
  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget          !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iBudgetType,iLUType,iSubregionID  
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)     !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'GetBudget_MonthlyFlows_GivenFile'
    INTEGER                      :: iErrorCode  
    CHARACTER(:),ALLOCATABLE     :: cVersion
    TYPE(RootZoneType)           :: RootZone
    
    !Get version number
    CALL GetPackageVersion(Budget,cVersion)
    
    !Based on component version, allocate base stream type
    SELECT CASE (cVersion)
        CASE ('4.0')
            ALLOCATE(RootZone_v40_Type :: RootZone%Me)
        CASE ('4.01')
            ALLOCATE(RootZone_v401_Type :: RootZone%Me)
        CASE ('4.1')
            ALLOCATE(RootZone_v41_Type :: RootZone%Me)
        CASE ('4.11')
            ALLOCATE(RootZone_v411_Type :: RootZone%Me)
        CASE ('5.0')
            ALLOCATE(RootZone_v50_Type :: RootZone%Me)
        CASE DEFAULT
            CALL SetLastMessage('Root Zone Component version number is not recognized ('//TRIM(cVersion)//')!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Get monthly data    
    CALL RootZone%Me%GetBudget_MonthlyFlows_GivenFile(Budget,iBudgetType,iLUType,iSubregionID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    
    !Clear memory
    DEALLOCATE (RootZone%Me , cVersion , STAT=iErrorCode)
    
  CONTAINS
   
    !#######################################################
    !### FIND THE VERSION NUMBER OF THE STREAM PACKAGE USED
    !#######################################################
    SUBROUTINE GetPackageVersion(Budget,cVersion)
      TYPE(BudgetType),INTENT(IN) :: Budget
      CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cVersion
      
      !Local variables
      INTEGER                      :: iNTitles,iLenTitles,indx,iLoc
      CHARACTER(LEN=:),ALLOCATABLE :: cTitles(:)
      CHARACTER(:),ALLOCATABLE     :: cTitlesConc
     
      !Get the ASCII titles from the Budget file; one of these titles include component version number
      iNTitles   = Budget%GetNPersistentTitles()
      iLenTitles = Budget%GetTitleLen()
      ALLOCATE (CHARACTER(iLenTitles) :: cTitles(iNTitles))
      cTitles = Budget%GetPersistentTitles(iNTitles)
      
      !Concotonate titles
      ALLOCATE (CHARACTER(LEN=iNTitles*iLenTitles) :: cTitlesConc)
      cTitlesConc = ''
      DO indx=1,iNTitles
          cTitlesConc = TRIM(cTitlesConc) // TRIM(ADJUSTL(cTitles(indx)))
      END DO  
      
      !Check for version 4.0
      CALL FindSubStringInString('v4.0.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '4.0'
          RETURN
      END IF 
      
      !Check for version 4.01
      CALL FindSubStringInString('v4.01.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(4) :: cVersion)
          cVersion = '4.01'
          RETURN
      END IF  

      !Check for version 4.1
      CALL FindSubStringInString('v4.1.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '4.1'
          RETURN
      END IF  

      !Check for version 4.11
      CALL FindSubStringInString('v4.11.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(4) :: cVersion)
          cVersion = '4.11'
          RETURN
      END IF  

      !Check for version 5.0
      CALL FindSubStringInString('v5.0.',TRIM(cTitlesConc),iLoc)
      IF (iLoc .GT. 0) THEN
          ALLOCATE (CHARACTER(3) :: cVersion)
          cVersion = '5.0'
          RETURN
      END IF 
      
      !If made to this point, something is wrong
      ALLOCATE (CHARACTER(3) :: cVersion)
      cVersion = '0.0'

    END SUBROUTINE GetPackageVersion
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(RootZone,iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: iBudgetType,iSubregionID,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)    :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)             :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)            :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)            :: iDataTypes(:),inActualOutput,iStat
    
    IF (ALLOCATED(RootZone%Me)) THEN    
        CALL RootZone%Me%GetBudget_TSData(iBudgetType,iSubregionID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    ELSE
        iStat          = 0
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
    END IF
           
  END SUBROUTINE GetBudget_TSData
  
  
  ! -------------------------------------------------------------
  ! --- GET MAXIMUM AND MINIMUM NET RETURN FLOW FRACTIONS USED DURING THE ENTIRE SIMULATION PERIOD
  ! -------------------------------------------------------------
  SUBROUTINE GetMaxAndMinNetReturnFlowFrac(RootZone,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    CLASS(RootZoneType)           :: RootZone
    TYPE(TimeStepType),INTENT(IN) :: FirstTimeStep
    REAL(8),INTENT(OUT)           :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT)           :: iStat
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetMaxAndMinNetReturnFlowFrac(FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    ELSE
        rMaxFrac = 1.0
        rMinFrac = 0.0
        iStat    = 0
    END IF
    
  END SUBROUTINE GetMaxAndMinNetReturnFlowFrac
  
  
  ! -------------------------------------------------------------
  ! --- GET Z-BUDGET LIST (THIS WILL BE OVERWRITTEN WITH VERSIONS THAT HAVE ZBUDGET OUTPUT)
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_List(RootZone,iZBudgetTypeList,cZBudgetDescriptions,cZBudgetFiles)
     CLASS(RootZoneType),INTENT(IN)           :: RootZone
     INTEGER,ALLOCATABLE,INTENT(OUT)          :: iZBudgetTypeList(:)          
     CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cZBudgetDescriptions(:),cZBudgetFiles(:)
     
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetZBudget_List(iZBudgetTypeList,cZBudgetDescriptions,cZBudgetFiles)
    ELSE
        ALLOCATE (iZBudgetTypeList(0) , cZBudgetDescriptions(0) , cZBudgetFiles(0))
    END IF
     
  END SUBROUTINE GetZBudget_List


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS FOR A ZBUDGET FILE (TIME COLUMN EXCLUDED)
  ! -------------------------------------------------------------
  FUNCTION GetZBudget_NColumns(RootZone,iZBudgetType) RESULT(iNCols)
     CLASS(RootZoneType),INTENT(IN) :: RootZone
     INTEGER,INTENT(IN)             :: iZBudgetType
     INTEGER                        :: iNCols
     
    IF (ALLOCATED(RootZone%Me)) THEN
        iNCols = RootZone%Me%GetZBudget_NColumns(iZBudgetType)
    ELSE
        iNCols = 0
    END IF
    
  END FUNCTION GetZBudget_NColumns


  ! -------------------------------------------------------------
  ! --- GET COLUMN TITLES FOR A ZBUDGET FILE (TIME COlUMN EXCLUDED)
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_ColumnTitles(RootZone,iZBudgetType,cUnitAR,cUnitVL,cColTitles,iStat)
     CLASS(RootZoneType),INTENT(IN)           :: RootZone
     INTEGER,INTENT(IN)                       :: iZBudgetType
     CHARACTER(LEN=*),INTENT(IN)              :: cUnitAR,cUnitVL
     CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)
     INTEGER,INTENT(OUT)                      :: iStat
     
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetZBudget_ColumnTitles(iZBudgetType,cUnitAR,cUnitVL,cColTitles,iStat)
    ELSE
        iStat = 0
        ALLOCATE (cColTitles(0))
    END IF
    
  END SUBROUTINE GetZBudget_ColumnTitles


  ! -------------------------------------------------------------
  ! --- RETRIEVE MONTHLY FLOW TERMS FROM RootZone OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_MonthlyFlows_GivenRootZone(RootZone,iZBudgetType,iLUType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(RootZoneType),INTENT(IN)           :: RootZone 
    INTEGER,INTENT(IN)                       :: iZBudgetType,iZoneID,iLUType,iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate  !Assumes cBeginDate and cEndDate are properly set for monthly average values
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)          !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetZBudget_MonthlyFlows_GivenRootZone(iZBudgetType,iLUType,iZoneID,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    ELSE
        iStat = 0
        ALLOCATE (rFlows(0,0) , cFlowNames(0))
    END IF
    
  END SUBROUTINE GetZBudget_MonthlyFlows_GivenRootZone
    
  
  ! -------------------------------------------------------------
  ! --- RETRIEVE MONTHLY FLOW TERMS FROM ZBUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_MonthlyFlows_GivenFile(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(ZBudgetType),INTENT(IN)             :: ZBudget              !Assumes ZBudget file is already open 
    TYPE(ZoneListType),INTENT(IN)            :: ZoneList
    INTEGER,INTENT(IN)                       :: iZBudgetType,iZoneID,iLUType
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate  !Assumes cBeginDate and cEndDate are properly set for monthly average values
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)          !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER            :: iLoc
    TYPE(RootZoneType) :: RootZone
    
    !Is this root zone v4.01?
    CALL FindSubStringInString('v4.01',ZBudget%Header%cSoftwareVersion,iLoc)
    IF (iLoc .GT. 0) THEN
        ALLOCATE (RootZone_v401_Type :: RootZone%Me)
        CALL RootZone%Me%GetZBudget_MonthlyFlows_GivenFile(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
        CALL RootZone%Kill()
        RETURN
    END IF

    !Is this root zone v4.11?
    CALL FindSubStringInString('v4.11',ZBudget%Header%cSoftwareVersion,iLoc)
    IF (iLoc .GT. 0) THEN
        ALLOCATE (RootZone_v411_Type :: RootZone%Me)
        CALL RootZone%Me%GetZBudget_MonthlyFlows_GivenFile(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
        CALL RootZone%Kill()
        RETURN
    END IF
    
    !Is this root zone v5.0?
    CALL FindSubStringInString('v5.0',ZBudget%Header%cSoftwareVersion,iLoc)
    IF (iLoc .GT. 0) THEN
        ALLOCATE (RootZone_v50_Type :: RootZone%Me)
        CALL RootZone%Me%GetZBudget_MonthlyFlows_GivenFile(ZBudget,iZBudgetType,ZoneList,iZoneID,iLUType,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
        CALL RootZone%Kill()
        RETURN
    END IF
    
    !Otherwise, allocate all return data with null values
    iStat = 0
    ALLOCATE (rFlows(0,0) , cFlowNames(0))
    
  END SUBROUTINE GetZBudget_MonthlyFlows_GivenFile
    
  
  ! -------------------------------------------------------------
  ! --- GET TIME SERIES DATA FROM ZBUDGET FILE FOR A SELECTED ZONE AND SELECTED COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE GetZBudget_TSData(RootZone,iZBudgetType,iZoneID,iCols,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,cInterval,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: iZBudgetType,iZoneID,iCols(:),iZExtent,iElems(:),iLayers(:),iZoneIDs(:)
    CHARACTER(LEN=*),INTENT(IN)    :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)             :: rFactAR,rFactVL
    REAL(8),INTENT(OUT)            :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)            :: iDataTypes(:),inActualOutput,iStat
    
    IF (ALLOCATED(RootZone%Me)) THEN
        CALL RootZone%Me%GetZBudget_TSData(iZBudgetType,iZoneID,iCols,iZExtent,iElems,iLayers,iZoneIDs,cBeginDate,cEndDate,cInterval,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    ELSE
        iStat         = 0
        rOutputDates  = 0.0
        rOutputValues = 0.0
        iDataTypes    = -1
        inActualOutput = 0
    END IF
    
  END SUBROUTINE GetZBudget_TSData

  
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
                iCalcLocation = f_iFlowDest_Subregion
            CLASS DEFAULT
                iCalcLocation = f_iFlowDest_Element
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
  ! --- GET TOTAL WATER SUPPLIES TO DEMAND LOCATIONS 
  ! -------------------------------------------------------------
  SUBROUTINE GetWaterSupply(RootZone,AppGrid,iSupplyFor,rSupply)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid
    INTEGER,INTENT(IN)             :: iSupplyFor
    REAL(8)                        :: rSupply(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        rSupply = 0.0
    ELSE
        CALL RootZone%Me%GetWaterSupply(AppGrid,iSupplyFor,rSupply)
    END IF
        
  END SUBROUTINE GetWaterSupply      
  
  
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
  ! --- GET PRECIPITATION AT EACH ELEMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetElementPrecip(RootZone,rElemArea,rPrecip)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    REAL(8),INTENT(IN)             :: rElemArea(:)
    REAL(8)                        :: rPrecip(:)
    
    IF (RootZone%iVersion .EQ. 0) THEN
        rPrecip = 0.0
    ELSE
        CALL RootZone%Me%GetElementPrecip(rElemArea,rPrecip)
    END IF

  END SUBROUTINE GetElementPrecip
  
  
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
  ! --- GET WATER DEMAND AT ALL DEMAND LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetWaterDemandAll(RootZone,iDemandFor,rDemand)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    INTEGER,INTENT(IN)             :: iDemandFor
    REAL(8)                        :: rDemand(:)
    
    CALL RootZone%Me%GetWaterDemandAll(iDemandFor,rDemand)

  END SUBROUTINE GetWaterDemandAll
  
  
  ! -------------------------------------------------------------
  ! --- GET WATER DEMAND AT SOME LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetWaterDemandAtLocations(RootZone,AppGrid,iLocationTypeID,iLocationList,iDemandFor,rDemand,iStat)
    CLASS(RootZoneType),INTENT(IN) :: RootZone
    TYPE(AppGridType),INTENT(IN)   :: AppGrid 
    INTEGER,INTENT(IN)             :: iLocationTypeID,iLocationList(:),iDemandFor
    REAL(8)                        :: rDemand(:)
    INTEGER,INTENT(OUT)            :: iStat
    
    CALL RootZone%Me%GetWaterDemandAtLocations(AppGrid,iLocationTypeID,iLocationList,iDemandFor,rDemand,iStat)

  END SUBROUTINE GetWaterDemandAtLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET FUTURE DEMANDS AT A SPECIFIED DATE
  ! --- Note: Must be called after ReadTSData method for proper operation
  ! -------------------------------------------------------------
  SUBROUTINE GetFutureWaterDemands(RootZone,AppGrid,TimeStep,Precip,ET,cFutureDemandDate,rElemAgDemand,rElemUrbDemand,iStat)
    CLASS(RootZoneType)           :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(PrecipitationType)       :: Precip
    TYPE(ETType)                  :: ET
    CHARACTER(LEN=*),INTENT(IN)   :: cFutureDemandDate
    REAL(8),INTENT(OUT)           :: rElemAgDemand(:),rElemUrbDemand(:)  !Return data for each (element) combination
    INTEGER,INTENT(OUT)           :: iStat
    
    
    IF (RootZone%iVersion .NE. 0) THEN
        CALL RootZone%Me%GetFutureDemands(AppGrid,TimeStep,Precip,ET,cFutureDemandDate,rElemAgDemand,rElemUrbDemand,iStat)
    ELSE
        iStat          = 0
        rElemAgDemand  = 0.0
        rElemUrbDemand = 0.0
    END IF
    
  END SUBROUTINE GetFutureWaterDemands
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY SHORTAGE AT DESTINATIONS FOR SOME SUPPLIES
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyShortAtDestination_ForSomeSupplies(RootZone,AppGrid,iSupplyList,iSupplyFor,SupplyDestConnector,rSupplyShortAtDest)
    CLASS(RootZoneType),INTENT(IN)                  :: RootZone
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    INTEGER,INTENT(IN)                              :: iSupplyList(:),iSupplyFor
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: SupplyDestConnector
    REAL(8),INTENT(OUT)                             :: rSupplyShortAtDest(:) 
  
    CALL RootZone%Me%GetSupplyShortAtDestination_ForSomeSupplies(AppGrid,iSupplyList,iSupplyFor,SupplyDestConnector,rSupplyShortAtDest)
    
  END SUBROUTINE GetSupplyShortAtDestination_ForSomeSupplies

  
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
    cVrs      = TRIM(MyVersion%GetVersion()) // ' (Interface) ; ' // TRIM(v40%GetVersion()) // ', ' // TRIM(v401%GetVersion()) //  ', ' // TRIM(v41%GetVersion()) //  ', ' // TRIM(v411%GetVersion()) // ', ' // TRIM(v50%GetVersion()) // ' (Components)'
    
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
  SUBROUTINE SetSupply(RootZone,rSupply,iSupplyType,iSupplyFor)
    CLASS(RootZoneType) :: RootZone
    REAL(8),INTENT(IN)  :: rSupply(:)
    INTEGER,INTENT(IN)  :: iSupplyType,iSupplyFor
    
    IF (RootZone%iVersion .EQ. 0) RETURN
    
    CALL RootZone%Me%SetSupply(rSupply,iSupplyType,iSupplyFor)
    
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
  ! --- ZERO OUT SURFACE FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE ZeroSurfaceFlows(RootZone)
    CLASS(RootZoneType) :: RootZone
  
    CALL RootZone%Me%ZeroSurfaceFlows()
    
  END SUBROUTINE ZeroSurfaceFlows
  
  
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


  ! -------------------------------------------------------------
  ! --- COMPUTE FUTURE DEMANDS NTIL A SPECIFIED DATE
  ! --- Note: Must be called after ReadTSData method for proper operation
  ! -------------------------------------------------------------
  SUBROUTINE ComputeFutureWaterDemands(RootZone,AppGrid,TimeStep,Precip,ET,cEndFutureDemandDate,iStat)
    CLASS(RootZoneType)           :: RootZone
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(PrecipitationType)       :: Precip
    TYPE(ETType)                  :: ET
    CHARACTER(LEN=*),INTENT(IN)   :: cEndFutureDemandDate
    INTEGER,INTENT(OUT)           :: iStat
    
    
    IF (RootZone%iVersion .NE. 0) THEN
        CALL RootZone%Me%ComputeFutureWaterDemand(AppGrid,TimeStep,Precip,ET,cEndFutureDemandDate,iStat)
    ELSE
        iStat = 0
    END IF
    
  END SUBROUTINE ComputeFutureWaterDemands
    
END MODULE


