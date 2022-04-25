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
MODULE Class_UrbanLandUse_v50
  USE IOInterface             , ONLY: GenericFileType                         
  USE MessageLogger           , ONLY: LogMessage                              , &
                                      SetLastMessage                          , &
                                      EchoProgress                            , &
                                      MessageArray                            , &
                                      f_iFatal                                , &
                                      f_iInfo
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter                 , &
                                      CleanSpecialCharacters                  , &
                                      IntToText                               , &
                                      NormalizeArray                          , &
                                      ConvertID_To_Index                      , &
                                      EstablishAbsolutePathFileName
  USE TimeSeriesUtilities     , ONLY: TimeStepType                            , &
                                      IncrementTimeStamp                      , &
                                      OPERATOR(.TSGT.)
  USE Package_Discretization  , ONLY: AppGridType
  USE Class_GenericLandUse    , ONLY: GenericLandUseType
  USE Class_LandUseDataFile   , ONLY: LandUseDataFileType    
  USE Package_Misc            , ONLY: RealTSDataInFileType                    , &
                                      SolverDataType                          , &
                                      f_iFlowDest_Outside                     , &
                                      f_iFlowDest_StrmNode                    , &
                                      f_iFlowDest_Lake                        , &
                                      f_iFlowDest_Subregion                   , &
                                      f_iFlowDest_GWElement                   , &
                                      Package_Misc_ReadTSData   => ReadTSData
  USE Util_Package_RootZone   , ONLY: ReadRealData
  USE Package_PrecipitationET , ONLY: ETType
  USe Package_UnsatZone       , ONLY: RootZoneSoilType                        , &
                                      NonPondedLUMoistureRouter
  USE Class_BaseRootZone      , ONLY: ElemSurfaceFlowToDestType               , &
                                      TrackMoistureDueToSource                , &
                                      CompileElemSurfaceFlowToDestinationList
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
  PUBLIC :: UrbanDatabase_v50_Type


  ! -------------------------------------------------------------
  ! --- URBAN LAND DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLandUseType) :: UrbanType
      REAL(8) :: IrigInfilt = 0.0     !Infiltration due to irrigation
      REAL(8) :: Reuse      = 0.0     !Reused return flow 
      REAL(8) :: ReturnFlow = 0.0     !Return flow
  END TYPE UrbanType
  
  
  ! -------------------------------------------------------------
  ! --- URBAN LAND DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE UrbanDatabase_v50_Type
    TYPE(UrbanType),ALLOCATABLE                 :: UrbData(:,:)                    !Urban data for each (soil,subregion) combination
    REAL(8)                                     :: RootDepth               = 0.0   !Urban root depth
    INTEGER,ALLOCATABLE                         :: iColReturnFrac(:)               !Column number in the return flow fraction data file defined for each (subregion)
    INTEGER,ALLOCATABLE                         :: iColReuseFrac(:)                !Column number in the re-use fraction data file defined for each (subregion)
    INTEGER,ALLOCATABLE                         :: iColWaterDemand(:)              !Column number in the water demand data file defined for each (subregion)
    INTEGER,ALLOCATABLE                         :: iColWaterUseSpec(:)             !Column number in the urban water use specs data file for each (subregion)
    REAL(8),ALLOCATABLE                         :: ElementalArea(:)                !Urban area at each (element) at current time step
    REAL(8),ALLOCATABLE                         :: ElementalArea_P(:)              !Urban area at each (element) at previous time step
    REAL(8),ALLOCATABLE                         :: SubregionalArea(:)              !Total urban area for each (subregion)             
    REAL(8),ALLOCATABLE                         :: Demand(:)                       !Urban water demand for each (subregion)
    REAL(8)                                     :: DemandConversionFactor  = 1.0   !Conversion factor for urban water demand
    REAL(8),ALLOCATABLE                         :: PerviousFrac(:)                 !Fraction of pervious area to total urban area at each (subregion)
    INTEGER,ALLOCATABLE                         :: ElemToOutside(:)                !List of elements where surface flow goes outside model domain
    TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemToStreams(:)                !List of elements and corresponding stream nodes where surface flow goes into streams
    TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemToLakes(:)                  !List of elements and corresponding lakes where surface flow goes into lakes
    TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemToSubregions(:)             !List of elements and corresponding subregions where surface flow goes into subregions
    INTEGER,ALLOCATABLE                         :: ElemToGW(:)                     !List of elements where surface flow goes into groundwater at the same element
    REAL(8),ALLOCATABLE                         :: RegionETPot(:)                  !Regional urban potential ET
    TYPE(LandUseDataFileType)                   :: LandUseDataFile                 !Land use data file
    TYPE(RealTSDataInFileType)                  :: WaterDemandFile                 !Urban water demand data file
    TYPE(RealTSDataInFileType)                  :: WaterUseSpecsFile               !Urban water use specs data file
  CONTAINS
    PROCEDURE,PASS :: New
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: GetMaxAndMinNetReturnFlowFrac
    PROCEDURE,PASS :: SetAreas
    PROCEDURE,PASS :: ReadTSData
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: SoilMContent_To_Depth
    PROCEDURE,PASS :: AdvanceAreas
    PROCEDURE,PASS :: Simulate
    PROCEDURE,PASS :: RewindTSInputFilesToTimeStamp          
  END TYPE UrbanDatabase_v50_Type


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 24
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_UrbanLandUse_v50::'
  
  
  
  
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
  ! --- NEW URBAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(UrbLand,cFileName,cWorkingDirectory,AppGrid,FactCN,NSoils,iElemIDs,iSubregionIDs,TrackTime,iStat,iStrmNodeIDs,iLakeIDs)
    CLASS(UrbanDatabase_v50_Type) :: UrbLand
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    REAL(8),INTENT(IN)            :: FACTCN
    INTEGER,INTENT(IN)            :: NSoils,iElemIDs(AppGrid%NElements),iSubregionIDs(AppGrid%NSubregions)
    LOGICAL,INTENT(IN)            :: TrackTime
    INTEGER,INTENT(OUT)           :: iStat
    INTEGER,OPTIONAL,INTENT(IN)   :: iStrmNodeIDs(:),iLakeIDs(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3)                 :: ThisProcedure = ModName // 'New'
    CHARACTER                                   :: ALine*1000
    INTEGER                                     :: ErrorCode,indxRegion,indxElem,NElements,NSubregions,iRegion,ID,iStrmNode,  &
                                                   SurfaceFlowDestType(AppGrid%NElements),SurfaceFlowDest(AppGrid%NElements), &
                                                   iElem,iLake
    REAL(8)                                     :: FACT,Factor(1)
    REAL(8),ALLOCATABLE                         :: DummyArray(:,:)
    LOGICAL                                     :: lProcessed(AppGrid%NSubregions),lProcessed_Elem(AppGrid%NElements)
    TYPE(GenericFileType)                       :: UrbanDataFile
    TYPE(ElemSurfaceFlowToDestType),ALLOCATABLE :: ElemToGW(:),ElemToOutside(:)
    CHARACTER(:),ALLOCATABLE                    :: cAbsPathFileName
    
    !Initialize
    iStat = 0
   
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Initialize
    NElements   = AppGrid%NElements
    NSubregions = AppGrid%NSubregions
    
    !Open file
    CALL UrbanDataFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (UrbLand%UrbData(NSoils,NSubregions)    , &
              UrbLand%iColReturnFrac(NSubregions)    , &
              UrbLand%iColReuseFrac(NSubregions)     , &
              UrbLand%iColWaterDemand(NSubregions)   , &
              UrbLand%iColWaterUseSpec(NSubregions)  , &
              UrbLand%ElementalArea(NElements)       , &
              UrbLand%ElementalArea_P(NElements)     , &
              UrbLand%SubregionalArea(NSubregions)   , &
              UrbLand%Demand(NSubregions)            , &
              UrbLand%PerviousFrac(NSubregions)      , &
              UrbLand%RegionETPot(NSubregions)       , &
              STAT=ErrorCode                         )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for urban data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize elemental area to zero
    UrbLand%ElementalArea = 0.0
    
    !Land use data file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%LandUseDataFile%New(cAbsPathFileName,cWorkingDirectory,'Urban area file',NElements,1,TrackTime,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Rooting depth
    CALL UrbanDataFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL UrbanDataFile%ReadData(UrbLand%RootDepth,iStat)  ;  IF (iStat .EQ. -1) RETURN
    UrbLand%RootDepth = UrbLand%RootDepth * FACT
    
    !Read CN column pointers
    CALL ReadRealData(UrbanDataFile,'curve numbers for urban lands','subregions',NSubregions,NSoils+1,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = INT(DummyArray(indxRegion,1))
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Curve numbers for urban lands for subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)             = .TRUE.
        UrbLand%UrbData(:,iRegion)%SMax = (1000.0/DummyArray(indxRegion,2:)-10.0) * FACTCN
    END DO
    
    !Urban water demand file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%WaterDemandFile%Init(cAbsPathFileName,cWorkingDirectory,'Urban water demand data file',TrackTime,BlocksToSkip=1,lFactorDefined=.TRUE.,Factor=Factor,RateTypeData=(/.TRUE./),iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    UrbLand%DemandConversionFactor = Factor(1)
      
    !Urban water use specifications data file
    CALL UrbanDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/') 
    CALL CleanSpecialCharacters(ALine)
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
    CALL UrbLand%WaterUseSpecsFile%Init(cAbsPathFileName,cWorkingDirectory,'Urban water use specifications',TrackTime,BlocksToSkip=1,lFactorDefined=.FALSE.,Factor=Factor,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN

    !Read other data
    CALL ReadRealData(UrbanDataFile,'water use and management data for urban lands','subregions',NSubregions,7,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed = .FALSE.
    DO indxRegion=1,NSubregions
        iRegion = INT(DummyArray(indxRegion,1))
        IF (lProcessed(iRegion)) THEN
            ID = iSubregionIDs(iRegion)
            CALL SetLastMessage('Water use and management data for urban lands for subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)                = .TRUE.
        UrbLand%PerviousFrac(iRegion)      =     DummyArray(indxRegion,2)
        UrbLand%iColWaterDemand(iRegion)   = INT(DummyArray(indxRegion,3))
        UrbLand%iColWaterUseSpec(iRegion)  = INT(DummyArray(indxRegion,4))
        UrbLand%UrbData(:,iRegion)%iColETc = INT(DummyArray(indxRegion,5))
        UrbLand%iColReturnFrac(iRegion)    = INT(DummyArray(indxRegion,6))
        UrbLand%iColReuseFrac(iRegion)     = INT(DummyArray(indxRegion,7))
    END DO
    
    !Urban surface flow destinations
    CALL ReadRealData(UrbanDataFile,'surface flow destinations for urban lands','subregions',NElements,3,iElemIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    lProcessed_Elem = .FALSE.
    DO indxElem=1,NElements
        iElem = INT(DummyArray(indxElem,1))
        ID    = iElemIDs(iElem)
        IF (lProcessed_Elem(iElem)) THEN
            CALL SetLastMessage('Surface flow destination for urban lands for element '//TRIM(IntToText(ID))//' is defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed_Elem(iElem)     = .TRUE.
        SurfaceFlowDestType(iElem) = INT(DummyArray(indxElem,2))
        SurfaceFlowDest(iElem)     = INT(DummyArray(indxElem,3))
    
        !Make sure destination types and destinations are recognized
        IF (SurfaceFlowDestType(iElem) .NE. f_iFlowDest_Outside    .AND.   &
            SurfaceFlowDestType(iElem) .NE. f_iFlowDest_StrmNode   .AND.   &
            SurfaceFlowDestType(iElem) .NE. f_iFlowDest_Lake       .AND.   &
            SurfaceFlowDestType(iElem) .NE. f_iFlowDest_Subregion  .AND.   &
            SurfaceFlowDestType(iElem) .NE. f_iFlowDest_GWElement       )  THEN
            CALL SetLastMessage('Surface flow destination type for urban surface runoff at element ' // TRIM(IntToText(ID)) // ' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
            
        SELECT CASE (SurfaceFlowDestType(iElem))               
            CASE (f_iFlowDest_StrmNode)
                IF (PRESENT(iStrmNodeIDs)) THEN
                    CALL ConvertID_To_Index(SurfaceFlowDest(iElem),iStrmNodeIDs,iStrmNode)
                    IF (iStrmNode .EQ. 0) THEN
                        CALL SetLastMessage('Urban surface flow from element '//TRIM(IntToText(ID))//' flows into a stream node ('//TRIM(IntToText(SurfaceFlowDest(iElem)))//') that is not modeled!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    SurfaceFlowDest(iElem) = iStrmNode
                END IF
                
            CASE (f_iFlowDest_Lake)
                IF (PRESENT(iLakeIDs)) THEN
                    CALL ConvertID_To_Index(SurfaceFlowDest(iElem),iLakeIDs,iLake)
                    IF (iStrmNode .EQ. 0) THEN
                        CALL SetLastMessage('Urban surface flow from element '//TRIM(IntToText(ID))//' flows into a lake ('//TRIM(IntToText(SurfaceFlowDest(iElem)))//') that is not modeled!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    SurfaceFlowDest(iElem) = iLake
                END IF
          
            CASE (f_iFlowDest_Subregion)
                CALL ConvertID_To_Index(SurfaceFlowDest(iElem),iSubregionIDs,iRegion)
                IF (iRegion .EQ. 0) THEN
                    CALL SetLastMessage('Urban surface flow from element '//TRIM(IntToText(ID))//' goes to a subregion ('//TRIM(IntToText(SurfaceFlowDest(iElem)))//') that is not modeled!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                SurfaceFlowDest(iElem) = iRegion
                IF (SurfaceFlowDest(iElem) .EQ. AppGrid%AppElement(iElem)%Subregion) THEN
                    CALL SetLastMessage('Urban surface flow from element '//TRIM(IntToText(ID))//' cannot go to the same subregion which the element belongs to!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
        END SELECT              
    END DO
    
    !Compile element-flow-to-outside connection list
    CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_Outside,SurfaceFlowDest,SurfaceFlowDestType,ElemToOutside,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (UrbLand%ElemToOutside(SIZE(ElemToOutside)))
    UrbLand%ElemToOutside = ElemToOutside%iElement
    
    !Compile element-flow-to-stream-node connection list
    CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_StrmNode,SurfaceFlowDest,SurfaceFlowDestType,UrbLand%ElemToStreams,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Compile element-flow-to-lake connection list
    CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_Lake,SurfaceFlowDest,SurfaceFlowDestType,UrbLand%ElemToLakes,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Compile element-flow-to-subregion connection list
    CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_Subregion,SurfaceFlowDest,SurfaceFlowDestType,UrbLand%ElemToSubregions,iStat)  
    IF (iStat .EQ. -1) RETURN

    !Compile element-flow-to-groundwater connection list
    CALL CompileElemSurfaceFlowToDestinationList(f_iFlowDest_GWElement,SurfaceFlowDest,SurfaceFlowDestType,ElemToGW,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (UrbLand%ElemToGW(SIZE(ElemToGW)))
    UrbLand%ElemToGW = ElemToGW%iElement
    
    !Initial conditions
    !------------------
    CALL ReadRealData(UrbanDataFile,'urban root zone initial conditions','subregions',NSubregions,2*NSoils+1,iSubregionIDs,DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Make sure fractions due to precipitation are between 0 and 1
    IF (MINVAL(DummyArray(:,2::2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,2::2)) .GT. 1.0         ) THEN
        MessageArray(1) = 'Some fractions of initial soil moisture due to precipitation is less '
        MessageArray(2) = 'than 0.0 or greater than 1.0 for urban areas!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)      
        iStat = -1
        RETURN
    END IF 
        
    !Make sure initial moisture contents are between 0 and 1 
    IF (MINVAL(DummyArray(:,3::2)) .LT. 0.0   .OR.  &
        MAXVAL(DummyArray(:,3::2)) .GT. 1.0          ) THEN
        MessageArray(1) = 'Some or all initial root zone moisture contents are less than'
        MessageArray(2) = '0.0 or greater than 1.0 for urban areas!'
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
            CALL SetLastMessage('Initial conditions for urban lands for subregion '//TRIM(IntToText(ID))//' are defined more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iRegion)                                    = .TRUE.
        UrbLand%UrbData(:,iRegion)%SoilM_Precip                = DummyArray(indxRegion,2::2) * DummyArray(indxRegion,3::2)
        UrbLand%UrbData(:,iRegion)%SoilM_AW                    = (1d0 - DummyArray(indxRegion,2::2)) * DummyArray(indxRegion,3::2)
        UrbLand%UrbData(:,iRegion)%SoilM_Precip_P              = UrbLand%UrbData(:,iRegion)%SoilM_Precip 
        UrbLand%UrbData(:,iRegion)%SoilM_AW_P                  = UrbLand%UrbData(:,iRegion)%SoilM_AW
        UrbLand%UrbData(:,iRegion)%SoilM_Precip_P_BeforeUpdate = UrbLand%UrbData(:,iRegion)%SoilM_Precip 
        UrbLand%UrbData(:,iRegion)%SoilM_AW_P_BeforeUpdate     = UrbLand%UrbData(:,iRegion)%SoilM_AW
    END DO
    
    !Close file
    CALL UrbanDataFile%Kill()
    
    !Clear memeory
    DEALLOCATE (DummyArray , ElemToGW , ElemToOutside , STAT=ErrorCode)

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
  ! --- KILL URBAN LAND USE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(UrbLand)
    CLASS(UrbanDatabase_v50_Type) :: UrbLand

    !Local variables
    INTEGER                       :: ErrorCode
    TYPE(UrbanDatabase_v50_Type) :: DefaultUrbLand
    
    !Deallocate arrays
    DEALLOCATE (UrbLand%UrbData             , &
                UrbLand%iColReturnFrac      , &
                UrbLand%iColReuseFrac       , &
                UrbLand%iColWaterDemand     , &
                UrbLand%iColWaterUSeSpec    , &
                UrbLand%ElementalArea       , &
                UrbLand%ElementalArea_P     , &
                UrbLand%SubregionalArea     , &
                UrbLand%Demand              , &
                UrbLand%PerviousFrac        , &
                UrbLand%ElemToOutside       , &
                UrbLand%ElemToStreams       , &
                UrbLand%ElemToLakes         , &
                UrbLand%ElemToSubregions    , &
                UrbLand%ElemToGW            , &
                UrbLand%RegionETPot         , &
                STAT = ErrorCode            )
    
    !Close files
    CALL UrbLand%LandUseDataFile%Kill()
    CALL UrbLand%WaterDemandFile%Close()
    CALL UrbLand%WaterUseSpecsFile%Close()
    
    !Assign default values to components
    SELECT TYPE (UrbLand)
        TYPE IS (UrbanDatabase_v50_Type)
            UrbLAnd = DefaultUrbLand
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
  SUBROUTINE GetMaxAndMinNetReturnFlowFrac(UrbLand,ReturnFracFile,ReuseFracFile,FirstTimeStep,rMaxFrac,rMinFrac,iStat)
    CLASS(UrbanDatabase_v50_Type),INTENT(IN) :: UrbLand
    TYPE(RealTSDataInFileType)               :: ReturnFracFile,ReuseFracFile
    TYPE(TimeStepType),INTENT(IN)            :: FirstTimeStep
    REAL(8),INTENT(OUT)                      :: rMaxFrac,rMinFrac
    INTEGER,INTENT(OUT)                      :: iStat
    
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
        CALL Package_Misc_ReadTSData(TimeStep,'Return flow fractions data',ReturnFracFile,FileReadCode_Return,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL Package_Misc_ReadTSData(TimeStep,'Reuse fractions data',ReuseFracFile,FileReadCode_Reuse,iStat)          ;  IF (iStat .EQ. -1) RETURN
        
        !If new data is read, find min and max
        IF (FileReadCode_Return.EQ.0  .OR.  FileReadCode_Reuse.EQ.0) THEN
            DO indx=1,SIZE(UrbLand%iColReturnFrac)
                rRT      = ReturnFracFile%rValues(UrbLand%iColReturnFrac(indx))
                rRU      = ReuseFracFile%rValues(UrbLand%iColReuseFrac(indx))
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
  SUBROUTINE SetAreas(UrbLand,AppGrid,iSoilType,AreaElem)
    CLASS(UrbanDatabase_v50_Type) :: UrbLand
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(IN)            :: iSoilType(:)
    REAL(8),INTENT(IN)            :: AreaElem(:)
   
    !Local variables
    INTEGER :: indxElem,iSoil,iRegion
    
    !Initialize
    UrbLand%UrbData%Area = 0.0
    
    !Set elemental areas
    UrbLand%ElementalArea = AreaElem
    
    !Calulcuate areas for (soil,region) combinations
    DO indxElem=1,AppGrid%NElements
        iRegion                             = AppGrid%AppElement(indxElem)%Subregion
        iSoil                               = iSoilType(indxElem)
        UrbLand%UrbData(iSoil,iRegion)%Area = UrbLand%UrbData(iSoil,iRegion)%Area + AreaElem(indxElem)
    END DO
    
    !Update subregional areas
    UrbLand%SubregionalArea = SUM(UrbLand%UrbData%Area , DIM=1)
    
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
  SUBROUTINE ReadRestartData(UrbanLand,InFile,iStat)
    CLASS(UrbanDatabase_v50_Type) :: UrbanLand
    TYPE(GenericFileType)         :: InFile
    INTEGER,INTENT(OUT)           :: iStat
    
    CALL InFile%ReadData(UrbanLand%UrbData%Runoff,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%ReturnFlow,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%Area,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%Area_P,iStat)          ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Precip_P,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Precip,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_AW_P,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_AW,iStat)        ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Oth_P,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%UrbData%SoilM_Oth,iStat)       ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%ElementalArea,iStat)           ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(UrbanLand%ElementalArea_P,iStat)  
    
  END SUBROUTINE ReadRestartData

    
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES DATA FOR URBAN LANDS
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(UrbanLand,ElemSoilTypes,iSubregionIDs,rRegionAreas,lLakeElem,TimeStep,AppGrid,iStat)
    CLASS(UrbanDataBase_v50_Type) :: UrbanLand
    INTEGER,INTENT(IN)            :: ElemSoilTypes(:),iSubregionIDs(:)
    REAL(8),INTENT(IN)            :: rRegionAreas(:)
    LOGICAL,INTENT(IN)            :: lLakeElem(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'ReadTSData'
    INTEGER                      :: indxElem,iSoil,iRegion,indxRegion,iCol,FileErrorCode
    
    !Initialize
    iStat = 0

    !Echo progress
    CALL EchoProgress('Reading time series data for urban lands')
    
    !Land use areas
    CALL UrbanLand%LandUseDataFile%ReadTSData('Urban areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (UrbanLand%LandUseDataFile%lUpdated) THEN
        ASSOCIATE (pArea     => UrbanLand%UrbData%Area  , &
                   pElemArea => UrbanLand%ElementalArea )
            pArea = 0.0
            DO indxElem=1,AppGrid%NElements
                IF (lLakeElem(indxElem)) CYCLE
                iSoil                = ElemSoilTypes(indxElem)
                iRegion              = AppGrid%AppElement(indxElem)%Subregion
                pElemArea(indxElem)  = UrbanLand%LandUseDataFile%rValues(indxElem,2)
                pArea(iSoil,iRegion) = pArea(iSoil,iRegion) + pElemArea(indxElem)
            END DO
            
            !Update subregional areas
            UrbanLand%SubregionalArea = SUM(pArea , DIM=1)
        END ASSOCIATE        
    END IF
    
    !Water demand
    CALL Package_Misc_ReadTSData(TimeStep,'water demand data',UrbanLand%WaterDemandFile,FileErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (UrbanLand%WaterDemandFile%lUpdated) THEN
        UrbanLand%Demand = UrbanLand%WaterDemandFile%rValues(UrbanLand%iColWaterDemand) * UrbanLand%DemandConversionFactor
        !Make sure urban area is non-zero if demand is non-zero
        DO indxRegion=1,AppGrid%NSubregions
            IF (UrbanLand%Demand(indxRegion) .GT. 0.0) THEN
                IF (UrbanLand%SubregionalArea(indxRegion) .EQ. 0.0) THEN
                    MessageArray(1) = 'Urban area in subregion '//TRIM(IntTotext(iSubregionIDs(indxRegion)))// ' is zero.'
                    MessageArray(2) = 'Setting urban water demand at this subregion to zero!' 
                    CALL LogMessage (MessageArray(1:2),f_iInfo,ThisProcedure)
                    UrbanLand%Demand(indxRegion) = 0.0
                END IF
            END If
        END DO
    END IF
    
    !Water use specifications
    CALL Package_Misc_ReadTSData(TimeStep,'Urban water use specifications',UrbanLand%WaterUseSpecsFile,FileErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
    !Make sure indoors water use fractions and indoors area are consistent
    IF (UrbanLand%WaterUseSpecsFile%lUpdated) THEN
        DO indxRegion=1,AppGrid%NSubregions
            iCol = UrbanLand%iColWaterUseSpec(indxRegion)
            !Make sure water use fraction is between 0 and 1
            IF (UrbanLand%WaterUseSpecsFile%rValues(iCol).GT.1.0   .OR.  UrbanLand%WaterUseSpecsFile%rValues(iCol).LT.0.0) THEN
                WRITE(MessageArray(1),'(A,F4.1,A)') 'Urban indoor water use fraction at subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//' is specified as ',UrbanLand%WaterUseSpecsFile%rValues(iCol),'!'
                MessageArray(2) = 'It must be between 0.0 and 1.0.'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Make sure urban indoor or outdoor water is zero if respective area is zero
            IF (UrbanLand%PerviousFrac(indxRegion) .EQ. 0.0) THEN
                IF (UrbanLand%WaterUseSpecsFile%rValues(iCol) .NE. 1.0) THEN
                    MessageArray(1) = 'Urban outdoors applied water fraction in subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//' is not zero'
                    MessageArray(2) = 'when the urban outdoors area is zero. Adjusting the water use fraction accordingly!'
                    CALL LogMessage(MessageArray(1:2),f_iInfo,ThisProcedure)
                    UrbanLand%WaterUseSpecsFile%rValues(iCol) = 1.0
                END IF
            ELSEIF (UrbanLand%PerviousFrac(indxRegion) .EQ. 1.0) THEN
                IF (UrbanLand%WaterUseSpecsFile%rValues(iCol) .NE. 0.0) THEN
                    MessageArray(1) = 'Urban indoors water fraction in subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//' is not zero'
                    MessageArray(2) = 'when the urban indoors area is zero. Adjusting the water use fraction accordingly!'
                    CALL LogMessage(MessageArray(1:2),f_iInfo,ThisProcedure)
                    UrbanLand%WaterUseSpecsFile%rValues(iCol) = 0.0
                END IF
            END IF
        END DO
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
  SUBROUTINE PrintRestartData(UrbanLand,OutFile)
    CLASS(UrbanDatabase_v50_Type),INTENT(IN) :: UrbanLand
    TYPE(GenericFileType)                    :: OutFile
    
    CALL OutFile%WriteData(UrbanLand%UrbData%Runoff)
    CALL OutFile%WriteData(UrbanLand%UrbData%ReturnFlow)
    CALL OutFile%WriteData(UrbanLand%UrbData%Area)
    CALL OutFile%WriteData(UrbanLand%UrbData%Area_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Precip_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Precip)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_AW_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_AW)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Oth_P)
    CALL OutFile%WriteData(UrbanLand%UrbData%SoilM_Oth)
    CALL OutFile%WriteData(UrbanLand%ElementalArea)
    CALL OutFile%WriteData(UrbanLand%ElementalArea_P)
    
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
  ! --- CONVERT SOIL INITIAL MOISTURE CONTENTS TO DEPTHS
  ! ---  Note: Called only once at the beginning of simulation
  ! -------------------------------------------------------------
  SUBROUTINE SoilMContent_To_Depth(UrbanLand,NSoils,NRegions,iSubregionIDs,TotalPorosity,iStat)
    CLASS(UrbanDatabase_v50_Type) :: UrbanLand
    INTEGER,INTENT(IN)            :: NSoils,NRegions,iSubregionIDs(NRegions)
    REAL(8),INTENT(IN)            :: TotalPorosity(NSoils,NRegions)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SoilMContent_To_Depth'
    INTEGER                      :: indxRegion,indxSoil
    REAL(8)                      :: RootDepth
    
    !Initialzie
    iStat = 0
    
    !Return if urban lands are not simulated
    IF (SIZE(UrbanLand%UrbData) .EQ. 0) RETURN
    
    !Initialize
    RootDepth = UrbanLand%RootDepth
    
    !Check if initial conditions are greater than total porosity, if not convert contents to depths and equate SoilM_P to SoilM
    ASSOCIATE (pUrbData => UrbanLand%UrbData) 
        DO indxRegion=1,NRegions
            DO indxSoil=1,NSoils
                IF ((pUrbData(indxSoil,indxRegion)%SoilM_Precip + pUrbData(indxSoil,indxRegion)%SoilM_AW + pUrbData(indxSoil,indxRegion)%SoilM_Oth) .GT. TotalPorosity(indxSoil,indxRegion)) THEN
                    CALL SetLastMessage('Initial moisture content for urban land with soil type ' // TRIM(IntToText(indxSoil)) // ' at subregion ' // TRIM(IntToText(iSubregionIDs(indxRegion))) // ' is greater than total porosity!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                pUrbData(indxSoil,indxRegion)%SoilM_Precip   = pUrbData(indxSoil,indxRegion)%SoilM_Precip * RootDepth
                pUrbData(indxSoil,indxRegion)%SoilM_AW       = pUrbData(indxSoil,indxRegion)%SoilM_AW * RootDepth
                pUrbData(indxSoil,indxRegion)%SoilM_Oth      = pUrbData(indxSoil,indxRegion)%SoilM_Oth * RootDepth
                pUrbData(indxSoil,indxRegion)%SoilM_Precip_P = pUrbData(indxSoil,indxRegion)%SoilM_Precip
                pUrbData(indxSoil,indxRegion)%SoilM_AW_P     = pUrbData(indxSoil,indxRegion)%SoilM_AW
                pUrbData(indxSoil,indxRegion)%SoilM_Oth_P    = pUrbData(indxSoil,indxRegion)%SoilM_Oth
            END DO
        END DO 
    END ASSOCIATE
    
  END SUBROUTINE SoilMContent_To_Depth


  ! -------------------------------------------------------------
  ! --- ADVANCE AREAS IN TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceAreas(UrbanLand) 
    CLASS(UrbanDatabase_v50_Type) :: UrbanLand
    
    UrbanLand%ElementalArea_P = UrbanLand%ElementalArea
    UrbanLand%UrbData%Area_P  = UrbanLand%UrbData%Area
    
  END SUBROUTINE AdvanceAreas

  
  ! -------------------------------------------------------------
  ! --- SIMULATE FLOW PROCESSES AT URBAN AREAS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(UrbanLand,ETData,iSubregionIDs,DeltaT,Precip,GenericMoisture,SubregionSoilsData,WaterSupply,ReuseFrac,ReturnFrac,SolverData,iStat)
    CLASS(UrbanDatabase_v50_Type)     :: UrbanLand
    TYPE(ETType),INTENT(IN)           :: ETData
    INTEGER,INTENT(IN)                :: iSubregionIDs(:)
    REAL(8),INTENT(IN)                :: DeltaT,Precip(:,:),GenericMoisture(:,:),WaterSupply(:),ReuseFrac(:),ReturnFrac(:)
    TYPE(RootZoneSoilType),INTENT(IN) :: SubregionSoilsData(:,:)
    TYPE(SolverDataType),INTENT(IN)   :: SolverData
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8) :: ThisProcedure = ModName // 'Simulate'
    INTEGER                     :: indxRegion,indxSoil,iColETc(1),KunsatMethod
    REAL(8)                     :: AchievedConv,Area,ETc(1),HydCond,TotalPorosity,Area_Indoors,Area_Outdoors, &
                                   FieldCapacity,TotalPorosityUrban,FieldCapacityUrban,RootDepth,Lambda,      &
                                   AW_Outdoors,AW_Indoors,WiltingPoint,WiltingPointUrban,SoilM_P,SoilM,fRU,   &
                                   fRF,GM,PrecipD,Excess,Inflow,Supply,ratio(3),SoilM_P_Array(3),Infilt(3),   &
                                   SoilM_Array(3),ETPartition(3),PerviousFrac,rFactor
    LOGICAL                     :: lNegativeMoist
    
    !Initialize
    iStat = 0
  
    !Inform user
    CALL EchoProgress('Simulating flows at urban lands')
    
    !Initialize
    RootDepth = UrbanLand%RootDepth   
    
    DO indxRegion=1,SIZE(UrbanLand%UrbData,DIM=2)
        iColETc(1)   = UrbanLand%UrbData(1,indxRegion)%iColETc  !ETc column pointers are the same for all soils in a subregion
        ETc          = ETData%GetValues(iColETc)       !ETc values are the same for all soils in a subregion
        PerviousFrac = UrbanLand%PerviousFrac(indxRegion)
        IF (PerviousFrac .EQ. 1.0) THEN
            AW_Indoors  = 0.0
            AW_Outdoors = WaterSupply(indxRegion)
        ELSEIF (PerviousFrac .EQ. 0.0) THEN
            AW_Indoors  = WaterSupply(indxRegion)
            AW_Outdoors = 0.0
        ELSE
            AW_Indoors  = WaterSupply(indxRegion) * UrbanLand%WaterUseSpecsFile%rValues(UrbanLand%iColWaterUseSpec(indxRegion))
            AW_Outdoors = WaterSupply(indxRegion) - AW_Indoors
        END IF
        Supply = AW_Outdoors * DeltaT
        
        !Infiltration and return flow due to applied water
        fRF                                        = ReturnFrac(UrbanLand%iColReturnFrac(indxRegion))
        fRU                                        = ReuseFrac(UrbanLand%iColReuseFrac(indxRegion))
        UrbanLand%UrbData(:,indxRegion)%IrigInfilt = MIN(Supply*(1d0-(fRF-fRU)) , Supply)
        UrbanLand%UrbData(:,indxRegion)%ReturnFlow = Supply - UrbanLand%UrbData(:,indxRegion)%IrigInfilt          

        DO indxSoil=1,SIZE(UrbanLand%UrbData,DIM=1)
            !Cycle if Area is zero
            Area = UrbanLand%UrbData(indxSoil,indxRegion)%Area
            IF (Area .EQ. 0.0) THEN
                UrbanLand%UrbData(indxSoil,indxRegion)%Runoff       = 0.0
                UrbanLand%UrbData(indxSoil,indxRegion)%PrecipInfilt = 0.0                     
                UrbanLand%UrbData(indxSoil,indxRegion)%ETa          = 0.0                     
                UrbanLand%UrbData(indxSoil,indxRegion)%Perc         = 0.0                    
                UrbanLand%UrbData(indxSoil,indxRegion)%ReturnFlow   = 0.0
                UrbanLand%UrbData(indxSoil,indxRegion)%IrigInfilt   = 0.0  
                UrbanLand%UrbData(indxSoil,indxRegion)%GMExcess     = 0.0
                UrbanLand%UrbData(indxSoil,indxRegion)%Reuse        = 0.0
                CYCLE
            END IF
            
            !Soil parameters
            WiltingPoint  = SubregionSoilsData(indxSoil,indxRegion)%WiltingPoint
            FieldCapacity = SubregionSoilsData(indxSoil,indxRegion)%FieldCapacity
            TotalPorosity = SubregionSoilsData(indxSoil,indxRegion)%TotalPorosity
            HydCond       = SubregionSoilsData(indxSoil,indxRegion)%HydCond
            Lambda        = SubregionSoilsData(indxSoil,indxRegion)%Lambda
            KunsatMethod  = SubregionSoilsData(indxSoil,indxRegion)%KunsatMethod
            PrecipD       = Precip(indxSoil,indxRegion) * DeltaT
            GM            = GenericMoisture(indxSoil,indxRegion) * RootDepth * DeltaT
            ASSOCIATE (pUrban => UrbanLand%UrbData(indxSoil,indxRegion))          
                !Initialize
                Area_Outdoors      = Area * PerviousFrac
                Area_Indoors       = Area - Area_Outdoors 
                WiltingPointUrban  = WiltingPoint  * RootDepth
                FieldCapacityUrban = FieldCapacity * RootDepth
                TotalPorosityUrban = TotalPorosity * RootDepth
                SoilM_P            = pUrban%SoilM_Precip_P + pUrban%SoilM_AW_P + pUrban%SoilM_Oth_P
      
                !Total inflow to the root zone
                Inflow = GM + pUrban%IrigInfilt
        
                !Simulate
                CALL NonPondedLUMoistureRouter(PrecipD                                 ,  &
                                               pUrban%SMax                             ,  &
                                               SoilM_P                                 ,  &
                                               ETc(1)*DeltaT                           ,  & 
                                               HydCond                                 ,  & 
                                               TotalPorosityUrban                      ,  & 
                                               FieldCapacityUrban                      ,  & 
                                               WiltingPointUrban                       ,  &
                                               Lambda                                  ,  & 
                                               Inflow                                  ,  &
                                               SolverData%Tolerance*TotalPorosityUrban ,  &
                                               KunsatMethod                            ,  &
                                               SolverData%IterMax                      ,  &
                                               SoilM                                   ,  & 
                                               pUrban%Runoff                           ,  & 
                                               pUrban%PrecipInfilt                     ,  & 
                                               pUrban%ETa                              ,  & 
                                               pUrban%Perc                             ,  & 
                                               Excess                                  ,  &
                                               AchievedConv                            ) 
 
                !Generate error if convergence is not achieved
                IF (AchievedConv .NE. 0.0) THEN
                    MessageArray(1) = 'Convergence error in soil moisture routing for urban lands!'
                    MessageArray(2) =                   'Soil type            = '//TRIM(IntToText(indxSoil))
                    MessageArray(3) =                   'Subregion            = '//TRIM(IntToText(iSubregionIDs(indxRegion)))
                    WRITE (MessageArray(4),'(A,F11.8)') 'Desired convergence  = ',SolverData%Tolerance*TotalPorosityUrban
                    WRITE (MessageArray(5),'(A,F11.8)') 'Achieved convergence = ',ABS(AchievedConv)
                    CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
 
                !Reduce inflows based on correction for total porosity
                IF (Excess .NE. 0.0) THEN
                    ratio = [pUrban%PrecipInfilt , pUrban%IrigInfilt , GM]
                    CALL NormalizeArray(ratio)
                    pUrban%Runoff       = pUrban%Runoff     + Excess * ratio(1) 
                    pUrban%ReturnFlow   = pUrban%ReturnFlow + Excess * ratio(2)
                    pUrban%GMExcess     = Excess * ratio(3)
                    pUrban%PrecipInfilt = PrecipD - pUrban%Runoff
                    pUrban%IrigInfilt   = Supply  - pUrban%ReturnFlow
                END IF
      
                !Compute re-use based on return flow
                IF (fRF .GT. 0.0) pUrban%Reuse = pUrban%ReturnFlow * fRU / fRF
        
                !Compute moisture from precip and irrigation
                SoilM_P_Array = [pUrban%SoilM_Precip_P , pUrban%SoilM_AW_P , pUrban%SoilM_Oth_P  ]
                Infilt        = [pUrban%PrecipInfilt   , pUrban%IrigInfilt , GM - pUrban%GMExcess]
                CALL TrackMoistureDueToSource(SoilM_P_Array   ,  &
                                              Infilt          ,  &
                                              pUrban%Perc     ,  &
                                              pUrban%ETa      ,  &
                                              0d0             ,  &
                                              SoilM_Array     ,  &
                                              ETPartition     )
                pUrban%SoilM_Precip = SoilM_Array(1)
                pUrban%SoilM_AW     = SoilM_Array(2)
                pUrban%SoilM_Oth    = SoilM_Array(3)
 
                !Make sure soil moisture is not less than zero
                lNegativeMoist = .FALSE.
                IF (pUrban%SoilM_Precip .LT. 0.0) lNegativeMoist = .TRUE.
                IF (pUrban%SoilM_AW     .LT. 0.0) lNegativeMoist = .TRUE.
                IF (pUrban%SoilM_Oth    .LT. 0.0) lNegativeMoist = .TRUE.
                IF (lNegativeMoist) THEN
                    MessageArray(1) = 'Soil moisture content becomes negative at subregion '//TRIM(IntToText(iSubregionIDs(indxRegion)))//'.'
                    MessageArray(2) = 'This may be due to a too high convergence criteria set for the iterative solution.'
                    MessageArray(3) = 'Try using a smaller value for RZCONV and a higher value for RZITERMX parameters'
                    MessageArray(4) = 'in the Root Zone Main Input File.'
                    CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
 
                !Combine indoor and outdoor values as needed and spread the values to the entire urban area (indoors + outdoors)
                rFactor             = Area_Outdoors / Area
                pUrban%Runoff       = (pUrban%Runoff     * Area_Outdoors + PrecipD    * Area_Indoors) / Area
                pUrban%ReturnFlow   = (pUrban%ReturnFlow * Area_Outdoors + AW_Indoors * Area_Indoors) / Area
                pUrban%Reuse        = pUrban%Reuse * rFactor
                pUrban%PrecipInfilt = pUrban%PrecipInfilt * rFactor
                pUrban%IrigInfilt   = pUrban%IrigInfilt * rFactor
                pUrban%ETa          = pUrban%ETa * rFactor
                pUrban%Perc         = pUrban%Perc * rFactor
                          
            END ASSOCIATE
        END DO
    END DO
    
  END SUBROUTINE Simulate

  
  ! -------------------------------------------------------------
  ! --- REWIND TIMESERIES INPUT FILES TO A SPECIFED TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE RewindTSInputFilesToTimeStamp(UrbanLand,iSubregionIDs,rRegionAreas,TimeStep,iStat)
    CLASS(UrbanDatabase_v50_Type) :: UrbanLand
    INTEGER,INTENT(IN)            :: iSubregionIDs(:)
    REAL(8),INTENT(IN)            :: rRegionAreas(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep 
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    CALL UrbanLand%LandUseDataFile%File%RewindFile_To_BeginningOfTSData(iStat)                          ;  IF (iStat .NE. 0) RETURN
    CALL UrbanLand%LandUseDataFile%ReadTSData('Urban areas',TimeStep,rRegionAreas,iSubregionIDs,iStat)  ;  IF (iStat .NE. 0) RETURN

    CALL UrbanLand%WaterDemandFile%File%RewindFile_To_BeginningOfTSData(iStat)                                ;  IF (iStat .NE. 0) RETURN
    CALL Package_Misc_ReadTSData(TimeStep,'water demand data',UrbanLand%WaterDemandFile,iFileReadCode,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    CALL UrbanLand%WaterUseSpecsFile%File%RewindFile_To_BeginningOfTSData(iStat)                                              ;  IF (iStat .NE. 0) RETURN
    CALL Package_Misc_ReadTSData(TimeStep,'Urban water use specifications',UrbanLand%WaterUseSpecsFile,iFileReadCode,iStat)  
    
  END SUBROUTINE RewindTSInputFilesToTimeStamp

END MODULE