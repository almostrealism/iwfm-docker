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
MODULE SupplyAdjustment
  USE MessageLogger               , ONLY: SetLastMessage                 , &
                                          EchoProgress                   , &
                                          MessageArray                   , &
                                          f_iFatal
  USE GeneralUtilities            , ONLY: NormalizeArray
  USE TimeSeriesUtilities         , ONLY: TimeStepType
  USE Package_ComponentConnectors , ONLY: SupplyToDestinationType        , &
                                          DestinationToSupplyType        , &
                                          SupplyDestinationConnectorType
  USE Package_Misc                , ONLY: IntTSDataInFileType            , &
                                          ReadTSData                     , &
                                          f_iSupply_Diversion            , &
                                          f_iSupply_ElemPump             , &
                                          f_iSupply_Well                 , &
                                          f_iFlowDest_Element            , &
                                          f_iAg                          , &
                                          f_iUrb
  USE Package_AppGW               , ONLY: AppGWType                      , &
                                          f_iPump_Well                   , &
                                          f_iPump_ElemPump
  USE Package_AppStream           , ONLY: AppStreamType
  USE Package_RootZone            , ONLY: RootZoneType
  USE Package_Discretization      , ONLY: AppGridType                    , &
                                          StratigraphyType
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
  PUBLIC :: SupplyAdjustmentType                       ,  &
            f_iAdjustNone                              ,  &
            f_iAdjustPump                              ,  &
            f_iAdjustDiver                             ,  &
            f_iAdjustPumpDiver                         ,  &
            f_iAdjustForNone                           ,  &
            f_iAdjustForAg                             ,  &
            f_iAdjustForUrb                            ,  &
            f_iAdjustForAgUrb                            
            
  
  
  ! -------------------------------------------------------------
  ! --- ADJUSTMENT FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iAdjustNone      = 00  , &   !No adjustment
                       f_iAdjustPump      = 10  , &   !Adjust pumping only
                       f_iAdjustDiver     = 01  , &   !Adjust diversions only
                       f_iAdjustPumpDiver = 11  , &   !Adjust both diversion and pumping
                       f_iAdjustForNone   = 00  , &   !Do not adjust supply for ag or urban 
                       f_iAdjustForAg     = 10  , &   !Adjust supply for ag only
                       f_iAdjustForUrb    = 01  , &   !Adjust supply for urban only
                       f_iAdjustForAgUrb  = 11        !Adjust supply for ag and urban both
                       

  ! -------------------------------------------------------------
  ! --- SUPPLY ADJUSTMENT DATA TYPE
  ! -------------------------------------------------------------
  TYPE SupplyAdjustmentType
    PRIVATE
    INTEGER                   :: iAdjust               = f_iAdjustNone  !Will pumping, diversions or both will be adjusted
    INTEGER                   :: NAdjustLocations      = 0              !Number of demand locations (e.g. elements or subregions) for which supply will be adjusted
    INTEGER                   :: NMaxPumpAdjustIter    = 0              !Number of maximum supply adjustment iterations (only used for pumping; for diversions diversion rank is used to limit the iterations)
    INTEGER                   :: iWellPumpAdjustIter   = 0              !Current well pumping adjustment iteration 
    INTEGER                   :: iElemPumpAdjustIter   = 0              !Current element pumping adjustment iteration 
    INTEGER                   :: iDiverAdjustIter      = -1             !Current diversion adjustment iteration; set to -1 since smallest diversion rank is zero
    INTEGER                   :: iGlobalAdjustIter     = 1              !Current global iteration counting all pumping and diversion adjustment iterations
    REAL(8)                   :: Tolerance             = 0.0            !Tolerance as a fraction of demand that supply will be adjusted to (has to be a number between 0 and 1)
    LOGICAL                   :: lAdjust               = .FALSE.        !Flag to check further adjustment runs will be performed
    TYPE(IntTSDataInFileType) :: SpecFile                               !Supply adjustment specifications file
  CONTAINS
    PROCEDURE,PASS :: New  
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: IsAdjust                  
    PROCEDURE,PASS :: IsDiversionAdjusted       
    PROCEDURE,PASS :: IsPumpingAdjusted         
    PROCEDURE,PASS :: GetAdjustFlag             
    PROCEDURE,PASS :: GetAdjustIter
    PROCEDURE,PASS :: GetFileName
    PROCEDURE,PASS :: SetTolerance              
    PROCEDURE,PASS :: SetAdjustFlag             
    PROCEDURE,PASS :: SetMaxPumpAdjustIter      
    PROCEDURE,PASS :: ReadTSData           => SupplyAdjustment_ReadTSData      
    PROCEDURE,PASS :: Adjust                    
    PROCEDURE,PASS :: ResetState              
  END TYPE SupplyAdjustmentType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen      = 18
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName         = 'SupplyAdjustment::'
  
  
  
  
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
  ! --- NEW SUPPLY ADJUSTMENT DATA
  ! --- *** Note: Assumes iAdjust and NMaxPumpAdjustIter flags are already set
  ! -------------------------------------------------------------
  SUBROUTINE New(SupplyAdjustment,cFileName,cWorkingDirectory,NDemandLocations,TimeStep,iStat)
    CLASS(SupplyAdjustmentType)   :: SupplyAdjustment
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)            :: NDemandLocations
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    
    !Initialize
    iStat = 0
    
    !Always instantiate SupplyAdjustment object since it maybe modified later and info will be needed
    !IF (SupplyAdjustment%iAdjust .EQ. f_iAdjustNone) RETURN
    
    !Stop if no filename is given
    IF (cFileName .EQ. '') THEN
        MessageArray(1) = 'Supply adjustment specifications file must be specified'
        MessageArray(2) = ' when pumping or diversions are defined!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Instantiate data
    CALL SupplyAdjustment%SpecFile%Init(cFileName,cWorkingDirectory,'supply adjustment specifications file',TimeStep%TrackTime,BlocksToSkip=1,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Set number of locations for which supply adjustment will be performed
    SupplyAdjustment%NAdjustLocations = NDemandLocations
    
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
  ! --- KILL SUPPLY ADJUSTMENT DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(SupplyAdjust)
    CLASS(SupplyAdjustmentType) :: SupplyAdjust
    
    !Local variables
    TYPE(SupplyAdjustmentType) :: DummyData
    
    !Set atrributes to their default values
    SupplyAdjust%iAdjust             = DummyData%iAdjust
    SupplyAdjust%NAdjustLocations    = DummyData%NAdjustLocations    
    SupplyAdjust%NMaxPumpAdjustIter  = DummyData%NMaxPumpAdjustIter  
    SupplyAdjust%iWellPumpAdjustIter = DummyData%iWellPumpAdjustIter 
    SupplyAdjust%iElemPumpAdjustIter = DummyData%iElemPumpAdjustIter 
    SupplyAdjust%iDiverAdjustIter    = DummyData%iDiverAdjustIter    
    SupplyAdjust%iGlobalAdjustIter   = DummyData%iGlobalAdjustIter   
    SupplyAdjust%Tolerance           = DummyData%Tolerance           
    SupplyAdjust%lAdjust             = DummyData%lAdjust             
    
    !Close the attached time-series data
    CALL SupplyAdjust%SpecFile%Close()
        
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
  ! --- GET SUPPLY ADJUSTMENT FLAGS FILENAME
  ! -------------------------------------------------------------
  SUBROUTINE GetFileName(SupplyAdjustment,cFileName)
    CLASS(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    CHARACTER(:),ALLOCATABLE,INTENT(OUT)   :: cFileName
    
    CALL SupplyAdjustment%SpecFile%GetFileName(cFileName)
    
  END SUBROUTINE GetFileName
  

  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAG
  ! -------------------------------------------------------------
  PURE FUNCTION GetAdjustFlag(SupplyAdjustment) RESULT(iFlag)
    CLASS(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    INTEGER                                :: iFlag
    
    iFlag = SupplyAdjustment%iAdjust
    
  END FUNCTION GetAdjustFlag
  

  ! -------------------------------------------------------------
  ! --- GET GLOBAL SUPPLY ADJUSTMENT ITERATION NUMBER
  ! -------------------------------------------------------------
  PURE FUNCTION GetAdjustIter(SupplyAdjustment) RESULT(iGlobalIter)
    CLASS(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    INTEGER                                :: iGlobalIter
    
    iGlobalIter = SupplyAdjustment%iGlobalAdjustIter
    
  END FUNCTION GetAdjustIter
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAG TO SEE IF FURTHER ADJUSTMENT RUNS ARE TO BE PERFORMED
  ! -------------------------------------------------------------
  PURE FUNCTION IsAdjust(SupplyAdjustment) RESULT(lAdjust)
    CLASS(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    LOGICAL                                :: lAdjust
    
    lAdjust = SupplyAdjustment%lAdjust
    
  END FUNCTION IsAdjust
  
  
  ! -------------------------------------------------------------
  ! --- ARE DIVERSIONS ADJUSTED?
  ! -------------------------------------------------------------
  PURE FUNCTION IsDiversionAdjusted(SupplyAdjustment) RESULT(lAdjust)
    CLASS(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    LOGICAL                                :: lAdjust
    
    IF (SupplyAdjustment%iAdjust.EQ.f_iAdjustDiver  .OR.  SupplyAdjustment%iAdjust.EQ.f_iAdjustPumpDiver) THEN
      lAdjust = .TRUE.
    ELSE
      lAdjust = .FALSE.
    END IF
    
  END FUNCTION IsDiversionAdjusted
  
  
  ! -------------------------------------------------------------
  ! --- IS PUMPING ADJUSTED?
  ! -------------------------------------------------------------
  PURE FUNCTION IsPumpingAdjusted(SupplyAdjustment) RESULT(lAdjust)
    CLASS(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    LOGICAL                                :: lAdjust
    
    IF (SupplyAdjustment%iAdjust.EQ.f_iAdjustPump  .OR.  SupplyAdjustment%iAdjust.EQ.f_iAdjustPumpDiver) THEN
      lAdjust = .TRUE.
    ELSE
      lAdjust = .FALSE.
    END IF
    
  END FUNCTION IsPumpingAdjusted
  
  
  
  
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
  ! --- SET SUPPLY ADJUSTMENT TOLERANCE
  ! -------------------------------------------------------------
  SUBROUTINE SetTolerance(SupplyAdjustment,Toler,iStat)
    REAL(8),INTENT(IN)          :: Toler
    CLASS(SupplyAdjustmentType) :: SupplyAdjustment
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12) :: ThisProcedure = ModName // 'SetTolerance'
    
    !Initialize
    iStat = 0
    
    !Make sure tolerance is set to a value between 0 and 1
    IF (Toler.GT.1.0  .OR. Toler.LT.0.0) THEN
        CALL SetLastMessage('Supply adjustment tolerance must be a value between 0 and 1',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Set the variable
    SupplyAdjustment%Tolerance = Toler
    
  END SUBROUTINE SetTolerance
  
  
  ! -------------------------------------------------------------
  ! --- SET MAXIMUM NUMBER OF ADJUSTMENTS FOR PUMPING
  ! -------------------------------------------------------------
  SUBROUTINE SetMaxPumpAdjustIter(SupplyAdjustment,iMaxIter,iStat)
    CLASS(SupplyAdjustmentType) :: SupplyAdjustment
    INTEGER,INTENT(IN)          :: iMaxIter
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'SetMaxPumpAdjustIter'
    
    !Initialize
    iStat = 0
    
    !Make sure that iMaxIter is not less than 1
    IF (iMaxIter .LE. 0) THEN
        CALL SetLastMessage('Maximum supply adjustment iteration number must be greater than zero!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SupplyAdjustment%NMaxPumpAdjustIter = iMaxIter
    
  END SUBROUTINE SetMaxPumpAdjustIter
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY ADJUSTMENT FLAG
  ! -------------------------------------------------------------
  SUBROUTINE SetAdjustFlag(SupplyAdjustment,iAdjust,iStat)
    CLASS(SupplyAdjustmentType) :: SupplyAdjustment
    INTEGER,INTENT(IN)          :: iAdjust
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'SetAdjustFlag'
    
    !Initialize
    iStat = 0
    
    !Make sure that adjustment flag is recognised
    IF (iAdjust .NE. f_iAdjustNone) THEN      
        IF (iAdjust .NE. f_iAdjustPump) THEN      
            IF (iAdjust .NE. f_iAdjustDiver) THEN
                IF (iAdjust .NE. f_iAdjustPumpDiver) THEN
                    CALL SetLastMessage('Supply adjustment flag to adjust pumping, diversions or both is not recognized!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END IF
        END IF
    END IF 
    
    SupplyAdjustment%iAdjust = iAdjust
    
    !Set the flag to check if adjustment runs will be performed
    IF (iAdjust .NE. f_iAdjustNone) THEN
        SupplyAdjustment%lAdjust = .TRUE.
    ELSE
        SupplyAdjustment%lAdjust = .FALSE.
    END IF
        
  END SUBROUTINE SetAdjustFlag
    
    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READER
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ SUPPLY ADJUSTMENT SPECS
  ! -------------------------------------------------------------
  SUBROUTINE SupplyAdjustment_ReadTSData(SupplyAdjustment,TimeStep,iStat)
    CLASS(SupplyAdjustmentType)   :: SupplyAdjustment
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    INTEGER :: FileReadCode
    
    !Return if no adjustment is asked for
    IF (SupplyAdjustment%iAdjust .EQ. f_iAdjustNone) THEN
        iStat = 0
        RETURN
    END IF

    !Read data
    CALL ReadTSData(TimeStep,'supply adjustment specs data',SupplyAdjustment%SpecFile,FileReadCode,iStat)

  END SUBROUTINE SupplyAdjustment_ReadTSData
  
  
  
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
  ! --- ADJUST SUPPLIES
  ! -------------------------------------------------------------
  SUBROUTINE Adjust(SupplyAdjustment,AppGrid,RootZone,AppGW,AppStream,DiverDestConnector,WellDestConnector,ElemPumpDestConnector)
    CLASS(SupplyAdjustmentType)          :: SupplyAdjustment
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    TYPE(RootZoneType),INTENT(IN)        :: RootZone
    TYPE(AppGWType)                      :: AppGW
    TYPE(AppStreamType)                  :: AppStream
    TYPE(SupplyDestinationConnectorType) :: DiverDestConnector,WellDestConnector,ElemPumpDestConnector
    
    !Local variables
    INTEGER                                              :: NDiver,NPump
    REAL(8),DIMENSION(SupplyAdjustment%NAdjustLocations) :: AgDiff,UrbDiff,AgDemand,UrbDemand,AgToler,UrbToler,AgSupply,UrbSupply
    LOGICAL                                              :: lAdjusted
    
    !Return if there is no need to adjust
    IF (SupplyAdjustment%iAdjust .EQ. f_iAdjustNone) RETURN
    
    !Initialize
    SupplyAdjustment%lAdjust = .TRUE.
    
    !Inform user
    CALL EchoProgress('Adjusting supplies to meet water demand')
    
    !Get water demands
    CALL RootZone%GetWaterDemand(f_iAg,AgDemand)
    CALL RootZone%GetWaterDemand(f_iUrb,UrbDemand)
    
    !Get water suppies
    CALL RootZone%GetWaterSupply(AppGrid,f_iAg,AgSupply)
    CALL RootZone%GetWaterSupply(AppGrid,f_iUrb,UrbSupply)
    
    !Supply shortage (demand-supply) at each demand location
    AgDiff  = AgDemand - AgSupply
    UrbDiff = UrbDemand - UrbSupply
    
    !Supply adjustment tolerance at each demand location
    AgToler  = AgDemand * SupplyAdjustment%Tolerance
    UrbToler = UrbDemand * SupplyAdjustment%Tolerance

    !First adjust diversions
    IF (IsDiversionsAdjustable(SupplyAdjustment,AppStream)) THEN
        NDiver = AppStream%GetNDiver()
        CALL AdjustDiversions(AppStream                          , &
                              DiverDestConnector                 , &
                              NDiver                             , &
                              AgToler                            , &
                              UrbToler                           , &
                              AgDiff                             , &
                              UrbDiff                            , &
                              SupplyAdjustment%SpecFile%iValues  , &
                              SupplyAdjustment%iGlobalAdjustIter , &
                              SupplyAdjustment%iDiverAdjustIter  , &
                              lAdjusted                          )
                            
        !If nothing is adjusted, is it possible to adjust other supplies?
        IF (.NOT. lAdjusted) THEN
            SupplyAdjustment%iDiverAdjustIter = AppStream%GetMaxDiversionRank() + 1
            IF (.NOT. IsMoreAdjustmentPossible(SupplyAdjustment,AppGW,AppStream)) THEN
                SupplyAdjustment%lAdjust = .FALSE.
                RETURN
            END IF
        ELSE
            RETURN
      END IF
      
    END IF
      
    !Then adjust wells
    IF (IsPumpingAdjustable(SupplyAdjustment,AppGW,f_iSupply_Well)) THEN
        NPump = AppGW%GetNWells()
        CALL AdjustPumping(AppGW                                 , &
                           WellDestConnector                     , &
                           NPump                                 , &
                           f_iSupply_Well                        , &
                           AgToler                               , &
                           UrbToler                              , &
                           AgDiff                                , &
                           UrbDiff                               , &
                           SupplyAdjustment%SpecFile%iValues     , &
                           SupplyAdjustment%iGlobalAdjustIter    , &
                           SupplyAdjustment%iWellPumpAdjustIter  , &
                           lAdjusted                             )

        !If nothing is adjusted, is it possible to adjust other supplies?
        IF (.NOT. lAdjusted) THEN
            SupplyAdjustment%iWellPumpAdjustIter = SupplyAdjustment%NMaxPumpAdjustIter + 1
            IF (.NOT. IsMoreAdjustmentPossible(SupplyAdjustment,AppGW,AppStream)) THEN
                SupplyAdjustment%lAdjust = .FALSE.
                RETURN
            END IF
        ELSE
            RETURN
      END IF

    END IF
      
    !Finally adjust elemental pumping
    IF (IsPumpingAdjustable(SupplyAdjustment,AppGW,f_iSupply_ElemPump)) THEN
        NPump = AppGW%GetNElemPumps()
        CALL AdjustPumping(AppGW                                 , &
                           ElemPumpDestConnector                 , &
                           NPump                                 , &
                           f_iSupply_ElemPump                    , &
                           AgToler                               , &
                           UrbToler                              , &
                           AgDiff                                , &
                           UrbDiff                               , &
                           SupplyAdjustment%SpecFile%iValues     , &
                           SupplyAdjustment%iGlobalAdjustIter    , &
                           SupplyAdjustment%iElemPumpAdjustIter  , &
                           lAdjusted                             )

        !If nothing is adjusted, is it possible to adjust other supplies?
        IF (.NOT. lAdjusted) THEN
            SupplyAdjustment%iElemPumpAdjustIter = SupplyAdjustment%NMaxPumpAdjustIter + 1
            SupplyAdjustment%lAdjust             = .FALSE.
        END IF
        RETURN
    END IF
    
    !If made to this point, no more adjustment is possible
    SupplyAdjustment%lAdjust = .FALSE.
    
  END SUBROUTINE Adjust
  
  
  ! -------------------------------------------------------------
  ! --- ADJUST DIVERSIONS TO MEET ELEMENT LEVEL DEMANDS
  ! -------------------------------------------------------------
  SUBROUTINE AdjustDiversions(AppStream,DiverDestConnector,NDiver,AgToler,UrbToler,AgDiff,UrbDiff,iAdjustFlags,iGlobalAdjustIter,iDiverAdjustIter,lAdjusted)
    TYPE(AppStreamType)                  :: AppStream
    TYPE(SupplyDestinationConnectorType) :: DiverDestConnector
    INTEGER,INTENT(IN)                   :: NDiver,iAdjustFlags(:)
    REAL(8),INTENT(IN)                   :: AgToler(:),UrbToler(:),AgDiff(:),UrbDiff(:)
    INTEGER                              :: iGlobalAdjustIter,iDiverAdjustIter
    LOGICAL,INTENT(OUT)                  :: lAdjusted
    
    !Local variables
    INTEGER :: iAdjustFlagsLocal(0:SIZE(iAdjustFlags)),iColAdjust(NDiver),     &
               iDiverRank(NDiver),iDiverAdjustFlags(NDiver)
    REAL(8) :: DeliRequired(NDiver),DeliMax(NDiver),DeliActual(NDiver),IrigFracs(NDiver)
    
    !Initialize
    iAdjustFlagsLocal(0)  = 0
    iAdjustFlagsLocal(1:) = iAdjustFlags
    
    !Get diversions specs data
    CALL AppStream%GetSupplyAdjustData(iDiverRank     , &
                                       iColAdjust     , &
                                       DeliRequired   , &
                                       DeliMax        , &
                                       DeliActual     , &
                                       IrigFracs      )
    iDiverAdjustFlags = iAdjustFlagsLocal(iColAdjust)
    
    !Increment diversion adjustment counter by 1 until at least one diversion with that rank or higher exist
    DO
      iDiverAdjustIter = iDiverAdjustIter + 1
      IF (iDiverAdjustIter .GT. AppStream%GetMaxDiversionRank()) RETURN
      IF (.NOT. ANY(iDiverRank .EQ. iDiverAdjustIter)) CYCLE
      IF (ANY(iDiverRank .GE. iDiverAdjustIter)) EXIT
    END DO 
        
    !Adjust diversions
    CALL AdjustSupplies(f_iSupply_Diversion  , &
                        iDiverRank           , &
                        iDiverAdjustIter     , &
                        iDiverAdjustFlags    , &
                        AgDiff               , &
                        UrbDiff              , &
                        AgToler              , &
                        UrbToler             , &
                        DeliActual           , &
                        DeliMax              , &
                        DiverDestConnector   , &
                        DeliRequired         , &
                        IrigFracs            , &
                        lAdjusted            )
    
    !If adjusted, update the supply specs with stream object
    IF (lAdjusted) THEN
      CALL AppStream%SetSupplySpecs(DiverDestConnector,DeliRequired,IrigFracs,DiverDestConnector%SupplyToDestination)
    
      !Increment global adjustment iteration number
      iGlobalAdjustIter = iGlobalAdjustIter + 1 
    END IF
    
  END SUBROUTINE AdjustDiversions
  

  ! -------------------------------------------------------------
  ! --- ADJUST WELL/ELEMENT PUMPING TO MEET ELEMENT LEVEL DEMANDS
  ! -------------------------------------------------------------
  SUBROUTINE AdjustPumping(AppGW,PumpDestConnector,NPump,iSupplyType,AgToler,UrbToler,AgDiff,UrbDiff,iAdjustFlags,iGlobalAdjustIter,iPumpAdjustIter,lAdjusted)
    TYPE(AppGWType)                      :: AppGW
    TYPE(SupplyDestinationConnectorType) :: PumpDestConnector
    INTEGER,INTENT(IN)                   :: NPump,iSupplyType,iAdjustFlags(:)
    REAL(8),INTENT(IN)                   :: AgToler(:),UrbToler(:),AgDiff(:),UrbDiff(:)
    INTEGER                              :: iGlobalAdjustIter,iPumpAdjustIter
    LOGICAL,INTENT(OUT)                  :: lAdjusted
    
    !Local variables
    INTEGER           :: iAdjustFlagsLocal(0:SIZE(iAdjustFlags)),iColAdjust(NPump),iPumpAdjustFlags(NPump)
    INTEGER,PARAMETER :: iDummyArray(1) = [0]
    REAL(8)           :: PumpRequired(NPump),PumpMax(NPump),PumpActual(NPump),IrigFracs(NPump)
    
    !Initialize
    iAdjustFlagsLocal(0)  = 0
    iAdjustFlagsLocal(1:) = iAdjustFlags
    
    !Get pumping specs data
    CALL AppGW%GetSupplyAdjustData(iSupplyType     , &
                                   iColAdjust      , &
                                   PumpRequired    , &
                                   PumpMax         , &
                                   PumpActual      , &
                                   IrigFracs       )
    iPumpAdjustFlags = iAdjustFlagsLocal(iColAdjust)
    
    !Adjust pumping
    CALL AdjustSupplies(iSupplyType        , &
                        iDummyArray        , &
                        iPumpAdjustIter    , &
                        iPumpAdjustFlags   , &
                        AgDiff             , &
                        UrbDiff            , &
                        AgToler            , &
                        UrbToler           , &
                        PumpActual         , &
                        PumpMax            , &
                        PumpDestConnector  , &  
                        PumpRequired       , &
                        IrigFracs          , &
                        lAdjusted          )
    
    !If any pumping is adjusted...
    IF (lAdjusted) THEN
      !Update the supply specs with pumping object
      CALL AppGW%SetSupplySpecs(PumpDestConnector,iSupplyType,PumpRequired,IrigFracs,PumpDestConnector%SupplyToDestination)
   
      !Increment global and pumping specific adjustment iteration number
      iGlobalAdjustIter = iGlobalAdjustIter + 1 
      iPumpAdjustIter   = iPumpAdjustIter + 1 
    END IF
    
  END SUBROUTINE AdjustPumping


  ! -------------------------------------------------------------
  ! --- ADJUST SUPPLIES
  ! -------------------------------------------------------------
  SUBROUTINE AdjustSupplies(iSupplyType,iDiverRanks,iDiverAdjustIter,iAdjustFlags,AgDiff,UrbDiff,AgToler,UrbToler,SourcesActual,SourcesMax,SupplyDestConnector,SourcesRequired,IrigFracs,lAdjusted)
    INTEGER,INTENT(IN)                   :: iSupplyType,iDiverRanks(:),iDiverAdjustIter,iAdjustFlags(:)
    REAL(8),INTENT(IN)                   :: AgDiff(:),UrbDiff(:),AgToler(:),UrbToler(:)
    REAL(8),INTENT(IN)                   :: SourcesActual(:),SourcesMax(:)
    TYPE(SupplyDestinationConnectorType) :: SupplyDestConnector
    REAL(8)                              :: SourcesRequired(:),IrigFracs(:)
    LOGICAL,INTENT(OUT)                  :: lAdjusted
    
    !Local variables
    INTEGER :: indxDest,nAdjustable,nSupply,indxSupply
    REAL(8) :: Diff,SourcesRequired_Original(SupplyDestConnector%NSupply),Toler,IrigFracs_Original(SupplyDestConnector%NSupply)
    LOGICAL :: lAdjustSuppliesForADest(500),lAdjustedSupplies(SupplyDestConnector%NSupply)
    
    !Initialize
    lAdjusted                = .FALSE.
    lAdjustedSupplies        = .FALSE.
    SourcesRequired_Original = SourcesRequired
    SourcesRequired          = SourcesActual
    IrigFracs_Original       = IrigFracs
    
    ASSOCIATE (pSupplyToDest => SupplyDestConnector%SupplyToDestination , &
               pDestToSupply => SupplyDestConnector%DestinationToSupply )
        
        !First convert supply to destination fractions into volumes
        DO indxSupply=1,SupplyDestConnector%NSupply
            pSupplyToDest(indxSupply)%SupplyToDestFracs_Ag  = SourcesActual(indxSupply) * IrigFracs(indxSupply)       * pSupplyToDest(indxSupply)%SupplyToDestFracs_Ag
            pSupplyToDest(indxSupply)%SupplyToDestFracs_Urb = SourcesActual(indxSupply) * (1d0-IrigFracs(indxSupply)) * pSupplyToDest(indxSupply)%SupplyToDestFracs_Urb
        END DO
        
        !Iterate over destinations
        Destination_Loop:  &
        DO indxDest=1,SupplyDestConnector%NDestination
          nSupply = pDestToSupply(indxDest)%nSupply
          
          !If there are no sources serving this destination, cycle
          IF (nSupply .EQ. 0) CYCLE 
          
          !Adjust for ag demand
          !--------------------
          Diff  = AgDiff(indxDest)
          Toler = AgToler(indxDest)
          Ag_Supply_Adjustment_Loop: &
          DO
            !Adjust if the demand-supply difference is greater than the tolerance
            IF (ABS(Diff) .LE. Toler) EXIT
            
            !Count adjustable supplies
            CALL TagAdjustableSupplies(iSupplyType             , &
                                       Diff                    , &
                                       f_iAg                   , &
                                       iAdjustFlags            , &
                                       SourcesRequired_Original, &
                                       SourcesRequired         , &
                                       SourcesActual           , &
                                       SourcesMax              , &
                                       iDiverRanks             , &
                                       iDiverAdjustIter        , &
                                       pDestToSupply(indxDest) , &
                                       pSupplyToDest           , &
                                       lAdjustSuppliesForADest )
            nAdjustable = COUNT(lAdjustSuppliesForADest(1:nSupply))
          
            !Exit if there are no adjustable supplies
            IF (nAdjustable .EQ. 0) EXIT
            
            !Something is adjusted
            lAdjusted = .TRUE.
            
            !Tag the supplies as adjusted
            DO indxSupply=1,nSupply
                IF (.NOT. lAdjustSuppliesForADest(indxSupply)) CYCLE
                lAdjustedSupplies(pDestToSupply(indxDest)%iSupplies(indxSupply)) = .TRUE.
            END DO
          
            !Adjust supply
            CALL Adjust(f_iAg,Toler,lAdjustSuppliesForADest,SourcesMax,pDestToSupply(indxDest),pSupplyToDest,SourcesActual,SourcesRequired)
          
          END DO Ag_Supply_Adjustment_Loop

          !Adjust for urb demand
          !--------------------
          Diff  = UrbDiff(indxDest)
          Toler = UrbToler(indxDest)
          Urb_Supply_Adjustment_Loop: &
          DO
            !Adjust if the demand-supply difference is greater than the tolerance
            IF (ABS(Diff) .LE. Toler) EXIT
            
            !Count adjustable supplies
            CALL TagAdjustableSupplies(iSupplyType             , &
                                       Diff                    , &
                                       f_iUrb                  , &
                                       iAdjustFlags            , &
                                       SourcesRequired_Original, &
                                       SourcesRequired         , &
                                       SourcesActual           , &
                                       SourcesMax              , &
                                       iDiverRanks             , &
                                       iDiverAdjustIter        , &
                                       pDestToSupply(indxDest) , &
                                       pSupplyToDest           , &
                                       lAdjustSuppliesForADest )
            nAdjustable = COUNT(lAdjustSuppliesForADest(1:nSupply))
          
            !Exit if there are no adjustable supplies
            IF (nAdjustable .EQ. 0) EXIT
          
            !Something is adjusted
            lAdjusted = .TRUE.

            !Tag the supplies as adjusted
            DO indxSupply=1,nSupply
                IF (.NOT. lAdjustSuppliesForADest(indxSupply)) CYCLE
                lAdjustedSupplies(pDestToSupply(indxDest)%iSupplies(indxSupply)) = .TRUE.
            END DO
          
            !Adjust supply
            CALL Adjust(f_iUrb,Toler,lAdjustSuppliesForADest,SourcesMax,pDestToSupply(indxDest),pSupplyToDest,SourcesActual,SourcesRequired)
          
          END DO Urb_Supply_Adjustment_Loop
          
        END DO Destination_Loop
    
        !Finally convert volumetric supply-to-destination connections to fractions
        DO indxSupply=1,SupplyDestConnector%NSupply
            IF (.NOT. lAdjustedSupplies(indxSupply)) THEN
                SourcesRequired(indxSupply) = SourcesRequired_Original(indxSupply)
                IrigFracs(indxSupply)       = IrigFracs_Original(indxSupply)
            ELSE
                IF (SourcesRequired(indxSupply) .GT. 0.0) THEN
                    IrigFracs(indxSupply) = SUM(pSupplyToDest(indxSupply)%SupplyToDestFracs_Ag) / SourcesRequired(indxSupply)
                ELSE
                    IrigFracs(indxSupply) = IrigFracs_Original(indxSupply)
                END IF
            END IF
            IF (pSupplyToDest(indxSupply)%nDest .EQ. 1) THEN
                pSupplyToDest(indxSupply)%SupplyToDestFracs_Ag  = 1.0
                pSupplyToDest(indxSupply)%SupplyToDestFracs_Urb = 1.0
            ELSE
                CALL NormalizeArray(pSupplyToDest(indxSupply)%SupplyToDestFracs_Ag)
                CALL NormalizeArray(pSupplyToDest(indxSupply)%SupplyToDestFracs_Urb)
            END IF
        END DO
    
    END ASSOCIATE
  
    
  CONTAINS
  
  
    ! ############################################
    ! --- ADJUST SUPPLIES FOR AN ELEMENT
    ! ############################################
    SUBROUTINE Adjust(iAdjustFor,Toler,lAdjustSupplies,SourcesMax,DestToSupply,SupplyToDest,SourcesActual,SourcesRequired)
      INTEGER,INTENT(IN)                       :: iAdjustFor
      REAL(8),INTENT(IN)                       :: Toler,SourcesActual(:),SourcesMax(:)
      LOGICAL,INTENT(IN)                       :: lAdjustSupplies(:)
      TYPE(DestinationToSupplyType),INTENT(IN) :: DestToSupply
      TYPE(SupplyToDestinationType)            :: SupplyToDest(:)
      REAL(8)                                  :: SourcesRequired(:)

      !Local variables
      INTEGER             :: indxSupply,iSupply,indxInServedDest,iErrorCode,inDest
      REAL(8)             :: rCorrection,rAdjustedSupply,rCheck,AgSupplyTotal,         &
                             DestSupply_Ag_Old,DestSupply_Urb_Old,DestSupply_Ag_New,   &
                             DestSupply_Urb_New 
      REAL(8),ALLOCATABLE :: SourcesMax_Local(:)
                 
    
      !Find correction
      rCorrection = Diff / REAL(nAdjustable,8)
      
      !Adjust each adjustable supply
      Diff = 0.0
      DO indxSupply=1,DestToSupply%nSupply
          IF (.NOT. lAdjustSupplies(indxSupply)) CYCLE
          iSupply            =  DestToSupply%iSupplies(indxSupply)
          indxInServedDest   =  DestToSupply%iIndexInServedDest(indxSupply)
          ASSOCIATE (pSupplyToDest      => SupplyToDest(iSupply)    )
              
              DestSupply_Ag_Old  = pSupplyToDest%SupplyToDestFracs_Ag(indxInServedDest)
              DestSupply_Urb_Old = pSupplyToDest%SupplyToDestFracs_Urb(indxInServedDest)
              IF (iAdjustFor .EQ. f_iAg) THEN
                  rAdjustedSupply = DestSupply_Ag_Old 
              ELSE
                  rAdjustedSupply = DestSupply_Urb_Old
              END IF 
               
              !If the supply is being reduced
              IF (rCorrection .LT. 0.0) THEN
                  !If Tolerance is zero, the demand is zero. Set the supply to zero
                  IF (Toler .EQ. 0.0) THEN
                      rAdjustedSupply = 0.0
                  ELSE
                      rCheck = rAdjustedSupply + rCorrection
                      !Adjusted supply cannot be less than zero
                      IF (rCheck .LT. 0.0) THEN
                          Diff            = Diff + rCheck
                          rAdjustedSupply = 0.0
                      ELSE
                          rAdjustedSupply = rCheck
                      END IF
                  END IF
          
              !If the supply is being increased
              ELSE
                  rAdjustedSupply = rAdjustedSupply + rCorrection
                  inDest          = pSupplyToDest%nDest
                  DEALLOCATE (SourcesMax_Local , STAT=iErrorCode)
                  ALLOCATE (SourcesMax_Local(inDest))
                  IF (iAdjustFor .EQ. f_iAg) THEN
                      SourcesMax_Local   = pSupplyToDest%SupplyToDestFracs_Ag
                      IF (ALL(SourcesMax_Local .EQ. 0d0)) THEN
                          SourcesMax_Local = 1d0 / REAL(inDest,8)
                      ELSE
                          CALL NormalizeArray(SourcesMax_Local)
                      END IF
                      SourcesMax_Local   = SourcesMax(iSupply) * SourcesMax_Local
                      rAdjustedSupply    = MIN(rAdjustedSupply , SourcesMax_Local(indxInServedDest))
                  ELSE
                      SourcesMax_Local   = pSupplyToDest%SupplyToDestFracs_Urb
                      IF (ALL(SourcesMax_Local .EQ. 0d0)) THEN
                          SourcesMax_Local = 1d0 / REAL(inDest,8)
                      ELSE
                          CALL NormalizeArray(SourcesMax_Local)
                      END IF
                      SourcesMax_Local   = SourcesMax(iSupply) * SourcesMax_Local
                      rAdjustedSupply    = MIN(rAdjustedSupply , SourcesMax_Local(indxInServedDest))
                  END IF
              END IF
              
              !Store adjusted supplies
              IF (iAdjustFor .EQ. f_iAg) THEN
                DestSupply_Ag_New  = rAdjustedSupply
                DestSupply_Urb_New = DestSupply_Urb_Old
              ELSE
                DestSupply_Ag_New  = DestSupply_Ag_Old
                DestSupply_Urb_New = rAdjustedSupply
              END IF
              SourcesRequired(iSupply)                              = SourcesRequired(iSupply) - (DestSupply_Ag_Old + DestSupply_Urb_Old) + (DestSupply_Ag_New + DestSupply_Urb_New)
              SourcesRequired(iSupply)                              = MAX(0d0 , SourcesRequired(iSupply))   !Check for round-off errors; sometimes sources become a tiny negative value
              pSupplyToDest%SupplyToDestFracs_Ag(indxInServedDest)  = DestSupply_Ag_New
              pSupplyToDest%SupplyToDestFracs_Urb(indxInServedDest) = DestSupply_Urb_New
              
          END ASSOCIATE
      END DO

    END SUBROUTINE Adjust
    
  END SUBROUTINE AdjustSupplies


  ! -------------------------------------------------------------
  ! --- TAG ADJUSTABLE SUPPLIES
  ! -------------------------------------------------------------
  SUBROUTINE TagAdjustableSupplies(iSupplyType,Diff,iAdjustFor,iAdjustFlags,SourcesRequired_Old,SourcesRequired_New,SourcesActual,MaxSources,iDiverRanks,iDiverAdjustIter,DestToSupply,SupplyToDest,lAdjustSupplies)
    INTEGER,INTENT(IN)                       :: iSupplyType,iAdjustFor,iDiverAdjustIter,iDiverRanks(:),iAdjustFlags(:)
    REAL(8),INTENT(IN)                       :: Diff,SourcesRequired_Old(:),SourcesRequired_New(:),SourcesActual(:),MaxSources(:)
    TYPE(DestinationToSupplyType),INTENT(IN) :: DestToSupply
    TYPE(SupplyToDestinationType),INTENT(IN) :: SupplyToDest(:)
    LOGICAL,INTENT(OUT)                      :: lAdjustSupplies(:)
    
    !Local variables
    INTEGER :: indxSupply,iAdjustSupplyFor,iSupply,indxInServedDest
    REAL(8) :: Supply
    
    !Initialize
    lAdjustSupplies(1:DestToSupply%nSupply) = .TRUE.
    
    !Tag the adjustable supplies
    DO indxSupply=1,DestToSupply%nSupply
      iSupply          = DestToSupply%iSupplies(indxSupply)
      indxInServedDest = DestToSupply%iIndexInServedDest(indxSupply)
      
      !Supply must be greater than or equal to zero to be adjustable
      IF (SourcesRequired_Old(iSupply) .LT. 0.0) THEN
          lAdjustSupplies(indxSupply) = .FALSE.
          CYCLE
      END IF
      
      !Is the supply tagged for adjustment?
      iAdjustSupplyFor = iAdjustFlags(iSupply)
      IF (iAdjustFor .EQ. f_iAg) THEN
        IF (iAdjustSupplyFor.EQ.f_iAdjustForUrb .OR. iAdjustSupplyFor.EQ.f_iAdjustForNone) THEN
          lAdjustSupplies(indxSupply) = .FALSE.
          CYCLE
        END IF
      ELSE
        IF (iAdjustSupplyFor.EQ.f_iAdjustForAg .OR. iAdjustSupplyFor.EQ.f_iAdjustForNone) THEN
          lAdjustSupplies(indxSupply) = .FALSE.
          CYCLE
        END IF
      END IF
      
      !Is the rank of the diversion greater than or equal to the rank being considered?
      IF (iSupplyType .EQ. f_iSupply_Diversion) THEN
        IF (iDiverRanks(indxSupply) .LT. iDiverAdjustIter) THEN
          lAdjustSupplies(indxSupply) = .FALSE.
          CYCLE
        END IF
      END IF
      
      !If the supply needs to be decreased...
      IF (Diff .LT. 0.0) THEN
        IF (iAdjustFor .EQ. f_iAg) THEN
          Supply = SupplyToDest(iSupply)%SupplyToDestFracs_Ag(indxInServedDest)
        ELSE
          Supply = SupplyToDest(iSupply)%SupplyToDestFracs_Urb(indxInServedDest)
        END IF
        !Is the original supply greater than zero?
        IF (Supply .EQ. 0d0) THEN
          lAdjustSupplies(indxSupply) = .FALSE.
          CYCLE
        END IF
        
      !If the supply needs to be increased...
      ELSE   
        !Is the water at source less than the maximum amount?
        IF (SourcesRequired_New(iSupply) .GT. 0.99999d0*MaxSources(iSupply)) THEN  !Use a slightly decreased version of MAxSource in comparison to cover for round-off errors that might show up in SourcesRequired_New 
            lAdjustSupplies(indxSupply) = .FALSE.
            CYCLE
        END IF
        !Is the actual source less than required source indicating that there is not enough water at the source?
        IF (SourcesRequired_Old(iSupply) .GT. SourcesActual(iSupply)) THEN
            lAdjustSupplies(indxSupply) = .FALSE.
            CYCLE
        END IF              
      END IF

    END DO
    
  END SUBROUTINE TagAdjustableSupplies
  
  
  ! -------------------------------------------------------------
  ! --- RESET THE STATE OF THE SUPPLY ADJUSTMENT OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE ResetState(SupplyAdjustment)
    CLASS(SupplyAdjustmentType) :: SupplyAdjustment
    
    !Local variables
    TYPE(SupplyAdjustmentType) :: DefaultData
    
    !No need to do anything if supply adjustment is not on
    IF (SupplyAdjustment%iAdjust .EQ. f_iAdjustNone) RETURN
    
    SupplyAdjustment%iWellPumpAdjustIter = DefaultData%iWellPumpAdjustIter
    SupplyAdjustment%iElemPumpAdjustIter = DefaultData%iElemPumpAdjustIter
    SupplyAdjustment%iDiverAdjustIter    = DefaultData%iDiverAdjustIter
    SupplyAdjustment%iGlobalAdjustIter   = DefaultData%iGlobalAdjustIter
    SupplyAdjustment%lAdjust             = .TRUE.
    
  END SUBROUTINE ResetState
  
  
  ! -------------------------------------------------------------
  ! --- CAN DIVERSIONS BE ADJUSTED?
  ! -------------------------------------------------------------
  FUNCTION IsDiversionsAdjustable(SupplyAdjustment,AppStream) RESULT(lAdjustable)
    TYPE(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    TYPE(AppStreamType),INTENT(IN)        :: AppStream
    LOGICAL                               :: lAdjustable
    
    !Local variables
    INTEGER             :: NDiver,indxDiver,ErrorCode
    INTEGER,ALLOCATABLE :: iColAdjust(:)
    LOGICAL             :: lAdjusting
    
    !Initialize
    lAdjustable = .FALSE.
    NDiver      = AppStream%GetNDiver()
    
    !Are there any diversions defined?
    IF (NDiver .EQ. 0) RETURN
    
    !Has the user asked for diversion adjustment?
    IF (SupplyAdjustment%iAdjust.EQ.f_iAdjustNone .OR. SupplyAdjustment%iAdjust.EQ.f_iAdjustPump) RETURN
    
    !Is the iteration number less than the maximum?
    IF (SupplyAdjustment%iDiverAdjustIter .GT. AppStream%GetMaxDiversionRank()) RETURN
    
    !Are any of the diversions adjustable for the currrent time step?
    ALLOCATE (iColAdjust(NDiver))
    CALL AppStream%GetiColAdjust(iColAdjust)
    lAdjusting = .FALSE.
    DO indxDiver=1,NDiver
        IF (iColAdjust(indxDiver) .EQ. 0) CYCLE
        IF (SupplyAdjustment%SpecFile%iValues(iColAdjust(indxDiver)) .NE. f_iAdjustForNone) THEN
            lAdjusting = .TRUE.
            EXIT
        END IF
    END DO
    DEALLOCATE (iColAdjust ,STAT=ErrorCode)
    IF (.NOT. lAdjusting) RETURN
    
    !If made to to this point, diversions are adjustable    
    lAdjustable = .TRUE.
    
  END FUNCTION IsDiversionsAdjustable

  
  ! -------------------------------------------------------------
  ! --- CAN PUMPING BE ADJUSTED?
  ! -------------------------------------------------------------
  FUNCTION IsPumpingAdjustable(SupplyAdjustment,AppGW,iSupplyType) RESULT(lAdjustable)
    TYPE(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    TYPE(AppGWType),INTENT(IN)            :: AppGW
    INTEGER,INTENT(IN)                    :: iSupplyType
    LOGICAL                               :: lAdjustable
    
    !Local variables 
    INTEGER             :: iIter,NPumps,indxPump,iPumpType,ErrorCode
    INTEGER,ALLOCATABLE :: iColAdjust(:)
    LOGICAL             :: lAdjusting
    
    !Initialize
    lAdjustable = .FALSE.
    SELECT CASE (iSupplyType)
      CASE (f_iSupply_Well)
        NPumps    = AppGW%GetNWells()
        iIter     = SupplyAdjustment%iWellPumpAdjustIter
        iPumpType = f_iPump_Well
        
      CASE (f_iSupply_ElemPump)
        NPumps    = AppGW%GetNElemPumps()
        iIter     = SupplyAdjustment%iElemPumpAdjustIter
        iPumpType = f_iPump_ElemPump
    END SELECT
    
    !Are there any pumping defined?
    IF (NPumps .EQ. 0) RETURN
    
    !Has the user asked for pumping adjustment?
    IF (SupplyAdjustment%iAdjust.EQ.f_iAdjustNone .OR. SupplyAdjustment%iAdjust.EQ.f_iAdjustDiver) RETURN
    
    !Is the iteration number less than the maximum?
    IF (iIter .GE. SupplyAdjustment%nMaxPumpAdjustIter) RETURN
    
    !Are any of the wells/element pumps adjustable for the currrent time step?
    ALLOCATE (iColAdjust(NPumps))
    CALL AppGW%GetiColAdjust(iPumpType,iColAdjust)
    lAdjusting = .FALSE.
    DO indxPump=1,NPumps
      IF (iColAdjust(indxPump) .EQ. 0) CYCLE
      IF (SupplyAdjustment%SpecFile%iValues(iColAdjust(indxPump)) .NE. f_iAdjustForNone) THEN
        lAdjusting = .TRUE.
        EXIT
      END IF
    END DO
    DEALLOCATE (iColAdjust ,STAT=ErrorCode)
    IF (.NOT. lAdjusting) RETURN
    
    !If made to to this point, pumping is adjustable    
    lAdjustable = .TRUE.
    
  END FUNCTION IsPumpingAdjustable
  
  
  ! -------------------------------------------------------------
  ! --- CAN MORE SUPPLIES BE ADJUSTED?
  ! -------------------------------------------------------------
  FUNCTION IsMoreAdjustmentPossible(SupplyAdjustment,AppGW,AppStream) RESULT(lPossible)
    TYPE(SupplyAdjustmentType),INTENT(IN) :: SupplyAdjustment
    TYPE(AppGWType),INTENT(IN)            :: AppGW
    TYPE(AppStreamType),INTENT(IN)        :: AppStream
    LOGICAL                               :: lPossible
    
    !Initialize
    lPossible = .FALSE.
    
    !Any diversions adjustable?
    IF (IsDiversionsAdjustable(SupplyAdjustment,AppStream)) THEN
      lPossible = .TRUE.
      RETURN
    END IF
    
    !Any wells adjustable?
    IF (IsPumpingAdjustable(SupplyAdjustment,AppGW,f_iSupply_Well)) THEN
      lPossible = .TRUE.
      RETURN
    END IF
    
    !Any element pumping adjustable?
    IF (IsPumpingAdjustable(SupplyAdjustment,AppGW,f_iSupply_ElemPump)) lPossible = .TRUE.
    
  END FUNCTION IsMoreAdjustmentPossible
 
  
END MODULE