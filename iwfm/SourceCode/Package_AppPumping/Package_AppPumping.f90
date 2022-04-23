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
MODULE Package_AppPumping
  USE Class_Version               , ONLY: ReadVersion
  USE MessageLogger               , ONLY: SetLastMessage                           , &
                                          LogMessage                               , &
                                          EchoProgress                             , &
                                          MessageArray                             , &
                                          iFatal                                   , &
                                          iMessage        
  USE TimeSeriesUtilities
  USE GeneralUtilities
  USE IOInterface
  USE Package_Misc                , ONLY: RealTSDataInFileType                     , &
                                          ReadTSData                               , &
                                          iGWComp                                  , &
                                          iSupply_Well                             , &
                                          iSupply_ElemPump
  USE Package_Discretization
  USE Package_ComponentConnectors , ONLY: SupplyToDestinationType                  , &
                                          Supply_GetSupply                         , &
                                          Supply_GetDestination                    , &
                                          Supply_SetIrigFracsRead                  , &
                                          Supply_SetSupplySpecs                    , &
                                          Supply_ResetIrigFracs                    , &
                                          Supply_CheckSupplyDestinationConnection  
  USE Class_Pumping
  USE Class_Well
  USE Class_ElementPumping
  USE Class_PumpsAtElem
  USE Package_Matrix              , ONLY: MatrixType
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
  PUBLIC :: AppPumpingType                          , &
            iPump_Well                              , &
            iPump_ElemPump                          


  ! -------------------------------------------------------------
  ! --- PUMPING TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iPump_Well     = iSupply_Well     , &
                       iPump_ElemPump = iSupply_ElemPump
                       
                       
  ! -------------------------------------------------------------
  ! --- APPLICATION PUMPING TYPE
  ! -------------------------------------------------------------
  TYPE AppPumpingType
      PRIVATE
      LOGICAL                           :: lThereIsPumping  = .FALSE.       !Flag to check if there is at least one pumping (negative value) is specified
      INTEGER                           :: NWells           = 0             !Number of wells simulated
      INTEGER                           :: NElemPumps       = 0             !Number of element pumping simulated
      TYPE(WellType),ALLOCATABLE        :: Wells(:)                         !Well data
      TYPE(ElemPumpType),ALLOCATABLE    :: ElemPumps(:)                     !Element pumping data
      TYPE(PumpsAtElemType),ALLOCATABLE :: WellsAtElems(:)                  !Well IDs at (element)
      TYPE(PumpsAtElemType),ALLOCATABLE :: ElemPumpsAtElems(:)              !Element pumping IDs at (element)
      REAL(8),ALLOCATABLE               :: NodalPumpRequired(:,:)           !Required pumping at (node,layer) combination
      REAL(8),ALLOCATABLE               :: NodalPumpActual(:,:)             !Actual pumping at (node,layer) combination
      REAL(8)                           :: rPumpFactor      = 1.0           !Pumping conversion factor
      TYPE(RealTSDataInFileType)        :: TSPumpFile                       !Relevant data for time series pumping data
  CONTAINS
      PROCEDURE,PASS :: New                          
      PROCEDURE,PASS :: Kill                         
      PROCEDURE,PASS :: GetNWells                    
      PROCEDURE,PASS :: GetNElemPumps                
      PROCEDURE,PASS :: GetElement                   
      PROCEDURE,PASS :: GetLayerFactors
      PROCEDURE,PASS :: GetNodeLayerFactors
      PROCEDURE,PASS :: GetPumpDestination
      PROCEDURE,PASS :: GetPumpingAtElementLayerNode 
      PROCEDURE,PASS :: GetSupply    
      PROCEDURE,PASS :: GetSupplySpecs
      PROCEDURE,PASS :: GetPumpActual                
      PROCEDURE,PASS :: GetSupplyAdjustData          
      PROCEDURE,PASS :: GetSubregionalPumping        
      PROCEDURE,PASS :: GetSubregionalRecharge       
      PROCEDURE,PASS :: GetNodalPumpActual
      PROCEDURE,PASS :: GetNodalPumpRequired
      PROCEDURE,PASS :: GetElementalPumpActual       
      PROCEDURE,PASS :: GetiColAdjust                
      PROCEDURE,PASS :: IsDestinationToModelDomain   
      PROCEDURE,PASS :: SetNodalPumpActual           
      PROCEDURE,PASS :: SetIrigFracsRead             
      PROCEDURE,PASS :: SetSupplySpecs               
      PROCEDURE,PASS :: ReadTSData                    => AppPumping_ReadTSData        
      PROCEDURE,PASS :: UpdatePumpDistFactors        
      PROCEDURE,PASS :: ComputeNodalPumpActual       
      PROCEDURE,PASS :: ResetIrigFracs               
      PROCEDURE,PASS :: CheckSupplyDestinationConnection 
      PROCEDURE,PASS :: Simulate
      PROCEDURE,PASS :: ResetActualPumping
  END TYPE AppPumpingType 
  

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_AppPumping::'
  
  
  
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
  ! --- INSTANTIATE PUMPING COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE New(AppPumping,cFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,iStat) 
    CLASS(AppPumpingType),INTENT(OUT) :: AppPumping
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3),PARAMETER :: ThisProcedure = ModName // 'New'
    INTEGER                               :: NElements,NNodes,NLayers,ErrorCode
    REAL(8)                               :: Factor(1)
    LOGICAL                               :: DummyArray(1) = (/.TRUE./)
    TYPE(GenericFileType)                 :: PumpDataFile
    CHARACTER(LEN=2000)                   :: ALine
    CHARACTER(:),ALLOCATABLE              :: cVersion,cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no filename is specified
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('Instantiating pumping data')
    
    !Initialize
    NElements = AppGrid%NElements
    NNodes    = AppGrid%NNodes
    NLayers   = Stratigraphy%NLayers

    !Open file
    CALL PumpDataFile%New(Filename=cFileName , InputFile=.TRUE.,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read first line that holds version number
    CALL ReadVersion(PumpDataFile,'PUMPING',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read well specifications
    CALL PumpDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ALine),cWorkingDirectory,cAbsPathFileName)
        CALL Well_New(cAbsPathFileName,AppGrid,Stratigraphy,AppPumping%Wells,iStat)
        IF (iStat .EQ. -1) RETURN
        AppPumping%NWells = SIZE(AppPumping%Wells)
    END IF
    
    !Read element pumping specifications
    CALL PumpDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ALine),cWorkingDirectory,cAbsPathFileName)
        CALL ElemPump_New(cAbsPathFileName,AppGrid,Stratigraphy,AppPumping%ElemPumps,iStat)
        IF (iStat .EQ. -1) RETURN
        AppPumping%NElemPumps = SIZE(AppPumping%ElemPumps)
    END IF
    
    !If no wells or element pumping is defined return
    IF (AppPumping%NElemPumps.EQ.0  .AND.  AppPumping%NWells.EQ.0) THEN
      CALL PumpDataFile%Kill()
      RETURN
    END IF
    
    !Time series pumping data file
    CALL PumpDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ALine),cWorkingDirectory,cAbsPathFileName)
        CALL AppPumping%TSPumpFile%Init(cAbsPathFileName,'Time series pumping data file',TimeStep%TrackTime,1,.TRUE.,Factor,DummyArray,iStat=iStat)
        IF (iStat .EQ. -1) RETURN
        AppPumping%rPumpFactor = Factor(1)
    END IF
    
    !Make sure that time series pumping data is defined if any well or element pumping is pointing a column in it
    IF (AppPumping%TSPumpFile%File%iGetFileType() .EQ. UNKNOWN) THEN
        !Check with element pumping
        IF (AppPumping%NElemPumps .GT. 0) THEN
            IF (ANY(AppPumping%ElemPumps%iColPump.GT.0)) THEN
                CALL SetLastMessage('Time series pumping data must be specified when element pumping refers to a column in this file!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        !Check with wells
        IF (AppPumping%NWells .GT. 0) THEN
            IF (ANY(AppPumping%Wells%iColPump.GT.0)) THEN
                CALL SetLastMessage('Time series pumping data must be specified when well pumping refers to a column in this file!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END IF
        
    !Check if specified locations have at least one active node
    CALL CheckActiveLayers(AppPumping,AppGrid,Stratigraphy,iStat)
    IF (iStat .EQ. -1) RETURN
         
    !Allocate memory for nodal pumping
    ALLOCATE (AppPumping%NodalPumpRequired(NNodes,NLayers) , &
              AppPumping%NodalPumpActual(NNodes,NLayers)   , &
              STAT=ErrorCode                               )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for nodal pumping!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Allocate memory for pumps at each element
    ALLOCATE (AppPumping%WellsAtElems(NElements) , AppPumping%ElemPumpsAtElems(NElements))
    IF (AppPumping%NWells .GT. 0)     CALL PumpsAtElem_New(NElements,AppPumping%Wells%Element,AppPumping%WellsAtElems)
    IF (AppPumping%NElemPumps .GT. 0) CALL PumpsAtElem_New(NElements,AppPumping%ElemPumps%Element,AppPumping%ElemPumpsAtElems)
    
    !Check if there are enough columns in the pumping data file
    CALL AppPumping%TSPumpFile%CheckColNum('time series pumping data file',AppPumping%Wells%iColPump,lCheckMinColNum=.FALSE.,iStat=iStat)         ;  IF (iStat .EQ. -1) RETURN
    CALL AppPumping%TSPumpFile%CheckColNum('time series pumping data file',AppPumping%Wells%iColPumpMax,lCheckMinColNum=.FALSE.,iStat=iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL AppPumping%TSPumpFile%CheckColNum('time series pumping data file',AppPumping%ElemPumps%iColPump,lCheckMinColNum=.FALSE.,iStat=iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL AppPumping%TSPumpFile%CheckColNum('time series pumping data file',AppPumping%ElemPumps%iColPumpMax,lCheckMinColNum=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Close file
    CALL PumpDataFile%Kill()
  
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
  ! --- KILL PUMPING DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppPumping)
    CLASS(AppPumpingType) :: AppPumping
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (AppPumping%Wells                 , &
                AppPumping%ElemPumps             , &
                AppPumping%WellsAtElems          , &
                AppPumping%ElemPumpsAtElems      , &
                AppPumping%NodalPumpRequired     , &
                AppPumping%NodalPumpActual       , &
                STAT=ErrorCode                   ) 
    CALL AppPumping%TSPumpFile%Close()
    
    !Set the attributes to their default values
    AppPumping%lThereIsPumping = .FALSE.
    AppPumping%NWells          = 0
    AppPumping%NElemPumps      = 0
    AppPumping%rPumpFactor     = 1.0    
  
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
  ! --- GET SUPPLY DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpDestination(AppPumping,iPumpType,Destination)
    CLASS(AppPumpingType),INTENT(IN)      :: AppPumping
    INTEGER,INTENT(IN)                    :: iPumpType
    TYPE(FlowDestinationType),ALLOCATABLE :: Destination(:)
    
    SELECT CASE (iPumpType)
        CASE (iPump_Well)
            CALL Supply_GetDestination(AppPumping%Wells , Destination)
            
        CASE (iPump_ElemPump)
            CALL Supply_GetDestination(AppPumping%ElemPumps , Destination)
            
        END SELECT
        
  END SUBROUTINE GetPumpDestination
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY DATA SPECS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetSupplySpecs(AppPumping,iPumpType,SupplySpecs)
    CLASS(AppPumpingType),INTENT(IN)         :: AppPumping
    INTEGER,INTENT(IN)                       :: iPumpType
    TYPE(SupplyType),ALLOCATABLE,INTENT(OUT) :: SupplySpecs(:)
    
    SELECT CASE (iPumpType)
        CASE (iPump_Well)
            ALLOCATE (SupplySpecs(AppPumping%NWells))
            SupplySpecs = AppPumping%Wells%SupplyType
        
        CASE (iPump_ElemPump)
            ALLOCATE (SupplySpecs(AppPumping%NElemPumps))
            SupplySpecs = AppPumping%ElemPumps%SupplyType
    END SELECT
        
  END SUBROUTINE GetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT ALL ELEMENTS
  ! -------------------------------------------------------------
  FUNCTION GetElementalPumpActual(AppPumping,NElems) RESULT(ElemPumpActual)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: NElems
    REAL(8)                          :: ElemPumpActual(NElems)
    
    !Initialize
    ElemPumpActual = 0.0
    
    !Process wells
    CALL Compute(AppPumping%Wells%PumpingType)
    
    !Process element pumping
    CALL Compute(AppPumping%ElemPumps%PumpingType)
        
    
  CONTAINS
  
  
    ! ############################################
    ! --- COMPUTE
    ! ############################################
    SUBROUTINE Compute(Pumping)
      TYPE(PumpingType),INTENT(IN) :: Pumping(:)
      
      !Local variables
      INTEGER :: indxPump,iElem,iRegion
      
      DO indxPump=1,SIZE(Pumping)
        IF (Pumping(indxPump)%SupplyActual .LE. 0.0) CYCLE
        iElem                 = Pumping(indxPump)%Element
        ElemPumpActual(iElem) = ElemPumpActual(iElem) + Pumping(indxPump)%SupplyActual
      END DO
      
    END SUBROUTINE Compute    
    
  END FUNCTION GetElementalPumpActual
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalPumpActual(AppPumping,NodalPumpActual)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    REAL(8),INTENT(OUT)              :: NodalPumpActual(:,:)
    
    NodalPumpActual = AppPumping%NodalPumpActual
    
  END SUBROUTINE GetNodalPumpActual
  
      
  ! -------------------------------------------------------------
  ! --- GET REQUIRED NODAL PUMPING
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodalPumpRequired(AppPumping,NodalPumpRequired)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    REAL(8),INTENT(OUT)              :: NodalPumpRequired(:,:)
    
    NodalPumpRequired = AppPumping%NodalPumpRequired
    
  END SUBROUTINE GetNodalPumpRequired
  
      
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RECHARGE DUE TO INJECTION 
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalRecharge(AppPumping,AppGrid) RESULT(Recharge)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    TYPE(AppGridType),INTENT(IN)     :: AppGrid
    REAL(8)                          :: Recharge(AppGrid%NSubregions)
    
    !Initialize
    Recharge = 0.0
    
    !Process wells
    CALL Compute(AppPumping%Wells%PumpingType)
    
    !Process element pumping
    CALL Compute(AppPumping%ElemPumps%PumpingType)
    
    
  CONTAINS
  
  
    ! ############################################
    ! --- COMPUTE
    ! ############################################
    SUBROUTINE Compute(Pumping)
      TYPE(PumpingType),INTENT(IN) :: Pumping(:)
      
      !Local variables
      INTEGER :: indxPump,iElem,iRegion
      
      DO indxPump=1,SIZE(Pumping)
        IF (Pumping(indxPump)%PumpRead .LE. 0.0) CYCLE
        iElem             = Pumping(indxPump)%Element
        iRegion           = AppGrid%AppElement(iElem)%Subregion
        Recharge(iRegion) = Recharge(iRegion) + Pumping(indxPump)%PumpRead
      END DO
      
    END SUBROUTINE Compute
    
  END FUNCTION GetSubregionalRecharge
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL PUMPING 
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalPumping(AppPumping,AppGrid) RESULT(Pump)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    TYPE(AppGridType),INTENT(IN)     :: AppGrid
    REAL(8)                          :: Pump(AppGrid%NSubregions)
    
    !Initialize
    Pump = 0.0
    
    !Process wells
    CALL Compute(AppPumping%Wells%PumpingType)
    
    !Process element pumping
    CALL Compute(AppPumping%ElemPumps%PumpingType)
        
    
  CONTAINS
  
  
    ! ############################################
    ! --- COMPUTE
    ! ############################################
    SUBROUTINE Compute(Pumping)
      TYPE(PumpingType),INTENT(IN) :: Pumping(:)
      
      !Local variables
      INTEGER :: indxPump,iElem,iRegion
      
      DO indxPump=1,SIZE(Pumping)
        IF (Pumping(indxPump)%SupplyActual .LE. 0.0) CYCLE
        iElem         = Pumping(indxPump)%Element
        iRegion       = AppGrid%AppElement(iElem)%Subregion
        Pump(iRegion) = Pump(iRegion) + Pumping(indxPump)%SupplyActual
      END DO
      
    END SUBROUTINE Compute
    
  END FUNCTION GetSubregionalPumping
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY ADJUSTMENT FLAGS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetiColAdjust(AppPumping,iPumpType,iColAdjust)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: iPumpType
    INTEGER,INTENT(OUT)              :: iColAdjust(:)
    
    SELECT CASE (iPumpType)
      CASE (iPump_Well)
        iColAdjust = AppPumping%Wells%SupplyType%iColAdjust
        
      CASE (iPump_ElemPump)
        iColAdjust = AppPumping%ElemPumps%SupplyType%iColAdjust 
    END SELECT
    
  END SUBROUTINE GetiColAdjust


  ! -------------------------------------------------------------
  ! --- GET DATA FOR SUPPLY ADJUSTMENT 
  ! -------------------------------------------------------------
  SUBROUTINE GetSupplyAdjustData(AppPumping,iPumpType,iColAdjust,PumpRequired,PumpMax,PumpActual,IrigFracs)
    CLASS(AppPumpingType),TARGET,INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)                      :: iPumpType
    INTEGER,INTENT(OUT)                     :: iColAdjust(:)
    REAL(8),INTENT(OUT)                     :: PumpRequired(:),PumpMax(:),PumpActual(:),IrigFracs(:)
    
    !Local variables
    CLASS(PumpingType),POINTER :: pPumping(:)
    
    !Initialize
    SELECT CASE (iPumpType)
        CASE (iPump_Well)
            pPumping => AppPumping%Wells
        CASE (iPump_ElemPump)
            pPumping => AppPumping%ElemPumps
    END SELECT
    
    !Assign values to return variables
    iColAdjust   = pPumping%iColAdjust
    PumpRequired = pPumping%SupplyRequired
    PumpMax      = -pPumping%PumpMax
    PumpActual   = pPumping%SupplyActual
    IrigFracs    = pPumping%IrigFrac
    
    !Clear pointer
    NULLIFY(pPumping)
    
  END SUBROUTINE GetSupplyAdjustData
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF WELLS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetNWells(AppPumping) RESULT(N)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER                          :: N
    
    N = AppPumping%NWells
    
  END FUNCTION GetNWells
  
   
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ELEMENT PUMPING 
  ! -------------------------------------------------------------
  PURE FUNCTION GetNElemPumps(AppPumping) RESULT(N)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER                          :: N
    
    N = AppPumping%NElemPumps
    
  END FUNCTION GetNElemPumps
  
   
  ! -------------------------------------------------------------
  ! --- GET ELEMENT NUMBER WHERE PUMPING OCCURS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetElement(AppPumping,indxPump,iPumpType) RESULT(iElem)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: indxPump,iPumpType
    INTEGER                          :: iElem
    
    SELECT CASE (iPumpType)
      CASE (iPump_Well)
        iElem = AppPumping%Wells(indxPump)%Element
        
      CASE (iPump_ElemPump)
        iElem = AppPumping%ElemPumps(indxPump)%Element
      
    END SELECT
    
  END FUNCTION GetElement
  
   
  ! -------------------------------------------------------------
  ! --- GET PUMPING LAYER FACTORS 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetLayerFactors(AppPumping,indxPump,iPumpType,Factors)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: indxPump,iPumpType
    REAL(8),INTENT(OUT)              :: Factors(:)
    
    SELECT CASE (iPumpType)
      CASE (iPump_Well)
        Factors = AppPumping%Wells(indxPump)%rLayerFactor
        
      CASE (iPump_ElemPump)
        Factors = AppPumping%ElemPumps(indxPump)%rLayerFactor
      
    END SELECT
    
  END SUBROUTINE GetLayerFactors


  ! -------------------------------------------------------------
  ! --- GET PUMPING NODE-LAYER FACTORS 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodeLayerFactors(AppPumping,indxPump,iPumpType,Factors)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: indxPump,iPumpType
    REAL(8),INTENT(OUT)              :: Factors(:,:)
    
    SELECT CASE (iPumpType)
      CASE (iPump_Well)
        Factors = AppPumping%Wells(indxPump)%rNodePumpFactor
        
      CASE (iPump_ElemPump)
        Factors = AppPumping%ElemPumps(indxPump)%rNodePumpFactor
      
    END SELECT
    
  END SUBROUTINE GetNodeLayerFactors
  
  
  ! -------------------------------------------------------------
  ! --- GET PUMPING AT (ELEMENT,LAYER) 
  ! -------------------------------------------------------------
  FUNCTION GetPumpingAtElementLayerNode(AppPumping,iElem,iLayer,indxNode,iPumpType) RESULT(Pumping)
    CLASS(AppPumpingType),TARGET,INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)                      :: iElem,iLayer,indxNode,iPumpType
    REAL(8)                                 :: Pumping
    
    !Local variables
    INTEGER                   :: indxPump,iPump
    TYPE(PumpsAtElemType)     :: PumpsAtElem
    TYPE(PumpingType),POINTER :: pPumps(:)
    
    !Initialize
    Pumping = 0.0
    
    SELECT CASE (iPumpType)
      CASE (iPump_ElemPump)
        PumpsAtElem =  AppPumping%ElemPumpsAtElems(iElem)
        pPumps      => AppPumping%ElemPumps%PumpingType
    
      CASE (iPump_Well)
        PumpsAtElem =  AppPumping%WellsAtElems(iElem)
        pPumps      => AppPumping%Wells%PumpingType

    END SELECT
    
    DO indxPump=1,PumpsAtElem%nPumps
      iPump   = PumpsAtElem%iPumpIDs(indxPump)
      IF (pPumps(iPump)%PumpRead .GT. 0.0) THEN
          Pumping = Pumping + pPumps(iPump)%PumpRead * pPumps(iPump)%rNodePumpFactor(indxNode,iLayer)
      ELSE
          Pumping = Pumping - pPumps(iPump)%SupplyActual * pPumps(iPump)%rNodePumpFactor(indxNode,iLayer)
      END IF
    END DO
    
    !Release memory
    NULLIFY(pPumps)

  END FUNCTION GetPumpingAtElementLayerNode
  
  
  ! -------------------------------------------------------------
  ! --- GET AGRICULTURAL AND URBAN PUMPING SUPPLY TO EACH ELEMENT 
  ! -------------------------------------------------------------
  SUBROUTINE GetSupply(AppPumping,WellDestConnector,ElemPumpDestConnector,PumpSupply_Ag,PumpSupply_Urb)
    CLASS(AppPumpingType),INTENT(IN)                :: AppPumping
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    REAL(8),INTENT(OUT)                             :: PumpSupply_Ag(:),PumpSupply_Urb(:)
    
    !Local variables
    REAL(8),DIMENSION(SIZE(PumpSupply_Ag)) :: WellSupply_Ag,WellSupply_Urb,         &
                                              ElemPumpSupply_Ag,ElemPumpSupply_Urb
    
    !Wells
    CALL Supply_GetSupply(AppPumping%Wells,WellDestConnector,WellSupply_Ag,WellSupply_Urb)
    
    !Element pumping
    CALL Supply_GetSupply(AppPumping%ElemPumps,ElemPumpDestConnector,ElemPumpSupply_Ag,ElemPumpSupply_Urb)
    
    !Final supplies
    PumpSupply_Ag  = WellSupply_Ag  + ElemPumpSupply_Ag
    PumpSupply_Urb = WellSupply_Urb + ElemPumpSupply_Urb
    
  END SUBROUTINE GetSupply
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING 
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpActual(AppPumping,iPumpType,PumpActual)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: iPumpType
    REAL(8),ALLOCATABLE,INTENT(OUT)  :: PumpActual(:)
    
    !Local variables
    INTEGER :: ErrorCode,indxPump
    
    !Allocate memory
    DEALLOCATE (PumpActual , STAT=ErrorCode)
    
    SELECT CASE (iPumpType)
      CASE (iPump_Well)
        ALLOCATE (PumpActual(AppPumping%NWells))
        DO indxPump=1,AppPumping%NWells
            IF (AppPumping%Wells(indxPump)%PumpRead .GT. 0.0) THEN
                PumpActual(indxPump) = AppPumping%Wells(indxPump)%PumpRead
            ELSE
                PumpActual(indxPump) = -AppPumping%Wells(indxPump)%SupplyActual
            END IF
        END DO
        
      CASE (iPump_ElemPump)
        ALLOCATE (PumpActual(AppPumping%NElemPumps))
        DO indxPump=1,AppPumping%NElemPumps
            IF (AppPumping%ElemPumps(indxPump)%PumpRead .GT. 0.0) THEN
                PumpActual(indxPump) = AppPumping%ElemPumps(indxPump)%PumpRead
            ELSE
                PumpActual(indxPump) = -AppPumping%ElemPumps(indxPump)%SupplyActual
            END IF
        END DO
    END SELECT
    
  END SUBROUTINE GetPumpActual

  
  
  
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
  ! --- SET IRRIGATION FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE SetIrigFracsRead(AppPumping,IrigFrac)
    CLASS(AppPumpingType) :: AppPumping
    REAL(8),INTENT(IN)    :: IrigFrac(:)
    
    !Wells
    IF (AppPumping%NWells .GT. 0)   &
      CALL Supply_SetIrigFracsRead(AppPumping%Wells,IrigFrac) 
      
    !Element pumps
    IF (AppPumping%NElemPumps .GT. 0)   &
      CALL Supply_SetIrigFracsRead(AppPumping%ElemPumps,IrigFrac) !AppPumping%ElemPumps,IrigFrac)
    
  END SUBROUTINE SetIrigFracsRead
  
  
  ! -------------------------------------------------------------
  ! --- SET ACTUAL NODAL PUMPING
  ! -------------------------------------------------------------
  SUBROUTINE SetNodalPumpActual(AppPumping,AppGrid,NodalPumpActual)
    CLASS(AppPumpingType)         :: AppPumping
    CLASS(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(IN)            :: NodalPumpActual(:,:)
    
    AppPumping%NodalPumpActual = NodalPumpActual
    
    IF (AppPumping%NWells .GT. 0)   &
      CALL AccumulateNodePump(AppPumping%Wells,AppPumping%NodalPumpActual,AppPumping%NodalPumpRequired,AppGrid)

    IF (AppPumping%NElemPumps .GT. 0)   &
      CALL AccumulateNodePump(AppPumping%ElemPumps,AppPumping%NodalPumpActual,AppPumping%NodalPumpRequired,AppGrid)

  END SUBROUTINE SetNodalPumpActual
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY SPECS
  ! -------------------------------------------------------------
  SUBROUTINE SetSupplySpecs(AppPumping,SupplyDestConnector,iPumpType,AppGrid,Stratigraphy,HHydCond,HeadGW,PumpRequired,IrigFracs,SupplyToDest)
    CLASS(AppPumpingType)                    :: AppPumping
    TYPE(SupplyDestinationConnectorType)     :: SupplyDestConnector
    INTEGER,INTENT(IN)                       :: iPumpType
    TYPE(AppGridType),INTENT(IN)             :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)        :: Stratigraphy
    REAL(8),INTENT(IN)                       :: HHydCond(:,:),HeadGW(:,:),PumpRequired(:),IrigFracs(:)
    TYPE(SupplyToDestinationType),INTENT(IN) :: SupplyToDest(:)
    
    SELECT CASE (iPumpType)
        CASE (iPump_Well)
            CALL Supply_SetSupplySpecs(AppPumping%Wells,SupplyDestConnector,PumpRequired,IrigFracs,SupplyToDest)
        
        CASE (iPump_ElemPump)
            CALL Supply_SetSupplySpecs(AppPumping%ElemPumps,SupplyDestConnector,PumpRequired,IrigFracs,SupplyToDest)
        
    END SELECT
    
    !CALL DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)

  END SUBROUTINE SetSupplySpecs

  


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
  ! --- READ TIME SERIES PUMPING DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppPumping_ReadTSData(AppPumping,AppGrid,Stratigraphy,HHydCond,HeadGW,lPumpAdjusted,TimeStep,iStat)
    CLASS(AppPumpingType)             :: AppPumping
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: HHydCond(:,:),HeadGW(:,:)
    LOGICAL,INTENT(IN)                :: lPumpAdjusted
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER :: FileReadCode,indxPump
    
    !Initialize
    iStat = 0
    
    !Return if time-series pumping file is not defined; but calculate the nodal pumping distribution in case pumping was defined outside IWFM by an external program
    IF (AppPumping%TSPumpFile%File%iGetFileType() .EQ. UNKNOWN) THEN
        CALL DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)
        RETURN
    END IF
    
    !Read time series data
    CALL ReadTSData(TimeStep,'Pumping data',AppPumping%TSPumpFile,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Proceeed based on returned error code
    SELECT CASE (FileReadCode)
      !It was not time to read
      CASE (-1)
        !If pumping was adjusted, re-define required and actual pumping as previously read values
        IF (lPumpAdjusted) THEN
            !Element pumps
            DO indxPump=1,AppPumping%NElemPumps
                IF (AppPumping%ElemPumps(indxPump)%PumpRead .LE. 0.0) THEN
                    AppPumping%ElemPumps(indxPump)%SupplyRequired = -AppPumping%ElemPumps(indxPump)%PumpRead
                    AppPumping%ElemPumps(indxPump)%SupplyActual   = AppPumping%ElemPumps(indxPump)%SupplyRequired
                END IF
            END DO
            !Wells
            DO indxPump=1,AppPumping%NWells
                IF (AppPumping%Wells(indxPump)%PumpRead .LE. 0.0) THEN
                    AppPumping%Wells(indxPump)%SupplyRequired = -AppPumping%Wells(indxPump)%PumpRead
                    AppPumping%Wells(indxPump)%SupplyActual   = AppPumping%Wells(indxPump)%SupplyRequired
                END IF
            END DO
            CALL DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)
        !In case actual pumping was different than required, equate actual to required
        ELSE
          AppPumping%ElemPumps%SupplyActual = AppPumping%ElemPumps%SupplyRequired
          AppPumping%Wells%SupplyActual     = AppPumping%Wells%SupplyRequired
          CALL DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)
        END IF
        
      !Data was read with no problem
      CASE (0)
        AppPumping%TSPumpFile%rValues = AppPumping%TSPumpFile%rValues * AppPumping%rPumpFactor
        CALL UpdatePumpValues(AppPumping%Wells,AppPumping%TSPumpFile%rValues)
        CALL UpdatePumpValues(AppPumping%ElemPumps,AppPumping%TSPumpFile%rValues)
        CALL DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)
  
    END SELECT
    
    !Set the flag to check if there is pumping (i.e. supply is positive) defined
    AppPumping%lThereIsPumping = .FALSE.
    DO indxPump=1,AppPumping%NWells
      IF (AppPumping%Wells(indxPump)%SupplyRequired .GT. 0.0) THEN
        AppPumping%lThereIsPumping = .TRUE.
        EXIT
      END IF 
    END DO
    IF (.NOT. AppPumping%lThereIsPumping) THEN
      DO indxPump=1,AppPumping%NElemPumps
        IF (AppPumping%ElemPumps(indxPump)%SupplyRequired .GT. 0.0) THEN
          AppPumping%lThereIsPumping = .TRUE.
          EXIT
        END IF 
      END DO
    END IF
      
  END SUBROUTINE AppPumping_ReadTSData
  
  
  
  
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
  ! --- SET ACTUAL PUMPING TO REQUIRED PUMPING AND DISTRIBUTE PUMPING TO NODES
  ! -------------------------------------------------------------
  SUBROUTINE ResetActualPumping(AppPumping,AppGrid,Stratigraphy,HHydCond,HeadGW)
    CLASS(AppPumpingType)             :: AppPumping
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: HHydCond(:,:),HeadGW(:,:)
    
    !Set actual supply to required supply for wells and element pumps
    AppPumping%Wells%SupplyActual     = AppPumping%Wells%SupplyRequired
    AppPumping%ElemPumps%SupplyActual = AppPumping%ElemPumps%SupplyRequired
    
    !Distribute pumping to nodes
    CALL DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)
    
  END SUBROUTINE ResetActualPumping
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ANY OF THE PUMPING GOES TO MODEL DOMAIN
  ! -------------------------------------------------------------
  PURE FUNCTION IsDestinationToModelDomain(AppPumping) RESULT(lDest)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    LOGICAL                          :: lDest
    
    !Local variables
    INTEGER :: indxPump
    
    !Initialize
    lDest = .FALSE.
    
    !Check wells
    DO indxPump=1,AppPumping%NWells
      IF (AppPumping%Wells(indxPump)%Destination%iDestType .NE. FlowDest_Outside) THEN
        lDest = .TRUE.
        RETURN
      END IF
    END DO
    
    !Check element pumping
    DO indxPump=1,AppPumping%NElemPumps
      IF (AppPumping%ElemPumps(indxPump)%Destination%iDestType .NE. FlowDest_Outside) THEN
        lDest = .TRUE.
        RETURN
      END IF
    END DO
  
  END FUNCTION IsDestinationToModelDomain
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE SUPPLY TO MEET DEMAND GOES TO MODELED DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE CheckSupplyDestinationConnection(AppPumping,WellDestConnector,ElemPumpDestConnector,iStat)
    CLASS(AppPumpingType),INTENT(IN)                :: AppPumping
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    INTEGER,INTENT(OUT)                             :: iStat
    
    !Initialize
    iStat = 0
  
    !Check wells
    IF (AppPumping%NWells .GT. 0)  &
      CALL Supply_CheckSupplyDestinationConnection(AppPumping%Wells,WellDestConnector,"well",iStat)
      
    !Check element pumping
    IF (AppPumping%NElemPumps .GT. 0)  &
      CALL Supply_CheckSupplyDestinationConnection(AppPumping%ElemPumps,ElemPumpDestConnector,"element pumping",iStat)
      
  END SUBROUTINE CheckSupplyDestinationConnection
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF PUMPING LOCATIONS ARE NOT SURROUNDED BY ALL INACTIVE NODES
  ! -------------------------------------------------------------
  SUBROUTINE CheckActiveLayers(AppPumping,AppGrid,Stratigraphy,iStat)
    TYPE(AppPumpingType),INTENT(IN)     :: AppPumping
    TYPE(AppGridType),TARGET,INTENT(IN) :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)   :: Stratigraphy
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'CheckActiveLayers'
    INTEGER                                :: indx,indxElem,ErrorCode,NVertex,Vertex(4)
    
    !Initialize
    iStat     = 0
    ErrorCode = 0
    
    !Check wells
    DO indx=1,AppPumping%NWells
        indxElem = AppPumping%Wells(indx)%Element
        NVertex  = AppGrid%Element(indxElem)%NVertex
        Vertex   = AppGrid%Element(indxElem)%Vertex
        IF (ALL(Stratigraphy%ActiveNode(Vertex(1:NVertex),:) .EQ. .FALSE.)) THEN
            WRITE (MessageArray(1),'(A10,i6,A12,i8)') 'Well ID = ',indx,' at element ',indxElem
            CALL LogMessage(MessageArray(1),iMessage,ThisProcedure)
            ErrorCode = 1
        END IF
    END DO
    
    !Check element pumping
    DO indx=1,AppPumping%NElemPumps
        indxElem = AppPumping%ElemPumps(indx)%Element
        NVertex  = AppGrid%Element(indxElem)%NVertex
        Vertex   = AppGrid%Element(indxElem)%Vertex
        IF (ALL(Stratigraphy%ActiveNode(Vertex(1:NVertex),:) .EQ. .FALSE.)) THEN
            WRITE (MessageArray(1),'(A28,i8)') 'Elem. Pump       at element ',indxElem
            CALL LogMessage(MessageArray(1),iMessage,ThisProcedure)
            ErrorCode = 1
        END IF
    END DO
    
    !Stop the program if necessary
    IF (ErrorCode .GT. 0) THEN
        MessageArray(1) = 'Above elements for pumping have all their surrounding nodes inactive!'
        MessageArray(2) = 'Pumping at these elements are redundent.'
        CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
  
  END SUBROUTINE CheckActiveLayers
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE PUMPING DISTRIBUTION FACTORS
  ! -------------------------------------------------------------
  SUBROUTINE UpdatePumpDistFactors(AppPumping,WellDestConnector,ElemPumpDestConnector,AppGrid,iDestType,DestAgArea,DestUrbArea)
    CLASS(AppPumpingType)                           :: AppPumping
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: WellDestConnector,ElemPumpDestConnector
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    INTEGER,INTENT(IN)                              :: iDestType
    REAL(8),INTENT(IN)                              :: DestAgArea(:),DestUrbArea(:)
    
    IF (AppPumping%NElemPumps .GT. 0) CALL ComputerFPumpCol(AppPumping%ElemPumps,ElemPumpDestConnector,AppGrid,iDestType,DestAgArea,DestUrbArea)
    IF (AppPumping%NWells .GT. 0) CALL ComputerFPumpCol(AppPumping%Wells,WellDestConnector,AppGrid,iDestType,DestAgArea,DestUrbArea)
    
  END SUBROUTINE UpdatePumpDistFactors
  
  
  ! -------------------------------------------------------------
  ! --- DISTRIBUTE PUMPING TO NODES
  ! -------------------------------------------------------------
  SUBROUTINE DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: HHydCond(:,:),HeadGW(:,:)
    TYPE(AppPumpingType)              :: AppPumping
    
    !Initialize
    AppPumping%NodalPumpRequired = 0.0 
       
    !Process wells
    IF (AppPumping%NWells .GT. 0)   &
      CALL DistributePumpToNodes(AppPumping%Wells,AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping%NodalPumpRequired)
      
    !Process element pumping
    IF (AppPumping%NElemPumps .GT. 0)   &
      CALL DistributePumpToNodes(AppPumping%ElemPumps,AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping%NodalPumpRequired)
  
    !Initially assume actual pumping is equal to required values
    AppPumping%NodalPumpActual = AppPumping%NodalPumpRequired 
 
  END SUBROUTINE DistPumpToNodes
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE PUMPING BY MODIFYING THE RHS VECTOR
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppPumping,Matrix)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    TYPE(MatrixType)                 :: Matrix
    
    !Local variables
    REAL(8) :: rUpdateValues(SIZE(AppPumping%NodalPumpActual))
    
    !Initialize
    rUpdateValues = - PACK(AppPumping%NodalPumpActual , MASK=.TRUE.)
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(iGWComp,1,rUpdateValues)
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE ACTUAL PUMPING IN CASE AQUIFER DRIES
  ! -------------------------------------------------------------
  SUBROUTINE ComputeNodalPumpActual(AppPumping,AppGrid,Stratigraphy,TimeStep,STOPC,HN,HP,AS_P)
    CLASS(AppPumpingType)             :: AppPumping
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8),INTENT(IN)                :: STOPC,HN(:,:),HP(:,:),AS_P(:,:)
    
    !Local variables
    INTEGER :: indxLayer,indxNode,NLayers,DeltaT
    REAL(8) :: BottomElev,NodalPumpActual_Old(AppGrid%NNodes,Stratigraphy%NLayers), &
               rNodalPumpActual_Old,NodalPumpRequired,rPumpNew
    LOGICAL :: lPumpModified
    
    !If no pumping is defined return
    IF (.NOT. AppPumping%lThereIsPumping) RETURN
    
    !Initialize
    NLayers             = Stratigraphy%NLayers
    DeltaT              = TimeStep%DeltaT
    NodalPumpActual_Old = AppPumping%NodalPumpActual
    lPumpModified       = .FALSE.
    
    !Recompute pumping values in case a node is dried due to pumping
    DO indxLayer=1,NLayers
      DO indxNode=1,AppGrid%NNodes
        rNodalPumpActual_Old = NodalPumpActual_Old(indxNode,indxLayer)
        IF (rNodalPumpActual_Old .GE. 0.0) CYCLE
        BottomElev        = Stratigraphy%BottomElev(indxNode,indxLayer)
        IF (HN(indxNode,indxLayer) .EQ. HP(indxNode,indxLayer)) CYCLE
        NodalPumpRequired = AppPumping%NodalPumpRequired(indxNode,indxLayer)
        IF (HN(indxNode,indxLayer) .LT. BottomElev-STOPC) THEN
          rPumpNew = MIN(-(-rNodalPumpActual_Old*DeltaT-(BottomElev-HN(indxNode,indxLayer))*AS_P(indxNode,indxLayer))/DeltaT , 0.0)
        ELSE 
          rPumpNew = rNodalPumpActual_Old
          IF (NodalPumpRequired .LT. rNodalPumpActual_Old) & 
            rPumpNew = MIN(-MIN((HN(indxNode,indxLayer)-BottomElev)*AS_P(indxNode,indxLayer) , -NodalPumpRequired*DeltaT)/DeltaT , 0.0)
        END IF
        rPumpNew = (rPumpNew + rNodalPumpActual_Old)/2.0
        IF (ABS((rPumpNew - rNodalPumpActual_Old)/rNodalPumpActual_Old) .LT. STOPC) rPumpNew = rNodalPumpActual_Old
        IF (rPumpNew .NE. rNodalPumpActual_Old) THEN
          AppPumping%NodalPumpActual(indxNode,indxLayer) = rPumpNew 
          lPumpModified                                  = .TRUE.
        END IF     
      END DO
    END DO
    
    !If none of the pumping is modified, return
    IF (.NOT. lPumpModified) RETURN
    
    !Update element pumping and corresponding node-layer distribution factors
    IF (AppPumping%NElemPumps .GT. 0) &
      CALL UpdatePumpActualAndNodeFactors(AppPumping%ElemPumps,AppGrid,NLayers,AppPumping%NodalPumpActual,NodalPumpActual_Old)
    
    !Update well pumping and corresponding node-layer distribution factors
    IF (AppPumping%NWells .GT. 0) &
      CALL UpdatePumpActualAndNodeFactors(AppPumping%Wells,AppGrid,NLayers,AppPumping%NodalPumpActual,NodalPumpActual_Old)
    
  END SUBROUTINE ComputeNodalPumpActual
  
  
  ! -------------------------------------------------------------
  ! --- RESET IRRIGTAION FRACTIONS TO THOSE READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE ResetIrigFracs(AppPumping)
    CLASS(AppPumpingType) :: AppPumping
    
    !Wells
    CALL Supply_ResetIrigFracs(AppPumping%Wells%SupplyType)

    !Element pumps
    CALL Supply_ResetIrigFracs(AppPumping%ElemPumps%SupplyType)

  END SUBROUTINE ResetIrigFracs


END MODULE