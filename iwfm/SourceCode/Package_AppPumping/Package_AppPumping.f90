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
MODULE Package_AppPumping
  USE Class_Version               , ONLY: ReadVersion
  USE MessageLogger               , ONLY: SetLastMessage                           , &
                                          LogMessage                               , &
                                          EchoProgress                             , &
                                          MessageArray                             , &
                                          f_iFatal                                 , &
                                          f_iMessage        
  USE TimeSeriesUtilities         , ONLY: TimeStepType
  USE GeneralUtilities            , ONLY: StripTextUntilCharacter                  , &
                                          CleanSpecialCharacters                   , &
                                          EstablishAbsolutePathFileName            , &
                                          IntToText                                , &
                                          PrepareTitle                             , &
                                          ArrangeText                              , &
                                          f_cLineFeed
  USE IOInterface                 , ONLY: GenericFileType                          , & 
                                          iGetFileType_FromName                    , &
                                          f_iTXT                                   , &
                                          f_iDSS                                   , & 
                                          f_iUNKNOWN
  USE Package_Misc                , ONLY: RealTSDataInFileType                     , &
                                          FlowDestinationType                      , &
                                          ReadTSData                               , &
                                          PrepareTSDOutputFile                     , &
                                          f_rSmoothMaxP                            , &
                                          f_iGWComp                                , &
                                          f_iFlowDest_Outside                      , &
                                          f_iSupply_Well                           , &
                                          f_iSupply_ElemPump
  USE Package_Discretization      , ONLY: AppGridType                              , &
                                          StratigraphyType
  USE Package_ComponentConnectors , ONLY: SupplyType                               , &
                                          SupplyDestinationConnectorType           , &
                                          SupplyToDestinationType                  , &
                                          Supply_GetSupply                         , &
                                          Supply_GetDestination                    , &
                                          Supply_SetIrigFracsRead                  , &
                                          Supply_SetSupplySpecs                    , &
                                          Supply_ResetIrigFracs                    , &
                                          Supply_CheckSupplyDestinationConnection  
  USE Class_Pumping               , ONLY: PumpingType                              , &
                                          GetPumpPurpose                           , &
                                          UpdatePumpValues                         , &
                                          ComputePumpActual                        , &
                                          DistributePumpToNodes                    , &
                                          ComputerFPumpCol               
  USE Class_Well                  , ONLY: WellType                                 , &
                                          Well_New
  USE Class_ElementPumping        , ONLY: ElemPumpType                             , &
                                          ElemPump_New
  USE Class_PumpsAtElem           , ONLY: PumpsAtElemType                          , &
                                          PumpsAtElem_New
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
            f_iPump_Well                            , &
            f_iPump_ElemPump                          


  ! -------------------------------------------------------------
  ! --- PUMPING TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iPump_Well     = f_iSupply_Well     , &
                       f_iPump_ElemPump = f_iSupply_ElemPump
                       
                       
  ! -------------------------------------------------------------
  ! --- APPLICATION PUMPING TYPE
  ! -------------------------------------------------------------
  TYPE AppPumpingType
      PRIVATE
      LOGICAL                           :: lThereIsPumping  = .FALSE.                !Flag to check if there is at least one pumping (negative value) is specified
      INTEGER                           :: NWells           = 0                      !Number of wells simulated
      INTEGER                           :: NElemPumps       = 0                      !Number of element pumping simulated
      TYPE(WellType),ALLOCATABLE        :: Wells(:)                                  !Well data
      TYPE(ElemPumpType),ALLOCATABLE    :: ElemPumps(:)                              !Element pumping data
      TYPE(PumpsAtElemType),ALLOCATABLE :: WellsAtElems(:)                           !Well IDs at (element)
      TYPE(PumpsAtElemType),ALLOCATABLE :: ElemPumpsAtElems(:)                       !Element pumping IDs at (element)
      REAL(8),ALLOCATABLE               :: NodalPumpRequired(:,:)                    !Required pumping at (node,layer) combination
      REAL(8),ALLOCATABLE               :: NodalPumpActual(:,:)                      !Actual pumping at (node,layer) combination
      REAL(8)                           :: rPumpFactor      = 1.0                    !Pumping conversion factor
      TYPE(RealTSDataInFileType)        :: TSPumpFile                                !Relevant data for time series pumping data
      LOGICAL                           :: lElemWellPumpingOutFile_Defined = .FALSE. !Flag to check if element and well pumping output file is defined
      TYPE(GenericFileType)             :: ElemWellPumpingOutFile                    !Output file for element and well pumping
  CONTAINS
      PROCEDURE,PASS :: New                          
      PROCEDURE,PASS :: Kill                         
      PROCEDURE,PASS :: GetNWells                    
      PROCEDURE,PASS :: GetNElemPumps 
      PROCEDURE,PASS :: GetElemPumpIDs
      PROCEDURE,PASS :: GetWellIDs
      PROCEDURE,PASS :: GetElement                   
      PROCEDURE,PASS :: GetLayerFactors
      PROCEDURE,PASS :: GetActualNodeLayerPump_ForAPump
      PROCEDURE,PASS :: GetPumpingPurpose
      PROCEDURE,PASS :: GetPumpDestination
      PROCEDURE,PASS :: GetActualPumpingAtElementLayerNode 
      PROCEDURE,PASS :: GetSupply    
      PROCEDURE,PASS :: GetSupplySpecs
      PROCEDURE,PASS :: GetPumpActual                
      PROCEDURE,PASS :: GetSupplyAdjustData          
      PROCEDURE,PASS :: GetSubregionalPumping        
      PROCEDURE,PASS :: GetSubregionalRecharge       
      PROCEDURE,PASS :: GetNodalPumpActual
      PROCEDURE,PASS :: GetNodalPumpRequired
      PROCEDURE,PASS :: GetElementPumpActual       !Includes both element and well pumping at each element
      PROCEDURE,PASS :: GetiColAdjust                
      PROCEDURE,PASS :: IsDestinationToModelDomain   
      PROCEDURE,PASS :: SetIrigFracsRead             
      PROCEDURE,PASS :: SetSupplySpecs               
      PROCEDURE,PASS :: ReadTSData                    => AppPumping_ReadTSData        
      PROCEDURE,PASS :: UpdatePumpDistFactors        
      PROCEDURE,PASS :: ResetIrigFracs               
      PROCEDURE,PASS :: CheckSupplyDestinationConnection 
      PROCEDURE,PASS :: Simulate
      PROCEDURE,PASS :: ResetActualPumping
      PROCEDURE,PASS :: RestorePumpingToReadValues
      PROCEDURE,PASS :: PrintResults                  => PrintElemWellPumping
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
  SUBROUTINE New(AppPumping,lIsForInquiry,cFileName,cWorkingDirectory,AppGrid,Stratigraphy,TimeStep,iStat) 
    CLASS(AppPumpingType),INTENT(OUT) :: AppPumping
    LOGICAL,INTENT(IN)                :: lIsForInquiry
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
        CALL AppPumping%TSPumpFile%Init(cAbsPathFileName,cWorkingDirectory,'Time series pumping data file',TimeStep%TrackTime,1,.TRUE.,Factor,DummyArray,iStat=iStat)
        IF (iStat .EQ. -1) RETURN
        AppPumping%rPumpFactor = Factor(1)
        
        !Make sure that pumping factor is greater than or equal to zero
        IF (AppPumping%rPumpFactor .LT. 0.0) THEN
            MessageArray(1) = 'To avoid confusion, conversion factor in the timeseries pumping rate file cannot be less than zero!'
            MessageArray(2) = 'Pumping must be specified as a negative value in the data columns, and recharge as a positive value.'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Make sure that time series pumping data is defined if any well or element pumping is pointing a column in it
    IF (AppPumping%TSPumpFile%File%iGetFileType() .EQ. f_iUNKNOWN) THEN
        !Check with element pumping
        IF (AppPumping%NElemPumps .GT. 0) THEN
            IF (ANY(AppPumping%ElemPumps%iColPump.GT.0)) THEN
                CALL SetLastMessage('Time series pumping data must be specified when element pumping refers to a column in this file!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        !Check with wells
        IF (AppPumping%NWells .GT. 0) THEN
            IF (ANY(AppPumping%Wells%iColPump.GT.0)) THEN
                CALL SetLastMessage('Time series pumping data must be specified when well pumping refers to a column in this file!',f_iFatal,ThisProcedure)
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
        CALL SetLastMessage('Error in allocating memory for nodal pumping!',f_iFatal,ThisProcedure)
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
    
    !Read pumping output file name (BACKWARD COMPATIBILITY: CHECK IF ENTRY IS PROVIDED)
    CALL PumpDataFile%ReadData(ALine,iStat)
    IF (iStat .EQ. 0) THEN
        ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
        CALL CleanSpecialCharacters(ALine)
        IF (ALine .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ALine),cWorkingDirectory,cAbsPathFileName)
            CALL InitElemWellPumpingOutFile(lIsForInquiry,cAbsPathFileName,TimeStep%Unit,AppPumping%rPumpFactor,AppGrid%AppElement%ID,AppPumping%Wells%ID,AppPumping%ElemWellPumpingOutFile,iStat)
            IF (iStat .EQ. -1) RETURN
            AppPumping%lElemWellPumpingOutFile_Defined = .TRUE.
        END IF
    ELSE
        iStat = 0
    END IF
    
    !Close file
    CALL PumpDataFile%Kill()
      
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! ---INSTANTIATE PUMPING OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE InitElemWellPumpingOutFile(lIsForInquiry,cFileName,cUnitT,rPumpFactor,iElemIDs,iWellIDs,OutFile,iStat)
    LOGICAL,INTENT(IN)           :: lIsForInquiry
    CHARACTER(LEN=*),INTENT(IN)  :: cFileName,cUnitT
    REAL(8),INTENT(IN)           :: rPumpFactor
    INTEGER,INTENT(IN)           :: iElemIDs(:),iWellIDs(:)
    TYPE(GenericFileType)        :: OutFile
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26),PARAMETER :: ThisProcedure = ModName // 'InitElemWellPumpingOutFile'
    INTEGER                                :: iNCol,indxS,indxL,indxSO,indxLO,indx,iNElements,iNWells
    CHARACTER                              :: cFormatSpec*30,cTitleLines(1)*800,cHeaderFormat(5)*75,FPart(1)*32,   &
                                              cDataUnit(1)*10,cDataType(1)*10
    CHARACTER,ALLOCATABLE                  :: cHeaders(:,:)*50,CPart(:)*32,BPart(:)*32 
    
    !Initialize
    iStat      = 0
    iNElements = SIZE(iElemIDS)
    iNWells    = SIZE(iWellIDs)
    iNCol      = 2*(iNElements+iNWells)
    
    !Make sure that file is either text or DSS file
    IF (iGetFileType_FromName(cFileName) .NE. f_iTXT) THEN
        CALL SetLastMessage('Element/well pumping output file must be a text file!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    IF (lIsForInquiry) THEN
        CALL OutFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='ag. and urban element/well pumping output',iStat=iStat)
        RETURN
    ELSE
        CALL OutFile%New(FileName=cFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='ag. and urban element/well pumping output',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Print-out format
    cFormatSpec = '(A16,'//TRIM(IntToText(iNCol))//'F14.3)'
    
    !Title lines
    cTitleLines(1) = 'C' // REPEAT('-',100)
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C                    AGRICULTURAL AND URBAN PUMPING AT ELEMENTS AND WELLS'
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C                         (UNIT=Same as in Input Pumping Rate File)'
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C' // REPEAT('-',100)
    WRITE (cTitleLines(1),'(A,I14,A)')   TRIM(cTitleLines(1))//f_cLineFeed,iNCol      ,'      / NCOLPUMP' 
    WRITE (cTitleLines(1),'(A,F14.2,A)') TRIM(cTitleLines(1))//f_cLineFeed,rPumpFactor,'      / FACTPUMP' 
    WRITE (cTitleLines(1),'(A,I14,A)')   TRIM(cTitleLines(1))//f_cLineFeed,1          ,'      / NSPPUMP' 
    WRITE (cTitleLines(1),'(A,I14,A)')   TRIM(cTitleLines(1))//f_cLineFeed,0          ,'      / NFQPUMP'
    WRITE (cTitleLines(1),'(A,14X,A)')   TRIM(cTitleLines(1))//f_cLineFeed            ,'      / DSSFL' 
    cTitleLines(1) = TRIM(cTitleLines(1)) // f_cLineFeed // 'C' // REPEAT('-',100)
    
    !Column headers and format
    ALLOCATE (cHeaders(5,1+2*(iNElements+iNWells)))
    cHeaders(1,1) = 'C   TYPE'  
      indxS = 2          ;  indxL = indxS + iNElements - 1  ;  cHeaders(1,indxS:indxL) = 'Elem_Ag'  
      indxS = indxL + 1  ;  indxL = indxS + iNElements - 1  ;  cHeaders(1,indxS:indxL) = 'Elem_Urb'
      indxS = indxL + 1  ;  indxL = indxS + iNWells - 1     ;  cHeaders(1,indxS:indxL) = 'Well_Ag'
      indxS = indxL + 1  ;  indxL = indxS + iNWells - 1     ;  cHeaders(1,indxS:indxL) = 'Well_Urb'
    cHeaders(2,1) = 'C   ELEMENT/WELL'
      indxS = 2          ;  indxL = indxS + iNElements - 1  ;  cHeaders(2,indxS:indxL) = [(TRIM(IntToText(iElemIDs(indx))),indx=1,iNElements)]  ;  indxSO = indxS  ;  indxLO = indxL
      indxS = indxL + 1  ;  indxL = indxS + iNElements - 1  ;  cHeaders(2,indxS:indxL) = cHeaders(2,indxSO:indxLO)  
      indxS = indxL + 1  ;  indxL = indxS + iNWells - 1     ;  cHeaders(2,indxS:indxL) = [(TRIM(IntToText(iWellIDs(indx))),indx=1,iNWells)]  ;  indxSO = indxS  ;  indxLO = indxL 
      indxS = indxL + 1  ;  indxL = indxS + iNWells - 1     ;  cHeaders(2,indxS:indxL) = cHeaders(2,indxSO:indxLO)
    cHeaders(3,1) = 'C   COLUMN'
      cHeaders(3,2:) = [(TRIM(IntTotext(indx)),indx=1,iNCol)]
    cHeaders(4,1) = 'C      TIME'
      cHeaders(4,2:) = ''
    cHeaders(5,1) = 'C' // REPEAT('-',15)
      cHeaders(5,2:) = REPEAT('-',50)
    cHeaderFormat(1) = '(A8,8X,' // TRIM(IntToText(iNCol)) // 'A14)' 
    cHeaderFormat(2) = '(A16,' // TRIM(IntTotext(iNCol)) // 'A14)'
    cHeaderFormat(3) = '(A10,6X,' // TRIM(INTToText(iNCol)) // 'A14)'
    cHeaderFormat(4) = '(' // TRIM(IntToText(iNCol+1)) // 'A)'
    cHeaderFormat(5) = '(A16,' // TRIM(IntToText(iNCol)) // 'A14)'
    
    !Data unit and type for DSS files
    cDataUnit = ''
    cDataType = 'INST-VAL'
     
    !B, C and F parts for DSS files
    ALLOCATE (BPart(iNCol) , CPart(iNCol))
    indxS = 1          ;  indxL = indxS + iNElements - 1  ;  BPart(indxS:indxL) = [('E:'//TRIM(IntToText(iElemIDs(indx))),indx=1,iNElements)]  ;  CPart(indxS:indxL) = 'PUMPING_AG'  ;  indxSO = indxS  ; indxLO = indxL  
    indxS = indxL + 1  ;  indxL = indxS + iNElements - 1  ;  BPart(indxS:indxL) = BPart(indxSO:indxLO)  ;  CPart(indxS:indxL) = 'PUMPING_URB'
    indxS = indxL + 1  ;  indxL = indxS + iNWells - 1     ;  BPart(indxS:indxL) = [('WELL:'//TRIM(IntToText(iWellIDs(indx))),indx=1,iNWells)]  ;  CPart(indxS:indxL) = 'PUMPING_AG'  ;  indxSO = indxS  ; indxLO = indxL 
    indxS = indxL + 1  ;  indxL = indxS + iNWells - 1     ;  BPart(indxS:indxL) = BPart(indxSO:indxLO)  ;  CPart(indxS:indxL) = 'PUMPING_URB'
    FPart = 'ELEM_WELL_PUMPING'
    
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(OutFile                                           , &
                              NColumnsOfData          = iNCol                   , &
                              NRowsOfData             = 1                       , &
                              OverwriteNColumnsOfData = .FALSE.                 , &
                              FormatSpec              = TRIM(cFormatSpec)       , &
                              Title                   = cTitleLines             , &
                              Header                  = cHeaders                , &
                              HeaderFormat            = cHeaderFormat           , &
                              PrintColumnNo           = .FALSE.                 , &
                              DataUnit                = cDataUnit               , &
                              DataType                = cDataType               , &
                              CPart                   = CPart                   , &
                              FPart                   = FPart                   , &
                              UnitT                   = cUnitT                  , &
                              MiscArray               = BPart                   , &
                              iStat                   = iStat                   )

  END SUBROUTINE InitElemWellPumpingOutFile
  
  
  

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
    INTEGER              :: ErrorCode
    TYPE(AppPumpingType) :: Dummy
    
    DEALLOCATE (AppPumping%Wells                 , &
                AppPumping%ElemPumps             , &
                AppPumping%WellsAtElems          , &
                AppPumping%ElemPumpsAtElems      , &
                AppPumping%NodalPumpRequired     , &
                AppPumping%NodalPumpActual       , &
                STAT=ErrorCode                   ) 
    CALL AppPumping%TSPumpFile%Close()
    
    !Close element/well pumping output file
    CALL AppPumping%ElemWellPumpingOutFile%Kill()
  
    !Set the attributes to their default values
    SELECT TYPE (p => AppPumping)
        TYPE IS (AppPumpingType)
            p = Dummy
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
  ! --- GET THE PURPOSE OF PUMPING (IF IT SERVES AG, URBAN OR BOTH) BEFORE SUPPLY ADJUSTMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpingPurpose(AppPumping,iPumpType,iPumps,iAgOrUrban,iStat)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: iPumpType,iPumps(:)
    INTEGER,INTENT(OUT)              :: iAgOrUrban(:),iStat
    
    SELECT CASE (iPumpType)
        CASE (f_iPump_Well)
            CALL GetPumpPurpose(AppPumping%Wells(iPumps),iAgOrUrban,iStat)
            
        CASE (f_iPump_ElemPump)
            CALL GetPumpPurpose(AppPumping%ElemPumps(iPumps),iAgOrUrban,iStat)
    END SELECT
    
  END SUBROUTINE GetPumpingPurpose
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY DESTINATIONS
  ! -------------------------------------------------------------
  SUBROUTINE GetPumpDestination(AppPumping,iPumpType,Destination)
    CLASS(AppPumpingType),INTENT(IN)      :: AppPumping
    INTEGER,INTENT(IN)                    :: iPumpType
    TYPE(FlowDestinationType),ALLOCATABLE :: Destination(:)
    
    SELECT CASE (iPumpType)
        CASE (f_iPump_Well)
            CALL Supply_GetDestination(AppPumping%Wells , Destination)
            
        CASE (f_iPump_ElemPump)
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
        CASE (f_iPump_Well)
            ALLOCATE (SupplySpecs(AppPumping%NWells))
            SupplySpecs = AppPumping%Wells%SupplyType
        
        CASE (f_iPump_ElemPump)
            ALLOCATE (SupplySpecs(AppPumping%NElemPumps))
            SupplySpecs = AppPumping%ElemPumps%SupplyType
    END SELECT
        
  END SUBROUTINE GetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT ALL ELEMENTS (INCLUDES ELEMENT AND WELL PUMPING)
  ! -------------------------------------------------------------
  FUNCTION GetElementPumpActual(AppPumping,NElems) RESULT(ElemPumpActual)
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
    
  END FUNCTION GetElementPumpActual
  
  
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
      CASE (f_iPump_Well)
        iColAdjust = AppPumping%Wells%SupplyType%iColAdjust
        
      CASE (f_iPump_ElemPump)
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
    INTEGER                    :: indx  
    CLASS(PumpingType),POINTER :: pPumping(:)
    
    !Initialize
    SELECT CASE (iPumpType)
        CASE (f_iPump_Well)
            pPumping => AppPumping%Wells
        CASE (f_iPump_ElemPump)
            pPumping => AppPumping%ElemPumps
    END SELECT
    
    !Assign values to return variables
    DO indx=1,SIZE(pPumping)
        iColAdjust(indx)   = pPumping(indx)%iColAdjust
        PumpRequired(indx) = pPumping(indx)%SupplyRequired
        PumpMax(indx)      = -pPumping(indx)%PumpMax
        PumpActual(indx)   = pPumping(indx)%SupplyActual
        IrigFracs(indx)    = pPumping(indx)%IrigFrac
    END DO
    
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
  ! --- GET WELL IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetWellIDs(AppPumping,IDs)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(OUT)              :: IDs(:)
    
    IDs = AppPumping%Wells%ID
    
  END SUBROUTINE GetWellIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET ELEMENT PUMPING IDs
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetElemPumpIDs(AppPumping,IDs)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(OUT)              :: IDs(:)
    
    IDs = AppPumping%ElemPumps%ID
    
  END SUBROUTINE GetElemPumpIDs
  
   
  ! -------------------------------------------------------------
  ! --- GET ELEMENT NUMBER WHERE PUMPING OCCURS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetElement(AppPumping,indxPump,iPumpType) RESULT(iElem)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: indxPump,iPumpType
    INTEGER                          :: iElem
    
    SELECT CASE (iPumpType)
      CASE (f_iPump_Well)
        iElem = AppPumping%Wells(indxPump)%Element
        
      CASE (f_iPump_ElemPump)
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
      CASE (f_iPump_Well)
        Factors = AppPumping%Wells(indxPump)%rLayerFactor
        
      CASE (f_iPump_ElemPump)
        Factors = AppPumping%ElemPumps(indxPump)%rLayerFactor
      
    END SELECT
    
  END SUBROUTINE GetLayerFactors


  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT NODES AND LAYERS FOR A PUMP 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetActualNodeLayerPump_ForAPump(AppPumping,indxPump,iPumpType,rPump)
    CLASS(AppPumpingType),INTENT(IN) :: AppPumping
    INTEGER,INTENT(IN)               :: indxPump,iPumpType
    REAL(8),INTENT(OUT)              :: rPump(:,:)
    
    SELECT CASE (iPumpType)
        CASE (f_iPump_Well)
            IF (AppPumping%Wells(indxPump)%SupplyActual .EQ. 0.0) THEN
                rPump = 0.0
            ELSE
                rPump = AppPumping%Wells(indxPump)%rNodePumpActual / AppPumping%Wells(indxPump)%SupplyActual
            END IF
          
        CASE (f_iPump_ElemPump)
            IF (AppPumping%ElemPumps(indxPump)%SupplyActual .EQ. 0.0) THEN
                rPump = 0.0
            ELSE
                rPump = AppPumping%ElemPumps(indxPump)%rNodePumpActual / AppPumping%ElemPumps(indxPump)%SupplyActual
            END IF
      
    END SELECT
    
  END SUBROUTINE GetActualNodeLayerPump_ForAPump
  
  
  ! -------------------------------------------------------------
  ! --- GET ACTUAL PUMPING AT (ELEMENT,LAYER) 
  ! -------------------------------------------------------------
  FUNCTION GetActualPumpingAtElementLayerNode(AppPumping,iElem,iLayer,indxNode,iPumpType) RESULT(Pumping)
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
        CASE (f_iPump_ElemPump)
            PumpsAtElem =  AppPumping%ElemPumpsAtElems(iElem)
            pPumps      => AppPumping%ElemPumps%PumpingType
        
        CASE (f_iPump_Well)
            PumpsAtElem =  AppPumping%WellsAtElems(iElem)
            pPumps      => AppPumping%Wells%PumpingType

    END SELECT
    
    DO indxPump=1,PumpsAtElem%nPumps
        iPump   = PumpsAtElem%iPumpIDs(indxPump)
        Pumping = Pumping + pPumps(iPump)%rNodePumpActual(indxNode,iLayer)
    END DO
    
    !Release memory
    NULLIFY(pPumps)

  END FUNCTION GetActualPumpingAtElementLayerNode
  
  
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
      CASE (f_iPump_Well)
        ALLOCATE (PumpActual(AppPumping%NWells))
        DO indxPump=1,AppPumping%NWells
            IF (AppPumping%Wells(indxPump)%PumpRead .GT. 0.0) THEN
                PumpActual(indxPump) = AppPumping%Wells(indxPump)%PumpRead
            ELSE
                PumpActual(indxPump) = -AppPumping%Wells(indxPump)%SupplyActual
            END IF
        END DO
        
      CASE (f_iPump_ElemPump)
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
        CALL Supply_SetIrigFracsRead(AppPumping%ElemPumps,IrigFrac) 
    
  END SUBROUTINE SetIrigFracsRead
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY SPECS
  ! -------------------------------------------------------------
  SUBROUTINE SetSupplySpecs(AppPumping,SupplyDestConnector,iPumpType,PumpRequired,IrigFracs,SupplyToDest)
    CLASS(AppPumpingType)                    :: AppPumping
    TYPE(SupplyDestinationConnectorType)     :: SupplyDestConnector
    INTEGER,INTENT(IN)                       :: iPumpType
    REAL(8),INTENT(IN)                       :: PumpRequired(:),IrigFracs(:)
    TYPE(SupplyToDestinationType),INTENT(IN) :: SupplyToDest(:)
    
    SELECT CASE (iPumpType)
        CASE (f_iPump_Well)
            CALL Supply_SetSupplySpecs(AppPumping%Wells,SupplyDestConnector,PumpRequired,IrigFracs,SupplyToDest)
        
        CASE (f_iPump_ElemPump)
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
    IF (AppPumping%TSPumpFile%File%iGetFileType() .EQ. f_iUNKNOWN) THEN
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
            CALL AppPumping%RestorePumpingToReadValues(AppGrid,Stratigraphy,HHydCond,HeadGW)
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
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PRINT ELEMENT AND WELL LEVEL PUMPING 
  ! -------------------------------------------------------------
  SUBROUTINE PrintElemWellPumping(AppPumping,iNElements,lEndOfSimulation,TimeStep)
    CLASS(AppPumpingType)         :: AppPumping
    INTEGER,INTENT(IN)            :: iNElements
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
     
    !Local variables
    INTEGER   :: indxPump,iElem,iNWells
    REAL(8)   :: rDummyArray(2*(iNElements+AppPumping%NWells)),rPumpFactor
    CHARACTER :: cSimulationTime*21
    
    IF (.NOT. AppPumping%lElemWellPumpingOutFile_Defined) RETURN
    
    !Initailize
    iNWells                     = AppPumping%NWells
    rDummyArray(1:2*iNElements) = 0.0
    rPumpFactor                 = 1D0 / AppPumping%rPumpFactor
    
    !Process ag and urban element pumping
    DO indxPump=1,AppPumping%NElemPumps
        iElem                         = AppPumping%ElemPumps(indxPump)%Element
        rDummyArray(iElem)            = rDummyArray(iElem) - AppPumping%ElemPumps(indxPump)%SupplyActual * AppPumping%ElemPumps(indxPump)%IrigFrac * rPumpFactor
        rDummyArray(iNElements+iElem) = rDummyArray(iNElements+iElem) - AppPumping%ElemPumps(indxPump)%SupplyActual * (1D0 - AppPumping%ElemPumps(indxPump)%IrigFrac) * rPumpFactor
    END DO
    
    !Process ag and urban well pumping
    DO indxPump=1,AppPumping%NWells
        rDummyArray(2*iNElements+indxPump)         = -AppPumping%Wells(indxPump)%SupplyActual * AppPumping%Wells(indxPump)%IrigFrac * rPumpFactor
        rDummyArray(2*iNElements+iNWells+indxPump) = -AppPumping%Wells(indxPump)%SupplyActual * (1D0 - AppPumping%Wells(indxPump)%IrigFrac) * rPumpFactor
    END DO
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
        cSimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
        WRITE(cSimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print out the results
    CALL AppPumping%ElemWellPumpingOutFile%WriteData(cSimulationTime,rDummyArray,FinalPrint=lEndOfSimulation)
    

  END SUBROUTINE PrintElemWellPumping
  
  
  
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
  ! --- RESTORE PUMPING TO READ VALUES
  ! -------------------------------------------------------------
  SUBROUTINE RestorePumpingToReadValues(AppPumping,AppGrid,Stratigraphy,HHydCond,HeadGW)
    CLASS(AppPumpingType)             :: AppPumping
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: HHydCond(:,:),HeadGW(:,:)
    
    !Local variables
    INTEGER :: indxPump
    
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
    
    !Distribute pumping to nodes
    CALL DistPumpToNodes(AppGrid,Stratigraphy,HHydCond,HeadGW,AppPumping)
    
  END SUBROUTINE RestorePumpingToReadValues
  
  
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
      IF (AppPumping%Wells(indxPump)%Destination%iDestType .NE. f_iFlowDest_Outside) THEN
        lDest = .TRUE.
        RETURN
      END IF
    END DO
    
    !Check element pumping
    DO indxPump=1,AppPumping%NElemPumps
      IF (AppPumping%ElemPumps(indxPump)%Destination%iDestType .NE. f_iFlowDest_Outside) THEN
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
    INTEGER                                :: indx,indxElem,ErrorCode,NVertex,Vertex(4),iElemID,iWellID
    
    !Initialize
    iStat     = 0
    ErrorCode = 0
    
    !Check wells
    DO indx=1,AppPumping%NWells
        indxElem = AppPumping%Wells(indx)%Element
        NVertex  = AppGrid%NVertex(indxElem)
        Vertex   = AppGrid%Vertex(:,indxElem)
        IF (ALL(Stratigraphy%ActiveNode(Vertex(1:NVertex),:) .EQ. .FALSE.)) THEN
            iWellID = AppPumping%Wells(indx)%ID
            iElemID = AppGrid%AppElement(indxElem)%ID
            WRITE (MessageArray(1),'(A10,i6,A12,i8)') 'Well ID = ',iWellID,' at element ',iElemID
            CALL LogMessage(MessageArray(1),f_iMessage,ThisProcedure)
            ErrorCode = 1
        END IF
    END DO
    
    !Check element pumping
    DO indx=1,AppPumping%NElemPumps
        indxElem = AppPumping%ElemPumps(indx)%Element
        NVertex  = AppGrid%NVertex(indxElem)
        Vertex   = AppGrid%Vertex(:,indxElem)
        IF (ALL(Stratigraphy%ActiveNode(Vertex(1:NVertex),:) .EQ. .FALSE.)) THEN
            iElemID = AppGrid%AppElement(indxElem)%ID
            WRITE (MessageArray(1),'(A28,i8)') 'Elem. Pump at element ',iElemID
            CALL LogMessage(MessageArray(1),f_iMessage,ThisProcedure)
            ErrorCode = 1
        END IF
    END DO
    
    !Stop the program if necessary
    IF (ErrorCode .GT. 0) THEN
        MessageArray(1) = 'Above elements for pumping have all their surrounding nodes inactive!'
        MessageArray(2) = 'Pumping at these elements are redundent.'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
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
  SUBROUTINE Simulate(AppPumping,AppGrid,Stratigraphy,rStorage,rdStorage,Matrix)
    CLASS(AppPumpingType)             :: AppPumping
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: rStorage(:,:),rdStorage(:,:)
    TYPE(MatrixType)                  :: Matrix
    
    !Local variables
    INTEGER                                                      :: indxNode,indxLayer,iGWNode(1),NLayers,indx
    REAL(8)                                                      :: rUpdateCOEFF(1),rDiffQ,rDiffQSQRT,rPumpRequired, &
                                                                    rUpdateRHS(AppGrid%NNodes*Stratigraphy%NLayers), &
                                                                    rStoragePos,rStor,rStorSQRT
    REAL(8),DIMENSION(SIZE(rStorage,DIM=1),SIZE(rStorage,DIM=2)) :: rNodalPumpActual_New,rNodalPumpActual_Old
    INTEGER,PARAMETER                                            :: iCompIDs(1) = [f_iGWComp]
    
    !Initialize
    NLayers = Stratigraphy%NLayers
    
    !Keep the original actual pumping
    rNodalPumpActual_Old = AppPumping%NodalPumpActual
    
    !Loop through nodes and layers
    indx = 0
    DO indxLayer=1,NLayers
        DO indxNode=1,AppGrid%NNodes
            indx = indx + 1
            
            !Convert the sign of required pumping to be compared to storage and applied to RHS
            rPumpRequired = -AppPumping%NodalPumpRequired(indxNode,indxLayer)
            
            !If pumping is zero, cycle
            IF (rPumpRequired .EQ. 0.0) THEN
                rNodalPumpActual_New(indxNode,indxLayer) = 0.0
                rUpdateRHS(indx)                         = 0.0
                CYCLE
            END IF
            
            !If this is recharge, cycle (negative sign means recharge)
            IF (rPumpRequired .LT. 0.0) THEN
                rNodalPumpActual_New(indxNode,indxLayer) = -rPumpRequired
                rUpdateRHS(indx)                         = rPumpRequired
                CYCLE
            END IF
            
            !Actual pumping, limited by available storage
            rStor                                    = rStorage(indxNode,indxLayer)
            rStoragePos                              = MAX(rStor , 0.0)
            rNodalPumpActual_New(indxNode,indxLayer) = rPumpRequired - MAX(rPumpRequired-rStoragePos , 0.0)
            rUpdateRHS(indx)                         = rNodalPumpActual_New(indxNode,indxLayer)
            
            !Update Jacobian; use Jacobian smoothing
            rDiffQ          = rPumpRequired - rStoragePos
            rDiffQSQRT      = SQRT(rDiffQ*rDiffQ + f_rSmoothMaxP)
            rStorSQRT       = SQRT(rStor*rStor + f_rSmoothMaxP)  
            rUpdateCOEFF(1) = 0.25d0 * (1d0+rDiffQ/rDiffQSQRT) * (1d0+rStor/rStorSQRT) * rdStorage(indxNode,indxLayer)
            iGWNode(1)      = indx
            CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode(1),1,iCompIDs,iGWNode,rUpdateCOEFF)

            !Convert sign of actual pumping back to what it is supposed to be
            rNodalPumpActual_New(indxNode,indxLayer) = -rNodalPumpActual_New(indxNode,indxLayer)

            !Store the newly computed actual pumping
            AppPumping%NodalPumpActual(indxNode,indxLayer) = rNodalPumpActual_New(indxNode,indxLayer)
        END DO
    END DO
    
    !Update RHS vector
    CALL Matrix%UpdateRHS(f_iGWComp,1,rUpdateRHS)
       
    !Update element pumping and corresponding node-layer distribution factors based on new actual nodal pumping
    IF (AppPumping%NElemPumps .GT. 0) &
        CALL ComputePumpActual(AppPumping%ElemPumps,AppGrid,NLayers,rNodalPumpActual_New,AppPumping%NodalPumpRequired)
    
    !Update well pumping and corresponding node-layer distribution factors based on new actual nodal pumping
    IF (AppPumping%NWells .GT. 0) &
        CALL ComputePumpActual(AppPumping%Wells,AppGrid,NLayers,rNodalPumpActual_New,AppPumping%NodalPumpRequired)
    
  END SUBROUTINE Simulate
  
  
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