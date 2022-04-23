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
MODULE Class_AppBC
  USE GeneralUtilities        
  USE TimeSeriesUtilities     , ONLY: TimeStepType            , &
                                      TimeIntervalConversion
  USE IOInterface             
  USE MessageLogger           , ONLY: SetLastMessage          , &
                                      EchoProgress            , &
                                      MessageArray            , &
                                      iFatal
  USE Package_Misc            , ONLY: RealTSDataInFileType    , &
                                      PrepareTSDOutputFile    
  USE Package_Discretization  , ONLY: AppGridType             , &
                                      StratigraphyType        , &
                                      AppFaceType
  USE Class_LayerBC
  USE Class_TSBCDataFile
  USE Package_Matrix          , ONLY: MatrixType
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
  PUBLIC :: AppBCType                              
  
  
  ! -------------------------------------------------------------
  ! --- BOUNDARY NODE FLOW OUTPUT DATA TYPE
  ! -------------------------------------------------------------
  TYPE BCFlowOutputType
      TYPE(GenericFileType) :: OutFile         !Hydrograph output file
      INTEGER               :: NHyd       = 0  !Number of hydrograhs to be printed
      INTEGER,ALLOCATABLE   :: iBCNodes(:)     !Boundary node list for flow hydrograph print-out
      INTEGER,ALLOCATABLE   :: iLayers(:)      !Aquifer layers for boundary node hydrograph print-out
  END TYPE BCFlowOutputType
  
  
  ! -------------------------------------------------------------
  ! --- BOUNDARY CONDITIONS DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE AppBCType
      PRIVATE
      TYPE(LayerBCType),ALLOCATABLE      :: LayerBC(:)                          !Boundary conditions for each aquifer layers
      CHARACTER(LEN=6)                   :: TimeUnit_SpecifiedFlowBC = ''       !Time unit for the specified flow b.c.
      CHARACTER(LEN=6)                   :: TimeUnit_GHBC            = ''       !Time unit for the conductance term of general head b.c.
      CHARACTER(LEN=6)                   :: TimeUnit_ConstrainedGHBC = ''       !Time unit for the maximum flow and conductance terms of constrained general head b.c.
      TYPE(TSBCDataFileType)             :: TSBCDataFile                        !Time series b.c. input data file
      TYPE(BCFlowOutputType),ALLOCATABLE :: BCFlowOutput                        !Boundary node flow print-out data
      LOGICAL                            :: lBCFlowOutput_Defined    = .FALSE.  !Flag to check if boundary node flow print-out is defined
  CONTAINS
      PROCEDURE,PASS :: New                               
      PROCEDURE,PASS :: Kill                              
      PROCEDURE,PASS :: GetNNodesWithBCType               
      PROCEDURE,PASS :: GetNodesWithBCType    
      PROCEDURE,PASS :: GetBoundaryFlowAtFaceLayer
      PROCEDURE,PASS :: GetBoundaryFlowAtElementNodeLayer 
      PROCEDURE,PASS :: GetSubregionalFlows               
      PROCEDURE,PASS :: IsDefined  
      PROCEDURE,PASS :: IsBoundaryFlowNode
      PROCEDURE,PASS :: ReadTSData                         => AppBC_ReadTSData                       
      PROCEDURE,PASS :: PrintResults                      
      PROCEDURE,PASS :: ConvertTimeUnit                   
      PROCEDURE,PASS :: ResetSpecifiedHeadBC
      PROCEDURE,PASS :: Simulate
  END TYPE AppBCType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 13
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_AppBC::'

  
  
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
  ! --- INSTANTIATE APPLICATION BOUNDARY CONDITIONS
  ! -------------------------------------------------------------
  SUBROUTINE New(AppBC,IsForInquiry,cFileName,cWorkingDirectory,AppGrid,Stratigraphy,UNITVLOU,TimeStep,GWHeads,iStat)
    CLASS(AppBCType),INTENT(OUT)      :: AppBC
    LOGICAL,INTENT(IN)                :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,cWorkingDirectory,UNITVLOU
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    REAL(8)                           :: GWHeads(:,:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: NNodes,NLayers,ErrorCode,NFlowBCCols,NHeadBCCols
    INTEGER,ALLOCATABLE         :: iTSFlowBCColumns(:),iTSHeadBCColumns(:)
    CHARACTER                   :: cErrorMsg*500,ALine*1500
    TYPE(GenericFileType)       :: BCFile
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    !Return if no filename is specified
    IF (cFileName .EQ. '') RETURN
    
    !Inform user
    CALL EchoProgress('   Instantiating groundwater boundary conditions...')
    
    !Initialize
    NNodes  = AppGrid%NNodes
    NLayers = Stratigraphy%NLayers
    
    !Allocate memory
    ALLOCATE (AppBC%LayerBC(NLayers) ,STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for groundwater boundary conditions for each layer.'//NEW_LINE('')//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL BCFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='main groundwater boundary conditions data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Specified flow b.c.
    CALL BCFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LayerBC_InitSpecifiedFlowBC(cAbsPathFileName,NNodes,Stratigraphy,AppBC%TimeUnit_SpecifiedFlowBC,AppBC%LayerBC,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !Specified head b.c.
    CALL BCFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LayerBC_InitSpecifiedHeadBC(cAbsPathFileName,NNodes,Stratigraphy,GWHeads,AppBC%LayerBC,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !General head b.c.
    CALL BCFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LayerBC_InitGeneralHeadBC(cAbsPathFileName,NNodes,Stratigraphy,AppBC%TimeUnit_GHBC,AppBC%LayerBC,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Constrained general head b.c.
    CALL BCFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (ALine .NE. '') THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
        CALL LayerBC_InitConstrainedGeneralHeadBC(cAbsPathFileName,NNodes,Stratigraphy,AppBC%TimeUnit_ConstrainedGHBC,AppBC%LayerBC,iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Compile column numbers for time series flow and head b.c.
    CALL LayerBC_GetTSFlowBCColumns(AppBC%LayerBC,iTSFlowBCColumns,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL LayerBC_GetTSHeadBCColumns(AppBC%LayerBC,iTSHeadBCColumns,iStat)  ;  IF (iStat .EQ. -1) RETURN
    NFlowBCCols = SIZE(iTSFlowBCColumns)
    NHeadBCCols = SIZE(iTSHeadBCColumns)
    
    !Time series boundary conditions data file
    CALL BCFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    ALine = StripTextUntilCharacter(ALine,'/')  
    CALL CleanSpecialCharacters(ALine)
    IF (NFlowBCCols .GT. 0   .OR.   NHeadBCCols .GT. 0) THEN
        IF (ALine .NE. '') THEN
            CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(ALine)),cWorkingDirectory,cAbsPathFileName)
            CALL TSBCDataFile_New(cAbsPathFileName,iTSFlowBCColumns,TimeStep,AppBC%TSBCDataFile,iStat)
            IF (iStat .EQ. -1) RETURN
        ELSE
            MessageArray(1) = 'Time Series Boundary Conditions Data File must be specified when'
            MessageArray(2) = 'one or more time series boundary condition data columns are referred!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Make sure there are no errors in b.c. data
    CALL LayerBC_CheckConsistency(AppBC%LayerBC,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Apply time series b.c. that is initially read
    IF (AppBC%TSBCDataFile%lUpdated)  &
        CALL LayerBC_SetTSBoundaryConditions(Stratigraphy%BottomElev,AppBC%TSBCDataFile%rValues,GWHeads,AppBC%LayerBC)
    
    !Instantiate boundary node flow hydrograph output data
    ALLOCATE (AppBC%BCFlowOutput , STAT=ErrorCode ,ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for boundary node flow hydrograph printing!'//NEW_LINE('')//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL BCFlowOutput_New(IsForInquiry,BCFile,cWorkingDirectory,NLayers,AppBC%LayerBC,UNITVLOU,TimeStep,AppBC%BCFlowOutput,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (AppBC%BCFlowOutput%OutFile%iGetFileType() .EQ. UNKNOWN) THEN
        DEALLOCATE (AppBC%BCFlowOutput , STAT=ErrorCode)
    ELSE
        AppBC%lBCFlowOutput_Defined = .TRUE.
    END IF
        
    !Clear memory and close input file
    DEALLOCATE (iTSFlowBCColumns , iTSHeadBCColumns , STAT=ErrorCode)
    CALL BCFile%Kill()
             
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- NEW BOUNDARY NODE FLOW OUTPUT DATASET
  ! -------------------------------------------------------------
  SUBROUTINE BCFlowOutput_New(IsForInquiry,InFile,cWorkingDirectory,NLayers,LayerBC,UNITVLOU,TimeStep,BCFlowOutput,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    TYPE(GenericFileType)         :: InFile
    INTEGER,INTENT(IN)            :: NLayers
    TYPE(LayerBCType),INTENT(IN)  :: LayerBC(:)
    CHARACTER(LEN=*),INTENT(IN)   :: cWorkingDirectory,UNITVLOU
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(BCFlowOutputType)        :: BCFlowOutput
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'BCFlowOutput_New'
    INTEGER                      :: NOUTB,ErrorCode,indx,indx1,ID,iHydLayer,iHydNode,iDummyArray(3), &
                                    iLoc
    CHARACTER                    :: cFileName*1200,ALine*1000,cErrorMsg*300
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    
    !Initialize
    iStat = 0

    !Read the general data
    CALL InFile%ReadData(NOUTB,iStat)      ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(cFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    cFileName = StripTextUntilCharacter(cFileName,'/')  
    CALL CleanSpecialCharacters(cFileName)
    
    !If the filename is empty or the number of hydrographs is set to zero, return
    IF (cFileName .EQ. '') THEN
        DO indx=1,NOUTB
            CALL InFile%ReadData(ALine,iStat)  
            IF (iStat .EQ. -1) RETURN
        END DO
        RETURN
    ELSE
        IF (NOUTB .EQ. 0) RETURN
    END IF
    
    !Set the NHyd variable
    BCFlowOutput%NHyd = NOUTB
    
    !Allocate memory
    ALLOCATE (BCFlowOutput%iBCNodes(NOUTB) , BCFlowOutput%iLayers(NOUTB) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for boundary node flow hydrograph list!'//NEW_LINE(' ')//TRIM(cErrorMsg),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read and process data
    DO indx=1,NOUTB
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)  
        ALine = ADJUSTL(ALine)
        DO indx1=1,3
            READ (ALine,*) iDummyArray(indx1)
            iLoc = FirstLocation(' ',ALine)
            IF (iLoc .EQ. 0) THEN
                CALL SetLastMessage('Error in data entry for face flow hydrograph specification '//TRIM(IntToText(indx))//'!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            ALine = ADJUSTL(ALine(iLoc:LEN_TRIM(ALine)))
        END DO
        ID        = iDummyArray(1)
        iHydLayer = iDummyArray(2)
        iHydNode  = iDummyArray(3)

        !Make sure hydrographs are entered sequentially
        IF (ID .NE. indx) THEN
            MessageArray(1) = 'Boundary node flow hydrograph print-out specifications must be entered sequentially.'
            MessageArray(2) = 'Expected hydrograph ID = ' // TRIM(IntToText(indx))
            MessageArray(3) = 'Entered hydrograph ID  = ' // TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure layer is modeled
        IF (iHydLayer .LT. 1   .OR.   iHydLayer .GT. NLayers) THEN
            CALL SetLastMessage('Boundary node flow hydrograph layer listed for hydrograph ID '//TRIM(IntToText(ID))//' is outside model bounds!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Check if the listed nodes are actually boundary nodes
        IF (LayerBC_IsBCNode(iHydNode,LayerBC(iHydLayer))) THEN
            BCFlowOutput%iLayers(indx)  = iHydLayer
            BCFlowOutput%iBCNodes(indx) = iHydNode
        ELSE
            MessageArray(1) = 'Node '//TRIM(IntToText(iHydNode))//' in layer '//TRIM(IntToText(iHydLayer))//' for boundary flow printing'
            MessageArray(2) = 'is not specified as a boundary node.'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure) 
            iStat = -1
            RETURN
        END IF           

    END DO
    
    !Instantiate the output file 
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cFileName)),cWorkingDirectory,cAbsPathFileName)
    CALL PrepBCFlowOutFile(IsForInquiry,cAbsPathFileName,UNITVLOU,NOUTB,BCFlowOutput%iBCNodes,BCFlowOutput%iLayers,TimeStep,BCFlowOutput%OutFile,iStat)

  END SUBROUTINE BCFlowOutput_New

  


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
  ! --- KILL BOUNDARY CONDITIONS DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppBC)
    CLASS(AppBCType) :: AppBC
    
    !Local variables
    INTEGER :: ErrorCode
    
    CALL LayerBC_Kill(AppBC%LayerBC)
    DEALLOCATE(AppBC%LayerBC , STAT=ErrorCode)
    
    CALL TSBCDataFile_Kill(AppBC%TSBCDataFile)
    
    IF (AppBC%lBCFlowOutput_Defined) THEN
        CALL BCFlowOutput_Kill(AppBC%BCFlowOutput)
        DEALLOCATE (AppBC%BCFlowOutput)
    END IF
    
    AppBC%TimeUnit_SpecifiedFlowBC = ''
    AppBC%TimeUnit_GHBC            = ''
    AppBC%TimeUnit_ConstrainedGHBC = ''
    AppBC%lBCFlowOutput_Defined    = .FALSE.
    
    
  END SUBROUTINE Kill
  
 
  ! -------------------------------------------------------------
  ! --- KILL B.C. FLOW OUTPUT DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE BCFlowOutput_Kill(BCFlowOutput)
    TYPE(BCFlowOutputType) :: BCFlowOutput
    
    !Local variables
    INTEGER                :: ErrorCode
    TYPE(BCFlowOutputType) :: Dummy
    
    CALL BCFlowOutput%OutFile%Kill()
    DEALLOCATE (BCFlowOutput%iBCNodes , &
                BCFlowOutput%iLayers  , &
                STAT = ErrorCode      )
    
    BCFlowOutput = Dummy
    
  END SUBROUTINE BCFlowOutput_Kill
  

  
  
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
  ! --- GET SUBREGIONAL BOUNDARY FLOWS
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalFlows(AppBC,AppGrid) RESULT(rRegionFlows)
    CLASS(AppBCType),INTENT(IN)  :: AppBC
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8)                      :: rRegionFlows(AppGrid%NSubregions)
    
    !Local variables
    INTEGER :: indxLayer
    
    !Initialize
    rRegionFlows = 0.0
    
    !Compile subregional flows
    DO indxLayer=1,SIZE(AppBC%LayerBC)
        !Specified flow b.c.
        rRegionFlows = rRegionFlows + SubregionalFlowsForBCType(indxLayer,SpFlowBCID,AppGrid,AppBC)
        
        !Specified head b.c.
        rRegionFlows = rRegionFlows + SubregionalFlowsForBCType(indxLayer,SpHeadBCID,AppGrid,AppBC)
        
        !General head b.c.
        rRegionFlows = rRegionFlows + SubregionalFlowsForBCType(indxLayer,GHBCID,AppGrid,AppBC)
        
        !Constrained general head b.c.
        rRegionFlows = rRegionFlows + SubregionalFlowsForBCType(indxLayer,ConstrainedGHBCID,AppGrid,AppBC)
    END DO
    
    
  CONTAINS
  
     
    !##############################################################
    !###  COMPILE SUBREGIONAL BOUNDARY FLOWS FOR A GIVEN B.C. TYPE
    !##############################################################
    FUNCTION SubregionalFlowsForBCType(iLayer,iBCType,AppGrid,AppBC) RESULT(rRegionFlows)
      INTEGER,INTENT(IN)           :: iLayer,iBCType
      TYPE(AppGridType),INTENT(IN) :: AppGrid
      TYPE(AppBCType),INTENT(IN)   :: AppBC
      REAL(8)                      :: rRegionFlows(AppGrid%NSubregions)
      
      !Local variables
      INTEGER             :: NBCNodes,indxNode,iNode,indxElem,iElem,iRegion,indxVertex
      INTEGER,ALLOCATABLE :: iBCNodes(:)
      REAL(8)             :: rFlow
      
      !Initialize
      rRegionFlows = 0.0

      !Get the number and list of b.c. nodes with the defined b.c. type
      CALL AppBC%GetNodesWithBCType(iLayer,iBCType,iBCNodes)
      NBCNodes = SIZE(iBCNodes)
      
      !Compile b.c. flows into subregional flows
      DO indxNode=1,NBCNodes
          iNode = iBCNodes(indxNode)
          DO indxElem=1,SIZE(AppGrid%AppNode(iNode)%SurroundingElement)
              iElem                 = AppGrid%AppNode(iNode)%SurroundingElement(indxElem)
              iRegion               = AppGrid%AppElement(iElem)%Subregion
              indxVertex            = LocateInList(iNode,AppGrid%Element(iElem)%Vertex) 
              CALL AppBC%GetBoundaryFlowAtElementNodeLayer(iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow)
              rRegionFlows(iRegion) = rRegionFlows(iRegion) + rFlow
          END DO
      END DO

    END FUNCTION SubregionalFlowsForBCType
      
    
  END FUNCTION GetSubregionalFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FACE FLOW AT A LAYER
  ! --- NOte: It is assumed that iFace is a boundary face
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtFaceLayer(AppBC,AppGrid,iFace,iLayer,rFlow)
    CLASS(AppBCType),INTENT(IN)  :: AppBC
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(IN)           :: iFace,iLayer
    REAL(8),INTENT(OUT)          :: rFlow
    
    !Local variables
    INTEGER :: iNodes(2),indxNode,iNode,iElem,iVertex
    REAL(8) :: rFlowTemp
    
    !Initialize
    rFlow  = 0.0
    iNodes = AppGrid%AppFace(iFace)%Node
    iElem  = MAXVAL(AppGrid%AppFace(iFace)%Element)
    
    !Find flows for each node
    DO indxNode=1,2
        iNode   = iNodes(indxNode)
        
        !Cycle if there is no b.c. defined for this node
        IF (.NOT. LayerBC_IsBCNode(iNode,AppBC%LayerBC(iLayer))) CYCLE
        
        !Vertex index for the element
        iVertex = LocateInList(iNode,AppGrid%Element(iElem)%Vertex)
        
        !Specified flow
        CALL GetBoundaryFlowAtElementNodeLayer(AppBC,SpFlowBCID,iElem,iVertex,iLayer,AppGrid,rFlowTemp)
        rFlow = rFlow - rFlowTemp
        
        !Specified head
        CALL GetBoundaryFlowAtElementNodeLayer(AppBC,SpHeadBCID,iElem,iVertex,iLayer,AppGrid,rFlowTemp)
        rFlow = rFlow - rFlowTemp
        
        !General head boundary condition
        CALL GetBoundaryFlowAtElementNodeLayer(AppBC,GHBCID,iElem,iVertex,iLayer,AppGrid,rFlowTemp)
        rFlow = rFlow - rFlowTemp
        
        !Constrained general head boundary condition
        !This type of b.c. is assumed to flow into system vertically so it won't have a horizontal flow component
        
    END DO
      
  END SUBROUTINE GetBoundaryFlowAtFaceLayer
  
  
  ! -------------------------------------------------------------
  ! --- GET BOUNDARY FLOW TO ELEMENT AT ITS VERTEX WITH DEFINED B.C. AT A LAYER
  ! -------------------------------------------------------------
  SUBROUTINE GetBoundaryFlowAtElementNodeLayer(AppBC,iBCType,iElem,indxVertex,iLayer,AppGrid,rFlow,lAddToRHS)
    CLASS(AppBCType),INTENT(IN)  :: AppBC
    INTEGER,INTENT(IN)           :: iBCType,iElem,indxVertex,iLayer
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    REAL(8),INTENT(OUT)          :: rFlow
    LOGICAL,OPTIONAL,INTENT(OUT) :: lAddToRHS
    
    !Local variables
    INTEGER :: iNode,NFaceID,iRHSRow
    REAL(8) :: rL1,rLn
    LOGICAL :: lAddToRHSLocal
    
    !Initialize
    iNode          = AppGrid%Element(iElem)%Vertex(indxVertex)
    iRHSRow        = LocateInList(iElem,AppGrid%AppNode(iNode)%ElemID_OnCCWSide)  !Locate the row number for node RHS that correspond to element
    NFaceID        = AppGrid%AppNode(iNode)%NFaceID
    rFlow          = LayerBC_GetNetBCFlowWithBCType(iNode,iBCType,AppBC%LayerBC(iLayer))
    lAddToRHSLocal = .TRUE.

    IF (rFlow .NE. 0.0) THEN
        IF (AppGrid%AppNode(iNode)%BoundaryNode) THEN
            lAddToRHSLocal = .FALSE.
            
            !If NFaceID is 2 (i.e. a boundary node that has only 1 element surrounding it, then the boundary flow to element is the full boundary flow
            IF (NFaceID .EQ. 2) THEN
                IF (PRESENT(lAddToRHS)) lAddToRHS = lAddToRHSLocal
                RETURN
            END IF
            
            !Initialize
            rL1 = AppGrid%AppNode(iNode)%IrrotationalCoeff(NFaceID)  !This is the length of the first element face that is associtaed with the boundary flow
            rLn = AppGrid%AppNode(iNode)%IrrotationalCoeff(1)        !This is the length of the last element face that is associated with the boundary flow
            
            !If indx is 1 then this is one of the boundary faces with the unknown boundary flow as inflow
            IF (iRHSRow .EQ. 1) THEN
                rFlow = rFlow * rL1 / (rL1+rLn)
            
            !If indx is NFaceID-1 then this is one of the boundary faces with the unknown boundary flow as outflow
            ELSEIF (iRHSRow .EQ. NFaceID-1) THEN
                rFlow = rFlow * rLn / (rL1+rLn)
         
            !Otherwise, this is not a boundary face and the boundary flow does not cross it
            ELSE
                rFlow = 0.0
            END IF
          
        ELSE
            rFlow = rFlow * AppGrid%AppElement(iElem)%VertexArea(indxVertex) / AppGrid%AppNode(iNode)%Area
        END IF
    ELSE    
        lAddToRHSLocal = .FALSE.
    END IF
    
    IF (PRESENT(lAddToRHS)) lAddToRHS = lAddToRHSLocal
    
  END SUBROUTINE GetBoundaryFlowAtElementNodeLayer
  
    
  ! -------------------------------------------------------------
  ! --- GET THE LIST OF NODES WITH A SPECIFIED B.C. TYPE AT A LAYER
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNodesWithBCType(AppBC,iLayer,iBCType,iNodes)
    CLASS(AppBCType),INTENT(IN)     :: AppBC
    INTEGER,INTENT(IN)              :: iLayer,iBCType
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iNodes(:)
    
    !Local variables
    INTEGER :: ErrorCode

    !Initialize
    DEALLOCATE (iNodes , STAT=ErrorCode)

    !Count nodes with the specified b.c.
    SELECT CASE (iBCType)
      !Specified flow
      CASE (SpFlowBCID)
        ALLOCATE (iNodes(AppBC%LayerBC(iLayer)%NSpecFlowBC))
        iNodes = AppBC%LayerBC(iLayer)%SpecFlowBC%iNode

      !Specified head
      CASE (SpHeadBCID)
        ALLOCATE (iNodes(AppBC%LayerBC(iLayer)%NSpecHeadBC))
        iNodes = AppBC%LayerBC(iLayer)%SpecHeadBC%iNode

      !General head
      CASE (GHBCID)
        ALLOCATE (iNodes(AppBC%LayerBC(iLayer)%NGHBC))
        iNodes = AppBC%LayerBC(iLayer)%GHBC%iNode

      !Constrained general head
      CASE (ConstrainedGHBCID)
        ALLOCATE (iNodes(AppBC%LayerBC(iLayer)%NConstrainedGHBC))
        iNodes = AppBC%LayerBC(iLayer)%ConstrainedGHBC%iNode
        
      END SELECT

  END SUBROUTINE GetNodesWithBCType
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF NODES WITH A SPECIFIED B.C. TYPE AT A LAYER
  ! -------------------------------------------------------------
  PURE FUNCTION GetNNodesWithBCType(AppBC,iLayer,iBCType) RESULT(NNodes)
    CLASS(AppBCType),INTENT(IN) :: AppBC
    INTEGER,INTENT(IN)          :: iLayer,iBCType
    INTEGER                     :: NNodes
    
    !Initialize
    NNodes = 0

    !Count nodes with the specified b.c.
    SELECT CASE (iBCType)
      !Specified flow
      CASE (SpFlowBCID)
        NNodes = AppBC%LayerBC(iLayer)%NSpecFlowBC

      !Specified head
      CASE (SpHeadBCID)
        NNodes = AppBC%LayerBC(iLayer)%NSpecHeadBC

      !General head
      CASE (GHBCID)
        NNodes = AppBC%LayerBC(iLayer)%NGHBC

      !Constrained general head
      CASE (ConstrainedGHBCID)
        NNodes = AppBC%LayerBC(iLayer)%NConstrainedGHBC
        
    END SELECT
  
  END FUNCTION GetNNodesWithBCType
  
  
  
  
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
  ! --- READ TIME SERIES BOUNDARY CONDITIONS
  ! -------------------------------------------------------------
  SUBROUTINE AppBC_ReadTSData(AppBC,TimeStep,BottomElev,Heads,iStat)
    CLASS(AppBCType)              :: AppBC
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    REAL(8),INTENT(IN)            :: BottomElev(:,:)
    REAL(8)                       :: Heads(:,:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Read time series boundary conditions data
    CALL TSBCDataFile_ReadTSData(TimeStep,AppBC%TSBCDataFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Use the newly read time series b.c. data to set the layer b.c. data
    IF (AppBC%TSBCDataFile%lUpdated)   &
        CALL LayerBC_SetTSBoundaryConditions(BottomElev,AppBC%TSBCDataFile%rValues,Heads,AppBC%LayerBC)
    
  END SUBROUTINE AppBC_ReadTSData
  
  
  
  
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
  ! --- GATEWAY METHOD FOR RESULTS PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppBC,FACTVLOU,TimeStep,lEndOfSimulation)
    CLASS(AppBCType)              :: AppBC
    REAL(8),INTENT(IN)            :: FACTVLOU
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    IF (AppBC%lBCFlowOutput_Defined)   &
        CALL BCFlowOutput_PrintResults(AppBC%LayerBC,FACTVLOU,TimeStep,lEndOfSimulation,AppBC%BCFlowOutput)
    
  END SUBROUTINE PrintResults

  
  ! -------------------------------------------------------------
  ! --- PRINT BOUNDARY NODE FLOW HYDROGRAPHS
  ! -------------------------------------------------------------
  SUBROUTINE BCFlowOutput_PrintResults(LayerBC,FACTVLOU,TimeStep,lEndOfSimulation,BCFlowOutput)
    TYPE(LayerBCType),INTENT(IN)  :: LayerBC(:)
    REAL(8),INTENT(IN)            :: FACTVLOU
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    TYPE(BCFlowOutputType)        :: BCFlowOutput
    
    !Local variables
    INTEGER :: indx,iLayer,iNode
    REAL(8) :: DummyArray(BCFlowOutput%NHyd)
    CHARACTER :: SimulationTime*16
    
    !Compile flows
    DO indx=1,BCFlowOutput%NHyd
        iLayer           = BCFlowOutput%iLayers(indx)
        iNode            = BCFlowOutput%iBCNodes(indx)
        DummyArray(indx) = LayerBC_GetNetBCFlow(iNode,LayerBC(iLayer)) * FACTVLOU
    END DO
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print out the results
    CALL BCFlowOutput%OutFile%WriteData(SimulationTime,DummyArray,FinalPrint=lEndOfSimulation)
            
  END SUBROUTINE BCFlowOutput_PrintResults
  





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
  ! --- CHECK IF ANY BOUNDARY NODE IS A FLOW NODE
  ! --- Note: It is assumed that the node is on the boundary
  ! -------------------------------------------------------------
  FUNCTION IsBoundaryFlowNode(AppBC,iNode,iLayer) RESULT(lFlowNode)
    CLASS(AppBCType),INTENT(IN) :: AppBC
    INTEGER,INTENT(IN)          :: iNode,iLayer
    LOGICAL                     :: lFlowNode
    
    !Local variables
    INTEGER,PARAMETER :: iBCTypes(3) = [SpFlowBCID,SpHeadBCID,GHBCID]
    
    !No-flow node if no b.c. are defined
    IF (.NOT. AppBC%IsDefined()) THEN
        lFlowNode = .FALSE.
        RETURN
    END IF
    
    !Otherwise see if a b.c. is defined at the node
    lFlowNode = LayerBC_IsBCNodeWithBCTypes(iNode,iBCTypes,AppBC%LayerBC(iLayer))
    
  END FUNCTION IsBoundaryFlowNode
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ANY BOUNDARY CONDITIONS (EXCEPT NO-FLOW B.C.) ARE DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsDefined(AppBC) RESULT(lDefined)
    CLASS(AppBCType),INTENT(IN) :: AppBC
    LOGICAL                     :: lDefined
    
    !Local variables
    INTEGER :: indxLayer
    
    !Initialize
    lDefined = .FALSE.
    
    !If any b.c. is defined, set the flag
    DO indxLayer=1,SIZE(AppBC%LayerBC) 
        IF (LayerBC_GetNNodesWithBC(AppBC%LayerBC(indxLayer)) .GT. 0) THEN
            lDefined = .TRUE.
            EXIT
        END IF
    END DO
    
  END FUNCTION IsDefined
  
  
  
  
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
  ! --- PREPARE BOUNDARY NODE FLOW OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrepBCFlowOutFile(IsForInquiry,cFileName,UNITVLOU,NHyd,iBCNodes,iLayers,TimeStep,OutFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,UNITVLOU
    INTEGER,INTENT(IN)            :: NHyd,iBCNodes(NHyd),iLayers(NHyd)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(GenericFileType)         :: OutFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17) :: ThisProcedure = ModName // 'PrepBCFlowOutFile'
    INTEGER                      :: indx
    CHARACTER                    :: Text*20,cFormatSpec*30,DataUnit(1)*10,DataType(1)*10,CPart(1)*32,  &
                                    FPart(1)*32,Header(3,1+NHyd)*50,HeaderFormat(3)*500,               &
                                    TitleLines(1)*500,WorkArray(3)*500
    
    !Initialize
    iStat = 0
    
    !Open file
    IF (IsForInquiry) THEN
        CALL OutFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='boundary-node flow output',iStat=iStat)
        RETURN
    ELSE
        CALL OutFile%New(FileName=cFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='boundary-node flow output',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !Make sure that DSS file is used only if it is a time tracking simulation
    IF (OutFile%iGetFileType() .EQ. DSS) THEN
        IF (.NOT. TimeStep%TrackTime) THEN
            CALL SetLastMessage('DSS files for boundary node flow hydrograph printing can only be used for time-tracking simulations.',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Data for ASCII output
    Text         = IntToText(NHyd)
    cFormatSpec  = '(A21,'//TRIM(Text)//'(2X,F12.2))'
    WorkArray(1) = ArrangeText('BOUNDARY FLOW',42)
    WorkArray(2) = ArrangeText('(UNIT=',UNITVLOU,')',42)
    WorkArray(3) = ArrangeText('[NOTE: INFLOW TO THE BASIN IS POSITIVE]',42)
    Header       = ''
    CALL PrepareTitle(TitleLines(1),WorkArray,44,42)
    WRITE (Header(1,1),'(A1,15X,A5)') '*','LAYER'
    DO indx=1,NHyd
      WRITE (Header(1,indx+1),'(I12)') iLayers(indx)
    END DO
    WRITE (Header(2,1),'(A1,16X,A4)') '*','NODE'
    DO indx=1,NHyd
      WRITE (Header(2,indx+1),'(I12)') iBCNodes(indx)
    END DO
    WRITE (Header(3,1),'(A1,8X,A4)') '*','TIME'
    HeaderFormat(1) = '(A21,'//TRIM(Text)//'(2X,A12))'
    HeaderFormat(2) = '(A21,'//TRIM(Text)//'(2X,A12))'
    HeaderFormat(3) = '(A13,'//TRIM(Text)//'(A))'

    !Data for DSS file output
    DataUnit(1) = ADJUSTL(UNITVLOU)
    DataType(1) = 'PER-AVER'
    CPart(1)    = ADJUSTL('FLOW')
    FPart(1)    = 'boundary_node_flow'

    !Prepare header lines

    !Prepare the time series output file
    CALL PrepareTSDOutputFile(OutFile                                       , &
                              NColumnsOfData                = NHyd          , &
                              NRowsOfData                   = 1             , &
                              OverwriteNColumnsOfData       = .TRUE.        , &
                              FormatSpec                    = cFormatSpec   , &
                              Title                         = TitleLines    , &
                              Header                        = Header        , &
                              HeaderFormat                  = HeaderFormat  , &
                              PrintColumnNo                 = .FALSE.       , &
                              DataUnit                      = DataUnit      , &
                              DataType                      = DataType      , &
                              CPart                         = CPart         , &
                              FPart                         = FPart         , &
                              UnitT                         = TimeStep%Unit , &
                              Layers                        = iLayers       , &
                              GWNodes                       = iBCNodes      , &
                              iStat                         = iStat         )


  END SUBROUTINE PrepBCFlowOutFile
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF B.C. RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppBC,NewUnit)
    CLASS(AppBCType)            :: AppBC
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert specified flow b.c. time unit
    IF (AppBC%TimeUnit_SpecifiedFlowBC .NE. '') THEN
        Factor                         = TimeIntervalConversion(NewUnit,AppBC%TimeUnit_SpecifiedFlowBC)
        AppBC%TimeUnit_SpecifiedFlowBC = NewUnit
        CALL LayerBC_ConvertSpecifiedFlowTimeUnit(Factor,AppBC%LayerBC)
    END IF
    
    !Convert general head b.c. time unit
    IF (AppBC%TimeUnit_GHBC .NE. '') THEN
        Factor              = TimeIntervalConversion(NewUnit,AppBC%TimeUnit_GHBC)
        AppBC%TimeUnit_GHBC = NewUnit
        CALL LayerBC_ConvertGHBCTimeUnit(Factor,AppBC%LayerBC)
    END IF
    
    !Convert constrained general head b.c. time unit
    IF (AppBC%TimeUnit_ConstrainedGHBC .NE. '') THEN
        Factor                         = TimeIntervalConversion(NewUnit,AppBC%TimeUnit_ConstrainedGHBC)
        AppBC%TimeUnit_ConstrainedGHBC = NewUnit
        CALL LayerBC_ConvertConstrainedGHBCTimeUnit(Factor,AppBC%LayerBC)
    END IF
    
    !Convert time unit of the time series specified flow input data
    
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE BOUNDARY CONDITIONS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppBC,NNodes,Heads,Matrix)
    CLASS(AppBCType)   :: AppBC
    INTEGER,INTENT(IN) :: NNodes
    REAL(8),INTENT(IN) :: Heads(:,:)
    TYPE(MatrixType)   :: Matrix
    
    !Simulate boundary conditions
    CALL LayerBC_Simulate(AppBC%LayerBC,NNodes,Heads,Matrix)
    
  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- RESET HEADS AT TIME-SERIES SPECIFIED HEAD B.C. NODES
  ! -------------------------------------------------------------
  SUBROUTINE ResetSpecifiedHeadBC(AppBC,Heads)
    CLASS(AppBCType),INTENT(IN) :: AppBC
    REAL(8)                     :: Heads(:,:)
    
    !Local variables
    INTEGER :: indxLayer,indxBC,iNode
    
    DO indxLayer=1,SIZE(AppBC%LayerBC)
        DO indxBC=1,AppBC%LayerBC(indxLayer)%NSpecHeadBC
            iNode                  = AppBC%LayerBC(indxLayer)%SpecHeadBC(indxBC)%iNode
            Heads(iNode,indxLayer) = AppBC%LayerBC(indxLayer)%SpecHeadBC(indxBC)%rHead            
        END DO
    END DO
    
  END SUBROUTINE ResetSpecifiedHeadBC


END MODULE