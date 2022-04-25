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
MODULE VerticalFlow
  USE IOInterface              , ONLY: GenericFileType        
  USE MessageLogger            , ONLY: LogMessage           , &
                                       MessageArray         , &
                                       f_iWarn                
  USe GeneralUtilities         , ONLY: FEXP                 , &
                                       IntToText            , &
                                       ArrangeText          , &
                                       PrepareTitle
  USE TimeSeriesUtilities      , ONLY: TimeStepType
  USE Package_Misc             , ONLY: PrepareTSDOutputFile , &
                                       f_rSmoothStepP       , &
                                       f_rSmoothMaxP        
  USE Package_Discretization   , ONLY: AppGridType          , &
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
  PUBLIC :: VerticalFlowOutputType                             , &
            VerticalFlowOutput_New                             , &
            VerticalFlowOutput_Kill                            , &
            VerticalFlowOutput_PrintResults                    , &
  
            !Entities related to vertical flow computation
            VerticalFlow_ComputeAtNodesLayer                   , &
            VerticalFlow_ComputeDerivativesAtNodesLayer        , &
            VerticalFlow_ComputeElementsUpwardDownward_AtLayer 
  
  
  ! -------------------------------------------------------------
  ! --- VERTICAL FLOW OUTPUT DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericFileType) :: VerticalFlowOutputType
      PRIVATE
      LOGICAL :: lOutput_Defined = .FALSE.
  END TYPE VerticalFlowOutputType
  

  ! -------------------------------------------------------------
  ! --- MISC ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'VerticalFlow::'
  
  
  
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
  ! --- PREPARE LAYER VERTICAL FLOW OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE VerticalFlowOutput_New(IsForInquiry,TimeStep,NLayers,NRegions,UNITVLOU,cOutFileName,VertFlowOutput,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NLayers,NRegions
    CHARACTER(LEN=*),INTENT(IN)   :: UNITVLOU,cOutFileName
    TYPE(VerticalFlowOutputType)  :: VertFlowOutput
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'VerticalFlowOutput_New'
    CHARACTER                    :: Text*20,FormatSpec*500,DataUnit(1)*10,DataType(1)*10,         &
                                    CPart(1)*32,FPart(1)*32,Header(3,1+(NLayers-1)*NRegions)*50,  &
                                    HeaderFormat(3)*500,DummyCharArray((NLayers-1)*NRegions)*32,  &
                                    WorkArray(3)*3000,TitleLines(1)*3000
    INTEGER                      :: indx,indxRegion,indxLayer,NColumnsOfData,NRowsOfData,I,J
    LOGICAL                      :: OverwriteNColumnsOfData,PrintColumnNo
    
    !Initilaize
    iStat = 0
    
    !Return if no file name is specified
    IF (cOutFileName .EQ. '') RETURN
    
    !If only 1 aquifer layer is modeled inform user, and return
    IF (NLayers .EQ. 1) THEN
        MessageArray(1) = 'Only one aquifer layer is modeled!'
        MessageArray(2) = 'Generation of vertical flow output file is supressed.'
        CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    !Open file
    IF (IsForInquiry) THEN
        CALL VertFlowOutput%New(cOutFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='vertical flow output',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
        VertFlowOutput%lOutput_Defined = .TRUE.
        RETURN
    ELSE
        CALL VertFlowOutput%New(cOutFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='vertical flow output',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Prepare output file
    Text                    = IntToText((NLayers-1)*NRegions)
    NColumnsOfData          = (NLayers-1)*NRegions
    NRowsOfData             = 1
    OverwriteNColumnsOfData = .FALSE.
    PrintColumnNo           = .FALSE.
    Header                  = ''
    FormatSpec              = '(A21,'//TRIM(Text)//'(2X,F12.2))'
    DataUnit(1)             = ADJUSTL(UNITVLOU)
    DataType(1)             = 'PER-AVER'
    CPart(1)                = ADJUSTL('FLOW')
    FPart(1)                = 'vertical_flow'
    indx                    = 1
    DO indxRegion=1,NRegions
      DO indxLayer=1,NLayers-1
        DummyCharArray(indx)='L'//TRIM(IntToText(indxLayer))//'-L'//TRIM(IntToText(indxLayer+1))
        indx = indx + 1
      END DO
    END DO

    !Prepare header lines
    WorkArray(1) = ArrangeText('VERTICAL FLOW',37)
    WorkArray(2) = ArrangeText('(UNIT=',UNITVLOU,')',37)
    WorkArray(3) = ArrangeText('[POSITIVE IN UPWARD DIRECTION]',37)
    CALL PrepareTitle(TitleLines(1),WorkArray(1:3),39,42)
    WRITE (Header(1,1),'(A1,14X,A6)') '*','REGION'
    DO indx=1,NRegions
      WRITE (Header(1,indx+1),'(I12)') indx
    END DO
    WRITE (Header(2,1),'(A1,15X,A5)') '*','LAYER'
    DO indx=1,NColumnsOfData
      WRITE (Header(2,indx+1),'(A12)') TRIM(DummyCharArray(indx))
    END DO
    WRITE (Header(3,1),'(A1,8X,A4)') '*','TIME'
    HeaderFormat(1) = '(A21,'//TRIM(IntToText(NRegions))//'(2X,A12'
    IF (Nlayers .EQ. 2) THEN
        HeaderFormat(1) = TRIM(HeaderFormat(1)) // ')'
    ELSE
        HeaderFormat(1) = TRIM(HeaderFormat(1)) // ',' // TRIM(IntToText((NLayers-2)*14))//'X)'
    END IF
    IF (NColumnsOfData .EQ. NRegions) THEN
        HeaderFormat(1) = TRIM(HeaderFormat(1)) // ')'
    ELSE
        HeaderFormat(1) = TRIM(HeaderFormat(1)) // ',' // TRIM(IntToText(NColumnsOfData-NRegions))//'(A))'
    END IF
    HeaderFormat(2) = '(A21,'//TRIM(Text)//'(2X,A12))'
    HeaderFormat(3) = '(A13,'//TRIM(Text)//'(A))'

    !Prepare the time series output file
    CALL PrepareTSDOutputFile(VertFlowOutput%GenericFileType                  , &
                              NColumnsOfData                                  , &
                              NRowsOfData                                     , &
                              OverwriteNColumnsOfData                         , &
                              FormatSpec                                      , &
                              TitleLines                                      , &
                              Header                                          , &
                              HeaderFormat                                    , &
                              PrintColumnNo                                   , &
                              DataUnit                                        , &
                              DataType                                        , &
                              CPart                                           , &
                              FPart                                           , &
                              TimeStep%Unit                                   , &
                              Subregions=[((I,J=1,NLayers-1),I=1,NRegions)]   , &
                              MiscArray=DummyCharArray                        , &
                              iStat=iStat                                     )
    IF (iStat .EQ. -1) RETURN
    
    !Set the flag
    VertFlowOutput%lOutput_Defined = .TRUE.
   
  END SUBROUTINE VerticalFlowOutput_New
  
  

  
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
  ! --- KILL VERTICAL FLOW OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE VerticalFlowOutput_Kill(VertFlowOutput)
    TYPE(VerticalFlowOutputType) :: VertFlowOutPut
    
    CALL VertFlowOutput%Kill()
    VertFlowOutput%lOutput_Defined = .FALSE.
    
  END SUBROUTINE VerticalFlowOutput_Kill
  
  

  
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
  ! --- PRINT VERTICAL FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE VerticalFlowOutput_PrintResults(AppGrid,Stratigraphy,Head,LeakageV,FactorVLOut,TimeStep,lEndOfSimulation,VertFlowOutput)
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: Head(:,:),LeakageV(:,:)
    REAL(8),INTENT(IN)                :: FactorVLOut
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    LOGICAL,INTENT(IN)                :: lEndOfSimulation
    TYPE(VerticalFlowOutputType)      :: VertFlowOutput
    
    !Local variables
    INTEGER           :: indxLayer,NNodes,NLayers,NRegions
    REAL(8)           :: rVertFlow((Stratigraphy%NLayers-1)*AppGrid%NSubregions), &
                         rVertFlowNode(AppGrid%NNodes)
    CHARACTER(LEN=21) :: SimulationTime
    
    !Return if the vertical flow output is not defined
    IF (.NOT. VertFlowOutput%lOutput_Defined) RETURN
    
    !Initialize
    NNodes   = AppGrid%NNodes
    NLayers  = Stratigraphy%NLayers
    NRegions = AppGrid%NSubregions
       
    !Compute vertical flows
    DO indxLayer=1,NLayers-1
        CALL VerticalFlow_ComputeAtNodesLayer(indxLayer,NNodes,Stratigraphy,Head,LeakageV,rVertFlowNode)
        rVertFlow(indxLayer::NLayers-1) = AppGrid%AccumNodeValuesToSubregions(rVertFlowNode) * FactorVLOut
    END DO
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime=ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print out the results
    CALL VertFlowOutput%WriteData(SimulationTime,rVertFlow,FinalPrint=lEndOfSimulation)
    
  END SUBROUTINE VerticalFlowOutput_PrintResults

  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. ENTITIES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- COMPUTE VERTICAL FLOWS AT NODES OF A LAYER BETWEEN THAT LAYER AND LAYER BELOW
  ! -------------------------------------------------------------
  PURE SUBROUTINE VerticalFlow_ComputeAtNodesLayer(iLayer,NNodes,Stratigraphy,Head,LeakageV,rVertFlow)
    INTEGER,INTENT(IN)                :: iLayer,NNodes
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: Head(:,:),LeakageV(:,:)
    REAL(8),INTENT(OUT)               :: rVertFlow(NNodes)
    
    !Local variables
    INTEGER :: indxNode,iActiveLayerBelow(NNodes),iLayerBelow
    REAL(8) :: rBottomElev,rDisconnectElev
    
    !If only one layer simulated or the layer is the last (deepest) layer, vertical flow is zero
    IF (Stratigraphy%NLayers .EQ. 1  .OR.  iLayer .EQ. Stratigraphy%NLayers) THEN
        rVertFlow = 0.0
        RETURN
    END IF
    
    !Get active layer below at each node
    iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(iLayer)
    
    !Compute vertical flow betweeen layer and active layer below
    DO indxNode=1,NNodes
        !If node is inactive, vertical flow is zero
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            rVertFlow(indxNode) = 0.0
            CYCLE
        END IF
        
        !Active layer below; cycle if no active layer below
        iLayerBelow = iActiveLayerBelow(indxNode)
        IF (iLayerBelow .LE. 0) THEN
            rVertFlow(indxNode) = 0.0
            CYCLE
        END IF
        
        !Otherwise, compute the vertical flow
        rBottomElev         = Stratigraphy%BottomElev(indxNode,iLayer)
        rDisconnectElev     = Stratigraphy%TopElev(indxNode,iLayerBelow)
        rVertFlow(indxNode) = ComputeVerticalFlow(Head(indxNode,iLayer),Head(indxNode,iLayerBelow),LeakageV(indxNode,iLayerBelow),rBottomElev,rDisconnectElev)

    END DO
    
  END SUBROUTINE VerticalFlow_ComputeAtNodesLayer
  
    
  ! -------------------------------------------------------------
  ! --- COMPUTE DERIVATIVES OF VERTICAL FLOWS AT NODES OF A LAYER BETWEEN THAT LAYER AND LAYER BELOW
  ! -------------------------------------------------------------
  PURE SUBROUTINE VerticalFlow_ComputeDerivativesAtNodesLayer(iLayer,NNodes,Stratigraphy,rHead,rLeakageV,rVertFlow,rdVertFlow_dH,rdVertFlow_dHb)
    INTEGER,INTENT(IN)                :: iLayer,NNodes
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: rHead(NNodes,Stratigraphy%NLayers),rLeakageV(NNodes,Stratigraphy%NLayers),rVertFlow(NNodes)
    REAL(8),INTENT(OUT)               :: rdVertFlow_dH(NNodes),rdVertFlow_dHb(NNodes)
    
    !Local variables
    INTEGER           :: indxNode,iActiveLayerBelow(NNodes),iLayerBelow
    REAL(8)           :: rBottomElev,rDisconnectElev,rThisHead,rHeadBelow,rHeadDiff,rHeadDiffBelow,rThisLeakage,rFactor,rdFactor,  &
                         rExp,rExpPlusOne
    REAL(8),PARAMETER :: rHugeNumber = 1d100
    
    !If only one layer simulated or the layer is the last (deepest) layer, vertical flow derivatives are zero
    IF (Stratigraphy%NLayers .EQ. 1  .OR.  iLayer .EQ. Stratigraphy%NLayers) THEN
        rdVertFlow_dH  = 0.0
        rdVertFlow_dHb = 0.0
        RETURN
    END IF
    
    !Get active layer below at each node
    iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(iLayer)
    
    !Compute derivatives of vertical flow betweeen layer and active layer below
    DO indxNode=1,NNodes
        !If node is inactive, vertical flow derivatives are zero
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            rdVertFlow_dH(indxNode)  = 0.0
            rdVertFlow_dHb(indxNode) = 0.0
            CYCLE
        END IF
        
        !Active layer below; cycle if no active layer below
        iLayerBelow = iActiveLayerBelow(indxNode)
        IF (iLayerBelow .LE. 0) THEN
            rdVertFlow_dH(indxNode)  = 0.0
            rdVertFlow_dHb(indxNode) = 0.0
            CYCLE
        END IF
        
        !Otherwise, compute the derivatives of vertical flow
        rBottomElev     = Stratigraphy%BottomElev(indxNode,iLayer)
        rDisconnectElev = Stratigraphy%TopElev(indxNode,iLayerBelow)
        rThisHead       = rHead(indxNode,iLayer)
        rHeadBelow      = rHead(indxNode,iLayerBelow)
        rHeadDiff       = rThisHead - rBottomElev
        rHeadDiffBelow  = rHeadBelow - rDisconnectElev
        rThisLeakage    = rLeakageV(indxNode,iLayerBelow)
        IF (rVertFlow(indxNode) .LT. 0.0) THEN
            rExp = FEXP(-f_rSmoothStepP * rHeadDiff)
            IF (rExp .LT. rHugeNumber) THEN
                rExpPlusOne              = 1d0 + rExp
                rFactor                  = 1d0 / rExpPlusOne
                rdFactor                 = f_rSmoothStepP * rExp /(rExpPlusOne*rExpPlusOne)
                rdVertFlow_dH(indxNode)  = rdFactor * rThisLeakage * (MAX(rHeadBelow,rDisconnectElev) - MAX(rThisHead,rBottomElev))  &
                                         - rFactor * rThisLeakage * 0.5d0 * (1d0 + rHeadDiff/SQRT(rHeadDiff*rHeadDiff + f_rSmoothMaxP))
                rdVertFlow_dHb(indxNode) = rFactor * rThisLeakage * 0.5d0 *(1d0 + rHeadDiffBelow / SQRT(rHeadDiffBelow*rHeadDiffBelow + f_rSmoothMaxP))
            END IF
        ELSE
            rdVertFlow_dH(indxNode)  = -rThisLeakage * 0.5d0 * (1d0 + rHeadDiff/SQRT(rHeadDiff*rHeadDiff + f_rSmoothMaxP))
            rdVertFlow_dHb(indxNode) = rThisLeakage * 0.5d0 *(1d0 + rHeadDiffBelow / SQRT(rHeadDiffBelow*rHeadDiffBelow + f_rSmoothMaxP))
        END IF
    END DO
    
  END SUBROUTINE VerticalFlow_ComputeDerivativesAtNodesLayer
  
    
  ! -------------------------------------------------------------
  ! --- COMPUTE UPWARD AND DOWNWARD VERTICAL FLOWS AT ELEMENTS OF A LAYER BETWEEN THAT LAYER AND LAYER BELOW
  ! -------------------------------------------------------------
  PURE SUBROUTINE VerticalFlow_ComputeElementsUpwardDownward_AtLayer(iLayer,AppGrid,Stratigraphy,Head,LeakageV,rVertFlow_Downward,rVertFlow_Upward)
    INTEGER,INTENT(IN)                :: iLayer
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: Head(AppGrid%NNodes,Stratigraphy%NLayers),LeakageV(AppGrid%NNodes,Stratigraphy%NLayers)
    REAL(8),INTENT(OUT)               :: rVertFlow_Upward(AppGrid%NElements),rVertFlow_Downward(AppGrid%NElements)
    
    !Local variables
    INTEGER :: indxNode,iActiveLayerBelow(AppGrid%NNodes),iLayerBelow
    REAL(8) :: rBottomElev,rVertFlow,rVertFlow_Upward_Node(AppGrid%NNodes),rVertFlow_Downward_Node(AppGrid%NNodes), &
               rDisconnectElev
    
    !If only one layer simulated or the layer is the last (deepest) layer, vertical flows are zero
    IF (Stratigraphy%NLayers .EQ. 1  .OR.  iLayer .EQ. Stratigraphy%NLayers) THEN
        rVertFlow_Downward = 0.0
        rVertFlow_Upward   = 0.0
        RETURN
    END IF
    
    !Initialize the flows
    rVertFlow_Downward = 0.0
    rVertFlow_Upward   = 0.0
    
    !Get active layer below at each node
    iActiveLayerBelow = Stratigraphy%GetAllActiveLayerBelow(iLayer)
    
    !Compute vertical flow betweeen layer and active layer below
    DO indxNode=1,AppGrid%NNodes
        !If node is inactive, vertical flow is zero
        IF (.NOT. Stratigraphy%ActiveNode(indxNode,iLayer)) THEN
            rVertFlow_Upward_Node(indxNode)   = 0.0
            rVertFlow_Downward_Node(indxNode) = 0.0
            CYCLE
        END IF
        
        !Active layer below; cycle if no active layer below
        iLayerBelow = iActiveLayerBelow(indxNode)
        IF (iLayerBelow .LE. 0) THEN
            rVertFlow_Upward_Node(indxNode)   = 0.0
            rVertFlow_Downward_Node(indxNode) = 0.0
            CYCLE
        END IF
        
        !Otherwise, compute the vertical flow at node
        rBottomElev     = Stratigraphy%BottomElev(indxNode,iLayer)
        rDisconnectElev = Stratigraphy%TopElev(indxNode,iLayerBelow)
        rVertFlow       = ComputeVerticalFlow(Head(indxNode,iLayer),Head(indxNode,iLayerBelow),LeakageV(indxNode,iLayerBelow),rBottomElev,rDisconnectElev)

        !Seperate downward and upward flows 
        IF (rVertFlow .GT. 0.0) THEN
            rVertFlow_Upward_Node(indxNode) = rVertFlow
        ELSE
            rVertFlow_Downward_Node(indxNode) = -rVertFlow
        END IF
    END DO
    
    !Distribute nodal values to elements
    CALL AppGrid%NodeData_To_ElemData(rVertFlow_Upward_Node,rVertFlow_Upward)
    CALL AppGrid%NodeData_To_ElemData(rVertFlow_Downward_Node,rVertFlow_Downward)
    
  END SUBROUTINE VerticalFlow_ComputeElementsUpwardDownward_AtLayer
  
    
  ! -------------------------------------------------------------
  ! --- COMPUTE VERTICAL FLOW
  ! -------------------------------------------------------------
  PURE FUNCTION ComputeVerticalFlow(rHead,rHeadBelow,rLeakageV,rBottomElev,rDisconnectElev) RESULT(rVertFlow)
    REAL(8),INTENT(IN) :: rHead,rHeadBelow,rLeakageV,rBottomElev,rDisconnectElev
    REAL(8)            :: rVertFlow
    
    !Local variables
    REAL(8) :: rFactor
  
    rVertFlow = rLeakageV * (MAX(rHeadBelow,rDisconnectElev)- MAX(rHead,rBottomElev))
    
    !Correct vertical flow if the flow is out of the node so that head doesn't fall below the layer bottom
    IF (rVertFlow .LT. 0.0) THEN
        rFactor   = 1d0 / (1d0 + FEXP(f_rSmoothStepP * (rBottomElev-rHead)))
        rVertFlow = rFactor * rVertFlow
    END IF

  END FUNCTION ComputeVerticalFlow
  
END MODULE