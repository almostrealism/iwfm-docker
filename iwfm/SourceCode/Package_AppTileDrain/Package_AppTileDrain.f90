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
MODULE Package_AppTileDrain
  USE Class_Version           , ONLY: ReadVersion
  USE MessageLogger           , ONLY: SetLastMessage            , &
                                      EchoProgress              , &
                                      MessageArray              , &
                                      iFatal                    
  USE GeneralUtilities                                          
  USE TimeSeriesUtilities                                       
  USE IOInterface                                               
  USE Package_Discretization                                    
  USE Package_Misc            , ONLY: FlowDest_Outside          , &
                                      FlowDest_StrmNode         , &
                                      iGWComp                   , &
                                      iLocationType_TileDrain
  USE Class_BaseTileDrain     , ONLY: BaseTileDrainType
  USE TileDrainHydrograph     , ONLY: TileDrainHydrographType
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
  PUBLIC :: AppTileDrainType                 , &
            iTileDrain                       , &
            iSubIrig
  

  ! -------------------------------------------------------------
  ! --- FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iTileDrain = 1, &
                       iSubIrig   = 2


  ! -------------------------------------------------------------
  ! --- TILE DRAIN DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseTileDrainType) :: TileDrainType
    INTEGER :: iDestType = 0
    INTEGER :: iDest     = 0
  END TYPE TileDrainType
  

  ! -------------------------------------------------------------
  ! --- APPLICATION TILE DRAIN/SUB-IRRIGATION DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppTileDrainType
    PRIVATE
    INTEGER                             :: NDrain              = 0
    INTEGER                             :: NSubIrig            = 0
    CHARACTER(LEN=6)                    :: TimeUnitConductance = ''
    TYPE(TileDrainType),ALLOCATABLE     :: TileDrains(:)
    TYPE(BaseTileDrainType),ALLOCATABLE :: SubIrigs(:)
    TYPE(TileDrainHydrographType)       :: TileDrainHyd
  CONTAINS
    PROCEDURE,PASS :: New                
    PROCEDURE,PASS :: Kill 
    PROCEDURE,PASS :: GetNDataList_AtLocationType
    PROCEDURE,PASS :: GetDataList_AtLocationType
    PROCEDURE,PASS :: GetModelData_AtLocation
    PROCEDURE,PASS :: GetNDrain          
    PROCEDURE,PASS :: GetNSubIrig 
    PROCEDURE,PASS :: GetNHydrographs
    PROCEDURE,PASS :: GetHydrographNames
    PROCEDURE,PASS :: GetFlows           
    PROCEDURE,PASS :: GetFlowsToStreams  
    PROCEDURE,PASS :: GetGWNodesLayers    
    PROCEDURE,PASS :: GetSubregionalFlows
    PROCEDURE,PASS :: ConvertTimeUnit  
    PROCEDURE,PASS :: ReadRestartData
    PROCEDURE,PASS :: PrintResults
    PROCEDURE,PASS :: PrintRestartData
    PROCEDURE,PASS :: Simulate
    PROCEDURE,PASS :: TransferOutputToHDF
  END TYPE AppTileDrainType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: nData_AtTileDrain                        = 1                          , &
                                 iTileDrainHyd                            = 1                   
  CHARACTER(LEN=21),PARAMETER :: cDataList_AtTileDrain(nData_AtTileDrain) = ['Tile drain hydrograph']
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_AppTileDrain::' 


  
  
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
  ! --- NEW AppTileDrain DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(AppTileDrain,IsForInquiry,cFileName,cWorkingDirectory,NStrmNode,TimeStep,AppGrid,Stratigraphy,iStat) 
    CLASS(AppTileDrainType),INTENT(OUT) :: AppTileDrain
    LOGICAL,INTENT(IN)                  :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)                  :: NStrmNode
    TYPE(TimeStepType),INTENT(IN)       :: TimeStep
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)   :: Stratigraphy
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    TYPE(GenericFileType)    :: TDFile
    INTEGER                  :: NNodes,NDrain,NSubIrig
    REAL(8)                  :: Factor
    CHARACTER                :: TimeUnitConductance_TD*6,TimeUnitConductance_SI*6
    CHARACTER(:),ALLOCATABLE :: cVersion
    
    !Initailize
    iStat = 0

    !If no FileName, return
    IF (cFileName .EQ. '') RETURN
    
    !Initialize
    NNodes = AppGrid%NNodes

    !Open file
    CALL TDFile%New(cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Tile drain/subsurface irrigation data',iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read first line that stores version number
    CALL ReadVersion(TDFile,'TILE DRAINS',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN

    !Read tile drain data
    CALL TileDrain_New(TDFile,AppGrid,Stratigraphy,NNodes,NStrmNode,TimeUnitConductance_TD,AppTileDrain%TileDrains,iStat)
    IF (iStat .EQ. -1) RETURN
    NDrain              = SIZE(AppTileDrain%TileDrains)
    AppTileDrain%NDrain = NDrain
    
    !Read subsurface irrigation data
    CALL SubIrig_New(TDFile,AppGrid,Stratigraphy,NNodes,TimeUnitConductance_SI,AppTileDrain%SubIrigs,iStat) 
    IF (iStat .EQ. -1) RETURN
    NSubIrig              = SIZE(AppTileDrain%SubIrigs)
    AppTileDrain%NSubIrig = NSubIrig
    
    !Return if no data is specified
    IF (NDrain.EQ.0  .AND. NSubIrig.EQ. 0) RETURN 
    
    !Define a common time unit of conductance
    IF (NDrain .GT. 0) THEN
      AppTileDrain%TimeUnitConductance = TimeUnitConductance_TD
      IF (NSubIrig .GT. 0) THEN
        Factor                             = TimeIntervalConversion(TimeUnitConductance_TD,TimeUnitConductance_SI)
        AppTileDrain%SubIrigs%rConductance = AppTileDrain%SubIrigs%rConductance * Factor
      END IF
    ELSEIF (NSubIrig .GT. 0) THEN
      AppTileDrain%TimeUnitConductance = TimeUnitConductance_SI
      IF (NDrain .GT. 0) THEN
        Factor                               = TimeIntervalConversion(TimeUnitConductance_SI,TimeUnitConductance_TD)
        AppTileDrain%TileDrains%rConductance = AppTileDrain%TileDrains%rConductance * Factor
      END IF
    END IF
       
    !Hydrograph print control data
    CALL AppTileDrain%TileDrainHyd%New(IsForInquiry,cWorkingDirectory,AppTileDrain%SubIrigs%iGWNode,AppTileDrain%TileDrains%iGWNode,TimeStep,TDFile,iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Close file
    CALL TDFile%Kill()
    
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- NEW TileDrain DATA
  ! -------------------------------------------------------------
  SUBROUTINE TileDrain_New(InFile,AppGrid,Stratigraphy,NNodes,NStrmNode,TimeUnitConductance,TileDrains,iStat)
    TYPE(GenericFileType)                       :: InFile
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)           :: Stratigraphy
    INTEGER,INTENT(IN)                          :: NNodes,NStrmNode
    CHARACTER(LEN=6),INTENT(OUT)                :: TimeUnitConductance
    TYPE(TileDrainType),ALLOCATABLE,INTENT(OUT) :: TileDrains(:)
    INTEGER,INTENT(OUT)                         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'TileDrain_New'
    INTEGER                      :: NDrain,indx,iGWNode
    REAL(8)                      :: FactH,FactCDC,DummyArray(6)
    CHARACTER                    :: ALine*1000
    
    !Read data
    CALL InFile%ReadData(NDrain,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactH,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactCDC,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    TimeUnitConductance = UpperCase(ADJUSTL(StripTextUntilCharacter(ALine,'/')))
   
    !Return if there are no tile drains
    IF (NDrain .EQ. 0) RETURN
    
    !Allocate
    ALLOCATE (TileDrains(NDrain))
    
    !Read data
    DO indx=1,NDrain
      CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Make sure data is entered sequentially
      IF (INT(DummyArray(1)) .NE. indx) THEN
          MessageArray(1) = 'Tile drain data must entered sequentially!'
          MessageArray(2) = 'Tile drain ID expected:'//TRIM(IntToText(indx))
          MessageArray(3) = 'Tile drain ID entered :'//TRIM(IntToText(INT(DummyArray(1))))
          CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Assign data to persistent arrays
      iGWNode                       = INT(DummyArray(2))
      TileDrains(indx)%iGWNode      = iGWNode
      TileDrains(indx)%rElevation   = DummyArray(3) * FactH
      TileDrains(indx)%rConductance = DummyArray(4) * FactCDC
      TileDrains(indx)%iGWNodeLayer = Stratigraphy%GetLayerNumberForElevation(TileDrains(indx)%rElevation , &
                                                                              AppGrid%Node(iGWNode)%X     , &
                                                                              AppGrid%Node(iGWNode)%Y     , &
                                                                              AppGrid                     )
      TileDrains(indx)%iDestType    = INT(DummyArray(5))
      TileDrains(indx)%iDest        = INT(DummyArray(6))
      
      !Make sure gw node number is valid
      IF (TileDrains(indx)%iGWNode.LT.1  .OR. TileDrains(indx)%iGWNode.GT.NNodes) THEN
          CALL SetLastMessage('Groundwater node number for tile drain '//TRIM(IntToText(indx))//' is not modeled!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

     !Make sure that destination type is exceptible
      IF (TileDrains(indx)%iDestType.NE.FlowDest_Outside .AND. TileDrains(indx)%iDestType.NE.FlowDest_StrmNode) THEN
          CALL SetLastMessage('Flow destination type for tile drain '//TRIM(IntToText(indx))//' is not recognized!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Make sure strm node number is valid
      IF (TileDrains(indx)%iDestType .EQ. FlowDest_StrmNode) THEN
          IF (TileDrains(indx)%iDest.LT.1  .OR. TileDrains(indx)%iDest.GT.NStrmNode) THEN
              CALL SetLastMessage('Stream node number for tile drain '//TRIM(IntToText(indx))//' is not modeled!',iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
      END IF
      
    END DO
    
  END SUBROUTINE TileDrain_New


  ! -------------------------------------------------------------
  ! --- NEW SubIrig DATA
  ! -------------------------------------------------------------
  SUBROUTINE SubIrig_New(InFile,AppGrid,Stratigraphy,NNodes,TimeUnitConductance,SubIrigs,iStat)
    TYPE(GenericFileType)                           :: InFile
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)               :: Stratigraphy
    INTEGER,INTENT(IN)                              :: NNodes
    CHARACTER(LEN=6),INTENT(OUT)                    :: TimeUnitConductance
    TYPE(BaseTileDrainType),ALLOCATABLE,INTENT(OUT) :: SubIrigs(:)
    INTEGER,INTENT(OUT)                             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+11) :: ThisProcedure = ModName // 'SubIrig_New'
    INTEGER                      :: NDrain,indx,iGWNode
    REAL(8)                      :: FactH,FactCDC,DummyArray(4)
    CHARACTER                    :: ALine*1000
    
    !Read data
    CALL InFile%ReadData(NDrain,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactH,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactCDC,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    TimeUnitConductance = UpperCase(ADJUSTL(StripTextUntilCharacter(ALine,'/')))
   
    !Return if there are no sub-irigs
    IF (NDrain .EQ. 0) RETURN
    
    !Allocate
    ALLOCATE (SubIrigs(NDrain))
    
    !Read data
    DO indx=1,NDrain
      CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Make sure data is entered sequentially
      IF (INT(DummyArray(1)) .NE. indx) THEN
          MessageArray(1) = 'Subsurface irrigation data must entered sequentially!'
          MessageArray(2) = 'Subsurface irrigation ID expected:'//TRIM(IntToText(indx))
          MessageArray(3) = 'Subsurface irrigation ID entered :'//TRIM(IntToText(INT(DummyArray(1))))
          CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Assign data to persistent arrays
      iGWNode                     = INT(DummyArray(2))
      SubIrigs(indx)%iGWNode      = iGWNode
      SubIrigs(indx)%rElevation   = DummyArray(3) * FactH
      SubIrigs(indx)%rConductance = DummyArray(4) * FactCDC
      SubIrigs(indx)%iGWNodeLayer = Stratigraphy%GetLayerNumberForElevation(SubIrigs(indx)%rElevation   , &
                                                                            AppGrid%Node(iGWNode)%X     , &
                                                                            AppGrid%Node(iGWNode)%Y     , &
                                                                            AppGrid                     )
      
      !Make sure gw node number is valid
      IF (SubIrigs(indx)%iGWNode.LT.1  .OR. SubIrigs(indx)%iGWNode.GT.NNodes) THEN
          CALL SetLastMessage('Groundwater node number for subsurface irrigation '//TRIM(IntToText(indx))//' is not modeled!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
    END DO
    
  END SUBROUTINE SubIrig_New
  
  
  
  
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
  ! --- KILL TILE DRAIN DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(AppTileDrain)
    CLASS(AppTileDrainType) :: AppTileDrain
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (AppTileDrain%TileDrains , AppTileDrain%SubIrigs , STAT=ErrorCode)
    CALL AppTileDrain%TileDrainHyd%Kill()
    
    AppTileDrain%NDrain              = 0
    AppTileDrain%NSubIrig            = 0
    AppTileDrain%TimeUnitConductance = ''
    
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
  ! --- GET THE NUMBER OF DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  FUNCTION GetNDataList_AtLocationType(AppTileDrain) RESULT(NData) 
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER                            :: NData
    
    IF (AppTileDrain%TileDrainHyd%IsOutFileDefined()) THEN
        NData = nData_AtTileDrain
    ELSE
        NData = 0
    END IF
    
  END FUNCTION GetNDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF DATA TYPES FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE GetDataList_AtLocationType(AppTileDrain,cDataList,cFileList,lBudgetType) 
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    CHARACTER(LEN=*),ALLOCATABLE       :: cDataList(:),cFileList(:)
    LOGICAL,ALLOCATABLE                :: lBudgetType(:)
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cFileName
    
    IF (AppTileDrain%TileDrainHyd%IsOutFileDefined()) THEN
        ALLOCATE (cDataList(nData_AtTileDrain) , cFileList(nData_AtTileDrain) , lBudgetType(nData_AtTileDrain))
        cDataList   = cDataList_AtTileDrain(iTileDrainHyd)
        lBudgetType = .FALSE.
        CALL AppTileDrain%TileDrainHyd%GetOutFileName(cFileName)
        cFileList = ''
        cFileList = TRIM(StripTextUntilCharacter(ADJUSTL(cFileName),'.',Back=.TRUE.)) // '.hdf'  !Before this method, all hydrographs must have copied into an HDF file
    END IF
    
  END SUBROUTINE GetDataList_AtLocationType
  
  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN HYDROGRAPH FOR POST-PROCESSING AT A TILE DRAIN ID FROM FULL MODEL
  ! -------------------------------------------------------------
  SUBROUTINE GetModelData_AtLocation(AppTileDrain,iTDHydID,cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_VL,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(AppTileDrainType)     :: AppTileDrain
    INTEGER,INTENT(IN)          :: iTDHydID
    CHARACTER(LEN=*),INTENT(IN) :: cDataType,cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)          :: rFact_VL
    INTEGER,INTENT(OUT)         :: nActualOutput                           !This is the actual number of elements of rOutputValues and rOutputDates arrays that are populated (can be less than or equal to the size of these arrays)
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Initialize
    iStat = 0
    
    !If data type is not recognized return
    IF (TRIM(cDataType) .NE. TRIM(cDataList_AtTileDrain(iTileDrainHyd))) RETURN

    CALL AppTileDrain%TileDrainHyd%ReadTDHyd_AtHydrographLocation(iTDHydID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_VL,nActualOutput,rOutputDates,rOutputValues,iStat)

  END SUBROUTINE GetModelData_AtLocation
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOWS TO STREAMS
  ! -------------------------------------------------------------
  SUBROUTINE GetFlowsToStreams(AppTileDrain,QDrain)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    REAL(8),INTENT(OUT)                :: QDrain(:)
    
    !Local variables
    INTEGER :: indx,iDest
    
    !Initilaize
    QDrain =0.0
    
    !Compile flows to streams
    DO indx=1,AppTileDrain%NDrain
      IF (AppTileDrain%TileDrains(indx)%iDestType .NE. FlowDest_StrmNode) CYCLE
      iDest = AppTileDrain%TileDrains(indx)%iDest
      QDrain(iDest) = QDrain(iDest) - AppTileDrain%TileDrains(indx)%rFlow
    END DO
    
  END SUBROUTINE GetFlowsToStreams
  
  
  ! -------------------------------------------------------------
  ! --- ACCUMULATE TILE DRAINS/SUBSURFACE IRRIGATION TO SUBREGIONS
  ! -------------------------------------------------------------
  FUNCTION GetSubregionalFlows(AppTileDrain,iType,AppGrid) RESULT(Flows)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER,INTENT(IN)                 :: iType
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    REAL(8)                            :: Flows(AppGrid%NSubregions)
    
    SELECT CASE (iType)
      !Tile drains
      CASE (iTileDrain)
        IF (AppTileDrain%NDrain .EQ. 0) THEN
          Flows = 0.0
        ELSE
          Flows = AppGrid%AccumSomeNodeValuesToSubregions(AppTileDrain%TileDrains%iGWNode,AppTileDrain%TileDrains%rFlow)
        END IF
      
      !Subsurface irrigation  
      CASE (iSubIrig)
        IF (AppTileDrain%NSubirig .EQ. 0) THEN
          Flows = 0.0
        ELSE
          Flows = AppGrid%AccumSomeNodeValuesToSubregions(AppTileDrain%SubIrigs%iGWNode,AppTileDrain%SubIrigs%rFlow)
        END IF
    
    END SELECT
    
  END FUNCTION GetSubregionalFlows
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TILE DRAINS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNDrain(AppTileDrain) RESULT(NDrain)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER                            :: NDrain
    
    NDrain = AppTileDrain%NDrain
    
  END FUNCTION GetNDrain


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF HYDROGRAPHS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNHydrographs(AppTileDrain,iHydType) RESULT(NHyd)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER,INTENT(IN)                 :: iHydType   
    INTEGER                            :: NHyd
    
    NHyd = AppTileDrain%TileDrainHyd%GetNHydrographs(iHydType)
    
  END FUNCTION GetNHydrographs


  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographNames(AppTileDrain,cNamesList)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    CHARACTER(LEN=*),INTENT(OUT)       :: cNamesList(:)   !Assumes array is previously dimensioned based on the number of tile drain hydrographs
    
    CALL AppTileDrain%TileDrainHyd%GetHydrographNames(cNamesList)
    
  END SUBROUTINE GetHydrographNames


  ! -------------------------------------------------------------
  ! --- GET NUMBER OF SUB-IRIGS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNSubIrig(AppTileDrain) RESULT(NSubIrig)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER                            :: NSubIrig
    
    NSubIrig = AppTileDrain%NSubIrig
    
  END FUNCTION GetNSubIrig
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE GetFlows(AppTileDrain,iType,rFlows)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER,INTENT(IN)                 :: iType
    REAL(8),ALLOCATABLE,INTENT(OUT)    :: rFlows(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (rFlows , STAT=ErrorCode)
    
    !Get the nodes
    SELECT CASE (iType)
      CASE (iTileDrain)
        ALLOCATE (rFlows(AppTileDrain%NDrain))
        rFlows = AppTileDrain%TileDrains%rFlow
        
      CASE (iSubIrig)
        ALLOCATE (rFlows(AppTileDrain%NSubIrig))
        rFlows = AppTileDrain%SubIrigs%rFlow
        
    END SELECT
        
  END SUBROUTINE GetFlows  
  
  
  ! -------------------------------------------------------------
  ! --- GET GW NODES and CORRESPONDING LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE GetGWNodesLayers(AppTileDrain,iType,iGWNodes,iGWNodeLayers)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER,INTENT(IN)                 :: iType
    INTEGER,ALLOCATABLE,INTENT(OUT)    :: iGWNodes(:),iGWNodeLayers(:)
    
    !Local variables
    INTEGER :: ErrorCode,iDim
    
    !Initialize
    DEALLOCATE (iGWNodes , iGWNodeLayers , STAT=ErrorCode)
    
    !Get the nodes
    SELECT CASE (iType)
    CASE (iTileDrain)
        iDim = AppTileDrain%NDrain
        ALLOCATE (iGWNodes(iDim) , iGWNodeLayers(iDim))
        iGWNodes      = AppTileDrain%TileDrains%iGWNode
        iGWNodeLayers = AppTileDrain%TileDrains%iGWNodeLayer
        
    CASE (iSubIrig)
        iDim = AppTileDrain%NSubIrig
        ALLOCATE (iGWNodes(iDim) , iGWNodeLayers(iDim))
        iGWNodes      = AppTileDrain%SubIrigs%iGWNode
        iGWNodeLayers = AppTileDrain%SubIrigs%iGWNodeLayer
        
    END SELECT
        
  END SUBROUTINE GetGWNodesLayers    
  



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
  SUBROUTINE ReadRestartData(AppTileDrain,InFile,iStat)
    CLASS(AppTileDrainType) :: AppTileDrain
    TYPE(GenericFileType)   :: InFile
    INTEGER,INTENT(OUT)     :: iStat
    
    CALL InFile%ReadData(AppTileDrain%TileDrains%rFlow,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(AppTileDrain%SubIrigs%rFlow,iStat)  
    
  END SUBROUTINE ReadRestartData

  
  
  
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
  SUBROUTINE PrintRestartData(AppTileDrain,OutFile)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    TYPE(GenericFileType)              :: OutFile
    
    CALL OutFile%WriteData(AppTileDrain%TileDrains%rFlow)
    CALL OutFile%WriteData(AppTileDrain%SubIrigs%rFlow)
    
  END SUBROUTINE PrintRestartData
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT HYDROGRAPHS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(AppTileDrain,TimeStep,lEndOfSimulation)
    CLASS(AppTileDrainType)       :: AppTileDrain
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    
    !Return if no hydrograph file is defined
    IF (.NOT. AppTileDrain%TileDrainHyd%IsOutFileDefined()) RETURN
    
    !Print
    CALL AppTileDrain%TileDrainHyd%PrintResults(AppTileDrain%TileDrains%rFlow , &
                                                AppTileDrain%SubIrigs%rFlow   , &
                                                TimeStep                      , &
                                                lEndOfSimulation              )
    
  END SUBROUTINE PrintResults
  
  


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
  ! --- SIMULATE TILE DRAIN/SUBSURFACE IRRIGATION FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE Simulate(AppTileDrain,NNodes,HN,Matrix)
    CLASS(AppTileDrainType) :: AppTileDrain
    INTEGER,INTENT(IN)      :: NNodes
    REAL(8),INTENT(IN)      :: HN(:,:)
    TYPE(MatrixType)        :: Matrix

    !Local variables
    INTEGER           :: indx,iNode,iLayer,iGWNode,iNodeIDs(1)
    REAL(8)           :: rElevation,rConductance,rUpdateValues(1)
    INTEGER,PARAMETER :: iCompIDs(1) = [iGWComp]

    !Inform user
    CALL EchoProgress('Simulating tile drain/subsurface irrigation flows')

    !Tile drains
    ASSOCIATE (pTileDrains => AppTileDrain%TileDrains)
      DO indx=1,AppTileDrain%NDrain
        iNode                   = pTileDrains(indx)%iGWNode
        iLayer                  = pTileDrains(indx)%iGWNodeLayer
        iGWNode                 = (iLayer-1)*NNodes + iNode
        rConductance            = pTileDrains(indx)%rConductance
        rElevation              = pTileDrains(indx)%rElevation
        pTileDrains(indx)%rFlow = rConductance * MIN(rElevation-HN(iNode,iLayer) , 0.0)
        IF (rElevation .LT. HN(iNode,iLayer)) THEN
            !Node in consideartion
            iNodeIDs(1)      = iGWNode
            !Update RHS vector
            rUpdateValues(1) = -pTileDrains(indx)%rFlow
            CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateValues)
            !Update COEFF matrix
            rUpdateValues(1) = rConductance
            CALL Matrix%UpdateCOEFF(iGWComp,iGWNode,iCompIDs,iNodeIDs,rUpdateValues)
        END IF
      END DO
    END ASSOCIATE 

    !Subsurface irrigations 
    ASSOCIATE (pSubIrigs => AppTileDrain%SubIrigs)
      DO indx=1,AppTileDrain%NSubIrig
        iNode                 = pSubIrigs(indx)%iGWNode
        iLayer                = pSubIrigs(indx)%iGWNodeLayer
        iGWNode               = (iLayer-1)*NNodes + iNode
        rConductance          = pSubIrigs(indx)%rConductance
        rElevation            = pSubIrigs(indx)%rElevation
        pSubIrigs(indx)%rFlow = rConductance * MAX(rElevation-HN(iNode,iLayer) , 0.0)
        IF (rElevation .GT. HN(iNode,iLayer)) THEN
            !Node in consideartion
            iNodeIDs(1)      = iGWNode
            !Update RHS vector
            rUpdateValues(1) = -pSubIrigs(indx)%rFlow
            CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateValues)
            !Update COEFF matrix
            rUpdateValues(1) = rConductance
            CALL Matrix%UpdateCOEFF(iGWComp,iGWNode,iCompIDs,iNodeIDs,rUpdateValues)
        END IF
      END DO
    END ASSOCIATE 

  END SUBROUTINE Simulate
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF TILE DRAINS AND SUBSURFACE IRRIGATION RELATED ENTITIES
  ! -------------------------------------------------------------
  SUBROUTINE ConvertTimeUnit(AppTileDrain,NewUnit)
    CLASS(AppTileDrainType)     :: AppTileDrain
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    REAL(8) :: Factor
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !Convert conductance time unit
    Factor                               = TimeIntervalConversion(NewUnit,AppTileDrain%TimeUnitConductance)
    AppTileDrain%TimeUnitConductance     = NewUnit
    AppTileDrain%TileDrains%rConductance = AppTileDrain%TileDrains%rConductance * Factor
    AppTileDrain%SubIrigs%rConductance   = AppTileDrain%SubIrigs%rConductance   * Factor
    
  END SUBROUTINE ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- TRANSFER HEADS AT ALL NODES FROM TEXT/DSS FILE TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(AppTileDrain,NTIME,TimeStep,iStat)
    CLASS(AppTileDrainType)       :: AppTileDrain
    INTEGER,INTENT(IN)            :: NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    
    CALL AppTileDrain%TileDrainHyd%Transfer_To_HDF(NTIME,TimeStep,iStat)
    
  END SUBROUTINE TransferOutputToHDF

    
END MODULE