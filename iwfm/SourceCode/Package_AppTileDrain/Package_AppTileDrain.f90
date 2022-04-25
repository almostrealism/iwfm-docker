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
MODULE Package_AppTileDrain
  USE Class_Version           , ONLY: ReadVersion
  USE MessageLogger           , ONLY: SetLastMessage            , &
                                      EchoProgress              , &
                                      MessageArray              , &
                                      f_iFatal                    
  USE GeneralUtilities        , ONLY: ConvertID_To_Index        , &
                                      StripTextUntilCharacter   , &
                                      CleanSpecialCharacters    , &
                                      UpperCase                 , &
                                      IntToText                 , &
                                      LocateInList 
  USE TimeSeriesUtilities     , ONLY: TimeStepType              , &
                                      TimeIntervalConversion
  USE IOInterface             , ONLY: GenericFileType                                  
  USE Package_Discretization  , ONLY: AppGridType               , &
                                      StratigraphyType          
  USE Package_Misc            , ONLY: f_iFlowDest_Outside       , &
                                      f_iFlowDest_StrmNode      , &
                                      f_iGWComp                 , &
                                      f_rSmoothMaxP
  USE AppTileDrain_Parameters , ONLY: f_iTileDrain              , &
                                      f_iSubIrig
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
  PUBLIC :: AppTileDrainType         , &
            f_iTileDrain             , &
            f_iSubIrig               , &
            f_cDescription_TDHyd
  

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
    PROCEDURE,PASS :: GetNDrain          
    PROCEDURE,PASS :: GetNSubIrig 
    PROCEDURE,PASS :: GetDrainIDs
    PROCEDURE,PASS :: GetNHydrographs
    PROCEDURE,PASS :: GetHydrographIDs
    PROCEDURE,PASS :: GetHydrographCoordinates
    PROCEDURE,PASS :: GetHydrographNames
    PROCEDURE,PASS :: GetHydOutputFileName
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
  CHARACTER(LEN=21),PARAMETER :: f_cDescription_TDHyd = 'Tile drain hydrograph'
  
  
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
  SUBROUTINE New(AppTileDrain,IsForInquiry,cFileName,cWorkingDirectory,iStrmNodeIDs,TimeStep,AppGrid,Stratigraphy,iStat) 
    CLASS(AppTileDrainType),INTENT(OUT) :: AppTileDrain
    LOGICAL,INTENT(IN)                  :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)                  :: iStrmNodeIDs(:)
    TYPE(TimeStepType),INTENT(IN)       :: TimeStep
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)   :: Stratigraphy
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    TYPE(GenericFileType)    :: TDFile
    INTEGER                  :: NDrain,NSubIrig
    REAL(8)                  :: Factor
    CHARACTER                :: TimeUnitConductance_TD*6,TimeUnitConductance_SI*6
    CHARACTER(:),ALLOCATABLE :: cVersion
    
    !Initailize
    iStat = 0

    !If no FileName, return
    IF (cFileName .EQ. '') RETURN
    
    !Open file
    CALL TDFile%New(cFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Tile drain/subsurface irrigation data',iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Read first line that stores version number
    CALL ReadVersion(TDFile,'TILE DRAINS',cVersion,iStat)
    IF (iStat .EQ. -1) RETURN

    !Read tile drain data
    CALL TileDrain_New(TDFile,AppGrid,Stratigraphy,iStrmNodeIDs,TimeUnitConductance_TD,AppTileDrain%TileDrains,iStat)
    IF (iStat .EQ. -1) RETURN
    NDrain              = SIZE(AppTileDrain%TileDrains)
    AppTileDrain%NDrain = NDrain
    
    !Read subsurface irrigation data
    CALL SubIrig_New(TDFile,AppGrid,Stratigraphy,TimeUnitConductance_SI,AppTileDrain%SubIrigs,iStat) 
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
    CALL AppTileDrain%TileDrainHyd%New(IsForInquiry,cWorkingDirectory,AppGrid%AppNode%ID,AppTileDrain%SubIrigs%ID,AppTileDrain%TileDrains%ID,AppTileDrain%SubIrigs%iGWNode,AppTileDrain%TileDrains%iGWNode,TimeStep,TDFile,iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Close file
    CALL TDFile%Kill()
    
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- NEW TileDrain DATA
  ! -------------------------------------------------------------
  SUBROUTINE TileDrain_New(InFile,AppGrid,Stratigraphy,iStrmNodeIDs,TimeUnitConductance,TileDrains,iStat)
    TYPE(GenericFileType)                       :: InFile
    TYPE(AppGridType),INTENT(IN)                :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)           :: Stratigraphy
    INTEGER,INTENT(IN)                          :: iStrmNodeIDs(:)
    CHARACTER(LEN=6),INTENT(OUT)                :: TimeUnitConductance
    TYPE(TileDrainType),ALLOCATABLE,INTENT(OUT) :: TileDrains(:)
    INTEGER,INTENT(OUT)                         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'TileDrain_New'
    INTEGER                      :: NDrain,indx,indx1,iStrmNode,iGWNodeID,iGWNode,NodeIDs(AppGrid%NNodes),ID,iNLayers
    REAL(8)                      :: FactH,FactCDC,DummyArray(6)
    CHARACTER                    :: ALine*1000
    
    !Initialize
    iStat    = 0
    NodeIDS  = AppGrid%AppNode%ID
    iNLayers = Stratigraphy%NLayers
    
    !Read data
    CALL InFile%ReadData(NDrain,iStat)   ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactH,iStat)    ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FactCDC,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)    ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)
    TimeUnitConductance = UpperCase(ADJUSTL(StripTextUntilCharacter(ALine,'/')))
   
    !Return if there are no tile drains
    IF (NDrain .EQ. 0) RETURN
    
    !Allocate
    ALLOCATE (TileDrains(NDrain))
    
    !Read data
    DO indx=1,NDrain
        CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        ID = INT(DummyArray(1))
        
        !Assign data to persistent arrays
        iGWNodeID = INT(DummyArray(2))
        CALL ConvertID_To_Index(iGWNodeID,NodeIDs,iGWNode)
        IF (iGWNode .EQ. 0) THEN
            CALL SetLastMessage('Groundwater node '//TRIM(IntTotext(iGWNodeID))//' listed for tile drain ID '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        TileDrains(indx)%ID           = ID
        TileDrains(indx)%iGWNode      = iGWNode
        TileDrains(indx)%rElevation   = DummyArray(3) * FactH
        TileDrains(indx)%rConductance = DummyArray(4) * FactCDC
        TileDrains(indx)%iGWNodeLayer = Stratigraphy%GetLayerNumberForElevation(TileDrains(indx)%rElevation , &
                                                                                AppGrid%X(iGWNode)          , &
                                                                                AppGrid%Y(iGWNode)          , &
                                                                                AppGrid                     )
        TileDrains(indx)%iDestType    = INT(DummyArray(5))
        TileDrains(indx)%iDest        = INT(DummyArray(6))
        
        !Make sure that a proper aquifer layer is assigned to the tile drain
        IF (TileDrains(indx)%iGWNodeLayer.LT.1  .OR.  TileDrains(indx)%iGWNodeLayer.GT.iNLayers) THEN
            MessageArray(1) = 'Tile drain '//TRIM(IntToText(ID))//' cannot be assigned a valid aquifer layer!'
            MessageArray(2) = 'Check its elevation.'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure that destination type is exceptible
        IF (TileDrains(indx)%iDestType.NE.f_iFlowDest_Outside .AND. TileDrains(indx)%iDestType.NE.f_iFlowDest_StrmNode) THEN
            CALL SetLastMessage('Flow destination type for tile drain '//TRIM(IntToText(ID))//' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure stream node number is valid
        IF (TileDrains(indx)%iDestType .EQ. f_iFlowDest_StrmNode) THEN
            iStrmNode = LocateInList(TileDrains(indx)%iDest , iStrmNodeIDs)
            IF (iStrmNode .LT. 1) THEN
                CALL SetLastMessage('Stream node '//TRIM(IntToText(TileDrains(indx)%iDest))//' for tile drain '//TRIM(IntToText(ID))//' is not modeled!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            TileDrains(indx)%iDest = iStrmNode
        END IF
        
        !Make sure tile drain IDs are not repeated
        DO indx1=1,indx-1
            IF (TileDrains(indx)%ID .EQ. TileDrains(indx1)%ID) THEN
                CALL SetLastMessage('Tile drain ID number '//TRIM(IntToText(ID))//' is used more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END DO
    
  END SUBROUTINE TileDrain_New


  ! -------------------------------------------------------------
  ! --- NEW SubIrig DATA
  ! -------------------------------------------------------------
  SUBROUTINE SubIrig_New(InFile,AppGrid,Stratigraphy,TimeUnitConductance,SubIrigs,iStat)
    TYPE(GenericFileType)                           :: InFile
    TYPE(AppGridType),INTENT(IN)                    :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)               :: Stratigraphy
    CHARACTER(LEN=6),INTENT(OUT)                    :: TimeUnitConductance
    TYPE(BaseTileDrainType),ALLOCATABLE,INTENT(OUT) :: SubIrigs(:)
    INTEGER,INTENT(OUT)                             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+11) :: ThisProcedure = ModName // 'SubIrig_New'
    INTEGER                      :: NDrain,indx,indx1,iGWNode,iGWNodeID,ID,NodeIDs(AppGrid%NNodes)
    REAL(8)                      :: FactH,FactCDC,DummyArray(4)
    CHARACTER                    :: ALine*1000
    
    !Initialize
    iStat   = 0
    NodeIDs = AppGrid%AppNode%ID
    
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
        
        ID = INT(DummyArray(1))
        
        !Assign data to persistent arrays
        iGWNodeID = INT(DummyArray(2))
        CALL ConvertID_To_Index(iGWNodeID,NodeIDS,iGWNode)
        IF (iGWNode .EQ. 0) THEN
            CALL SetLastMessage('Groundwater node '//TRIM(IntTotext(iGWNodeID))//' listed for subsurface irrigation ID '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        SubIrigs(indx)%iGWNode      = iGWNode
        SubIrigs(indx)%rElevation   = DummyArray(3) * FactH
        SubIrigs(indx)%rConductance = DummyArray(4) * FactCDC
        SubIrigs(indx)%iGWNodeLayer = Stratigraphy%GetLayerNumberForElevation(SubIrigs(indx)%rElevation   , &
                                                                              AppGrid%X(iGWNode)          , &
                                                                              AppGrid%Y(iGWNode)          , &
                                                                              AppGrid                     )
              
        !Make sure subsurface irrigation IDs are not repeated
        DO indx1=1,indx-1
            IF (SubIrigs(indx)%ID .EQ. SubIrigs(indx1)%ID) THEN
                CALL SetLastMessage('Subsurface irrigation ID number '//TRIM(IntToText(ID))//' is used more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
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
  ! --- GET HYDROGRAPH OUTPUT FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetHydOutputFileName(AppTileDrain,cFileName)
    CLASS(AppTileDrainType),INTENT(IN)   :: AppTileDrain
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (cFileName , STAT=ErrorCode)
    CALL AppTileDrain%TileDrainHyd%GetOutFileName(cFileName)
    
  END SUBROUTINE GetHydOutputFileName
  
  
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
      IF (AppTileDrain%TileDrains(indx)%iDestType .NE. f_iFlowDest_StrmNode) CYCLE
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
      CASE (f_iTileDrain)
        IF (AppTileDrain%NDrain .EQ. 0) THEN
          Flows = 0.0
        ELSE
          Flows = AppGrid%AccumSomeNodeValuesToSubregions(AppTileDrain%TileDrains%iGWNode,AppTileDrain%TileDrains%rFlow)
        END IF
      
      !Subsurface irrigation  
      CASE (f_iSubIrig)
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
  ! --- GET TILE DRAIN IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetDrainIDs(AppTileDrain,IDs)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER,INTENT(OUT)                :: IDs(:)
    
    IDs = AppTileDrain%TileDrains%ID
    
  END SUBROUTINE GetDrainIDs


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
  ! --- GET HYDROGRAPH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(AppTileDrain,iHydType,IDs)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER,INTENT(IN)                 :: iHydType   
    INTEGER,INTENT(OUT)                :: IDs(:)
    
    !Local variables
    INTEGER,ALLOCATABLE :: iAllIDs(:) 
    
    !Initialize
    SELECT CASE (iHydType)
        CASE (f_iTileDrain)
            ALLOCATE (iAllIDs(AppTileDrain%NDrain))
            iAllIDs = AppTileDrain%TileDrains%ID
            
        CASE (f_iSubIrig)
            ALLOCATE (iAllIDs(AppTileDrain%NSubIrig))
            iAllIDs = AppTileDrain%SubIrigs%ID
    END SELECT
    
    CALL AppTileDrain%TileDrainHyd%GetHydrographIDs(iHydType,iAllIDs,IDs)
    
  END SUBROUTINE GetHydrographIDs


  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographCoordinates(AppTileDrain,iHydType,GridX,GridY,XHyd,YHyd)
    CLASS(AppTileDrainType),INTENT(IN) :: AppTileDrain
    INTEGER,INTENT(IN)                 :: iHydType 
    REAL(8),INTENT(IN)                 :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)                :: XHyd(:),YHyd(:)
    
    !Local variables
    INTEGER,ALLOCATABLE :: iNodes(:) 
    
    !Initialize
    SELECT CASE (iHydType)
        CASE (f_iTileDrain)
            ALLOCATE (iNodes(AppTileDrain%NDrain))
            iNodes = AppTileDrain%TileDrains%iGWNode
            
        CASE (f_iSubIrig)
            ALLOCATE (iNodes(AppTileDrain%NSubIrig))
            iNodes = AppTileDrain%SubIrigs%iGWNode
    END SELECT
    
    CALL AppTileDrain%TileDrainHyd%GetHydrographCoordinates(iHydType,iNodes,GridX,GridY,XHyd,YHyd)
    
  END SUBROUTINE GetHydrographCoordinates


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
      CASE (f_iTileDrain)
        ALLOCATE (rFlows(AppTileDrain%NDrain))
        rFlows = AppTileDrain%TileDrains%rFlow
        
      CASE (f_iSubIrig)
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
    CASE (f_iTileDrain)
        iDim = AppTileDrain%NDrain
        ALLOCATE (iGWNodes(iDim) , iGWNodeLayers(iDim))
        iGWNodes      = AppTileDrain%TileDrains%iGWNode
        iGWNodeLayers = AppTileDrain%TileDrains%iGWNodeLayer
        
    CASE (f_iSubIrig)
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
    REAL(8)           :: rElevation,rConductance,rUpdateValues(1),rHeadDiff
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]

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
            rHeadDiff               = HN(iNode,iLayer) - rElevation
            pTileDrains(indx)%rFlow = -rConductance * MAX(0.0 , rHeadDiff)
            IF (rElevation .LT. HN(iNode,iLayer)) THEN
                !Node in consideartion
                iNodeIDs(1)      = iGWNode
                !Update RHS vector
                rUpdateValues(1) = -pTileDrains(indx)%rFlow
                CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateValues)
                !Update COEFF matrix
                rUpdateValues(1) = 0.5d0 * rConductance * (1d0 + rHeadDiff/SQRT(rHeadDiff*rHeadDiff+f_rSmoothMaxP))
                CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,1,iCompIDs,iNodeIDs,rUpdateValues)
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
            rHeadDiff             = rElevation - HN(iNode,iLayer)
            pSubIrigs(indx)%rFlow = rConductance * MAX(rHeadDiff , 0.0)
            IF (rElevation .GT. HN(iNode,iLayer)) THEN
                !Node in consideartion
                iNodeIDs(1)      = iGWNode
                !Update RHS vector
                rUpdateValues(1) = -pSubIrigs(indx)%rFlow
                CALL Matrix%UpdateRHS(iCompIDs,iNodeIDs,rUpdateValues)
                !Update COEFF matrix
                rUpdateValues(1) = 0.5d0 * rConductance * (1d0 + rHeadDiff/SQRT(rHeadDiff*rHeadDiff+f_rSmoothMaxP))
                CALL Matrix%UpdateCOEFF(f_iGWComp,iGWNode,1,iCompIDs,iNodeIDs,rUpdateValues)
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