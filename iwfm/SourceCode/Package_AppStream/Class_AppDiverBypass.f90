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
MODULE Class_AppDiverBypass
  USE MessageLogger                , ONLY: LogMessage         , &
                                           SetLastMessage     , &
                                           EchoProgress       , &
                                           MessageArray       , &
                                           iFatal             , &
                                           iInfo
  USE TimeSeriesUtilities
  USE GeneralUtilities
  USE IOInterface
  USE Package_Misc
  USE Package_Discretization
  USE Package_Budget               , ONLY: BudgetType         , &
                                           BudgetHeaderType   , &
                                           VolumeUnitMarker   , &
                                           LocationNameMarker , &
                                           VR                 , &
                                           PER_CUM
  USE Package_ComponentConnectors
  USE Class_ElemToRecvLoss
  USE Class_Diversion   
  USE Class_Bypass
  USE Class_RechargeZone
  USE Class_StrmReach
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
  PUBLIC :: AppDiverBypassType                      , &
            iDiverRecvLoss                          , &
            iBypassRecvLoss                         , &
            iAllRecvLoss
     


  ! -------------------------------------------------------------
  ! --- DIVERSIONS DATA FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: DiverFileType
    PRIVATE
    REAL(8) :: Fact     = 1.0
  END TYPE DiverFileType
  
  
  ! -------------------------------------------------------------
  ! --- DIVERSIONS DATA TYPE
  ! -------------------------------------------------------------
  TYPE AppDiverBypassType
    INTEGER                                   :: NDiver                         = 0
    INTEGER                                   :: NBypass                        = 0
    TYPE(DiversionType),ALLOCATABLE           :: Diver(:)
    CHARACTER(LEN=6)                          :: TimeUnitStrmFlow               = ''
    CHARACTER(LEN=6)                          :: TimeUnitBypass                 = ''
    TYPE(BypassType),ALLOCATABLE              :: Bypasses(:)
    TYPE(ElemToRecvLossType),ALLOCATABLE      :: ElemToDiverRecvLoss(:)
    TYPE(ElemToRecvLossType),ALLOCATABLE      :: ElemToByPassRecvLoss(:)
    TYPE(DiverFileType)                       :: DiverFile
    LOGICAL                                   :: lDiverRequired_Updated         = .FALSE.   !Flag to check if diversions are updated (either read from file or adjusted in supply-adjustment run)
    REAL(8),ALLOCATABLE                       :: NodalDiverRequired(:)                      !Required diversions at each stream node
    REAL(8),ALLOCATABLE                       :: NodalDiverActual(:)                        !Actual diversions at each stream node
    LOGICAL                                   :: DiverDetailsBudRawFile_Defined = .FALSE.
    TYPE(BudgetType)                          :: DiverDetailsBudRawFile
  CONTAINS
    PROCEDURE,PASS :: New                            => AppDiverBypass_New
    PROCEDURE,PASS :: Kill                           => AppDiverBypass_Kill
    PROCEDURE,PASS :: GetBypassRecieved              => AppDiverBypass_GetBypassRecieved       
    PROCEDURE,PASS :: GetElemRecvLosses              => AppDiverBypass_GetElemRecvLosses        
    PROCEDURE,PASS :: GetSubregionalRecvLosses       => AppDiverBypass_GetSubregionalRecvLosses
    PROCEDURE,PASS :: GetNodeDiversions              => AppDiverBypass_GetNodeDiversions       
    PROCEDURE,PASS :: GetReachDiversions             => AppDiverBypass_GetReachDiversions      
    PROCEDURE,PASS :: GetNodeDiversionShort          => AppDiverBypass_GetNodeDiversionShort    
    PROCEDURE,PASS :: GetReachDiversionShort         => AppDiverBypass_GetReachDiversionShort   
    PROCEDURE,PASS :: GetNodeNetBypass               => AppDiverBypass_GetNodeNetBypass        
    PROCEDURE,PASS :: GetReachNetBypass              => AppDiverBypass_GetReachNetBypass
    PROCEDURE,PASS :: GetExportNode
    PROCEDURE,PASS :: IsBypassNode
    PROCEDURE,PASS :: IsRatingTableTypeBypassNode
    PROCEDURE,PASS :: ConvertTimeUnit                => AppDiverBypass_ConvertTimeUnit         
    PROCEDURE,PASS :: CompileNodalDiversions         => AppDiverBypass_CompileNodalDiversions   
    PROCEDURE,PASS :: ComputeBypass                  => AppDiverBypass_ComputeBypass            
    PROCEDURE,PASS :: ComputeDiversions              => AppDiverBypass_ComputeDiversions        
    PROCEDURE,PASS :: ReadTSData                     => AppDiverBypass_ReadTSData               
    PROCEDURE,PASS :: PrintResults                   => AppDiverBypass_PrintResults
  END TYPE AppDiverBypassType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen      = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName         = 'Class_AppDiverBypass::'
  INTEGER,PARAMETER                   :: iDiverRecvLoss  = 1 , &
                                         iBypassRecvLoss = 2 , &
                                         iAllRecvLoss    = 3

  
  

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
  ! --- INSTANTIATE DIVERSIONS DATABASE
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_New(AppDiverBypass,IsForInquiry,DiverSpecFileName,BypassSpecFileName,DiverFileName,DiverDetailBudFileName,cVersion,NTIME,TimeStep,NStrmNodes,Reaches,AppGrid,StrmLakeConnector,iStat)
    CLASS(AppDiverBypassType),INTENT(OUT) :: AppDiverBypass
    LOGICAL,INTENT(IN)                    :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: DiverSpecFileName,BypassSpecFileName,DiverFileName,DiverDetailBudFileName,cVersion
    INTEGER,INTENT(IN)                    :: NTIME,NStrmNodes
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    TYPE(StrmReachType),INTENT(IN)        :: Reaches(:)
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    TYPE(StrmLakeConnectorType)           :: StrmLakeConnector
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'AppDiverBypass_New'
    INTEGER                      :: NElements,NSubregions,ErrorCode,iSize
    TYPE(BudgetHeaderType)       :: BudHeader

    !Initialize
    iStat       = 0
    NElements   = AppGrid%NElements
    NSubregions = AppGrid%NSubregions
    
    !Instantiate the diversions data file
    CALL DiverFile_New(DiverFileName,TimeStep,AppDiverBypass%DiverFile,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate the diversions and delivery database
    CALL Diversion_New(DiverSpecFileName,AppGrid,Reaches,AppDiverBypass%Diver,iStat)
    IF (iStat .EQ. -1) RETURN
    AppDiverBypass%NDiver = SIZE(AppDiverBypass%Diver) 
    
    !Allocate memory for element-to-diversion-recoverable-loss pointers
    IF (AppDiverBypass%NDiver .GT. 0) THEN
        IF (SUM(AppDiverBypass%Diver%Recharge%NZones) .GT. 0) THEN
           ALLOCATE (AppDiverBypass%ElemToDiverRecvLoss(NElements) ,STAT=ErrorCode)
           IF (ErrorCode .NE. 0) THEN
               CALL SetLastMessage('Error in allocating memory for element-to-diversion-recoverable-loss pointers!',iFatal,ThisProcedure)
               iStat = -1
               RETURN
           END IF
           CALL ElemToRecvLossPointers(NElements,AppDiverBypass%Diver%Recharge,AppDiverBypass%ElemToDiverRecvLoss)
        END IF
    END IF
    
    !Instantiate the bypass database
    CALL Bypass_New(BypassSpecFileName,NStrmNodes,Reaches,StrmLakeConnector,AppDiverBypass%TimeUnitStrmFlow,AppDiverBypass%TimeUnitBypass,AppDiverBypass%Bypasses,iStat)
    IF (iStat .EQ. -1) RETURN
    AppDiverBypass%NBypass = SIZE(AppDiverBypass%Bypasses)
    
    !Allocate memory for element-to-bypass-recoverable-loss pointers
    IF (AppDiverBypass%NBypass .GT. 0) THEN
        IF (SUM(AppDiverBypass%Bypasses%Recharge%NZones) .GT. 0) THEN
            ALLOCATE (AppDiverBypass%ElemToBypassRecvLoss(NElements) ,STAT=ErrorCode)
            IF (ErrorCode .NE. 0) THEN
                CALL SetLastMessage('Error in allocating memory for element-to-bypass-recoverable-loss pointers!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            CALL ElemToRecvLossPointers(NElements,AppDiverBypass%Bypasses%Recharge,AppDiverBypass%ElemToBypassRecvLoss)
        END IF
    END IF
    
    !Allocate memory for diversions at each stream node
    IF (AppDiverBypass%NDiver .GT. 0) THEN
        iSize = NStrmNodes
    ELSE
        iSize = 0
    END IF
    ALLOCATE (AppDiverBypass%NodalDiverRequired(iSize) , AppDiverBypass%NodalDiverActual(iSize))
    
    !Make sure the diversions file is specified if diversions and/or bypasses are defined that needs that file
    IF (DiverFileName .EQ. '') THEN
        !Are any diversions that point to diversions file defined?
        IF (AppDiverBypass%NDiver .GT. 0) THEN
            MessageArray(1) = 'Time-series diversions data file needs to be '
            MessageArray(2) = 'specified when there are diversions modeled!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Any bypasses that point to diversions file?
        IF (ANY(AppDiverBypass%Bypasses%iColBypass.GT.0)) THEN
            MessageArray(1) = 'Time-series diversions data file needs to be specified'
            MessageArray(2) = 'when there are bypasses with pre-defined bypass rates!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Make sure that there are enough data columns in the diversions data file
    IF (DiverFileName .NE. '') THEN
        !Check diversions
        CALL AppDiverBypass%DiverFile%CheckColNum('time-series diversions file',AppDiverBypass%Diver%iMaxDiverCol,.FALSE.,iStat)    ;  IF (iStat .EQ. -1) RETURN
        CALL AppDiverBypass%DiverFile%CheckColNum('time-series diversions file',AppDiverBypass%Diver%iColRecvLoss,.TRUE.,iStat)     ;  IF (iStat .EQ. -1) RETURN
        CALL AppDiverBypass%DiverFile%CheckColNum('time-series diversions file',AppDiverBypass%Diver%iColNonRecvLoss,.TRUE.,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL AppDiverBypass%DiverFile%CheckColNum('time-series diversions file',AppDiverBypass%Diver%Deli%iColDeli,.TRUE.,iStat)    ;  IF (iStat .EQ. -1) RETURN
        
        !Check bypasses
        CALL AppDiverBypass%DiverFile%CheckColNum('time-series diversions file',AppDiverBypass%Bypasses%iColBypass,.FALSE.,iStat)   ;  IF (iStat .EQ. -1) RETURN

    END IF
    
    !Instantiate diversion details budget output file
    IF (DiverDetailBudFileName .NE. '') THEN
        !Make sure that diversions are specified
        IF (AppDiverBypass%NDiver .EQ. 0) THEN
            MessageArray(1) = 'There are no diversions specified.'
            MessageArray(2) = 'Print-out of diversion details budget file is suppressed!'
            CALL LogMessage(MessageArray(1:2),iInfo,ThisProcedure)
        ELSE 
            IF (IsForInquiry) THEN
                CALL AppDiverBypass%DiverDetailsBudRawFile%New(TRIM(DiverDetailBudFileName),iStat)
                IF (iStat .EQ. -1) RETURN
            ELSE
                BudHeader = PrepareDiverDetailsBudgetHeader(AppDiverBypass%NDiver,TimeStep,NTIME,AppDiverBypass%Diver,cVersion)
                CALL AppDiverBypass%DiverDetailsBudRawFile%New(TRIM(DiverDetailBudFileName),BudHeader,iStat)
                IF (iStat .EQ. -1) RETURN
                CALL BudHeader%Kill()
            END IF
            AppDiverBypass%DiverDetailsBudRawFile_Defined = .TRUE.
      END IF
    END IF  
     
  END SUBROUTINE AppDiverBypass_New
  
  
  ! -------------------------------------------------------------
  ! --- INITIALIZE DIVERSIONS TIME SERIES DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE DiverFile_New(FileName,TimeStep,DiverDataFile,iStat)
    CHARACTER(LEN=*),INTENT(IN)   :: FileName
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(DiverFileType)           :: DiverDataFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    REAL(8) :: Factor(1)
    LOGICAL :: DummyArray(1) = (/.TRUE./)
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (FileName .EQ. '') RETURN
    
    !Print progress
    CALL EchoProgress('Instantiating diversions data file')

    !Instantiate
    CALL DiverDataFile%Init(FileName,'diversions data file',TimeStep%TrackTime,1,.TRUE.,Factor,DummyArray,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    DiverDataFile%Fact = Factor(1)

  END SUBROUTINE DiverFile_New




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
  ! --- KILL DIVERSIONS/BYPASSES DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_Kill(AppDiverBypass)
    CLASS(AppDiverBypassType) :: AppDiverBypass
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Deallocate array attributes
    DEALLOCATE (AppDiverBypass%Diver                , &
                AppDiverBypass%Bypasses             , &
                AppDiverBypass%ElemToDiverRecvLoss  , &
                AppDiverBypass%ElemToBypassRecvLoss , &
                AppDiverBypass%NodalDiverRequired   , &
                AppDiverBypass%NodalDiverActual     , &
                STAT=ErrorCode                      )
    
    !Close diversion data file
    CALL DiverFile_Kill(AppDiverBypass%DiverFile)
    
    !Close diversion details output file
    IF (AppDiverBypass%DiverDetailsBudRawFile_Defined) CALL AppDiverBypass%DiverDetailsBudRawFile%Kill()
    
    !Set attributes to their default values
    AppDiverBypass%NDiver  = 0
    AppDiverBypass%NBypass = 0
    AppDiverBypass%TimeUnitStrmFlow = ''
    AppDiverBypass%TimeUnitBypass   = ''
    AppDiverBypass%lDiverRequired_Updated = .FALSE.
    AppDiverBypass%DiverDetailsBudRawFile_Defined = .FALSE.    
    
  END SUBROUTINE AppDiverBypass_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL DIVERSION DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE DiverFile_Kill(DiverFile)
    TYPE(DiverFileType) :: DiverFile
    
    !Local variables
    TYPE(DiverFileType) :: Dummy
    
    !Close file
    CALL DiverFile%Close()
    
    !Set attributes to their default values
    DiverFile = Dummy
    
  END SUBROUTINE DiverFile_Kill
  
  
  
  
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
  ! --- GET BYPASS EXPORT NODE
  ! -------------------------------------------------------------
  SUBROUTINE GetExportNode(AppDiverBypass,BypassID,iNode_Exp,iStat)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: BypassID
    INTEGER,INTENT(OUT)                  :: iNode_Exp,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'GetExportNode'
    
    !INitialize
    iStat = 0
    
    !Make sure bypass ID is defined 
    IF (BypassID.LT.1  .OR.  BypassID.GT.AppDiverBypass%NBypass) THEN
        CALL SetLastMessage('Bypass ID '//TRIM(IntToText(BypassID))//' is not simulated!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Get the export node
    iNode_Exp = AppDiverBypass%Bypasses(BypassID)%iNode_Exp
    
  END SUBROUTINE GetExportNode
  

  ! -------------------------------------------------------------
  ! --- GET NET BYPASS FROM A SET OF NODES 
  ! -------------------------------------------------------------
  FUNCTION AppDiverBypass_GetNodeNetBypass(AppDiverBypass,iNodes) RESULT(Bypasses)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: iNodes(:)
    REAL(8)                              :: Bypasses(SIZE(iNodes))
    
    !Local variables
    INTEGER :: indxBypass,iNode_Exp,iNodeRecieve,iLoc
    
    !Initialize
    Bypasses = 0.0
    
    !Return, if there are no bypasses
    IF (AppDiverBypass%NBypass .EQ. 0) RETURN
    
    !Compile nodal bypasses first
    DO indxBypass=1,AppDiverBypass%NBypass
      iNode_Exp = AppDiverBypass%Bypasses(indxBypass)%iNode_Exp
      iLoc      = LocateInList(iNode_Exp,iNodes)
      IF (iLoc .GT. 0) &
        Bypasses(iLoc) = Bypasses(iLoc) + AppDiverBypass%Bypasses(indxBypass)%Bypass_Out
      IF (AppDiverBypass%Bypasses(indxBypass)%iDestType .EQ. FlowDest_StrmNode) THEN
        iNodeRecieve = AppDiverBypass%Bypasses(indxBypass)%iDest
        iLoc = LocateInList(iNodeRecieve,iNodes)
        IF (iLoc .GT. 0) &
          Bypasses(iLoc) = Bypasses(iLoc) - AppDiverBypass%Bypasses(indxBypass)%Bypass_Recieved
      END IF
    END DO
    
  END FUNCTION AppDiverBypass_GetNodeNetBypass
  
    
  ! -------------------------------------------------------------
  ! --- GET NET BYPASS FROM EACH REACH 
  ! -------------------------------------------------------------
  FUNCTION AppDiverBypass_GetReachNetBypass(AppDiverBypass,NReaches,Reaches) RESULT(Bypasses)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: NReaches
    TYPE(StrmReachType),INTENT(IN)       :: Reaches(NReaches)
    REAL(8)                              :: Bypasses(NReaches)
    
    !Local variables
    INTEGER                                           :: indxBypass,iNode_Exp,iNodeRecieve,indxReach, &
                                                         iUpstrmNode,iDownstrmNode
    REAL(8),DIMENSION(Reaches(NReaches)%DownStrmNode) :: NodalBypassOut,NodalBypassRecieved
    
    !Initialize
    Bypasses = 0.0
    
    !Return, if there are no bypasses
    IF (AppDiverBypass%NBypass .EQ. 0) RETURN
    
    !Compile nodal bypasses first
    NodalBypassOut      = 0.0
    NodalBypassRecieved = 0.0
    DO indxBypass=1,AppDiverBypass%NBypass
      iNode_Exp                 = AppDiverBypass%Bypasses(indxBypass)%iNode_Exp
      NodalBypassOut(iNode_Exp) = NodalBypassOut(iNode_Exp) + AppDiverBypass%Bypasses(indxBypass)%Bypass_Out
      IF (AppDiverBypass%Bypasses(indxBypass)%iDestType .EQ. FlowDest_StrmNode) THEN
        iNodeRecieve                      = AppDiverBypass%Bypasses(indxBypass)%iDest
        NodalBypassRecieved(iNodeRecieve) = NodalBypassRecieved(iNodeRecieve) + AppDiverBypass%Bypasses(indxBypass)%Bypass_Recieved
      END IF
    END DO
    
    !Then, accumulate for reaches
    DO indxReach=1,NReaches
      iUpstrmNode         = Reaches(indxReach)%UpstrmNode
      iDownstrmNode       = Reaches(indxReach)%DownstrmNode
      Bypasses(indxReach) = SUM(NodalBypassOut(iUpstrmNode:iDownstrmNode) - NodalBypassRecieved(iUpstrmNode:iDownstrmNode))
    END DO
    
  END FUNCTION AppDiverBypass_GetReachNetBypass
  
    
  ! -------------------------------------------------------------
  ! --- GET TOTAL DIVERSIONS FROM A SET OF NODES 
  ! -------------------------------------------------------------
  FUNCTION AppDiverBypass_GetNodeDiversions(AppDiverBypass,iNodes) RESULT(Diversions)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: iNodes(:)
    REAL(8)                              :: Diversions(SIZE(iNodes))
    
    IF (AppDiverBypass%NDiver .EQ. 0) THEN
        Diversions = 0.0
    ELSE
        Diversions = AppDiverBypass%NodalDiverActual(iNodes)
    END IF
    
  END FUNCTION AppDiverBypass_GetNodeDiversions
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL DIVERSIONS FROM EACH REACH 
  ! -------------------------------------------------------------
  FUNCTION AppDiverBypass_GetReachDiversions(AppDiverBypass,NReaches,Reaches) RESULT(Diversions)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: NReaches
    TYPE(StrmReachType),INTENT(IN)       :: Reaches(NReaches)
    REAL(8)                              :: Diversions(NReaches)
    
    !Local variables
    INTEGER :: indxReach,iUpstrmNode,iDownstrmNode
    
    !Initialize
    Diversions = 0.0
    
    !Return, if there are no diversions
    IF (AppDiverBypass%NDiver .EQ. 0) RETURN
    
    !Accumulate nodal diversions to reaches
    DO indxReach=1,NReaches
      iUpstrmNode           = Reaches(indxReach)%UpstrmNode
      iDownstrmNode         = Reaches(indxReach)%DownstrmNode
      Diversions(indxReach) = SUM(AppDiverBypass%NodalDiverActual(iUpstrmNode:iDownstrmNode))
    END DO
    
  END FUNCTION AppDiverBypass_GetReachDiversions
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL DIVERSION SHORTAGES FOR A SET OF NODES 
  ! -------------------------------------------------------------
  FUNCTION AppDiverBypass_GetNodeDiversionShort(AppDiverBypass,iNodes) RESULT(Shorts)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: iNodes(:)
    REAL(8)                              :: Shorts(SIZE(iNodes))
    
    !Local variables
    INTEGER :: iLoc,indxDiver
    
    !Initialize
    Shorts = 0.0
    
    !Return, if there are no diversions
    IF (AppDiverBypass%NDiver .EQ. 0) RETURN
    
    !Accumulate shortes at selected nodes
    DO indxDiver=1,AppDiverBypass%NDiver
      iLoc  = LocateInList(AppDiverBypass%Diver(indxDiver)%iStrmNode,iNodes)
      IF (iLoc .GT. 0) Shorts(iLoc) = Shorts(iLoc) + AppDiverBypass%Diver(indxDiver)%DiverRequired - AppDiverBypass%Diver(indxDiver)%DiverActual
    END DO
    
  END FUNCTION AppDiverBypass_GetNodeDiversionShort
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL DIVERSION SHORTAGES FOR EACH REACH 
  ! -------------------------------------------------------------
  FUNCTION AppDiverBypass_GetReachDiversionShort(AppDiverBypass,NReaches,Reaches) RESULT(Shorts)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: NReaches
    TYPE(StrmReachType),INTENT(IN)       :: Reaches(NReaches)
    REAL(8)                              :: Shorts(NReaches)
    
    !Local variables
    INTEGER :: indxReach,iUpstrmNode,iDownstrmNode,indxDiver
    REAL(8) :: NodeShorts(0:Reaches(NReaches)%DownStrmNode)
    
    !Initialize
    Shorts = 0.0
    
    !Return, if there are no diversions
    IF (AppDiverBypass%NDiver .EQ. 0) RETURN
    
    !Compile nodal diversions first
    NodeShorts = 0.0
    DO indxDiver=1,AppDiverBypass%NDiver
        NodeShorts(AppDiverBypass%Diver(indxDiver)%iStrmNode) = NodeShorts(AppDiverBypass%Diver(indxDiver)%iStrmNode) + AppDiverBypass%Diver(indxDiver)%DiverRequired - AppDiverBypass%Diver(indxDiver)%DiverActual
    END DO
    
    !Then, accumulate nodel diversions to reaches
    DO indxReach=1,NReaches
      iUpstrmNode       = Reaches(indxReach)%UpstrmNode
      iDownstrmNode     = Reaches(indxReach)%DownstrmNode
      Shorts(indxReach) = SUM(NodeShorts(iUpstrmNode:iDownstrmNode))
    END DO
    
  END FUNCTION AppDiverBypass_GetReachDiversionShort
  
  
  ! -------------------------------------------------------------
  ! --- GET SUBREGIONAL RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  PURE FUNCTION AppDiverBypass_GetSubregionalRecvLosses(AppDiverBypass,AppGrid) RESULT(RecvLosses)
    CLASS(AppDiverBypassTYpe),INTENT(IN) :: AppDiverBypass
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    REAL(8)                              :: RecvLosses(AppGrid%NSubregions)
    
    !Local variables
    INTEGER :: indx,indxZone,iElem,iRegion
    REAL(8) :: Frac
    
    !Initialize
    RecvLosses = 0.0
    
    !Recoverable losses from diversions
    ASSOCIATE (pDivers => AppDiverBypass%Diver      , &
               pBypasses => AppDiverBypass%Bypasses )
               
      !Process diversions
      DO indx=1,AppDiverBypass%NDiver
        DO indxZone=1,pDivers(indx)%Recharge%NZones
          iElem               = pDivers(indx)%Recharge%Zones(indxZone)
          Frac                = pDivers(indx)%Recharge%Fracs(indxZone)
          iRegion             = AppGrid%AppElement(iElem)%Subregion
          RecvLosses(iRegion) = RecvLosses(iRegion) + pDivers(indx)%RecvLoss * Frac
        END DO
      END DO
      
      !Process bypasses
      DO indx=1,AppDiverBypass%NBypass
        DO indxZone=1,pBypasses(indx)%Recharge%NZones
          iElem               = pBypasses(indx)%Recharge%Zones(indxZone)
          Frac                = pBypasses(indx)%Recharge%Fracs(indxZone)
          iRegion             = AppGrid%AppElement(iElem)%Subregion
          RecvLosses(iRegion) = RecvLosses(iRegion) + pBypasses(indx)%RecvLoss * Frac
        END DO
      END DO

    END ASSOCIATE
    
  END FUNCTION AppDiverBypass_GetSubregionalRecvLosses
  
  
  ! -------------------------------------------------------------
  ! --- GET ELEMENT LEVEL RECOVERABLE LOSSES
  ! -------------------------------------------------------------
  PURE FUNCTION AppDiverBypass_GetElemRecvLosses(AppDiverBypass,NElements,iSource) RESULT(ElemRecvLosses)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: NElements,iSource
    REAL(8)                              :: ElemRecvLosses(NElements)
    
    !Local variables
    INTEGER :: NRecharge
    
    SELECT CASE (iSource)
      !Recoverable losses from diversions
      CASE (iDiverRecvLoss)
        NRecharge = AppDiverBypass%NDiver
        IF (NRecharge .GT. 0) THEN
          ElemRecvLosses = CompileRecvLosses(NElements,NRecharge,AppDiverBypass%Diver%RecvLoss,AppDiverBypass%Diver%Recharge)
        ELSE
          ElemRecvLosses = 0.0
        END IF
        
      !Recoverable losses from bypasses
      CASE (iBypassRecvLoss) 
        NRecharge = AppDiverBypass%NBypass
        IF (NRecharge .GT. 0) THEN
          ElemRecvLosses = CompileRecvLosses(NElements,NRecharge,AppDiverBypass%Bypasses%RecvLoss,AppDiverBypass%Bypasses%Recharge)
        ELSE
          ElemRecvLosses = 0.0
        END IF
          
      !Recoverable losses from both diversions and bypasses
      CASE (iAllRecvLoss)
        IF (AppDiverBypass%NDiver.EQ.0  .AND. AppDiverBypass%NBypass.EQ.0) THEN
          ElemRecvLosses = 0.0
        ELSE
          !Diversions
          NRecharge = AppDiverBypass%NDiver
          IF (NRecharge .GT. 0) THEN
            ElemRecvLosses = CompileRecvLosses(NElements,NRecharge,AppDiverBypass%Diver%RecvLoss,AppDiverBypass%Diver%Recharge)
          ELSE
            ElemRecvLosses = 0.0
          END IF
          !Bypasses
          NRecharge = AppDiverBypass%NBypass
          IF (NRecharge .GT. 0)  &
            ElemRecvLosses = ElemRecvLosses + CompileRecvLosses(NElements,NRecharge,AppDiverBypass%Bypasses%RecvLoss,AppDiverBypass%Bypasses%Recharge)
        END IF

    END SELECT
    
  
  CONTAINS  


    ! ##################################################
    ! ### COMPILE ELEMENT LEVEL RECOVERABLE LOSSES
    ! ##################################################
    PURE FUNCTION CompileRecvLosses(NElements,NRecharge,RecvLosses,Recharge) RESULT(ElemRecvLosses)
      INTEGER,INTENT(IN)                :: NElements,NRecharge
      REAL(8),INTENT(IN)                :: RecvLosses(:)
      TYPE(RechargeZoneType),INTENT(IN) :: Recharge(:)
      REAL(8)                           :: ElemRecvLosses(NElements)
    
      !Local variables
      INTEGER :: indx,indxZone,iElem
      REAL(8) :: Frac
    
      !Initialize
      ElemRecvLosses = 0.0
    
      !Compile
      DO indx=1,NRecharge
        DO indxZone=1,Recharge(indx)%NZones
          iElem                 = Recharge(indx)%Zones(indxZone)
          Frac                  = Recharge(indx)%Fracs(indxZone)
          ElemRecvLosses(iElem) = ElemRecvLosses(iElem) + RecvLosses(indx) * Frac
        END DO
      END DO
    
    END FUNCTION CompileRecvLosses
    
  END FUNCTION AppDiverBypass_GetElemRecvLosses


  ! -------------------------------------------------------------
  ! --- GET RECEIVED BYPASS FLOW AT A DESTINATION
  ! -------------------------------------------------------------
  FUNCTION AppDiverBypass_GetBypassRecieved(AppDiverBypass,iDestType,iDest) RESULT(BypassRecieved)
    CLASS(AppDiverBypassType),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: iDestType,iDest
    REAL(8)                              :: BypassRecieved
    
    !Local variables
    INTEGER :: indxBypass
    
    !Initialize
    BypassRecieved = 0.0
    
    !Find the recieved bypass
    ASSOCIATE (pBypasses => AppDiverBypass%Bypasses)
      DO indxBypass=1,AppDiverBypass%NBypass
        IF (pBypasses(indxBypass)%iDest .EQ. iDest) THEN
          IF (pBypasses(indxBypass)%iDestType .EQ. iDestType)  &
            BypassRecieved = BypassRecieved + pBypasses(indxBypass)%Bypass_Recieved
        END IF
      END DO
    END ASSOCIATE
    
  END FUNCTION AppDiverBypass_GetBypassRecieved
  
  
    

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
  ! --- IS A NODE RATING TABLE TYPE BYPASS NODE
  ! -------------------------------------------------------------
  FUNCTION IsRatingTableTypeBypassNode(AppDiverBypass,iNode) RESULT(lRTBypassNode)
    CLASS(AppDiverBypassTYPE),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: iNode
    LOGICAL                              :: lRTBypassNode
    
    !Local variables
    INTEGER :: indxBypass
    
    !Initialize
    lRTBypassNode = .FALSE.
    
    DO indxBypass=1,AppDiverBypass%NBypass
        IF (AppDiverBypass%Bypasses(indxBypass)%iNode_Exp .EQ. iNode) THEN
            IF (AppDiverBypass%Bypasses(indxBypass)%iColBypass .EQ. 0) THEN
                lRTBypassNode = .TRUE.
                EXIT
            END IF
        END IF
    END DO
    
  END FUNCTION IsRatingTableTypeBypassNode

  
  ! -------------------------------------------------------------
  ! --- IS A NODE BYPASS NODE
  ! -------------------------------------------------------------
  FUNCTION IsBypassNode(AppDiverBypass,iNode) RESULT(lBypassNode)
    CLASS(AppDiverBypassTYPE),INTENT(IN) :: AppDiverBypass
    INTEGER,INTENT(IN)                   :: iNode
    LOGICAL                              :: lBypassNode
    
    !Local variables
    INTEGER :: indxBypass
    
    !Initialize
    lBypassNode = .FALSE.
    
    DO indxBypass=1,AppDiverBypass%NBypass
        IF (AppDiverBypass%Bypasses(indxBypass)%iNode_Exp .EQ. iNode) THEN
            lBypassNode = .TRUE.
            EXIT
        END IF
    END DO
    
  END FUNCTION IsBypassNode
  
  
  
  
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
  ! --- READ DIVERSIONS TIME SERIES DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_ReadTSData(AppDiverBypass,lDiverAdjusted,TimeStep,iStat,DiversionsOverwrite)
    CLASS(AppDiverBypassType)     :: AppDiverBypass
    LOGICAL,INTENT(IN)            :: lDiverAdjusted
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    REAL(8),OPTIONAL,INTENT(IN)   :: DiversionsOverwrite(:)

    !Local variables
    CHARACTER(LEN=ModNameLen+25) :: ThisProcedure = ModName // 'AppDiverBypass_ReadTSData'
    INTEGER                      :: indx,iMaxDiverCol,FileReadCode
    REAL(8)                      :: RecvLoss,NonRecvLoss,Factor
    
    !Initialize
    iStat = 0

    !Read data
    CALL ReadTSData(TimeStep,'diversion data',AppDiverBypass%DiverFile%RealTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN

    !Proceed according to the returned error code
    SELECT CASE (FileReadCode)
      !It wasn't time to read
      CASE (-1)
        IF (lDiverAdjusted) THEN
          AppDiverBypass%Diver%DiverRequired       = AppDiverBypass%Diver%DiverRead
          AppDiverBypass%Diver%Deli%SupplyRequired = AppDiverBypass%Diver%Deli%DeliRead
          AppDiverBypass%lDiverRequired_Updated    = .TRUE.
        END IF
    
      !Data was read with no problem
      CASE (0)
        AppDiverBypass%DiverFile%rValues = AppDiverBypass%DiverFile%rValues * AppDiverBypass%DiverFile%Fact
        IF (ANY(AppDiverBypass%DiverFile%rValues .LT. 0.0)) THEN
            MessageArray(1) = 'One or more diversions are less than zero.'
            MessageArray(2) = 'Diversions cannot be less than zero!'
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Diversions
        DO indx=1,AppDiverBypass%NDiver
          ASSOCIATE (pDiver => AppDiverBypass%Diver(indx))
            !Maximum diversions
            iMaxDiverCol = pDiver%iMaxDiverCol
            IF (iMaxDiverCol .GT. 0) pDiver%MaxDiver = AppDiverBypass%DiverFile%rValues(iMaxDiverCol) * pDiver%FracMaxDiver
            
            !Deliveries
            pDiver%Deli%SupplyRequired = AppDiverBypass%DiverFile%rValues(pDiver%Deli%iColDeli) * pDiver%Deli%FracDeli
            pDiver%Deli%DeliRead       = pDiver%Deli%SupplyRequired
          
            !Recoverable losses
            RecvLoss = AppDiverBypass%DiverFile%rValues(pDiver%iColRecvLoss) * pDiver%FracRecvLoss

            !Non-recoverable losses
            NonRecvLoss = AppDiverBypass%DiverFile%rValues(pDiver%iColNonRecvLoss) * pDiver%FracNonRecvLoss
            
            !Total diversions
            pDiver%DiverRead = pDiver%Deli%SupplyRequired + RecvLoss + NonRecvLoss
                      
            !Ratio of recoverable and non-recoverable losses to total diversions
            IF (pDiver%DiverRead .GT. 0.0) THEN
              pDiver%Ratio_RecvLoss    = RecvLoss / pDiver%DiverRead
              pDiver%Ratio_NonRecvLoss = NonRecvLoss / pDiver%DiverRead
            ELSE
              pDiver%Ratio_RecvLoss    = 0.0
              pDiver%Ratio_NonRecvLoss = 0.0
            END IF
            
            !Is diversion that is read is larger than maximum diversion? If so, update
            IF (pDiver%DiverRead .GT. pDiver%MaxDiver) THEN
              Factor                     = pDiver%MaxDiver / pDiver%DiverRead
              pDiver%Deli%SupplyRequired = pDiver%Deli%SupplyRequired * Factor
              pDiver%Deli%DeliRead       = pDiver%Deli%SupplyRequired
              pDiver%DiverRead           = pDiver%MaxDiver
            END IF
            
            !Equate required values to read values
            pDiver%DiverRequired = pDiver%DiverRead
                         
          END ASSOCIATE
        END DO
        
        !Update flag to check if DiverAdjusted is updated
        AppDiverBypass%lDiverRequired_Updated = .TRUE.
        
    END SELECT
      
    !If diversions to overwrite provided, do so
    IF (.NOT. PRESENT(DiversionsOverwrite)) RETURN
    DO indx=1,AppDiverBypass%NDiver
        !Deliveries
        AppDiverBypass%Diver(indx)%Deli%SupplyRequired = DiversionsOverwrite(indx) * AppDiverBypass%Diver(indx)%Deli%FracDeli
        AppDiverBypass%Diver(indx)%Deli%DeliRead       = AppDiverBypass%Diver(indx)%Deli%SupplyRequired
      
        !Recoverable losses
        RecvLoss = DiversionsOverwrite(indx) * AppDiverBypass%Diver(indx)%FracRecvLoss

        !Non-recoverable losses
        NonRecvLoss = DiversionsOverwrite(indx) * AppDiverBypass%Diver(indx)%FracNonRecvLoss
        
        !Total diversions
        AppDiverBypass%Diver(indx)%DiverRead = AppDiverBypass%Diver(indx)%Deli%SupplyRequired + RecvLoss + NonRecvLoss
                  
        !Ratio of recoverable and non-recoverable losses to total diversions
        IF (AppDiverBypass%Diver(indx)%DiverRead .GT. 0.0) THEN
          AppDiverBypass%Diver(indx)%Ratio_RecvLoss    = RecvLoss / AppDiverBypass%Diver(indx)%DiverRead
          AppDiverBypass%Diver(indx)%Ratio_NonRecvLoss = NonRecvLoss / AppDiverBypass%Diver(indx)%DiverRead
        ELSE
          AppDiverBypass%Diver(indx)%Ratio_RecvLoss    = 0.0
          AppDiverBypass%Diver(indx)%Ratio_NonRecvLoss = 0.0
        END IF
        
        !Is diversion that is read is larger than maximum diversion? If so, update
        IF (AppDiverBypass%Diver(indx)%DiverRead .GT. AppDiverBypass%Diver(indx)%MaxDiver) THEN
          Factor                                         = AppDiverBypass%Diver(indx)%MaxDiver / AppDiverBypass%Diver(indx)%DiverRead
          AppDiverBypass%Diver(indx)%Deli%SupplyRequired = AppDiverBypass%Diver(indx)%Deli%SupplyRequired * Factor
          AppDiverBypass%Diver(indx)%Deli%DeliRead       = AppDiverBypass%Diver(indx)%Deli%SupplyRequired
          AppDiverBypass%Diver(indx)%DiverRead           = AppDiverBypass%Diver(indx)%MaxDiver
        END IF
        
        !Equate required values to read values
        AppDiverBypass%Diver(indx)%DiverRequired = AppDiverBypass%Diver(indx)%DiverRead
                     
    END DO
        
  END SUBROUTINE AppDiverBypass_ReadTSData
  
  
  
  
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
  ! --- PRINT DIVERSION AND BYPASS REALTED RESULTS
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_PrintResults(AppDiverBypass)
    CLASS(AppDiverBypassType) :: AppDiverBypass
    
    !Local variables
    REAL(8) :: DummyArray(6,AppDiverBypass%NDiver)
    
    !Return if output file is not defined
    IF (.NOT. AppDiverBypass%DiverDetailsBudRawFile_Defined) RETURN
        
    !Prepare output data
    DummyArray(1,:) = AppDiverBypass%Diver%DiverActual
    DummyArray(2,:) = AppDiverBypass%Diver%DiverRequired - AppDiverBypass%Diver%DiverActual
    DummyArray(3,:) = AppDiverBypass%Diver%RecvLoss
    DummyArray(4,:) = AppDiverBypass%Diver%NonRecvLoss
    DummyArray(5,:) = AppDiverBypass%Diver%Deli%SupplyActual
    DummyArray(6,:) = AppDiverBypass%Diver%Deli%SupplyRequired - AppDiverBypass%Diver%Deli%SupplyActual
          
    !Write out data
    CALL AppDiverBypass%DiverDetailsBudRawFile%WriteData(DummyArray)

  END SUBROUTINE AppDiverBypass_PrintResults
  
  


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
  ! --- COMPILE DIVERSIONS AT EACH STREAM NODE
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_CompileNodalDiversions(AppDiverBypass)
    CLASS(AppDiverBypassType) :: AppDiverBypass
    
    !Local variables
    INTEGER :: indxDiver,iStrmNode
    
    !Return fi there is no need to compile stream node diversions
    IF (AppDiverBypass%lDiverRequired_Updated .EQ. .FALSE.) RETURN
    
    !Initialize
    AppDiverBypass%NodalDiverRequired = 0.0
    
    !Compile
    DO indxDiver=1,AppDiverBypass%NDiver
      iStrmNode = AppDiverBypass%Diver(indxDiver)%iStrmNode
      IF (iStrmNode .GT. 0) AppDiverBypass%NodalDiverRequired(iStrmNode) = AppDiverBypass%NodalDiverRequired(iStrmNode) + AppDiverBypass%Diver(indxDiver)%DiverRequired
    END DO
    
    !Set the flag properly
    AppDiverBypass%lDiverRequired_Updated = .FALSE.
    
  END SUBROUTINE AppDiverBypass_CompileNodalDiversions
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE ELEMENT-TO-RECOVERABLE LOSS POINTERS
  ! -------------------------------------------------------------
  SUBROUTINE ElemToRecvLossPointers(NElements,RechargeZones,ElemToRecvLoss)
    INTEGER,INTENT(IN)                       :: NElements
    TYPE(RechargeZoneType),TARGET,INTENT(IN) :: RechargeZones(:)
    TYPE(ElemToRecvLossType)                 :: ElemToRecvLoss(NElements)
    
    !Local variables
    INTEGER             :: indxZone,indx,iElem,iCount
    INTEGER,ALLOCATABLE :: iTemp(:)
    
    !Iterate over recharge zones
    DO indxZone=1,SIZE(RechargeZones)
      ASSOCIATE (pZone => RechargeZones(indxZone))
        DO indx=1,pZone%NZones
          iElem                                 = pZone%Zones(indx)
          iCount                                = ElemToRecvLoss(iElem)%nElemToRecvLoss + 1
          ElemToRecvLoss(iElem)%nElemToRecvLoss = iCount
          ALLOCATE (iTemp(iCount))
          iTemp(1:iCount-1)                     = ElemToRecvLoss(iElem)%iElemToRecvLoss
          iTemp(iCount)                         = indxZone
          CALL MOVE_ALLOC(iTemp,ElemToRecvLoss(iElem)%iElemToRecvLoss)
        END DO
      END ASSOCIATE
    END DO
    
  END SUBROUTINE ElemToRecvLossPointers
  
  
  ! -------------------------------------------------------------
  ! --- FUNCTION TO PREPARE THE BUDGET HEADER DATA FOR DIVERSION DETAILS OUTPUT
  ! -------------------------------------------------------------
  FUNCTION PrepareDiverDetailsBudgetHeader(NDiver,TimeStep,NTIME,Diversions,cVersion) RESULT(Header)
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    INTEGER,INTENT(IN)             :: NTIME,NDiver
    TYPE(DiversionType),INTENT(IN) :: Diversions(NDiver)
    CHARACTER(LEN=*),INTENT(IN)    :: cVersion
    TYPE(BudgetHeaderType)         :: Header
    
    !Local variables
    INTEGER,PARAMETER                 :: TitleLen           = 104  , &
                                         NTitles            = 3    , &
                                         NColumnHeaderLines = 4    , &  
                                         NColumns           = 6  
    INTEGER                           :: iCount,indxLocation,indxCol,indxDiver,I,      &
                                         iDest,iStrmNode,iCount1,iDestType
    CHARACTER                         :: UnitT*10,TextTime*17
    CHARACTER(LEN=15),PARAMETER       :: FParts(4) = (/'ACTUAL_DIV'      , &
                                                       'DIV_SHORT'       , &
                                                       'RECVRBL_LOSS'    , &
                                                       'NON_RCVRBL_LOSS' /)
    CHARACTER,DIMENSION(NDiver)       :: Text_ASCII*20,Text1_ASCII*14 , &
                                         Text_DSS*20,Text1_DSS*21
    TYPE(TimeStepType)                :: TimeStepLocal

    !Initialize
    DO indxDiver=1,NDiver
      iDestType =  Diversions(indxDiver)%Deli%Destination%iDestType
      
      SELECT CASE (iDestType)
          !Delivery to outside model area
          CASE (FlowDest_Outside)      
              Text_ASCII(indxDiver)  = 'Outside Model Area'
              Text1_ASCII(indxDiver) = 'Subreg. 0'  ;  Text1_ASCII(indxDiver) = ADJUSTR(Text1_ASCII(indxDiver))
              Text_DSS(indxDiver)    = 'ACT_DELI_SUBRG_0'
              Text1_DSS(indxDiver)   = 'DELI_SHORT_SUBRG_0'
      
          !Delivery to a single element
          CASE (FlowDest_Element) 
              iDest                  = Diversions(indxDiver)%Deli%Destination%iDest
              Text_ASCII(indxDiver)  = 'Element '//TRIM(IntToText(iDest))
              Text1_ASCII(indxDiver) = 'Elem. '//TRIM(IntToText(iDest))  ;  Text1_ASCII(indxDiver) = ADJUSTR(Text1_ASCII(indxDiver))
              Text_DSS(indxDiver)    = 'ACT_DELI_ELEM_'//TRIM(IntToText(iDest))
              Text1_DSS(indxDiver)   = 'DELI_SHORT_ELEM_'//TRIM(IntToText(iDest))
        
          !Delivery to a group of elements
          CASE (FlowDest_ElementSet)
              iDest                  = Diversions(indxDiver)%Deli%Destination%iDest
              Text_ASCII(indxDiver)  = 'Element Group '//TRIM(IntToText(iDest))
              Text1_ASCII(indxDiver) = 'Elem. Grp. '//TRIM(IntToText(iDest))  ;  Text1_ASCII(indxDiver) = ADJUSTR(Text1_ASCII(indxDiver))
              Text_DSS(indxDiver)    = 'ACT_DELI_ELEMGRP_'//TRIM(IntToText(iDest))
              Text1_DSS(indxDiver)   = 'DELI_SHORT_ELEMGRP_'//TRIM(IntToText(iDest))

          !Delivery to a subregion
          CASE (FlowDest_Subregion)
              iDest                  = Diversions(indxDiver)%Deli%Destination%iDest
              Text_ASCII(indxDiver)  = 'Subregion '//TRIM(IntToText(iDest))
              Text1_ASCII(indxDiver) = 'Subreg. '//TRIM(IntToText(iDest))  ;  Text1_ASCII(indxDiver) = ADJUSTR(Text1_ASCII(indxDiver))
              Text_DSS(indxDiver)    = 'ACT_DELI_SUBRG_'//TRIM(IntToText(iDest))
              Text1_DSS(indxDiver)   = 'DELI_SHORT_SUBRG_'//TRIM(IntToText(iDest))
      END SELECT

    END DO
 
    !Increment the initial simulation time to represent the data begin date for budget binary output files  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    TextTime = ArrangeText(TRIM(UnitT),17)
  
    !Budget descriptor
    Header%cBudgetDescriptor = 'diversion details'

    !Simulation time related data
    Header%NTimeSteps = NTIME
    Header%TimeStep   = TimeStepLocal

    !Areas
    Header%NAreas = 0
    ALLOCATE (Header%Areas(0))
                                                     
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => Header%ASCIIOutput)
      pASCIIOutput%TitleLen           = TitleLen
      pASCIIOutput%NTitles            = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
      pASCIIOutput%cTitles(1)         = ArrangeText('IWFM STREAM PACKAGE (v'//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(2)         = ArrangeText('DIVERSION AND DELIVERY DETAILS IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(3)         = REPEAT('-',pASCIIOutput%TitleLen)
      pASCIIOutput%lTitlePersist(1:2) = .TRUE.
      pASCIIOutput%lTitlePersist(3)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,2000(F13.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
   
    !Location names
    Header%NLocations = NDiver
    ALLOCATE (Header%cLocationNames(NDiver))
    DO indxLocation=1,NDiver
      Header%cLocationNames(indxLocation) = 'DIVERSION_'//TRIM(IntToText(indxLocation))//'(SN'//TRIM(IntToText(Diversions(indxLocation)%iStrmNode))//')' 
    END DO
    
    !Locations
    ALLOCATE (Header%Locations(NDiver))
    iCount = 0
    DO indxLocation=1,NDiver
      ASSOCIATE (pLocation => Header%Locations(indxLocation))
        pLocation%NDataColumns = NColumns
        ALLOCATE (pLocation%cFullColumnHeaders(NColumns+1)                  , &
                  pLocation%iDataColumnTypes(NColumns)                      , &
                  pLocation%iColWidth(NColumns+1)                           , &
                  pLocation%cColumnHeaders(NColumns+1,NColumnHeaderLines)   , &
                  pLocation%cColumnHeadersFormatSpec(NColumnHeaderLines)    )
        pLocation%cFullColumnHeaders(1:5) = (/'Time'                 , &
                                              'Actual Diversion'     , &
                                              'Diversion Shortage'   , &
                                              'Recoverable Loss'     , &
                                              'Non-recoverable Loss' /)
        ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                   pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
          pColumnHeaders(1:5,1) = (/'                 ','              ','              ','              ','       Non    '/)
          pColumnHeaders(1:5,2) = (/'      Time       ','       Actual ','     Diversion','   Recoverable','   Recoverable'/)
          pColumnHeaders(1:5,3) = (/      TextTime     ,'     Diversion','      Shortage','       Loss   ','       Loss   '/)
          pColumnHeaders(:,4)   = ''
          pFormatSpecs(1)       = '(A17,2000A14)'
          pFormatSpecs(2)       = '(A17,2000A14)'
          pFormatSpecs(3)       = '(A17,2000A14)'
          pFormatSpecs(4)       = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NColumns+1))//'A0)'
        END ASSOCIATE
        iCount  = iCount + 1
        indxCol = 6
        pLocation%cFullColumnHeaders(indxCol)   = 'Actual Delivery to '//TRIM(Text_ASCII(iCount))
        pLocation%cFullColumnHeaders(indxCol+1) = 'Delivery Shortage for '//TRIM(Text_ASCII(iCount))
        ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders)
          pColumnHeaders(indxCol:indxCol+1,1) = (/'      Actual  ','   Delivery   '/)
          pColumnHeaders(indxCol:indxCol+1,2) = (/'   Delivery to','  Shortage for'/)
          pColumnHeaders(indxCol:indxCol+1,3) =   Text1_ASCII(iCount)
        END ASSOCIATE
        pLocation%iDataColumnTypes = VR
        pLocation%iColWidth        = (/17,(14,I=1,NColumns)/)
      END ASSOCIATE
    END DO
    
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => Header%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(6*NDiver) , pDSSOutput%iDataTypes(1))
      iCount  = 0
      iCount1 = 0
      DO indxLocation=1,NDiver
        iStrmNode   = Diversions(indxLocation)%iStrmNode
        DO indxCol=1,4
          iCount                        = iCount+1
          pDSSOutput%cPathNames(iCount) = '/IWFM_DIVER_DETAIL/'                                                                      //  &  !A part
                                          'DIVER'//TRIM(IntToText(indxLocation))//'_SN'//TRIM(IntToText(iStrmNode))//'/'             //  &  !B part
                                          'VOLUME/'                                                                                  //  &  !C part
                                          '/'                                                                                        //  &  !D part
                                          TRIM(TimeStep%Unit)//'/'                                                                   //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                                                        !F part
        END DO
        iCount                        = iCount+1
        iCount1                       = iCount1 + 1
        pDSSOutput%cPathNames(iCount) = '/IWFM_DIVER_DETAIL/'                                                                      //  &  !A part
                                        'DIVER'//TRIM(IntToText(indxLocation))//'_SN'//TRIM(IntToText(iStrmNode))//'/'             //  &  !B part
                                        'VOLUME/'                                                                                  //  &  !C part
                                        '/'                                                                                        //  &  !D part
                                        TRIM(TimeStep%Unit)//'/'                                                                   //  &  !E part
                                        TRIM(Text_DSS(iCount1))//'/'                                                                      !F part
        iCount                        = iCount+1
        pDSSOutput%cPathNames(iCount) = '/IWFM_DIVER_DETAIL/'                                                                      //  &  !A part
                                        'DIVER'//TRIM(IntToText(indxLocation))//'_SN'//TRIM(IntToText(iStrmNode))//'/'             //  &  !B part
                                        'VOLUME/'                                                                                  //  &  !C part
                                        '/'                                                                                        //  &  !D part
                                        TRIM(TimeStep%Unit)//'/'                                                                   //  &  !E part
                                        TRIM(Text1_DSS(iCount1))//'/'                                                                     !F part
      END DO
      pDSSOutput%iDataTypes = PER_CUM
    END ASSOCIATE

  END FUNCTION PrepareDiverDetailsBudgetHeader


  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF BYPASS DATA
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_ConvertTimeUnit(AppDiverBypass,NewUnit)
    CLASS(AppDiverBypassType)   :: AppDiverBypass
    CHARACTER(LEN=*),INTENT(IN) :: NewUnit
    
    !Local variables
    INTEGER :: indxBypass
    REAL(8) :: FactorStrmFlow,FactorBypass
    
    !Make sure NewUnit is defined
    IF (NewUnit .EQ. '') RETURN
    
    !If no bypasses, return
    IF (AppDiverBypass%NBypass .EQ. 0) RETURN
    
    !Compute conversion factor for bypass stream flow time unit
    FactorStrmFlow                  = TimeIntervalConversion(NewUnit,AppDiverBypass%TimeUnitStrmFlow)
    AppDiverBypass%TimeUnitStrmFlow = NewUnit
    
    !Compute conversion factor for bypass flow time unit
    FactorBypass                  = TimeIntervalConversion(NewUnit,AppDiverBypass%TimeUnitBypass)
    AppDiverBypass%TimeUnitBypass = NewUnit
    
    !Convert bypass rating table
    DO indxBypass=1,AppDiverBypass%NBypass
      ASSOCIATE (pBypass => AppDiverBypass%Bypasses(indxBypass))
        IF (pBypass%iColBypass .GT. 0) CYCLE
        pBypass%RatingTable%XPoint = pBypass%RatingTable%XPoint * FactorStrmFlow
        pBypass%RatingTable%YPoint = pBypass%RatingTable%YPoint * FactorBypass
      END ASSOCIATE
    END DO
    
  END SUBROUTINE AppDiverBypass_ConvertTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE BYPASS FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_ComputeBypass(AppDiverBypass,iNode,rInflow,rOutflow,StrmLakeConnector,lDiverMet,CoeffBypass)
    CLASS(AppDiverBypassType)   :: AppDiverBypass
    INTEGER,INTENT(IN)          :: iNode
    REAL(8),INTENT(IN)          :: rInflow
    REAL(8),INTENT(INOUT)       :: rOutflow
    TYPE(StrmLakeConnectorType) :: StrmLakeConnector
    LOGICAL,INTENT(IN)          :: lDiverMet
    REAL(8),INTENT(OUT)         :: CoeffBypass
    
    !Local variables
    INTEGER :: indxBypass
    REAL(8) :: Bypass_Required,dBypass_dFlow,rAvailableFlow
    
    !Initialize
    CoeffBypass = 0.0
    
    !Locate the bypass from iNode
    indxBypass = LocateInList(iNode,AppDiverBypass%Bypasses%iNode_Exp)
    
    !Return if no bypasses are exported from iNode
    IF (indxBypass .EQ. 0) RETURN
    
    !Available flow for bypass
    rAvailableFlow = rInflow - rOutflow
    
    ASSOCIATE (pBypass => AppDiverBypass%Bypasses(indxBypass))
      !Specified bypass rate
      IF (pBypass%iColBypass .GT. 0) THEN
        Bypass_Required    = AppDiverBypass%DiverFile%rValues(pBypass%iColBypass)
        pBypass%Bypass_Out = MIN(rAvailableFlow , Bypass_Required)
        IF (pBypass%Bypass_Out .LT. Bypass_Required) THEN
          IF (lDiverMet) CoeffBypass = 1.0
        END IF
        
      !Rating-table type bypass rate
      ELSE
        CALL pBypass%RatingTable%EvaluateAndDerivative(rAvailableFlow,pBypass%Bypass_Out,dBypass_dFlow)
        pBypass%Bypass_Out = MIN(MAX(pBypass%Bypass_Out,0.0) , rAvailableFlow)
        IF (lDiverMet) CoeffBypass = dBypass_dFlow
      END IF
      
      !Received bypass, recoverable and non-recoverable flows
      pBypass%RecvLoss        = pBypass%Bypass_Out * pBypass%FracRecvLoss
      pBypass%NonRecvLoss     = pBypass%Bypass_Out * pBypass%FracNonRecvLoss
      pBypass%Bypass_Recieved = pBypass%Bypass_Out - pBypass%RecvLoss - pBypass%NonRecvLoss
      
      !If the bypass flow goes to a lake, store that data
      IF (pBypass%iDestType .EQ. FlowDest_Lake) CALL StrmLakeConnector%SetFlow(iBypassToLakeType,indxBypass,pBypass%iDest,pBypass%Bypass_Recieved)
      
      !Update the flow in the stream
      rOutflow = rOutflow + pBypass%Bypass_Out
      
    END ASSOCIATE
      
  END SUBROUTINE AppDiverBypass_ComputeBypass
  
 
  ! -------------------------------------------------------------
  ! --- SIMULATE ALL DIVERSION RELATED FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE AppDiverBypass_ComputeDiversions(AppDiverBypass,NStrmNodes)
    CLASS(AppDiverBypassType) :: AppDiverBypass
    INTEGER,INTENT(IN)        :: NStrmNodes
    
    !Local variables
    INTEGER :: indxDiver,indxNode,iNode
    REAL(8) :: Ratio(NStrmNodes)
    
    !Return if no diversions are specified
    IF (AppDiverBypass%NDiver .EQ. 0) RETURN
    
    !Compute scaling ratios
    DO indxNode=1,NStrmNodes
      IF (AppDiverBypass%NodalDiverRequired(indxNode) .GT. 0.0) THEN
        Ratio(indxNode) = AppDiverBypass%NodalDiverActual(indxNode) / AppDiverBypass%NodalDiverRequired(indxNode)
      ELSE
        Ratio(indxNode) = 0.0
      END IF
    END DO
     
    !Update actual diversions, deliveries,recoverable and non-recoverbale losses
    DO indxDiver=1,AppDiverBypass%NDiver
      ASSOCIATE (pDiver => AppDiverBypass%Diver(indxDiver))
        iNode = pDiver%iStrmNode
        IF (iNode .EQ. 0) THEN
          pDiver%DiverActual = pDiver%DiverRequired
        ELSE
          pDiver%DiverActual = pDiver%DiverRequired * Ratio(iNode)
        END IF
        pDiver%RecvLoss    = pDiver%DiverActual * pDiver%Ratio_RecvLoss
        pDiver%NonRecvLoss = pDiver%DiverActual * pDiver%Ratio_NonRecvLoss
        
        !Deliveries
        IF (iNode .EQ. 0) THEN
          pDiver%Deli%SupplyActual = pDiver%Deli%SupplyRequired
        ELSE
          pDiver%Deli%SupplyActual = pDiver%Deli%SupplyRequired * Ratio(iNode)
        END IF
        
      END ASSOCIATE
    END DO
    
  END SUBROUTINE AppDiverBypass_ComputeDiversions


END MODULE