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
MODULE Class_StrmInflow
  USE GeneralUtilities     , ONLY: IntToText               , &
                                   ConvertID_To_Index      , &
                                   LocateInList            , &
                                   StripTextUntilCharacter , &
                                   CleanSpecialCharacters
  USE TimeSeriesUtilities  , ONLY: TimeStepType
  USE MessageLogger        , ONLY: SetLastMessage          , &
                                   MessageArray            , &
                                   f_iFatal                  
  USE Package_Misc         , ONLY: RealTSDataInFileType    , &
                                   ReadTSData
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
  PUBLIC :: StrmInflowType         
  
  
  ! -------------------------------------------------------------
  ! --- STREAM INFLOW DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: StrmInflowType
    LOGICAL             :: lDefined            = .FALSE.   !Flag to check if stream inflows are specified
    REAL(8)             :: Fact                = 1.0       !Conversion factor for the stream inflows that are read from file
    INTEGER,ALLOCATABLE :: IDs(:)                          !Inflow ID numbers 
    INTEGER,ALLOCATABLE :: InflowNodes(:)                  !Nodes that receive inflows
    REAL(8),ALLOCATABLE :: Inflows(:)                      !Specified inflows at "all" stream nodes
    REAL(8),ALLOCATABLE :: ReadInflows(:)                  !Read inflows to be kept in case overwiting of some inflows happen so that we can still retrieve read values 
  CONTAINS
    PROCEDURE,PASS :: New
    PROCEDURE,PASS :: Kill 
    PROCEDURE,PASS :: GetInflow_AtANode
    PROCEDURE,PASS :: GetInflows_AtAllNodes
    PROCEDURE,PASS :: GetInflows_AtAllInflows
    PROCEDURE,PASS :: GetNInflows
    PROCEDURE,PASS :: GetInflowNodes
    PROCEDURE,PASS :: GetInflowIDs
    PROCEDURE,PASS :: SetInflow
    PROCEDURE,PASS :: ReadTSData    => StrmInflow_ReadTSData
    
  END TYPE StrmInflowType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 18
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmInflow::'

  
  
  
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
  ! --- INITIALIZE STREAM INFLOW TIME SERIES DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(StrmInflow,FileName,cWorkingDirectory,TimeStep,NStrmNodes,iStrmNodeIDs,iStat)
    CLASS(StrmInflowType),INTENT(OUT) :: StrmInflow
    CHARACTER(LEN=*),INTENT(IN)       :: FileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NStrmNodes,iStrmNodeIDs(NStrmNodes)
    INTEGER,INTENT(OUT)               :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+3)     :: ThisProcedure = ModName // 'New'
    INTEGER                         :: indx,ErrorCode,iStrmNodeID
    REAL(8)                         :: Factor(1)
    LOGICAL                         :: lReadID_And_Node,DummyArray(1) = [.TRUE.]
    CHARACTER(LEN=1000),ALLOCATABLE :: cInflowNodes(:,:)
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (FileName .EQ. '') RETURN
    
    !Instantiate
    CALL StrmInflow%Init(FileName,cWorkingDirectory,'Stream inflow data file',TimeStep%TrackTime,2,.TRUE.,Factor,DummyArray,cInflowNodes,iStat)
    IF (iStat .EQ. -1) RETURN
    StrmInflow%Fact = Factor(1)
    
    !Allocate variables
    ALLOCATE (StrmInflow%IDs(StrmInflow%iSize)         , &
              StrmInflow%InflowNodes(StrmInflow%iSize) , &
              StrmInflow%ReadInflows(StrmInflow%iSize) , &
              StrmInflow%Inflows(NStrmNodes)           , &
              STAT=ErrorCode                           )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for time series stream inflows!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Return if no stream nodes are receiving inflow
    IF (StrmInflow%iSize .EQ. 0) RETURN
    
    !Backward compatibility: Check if only stream nodes are listed, or inflow IDs and stream nodes
    CALL CleanSpecialCharacters(cInflowNodes(1,1))
    cInflowNodes(1,1) = ADJUSTL(StripTextUntilCharacter(cInflowNodes(1,1),"/"))
    READ (cInflowNodes(1,1),*,IOSTAT=ErrorCode) StrmInflow%IDs(1),iStrmNodeID
    IF (ErrorCode .EQ. 0) THEN
        lReadID_And_Node = .TRUE.
    ELSE
        lReadID_And_Node = .FALSE.
    END IF
    
    !Read inflow IDs, if applicable, and inflow receiving stream nodes
    DO indx=1,StrmInflow%iSize
        IF (lReadID_And_Node) THEN
            READ (cInflowNodes(indx,1),*) StrmInflow%IDs(indx),iStrmNodeID
        ELSE
            READ (cInflowNodes(indx,1),*) iStrmNodeID
            StrmInflow%IDs(indx) = indx
        END IF
        IF (iStrmNodeID .EQ. 0) THEN
            StrmInflow%InflowNodes(indx) = 0
            CYCLE
        END IF
        CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,StrmInflow%InflowNodes(indx))
        IF (StrmInflow%InflowNodes(indx) .EQ. 0) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' listed as receiving stream inflow is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO
    
    !Initialize stream inflows
    StrmInflow%Inflows = 0.0

    !Set the flag
    StrmInflow%lDefined = .TRUE.

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
  ! --- KILL STREAM INFLOW DATA FILE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(StrmInflow)
    CLASS(StrmInflowType) :: StrmInflow
    
    !Local variables
    INTEGER              :: ErrorCode
    TYPE(StrmInflowType) :: Dummy
    
    !Deallocate array attributes
    DEALLOCATE (StrmInflow%InflowNodes , &
                StrmInflow%Inflows     , &
                StrmInflow%ReadInflows , &
                StrmInflow%IDs         , &
                STAT=ErrorCode         )
    
    !Close data file
    CALL StrmInflow%Close()
    
    !Set attributes to their defaults
    SELECT TYPE (StrmInflow)
        TYPE IS (StrmInflowType)
            StrmInflow = Dummy
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
  ! --- GET STREAM INFLOWS AT ALL INFLOWS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetInflows_AtAllInflows(StrmInflow,rInflows)
    CLASS(StrmInflowType),INTENT(IN) :: StrmInflow
    REAL(8),INTENT(OUT)              :: rInflows(:)
    
    IF (StrmInflow%lDefined) THEN
        rInflows = StrmInflow%rValues
    ELSE
        rInflows = 0.0
    END IF
    
  END SUBROUTINE GetInflows_AtAllInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW AT ALL STREAM NODES
  ! -------------------------------------------------------------
  PURE FUNCTION GetInflows_AtAllNodes(StrmInflow,NStrmNodes) RESULT(Inflows)
    CLASS(StrmInflowType),INTENT(IN) :: StrmInflow
    INTEGER,INTENT(IN)               :: NStrmNodes
    REAL(8)                          :: Inflows(NStrmNodes)
    
    !Return if no inflows are specified
    IF (StrmInflow%lDefined) THEN
        Inflows = StrmInflow%Inflows
    ELSE
        Inflows = 0.0
    END IF
    
  END FUNCTION GetInflows_AtAllNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  PURE FUNCTION GetInflow_AtANode(StrmInflow,iStrmNode) RESULT(rInflow)
    CLASS(StrmInflowType),INTENT(IN) :: StrmInflow
    INTEGER,INTENT(IN)               :: iStrmNode
    REAL(8)                          :: rInflow
    
    IF (StrmInflow%lDefined) THEN
        rInflow = StrmInflow%Inflows(iStrmNode)
    ELSE
        rInflow = 0.0
    END IF
    
  END FUNCTION GetInflow_AtANode
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF INFLOWS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNInflows(StrmInflow) RESULT(iNInflows)
    CLASS(StrmInflowType),INTENT(IN) :: StrmInflow
    INTEGER                          :: iNInflows
    
    IF (StrmInflow%lDefined) THEN
        iNInflows = StrmInflow%iSize
    ELSE
        iNInflows = 0
    END IF
    
  END FUNCTION GetNInflows
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOW NODES
  ! -------------------------------------------------------------
  SUBROUTINE GetInflowNodes(StrmInflow,iNodes)
    CLASS(StrmInflowType),INTENT(IN) :: StrmInflow
    INTEGER,ALLOCATABLE              :: iNodes(:)
    
    IF (StrmInflow%lDefined) THEN
        ALLOCATE (iNodes(StrmInflow%iSize))
        iNodes = StrmInflow%InflowNodes
    ELSE
        ALLOCATE (iNodes(0))
    END IF
    
  END SUBROUTINE GetInflowNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET INFLOW IDs
  ! -------------------------------------------------------------
  SUBROUTINE GetInflowIDs(StrmInflow,IDs)
    CLASS(StrmInflowType),INTENT(IN) :: StrmInflow
    INTEGER,ALLOCATABLE              :: IDs(:)
    
    IF (StrmInflow%lDefined) THEN
        ALLOCATE (IDs(StrmInflow%iSize))
        IDs = StrmInflow%IDs
    ELSE
        ALLOCATE (IDs(0))
    END IF
    
  END SUBROUTINE GetInflowIDs
  
  


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
  ! --- SET STREAM INFLOW AT A NODE
  ! -------------------------------------------------------------
  SUBROUTINE SetInflow(StrmInflow,iStrmNode,rFlow,lAdd)
    CLASS(StrmInflowType) :: StrmInflow
    INTEGER,INTENT(IN)    :: iStrmNode
    REAL(8),INTENT(IN)    :: rFlow
    LOGICAL,INTENT(IN)    :: lAdd
    
    IF (lAdd) THEN
        StrmInflow%Inflows(iStrmNode) = StrmInflow%Inflows(iStrmNode) + rFlow
    ELSE
        StrmInflow%Inflows(iStrmNode) = rFlow
    END IF
    
  END SUBROUTINE SetInflow
  
  
  
  
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
  ! --- READ STREAM INFLOW DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmInflow_ReadTSData(StrmInflow,TimeStep,iStrmInflows,rStrmInflows,iStat)
    CLASS(StrmInflowType)         :: StrmInflow
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: iStrmInflows(:)  !Inflows which will be overwritten
    REAL(8),INTENT(IN)            :: rStrmInflows(:)  !Overwriting values
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'StrmInflow_ReadTSData'
    INTEGER                      :: indx,iNode,FileReadCode,ID
    REAL(8)                      :: rValue
    
    !Initialize
    iStat = 0

    !If no file is defined, return
    IF (StrmInflow%lDefined .EQ. .FALSE.) RETURN

    !Read data
    CALL ReadTSData(TimeStep,'Stream inflow data',StrmInflow%RealTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    SELECT CASE(FileReadCode)
        !It wasn't time to read; restore values to previously read values (in case some of them were overwritten) 
        CASE (-1)
            StrmInflow%rValues = StrmInflow%ReadInflows
            IF (SIZE(iStrmInflows) .GT. 0) THEN
                StrmInflow%rValues(iStrmInflows) = rStrmInflows
            END IF
            StrmInflow%Inflows = 0.0
            DO indx=1,StrmInflow%iSize
                iNode = StrmInflow%InflowNodes(indx)
                IF (iNode .EQ. 0) CYCLE
                StrmInflow%Inflows(iNode) = StrmInflow%Inflows(iNode) + StrmInflow%rValues(indx)
            END DO
            
        !It was time to read; process read values
        CASE (0)
            StrmInflow%rValues     = StrmInflow%rValues * StrmInflow%Fact
            StrmInflow%ReadInflows = StrmInflow%rValues
            !If overwritting inflows are provided, apply them
            IF (SIZE(iStrmInflows) .GT. 0) THEN
                StrmInflow%rValues(iStrmInflows) = rStrmInflows
            END IF
            !Process inflows for each stream node
            StrmInflow%Inflows = 0.0
            DO indx=1,StrmInflow%iSize
                iNode = StrmInflow%InflowNodes(indx)
                IF (iNode .EQ. 0) CYCLE
                rValue = StrmInflow%rValues(indx) 
                !Make sure that inflow is not less than zero
                IF (rValue .LT. 0.0) THEN
                    ID = StrmInflow%IDs(indx)
                    MessageArray(1) = 'Stream inflows cannot be less than zero.'
                    MessageArray(2) = 'Inflow specified at inflow ID '//TRIM(IntToText(ID))//' is less than zero!'
                    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                StrmInflow%Inflows(iNode) = StrmInflow%Inflows(iNode) + rValue
            END DO
    END SELECT
        
  END SUBROUTINE StrmInflow_ReadTSData


END MODULE