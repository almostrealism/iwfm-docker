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
MODULE Class_StrmInflow
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE MessageLogger        , ONLY: SetLastMessage  , &
                                   MessageArray    , &
                                   iFatal
  USE Package_Misc
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
    INTEGER,ALLOCATABLE :: InflowNodes(:)                  !Nodes that receive inflows
    REAL(8),ALLOCATABLE :: Inflows(:)                      !Specified inflows at "all" stream nodes
  CONTAINS
    PROCEDURE,PASS      :: New           => StrmInflow_New
    PROCEDURE,PASS      :: Kill          => StrmInflow_Kill
    PROCEDURE,PASS      :: GetInflows    => StrmInflow_GetInflows
    PROCEDURE,PASS      :: ReadTSData    => StrmInflow_ReadTSData
    
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
  SUBROUTINE StrmInflow_New(StrmInflow,FileName,TimeStep,NStrmNodes,iStat)
    CLASS(StrmInflowType),INTENT(OUT) :: StrmInflow
    CHARACTER(LEN=*),INTENT(IN)       :: FileName
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    INTEGER,INTENT(IN)                :: NStrmNodes
    INTEGER,INTENT(OUT)               :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+14)    :: ThisProcedure = ModName // 'StrmInflow_New'
    INTEGER                         :: indx,ErrorCode
    REAL(8)                         :: Factor(1)
    LOGICAL                         :: DummyArray(1) = (/.TRUE./)
    CHARACTER(LEN=1000),ALLOCATABLE :: cInflowNodes(:,:)
    
    !Initialize
    iStat = 0
    
    !Return if no file name is specified
    IF (FileName .EQ. '') RETURN
    
    !Instantiate
    CALL StrmInflow%Init(FileName,'Stream inflow data file',TimeStep%TrackTime,2,.TRUE.,Factor,DummyArray,cInflowNodes,iStat)
    IF (iStat .EQ. -1) RETURN
    StrmInflow%Fact = Factor(1)
    
    !Inflow nodes and inflow amounts at all nodes
    ALLOCATE (StrmInflow%InflowNodes(StrmInflow%iSize) , StrmInflow%Inflows(NStrmNodes) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for time series stream inflows!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    StrmInflow%Inflows = 0.0
    DO indx=1,StrmInflow%iSize
      READ (cInflowNodes(indx,1),*) StrmInflow%InflowNodes(indx)
    END DO
    
    !Set the flag
    StrmInflow%lDefined = .TRUE.

  END SUBROUTINE StrmInflow_New
  
  
  
  
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
  SUBROUTINE StrmInflow_Kill(StrmInflow)
    CLASS(StrmInflowType) :: StrmInflow
    
    !Local variables
    INTEGER              :: ErrorCode
    
    !Deallocate array attributes
    DEALLOCATE (StrmInflow%InflowNodes , &
                StrmInflow%Inflows     , &
                STAT=ErrorCode         )
    
    !Close data file
    CALL StrmInflow%Close()
    
    !Set attributes to their defaults
    StrmInflow%lDefined = .FALSE.
    StrmInflow%Fact     = 1.0
    
  END SUBROUTINE StrmInflow_Kill
  
  
  
  
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
  ! --- GET STREAM INFLOW AT ALL NODES
  ! -------------------------------------------------------------
  FUNCTION StrmInflow_GetInflows(StrmInflow,NStrmNodes) RESULT(Inflows)
    CLASS(StrmInflowType),INTENT(IN) :: StrmInflow
    INTEGER,INTENT(IN)               :: NStrmNodes
    REAL(8)                          :: Inflows(NStrmNodes)
    
    !Initialize
    Inflows = 0.0
    
    !Return if no inflows are specified
    IF (StrmInflow%lDefined .EQ. .FALSE.) RETURN
    
    !Otherwise, gather inflows into each stream node
    Inflows = StrmInflow%Inflows
    
  END FUNCTION StrmInflow_GetInflows
  
  


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
  SUBROUTINE StrmInflow_ReadTSData(StrmInflow,TimeStep,iStat)
    CLASS(StrmInflowType)         :: StrmInflow
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'StrmInflow_ReadTSData'
    INTEGER                      :: indx,iNode,FileReadCode
    REAL(8)                      :: Fact,rValue
    
    !Initialize
    iStat = 0

    !If no file is defined, return
    IF (StrmInflow%lDefined .EQ. .FALSE.) RETURN

    !Read data
    CALL ReadTSData(TimeStep,'Stream inflow data',StrmInflow%RealTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN

    !If error code returned was zero (data read successfully), scale stream inflow
    IF (FileReadCode .EQ. 0) THEN
        StrmInflow%Inflows = 0.0
        Fact               = StrmInflow%Fact
        DO indx=1,StrmInflow%iSize
            iNode  = StrmInflow%InflowNodes(indx)
            rValue = StrmInflow%rValues(indx) * Fact
            !Make sure that inflow is not less than zero
            IF (rValue .LT. 0.0) THEN
                 MessageArray(1) = 'Stream inflows cannot be less than zero.'
                 MessageArray(2) = 'Inflow specified at stream node '//TRIM(IntToText(iNode))//' is less than zero!'
                 CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                 iStat = -1
                 RETURN
            END IF
            StrmInflow%Inflows(iNode) = StrmInflow%Inflows(iNode) + rValue
        END DO
    END IF
    
  END SUBROUTINE StrmInflow_ReadTSData


END MODULE