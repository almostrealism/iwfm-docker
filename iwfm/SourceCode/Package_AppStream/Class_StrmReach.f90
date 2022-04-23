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
MODULE Class_StrmReach
  USE Package_Misc
  USE GeneralUtilities
  USE IOInterface
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
  PUBLIC :: StrmReachType                   , &
            StrmReach_New                   , &
            StrmReach_GetReachNumber        , &
            StrmReach_WritePreprocessedData , &
            StrmReach_CompileUpstrmReaches  , &
            StrmReach_IsUpstreamNode
  
  
  ! -------------------------------------------------------------
  ! --- REACH DATA TYPE
  ! -------------------------------------------------------------
  TYPE StrmReachType
    CHARACTER(LEN=20)   :: cName           = ''
    INTEGER             :: UpstrmNode      = 0
    INTEGER             :: DownstrmNode    = 0
    INTEGER             :: OutflowDestType = FlowDest_StrmNode
    INTEGER             :: OutflowDest     = 0
    INTEGER             :: NUpstrmReaches  = 0
    INTEGER,ALLOCATABLE :: UpstrmReaches(:)
  END TYPE StrmReachType
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  INTERFACE StrmReach_New
    MODULE PROCEDURE StrmReach_ReadPreprocessedData
  END INTERFACE StrmReach_New
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 17
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmReach::'
  
  
  
CONTAINS



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
  ! --- READ PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmReach_ReadPreprocessedData(NReach,InFile,Reaches,iStat)
    INTEGER,INTENT(IN)    :: NReach
    TYPE(GenericFileType) :: InFile
    TYPE(StrmReachType)   :: Reaches(NReach)
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    INTEGER :: indxReach,NUpstrmReaches
    
    DO indxReach=1,NReach
      CALL InFile%ReadData(Reaches(indxReach)%cName,iStat)            ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(Reaches(indxReach)%UpstrmNode,iStat)       ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(Reaches(indxReach)%DownstrmNode,iStat)     ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(Reaches(indxReach)%OutflowDestType,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(Reaches(indxReach)%OutflowDest,iStat)      ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(NUpstrmReaches,iStat)                      ;  IF (iStat .EQ. -1) RETURN
      Reaches(indxReach)%NUpstrmReaches = NUpstrmReaches
      ALLOCATE (Reaches(indxReach)%UpstrmReaches(NUpstrmReaches))
      CALL InFile%ReadData(Reaches(indxReach)%UpstrmReaches,iStat)    ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE StrmReach_ReadPreprocessedData



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
  ! --- WRITE PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmReach_WritePreprocessedData(Reaches,OutFile)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    TYPE(GenericFileType)          :: OutFile
    
    !Local variables
    INTEGER :: indxReach
    
    DO indxReach=1,SIZE(Reaches)
      CALL OutFile%WriteData(Reaches(indxReach)%cName)
      CALL OutFile%WriteData(Reaches(indxReach)%UpstrmNode)
      CALL OutFile%WriteData(Reaches(indxReach)%DownstrmNode)
      CALL OutFile%WriteData(Reaches(indxReach)%OutflowDestType)
      CALL OutFile%WriteData(Reaches(indxReach)%OutflowDest)
      CALL OutFile%WriteData(Reaches(indxReach)%NUpstrmReaches)
      CALL OutFile%WriteData(Reaches(indxReach)%UpstrmReaches)
    END DO
    
  END SUBROUTINE StrmReach_WritePreprocessedData
  
  
  

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
  ! --- FIND THE REACH NUMBER THAT A STREAM NODE BELONGS TO
  ! -------------------------------------------------------------
  FUNCTION StrmReach_GetReachNumber(StrmNode,Reaches) RESULT(iReach)
    INTEGER,INTENT(IN)             :: StrmNode
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    INTEGER                        :: iReach
    
    !Local variables
    INTEGER :: indxReach
    
    !Initialize
    iReach = 0
    
    !Find
    DO indxReach=1,SIZE(Reaches)
      IF (StrmNode.GE.Reaches(indxReach)%UpstrmNode  .AND. &
          StrmNode.LE.Reaches(indxReach)%DownstrmNode      ) THEN
        iReach = indxReach
        RETURN
      END IF
    END DO
    
  END FUNCTION StrmReach_GetReachNumber 
  
  
  
  
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
  ! --- COMPILE UPSTREAM REACHES FOR EACH REACH
  ! -------------------------------------------------------------
  SUBROUTINE StrmReach_CompileUpstrmReaches(Reaches)
    TYPE(StrmReachType) :: Reaches(:)
    
    !Local variables
    INTEGER :: NReach,indxReach,indxReach1,iCount,iUpstrmNode,iDownstrmNode,  &
               UpstrmReachesTemp(SIZE(Reaches)-1)
    
    !Initailzie
    NReach = SIZE(Reaches)
    
    !First reach never has upstream reaches
    Reaches(1)%NUpstrmReaches = 0
    ALLOCATE (Reaches(1)%UpstrmReaches(0))
    
    !Compile reach numbers flowing into every reach
    DO indxReach=2,NReach
      iCount        = 0
      iUpstrmNode   = Reaches(indxReach)%UpstrmNode
      iDownstrmNode = Reaches(indxReach)%DownstrmNode
      !Find the upstream reaches that are directly flowing into current reach
      DO indxReach1=1,indxReach-1
        IF (Reaches(indxReach1)%OutflowDestType .NE. FlowDest_StrmNode) CYCLE
        IF (Reaches(indxReach1)%OutflowDest.GE.iUpstrmNode .AND. Reaches(indxReach1)%OutflowDest.LE.iDownstrmNode) THEN
          iCount                    = iCount + 1
          UpstrmReachesTemp(iCount) = indxReach1
        END IF
      END DO
      !Store information in persistent arrays
      Reaches(indxReach)%NUpstrmReaches = iCount
      ALLOCATE (Reaches(indxReach)%UpstrmReaches(iCount))
      Reaches(indxReach)%UpstrmReaches = UpstrmReachesTemp(1:iCount)
      CALL ShellSort(Reaches(indxReach)%UpstrmReaches)
    END DO
    
  END SUBROUTINE StrmReach_CompileUpstrmReaches
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ONE STREAM NODE (Node1) IS UPSTREAM OF ANOTHER (Node2)
  ! -------------------------------------------------------------
  FUNCTION StrmReach_IsUpstreamNode(Reaches,Node1,Node2) RESULT(lUpstream)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    INTEGER,INTENT(IN)             :: Node1,Node2
    LOGICAL                        :: lUpstream
    
    !Local variables
    INTEGER :: Reach1,Reach2
    
    !If Node1 equals Node2 assume not upstream
    IF (Node1 .EQ. Node2) THEN
        lUpstream = .FALSE.
        RETURN
    END IF
    
    !Obtain the reach numbers the nodes belong to
    Reach1 = StrmReach_GetReachNumber(Node1,Reaches) 
    Reach2 = StrmReach_GetReachNumber(Node2,Reaches) 
    
    !Based on reach numbers find if Node1 is upstream of Node2
    IF (Reach1 .LT. Reach2) THEN
        lUpstream = IsUpstreamReach(Reaches,Reach1,Reach2)
        
    ELSEIF (Reach1 .GT. Reach2) THEN
        lUpstream = .FALSE.
        
    ELSE
        IF (Node1 .LT. Node2) THEN
            lUpstream = .TRUE.
        ELSE
            lUpstream = .FALSE.
        END IF
    END IF
    
  END FUNCTION StrmReach_IsUpstreamNode
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ONE STREAM REACH (Reach1) IS UPSTREAM OF ANOTHER (Reach2)
  ! -------------------------------------------------------------
  RECURSIVE FUNCTION IsUpstreamReach(Reaches,Reach1,Reach2) RESULT(lUpstrm)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    INTEGER,INTENT(IN)             :: Reach1,Reach2
    LOGICAL                        :: lUpstrm
    
    !Local variables
    INTEGER :: indxReachUp,iUpstrmReach
    
    !Initialize
    lUpstrm = .FALSE.
    
    DO indxReachUp=1,Reaches(Reach2)%NUpstrmReaches
        iUpstrmReach = Reaches(Reach2)%UpstrmReaches(indxReachUp)
        IF (iUpstrmReach .EQ. Reach1) THEN
            lUpstrm = .TRUE.
            EXIT
        END IF
        lUpstrm = IsUpstreamReach(Reaches,Reach1,iUpstrmReach)
    END DO
  
  END FUNCTION IsUpstreamReach


END MODULE