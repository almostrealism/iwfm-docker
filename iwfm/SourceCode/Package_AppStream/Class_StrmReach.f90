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
MODULE Class_StrmReach
  USE MessageLogger    , ONLY: SetLastMessage       , &
                               MessageArray         , &
                               f_iFatal
  USE Package_Misc     , ONLY: FlowDestinationType  , &
                               f_iFlowDest_StrmNode , &
                               f_iFlowDest_Lake
  USE GeneralUtilities , ONLY: IntToText            , &
                               LocateInList
  USE IOInterface      , ONLY: GenericFileType
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
  PUBLIC :: StrmReachType                          , &
            StrmReach_New                          , &
            StrmReach_GetReachNumber               , &
            StrmReach_WritePreprocessedData        , &
            StrmReach_CompileReachNetwork          , &
            StrmReach_DestinationIDs_To_Indices    , &
            StrmReach_ID_To_Index                  , &
            StrmReach_IsUpstreamNode               , &
            StrmReach_GetNReaches_InUpstrmNetwork  , &
            StrmReach_GetReaches_InUpstrmNetwork
  
  
  ! -------------------------------------------------------------
  ! --- REACH DATA TYPE
  ! -------------------------------------------------------------
  TYPE StrmReachType
      INTEGER             :: ID              = 0
      CHARACTER(LEN=20)   :: cName           = ''
      INTEGER             :: UpstrmNode      = 0
      INTEGER             :: DownstrmNode    = 0
      INTEGER             :: OutflowDestType = f_iFlowDest_StrmNode
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
      CALL InFile%ReadData(Reaches(indxReach)%ID,iStat)               ;  IF (iStat .EQ. -1) RETURN
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
      CALL OutFile%WriteData(Reaches(indxReach)%ID)
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
  ! --- CONVERT REACH ID TO INDEX
  ! -------------------------------------------------------------
  PURE FUNCTION StrmReach_ID_To_Index(Reaches,iReachID) RESULT(Index)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    INTEGER,INTENT(IN)             :: iReachID
    INTEGER                        :: Index
    
    !Local variables
    INTEGER :: indx
    
    Index = 0
    DO indx=1,SIZE(Reaches)
        IF (iReachID .EQ. Reaches(indx)%ID) THEN
            Index = indx
            EXIT
        END IF
    END DO
    
  END FUNCTION StrmReach_ID_To_Index
  
  ! -------------------------------------------------------------
  ! --- CONVERT REACH DESTINATION IDs TO INDICES (MAINLY FOR LAKE DESTINATIONS)
  ! -------------------------------------------------------------
  SUBROUTINE StrmReach_DestinationIDs_To_Indices(Reaches,iLakeIDs,iStat)
    TYPE(StrmReachType) :: Reaches(:)
    INTEGER,INTENT(IN)  :: iLakeIDs(:)
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+35),PARAMETER :: ThisProcedure = ModName // 'StrmReach_DestinationIDs_To_Indices'
    INTEGER                                :: indx,iLakeID,iLake,iReachID
    
    !Initialize
    iStat = 0
    
    !Convert lake IDs to indices
    DO indx=1,SIZE(Reaches)
        IF (Reaches(indx)%OutflowDestType .EQ. f_iFlowDest_Lake) THEN
            iLakeID = Reaches(indx)%OutflowDest
            iLake   = LocateInList(iLakeID,iLakeIDs)
            IF (iLake .EQ. 0) THEN
                iReachID = Reaches(indx)%ID
                CALL SetLastMessage('Lake '//TRIM(IntToText(iLakeID))//' that receive flow from stream reach '//TRIM(IntToText(iReachID))//' is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Reaches(indx)%OutflowDest = iLake
        END IF
    END DO
    
  END SUBROUTINE StrmReach_DestinationIDs_To_Indices
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE REACH NETWORK
  ! -------------------------------------------------------------
  SUBROUTINE StrmReach_CompileReachNetwork(NReaches,Reaches,iStat,iBypassOutReachIDs,iBypassInReachIDs)
    INTEGER,INTENT(IN)          :: NReaches
    TYPE(StrmReachType)         :: Reaches(NReaches)
    INTEGER,INTENT(OUT)         :: iStat
    INTEGER,OPTIONAL,INTENT(IN) :: iBypassOutReachIDs(:),iBypassInReachIDs(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+29),PARAMETER :: ThisProcedure = ModName // 'StrmReach_CompileReachNetwork'
    INTEGER                                :: indxReach,NDownstrmReaches,indx,iReachesAccounted(NReaches),iReachesUnaccounted(NReaches), &
                                              iCount,iTempReachIDs(NReaches),iNBP,indxBP,iReachIn,iReachOut,iReachInID,iReachOutID,      &
                                              iDim,iReachID,iReach,indxReach1,ErrorCode
    LOGICAL                                :: lDownstrmReach(NReaches),lReordered,lIncluded,lDoneOrderingForBypass
    TYPE(StrmReachType)                    :: ReachesOrdered(NReaches)
    INTEGER,ALLOCATABLE                    :: iDownstrmReaches(:),iBPReachIDs_Out(:),iBPReachIDs_In(:),iUpstrmReaches(:)
    
    !Initialize
    iStat          = 0
    lDownstrmReach = .FALSE.
    IF (PRESENT(iBypassOutReachIDs)) THEN
        iNBP = SIZE(iBypassOutReachIDs)
        ALLOCATE (iBPReachIDs_Out(iNBP) , iBPReachIDs_In(iNBP))
        iBPReachIDs_Out = iBypassOutReachIDs
        iBPReachIDs_In  = iBypassInReachIDs
    ELSE
        iNBP = 0
        ALLOCATE (iBPReachIDs_Out(iNBP) , iBPReachIDs_In(iNBP))
    END IF

    !First, find the most downstream reaches
    NDownstrmReaches = 0
    DO indxReach=1,NReaches
        IF (Reaches(indxReach)%OutflowDestType .NE. f_iFlowDest_StrmNode) THEN
            lDownstrmReach(indxReach) = .TRUE.
            NDownstrmReaches          = NDownstrmReaches + 1
        END IF
    END DO
    IF (NDownstrmReaches .EQ. 0) THEN
        MessageArray(1) = 'There is something wrong with stream network set-up!'
        MessageArray(2) = 'Cannot find the most downstream reach(es).'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Starting from the most downstream reaches, build stream network from downstream to upstream
    ALLOCATE (iDownstrmReaches(NDownstrmReaches))
    NDownstrmReaches = 0
    DO indxReach=1,NReaches
        IF (lDownstrmReach(indxReach)) THEN
            NDownstrmReaches                   = NDownstrmReaches + 1
            iDownstrmReaches(NDownstrmReaches) = indxReach
        END IF
    END DO
    indx = 0
    CALL BuildReachNetwork(NDownstrmReaches,iDownstrmReaches,NReaches,Reaches,indx,ReachesOrdered)
    
    !Make sure all reaches are accounted for
    IF (indx .NE. NReaches) THEN
        iReachesAccounted = ReachesOrdered%ID
        iCount            = 0
        DO indxReach=1,NReaches
            IF (LocateInList(Reaches(indxReach)%ID,iReachesAccounted) .EQ. 0) THEN
                iCount                      = iCount + 1
                iReachesUnaccounted(iCount) = Reaches(indxReach)%ID
            END IF
        END DO
        MessageArray(1) = 'Check the stream network set-up!'
        MessageArray(2) = 'Upstream/downstream reaches for the following stream reach IDs cannot be identified:'
        WRITE (MessageArray(3),'(500(I6,2H, ))') iReachesUnaccounted(1:iCount)
    END IF
    
    !Store reverse of the ordered reaches in the return argument (i.e. reverse downstream to upstream ordering to upstream to downstream ordering)
    Reaches = ReachesOrdered(NReaches:1:-1)
    
    !Compile upstream reaches for each reach
    CALL CompileUpstreamReaches(NReaches,Reaches)
    
    !Now, re-arrage based on bypasses
    IF (iNBP .EQ. 0) RETURN
    DO
        lDoneOrderingForBypass = .FALSE.
        lReordered             = .TRUE.
        DO indxBP=1,iNBP
            iReachInID  = iBPReachIDs_In(indxBP)
            iReachOutID = iBPReachIDs_Out(indxBP)
            IF (iReachInID .EQ. 0) THEN
                IF (indxBP .EQ. iNBP) lDoneOrderingForBypass = .TRUE.
                CYCLE
            END IF
            IF (iReachOutID .EQ. 0) THEN
                IF (indxBP .EQ. iNBP) lDoneOrderingForBypass = .TRUE.
                CYCLE
            END IF
            IF (lReordered) iTempReachIDs = Reaches%ID
            iReachIn  = LocateInList(iReachInID,iTempReachIDs)
            iReachOut = LocateInList(iReachOutID,iTempReachIDs)
            IF (iReachIn .LT. iReachOut) THEN
                DEALLOCATE (iUpstrmreaches ,STAT=ErrorCode)
                CALL StrmReach_GetReaches_InUpstrmNetwork(Reaches,iReachOut,iUpstrmReaches)
                iDim = SIZE(iUpstrmReaches)
                !Add uneffected reaches
                ReachesOrdered(1:iReachIn-1) = Reaches(1:iReachIn-1)
                iCount                       = iReachIn - 1
                !Add reaches upstream from bypass out reach
                DO indxReach=iDim,1,-1
                    iReach    = iUpstrmReaches(indxReach)
                    iReachID  = Reaches(iReach)%ID
                    lIncluded = .FALSE.
                    DO indxReach1=1,iReachIn-1
                        IF (ReachesOrdered(indxReach1)%ID .EQ. iReachID) THEN
                            lIncluded = .TRUE.
                            EXIT
                        END IF
                    END DO
                    IF (.NOT. lIncluded) THEN
                        iCount                 = iCount + 1
                        ReachesOrdered(iCount) = Reaches(iReach)
                    END IF
                END DO
                !Add bypass out and in reaches
                ReachesOrdered(iCount+1) = Reaches(iReachOut)
                ReachesOrdered(iCount+2) = Reaches(iReachIn)
                !Add the rest of the reaches
                iCount = iCount + 2
                DO indxReach=iReachIn,NReaches
                    iReachID  = Reaches(indxReach)%ID
                    lIncluded = .FALSE.
                    DO indxReach1=1,iCount
                        IF (ReachesOrdered(indxReach1)%ID .EQ. iReachID) THEN
                            lIncluded = .TRUE.
                            EXIT
                        END IF
                    END DO
                    IF (.NOT. lIncluded) THEN
                        iCount                 = iCount + 1
                        ReachesOrdered(iCount) = Reaches(indxReach)
                    END IF
                END DO
                Reaches    = ReachesOrdered
                lReordered = .TRUE.
                CALL CompileUpstreamReaches(NReaches,Reaches)
                EXIT
            ELSE
                lReordered = .FALSE.
                IF (indxBP .EQ. iNBP) lDoneOrderingForBypass = .TRUE.
            END IF
        END DO
        IF (lDoneOrderingForBypass) EXIT
    END DO
    
    
  CONTAINS
  
  
   ! #############################################################
   ! ### BUILD THE NETWORK FROM UPSTREAM TO DOWNSTREAM
   ! #############################################################
   RECURSIVE SUBROUTINE BuildReachNetwork(NDownstrmReaches,iDownstrmReaches,NReaches,Reaches,indx,ReachesOrdered)
      INTEGER,INTENT(IN)             :: NDownstrmReaches,iDownstrmReaches(NDownstrmReaches),NReaches
      TYPE(StrmReachType),INTENT(IN) :: Reaches(NReaches)
      INTEGER                        :: indx
      TYPE(StrmReachType)            :: ReachesOrdered(NReaches)
      
      !Local variables
      INTEGER             :: indxReach,indxReach1,iUpstrmReach,iStrmNode,NUpstrmReachesNew,iCount,iReach,ID
      LOGICAL             :: lUpstrmReach(NReaches)
      INTEGER,ALLOCATABLE :: iUpstrmReaches(:)
      
      !Add the downsteam reaches to ordered list
      CALL AddOrderedReaches(NReaches,Reaches,iDownstrmReaches,indx,ReachesOrdered)
      
      !Now find reaches that flow into downstream reaches
      DO indxReach=1,NDownstrmReaches
          iReach = iDownstrmReaches(indxReach)
          ID = Reaches(iReach)%ID
          CALL FindUpstreamReaches(iReach,NReaches,Reaches,iUpstrmReaches)
          IF (SIZE(iUpstrmReaches) .EQ. 0) CYCLE
          CALL BuildReachNetwork(SIZE(iUpstrmReaches),iUpstrmReaches,NReaches,Reaches,indx,ReachesOrdered)
      END DO
      
    END SUBROUTINE BuildReachNetwork
    
    
   ! #############################################################
   ! ### FIND UPSTREAM REACHES TO A REACH
   ! #############################################################
    SUBROUTINE FindUpstreamReaches(iReach,NReaches,Reaches,iUpstrmReaches)
      INTEGER,INTENT(IN)             :: iReach,NReaches
      TYPE(StrmReachType),INTENT(IN) :: Reaches(NReaches)
      INTEGER,ALLOCATABLE            :: iUpstrmReaches(:)
      
      !Local variables
      INTEGER :: indxReach,iOutStrmNode,iUpstrmNode,iDownstrmNode,iNUpstrmReaches,ErrorCode,iOutReach
      LOGICAL :: lUpstrmReach(NReaches)
      
      !Initialize
      iUpstrmNode   = Reaches(iReach)%UpstrmNode
      iDownstrmNode = Reaches(iReach)%DownstrmNode
      lUpstrmReach = .FALSE.
      
      !Find upstream reaches for a reach
      iNUpstrmReaches = 0
      !Process based on one reach flowing into our reach in question
      DO indxReach=1,NReaches
          IF (Reaches(indxReach)%OutflowDestType .NE. f_iFlowDest_StrmNode) CYCLE
          iOutStrmNode = Reaches(indxReach)%OutflowDest
          IF (iOutStrmNode .GE. iUpstrmNode) THEN
              IF (iOutStrmNode .LE. iDownstrmNode) THEN
                  lUpstrmReach(indxReach) = .TRUE.
                  iNUpstrmReaches         = iNUpstrmReaches + 1
              END IF
          END IF
      END DO
      
      !Store upstream reach numbers in the return argument
      DEALLOCATE (iUpstrmReaches , STAT=ErrorCode)
      ALLOCATE (iUpstrmReaches(iNUpstrmReaches))
      iNUpstrmReaches = 1
      DO indxReach=1,NReaches
          IF (lUpstrmReach(indxReach)) THEN
              iUpstrmReaches(iNUpstrmReaches) = indxReach
              iNUpstrmReaches                 = iNUpstrmReaches + 1
          END IF
      END DO
      
    END SUBROUTINE FindUpstreamReaches
    
    
   ! #############################################################
   ! ### ADD ORDERED REACHES TO THE GENERAL LIST
   ! #############################################################
    SUBROUTINE AddOrderedReaches(NReaches,Reaches,iReachesToAdd,indx,ReachesOrdered)
      INTEGER,INTENT(IN)             :: NReaches,iReachesToAdd(:)
      TYPE(StrmReachType),INTENT(IN) :: Reaches(NReaches)
      INTEGER                        :: indx
      TYPE(StrmReachType)            :: ReachesOrdered(NReaches)
      
      !Local variables
      INTEGER :: indxLocal,iCount,indx1,iReachAddID
      LOGICAL :: lAdd
      
      iCount = 0
      DO indxLocal=1,SIZE(iReachesToAdd)
          iReachAddID = Reaches(iReachesToAdd(indxLocal))%ID
          lAdd        = .TRUE.
          DO indx1=1,NReaches
              IF (ReachesOrdered(indx1)%ID .EQ. 0) EXIT
              IF (ReachesOrdered(indx1)%ID .EQ. iReachAddID) THEN
                  lAdd = .FALSE.
                  EXIT
              END IF
          END DO
          IF (.NOT. lAdd) CYCLE
          iCount = iCount + 1
          ReachesOrdered(indx+iCount) = Reaches(iReachesToAdd(indxLocal))
      END DO
      
      indx = indx + iCount
      
    END SUBROUTINE AddOrderedReaches

  END SUBROUTINE StrmReach_CompileReachNetwork
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ONE STREAM NODE (Node1) IS UPSTREAM OF ANOTHER (Node2)
  ! -------------------------------------------------------------
  RECURSIVE FUNCTION StrmReach_IsUpstreamNode(Reaches,iNode1,iNode2,iBypassExpNodes,BypassDests,iBypassProcessing) RESULT(lUpstream)
    TYPE(StrmReachType),INTENT(IN)       :: Reaches(:)
    INTEGER,INTENT(IN)                   :: iNode1,iNode2,iBypassExpNodes(:)
    TYPE(FlowDestinationType),INTENT(IN) :: BypassDests(:)
    INTEGER,OPTIONAL,INTENT(IN)          :: iBypassProcessing
    LOGICAL                              :: lUpstream
    
    !Local variables
    INTEGER :: iReach1,iReach2,indxBypass,iBypassDestNode,iBypassExpNode,iBypassProcessing_Local,iBypassReach
    LOGICAL :: lBypassUpstream
    
    !Initialize
    IF (PRESENT(iBypassProcessing)) THEN
        iBypassProcessing_Local = iBypassProcessing
    ELSE
        iBypassProcessing_Local = 0
    END IF
    
    !If iNode1 equals iNode2 assume not upstream
    IF (iNode1 .EQ. iNode2) THEN
        lUpstream = .FALSE.
        RETURN
    END IF
    
    !Obtain the reach numbers the nodes belong to
    iReach1 = StrmReach_GetReachNumber(iNode1,Reaches) 
    iReach2 = StrmReach_GetReachNumber(iNode2,Reaches) 
    
    !Based on reach numbers find if iNode1 is upstream of iNode2
    IF (iReach1 .LT. iReach2) THEN
        !In case the two nodes are in different networks
        lUpstream = IsUpstreamReach(Reaches,iReach1,iReach2)
        IF (lUpstream) RETURN
        
    ELSEIF (iReach1 .GT. iReach2) THEN
        lUpstream = .FALSE.
        RETURN
        
    ELSE
        IF (iNode1 .LT. iNode2) THEN
            lUpstream = .TRUE.
        ELSE
            lUpstream = .FALSE.
        END IF
        RETURN
    END IF
    
    !Process any bypasses into Reach2 at or above Node2
    DO indxBypass=1,SIZE(iBypassExpNodes)
        !Do not compare bypass to itself
        IF (iBypassProcessing_Local .EQ. indxBypass) CYCLE
        
        !Cycle if bypass is originating from outside model area
        iBypassExpNode = iBypassExpNodes(indxBypass)
        IF (iBypassExpNode .LT. 1) CYCLE
        
        !Only process bypasses that flow into stream nodes
        IF (BypassDests(indxBypass)%iDestType .EQ. f_iFlowDest_StrmNode) THEN
            
            !Is iNode1 upstream of iBypassExpNode?
            IF (iBypassExpNode .EQ. iNode1) THEN
                lUpstream = .TRUE.
                RETURN
            ELSE
                iBypassReach = StrmReach_GetReachNumber(iBypassExpNode,Reaches)
                IF (iBypassReach .LT. iReach1) THEN
                    lBypassUpstream = IsUpstreamReach(Reaches,iReach1,iBypassReach)
                    IF (.NOT. lBypassUpstream) CYCLE
                ELSEIF (iBypassReach .GT. iReach1) THEN
                    lBypassUpstream = .FALSE.
                    CYCLE
                ELSE
                    IF (iNode1 .LE. iBypassExpNode) THEN
                        lBypassUpstream = .TRUE.
                    ELSE
                        lBypassUpstream = .FALSE.
                        CYCLE
                    END IF
                END IF
            END IF
            
            !If made it to this point, iBypassExpNode is downstream of iNode1
            !Is iBypassDestNode upstream of iNode2?
            iBypassDestNode = BypassDests(indxBypass)%iDest
            IF (iBypassDestNode .EQ. iNode2) THEN
                lUpstream = .TRUE.
                RETURN
            ELSE
                lUpstream = StrmReach_IsUpstreamNode(Reaches,iBypassDestNode,iNode2,iBypassExpNodes,BypassDests,indxBypass)
                IF (lUpstream) RETURN
            END IF
        END IF
    END DO
    
  END FUNCTION StrmReach_IsUpstreamNode
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ONE STREAM REACH (Reach1) IS UPSTREAM OF ANOTHER (Reach2)
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  RECURSIVE FUNCTION IsUpstreamReach(Reaches,iReach1,iReach2) RESULT(lUpstrm)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    INTEGER,INTENT(IN)             :: iReach1,iReach2
    LOGICAL                        :: lUpstrm
    
    !Local variables
    INTEGER :: indxReachUp,iUpstrmReach
    
    !Initialize
    lUpstrm = .FALSE.
    
    !Check with reaches directly flowing into Reach2
    DO indxReachUp=1,Reaches(iReach2)%NUpstrmReaches
        IF (lUpstrm) EXIT
        iUpstrmReach = Reaches(iReach2)%UpstrmReaches(indxReachUp)
        IF (iUpstrmReach .EQ. iReach1) THEN
            lUpstrm = .TRUE.
            EXIT
        END IF
        lUpstrm = IsUpstreamReach(Reaches,iReach1,iUpstrmReach)
    END DO
    
  END FUNCTION IsUpstreamReach

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ALL REACHES WITHIN THE NETWORK UPSTREAM FROM A REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  RECURSIVE FUNCTION StrmReach_GetNReaches_InUpstrmNetwork(Reaches,iReach,iNReachesIn) RESULT(iNReachesOut)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    INTEGER,INTENT(IN)             :: iReach
    INTEGER,OPTIONAL,INTENT(IN)    :: iNReachesIn
    INTEGER                        :: iNReachesOut
    
    !Local variables
    INTEGER :: indxReachUp,iUpstrmReach,iNReachesIn_Local
    
    !Initialize
    IF (PRESENT(iNReachesIn)) THEN
        iNReachesIn_Local = iNReachesIn
    ELSE
        iNReachesIn_Local = 0
    END IF

    !Move upstream
    iNReachesOut = iNReachesIn_Local + Reaches(iReach)%NUpstrmReaches
    DO indxReachUp=1,Reaches(iReach)%NUpstrmReaches
        iUpstrmReach = Reaches(iReach)%UpstrmReaches(indxReachUp)
        iNReachesOut = StrmReach_GetNReaches_InUpstrmNetwork(Reaches,iUpstrmReach,iNReachesOut)
    END DO
    
  END FUNCTION StrmReach_GetNReaches_InUpstrmNetwork
  
  
  ! -------------------------------------------------------------
  ! --- GET ALL REACH NETWORK UPSTREAM FROM A REACH
  ! --- Note: Must be used after reach network is compiled
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE StrmReach_GetReaches_InUpstrmNetwork(Reaches,iReach,iReaches)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    INTEGER,INTENT(IN)             :: iReach
    INTEGER,ALLOCATABLE            :: iReaches(:)
    
    !Local variables
    INTEGER             :: indxReachUp,iUpstrmReach,iDim,iDim1
    INTEGER,ALLOCATABLE :: iTemp(:)
    
    !Store old and new information in temporary data
    IF (ALLOCATED(iReaches)) THEN
        iDim1 = SIZE(iReaches)
        iDim  = iDim1 + Reaches(iReach)%NUpstrmReaches
        ALLOCATE(iTemp(iDim))
        iTemp(1:iDim1) = iReaches
    ELSE
        iDim1 = 0
        iDim  = Reaches(iReach)%NUpstrmReaches
        ALLOCATE(iTemp(iDim))
    END IF
    iTemp(iDim1+1:) = Reaches(iReach)%UpstrmReaches
    
    !Transfer temporary data to perseitent array
    CALL MOVE_ALLOC(iTemp , iReaches)
    
    !Move upstream
    DO indxReachUp=1,Reaches(iReach)%NUpstrmReaches
        iUpstrmReach = Reaches(iReach)%UpstrmReaches(indxReachUp)
        CALL StrmReach_GetReaches_InUpstrmNetwork(Reaches,iUpstrmReach,iReaches)
    END DO
    
  END SUBROUTINE StrmReach_GetReaches_InUpstrmNetwork
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE UPSTREAM REACHES
  ! -------------------------------------------------------------
  SUBROUTINE CompileUpstreamReaches(NReaches,Reaches)
    INTEGER,INTENT(IN)  :: NReaches
    TYPE(StrmReachType) :: Reaches(NReaches)
    
    !Local variables
    INTEGER :: indxReach,ErrorCode,iUpstrmNode,iDownstrmNode,indxReach1,iOutStrmNode,iCount,iTempReachIDs(NReaches)
    
    DO indxReach=1,NReaches
        DEALLOCATE (Reaches(indxReach)%UpstrmReaches , STAT=ErrorCode)
        iUpstrmNode   = Reaches(indxReach)%UpstrmNode
        iDownstrmNode = Reaches(indxReach)%DownstrmNode
        iCount        = 0
        DO indxReach1=1,NReaches
            IF (Reaches(indxReach1)%OutflowDestType .NE. f_iFlowDest_StrmNode) CYCLE
            iOutStrmNode = Reaches(indxReach1)%OutflowDest
            IF (iOutStrmNode .GE. iUpstrmNode) THEN
                IF (iOutStrmNode .LE. iDownStrmNode) THEN
                    iCount                = iCount + 1
                    iTempReachIDs(iCount) = indxReach1
                END IF
            END IF
        END DO
        Reaches(indxReach)%NUpstrmReaches = iCount
        ALLOCATE (Reaches(indxReach)%UpstrmReaches(iCount))
        Reaches(indxReach)%UpstrmReaches = iTempReachIDs(1:iCount)
    END DO

  END SUBROUTINE CompileUpstreamReaches

END MODULE