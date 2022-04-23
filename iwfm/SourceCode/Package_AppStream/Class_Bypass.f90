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
MODULE Class_Bypass
  USE MessageLogger                , ONLY: SetLastMessage      , &
                                           EchoProgress        , &
                                           MessageArray        , &
                                           iFatal
  USE GeneralUtilities
  USE IOInterface
  USE Package_Misc                 , ONLY: PairedDataType      , &
                                           FlowDestinationType , &
                                           FlowDest_Outside    , &
                                           FlowDest_StrmNode   , &
                                           FlowDEst_Lake
  USE Class_RechargeZone
  USE Class_StrmReach
  USE Package_ComponentConnectors
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
  PUBLIC :: BypassType                 ,  &
            Bypass_New                 
            
  
  ! -------------------------------------------------------------
  ! --- BYPASS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(FlowDestinationType) :: BypassType
    CHARACTER(LEN=20)      :: cName           = ''
    INTEGER                :: iNode_Exp       = 0
    INTEGER                :: iColBypass      = 0
    TYPE(PairedDataType)   :: RatingTable
    REAL(8)                :: FracRecvLoss    = 0.0
    REAL(8)                :: FracNonRecvLoss = 0.0
    REAL(8)                :: Bypass_Out      = 0.0
    REAL(8)                :: Bypass_Recieved = 0.0
    REAL(8)                :: RecvLoss        = 0.0
    REAL(8)                :: NonRecvLoss     = 0.0
    TYPE(RechargeZoneType) :: Recharge
  END TYPE BypassType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_Bypass::'
  
  
  
  
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
  ! --- INSTANTIATE A SET OF BYPASSES FROM A FILE
  ! -------------------------------------------------------------
  SUBROUTINE Bypass_New(cFileName,NStrmNodes,Reaches,StrmLakeConnector,TUnitStrmFlow,TUnitBypass,Bypasses,iStat)
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName
    INTEGER,INTENT(IN)                  :: NStrmNodes
    TYPE(StrmReachType),INTENT(IN)      :: Reaches(:)
    TYPE(StrmLakeConnectorType)         :: StrmLakeConnector
    CHARACTER(LEN=6),INTENT(OUT)        :: TUnitStrmFlow,TUnitBypass
    TYPE(BypassType),TARGET,ALLOCATABLE :: Bypasses(:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'Bypass_New'
    INTEGER                      :: NBypass,ErrorCode,indxBypass,ID,iNum,NPoints,iReach_Exp,iReach_Imp, &
                                    iDest,indxBypass1
    TYPE(GenericFileType)        :: InFile
    REAL(8)                      :: FactFlow,FactBypass,DummyArray(7)
    CHARACTER                    :: ALine*2000
    TYPE(BypassType),POINTER     :: pBypass
    INTEGER,PARAMETER            :: iDestTypes(3) = [FlowDest_Outside,FlowDest_StrmNode,FlowDest_Lake]
    REAL(8),ALLOCATABLE          :: Dummy2DRealArray(:,:)
    
    !Initialize
    iStat = 0
    
    !Return if no filename is supplied
    IF (cFileName .EQ. '') THEN
        ALLOCATE (Bypasses(0))
        RETURN
    END IF
    
    !Echo progress
    CALL EchoProgress('Instantiating bypasses')

    !Open file
    CALL InFile%New(FileName=TRIM(ADJUSTL(cFileName)),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Bypass specifications data file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Number of bypasses
    CALL InFile%ReadData(NBypass,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Bypasses(NBypass) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for bypasses!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Return if no bypasses are specified
    IF (NBypass .EQ. 0) THEN
        CALL InFile%Kill()
        RETURN
    END IF
    
    !Read basic data
    CALL InFile%ReadData(FactFlow,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine)
    TUnitStrmFlow = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL InFile%ReadData(FactBypass,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine)
    TUnitBypass = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Read bypass data
    DO indxBypass=1,NBypass
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL GetArrayData(ALine,DummyArray,'by-pass number '//TRIM(IntToText(indxBypass)),iStat)
        IF (iStat .EQ. -1) RETURN
        
        pBypass           => Bypasses(indxBypass)
        pBypass%cName     =  ALine(1:20)
        ID                =  INT(DummyArray(1))
        pBypass%iNode_Exp =  INT(DummyArray(2))
        pBypass%iDestType =  INT(DummyArray(3))
        pBypass%iDest     =  INT(DummyArray(4))
        iNum              =  INT(DummyArray(5))
        IF (iNum .GT. 0) THEN
            Bypasses(indxBypass)%iColBypass = iNum
        ELSEIF (iNum .LT. 0) THEN
            NPoints = -iNum
        END IF
        pBypass%FracRecvLoss    = DummyArray(6)
        pBypass%FracNonRecvLoss = DummyArray(7)
        
        !Make sure bypasses are entered sequentially
        IF (ID .NE. indxBypass) THEN 
            MessageArray(1) = 'Bypass specifications should be entered sequentialy.'
            MessageArray(2) = 'Bypass number expected='//TRIM(IntToText(indxBypass))
            MessageArray(3) = 'Bypass number entered ='//TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure that iNode_Exp is greater than zero and less than the number of simulated stream nodes
        IF (pBypass%iNode_Exp.LT.0  .OR. pBypass%iNode_Exp.GT.NStrmNodes) THEN
            MessageArray(1) = 'For bypass ID '//TRIM(IntToText(ID))//', stream node number ('//TRIM(IntToText(pBypass%iNode_Exp))//') where the '
            MessageArray(2) = 'bypass originates from must be a simulated stream node!' 
            CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure that a rating table is not specified if the bypass is coming from outside the model area
        IF (pBypass%iNode_Exp .EQ. 0) THEN
            IF (pBypass%iColBypass .EQ. 0) THEN
                MessageArray(1) = 'A rating table for bypass number '//TRIM(IntToText(indxBypass))//' ,which originates '
                MessageArray(2) = 'outside the model area, cannot be specifed!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Make sure destination type is recognized
        IF (.NOT. ANY(pBypass%iDestType.EQ.iDestTypes)) THEN
            CALL SetLastMessage('Destination type for bypass number '//TRIM(IntToText(indxBypass))//' is not recognized!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
           
        !Make sure that destination stream node is simulated
        IF (pBypass%iDestType .EQ. FlowDest_StrmNode) THEN
            IF (pBypass%iDest.LT.1   .OR.  pBypass%iDest.GT.NStrmNodes) THEN
                MessageArray(1) = 'For bypass ID '//TRIM(IntToText(ID))//', stream node number ('//TRIM(IntToText(pBypass%iDest))//') where the '
                MessageArray(2) = 'bypass is delivered to must be a simulated stream node!' 
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Check if the exporting stream node is upstream from the importing stream node
        IF (pBypass%iNode_Exp .GT. 0) THEN
            IF (pBypass%iDestType .EQ. FlowDest_StrmNode) THEN
                !Find the stream reach that the exporting stream node belongs to
                iReach_Exp = StrmReach_GetReachNumber(pBypass%iNode_Exp,Reaches)
                !Find the stream reach that the importing stream node belongs to
                iReach_Imp = StrmReach_GetReachNumber(pBypass%iDest,Reaches)
                IF (iReach_Exp .EQ. iReach_Imp) THEN
                    IF (pBypass%iNode_Exp .GE. pBypass%iDest) THEN
                        CALL SetLastMessage('Upstream and downstream nodes for by-pass '//TRIM(IntToText(indxBypass))//' is out of sequence.',iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                ELSEIF (iReach_Exp .GT. iReach_Imp) THEN
                    CALL SetLastMessage('Upstream and downstream reaches for by-pass '//TRIM(IntToText(indxBypass))//' is out of sequence.',iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END IF
        END IF
        
        !Make sure there is only one bypass from a node
        DO indxBypass1=1,indxBypass-1
            IF (pBypass%iNode_Exp .EQ. Bypasses(indxBypass1)%iNode_Exp) THEN
                MessageArray(1) = 'There are multiple bypassses defined at stream node '//TRIM(IntToText(pBypass%iNode_Exp))//'.'
                MessageArray(2) = 'Only one bypass is allowed from a stream node!'
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Compute and save the points in rating table for bypass flows
        IF (iNum .LT. 0) THEN
            IF (NPoints .LT. 2) THEN
                CALL SetLastMessage('There should be at least 2 rating table points for bypass '//TRIM(IntToText(indxBypass))//'!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            CALL AllocArray(Dummy2DRealArray,NPoints,2,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL InFile%ReadData(Dummy2DRealArray,iStat)                     ;  IF (iStat .EQ. -1) RETURN
            CALL pBypass%RatingTable%New(NPoints,Dummy2DRealArray(:,1)*FactFlow,Dummy2DRealArray(:,2)*FactBypass,iStat)  
            IF (iStat .EQ. -1) RETURN
        END IF
        
        !Destination region
        SELECT CASE (pBypass%iDestType)
            CASE (FlowDest_Outside)
                !Do nothing
            CASE (FlowDest_StrmNode)
                iDest               = pBypass%iDest
                pBypass%iDestRegion = 0
            CASE (FlowDest_Lake)
                !Do nothing as far as the regions go, but add the bypass-lake connection to stream-lake connector database
                CALL StrmLakeConnector%AddData(iBypassToLakeType,indxBypass,pBypass%iDest)
        END SELECT
    END DO
    
    !Read the recharge zones
    CALL RechargeZone_New(NBypass,InFile,Bypasses%Recharge,iStat)
    IF (iStat .EQ. -1) RETURN

    !Close file
    CALL InFile%Kill()
    
    !Free memory
    DEALLOCATE (Dummy2DRealArray , STAT=ErrorCode)
    
  END SUBROUTINE Bypass_New 
  

END MODULE