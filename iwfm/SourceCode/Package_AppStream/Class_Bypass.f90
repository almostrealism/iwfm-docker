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
MODULE Class_Bypass
  USE MessageLogger                , ONLY: SetLastMessage                          , &
                                           EchoProgress                            , &
                                           MessageArray                            , &
                                           f_iFatal                                  
  USE GeneralUtilities             , ONLY: StripTextUntilCharacter                 , &
                                           CleanSpecialCharacters                  , & 
                                           IntToText                               , &
                                           GetArrayData                            , &
                                           AllocArray                              , &
                                           ConvertID_To_Index                      , &
                                           LocateInList
  USE IOInterface                  , ONLY: GenericFileType                                               
  USE Package_Misc                 , ONLY: PairedDataType                          , &
                                           FlowDestinationType                     , &
                                           f_iFlowDest_Outside                     , &
                                           f_iFlowDest_StrmNode                    , &
                                           f_iFlowDest_Lake                           
  USE Class_RechargeZone           , ONLY: RechargeZoneType                        , &
                                           RechargeZone_New                        
  USE Class_StrmReach              , ONLY: StrmReachType                           , &
                                           StrmReach_GetReachNumber                , &
                                           StrmReach_GetReaches_InUpstrmNetwork    , &
                                           StrmReach_CompileReachNetwork
  USE Package_ComponentConnectors  , ONLY: StrmLakeConnectorType                   , &
                                           f_iBypassToLakeFlow
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
            Bypass_New                 ,  &
            f_iDestTypes
            
  
  ! -------------------------------------------------------------
  ! --- BYPASS DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(FlowDestinationType) :: BypassType
      INTEGER                :: ID              = 0
      CHARACTER(LEN=20)      :: cName           = ''
      INTEGER                :: iNode_Exp       = 0
      INTEGER                :: iColBypass      = 0
      TYPE(PairedDataType)   :: RatingTable
      REAL(8)                :: FracRecvLoss    = 0.0
      REAL(8)                :: FracNonRecvLoss = 0.0
      REAL(8)                :: Bypass_Out      = 0.0
      REAL(8)                :: Bypass_Received = 0.0
      REAL(8)                :: RecvLoss        = 0.0
      REAL(8)                :: NonRecvLoss     = 0.0
      TYPE(RechargeZoneType) :: Recharge
  END TYPE BypassType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: f_iDestTypes(3) = [f_iFlowDest_Outside , f_iFlowDest_StrmNode , f_iFlowDest_Lake]
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
  SUBROUTINE Bypass_New(cFileName,NStrmNodes,iStrmNodeIDs,iElemIDs,iLakeIDs,Reaches,StrmLakeConnector,TUnitStrmFlow,TUnitBypass,Bypasses,iStat)
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName
    INTEGER,INTENT(IN)                  :: NStrmNodes,iStrmNodeIDs(NStrmNodes),iElemIDs(:),iLakeIDs(:)
    TYPE(StrmReachType)                 :: Reaches(:)
    TYPE(StrmLakeConnectorType)         :: StrmLakeConnector
    CHARACTER(LEN=6),INTENT(OUT)        :: TUnitStrmFlow,TUnitBypass
    TYPE(BypassType),TARGET,ALLOCATABLE :: Bypasses(:)
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10) :: ThisProcedure = ModName // 'Bypass_New'
    INTEGER                      :: NBypass,ErrorCode,indxBypass,ID,iNum,NPoints,iReach_Exp,iReach_Imp, &
                                    indxBypass1,iNode_Exp_ID,iDest_ID
    TYPE(GenericFileType)        :: InFile
    REAL(8)                      :: FactFlow,FactBypass,DummyArray(7)
    CHARACTER                    :: ALine*2000
    TYPE(BypassType),POINTER     :: pBypass
    INTEGER,ALLOCATABLE          :: iBypassIDs(:),iBypassOutReachIDs(:),iBypassInReachIDs(:),iReachesUpNetwork(:)
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
    ALLOCATE (Bypasses(NBypass) , iBypassIDs(NBypass) , iBypassOutReachIDs(NBypass) , iBypassInReachIDs(NBypass) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for bypasses!',f_iFatal,ThisProcedure)
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
        
        pBypass             => Bypasses(indxBypass)
        pBypass%cName       =  ALine(1:20)
        ID                  =  INT(DummyArray(1))
        pBypass%ID          =  ID
        iNode_Exp_ID        =  INT(DummyArray(2))
        pBypass%iDestType   =  INT(DummyArray(3))
        iDest_ID            =  INT(DummyArray(4))
        iNum                =  INT(DummyArray(5))
        IF (iNum .GT. 0) THEN
            Bypasses(indxBypass)%iColBypass = iNum
        ELSEIF (iNum .LT. 0) THEN
            NPoints = -iNum
        END IF
        pBypass%FracRecvLoss    = DummyArray(6)
        pBypass%FracNonRecvLoss = DummyArray(7)
        
        !Make sure same ID is not used more than once
        DO indxBypass1=1,indxBypass-1
            IF (ID .EQ. Bypasses(indxBypass1)%ID) THEN
                CALL SetLastMessage('Bypass ID '//TRIM(IntToText(ID))//' is used more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Make sure that iNode_Exp_ID is a legit stream node
        IF (iNode_Exp_ID .GT. 0) THEN
            CALL ConvertID_To_Index(iNode_Exp_ID,iStrmNodeIDs,pBypass%iNode_Exp)
            IF (pBypass%iNode_Exp .EQ. 0) THEN
                MessageArray(1) = 'For bypass ID '//TRIM(IntToText(ID))//', stream node number ('//TRIM(IntToText(iNode_Exp_ID))//') where the '
                MessageArray(2) = 'bypass originates from is not in the model!' 
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        ELSE
            pBypass%iNode_Exp = 0
        END IF
        
        !Make sure that a rating table is not specified if the bypass is coming from outside the model area
        IF (iNode_Exp_ID .EQ. 0) THEN
            IF (pBypass%iColBypass .EQ. 0) THEN
                MessageArray(1) = 'A rating table for bypass number '//TRIM(IntToText(ID))//' ,which originates '
                MessageArray(2) = 'outside the model area, cannot be specifed!'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Make sure destination type is recognized
        IF (.NOT. ANY(pBypass%iDestType.EQ.f_iDestTypes)) THEN
            CALL SetLastMessage('Destination type for bypass number '//TRIM(IntToText(ID))//' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
           
        !Make sure that destination location is simulated
        SELECT CASE (pBypass%iDestType)
            CASE (f_iFlowDest_StrmNode)
                CALL ConvertID_To_Index(iDest_ID,iStrmNodeIDs,pBypass%iDest)
                IF (pBypass%iDest .EQ. 0) THEN
                    CALL SetLastMessage('Stream node '//TRIM(IntToText(iDest_ID))//' that receives water from bypass '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            CASE (f_iFlowDest_Lake)
                CALL ConvertID_To_Index(iDest_ID,iLakeIDs,pBypass%iDest)
                IF (pBypass%iDest .EQ. 0) THEN
                    CALL SetLastMessage('Lake '//TRIM(IntToText(iDest_ID))//' that receives water from bypass '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
        END SELECT
        
        !Check if the exporting stream node is upstream from the importing stream node
        IF (pBypass%iNode_Exp .GT. 0) THEN
            IF (pBypass%iDestType .EQ. f_iFlowDest_StrmNode) THEN
                iReach_Exp = StrmReach_GetReachNumber(pBypass%iNode_Exp,Reaches)  !Stream reach that the exporting stream node belongs to
                iReach_Imp = StrmReach_GetReachNumber(pBypass%iDest,Reaches)      !Stream reach that the importing stream node belongs to
                
                !If exporting and importing nodes are in the same reach
                IF (iReach_Exp .EQ. iReach_Imp) THEN
                    IF (pBypass%iNode_Exp .GE. pBypass%iDest) THEN
                        CALL SetLastMessage('Exporting stream node for by-pass '//TRIM(IntToText(ID))//' must be upstream from the receiving node!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                
                !If exporting and importing nodes are in different reaches (checks only if the nodes are in the same reach network)
                ELSE IF (iReach_Exp .GT. iReach_Imp) THEN
                    DEALLOCATE (iReachesUpNetwork,STAT=ErrorCode)
                    CALL StrmReach_GetReaches_InUpstrmNetwork(Reaches,iReach_Exp,iReachesUpNetwork)
                    IF (LocateInList(iReach_Imp,iReachesUpNetwork) .GT. 0) THEN
                        CALL SetLastMessage('Exporting stream node for by-pass '//TRIM(IntToText(ID))//' must be upstream from the receiving node!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                END IF
            END IF
        END IF
        
        !Make sure there is only one bypass from a node
        DO indxBypass1=1,indxBypass-1
            IF (pBypass%iNode_Exp .EQ. Bypasses(indxBypass1)%iNode_Exp) THEN
                MessageArray(1) = 'There are multiple bypassses defined at stream node '//TRIM(IntToText(iNode_Exp_ID))//'.'
                MessageArray(2) = 'Only one bypass is allowed from a stream node!'
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Compute and save the points in rating table for bypass flows
        IF (iNum .LT. 0) THEN
            IF (NPoints .LT. 2) THEN
                CALL SetLastMessage('There should be at least 2 rating table points for bypass '//TRIM(IntToText(ID))//'!',f_iFatal,ThisProcedure)
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
            CASE (f_iFlowDest_Outside)
                !Do nothing
            CASE (f_iFlowDest_StrmNode)
                pBypass%iDestRegion = 0
            CASE (f_iFlowDest_Lake)
                CALL StrmLakeConnector%AddData(f_iBypassToLakeFlow,indxBypass,pBypass%iDest)
        END SELECT
            
        !Store export and import reach IDs for stream network processing
        iBypassOutReachIDs(indxBypass) = StrmReach_GetReachNumber(pBypass%iNode_Exp,Reaches)
        IF (iBypassOutReachIDs(indxBypass) .GT. 0) iBypassOutReachIDs(indxBypass) = Reaches(iBypassOutReachIDs(indxBypass))%ID
        IF (pBypass%iDestType .EQ. f_iFlowDest_StrmNode) THEN
            iBypassInReachIDs(indxBypass) = StrmReach_GetReachNumber(pBypass%iDest,Reaches)
            IF (iBypassInReachIDs(indxBypass) .GT. 0) iBypassInReachIDs(indxBypass) = Reaches(iBypassInReachIDs(indxBypass))%ID
        ELSE
            iBypassInReachIDs(indxBypass) = 0
        END IF
        
    END DO
    
    !Read the recharge zones
    iBypassIDs = Bypasses%ID
    CALL RechargeZone_New(NBypass,iBypassIDs,iElemIDs,'Bypass',InFile,Bypasses%Recharge,iStat)
    IF (iStat .EQ. -1) RETURN

    !Close file
    CALL InFile%Kill()
    
    !Re-order reaches based on bypasses
    CALL StrmReach_CompileReachNetwork(SIZE(Reaches),Reaches,iStat,iBypassOutReachIDs,iBypassInReachIDs)
    
    !Free memory
    DEALLOCATE (Dummy2DRealArray , iBypassIDs , iBypassOutReachIDs , iBypassInReachIDs , STAT=ErrorCode)
    
  END SUBROUTINE Bypass_New 
  

END MODULE