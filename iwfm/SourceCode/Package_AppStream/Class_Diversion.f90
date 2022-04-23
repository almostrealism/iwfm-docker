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
MODULE Class_Diversion
  USE MessageLogger               , ONLY: SetLastMessage  , &
                                          EchoProgress    , &
                                          MessageArray    , &
                                          iFatal
  USE GeneralUtilities
  USE IOInterface
  USE Package_Discretization
  USE Package_Misc
  USE Class_StrmReach
  USE Class_RechargeZone
  USE Package_ComponentConnectors  , ONLY: SupplyType     , &
                                           Supply_New
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
  PUBLIC :: DiversionType    , &
            DeliveryType     , &
            Diversion_New
  
  
  ! -------------------------------------------------------------
  ! --- DELIVERY DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(SupplyType) :: DeliveryType
      INTEGER :: iColDeli  = 0
      REAL(8) :: FracDeli  = 1.0
      REAL(8) :: DeliRead  = 0.0
  END TYPE DeliveryType
  
  
  ! -------------------------------------------------------------
  ! --- DIVERSION DATA TYPE
  ! -------------------------------------------------------------
  TYPE DiversionType
    CHARACTER(LEN=20)      :: cName               = ''          !Name of the diversion
    INTEGER                :: iStrmNode           = 0           !Stream node that the diversion originates from (0 means from outisde model area)
    INTEGER                :: Rank                = 0           !Rank of diversion
    REAL(8)                :: MaxDiver            = HUGE(1d0)   !Maximum diversion
    INTEGER                :: iMaxDiverCol        = 0           !Maximum diversion column associated with the diversion in the diversions dat file 
    REAL(8)                :: FracMaxDiver        = 1.0         !Fraction of the data read from the maximum diversion column that will be applied to this diversion
    INTEGER                :: iColRecvLoss        = 0    
    REAL(8)                :: FracRecvLoss        = 1.0
    INTEGER                :: iColNonRecvLoss     = 0
    REAL(8)                :: FracNonRecvLoss     = 1.0
    REAL(8)                :: DiverRead           = 0.0         !Diversion as read from file
    REAL(8)                :: DiverRequired       = 0.0         !Diversion required (might be different from what is read during Supply Adjustment runs)
    REAL(8)                :: DiverActual         = 0.0         !Actual diversion attained
    REAL(8)                :: Ratio_RecvLoss      = 0.0         !Ratio of recoverable loss to total diversion
    REAL(8)                :: Ratio_NonRecvLoss   = 0.0         !Ratio of non-recovreable loss to total diversion 
    REAL(8)                :: RecvLoss            = 0.0         !Actual recoverable loss
    REAL(8)                :: NonRecvLoss         = 0.0         !Actual non-recoverable loss
    TYPE(RechargeZoneType) :: Recharge                          !Information regarding the elements that receive recovrable loss from this diversion
    TYPE(DeliveryType)     :: Deli                              !Delivery information
  END TYPE DiversionType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 17
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_Diversion::'
  
  
  

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
  ! --- INSTANTIATE A SET OF DIVERSIONS FROM A FILE
  ! -------------------------------------------------------------
  SUBROUTINE Diversion_New(cFileName,AppGrid,Reaches,Diversions,iStat)
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    TYPE(StrmReachType),INTENT(IN)  :: Reaches(:)
    TYPE(DiversionType),ALLOCATABLE :: Diversions(:)
    INTEGER,iNTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13)                 :: ThisProcedure = ModName // 'Diversion_New'
    CHARACTER(LEN=2000)                          :: ALine
    INTEGER                                      :: NDiver,ErrorCode,ID,indxDiver,iElem,iRegion,indxGroup,iNGroup,NElem,  &
                                                    indxElem,iDest
    REAL(8)                                      :: DummyArray(14)
    INTEGER,ALLOCATABLE                          :: TempArray(:),iColIrigFrac(:),iColAdjust(:)
    TYPE(GenericFileType)                        :: InFile
    TYPE(FlowDestinationType),ALLOCATABLE        :: DeliDest(:)
    TYPE(ElemGroupType),ALLOCATABLE              :: ElemGroups(:)
    
    !Initialize
    iStat = 0
    
    !Return if the filename is empty
    IF (cFileName .EQ. '') THEN
        ALLOCATE (Diversions(0)) 
        RETURN
    END IF
    
    !Echo progress
    CALL EchoProgress('Instantiating diversions')

    !Open file
    CALL InFile%New(FileName=TRIM(ADJUSTL(cFileName)),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='Diversions specifications data file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Number of diversions
    CALL InFile%ReadData(NDiver,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (Diversions(NDiver) , DeliDest(NDiver) , iColIrigFrac(NDiver) , iColAdjust(NDiver) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for diversions data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !If no diversions are specified, return
    IF (NDiver .EQ. 0) THEN
      CALL InFile%Kill()
      RETURN
    END IF
    
    !Read diversion spec data
    DO indxDiver=1,NDiver
        ASSOCIATE (pDiver    => Diversions(indxDiver)      , &
                   pDeli     => Diversions(indxDiver)%Deli , &
                   pDeliDest => DeliDest(indxDiver)        )
            CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
            CALL CleanSpecialCharacters(ALine)
            ALine = StripTextUntilCharacter(ALine,'/')
            CALL GetArrayData(ALine,DummyArray,'diversion specifications number '//TRIM(IntToText(indxDiver)),iStat)  ;  IF (iStat .EQ. -1) RETURN
            pDiver%cName = ALine(1:20)
            
            ID = INT(DummyArray(1))
            !Make sure diversions are entered sequentailly
            IF (ID .NE. indxDiver) THEN 
                MessageArray(1) = 'Diversion specifications should be entered sequentialy.'
                MessageArray(2) = 'Specification expected='//TRIM(IntToText(indxDiver))
                MessageArray(3) = 'Specification entered ='//TRIM(IntToText(ID))
                CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            pDiver%iStrmNode          =  INT(DummyArray(2))
            pDiver%iMaxDiverCol       =  INT(DummyArray(3))
            pDiver%FracMaxDiver       =      DummyArray(4)
            pDiver%iColRecvLoss       =  INT(DummyArray(5))
            pDiver%FracRecvLoss       =      DummyArray(6)
            pDiver%iColNonRecvLoss    =  INT(DummyArray(7))
            pDiver%FracNonRecvLoss    =      DummyArray(8)
            pDeliDest%iDestType       =  INT(DummyArray(9))    
            pDeliDest%iDest           =  INT(DummyArray(10))   
            pDeli%iColDeli            =  INT(DummyArray(11))
            pDeli%FracDeli            =      DummyArray(12)
            iColIrigFrac(indxDiver)   =  INT(DummyArray(13))
            iColAdjust(indxDiver)     =  INT(DummyArray(14))
            
            !Check if destination is recognized
            SELECT CASE (pDeliDest%iDestType)
              CASE (FlowDest_Outside , FlowDest_Element , FlowDest_Subregion , FlowDest_ElementSet)
                  !Do nothing 
              CASE DEFAULT
                  CALL SetLastMessage('Destination type for diversion '//TRIM(IntToText(indxDiver))//' is not recognized!',iFatal,ThisProcedure)
                  iStat = -1
                  RETURN
            END SELECT

        END ASSOCIATE
    END DO
    
    !Read element group data served by diversions
    CALL InFile%ReadData(iNGroup,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (ElemGroups(iNGroup))
    DO indxGroup=1,iNGroup
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        READ (ALine,*) ID,NElem,iElem
        
        !Make sure groups are entered sequentially
        IF (ID .NE. indxGroup) THEN
            MessageArray(1) = 'Element group IDs for diversion delivery locations must be entered sequentially!'
            MessageArray(2) = 'Group ID expected = '//TRIM(IntToText(indxGroup))
            MessageArray(3) = 'Group ID entered  = '//TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Cycle if no elements are listed
        IF (NElem .LE. 0) CYCLE
        
        !Allocate memory and store the initial readings
        ALLOCATE (ElemGroups(indxGroup)%iElems(NElem))
        ElemGroups(indxGroup)%NElems    = NElem
        ElemGroups(indxGroup)%iElems(1) = iElem
        IF (iElem .GT. 0) iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Read the rest of the elements 
        DO indxElem=2,NElem
            CALL InFile%ReadData(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
            ElemGroups(indxGroup)%iElems(indxElem) = iElem
        END DO     
        
        !Order the element numbers
        CALL ShellSort(ElemGroups(indxGroup)%iElems)
        
        !Get rid of the dublicates
        CALL GetUniqueArrayComponents(ElemGroups(indxGroup)%iElems,TempArray)
        IF (SIZE(TempArray) .NE. ElemGroups(indxGroup)%NElems) THEN
            ElemGroups(indxGroup)%NElems = SIZE(TempArray)
            CALL MOVE_ALLOC(TempArray , ElemGroups(indxGroup)%iElems)
        END IF
        DEALLOCATE (TempArray , STAT=ErrorCode)
        
    END DO
    
    !Assign element groups to the corresponding deliveries
    DO indxDiver=1,NDiver
        IF (DeliDest(indxDiver)%iDestType .EQ. FlowDest_ElementSet) THEN
            iDest = DeliDest(indxDiver)%iDest
            
            !Make sure element group is defined
            IF (iDest.LT.1   .OR.   iDest.GT.iNGroup) THEN
                CALL SetLastMessage('Element group number '//TRIM(IntToText(iDest))//' to which diversion number '//TRIM(IntToText(indxDiver))//' is delivered is not defined!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Make sure there is at least one element in the group
            IF (ElemGroups(iDest)%NElems .EQ. 0) THEN
                CALL SetLastMessage('Element group '//TRIM(IntToText(iDest))//' as destination for diversion '//TRIM(IntToText(indxDiver))//' has no elements listed!',iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Assign element group to diversion
            DeliDest(indxDiver)%iDestRegion = AppGrid%AppElement(ElemGroups(iDest)%iElems(1))%Subregion
            DeliDest(indxDiver)%iDestElems  = ElemGroups(iDest)
        END IF
    END DO
    
    !Instantiate deliveries as a Supply objects
    CALL Supply_New(iColIrigFrac,iColAdjust,DeliDest,Diversions%Deli)
    
    !Read the recharge zone data
    CALL RechargeZone_New(NDiver,InFile,Diversions%Recharge,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile the dilevrey ranks
    CALL CompileDiversionRanks(Reaches,Diversions)
    
    !Clear memory
    DEALLOCATE (DeliDest , ElemGroups , iColIrigFrac , iColAdjust , STAT=ErrorCode)
    
    !Close file
    CALL InFile%Kill()
    
  END SUBROUTINE Diversion_New




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
  ! --- COMPUTE RANK OF DELIVERIES
  ! -------------------------------------------------------------
  SUBROUTINE CompileDiversionRanks(Reaches,Diversions)
    TYPE(StrmReachType),INTENT(IN) :: Reaches(:)
    TYPE(DiversionType),TARGET     :: Diversions(:)
    
    !Local variables
    INTEGER                    :: indxDiver,NDiver,iDiverNode,Node_Compared(SIZE(Diversions)),  &
                                  iDiverReach,indxDiverComp,iDiverNodeComp,iTotal_Compared,     &
                                  iDiverCompReach,iNode_Into,iNode_IntoReach
    
    !Initialize
    NDiver = SIZE(Diversions)
    
    !Iterate over diversions
    DO indxDiver=1,NDiver
      iDiverNode = Diversions(indxDiver)%iStrmNode         !Diversion stream node 
      IF (iDiverNode .EQ. 0) CYCLE                         !Skip rank computations if the diversion originates from outside the model area
      
      !Find the reach number where the diversion is located
      iDiverReach = StrmReach_GetReachNumber(iDiverNode,Reaches)
      
      !Compare the other diversions to the diversion in hand for ranking
      Node_Compared   = 0
      iTotal_Compared = 0
      DO indxDiverComp=1,NDiver
        iDiverNodeComp = Diversions(indxDiverComp)%iStrmNode
        IF (iDiverNodeComp .EQ. 0) CYCLE 
        IF (ANY(Node_Compared .EQ. iDiverNodeComp)) CYCLE
        IF (iDiverNode .EQ. iDiverNodeComp) CYCLE
        IF (indxDiverComp .EQ. indxDiver) CYCLE
        iTotal_Compared                = iTotal_Compared + 1
        Node_Compared(iTotal_Compared) = iDiverNodeComp
        !Find the reach number where the comparison diversion is located
        iDiverCompReach = StrmReach_GetReachNumber(iDiverNodeComp,Reaches)
        IF (iDiverCompReach.EQ.iDiverReach  .AND.  iDiverNode.GT.iDiverNodeComp) THEN
          Diversions(indxDiver)%Rank = Diversions(indxDiver)%Rank + 1
          CYCLE
        END IF
        !Check if flow from comparison reach ever flows into reach in question
        IF (Reaches(iDiverCompReach)%OutflowDestType .NE. FlowDest_StrmNode) CYCLE
        iNode_Into      = Reaches(iDiverCompReach)%OutflowDest
        iNode_IntoReach = StrmReach_GetReachNumber(iNode_Into,Reaches)
        DO 
          IF (iNode_IntoReach .EQ. iDiverReach) EXIT
          IF (Reaches(iNode_IntoReach)%OutflowDestType .NE. FlowDest_StrmNode) EXIT
          iNode_Into      = Reaches(iNode_IntoReach)%OutflowDest
          iNode_IntoReach = StrmReach_GetReachNumber(iNode_Into,Reaches)
        END DO
        IF (iNode_IntoReach .EQ. iDiverReach) Diversions(indxDiver)%Rank = Diversions(indxDiver)%Rank + 1
      END DO
    END DO
    
  END SUBROUTINE CompileDiversionRanks
  

END MODULE
