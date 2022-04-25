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
MODULE Class_Diversion
  USE MessageLogger                , ONLY: SetLastMessage           , &
                                           EchoProgress             , &
                                           MessageArray             , &
                                           f_iFatal                   
  USE GeneralUtilities             , ONLY: ConvertID_To_Index       , &
                                           IntToText                , &
                                           StripTextUntilCharacter  , &
                                           CleanSpecialCharacters   , &
                                           GetUniqueArrayComponents , &
                                           ShellSort                , &
                                           GetArrayData             , &
                                           LocateInList
  USE IOInterface                  , ONLY: GenericFileType
  USE Package_Discretization       , ONLY: AppGridType
  USE Package_Misc                 , ONLY: FlowDestinationType      , &
                                           ElemGroupType            , &
                                           f_iFlowDest_Element      , &
                                           f_iFlowDest_Subregion    , &
                                           f_iFlowDest_Outside      , &
                                           f_iFlowDest_StrmNode     , &
                                           f_iFlowDest_ElementSet
  USE Class_StrmReach              , ONLY: StrmReachType            , &
                                           StrmReach_GetReachNumber
  USE Class_RechargeZone           , ONLY: RechargeZoneType         , &
                                           RechargeZone_New
  USE Package_ComponentConnectors  , ONLY: SupplyType               , &
                                           Supply_New               , &
                                           Supply_GetPurpose  
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
  PUBLIC :: DiversionType        , &
            DeliveryType         , &
            Diversion_New        , &
            Diversion_GetPurpose
  
  
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
    !INTEGER               :: ID                  = 0           !Diversion ID number is not used; instead delivery IDs are used as part of the "Deli" attribute
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
  SUBROUTINE Diversion_New(cFileName,AppGrid,iElemIDs,iStrmNodeIDs,iSubregionIDs,Reaches,Diversions,iStat)
    CHARACTER(LEN=*),INTENT(IN)     :: cFileName
    TYPE(AppGridType),INTENT(IN)    :: AppGrid
    INTEGER,INTENT(IN)              :: iElemIDs(:),iStrmNodeIDs(:),iSubregionIDs(:)
    TYPE(StrmReachType),INTENT(IN)  :: Reaches(:)
    TYPE(DiversionType),ALLOCATABLE :: Diversions(:)
    INTEGER,iNTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13)                 :: ThisProcedure = ModName // 'Diversion_New'
    CHARACTER(LEN=2000)                          :: ALine
    INTEGER                                      :: NDiver,ErrorCode,ID,indxDiver,iElem,iRegion,indxGroup,iNGroup,NElem,    &
                                                    indxElem,iDest,indxDiver1,iStrmNode,indxGroup1,iDestID
    REAL(8)                                      :: DummyArray(14)
    INTEGER,ALLOCATABLE                          :: TempArray(:),iColIrigFrac(:),iColAdjust(:),iDiverIDs(:),Indices(:)
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
    ALLOCATE (Diversions(NDiver) , DeliDest(NDiver) , iColIrigFrac(NDiver) , iColAdjust(NDiver) , iDiverIDs(NDiver) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for diversions data!',f_iFatal,ThisProcedure)
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
                                      
            ID                      = INT(DummyArray(1))
            pDeli%ID                = ID
            pDiver%iStrmNode        = INT(DummyArray(2))
            pDiver%iMaxDiverCol     = INT(DummyArray(3))
            pDiver%FracMaxDiver     =     DummyArray(4)
            pDiver%iColRecvLoss     = INT(DummyArray(5))
            pDiver%FracRecvLoss     =     DummyArray(6)
            pDiver%iColNonRecvLoss  = INT(DummyArray(7))
            pDiver%FracNonRecvLoss  =     DummyArray(8)
            pDeliDest%iDestType     = INT(DummyArray(9))    
            pDeliDest%iDest         = INT(DummyArray(10))   
            pDeli%iColDeli          = INT(DummyArray(11))
            pDeli%FracDeli          =     DummyArray(12)
            iColIrigFrac(indxDiver) = INT(DummyArray(13))
            iColAdjust(indxDiver)   = INT(DummyArray(14))
            
            !Make sure same ID is not used more than once
            DO indxDiver1=1,indxDiver-1
                IF (ID .EQ. Diversions(indxDiver1)%Deli%ID) THEN 
                    CALL SetLastMessage('Diversion ID '//TRIM(IntToText(ID))//' is used more than once!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
            
            !Check if stream node is in the model
            IF (pDiver%iStrmNode .GT. 0) THEN
                CALL ConvertID_To_Index(pDiver%iStrmNode,iStrmNodeIDs,iStrmNode)
                IF (iStrmNode .EQ. 0) THEN
                    CALL SetLastMessage('Stream node '//TRIM(IntToText(pDiver%iStrmNode))//' where diversion '//TRIM(IntToText(ID))//' is originating from is not in the model!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                pDiver%iStrmNode = iStrmNode
            END IF
            
            !Check if destination is recognized
            SELECT CASE (pDeliDest%iDestType)
                CASE (f_iFlowDest_Outside) 
                    !Do nothing 
                
                CASE (f_iFlowDest_ElementSet) 
                    !Do nothing for now 
                
                CASE (f_iFlowDest_Element)
                    CALL ConvertID_To_Index(pDeliDest%iDest,iElemIDs,iElem)
                    IF (iElem .EQ. 0) THEN
                        CALL SetLastMessage('Element '//TRIM(IntToText(pDeliDest%iDest))//' that receives water from diversion '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    pDeliDest%iDest = iElem
                    
                CASE (f_iFlowDest_Subregion)
                    CALL ConvertID_To_Index(pDeliDest%iDest,iSubregionIDs,iRegion)
                    IF (iRegion .EQ. 0) THEN
                        CALL SetLastMessage('Subregion '//TRIM(IntToText(pDeliDest%iDest))//' that receives water from diversion '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    pDeliDest%iDest = iRegion
                    
                CASE DEFAULT
                    CALL SetLastMessage('Destination type for diversion '//TRIM(IntToText(ID))//' is not recognized!',f_iFatal,ThisProcedure)
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
        ElemGroups(indxGroup)%ID = ID
        
        !Make sure same element group ID is not used more than once
        DO indxGroup1=1,indxGroup-1
            IF (ID .EQ. ElemGroups(indxGroup1)%ID) THEN
                CALL SetLastMessage('Element group ID '//TRIM(IntToText(ID))//' for diversion destinations is specified more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Cycle if no elements are listed
        IF (NElem .LE. 0) CYCLE
        
        !Allocate memory and store the initial readings
        ALLOCATE (ElemGroups(indxGroup)%iElems(NElem))
        DEALLOCATE (Indices, STAT=ErrorCode)  ;  ALLOCATE (Indices(NElem))
        ElemGroups(indxGroup)%NElems    = NElem
        ElemGroups(indxGroup)%iElems(1) = iElem
        
        !Read the rest of the elements 
        DO indxElem=2,NElem
            CALL InFile%ReadData(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
            ElemGroups(indxGroup)%iElems(indxElem) = iElem
        END DO     
        
        !Order the element numbers
        CALL ShellSort(ElemGroups(indxGroup)%iElems)
        
        !Make sure elements are in the model
        CALL ConvertID_To_Index(ElemGroups(indxGroup)%iElems,iElemIDs,Indices)
        IF (ANY(Indices.EQ.0)) THEN
            CALL SetLastMessage('One or more elements listed in element group ID '//TRIM(IntToText(ID))//' listed for diversion destinations are not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        ElemGroups(indxGroup)%iElems = Indices
        
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
        IF (DeliDest(indxDiver)%iDestType .EQ. f_iFlowDest_ElementSet) THEN
            iDestID = DeliDest(indxDiver)%iDest
            
            iDest = LocateInList(iDestID,ElemGroups%ID)
            IF (iDest .EQ. 0) THEN
                ID = Diversions(indxDiver)%Deli%ID
                CALL SetLastMessage('Element group number '//TRIM(IntToText(iDestID))//' to which diversion '//TRIM(IntToText(ID))//' is delivered is not defined!',f_iFatal,ThisProcedure)  
                iStat = -1
                RETURN
            END IF
            DeliDest(indxDiver)%iDest = iDest
            
            !Make sure there is at least one element in the group
            IF (ElemGroups(iDest)%NElems .EQ. 0) THEN
                ID = Diversions(indxDiver)%Deli%ID
                CALL SetLastMessage('Element group '//TRIM(IntToText(iDestID))//' as destination for diversion '//TRIM(IntToText(ID))//' has no elements listed!',f_iFatal,ThisProcedure)
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
    iDiverIDs = Diversions%Deli%ID
    CALL RechargeZone_New(NDiver,iDiverIDs,iElemIDs,'Diversion',InFile,Diversions%Recharge,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile the delivery ranks
    CALL CompileDiversionRanks(Reaches,Diversions)
    
    !Clear memory
    DEALLOCATE (DeliDest , ElemGroups , iColIrigFrac , iColAdjust , Indices , iDiverIDs , STAT=ErrorCode)
    
    !Close file
    CALL InFile%Kill()
    
  END SUBROUTINE Diversion_New




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
  ! --- GET PURPOSE OF DIVERSIONS (IF THEY SERVE AG, URBAN OR BOTH) BEFORE ANY ADJUSTMENT
  ! -------------------------------------------------------------
  SUBROUTINE Diversion_GetPurpose(Divers,iAgOrUrban,iStat)
    TYPE(DiversionType),INTENT(IN) :: Divers(:)
    INTEGER,INTENT(OUT)            :: iAgOrUrban(:),iStat
    
    CALL Supply_GetPurpose(Divers%Deli,iAgOrUrban,iStat)
    
  END SUBROUTINE Diversion_GetPurpose

  

  
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
            IF (Reaches(iDiverCompReach)%OutflowDestType .NE. f_iFlowDest_StrmNode) CYCLE
            iNode_Into      = Reaches(iDiverCompReach)%OutflowDest
            iNode_IntoReach = StrmReach_GetReachNumber(iNode_Into,Reaches)
            DO 
                IF (iNode_IntoReach .EQ. iDiverReach) EXIT
                IF (Reaches(iNode_IntoReach)%OutflowDestType .NE. f_iFlowDest_StrmNode) EXIT
                iNode_Into      = Reaches(iNode_IntoReach)%OutflowDest
                iNode_IntoReach = StrmReach_GetReachNumber(iNode_Into,Reaches)
            END DO
            IF (iNode_IntoReach .EQ. iDiverReach) Diversions(indxDiver)%Rank = Diversions(indxDiver)%Rank + 1
        END DO
    END DO
    
  END SUBROUTINE CompileDiversionRanks
  

END MODULE
