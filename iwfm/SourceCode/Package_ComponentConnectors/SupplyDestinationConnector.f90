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
MODULE SupplyDestinationConnector
  USE MessageLogger          , ONLY: SetLastMessage         , &
                                     EchoProgress           , &
                                     MessageArray           , &
                                     f_iFatal                 
  USE GeneralUtilities       , ONLY: IntToText              , &
                                     LowerCase              , &
                                     NormalizeArray         , &
                                     AllocArray             
  USE Package_Discretization , ONLY: AppGridType            
  USE Package_Misc           , ONLY: FlowDestinationType    , &
                                     f_iFlowDest_Outside    , &
                                     f_iFlowDest_Element    , &
                                     f_iFlowDest_ElementSet , &
                                     f_iFlowDest_Subregion
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
  PUBLIC :: SupplyType                              , &
            SupplyDestinationConnectorType          , &
            SupplyToDestinationType                 , &
            DestinationToSupplyType                 , &
            Supply_New                              , &
            Supply_GetDestination                   , &
            Supply_GetSupply                        , &
            Supply_GetPurpose                       , &
            Supply_SetIrigFracsRead                 , &
            Supply_CheckSupplyDestinationConnection , &
            Supply_ResetIrigFracs                   , &
            Supply_SetSupplySpecs                   
  
  ! -------------------------------------------------------------
  ! --- SUPPLY TO DESTINATION CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE SupplyToDestinationType
      INTEGER             :: iDestType            = f_iFlowDest_Outside  !Supply destination type
      INTEGER             :: nDest                = 0                    !Number of destinations that is served by a generic water supply
      INTEGER,ALLOCATABLE :: iDests(:)                                   !List of destinationss served by a generic water supply
      REAL(8),ALLOCATABLE :: SupplyToDestFracs_Ag(:)                     !Fraction of the generic ag water supply that go to each destination to meet ag water demand
      REAL(8),ALLOCATABLE :: SupplyToDestFracs_Urb(:)                    !Fraction of the generic urban water supply that go to each destination to meet urban water demand
  CONTAINS
      PROCEDURE,PASS :: Kill    => SupplyToDestination_Kill
  END TYPE SupplyToDestinationType
  
 
  ! -------------------------------------------------------------
  ! --- SUPPLY DATA TYPE
  ! -------------------------------------------------------------
  TYPE SupplyType
      INTEGER                   :: ID                 = 0                    !Supply ID number
      REAL(8)                   :: SupplyRequired     = 0.0                  !Required amount of supply to meet a demand (either read from file or computed to meet a demand)
      REAL(8)                   :: SupplyActual       = 0.0                  !Actual amount of supply that may be les than required due to some limiting condition
      INTEGER                   :: iColIrigFrac       = 0                    !Pointer to irrigation fraction data column in the relevant file
      REAL(8)                   :: IrigFracRead       = 0.0                  !Irrigtaion fraction as read from file
      REAL(8)                   :: IrigFrac           = 0.0                  !Irrigtaion fraction that may be different than the read value due to supply adjustment
      INTEGER                   :: iColAdjust         = 0                    !Pointer to the supply adjustment spec column in the relevant file
      TYPE(FlowDestinationType) :: Destination                               !Destination information for supply
  END TYPE SupplyType
  

  ! -------------------------------------------------------------
  ! --- DESTINATION TO SUPPLY CONNECTOR TYPE
  ! -------------------------------------------------------------
  TYPE DestinationToSupplyType
      INTEGER             :: nSupply          = 0   !Number of a specific type of supply serving this destination
      INTEGER,ALLOCATABLE :: iSupplies(:)           !List of a specific type of supplies (e.g. diversions) serving this destination
      INTEGER,ALLOCATABLE :: iIndexInServedDest(:)  !Index in the SupplyType%ServedDestinations array to locate the information for the destination (from supply's perspective)
  CONTAINS
      PROCEDURE,PASS :: Kill => DestinationToSupply_Kill
  END TYPE DestinationToSupplyType
    

  ! -------------------------------------------------------------
  ! --- SUPPLY-DESTINATION CONNECTOR DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE SupplyDestinationConnectorType
      INTEGER                                   :: NSupply                = 0  !Number of supplies (i.e. size of SupplyToDestination array)
      INTEGER                                   :: NDestination           = 0  !Number of destinations (i.e. size of DestinationToSupply array)
      TYPE(SupplyToDestinationType),ALLOCATABLE :: SupplyToDestination(:)
      TYPE(DestinationToSupplyType),ALLOCATABLE :: DestinationToSupply(:)
  CONTAINS
      PROCEDURE,PASS :: New  => SupplyDestinationConnector_New
      PROCEDURE,PASS :: Kill => SupplyDestinationConnector_Kill
      PROCEDURE,PASS :: GetConnectionLists 
      PROCEDURE,PASS :: GetServedElemList
      PROCEDURE,PASS :: InitSupplyToAgUrbanFracs         
  END TYPE SupplyDestinationConnectorType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 28
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'SupplyDestinationConnector::'
  
  

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
  ! --- INSTANTAITE SupplyDestinationConnector OBJECT
  ! -------------------------------------------------------------  
  SUBROUTINE SupplyDestinationConnector_New(Connector,cSupplyDescription,iDemandCalcLocation,SupplyDest,AppGrid,iStat)
    CLASS(SupplyDestinationConnectorType) :: Connector
    CHARACTER(LEN=*),INTENT(IN)           :: cSupplyDescription
    INTEGER,INTENT(IN)                    :: iDemandCalcLocation
    TYPE(FlowDestinationType),INTENT(IN)  :: SupplyDest(:)
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'SupplyDestinationConnector_New'
    INTEGER                      :: indxSupply,NSupply,NDestination,NElements,NSubregions
    
    !Initialize
    iStat       = 0
    NSupply     = SIZE(SupplyDest)
    NElements   = AppGrid%NElements
    NSubregions = AppGrid%NSubregions
    IF (iDemandCalcLocation .EQ. f_iFlowDest_Element) THEN
        NDestination = NElements
    ELSEIF (iDemandCalcLocation .EQ. f_iFlowDest_Subregion) THEN
        NDestination = NSubregions
    ELSE
        CALL SetLastMessage('Computational unit for water demand calculations is not recognized!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Allocate memory
    ALLOCATE (Connector%SupplyToDestination(NSupply) , Connector%DestinationToSupply(NDestination))
    Connector%NSupply      = NSupply
    Connector%NDestination = NDestination
    
    !Compile SupplyToDestination connection list
    SELECT CASE (iDemandCalcLocation)
        CASE (f_iFlowDest_Element)
            DO indxSupply=1,NSupply
                CALL SupplyToElement_New(Connector%SupplyToDestination(indxSupply),cSupplyDescription,indxSupply,SupplyDest(indxSupply),AppGrid,iStat)
                IF (iStat .EQ. -1) RETURN
            END DO
            
        CASE (f_iFlowDest_Subregion)
            DO indxSupply=1,NSupply
                CALL SupplyToSubregion_New(Connector%SupplyToDestination(indxSupply),cSupplyDescription,indxSupply,SupplyDest(indxSupply),AppGrid,iStat) 
                IF (iStat .EQ. -1) RETURN
            END DO
    END SELECT
        
    !Compile DestinationToSupplyConnection list
    CALL DestinationToSupply_New(Connector%SupplyToDestination,Connector%DestinationToSupply)    
    
  END SUBROUTINE SupplyDestinationConnector_New
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTAITE Supply OBJECT
  ! -------------------------------------------------------------  
  SUBROUTINE Supply_New(iColIrigFrac,iColAdjust,Dest,Supply)
    INTEGER,INTENT(IN)                   :: iColIrigFrac(:),iColAdjust(:)
    TYPE(FlowDestinationType),INTENT(IN) :: Dest(:)
    CLASS(SupplyType)                    :: Supply(:)
    
    !Local variables
    INTEGER :: indx
    
    !Store iColIrigFrac and IColAdj values
    DO indx=1,SIZE(Supply)
        Supply(indx)%iColIrigFrac = iColIrigFrac(indx)
        Supply(indx)%iColAdjust   = iColAdjust(indx)
    END DO
    
    !Store destination information
    DO indx=1,SIZE(Dest)
        Supply(indx)%Destination = Dest(indx)
    END DO
    
  END SUBROUTINE Supply_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW SUPPLY TO ELEMENT CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE SupplyToElement_New(Connector,cDescription,iSupply,Destination,AppGrid,iStat) 
    TYPE(SupplyToDestinationType)        :: Connector
    CHARACTER(LEN=*),INTENT(IN)          :: cDescription
    INTEGER,INTENT(IN)                   :: iSupply
    TYPE(FlowDestinationType),INTENT(IN) :: Destination
    TYPE(AppGridType),INTENT(IN)         :: AppGrid
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'SupplyToElement_New'
    INTEGER                      :: NElems,iElem,indxElem,iRegion
    
    !Initialize
    iStat = 0
    
    !Clear the connector for a fresh start
    CALL Connector%Kill()
    
    SELECT CASE (Destination%iDestType)
        !Supply goes to outside model area
        CASE (f_iFlowDest_Outside)
            Connector%iDestType = f_iFlowDest_Outside
        
        !Supply goes to an element
        CASE (f_iFlowDest_Element)
            !Make sure element is modeled
            IF (Destination%iDest.LT.1   .OR.   Destination%iDest.GT.AppGrid%NElements) THEN
                MessageArray(1) = 'A ' // TRIM(LowerCase(cDescription)) //' is delivered to an element that is not in the model domain!'
                MessageArray(2) = TRIM(cDescription) // ' number = ' //TRIM(IntToText(iSupply))
                MessageArray(3) = 'Element delivered = ' // TRIM(IntToText(Destination%iDest))
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Store element info
            Connector%iDestType = f_iFlowDest_Element
            Connector%nDest     = 1
            ALLOCATE (Connector%iDests(1)               , &
                      Connector%SupplyToDestFracs_Ag(1) , &
                      Connector%SupplyToDestFracs_Urb(1))
            Connector%iDests(1)                = Destination%iDest
            Connector%SupplyToDestFracs_Ag(1)  = 1.0
            Connector%SupplyToDestFracs_Urb(1) = 1.0
        
        !Supply goes to a group of elements
        CASE (f_iFlowDest_ElementSet)
            NElems = Destination%iDestElems%NElems
            !Make sure elements are modeled
            DO indxElem=1,NElems
                iElem = Destination%iDestElems%iElems(indxElem)
                IF (iElem.LT.1   .OR.   iElem.GT.AppGrid%NElements) THEN
                    MessageArray(1) = 'A ' // TRIM(LowerCase(cDescription)) //' is delivered to an element that is not in the model domain!'
                    MessageArray(2) = TRIM(cDescription) // ' number = ' //TRIM(IntToText(iSupply))
                    MessageArray(3) = 'Element delivered = ' // TRIM(IntToText(iElem))
                    CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
            END DO
            
            !Store element data 
            Connector%iDestType = f_iFlowDest_Element
            Connector%nDest     = NElems
            ALLOCATE (Connector%iDests(NElems)               , &
                      Connector%SupplyToDestFracs_Ag(NElems) , & 
                      Connector%SupplyToDestFracs_Urb(NElems))
            Connector%iDests                = Destination%iDestElems%iElems
            Connector%SupplyToDestFracs_Ag  = 1d0 / REAL(NElems,8)  !This is an initialization
            Connector%SupplyToDestFracs_Urb = 1d0 / REAL(NElems,8)  !This is an initialization
        
        !Supply goes to a subregion
        CASE (f_iFlowDest_Subregion)
            iRegion = Destination%iDest
            !Make sure subregion is modeled
            IF (iRegion.LT.1  .OR. iRegion.GT.AppGrid%NSubregions) THEN
                MessageArray(1) = 'A ' // TRIM(LowerCase(cDescription)) //' is delivered to a subregion that is not in the model domain!'
                MessageArray(2) = TRIM(cDescription) // ' number = ' //TRIM(IntToText(iSupply))
                MessageArray(3) = 'Subregion delivered = ' // TRIM(IntToText(iRegion))
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Store in permanent arrays
            Connector%iDestType = f_iFlowDest_Element
            Connector%nDest     = AppGrid%AppSubregion(iRegion)%NRegionElements
            ALLOCATE (Connector%iDests(Connector%nDest)               , &
                      Connector%SupplyToDestFracs_Ag(Connector%nDest) , & 
                      Connector%SupplyToDestFracs_Urb(Connector%nDest))
            Connector%iDests                = AppGrid%AppSubregion(iRegion)%RegionElements
            Connector%SupplyToDestFracs_Ag  = 1.0
            Connector%SupplyToDestFracs_Urb = 1.0
            
    END SELECT
    
  END SUBROUTINE SupplyToElement_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW SUPPLY TO SUBREGION CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE SupplyToSubregion_New(Connector,cDescription,iSupply,Destination,AppGrid,iStat) 
    TYPE(SupplyToDestinationType)         :: Connector
    CHARACTER(LEN=*),INTENT(IN)           :: cDescription
    INTEGER,INTENT(IN)                    :: iSupply
    TYPE(FlowDestinationType),INTENT(IN)  :: Destination
    TYPE(AppGridType),INTENT(IN)          :: AppGrid
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21) :: ThisProcedure = ModName // 'SupplyToSubregion_New'
    INTEGER                      :: NElems,iRegion
    
    !Initialize
    iStat = 0
    
    !Clear the connector for a fresh start
    CALL Connector%Kill()
    
    SELECT CASE (Destination%iDestType)
        !Supply goes to outside model area
        CASE (f_iFlowDest_Outside)
            Connector%iDestType = f_iFlowDest_Outside
        
        !Supply goes to an element
        CASE (f_iFlowDest_Element)
            !Make sure element is modeled
            IF (Destination%iDest.LT.1   .OR.   Destination%iDest.GT.AppGrid%NElements) THEN
                MessageArray(1) = 'A ' // TRIM(LowerCase(cDescription)) //' is delivered to an element that is not in the model domain!'
                MessageArray(2) = TRIM(cDescription) // ' number = ' //TRIM(IntToText(iSupply))
                MessageArray(3) = 'Element delivered = ' // TRIM(IntToText(Destination%iDest))
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Store subregion info
            Connector%iDestType = f_iFlowDest_Subregion
            Connector%nDest     = 1
            ALLOCATE (Connector%iDests(1)               , &
                      Connector%SupplyToDestFracs_Ag(1) , &
                      Connector%SupplyToDestFracs_Urb(1))
            Connector%iDests(1)                = AppGrid%AppElement(Destination%iDest)%Subregion
            Connector%SupplyToDestFracs_Ag(1)  = 1.0
            Connector%SupplyToDestFracs_Urb(1) = 1.0
        
        !Supply goes to a group of elements
        !*** Note: It is assumed all elements are in the same subregion and this is already checked
        CASE (f_iFlowDest_ElementSet)
            NElems = Destination%iDestElems%NElems
            !Store subregion data 
            Connector%iDestType = f_iFlowDest_Subregion
            Connector%nDest     = 1
            ALLOCATE (Connector%iDests(1)               , &
                      Connector%SupplyToDestFracs_Ag(1) , & 
                      Connector%SupplyToDestFracs_Urb(1))
            Connector%iDests(1)                = AppGrid%AppElement(Destination%iDestElems%iElems(1))%Subregion
            Connector%SupplyToDestFracs_Ag(1)  = 1.0
            Connector%SupplyToDestFracs_Urb(1) = 1.0
        
        !Supply goes to a subregion
        CASE (f_iFlowDest_Subregion)
            iRegion = Destination%iDest
            !Make sure subregion is modeled
            IF (iRegion.LT.1  .OR. iRegion.GT.AppGrid%NSubregions) THEN
                MessageArray(1) = 'A ' // TRIM(LowerCase(cDescription)) //' is delivered to a subregion that is not in the model domain!'
                MessageArray(2) = TRIM(cDescription) // ' number = ' //TRIM(IntToText(iSupply))
                MessageArray(3) = 'Subregion delivered = ' // TRIM(IntToText(iRegion))
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Store in permanent arrays
            Connector%iDestType = f_iFlowDest_Subregion
            Connector%nDest     = 1
            ALLOCATE (Connector%iDests(1)               , &
                      Connector%SupplyToDestFracs_Ag(1) , & 
                      Connector%SupplyToDestFracs_Urb(1))
            Connector%iDests(1)                = iRegion
            Connector%SupplyToDestFracs_Ag(1)  = 1.0
            Connector%SupplyToDestFracs_Urb(1) = 1.0
            
    END SELECT
              
  END SUBROUTINE SupplyToSubregion_New

  
  ! -------------------------------------------------------------
  ! --- NEW DESTINATION TO SUPPLY CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE DestinationToSupply_New(SupplyToDest,Connectors)
    TYPE(SupplyToDestinationType),INTENT(IN)  :: SupplyToDest(:)
    TYPE(DestinationToSupplyType),INTENT(OUT) :: Connectors(:)
    
    !Local variables
    INTEGER :: indxSupply,indxDest,iDest
    
    DO indxSupply=1,SIZE(SupplyToDest)
        DO indxDest=1,SupplyToDest(indxSupply)%nDest
            iDest = SupplyToDest(indxSupply)%iDests(indxDest)
            CALL AddData(indxSupply,indxDest,Connectors(iDest))
        END DO
    END DO
            
    
  CONTAINS 
  
  
    ! ############################################
    ! --- ADD SUPPLY TO THE LIST OF SUUPLIES TO A GIVEN DESTINATION
    ! ############################################
    SUBROUTINE AddData(indxSupply,indxDest,Connector)
      INTEGER,INTENT(IN)            :: indxSupply,indxDest
      TYPE(DestinationToSupplyType) :: Connector
      
      !Local variables
      INTEGER             :: nSupply
      INTEGER,ALLOCATABLE :: iTempSupplies(:),iTempIndexInServedDest(:)
      
      nSupply = Connector%nSupply
      
      ALLOCATE (iTempSupplies(nSupply+1) , iTempIndexInServedDest(nSupply+1))
      iTempSupplies(1:nSupply)          = Connector%iSupplies
      iTempIndexInServedDest(1:nSupply) = Connector%iIndexInServedDest
      iTempSupplies(nSupply+1)          = indxSupply
      iTempIndexInServedDest(nSupply+1) = indxDest
      
      CALL MOVE_ALLOC(iTempSupplies,Connector%iSupplies)
      CALL MOVE_ALLOC(iTempIndexInServedDest,Connector%iIndexInServedDest)
      Connector%nSupply = nSupply + 1
      
    END SUBROUTINE AddData

  END SUBROUTINE DestinationToSupply_New

  
  
  
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
  ! --- KILL SUPPLY-TO-DESTINATION CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE SupplyToDestination_Kill(SupplyToDest)
    CLASS(SupplyToDestinationType) :: SupplyToDest
    
    !Local variables
    INTEGER                       :: ErrorCode
    TYPE(SupplyToDestinationType) :: Dummy
    
    DEALLOCATE (SupplyToDest%iDests                , &
                SupplyToDest%SupplyToDestFracs_Ag  , &
                SupplyToDest%SupplyToDestFracs_Urb , &
                STAT=ErrorCode                     )
    SupplyToDest%iDestType = Dummy%iDestType
    SupplyToDest%nDest     = Dummy%nDest
    
  END SUBROUTINE SupplyToDestination_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL DESTINATION-TO-SUPPLY CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE DestinationToSupply_Kill(DestToSupply)
    CLASS(DestinationToSupplyType) :: DestToSupply
    
    !Local variables
    INTEGER                       :: ErrorCode
    TYPE(DestinationToSupplyType) :: DummyData
    
    DestToSupply%nSupply = DummyData%nSupply
    
    DEALLOCATE (DestToSupply%iSupplies , DestToSupply%iIndexInServedDest , STAT=ErrorCode)
    
  END SUBROUTINE DestinationToSupply_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL SUPPLY-DESTINATION CONNECTOR
  ! -------------------------------------------------------------
  SUBROUTINE SupplyDestinationConnector_Kill(Connector)
    CLASS(SupplyDestinationConnectorType) :: Connector
    
    !Local variables
    INTEGER                              :: ErrorCode,indx
    TYPE(SupplyDestinationConnectorType) :: DummyData
    
    DO indx=1,Connector%NSupply
        CALL Connector%SupplyToDestination(indx)%Kill()
    END DO
    
    DO indx=1,Connector%NDestination
        CALL Connector%DestinationToSupply(indx)%Kill()
    END DO
    
    DEALLOCATE (Connector%SupplyToDestination , Connector%DestinationToSupply , STAT=ErrorCode)
    
    Connector%NSupply      = DummyData%NSupply
    Connector%NDestination = DummyData%NDestination
    
  END SUBROUTINE SupplyDestinationConnector_Kill
  
  
  
  
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
  ! --- GET LIST OF ELEMENTS SERVED BY A SUPPLY
  ! -------------------------------------------------------------
  SUBROUTINE GetServedElemList(Connector,iSupply,iElemList)
    CLASS(SupplyDestinationConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                               :: iSupply
    INTEGER,ALLOCATABLE,INTENT(OUT)                  :: iElemList(:)
    
    ASSOCIATE (pDest => Connector%SupplyToDestination(iSupply))
        SELECT CASE (pDest%iDestType)
            CASE DEFAULT
                ALLOCATE (iElemList(0))
                
            CASE (f_iFlowDest_Element)
                ALLOCATE (iElemList(pDest%nDest))
                iElemList = pDest%iDests
        END SELECT
    END ASSOCIATE
    
  END SUBROUTINE GetServedElemList

  
  ! -------------------------------------------------------------
  ! --- GET DESTINATION-TO-SUPPLY AND SUPPLY-TO-DESTINATION CONNECTION DATA 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetConnectionLists(Connector,SupplyToDest,DestToSupply)
    CLASS(SupplyDestinationConnectorType),INTENT(IN) :: Connector
    TYPE(SupplyToDestinationType),INTENT(OUT)        :: SupplyToDest(:)
    TYPE(DestinationToSupplyType),INTENT(OUT)        :: DestToSupply(:)
    
    SupplyToDest = Connector%SupplyToDestination
    DestToSupply = Connector%DestinationToSupply
    
  END SUBROUTINE GetConnectionLists
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY DESTINATION DATA 
  ! -------------------------------------------------------------
  SUBROUTINE Supply_GetDestination(Supply,Destination)
    CLASS(SupplyType),INTENT(IN)          :: Supply(:)
    TYPE(FlowDestinationType),ALLOCATABLE :: Destination(:)
    
    !Local variables
    INTEGER :: ErrorCode,indx,iDim
    
    !Clean Destination
    DEALLOCATE (Destination , STAT=ErrorCode)
    
    !Allocate memeory and store data
    iDim = SIZE(Supply)
    ALLOCATE (Destination(iDim))
    DO indx=1,iDim
        Destination(indx) = Supply(indx)%Destination
    END DO
    
  END SUBROUTINE Supply_GetDestination
  
  
  ! -------------------------------------------------------------
  ! --- GET SUPPLY TO DEMAND LOCATIONS
  ! -------------------------------------------------------------
  SUBROUTINE Supply_GetSupply(Supply,SupplyDestConnector,Supply_Ag,Supply_Urb)
    CLASS(SupplyType),INTENT(IN)                    :: Supply(:)
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: SupplyDestConnector
    REAL(8),INTENT(OUT)                             :: Supply_Ag(:),Supply_Urb(:)
    
    !Local variables
    INTEGER :: indx,indxDest,iDest
    REAL(8) :: SupplyActual,SupplyActual_Ag,SupplyActual_Urb
    
    !Initialize
    Supply_Ag  = 0.0
    Supply_Urb = 0.0
    
    !Compile
    DO indx=1,SIZE(Supply)
      ASSOCIATE (pServedDests => SupplyDestConnector%SupplyToDestination(indx))
        
        SupplyActual     = Supply(indx)%SupplyActual
        SupplyActual_Ag  = SupplyActual * Supply(indx)%IrigFrac
        SupplyActual_Urb = MAX(0.0 , SupplyActual - SupplyActual_Ag)
        DO indxDest=1,pServedDests%nDest
          iDest             = pServedDests%iDests(indxDest)
          Supply_Ag(iDest)  = Supply_Ag(iDest)  + SupplyActual_Ag * pServedDests%SupplyToDestFracs_Ag(indxDest)
          Supply_Urb(iDest) = Supply_Urb(iDest) + SupplyActual_Urb * pServedDests%SupplyToDestFracs_Urb(indxDest)
        END DO

      END ASSOCIATE
    END DO
  
  END SUBROUTINE Supply_GetSupply
  
  
  ! -------------------------------------------------------------
  ! --- GET FLAGS FOR SUPPLIES IF THEY SERVE AG, URBAN OR BOTH BASED ON INITIAL READINGS OF INPUT DATA (BEFORE ANY ADJUSTMENT) 
  ! -------------------------------------------------------------
  SUBROUTINE Supply_GetPurpose(Supply,iAgOrUrban,iStat)
    CLASS(SupplyType),INTENT(IN) :: Supply(:)
    INTEGER,INTENT(OUT)          :: iAgOrUrban(:),iStat
    
    !Local variables
    INTEGER :: indxSupply
    
    iStat = 0
    
    DO indxSupply=1,SIZE(Supply)
        IF (Supply(indxSupply)%IrigFracRead .EQ. 1.0) THEN
            iAgOrUrban(indxSupply) = 10
        ELSEIF (Supply(indxSupply)%IrigFracRead .EQ. 0.0) THEN
            iAgOrUrban(indxSupply) = 01
        ELSE
            iAgOrUrban(indxSupply) = 11
        END IF
    END DO
    
  END SUBROUTINE Supply_GetPurpose
  
  
  
  
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
  ! --- SET SUPPLY SPECS
  ! -------------------------------------------------------------
  SUBROUTINE Supply_SetSupplySpecs(Supply,SupplyDestConnector,SupplyRequired,IrigFracs,SupplyToDest)
    CLASS(SupplyType)                        :: Supply(:)
    TYPE(SupplyDestinationConnectorType)     :: SupplyDestConnector
    REAL(8),INTENT(IN)                       :: SupplyRequired(:),IrigFracs(:)
    TYPE(SupplyToDestinationType),INTENT(IN) :: SupplyToDest(:)
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(Supply)
        Supply(indx)%SupplyRequired                                         = SupplyRequired(indx)
        Supply(indx)%SupplyActual                                           = SupplyRequired(indx)
        Supply(indx)%IrigFrac                                               = IrigFracs(indx)
        SupplyDestConnector%SupplyToDestination(indx)%SupplyToDestFracs_Ag  = SupplyToDest(indx)%SupplyToDestFracs_Ag
        SupplyDestConnector%SupplyToDestination(indx)%SupplyToDestFracs_Urb = SupplyToDest(indx)%SupplyToDestFracs_Urb
    END DO
    
  END SUBROUTINE Supply_SetSupplySpecs
  
  
  ! -------------------------------------------------------------
  ! --- SET SUPPLY IRRIGATION FRACTIONS
  ! -------------------------------------------------------------
  SUBROUTINE Supply_SetIrigFracsRead(Supply,IrigFrac)
    CLASS(SupplyType)  :: Supply(:)
    REAL(8),INTENT(IN) :: IrigFrac(:)
    
    !Local variables
    INTEGER :: indx,iColIrigFrac
    
    DO indx=1,SIZE(Supply)
      iColIrigFrac = Supply(indx)%iColIrigFrac
      IF (iColIrigFrac .GT. 0) THEN
        Supply(indx)%IrigFracRead = IrigFrac(iColIrigFrac)
        Supply(indx)%IrigFrac     = IrigFrac(iColIrigFrac)
      END IF
    END DO
    
  END SUBROUTINE Supply_SetIrigFracsRead
  
  
  
  
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
  ! --- INITIALIZE AG AND URBAN FRACTION OF SUPPLIES THAT GO TO EACH DESTINATION
  ! -------------------------------------------------------------
  SUBROUTINE InitSupplyToAgUrbanFracs(Connector,AgDemand,AgArea,UrbDemand,UrbArea)
    CLASS(SupplyDestinationConnectorType) :: Connector
    REAL(8),INTENT(IN)                    :: AgDemand(:),AgArea(:),UrbDemand(:),UrbArea(:)
    
    !Local variables
    INTEGER :: indxSupply
    REAL(8) :: Demand_Total,Area_Total
    
    DO indxSupply=1,Connector%NSupply
        !Update only if supply serves more than one location
        IF (Connector%SupplyToDestination(indxSupply)%nDest .LE. 1) CYCLE
        
        !Ag supply fractions
        Demand_Total  = SUM(AgDemand(Connector%SupplyToDestination(indxSupply)%iDests))
        IF (Demand_Total .GT. 0.0) THEN
            Connector%SupplyToDestination(indxSupply)%SupplyToDestFracs_Ag = AgDemand(Connector%SupplyToDestination(indxSupply)%iDests) / Demand_Total
        ELSE
            Area_Total = SUM(AgArea(Connector%SupplyToDestination(indxSupply)%iDests))
            IF (Area_Total .GT. 0.0) THEN
                Connector%SupplyToDestination(indxSupply)%SupplyToDestFracs_Ag = AgArea(Connector%SupplyToDestination(indxSupply)%iDests) / Area_Total
            ELSE
                Connector%SupplyToDestination(indxSupply)%SupplyToDestFracs_Ag = 1.0 / REAL(Connector%SupplyToDestination(indxSupply)%nDest , 8)
            END IF
        END IF
        
        !Urban supply fractions
        Demand_Total  = SUM(UrbDemand(Connector%SupplyToDestination(indxSupply)%iDests))
        IF (Demand_Total .GT. 0.0) THEN
            Connector%SupplyToDestination(indxSupply)%SupplyToDestFracs_Urb = UrbDemand(Connector%SupplyToDestination(indxSupply)%iDests) / Demand_Total
        ELSE
            Area_Total = SUM(UrbArea(Connector%SupplyToDestination(indxSupply)%iDests))
            IF (Area_Total .GT. 0.0) THEN
                Connector%SupplyToDestination(indxSupply)%SupplyToDestFracs_Urb = UrbArea(Connector%SupplyToDestination(indxSupply)%iDests) / Area_Total
            ELSE
                Connector%SupplyToDestination(indxSupply)%SupplyToDestFracs_Urb = 1.0 / REAL(Connector%SupplyToDestination(indxSupply)%nDest , 8)
            END IF
        END IF
    END DO
    
  END SUBROUTINE InitSupplyToAgUrbanFracs
  
  
  ! -------------------------------------------------------------
  ! --- RESET IRRIGATION FRACTIONS TO THOSE READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE Supply_ResetIrigFracs(Supply)
    CLASS(SupplyType) :: Supply(:)
    
    !Local variables
    INTEGER :: indx
    
    DO indx=1,SIZE(Supply)
        Supply(indx)%IrigFrac = Supply(indx)%IrigFracRead
    END DO

  END SUBROUTINE Supply_ResetIrigFracs  
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE SUPPLY TO MEET DEMAND GOES TO DESIRED DESTINATION
  ! -------------------------------------------------------------
  SUBROUTINE Supply_CheckSupplyDestinationConnection(Supply,SupplyDestConnector,cSupplyDescription,iStat)
    CLASS(SupplyType),INTENT(IN)                    :: Supply(:)
    TYPE(SupplyDestinationConnectorType),INTENT(IN) :: SupplyDestConnector
    CHARACTER(LEN=*),INTENT(IN)                     :: cSupplyDescription
    INTEGER,INTENT(OUT)                             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+42) :: ThisProcedure = ModName // 'Supply_CheckSupplyDestinationConnection'
    INTEGER                      :: indx
    REAL(8)                      :: Supply_Ag,Supply_Urb,rFrac,IrigFrac
    
    !Initialize
    iStat = 0
    
    DO indx=1,SIZE(Supply)
      ASSOCIATE (pSupply    => Supply(indx)                                  , &
                 pConnector => SupplyDestConnector%SupplyToDestination(indx) )
      
        IF (pConnector%nDest .EQ. 0) CYCLE
        IF (pSupply%SupplyActual .GE. 0.0) CYCLE
        
        !Decide if IrigFrac is close to 1 (due to round-off errors)
        IF (ABS(1d0-pSupply%IrigFrac) .LT. 1d-3) THEN
          IrigFrac = 1.0
        ELSE
          IrigFrac = pSupply%IrigFrac
        END IF
      
        !Make sure all ag water supply goes to desired destinations
        Supply_Ag = -pSupply%SupplyActual * IrigFrac
        IF (Supply_Ag .GT. 0.0) THEN
            rFrac = SUM(pConnector%SupplyToDestFracs_Ag)
            IF (ABS(1d0-rFrac) .GT. 1d-2) THEN
                CALL SetLastMessage('Not all agricultural water supply for '//TRIM(cSupplyDescription)//' '//TRIM(IntToText(indx))//' is going to the desired demand location!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Make sure all urban water supply goes to elements
        Supply_Urb = -pSupply%SupplyActual - Supply_Ag
        IF (Supply_Urb .GT. 0.0) THEN
            rFrac = SUM(pConnector%SupplyToDestFracs_Urb)
            IF (ABS(1d0-rFrac) .GT. 1d-2) THEN
                CALL SetLastMessage('Not all urban water supply for '//TRIM(cSupplyDescription)//' '//TRIM(IntToText(indx))//' is going to the desired demand location!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF

      END ASSOCIATE
    END DO
    
  END SUBROUTINE Supply_CheckSupplyDestinationConnection
    
END MODULE