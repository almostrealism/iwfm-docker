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
MODULE StrmLakeConnector
  USE MessageLogger    , ONLY: SetLastMessage , &
                               MessageArray   , &
                               iFatal
  USE GeneralUtilities
  USE IOInterface
  USE Package_Matrix
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
  PUBLIC :: StrmLakeConnectorType  , &
            iStrmToLakeType        , &
            iBypassToLakeType      , &
            iLakeToStrmType   


  ! -------------------------------------------------------------
  ! --- INDIVIDUAL STREAM-LAKE CONNECTION DATA TYPE
  ! -------------------------------------------------------------
  TYPE SingleStrmLakeConnectorType
      PRIVATE
      INTEGER :: SourceID      = 0
      INTEGER :: DestinationID = 0
      REAL(8) :: Flow          = 0.0
  END TYPE SingleStrmLakeConnectorType

  
  ! -------------------------------------------------------------
  ! --- STREAM-LAKE CONNECTOR DATABASE TYPE
  ! -------------------------------------------------------------
  TYPE StrmLakeConnectorType
      PRIVATE
      INTEGER                                       :: NStrmToLake   = 0
      INTEGER                                       :: NBypassToLake = 0
      INTEGER                                       :: NLakeToStrm   = 0
      TYPE(SingleStrmLakeConnectorType),ALLOCATABLE :: StrmToLake(:)
      TYPE(SingleStrmLakeConnectorType),ALLOCATABLE :: BypassToLake(:)
      TYPE(SingleStrmLakeConnectorType),ALLOCATABLE :: LakeToStrm(:)
  CONTAINS
      PROCEDURE,PASS :: New     => ReadFromBinFile                    
      PROCEDURE,PASS :: AddData                
      PROCEDURE,PASS :: Kill                   
      PROCEDURE,PASS :: GetFlow                
      PROCEDURE,PASS :: GetSourceIDs           
      PROCEDURE,PASS :: GetDestinationIDs      
      PROCEDURE,PASS :: SetFlow                
      PROCEDURE,PASS :: WritePreprocesssedData 
      PROCEDURE,PASS :: RegisterWithMatrix      
      PROCEDURE,PASS :: ResetLakeToStrmFlows   
      PROCEDURE,PASS :: ResetStrmToLakeFlows
      PROCEDURE,PASS :: CheckForErrors
  END TYPE StrmLakeConnectorType


  ! -------------------------------------------------------------
  ! --- CONNECTION TYPE FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iStrmToLakeType   = 1 , &
                       iBypassToLakeType = 2 , &
                       iLakeToStrmType   = 3
  
  
  ! -------------------------------------------------------------
  ! --- MISC. DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 19
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'StrmLakeConnector::'




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
  ! --- ADD STREAM-TO-LAKE CONNECTION TO THE LIST
  ! -------------------------------------------------------------
  SUBROUTINE AddData(Connector,iConnectionType,SourceID,DestinationID)
    CLASS(StrmLakeConnectorType),TARGET :: Connector
    INTEGER,INTENT(IN)                  :: iConnectionType,SourceID,DestinationID
    
    !Local variables
    INTEGER,POINTER                               :: pNConnections
    TYPE(SingleStrmLakeConnectorType),ALLOCATABLE :: TempConnections(:)
    TYPE(SingleStrmLakeConnectorType),POINTER     :: pConnections(:)
    
    !Initialize
    SELECT CASE (iConnectionType)
        CASE (iStrmToLakeType)
          pNConnections => Connector%NStrmToLake
          pConnections  => Connector%StrmToLake
          
        CASE (iBypassToLakeType)
          pNConnections => Connector%NBypassToLake
          pConnections  => Connector%BypassToLake
          
        CASE (iLakeToStrmType)
          pNConnections => Connector%NLakeToStrm
          pConnections  => Connector%LakeToStrm
            
    END SELECT

    !Allocate temporary memory
    ALLOCATE (TempConnections(pNConnections+1))
    
    !Move currently stored data to temporary array
    TempConnections(1:pNConnections) = pConnections
    
    !Add new data
    pNConnections                                = pNConnections + 1
    TempConnections(pNConnections)%SourceID      = SourceID
    TempConnections(pNConnections)%DestinationID = DestinationID
        
    !Store data in permanent storage
    SELECT CASE (iConnectionType)
        CASE (iStrmToLakeType)
          CALL MOVE_ALLOC(TempConnections , Connector%StrmToLake)
          
        CASE (iBypassToLakeType)
          CALL MOVE_ALLOC(TempConnections , Connector%BypassToLake)
          
        CASE (iLakeToStrmType)
          CALL MOVE_ALLOC(TempConnections , Connector%LakeToStrm)
            
    END SELECT
    
  END SUBROUTINE AddData
  
 
  ! -------------------------------------------------------------
  ! --- INSTANTIATE STREAM-TO-LAKE CONNECTION FROM PRE-PROCESSOR BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadFromBinFile(Connector,BinFile,iStat) 
    CLASS(StrmLakeConnectorType),INTENT(OUT) :: Connector
    TYPE(GenericFileType)                    :: BinFile
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER :: NStrmToLake,NBypassToLake,NLakeToStrm
    
    CALL BinFile%ReadData(NStrmToLake,iStat)  ;  IF (iStat .EQ. -1) RETURN
    Connector%NStrmToLake = NStrmToLake
    IF (NStrmToLake .GT. 0) THEN
        ALLOCATE (Connector%StrmToLake(NStrmToLake))
        CALL BinFile%ReadData(Connector%StrmToLake%SourceID,iStat)       ;  IF (iStat .EQ. -1) RETURN
        CALL BinFile%ReadData(Connector%StrmToLake%DestinationID,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
    
    CALL BinFile%ReadData(NBypassToLake,iStat)  ;  IF (iStat .EQ. -1) RETURN
    Connector%NBypassToLake = NBypassToLake
    IF (NBypassToLake .GT. 0) THEN
        ALLOCATE (Connector%BypassToLake(NBypassToLake))
        CALL BinFile%ReadData(Connector%BypassToLake%SourceID,iStat)       ;  IF (iStat .EQ. -1) RETURN
        CALL BinFile%ReadData(Connector%BypassToLake%DestinationID,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF

    CALL BinFile%ReadData(NLakeToStrm,iStat)  ;  IF (iStat .EQ. -1) RETURN
    Connector%NLakeToStrm = NLakeToStrm
    IF (NLakeToStrm .GT. 0) THEN
        ALLOCATE (Connector%LakeToStrm(NLakeToStrm))
        CALL BinFile%ReadData(Connector%LakeToStrm%SourceID,iStat)       ;  IF (iStat .EQ. -1) RETURN
        CALL BinFile%ReadData(Connector%LakeToStrm%DestinationID,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
    
  END SUBROUTINE ReadFromBinFile
  
  

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
  ! --- KILL AN OBJECT OF THE CLASS
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Connector)
    CLASS(StrmLakeConnectorType) :: Connector
    
    !Local variables
    INTEGER                     :: ErrorCode
    TYPE(StrmLakeConnectorType) :: Dummy
    
    DEALLOCATE (Connector%StrmToLake , Connector%BypassToLake  , Connector%LakeToStrm  , STAT=ErrorCode)
    Connector%NStrmToLake   = Dummy%NStrmToLake
    Connector%NBypassToLake = Dummy%NBypassToLake
    Connector%NLakeToStrm   = Dummy%NLakeToStrm
    Connector%NStrmToLake   = Dummy%NStrmToLake
    
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
  ! --- GET DESTINATION IDs FOR A CONNECTION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetDestinationIDs(Connector,iConnectionType,iDestinationIDs)
    CLASS(StrmLakeConnectorType),TARGET,INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                             :: iConnectionType
    INTEGER,ALLOCATABLE,INTENT(OUT)                :: iDestinationIDs(:)
    
    !Local variables
    INTEGER,POINTER                           :: pNConnections
    TYPE(SingleStrmLakeConnectorType),POINTER :: pConnections(:)
    
    !Initialize
    SELECT CASE (iConnectionType)
        CASE (iStrmToLakeType)
            pNConnections => Connector%NStrmToLake
            pConnections  => Connector%StrmToLake
        
        CASE (iBypassToLakeType)
            pNConnections => Connector%NBypassToLake
            pConnections  => Connector%BypassToLake
            
        CASE (iLakeToStrmType)
            pNConnections => Connector%NLakeToStrm
            pConnections  => Connector%LakeToStrm
               
    END SELECT
        
    !Allocate and return data
    ALLOCATE (iDestinationIDs(pNConnections))
    idestinationIDs = pConnections%DestinationID
    
  END SUBROUTINE GetDestinationIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET SOURCE IDs FOR A CONNECTION TYPE
  ! -------------------------------------------------------------
  SUBROUTINE GetSourceIDs(Connector,iConnectionType,iSourceIDs)
    CLASS(StrmLakeConnectorType),TARGET,INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                             :: iConnectionType
    INTEGER,ALLOCATABLE,INTENT(OUT)                :: iSourceIDs(:)
    
    !Local variables
    INTEGER,POINTER                           :: pNConnections
    TYPE(SingleStrmLakeConnectorType),POINTER :: pConnections(:)
    
    !Initialize
    SELECT CASE (iConnectionType)
        CASE (iStrmToLakeType)
            pNConnections => Connector%NStrmToLake
            pConnections  => Connector%StrmToLake
        
        CASE (iBypassToLakeType)
            pNConnections => Connector%NBypassToLake
            pConnections  => Connector%BypassToLake
            
        CASE (iLakeToStrmType)
            pNConnections => Connector%NLakeToStrm
            pConnections  => Connector%LakeToStrm
               
    END SELECT
        
    !Allocate and return data
    ALLOCATE (iSourceIDs(pNConnections))
    iSourceIDs = pConnections%SourceID
    
  END SUBROUTINE GetSourceIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW FROM LAKES INTO A STREAM NODE
  ! -------------------------------------------------------------
  FUNCTION GetFlow(Connector,iConnectionType,DestinationID) RESULT(Flow)
    CLASS(StrmLakeConnectorType),TARGET,INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                             :: iConnectionType,DestinationID
    REAL(8)                                        :: Flow

    !Local variables
    TYPE(SingleStrmLakeConnectorType),POINTER :: pConnections(:)
    
    !Initialize
    Flow = 0.0
    SELECT CASE (iConnectionType)
        CASE (iStrmToLakeType)
            pConnections => Connector%StrmToLake
        
        CASE (iBypassToLakeType)
            pConnections => Connector%BypassToLake
            
        CASE (iLakeToStrmType)
            pConnections => Connector%LakeToStrm
               
    END SELECT

    !Sum the flows from all sources for the given type to specified destination
    Flow = SUM(pConnections%Flow , MASK=pConnections%DestinationID .EQ. DestinationID)

  END FUNCTION GetFlow

  
  
  
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
  ! --- SET FLOW FROM SOURCE TO DESTINATION
  ! -------------------------------------------------------------
  SUBROUTINE SetFlow(Connector,iConnectionType,SourceID,DestinationID,Flow)
    CLASS(StrmLakeConnectorType),TARGET :: Connector
    INTEGER,INTENT(IN)                  :: iConnectionType,SourceID,DestinationID
    REAL(8),INTENT(IN)                  :: Flow

    !Local variables
    INTEGER                                   :: indx
    TYPE(SingleStrmLakeConnectorType),POINTER :: pConnections(:)
    
    !Initialize
    SELECT CASE (iConnectionType)
        CASE (iStrmToLakeType)
            pConnections => Connector%StrmToLake
        
        CASE (iBypassToLakeType)
            pConnections => Connector%BypassToLake
            
        CASE (iLakeToStrmType)
            pConnections => Connector%LakeToStrm
               
    END SELECT

    !Find the connection that has matching source and destination IDs
    DO indx=1,SIZE(pConnections)
        IF (pConnections(indx)%SourceID .EQ. SourceID) THEN
            IF (pConnections(indx)%DestinationID .EQ. DestinationID) THEN
                pConnections(indx)%Flow = pConnections(indx)%Flow + Flow
                EXIT
            END IF
        END IF
    END DO   

  END SUBROUTINE SetFlow



    
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
  ! --- WRITE PRE-PROCESSED DATA TO BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE WritePreprocesssedData(Connector,BinFile)
    CLASS(StrmLakeConnectorType),INTENT(IN) :: Connector
    TYPE(GenericFileType)                   :: BinFile
    
    CALL BinFile%WriteData(Connector%NStrmToLake)
    IF (Connector%NStrmToLake .GT. 0) THEN
        CALL BinFile%WriteData(Connector%StrmToLake%SourceID)
        CALL BinFile%WriteData(Connector%StrmToLake%DestinationID)
    END IF
    
    CALL BinFile%WriteData(Connector%NBypassToLake)
    IF (Connector%NBypassToLake .GT. 0) THEN
        CALL BinFile%WriteData(Connector%BypassToLake%SourceID)
        CALL BinFile%WriteData(Connector%BypassToLake%DestinationID)
    END IF

    CALL BinFile%WriteData(Connector%NLakeToStrm)
    IF (Connector%NLakeToStrm .GT. 0) THEN
        CALL BinFile%WriteData(Connector%LakeToStrm%SourceID)
        CALL BinFile%WriteData(Connector%LakeToStrm%DestinationID)
    END IF
    
  END SUBROUTINE WritePreprocesssedData
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC.METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- RESET LAKE-TO-STREAM FLOWS 
  ! -------------------------------------------------------------
  SUBROUTINE ResetLakeToStrmFlows(Connector)
    CLASS(StrmLakeConnectorType) :: Connector
    
    Connector%LakeToStrm%Flow   = 0.0
    
  END SUBROUTINE ResetLakeToStrmFlows


  ! -------------------------------------------------------------
  ! --- RESET STREAM-TO-LAKE FLOWS 
  ! -------------------------------------------------------------
  SUBROUTINE ResetStrmToLakeFlows(Connector)
    CLASS(StrmLakeConnectorType) :: Connector
    
    Connector%StrmToLake%Flow   = 0.0
    Connector%BypassToLake%Flow = 0.0
    
  END SUBROUTINE ResetStrmToLakeFlows
  
  
  ! -------------------------------------------------------------
  ! --- ADD CONNECTIVITY TO MATRIX
  ! --- Note: This is currently a placeholder in case we need to add the connectivity to matrix
  ! -------------------------------------------------------------
  SUBROUTINE RegisterWithMatrix(Connector,Matrix)
    CLASS(StrmLakeConnectorType),INTENT(IN) :: Connector
    TYPE(MatrixType)                        :: Matrix
    
    !Do nothing for now; this procedure is currently a placeholder only
    
  END SUBROUTINE RegisterWithMatrix


  ! -------------------------------------------------------------
  ! --- CHECK FOR ERRORS
  ! -------------------------------------------------------------
  SUBROUTINE CheckForErrors(Connector,NLakes,NStrmNodes,iStat)
    CLASS(StrmLakeConnectorType),INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                      :: NLakes,NStrmNodes
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local varibles
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'CheckForErrors'
    INTEGER                      :: indx,SourceID,DestID
    
    !Initialize
    iStat = 0
    
    !Make sure that referenced lake and stream node IDs are simulated
    ASSOCIATE (pStrmToLake   => Connector%StrmToLake   , &
               pBypassToLake => Connector%BypassToLake , &
               pLakeToStrm   => Connector%LakeToStrm   )
        !Check stream-lake connection
        DO indx=1,Connector%NStrmToLake
            SourceID = pStrmToLake(indx)%SourceID
            DestID   = pStrmToLake(indx)%DestinationID
            IF (SourceID.LT.1  .OR.  SourceID.GT.NStrmNodes) THEN
                MessageArray(1) = 'Stream node number '//TRIM(IntToText(SourceID))//' that flows into '
                MessageArray(2) = 'lake ID '//TRIM(IntToText(DestID))//' is not simulated!' 
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
            IF (DestID.LT.1  .OR.  DestID.GT.NLakes) THEN
                MessageArray(1) = 'Lake ID '//TRIM(IntToText(DestID))//' that receives flow from '
                MessageArray(2) = 'stream node number '//TRIM(IntToText(SourceID))//' is not simulated!' 
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
        END DO
        
        !Check bypass-lake connection
        DO indx=1,Connector%NBypassToLake
            SourceID = pBypassToLake(indx)%SourceID
            DestID   = pBypassToLake(indx)%DestinationID
            IF (SourceID.LT.1  .OR.  SourceID.GT.NStrmNodes) THEN
                MessageArray(1) = 'Stream node number '//TRIM(IntToText(SourceID))//' from which a bypass originates and'
                MessageArray(2) = 'flows into lake ID '//TRIM(IntToText(DestID))//' is not simulated!' 
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
            IF (DestID.LT.1  .OR.  DestID.GT.NLakes) THEN
                MessageArray(1) = 'Lake ID '//TRIM(IntToText(DestID))//' that receives flow from stream node'
                MessageArray(2) = 'number '//TRIM(IntToText(SourceID))//' by means of a bypass is not simulated!' 
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
        END DO

        !Check lake-stream connection
        DO indx=1,Connector%NLakeToStrm
            SourceID = pLakeToStrm(indx)%SourceID
            DestID   = pLakeToStrm(indx)%DestinationID
            IF (SourceID.LT.1  .OR.  SourceID.GT.NLakes) THEN
                MessageArray(1) = 'Lake ID '//TRIM(IntToText(SourceID))//' which flows into stream'
                MessageArray(2) = 'node number '//TRIM(IntToText(DestID))//' is not simulated!' 
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
            IF (DestID.LT.1  .OR.  DestID.GT.NStrmNodes) THEN
                MessageArray(1) = 'Stream node number '//TRIM(IntToText(DestID))//' that receives flow '
                MessageArray(2) = 'from lake ID '//TRIM(IntToText(SourceID))//' is not simulated!' 
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
        END DO

    END ASSOCIATE
    
  END SUBROUTINE CheckForErrors

END MODULE
