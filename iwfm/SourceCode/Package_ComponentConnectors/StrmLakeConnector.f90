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
MODULE StrmLakeConnector
  USE MessageLogger      , ONLY: SetLastMessage   , &
                                 MessageArray     , &
                                 f_iFatal
  USE GeneralUtilities   , ONLY: LocateInList     , &
                                 IntToText
  USE IOInterface        , ONLY: GenericFileType
  USE Package_Matrix     , ONLY: MatrixType
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
            f_iStrmToLakeFlow      , &
            f_iBypassToLakeFlow    , &
            f_iLakeToStrmFlow   


  ! -------------------------------------------------------------
  ! --- INDIVIDUAL STREAM-LAKE CONNECTION DATA TYPE
  ! -------------------------------------------------------------
  TYPE SingleStrmLakeConnectorType
      PRIVATE
      INTEGER :: iSource      = 0
      INTEGER :: iDestination = 0
      REAL(8) :: Flow         = 0.0
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
      PROCEDURE,PASS :: IDs_To_Indices
  END TYPE StrmLakeConnectorType


  ! -------------------------------------------------------------
  ! --- CONNECTION TYPE FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iStrmToLakeFlow   = 1 , &
                       f_iBypassToLakeFlow = 2 , &
                       f_iLakeToStrmFlow   = 3
  
  
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
  SUBROUTINE AddData(Connector,iConnectionType,iSource,iDestinationID)
    CLASS(StrmLakeConnectorType),TARGET :: Connector
    INTEGER,INTENT(IN)                  :: iConnectionType,iSource,iDestinationID
    
    !Local variables
    INTEGER,POINTER                               :: pNConnections
    TYPE(SingleStrmLakeConnectorType),ALLOCATABLE :: TempConnections(:)
    TYPE(SingleStrmLakeConnectorType),POINTER     :: pConnections(:)
    
    !Initialize
    SELECT CASE (iConnectionType)
        CASE (f_iStrmToLakeFlow)
          pNConnections => Connector%NStrmToLake
          pConnections  => Connector%StrmToLake
          
        CASE (f_iBypassToLakeFlow)
          pNConnections => Connector%NBypassToLake
          pConnections  => Connector%BypassToLake
          
        CASE (f_iLakeToStrmFlow)
          pNConnections => Connector%NLakeToStrm
          pConnections  => Connector%LakeToStrm
            
    END SELECT

    !Allocate temporary memory
    ALLOCATE (TempConnections(pNConnections+1))
    
    !Move currently stored data to temporary array
    TempConnections(1:pNConnections) = pConnections
    
    !Add new data
    pNConnections                               = pNConnections + 1
    TempConnections(pNConnections)%iSource      = iSource
    TempConnections(pNConnections)%iDestination = iDestinationID
        
    !Store data in permanent storage
    SELECT CASE (iConnectionType)
        CASE (f_iStrmToLakeFlow)
          CALL MOVE_ALLOC(TempConnections , Connector%StrmToLake)
          
        CASE (f_iBypassToLakeFlow)
          CALL MOVE_ALLOC(TempConnections , Connector%BypassToLake)
          
        CASE (f_iLakeToStrmFlow)
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
        CALL BinFile%ReadData(Connector%StrmToLake%iSource,iStat)       ;  IF (iStat .EQ. -1) RETURN
        CALL BinFile%ReadData(Connector%StrmToLake%iDestination,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
    
    CALL BinFile%ReadData(NBypassToLake,iStat)  ;  IF (iStat .EQ. -1) RETURN
    Connector%NBypassToLake = NBypassToLake
    IF (NBypassToLake .GT. 0) THEN
        ALLOCATE (Connector%BypassToLake(NBypassToLake))
        CALL BinFile%ReadData(Connector%BypassToLake%iSource,iStat)       ;  IF (iStat .EQ. -1) RETURN
        CALL BinFile%ReadData(Connector%BypassToLake%iDestination,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF

    CALL BinFile%ReadData(NLakeToStrm,iStat)  ;  IF (iStat .EQ. -1) RETURN
    Connector%NLakeToStrm = NLakeToStrm
    IF (NLakeToStrm .GT. 0) THEN
        ALLOCATE (Connector%LakeToStrm(NLakeToStrm))
        CALL BinFile%ReadData(Connector%LakeToStrm%iSource,iStat)       ;  IF (iStat .EQ. -1) RETURN
        CALL BinFile%ReadData(Connector%LakeToStrm%iDestination,iStat)  ;  IF (iStat .EQ. -1) RETURN
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
        CASE (f_iStrmToLakeFlow)
            pNConnections => Connector%NStrmToLake
            pConnections  => Connector%StrmToLake
        
        CASE (f_iBypassToLakeFlow)
            pNConnections => Connector%NBypassToLake
            pConnections  => Connector%BypassToLake
            
        CASE (f_iLakeToStrmFlow)
            pNConnections => Connector%NLakeToStrm
            pConnections  => Connector%LakeToStrm
               
    END SELECT
        
    !Allocate and return data
    ALLOCATE (iDestinationIDs(pNConnections))
    iDestinationIDs = pConnections%iDestination
    
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
        CASE (f_iStrmToLakeFlow)
            pNConnections => Connector%NStrmToLake
            pConnections  => Connector%StrmToLake
        
        CASE (f_iBypassToLakeFlow)
            pNConnections => Connector%NBypassToLake
            pConnections  => Connector%BypassToLake
            
        CASE (f_iLakeToStrmFlow)
            pNConnections => Connector%NLakeToStrm
            pConnections  => Connector%LakeToStrm
               
    END SELECT
        
    !Allocate and return data
    ALLOCATE (iSourceIDs(pNConnections))
    iSourceIDs = pConnections%iSource
    
  END SUBROUTINE GetSourceIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET FLOW FROM LAKES INTO A STREAM NODE
  ! -------------------------------------------------------------
  FUNCTION GetFlow(Connector,iConnectionType,iDestination) RESULT(Flow)
    CLASS(StrmLakeConnectorType),TARGET,INTENT(IN) :: Connector
    INTEGER,INTENT(IN)                             :: iConnectionType,iDestination
    REAL(8)                                        :: Flow

    !Local variables
    TYPE(SingleStrmLakeConnectorType),POINTER :: pConnections(:)
    
    !Initialize
    Flow = 0.0
    SELECT CASE (iConnectionType)
        CASE (f_iStrmToLakeFlow)
            pConnections => Connector%StrmToLake
        
        CASE (f_iBypassToLakeFlow)
            pConnections => Connector%BypassToLake
            
        CASE (f_iLakeToStrmFlow)
            pConnections => Connector%LakeToStrm
               
    END SELECT

    !Sum the flows from all sources for the given type to specified destination
    Flow = SUM(pConnections%Flow , MASK=pConnections%iDestination .EQ. iDestination)

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
        CASE (f_iStrmToLakeFlow)
            pConnections => Connector%StrmToLake
        
        CASE (f_iBypassToLakeFlow)
            pConnections => Connector%BypassToLake
            
        CASE (f_iLakeToStrmFlow)
            pConnections => Connector%LakeToStrm
               
    END SELECT

    !Find the connection that has matching source and destination IDs
    DO indx=1,SIZE(pConnections)
        IF (pConnections(indx)%iSource .EQ. SourceID) THEN
            IF (pConnections(indx)%iDestination .EQ. DestinationID) THEN
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
        CALL BinFile%WriteData(Connector%StrmToLake%iSource)
        CALL BinFile%WriteData(Connector%StrmToLake%iDestination)
    END IF
    
    CALL BinFile%WriteData(Connector%NBypassToLake)
    IF (Connector%NBypassToLake .GT. 0) THEN
        CALL BinFile%WriteData(Connector%BypassToLake%iSource)
        CALL BinFile%WriteData(Connector%BypassToLake%iDestination)
    END IF

    CALL BinFile%WriteData(Connector%NLakeToStrm)
    IF (Connector%NLakeToStrm .GT. 0) THEN
        CALL BinFile%WriteData(Connector%LakeToStrm%iSource)
        CALL BinFile%WriteData(Connector%LakeToStrm%iDestination)
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
  ! --- CONVERT IDs TO INDICES
  ! -------------------------------------------------------------
  SUBROUTINE IDs_To_Indices(Connector,NLakes,NStrmNodes,iStrmNodeIDs,iLakeIDs,iStat)
    CLASS(StrmLakeConnectorType) :: Connector
    INTEGER,INTENT(IN)           :: NLakes,NStrmNodes,iStrmNodeIDs(NStrmNodes),iLakeIDs(NLakes)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local varibles
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'IDs_To_Indices'
    INTEGER                      :: indx,iSource,iDestID,iDest,iSourceID
    
    !Initialize
    iStat = 0
    
    !Make sure that referenced lake and stream node IDs are simulated
    !Note: Source IDs are converted to indices during the component instantiation; only convert destination IDs
    ASSOCIATE (pStrmToLake   => Connector%StrmToLake   , &
               pBypassToLake => Connector%BypassToLake , &
               pLakeToStrm   => Connector%LakeToStrm   )
        !Stream-lake connection
        DO indx=1,Connector%NStrmToLake
            iSource = pStrmToLake(indx)%iSource
            iDestID = pStrmToLake(indx)%iDestination
            iDest   = LocateInList(iDestID,iLakeIDs)
            IF (iDest .EQ. 0) THEN
                iSourceID = iStrmNodeIDs(iSource)
                CALL SetLastMessage('Lake '//TRIM(IntToText(iDestID))//' that receives flow from stream node '//TRIM(IntToText(iSourceID))//' is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF  
            pStrmToLake(indx)%iDestination = iDest
        END DO
        
        !Bypass-lake connection
        DO indx=1,Connector%NBypassToLake
            iSource = pBypassToLake(indx)%iSource
            iDestID = pBypassToLake(indx)%iDestination
            iDest   = LocateInList(iDestID,iLakeIDs)
            IF (iDest .EQ. 0) THEN
                iSourceID = iStrmNodeIDs(iSource)
                CALL SetLastMessage('Lake '//TRIM(IntToText(iDestID))//' that receives flow from stream node '//TRIM(IntToText(iSourceID))//' by means of a bypass is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
            pBypassToLake(indx)%iDestination = iDest
        END DO

        !Lake-stream connection
        DO indx=1,Connector%NLakeToStrm
            iSource = pLakeToStrm(indx)%iSource
            iDestID = pLakeToStrm(indx)%iDestination
            iDest   = LocateInList(iDestID,iStrmNodeIDs)
            IF (iDest .EQ. 0) THEN
                iSourceID = iLakeIDs(iSource)
                CALL SetLastMessage('Stream node '//TRIM(IntToText(iDestID))//' that receives flow from lake '//TRIM(IntToText(iSourceID))//' is not in the model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF    
            pLakeToStrm(indx)%iDestination = iDest
        END DO

    END ASSOCIATE
    
  END SUBROUTINE IDs_To_Indices

END MODULE
