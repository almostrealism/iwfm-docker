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
MODULE Package_Matrix
  !$ USE OMP_LIB
  USE Class_Version    , ONLY: VersionType
  USE MessageLogger    , ONLY: SetLastMessage           , &
                               MessageArray             , &
                               f_iFatal                   
  USE GeneralUtilities , ONLY: LocateInList             , &
                               IntToText                , &
                               GetUniqueArrayComponents , &
                               ShellSort
  USE IOInterface      , ONLY: GenericFileType
  USE Package_Misc     , ONLY: SolverDataType           , &
                               f_cCompNames
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
  PUBLIC :: MatrixType           , &
            ConnectivityListType
  
  
  ! -------------------------------------------------------------
  ! --- CONNECTIVITY TYPE
  ! -------------------------------------------------------------
  TYPE ConnectivityListType
      INTEGER             :: nConnectedNodes   = 0
      INTEGER,ALLOCATABLE :: ConnectedNodes(:)
  END TYPE ConnectivityListType
  
      
  ! -------------------------------------------------------------
  ! --- SOLVER DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(SolverDataType) :: SolverType
      INTEGER :: iSolver     = 0          !Solver to be used
      REAL(8) :: Relax       = 1.0        !Relaxation parameter to be used with SOR method
      LOGICAL :: ZeroReset   = .TRUE.     !Flag to see if the solution vector will be set to zero before the matrix equation is solved
  END TYPE SolverType
  
  
  ! -------------------------------------------------------------
  ! --- MATRIX DATA TYPE
  ! -------------------------------------------------------------
  TYPE MatrixType
      PRIVATE
      LOGICAL                                :: lConnectivityFlattened = .FALSE.   !Flag to check if ConnectivityList is flattened and saved in simple arrays of NJD and JND
      INTEGER                                :: nComps                 = 0         !Number of physical processes for which equations are solved 
      INTEGER,ALLOCATABLE                    :: iComps(:)                          !Physical process identifiers for which equations are being solved
      INTEGER,ALLOCATABLE                    :: iCompGlobalNodeStart(:)            !Starting global node number for the component
      INTEGER,ALLOCATABLE                    :: nCompNodes(:)                      !Number equations solved in each simulated physical process
      INTEGER,ALLOCATABLE                    :: NJD(:)                             !Array that holds data starting locations in JND and COEFF matrices that hold data for non-zero entries of the matrix
      INTEGER,ALLOCATABLE                    :: JND(:)                             !Column numbers of non-zero entries in matrix for each row
      INTEGER,ALLOCATABLE                    :: iIndexDiag(:)                      !Index in JND and COEFF arrays that store information for the diagonal of the matrix
      REAL(8),ALLOCATABLE                    :: COEFF(:)                           !Non-zero entries in the matrix for each row
      REAL(8),ALLOCATABLE                    :: RHS(:)                             !Right-hand-side vector of the matrix equation
      REAL(8),ALLOCATABLE,PUBLIC             :: HDelta(:)                          !Solution vector (represents the chnages in the state variable estimates)
      REAL(8),ALLOCATABLE,PUBLIC             :: RHSL2(:)                           !L2-norm of the RHS vector at each Newton-Raphson iteration
      TYPE(SolverType)                       :: Solver                             !Solver data to be used to invert matrix equation
      TYPE(ConnectivityListType),ALLOCATABLE :: ConnectivityList(:)                !Linked-list of connectivity at each global node
  CONTAINS
      PROCEDURE,PASS         :: AddComponent
      PROCEDURE,PASS,PRIVATE :: AddConnectivity_ToOne
      PROCEDURE,PASS,PRIVATE :: AddConnectivity_ToMany_Sequential
      PROCEDURE,PASS,PRIVATE :: AddConnectivity_ToMany_Random
      PROCEDURE,PASS         :: Kill
      PROCEDURE,PASS         :: FlattenConnectivity
      PROCEDURE,PASS         :: GetNJD
      PROCEDURE,PASS         :: GetJND
      PROCEDURE,PASS         :: GetConnectivityIndex
      PROCEDURE,PASS         :: GetConnectivitySize
      PROCEDURE,PASS         :: GetMaxIter
      PROCEDURE,PASS         :: GetRHSL2
      PROCEDURE,PASS,PRIVATE :: GetRHS_Component
      PROCEDURE,PASS         :: GetCompRowIndices
      PROCEDURE,PASS         :: GetMaxHDeltaNode_CompID
      PROCEDURE,NOPASS       :: GetVersion
      PROCEDURE,PASS         :: SetSolver
      PROCEDURE,PASS         :: SetRHS
      PROCEDURE,PASS,PRIVATE :: SetCOEFF_OneEntry
      PROCEDURE,PASS,PRIVATE :: SetCOEFF_RowToSameValue
      PROCEDURE,PASS,PRIVATE :: SetCOEFF_SomeValuesAtARow
      PROCEDURE,PASS,PRIVATE :: SetCOEFF_AllDiagonalsForComponent
      PROCEDURE,PASS         :: WritePreprocessedData
      PROCEDURE,PASS         :: Solve
      PROCEDURE,PASS         :: ResetToZero
      PROCEDURE,PASS         :: UpdateCOEFF
      PROCEDURE,PASS         :: PrintCOEFF_In_MatrixForm
      PROCEDURE,PASS,PRIVATE :: UpdateRHS_Random
      PROCEDURE,PASS,PRIVATE :: UpdateRHS_StartAtRow
      PROCEDURE,PASS         :: GlobalNode_to_LocalNode
      PROCEDURE,PASS         :: New             => ReadPreprocessedData
      GENERIC,PUBLIC         :: AddConnectivity => AddConnectivity_ToOne             , &
                                                   AddConnectivity_ToMany_Sequential , &
                                                   AddConnectivity_ToMany_Random
      GENERIC,PUBLIC         :: UpdateRHS       => UpdateRHS_Random                  , &
                                                   UpdateRHS_StartAtRow
      GENERIC,PUBLIC         :: SetCOEFF        => SetCOEFF_OneEntry                 , &
                                                   SetCOEFF_RowToSameValue           , &
                                                   SetCOEFF_SomeValuesAtARow         , &
                                                   SetCOEFF_AllDiagonalsForComponent
      GENERIC,PUBLIC         :: GetRHS          => GetRHS_Component                   
                                                   
  END TYPE MatrixType


  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------  
  !Convert local node number(s) to global node number(s)
  INTERFACE LocalNode_To_GlobalNode
    MODULE PROCEDURE LocalNode_To_GlobalNode_Array
    MODULE PROCEDURE LocalNode_To_GlobalNode_Array2
    MODULE PROCEDURE LocalNode_To_GlobalNode_Scalar
  END INTERFACE LocalNode_To_GlobalNode
  
  
  ! -------------------------------------------------------------
  ! --- VERSION RELATED ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '4.0.0000'
  INCLUDE 'Package_Matrix_Revision.fi'
  

  ! -------------------------------------------------------------
  ! --- SOLVER TYPES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iSolver_SOR    = 1  , &
                       iSolver_PGMRES = 2  , &
                       iSolverList(2) = [iSolver_SOR,iSolver_PGMRES]
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 16
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Package_Matrix::'
  
  
  
CONTAINS
    
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- KILL MATRIX OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Matrix)
    CLASS(MatrixType) :: Matrix
    
    !Local variables
    INTEGER          :: ErrorCode
    TYPE(MatrixType) :: Dummy
    
    !Deallocate memory
    DEALLOCATE (Matrix%iComps               , &
                Matrix%nCompNodes           , &
                Matrix%iCompGlobalNodeStart , &
                Matrix%NJD                  , &
                Matrix%JND                  , &
                Matrix%iIndexDiag           , &
                Matrix%COEFF                , &
                Matrix%RHS                  , &
                Matrix%HDelta               , &
                Matrix%RHSL2                , &
                Matrix%ConnectivityList     , &
                STAT=ErrorCode              )
    
    !Set the attributes to their default values
    Matrix%lConnectivityFlattened = Dummy%lConnectivityFlattened
    Matrix%nComps                 = Dummy%nComps
    
    
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
  ! --- GET INDEX OF JND ARRAY FOR CONNECTIVITY BETWEEN A NODE OF A COMPONENT AND ANOTHER NODE OF ANOTHER COMPONENT IN THE LIST FOR THE FIRST NODE
  ! -------------------------------------------------------------
  FUNCTION GetConnectivityIndex(Matrix,iCompID_1,iNodeID_1,iCompID_2,iNodeID_2) RESULT(iConnectivityIndex)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(IN)           :: iCompID_1,iNodeID_1,iCompID_2,iNodeID_2
    INTEGER                      :: iConnectivityIndex
    
    !Local variables
    INTEGER :: iGlobalNodeID_1,iGlobalNodeID_2,indxS,indxE,iLoc
    
    CALL LocalNode_to_GlobalNode_Scalar(Matrix,iCompID_1,iNodeID_1,iGlobalNodeID_1)
    CALL LocalNode_to_GlobalNode_Scalar(Matrix,iCompID_2,iNodeID_2,iGlobalNodeID_2)
    
    indxS = Matrix%NJD(iGlobalNodeID_1)
    indxE = Matrix%NJD(iGlobalNodeID_1+1) - 1
    iLoc  = LocateInList(iGlobalNodeID_2,Matrix%JND(indxS:indxE))
    iConnectivityIndex = indxS + iLoc - 1
    
  END FUNCTION GetConnectivityIndex
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NODE FOR MAXIMUM HDELTA AND THE COIRRESPONDING COMPONENT ID
  ! -------------------------------------------------------------
  SUBROUTINE GetMaxHDeltaNode_CompID(Matrix,iNodeMaxHDelta,cCompName)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(OUT)          :: iNodeMaxHDelta
    CHARACTER(:),ALLOCATABLE     :: cCompName
    
    !Local variables
    INTEGER :: iCompID,iNodeID
    
    IF (MAXVAL(ABS(Matrix%HDelta)) .GT. 0.0) THEN
        iNodeMaxHDelta = MAXLOC(ABS(Matrix%HDelta),DIM=1)
        CALL GlobalNode_to_LocalNode(Matrix,iNodeMaxHDelta,iCompID,iNodeID)
        iNodeMaxHDelta = iNodeID
        cCompName      = f_cCompNames(iCompID)
    ELSE
        iNodeMaxHDelta = 0
        cCompName      = ""
    END IF
    
  END SUBROUTINE GetMaxHDeltaNode_CompID
  
  
  ! -------------------------------------------------------------
  ! --- GET GLOBAL START AND END ROW INDICES FOR A COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE GetCompRowIndices(Matrix,iCompID,iStartRow,iEndRow)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(IN)           :: iCompID
    INTEGER,INTENT(OUT)          :: iStartRow,iEndRow
    
    !Local variables
    INTEGER :: indx
    
    !Find the information location for the component
    indx = LocateInList(iCompID , Matrix%iComps)
    
    !Return if the component is not simulated
    IF (indx .LT. 1) THEN
        iStartRow = 0
        iEndRow   = 0
        RETURN
    END IF
    
    !Otherwise get the start and end row numbers
    iStartRow = Matrix%iCompGlobalNodeStart(indx)
    iEndRow   = iStartRow + Matrix%nCompNodes(indx) - 1
    
  END SUBROUTINE GetCompRowIndices
  
    
  ! -------------------------------------------------------------
  ! --- GET L-2 NORM OF THE RHS VECTOR
  ! -------------------------------------------------------------
  FUNCTION GetRHSL2(Matrix) RESULT(RHSL2)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    REAL(8)                      :: RHSL2
    
    REAL(8),EXTERNAL :: DNRM2
    
    RHSL2 = DNRM2(SIZE(Matrix%RHS),Matrix%RHS,1)
    
  END FUNCTION GetRHSL2
  
  
  ! -------------------------------------------------------------
  ! --- GET THE RHS FOR A ROW OF A COMPONENT
  ! -------------------------------------------------------------
  FUNCTION GetRHS_Component(Matrix,iCompID,iNodeID) RESULT(RHS)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(IN)           :: iCompID,iNodeID
    REAL(8)                      :: RHS
    
    !Local variables
    INTEGER :: iGlobalNodeID
    
    !Convert local node numebr to global node number
    CALL LocalNode_To_GlobalNode(Matrix,iCompID,iNodeID,iGlobalNodeID)
    
    !Get the RHS value
    RHS = Matrix%RHS(iGlobalNodeID)
    
  END FUNCTION GetRHS_Component
    
    
  ! -------------------------------------------------------------
  ! --- GET THE SIZE OF CONNECTIVITY
  ! -------------------------------------------------------------
  PURE FUNCTION GetConnectivitySize(Matrix) RESULT(N)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER                      :: N
    
    N = SIZE(Matrix%JND)
    
  END FUNCTION GetConnectivitySize
  
  
  ! -------------------------------------------------------------
  ! --- GET NJD ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE GetNJD(Matrix,NJD)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER,ALLOCATABLE          :: NJD(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (NJD , STAT=ErrorCode)
    ALLOCATE (NJD(SIZE(Matrix%NJD)))
    NJD = Matrix%NJD
    
  END SUBROUTINE GetNJD
  
  
  ! -------------------------------------------------------------
  ! --- GET JND ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE GetJND(Matrix,JND)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER,ALLOCATABLE          :: JND(:)
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (JND , STAT=ErrorCode)
    ALLOCATE (JND(SIZE(Matrix%JND)))
    JND = Matrix%JND
    
  END SUBROUTINE GetJND
  
  
  ! -------------------------------------------------------------
  ! --- GET MAXIMUM NUMBER OF ITERATIONS
  ! -------------------------------------------------------------
  PURE FUNCTION GetMaxIter(Matrix) RESULT(IterMax)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER                      :: IterMax
    
    IterMax = Matrix%Solver%IterMax
    
  END FUNCTION GetMaxIter 
  
    

  
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
  ! --- SET THE SOLVER DATA
  ! -------------------------------------------------------------
  SUBROUTINE SetSolver(Matrix,iSolver,Toler,iMaxIter,Relax,iStat)
    CLASS(MatrixType)   :: Matrix
    INTEGER,INTENT(IN)  :: iSolver,iMaxIter
    REAL(8),INTENT(IN)  :: Toler,Relax
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+9) :: ThisProcedure = ModName // 'SetSolver'
    
    !Initialize
    iStat = 0
    
    !Make sure solver is recognized
    IF (LocateInList(iSolver,iSolverList) .EQ. 0)  THEN
        CALL SetLastMessage('Solver ID '//TRIM(IntToText(iSolver))//' is not recognized!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Make sure relaxation parameter is between 1 and 2.
    IF (iSolver .EQ. iSolver_SOR) THEN
        IF (Relax .LT. 1.0   .OR.   Relax .GT. 2.0)  THEN
            CALL SetLastMessage('Relaxation parameter for SOR matrix solver must be between 1.0 and 2.0!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    ASSOCIATE (pSolver => Matrix%Solver)
        pSolver%iSolver   = iSolver
        pSolver%Tolerance = Toler
        pSolver%IterMax   = iMaxIter
        pSolver%Relax     = Relax
    END ASSOCIATE
    
    !Allocate memeory for RHSL2
    ALLOCATE (Matrix%RHSL2(iMaxIter))
    
  END SUBROUTINE SetSolver
  
  
  ! -------------------------------------------------------------
  ! --- SET THE VALUE OF A RHS VECTOR ENTRY
  ! -------------------------------------------------------------
  SUBROUTINE SetRHS(Matrix,iCompID,iNodeID,rValue)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompID,iNodeID
    REAL(8),INTENT(IN) :: rValue
    
    !Local variables
    INTEGER :: iGlobalNodeID
    
    !Convert local node to global node
    CALL LocalNode_To_GlobalNode(Matrix,iCompID,iNodeID,iGlobalNodeID)
    
    !Set value
    Matrix%RHS(iGlobalNodeID) = rValue
    
  END SUBROUTINE SetRHS

  
  ! -------------------------------------------------------------
  ! --- SET COEFF FOR THE DIAGONAL FOR AN ENTIRE COMPONENT
  ! -------------------------------------------------------------
  SUBROUTINE SetCOEFF_AllDiagonalsForComponent(Matrix,iCompID,rValues)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompID
    REAL(8),INTENT(IN) :: rValues(:)
    
    !Local variables
    INTEGER :: iStartIndex,iEndIndex
    
    !Get the global start and end index for the component
    CALL Matrix%GetCompRowIndices(iCompID,iStartIndex,iEndIndex)
    
    !Set the diagonal
    Matrix%COEFF(Matrix%iIndexDiag(iStartIndex:iEndIndex)) = rValues
    
  END SUBROUTINE SetCOEFF_AllDiagonalsForComponent

 
  ! -------------------------------------------------------------
  ! --- SET THE VALUE OF A COEFF ENTRY
  ! -------------------------------------------------------------
  SUBROUTINE SetCOEFF_OneEntry(Matrix,iCompID,iNodeID,iCompID_Connect,iNodeID_Connect,rValue)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompID,iNodeID,iCompID_Connect,iNodeID_Connect
    REAL(8),INTENT(IN) :: rValue
    
    !Local variables
    INTEGER :: iGlobalNodeID,iGlobalNodeID_Connect,indx_S,indx_L,indx
    
    !Convert local nodes to global nodes
    CALL LocalNode_To_GlobalNode(Matrix,iCompID,iNodeID,iGlobalNodeID)
    CALL LocalNode_To_GlobalNode(Matrix,iCompID_Connect,iNodeID_Connect,iGlobalNodeID_Connect)
    
    !Get a pointer to entry in the COEFF array that will be set 
    indx_S = Matrix%NJD(iGlobalNodeID)
    indx_L = Matrix%NJD(iGlobalNOdeID+1) - 1
    DO indx=indx_S,indx_L
        IF (Matrix%JND(indx) .EQ. iGlobalNodeID_Connect) THEN
            Matrix%COEFF(indx) = rValue
            EXIT
        END IF
    END DO
    
  END SUBROUTINE SetCOEFF_OneEntry

 
  ! -------------------------------------------------------------
  ! --- SET THE VALUE OF AN ENTIRE COEFF ROW TO THE SAME VALUE
  ! -------------------------------------------------------------
  SUBROUTINE SetCOEFF_RowToSameValue(Matrix,iCompID,iNodeID,rValue)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompID,iNodeID
    REAL(8),INTENT(IN) :: rValue
    
    !Local variables
    INTEGER :: iGlobalNodeID,indx_S,indx_L
    
    !Convert local nodes to global nodes
    CALL LocalNode_To_GlobalNode(Matrix,iCompID,iNodeID,iGlobalNodeID)
    
    !Get a pointer to entry in the COEFF array that will be set 
    indx_S                      = Matrix%NJD(iGlobalNodeID)
    indx_L                      = Matrix%NJD(iGlobalNOdeID+1) - 1
    Matrix%COEFF(indx_S:indx_L) = rValue
    
  END SUBROUTINE SetCOEFF_RowToSameValue


  ! -------------------------------------------------------------
  ! --- SET THE VALUE OF AN SOME COEFF ENTRIES AT A ROW
  ! -------------------------------------------------------------
  SUBROUTINE SetCOEFF_SomeValuesAtARow(Matrix,iCompID,iNodeID,iCompIDs_Connect,iNodeIDs_Connect,rValues)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompID,iNodeID,iCompIDs_Connect(:),iNodeIDs_Connect(:)
    REAL(8),INTENT(IN) :: rValues(:)
    
    !Local variables
    INTEGER :: iGlobalNodeID,iGlobalNodeIDs_Connect(SIZE(iNodeIDs_Connect)),  &
               indx,indx_S,indx_L,iCount,nConnectedNodes
    
    !Initialize
    nConnectedNodes = SIZE(iCompIDs_Connect)
    
    !Convert to global node IDs
    CALL LocalNode_to_GlobalNode(Matrix,iCompID,iNodeID,iGlobalNodeID)
    CALL LocalNode_to_GlobalNode(Matrix,iCompIDs_Connect,iNodeIDs_Connect,iGlobalNodeIDs_Connect)
    
    !Sort the nodes along with update values
    IF (nConnectedNodes .GT. 1) CALL ShellSort(iGlobalNodeIDs_Connect,rValues)
    
    !Get a pointer to the data for the matrix row that will be updated
    indx_S = Matrix%NJD(iGlobalNodeID)
    indx_L = Matrix%NJD(iGlobalNodeID+1) - 1
    
    !Update values
    iCount = 1
    DO indx=indx_S,indx_L
        IF (Matrix%JND(indx) .EQ. iGlobalNodeIDs_Connect(iCount)) THEN
            Matrix%COEFF(indx) = rValues(iCount)
            iCount             = iCount + 1
            IF (iCount .GT. nConnectedNodes) EXIT
        END IF
    END DO
    
  END SUBROUTINE SetCOEFF_SomeValuesAtARow


  
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
  ! --- READ MATRIX DATA FROM BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadPreprocessedData(Matrix,BinFile,iStat)
    CLASS(MatrixType)     :: Matrix
    TYPE(GenericFileType) :: BinFile
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadPreprocessedData'
    INTEGER                      :: nComps,iDimJND,iDimNJD,ErrorCode
    CHARACTER                    :: cErrMessage*500
    
    !Initialize
    iStat = 0
    
    !Read data from Pre-processor binary file
    CALL BinFile%ReadData(nComps,iStat)  ;  IF (iStat .EQ. -1) RETURN
    Matrix%nComps = nComps
    ALLOCATE (Matrix%iComps(nComps)               , &
              Matrix%nCompNodes(nComps)           , &
              Matrix%iCompGlobalNodeStart(nComps) , &
              STAT=ErrorCode , ERRMSG=cErrMessage ) 
    IF (ErrorCode .NE. 0) CALL EmitError(cErrMessage)
    CALL BinFile%ReadData(Matrix%iComps,iStat)                ;  IF (iStat .EQ. -1) RETURN
    CALL BinFile%ReadData(Matrix%nCompNodes,iStat)            ;  IF (iStat .EQ. -1) RETURN
    CALL BinFile%ReadData(Matrix%iCompGlobalNodeStart,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    CALL BinFile%ReadData(iDimNJD,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Matrix%NJD(iDimNJD) , Matrix%iIndexDiag(iDimNJD-1) , STAT=ErrorCode , ERRMSG=cErrMessage)  
    IF (ErrorCode .NE. 0) THEN
        CALL EmitError(cErrMessage)
        RETURN
    END IF
    CALL BinFile%ReadData(Matrix%NJD,iStat)         ;  IF (iStat .EQ. -1) RETURN
    CALL BinFile%ReadData(Matrix%iIndexDiag,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    CALL BinFile%ReadData(iDimJND,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Matrix%JND(iDimJND) , STAT=ErrorCode , ERRMSG=cErrMessage)  
    IF (ErrorCode .NE. 0) THEN
        CALL EmitError(cErrMessage)
        RETURN
    END IF
    CALL BinFile%ReadData(Matrix%JND,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory for the other array attributes
    ALLOCATE (Matrix%COEFF(iDimJND) , Matrix%RHS(iDimNJD-1)  ,  Matrix%HDelta(iDimNJD-1) , STAT=ErrorCode , ERRMSG=cErrMessage)  
    IF (ErrorCode .NE. 0) THEN
        CALL EmitError(cErrMessage)
        RETURN
    END IF
    
    Matrix%lConnectivityFlattened = .TRUE.
    
  CONTAINS
  
    SUBROUTINE EmitError(cErrMessage)
      CHARACTER(LEN=*),INTENT(IN) :: cErrMessage
      
      CALL SetLastMessage('Error in allocating memory for the matrix equation!'//NEW_LINE('x')//TRIM(cErrMessage),f_iFatal,ThisProcedure)
      iStat = -1
    
    END SUBROUTINE EmitError
    
  END SUBROUTINE ReadPreprocessedData
  
  
    
    
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
  ! --- WRITE MATRIX DATA TO BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE WritePreprocessedData(Matrix,BinFile,iStat)
    CLASS(MatrixType)     :: Matrix
    TYPE(GenericFileType) :: BinFile
    INTEGER,INTENT(OUT)   :: iStat
    
    !First flatten the connectivity list
    CALL FlattenConnectivity(Matrix,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Then write data
    CALL BinFile%WriteData(Matrix%nComps)               
    CALL BinFile%WriteData(Matrix%iComps)               
    CALL BinFile%WriteData(Matrix%nCompNodes)           
    CALL BinFile%WriteData(Matrix%iCompGlobalNodeStart) 
    CALL BinFile%WriteData(SIZE(Matrix%NJD))            
    CALL BinFile%WriteData(Matrix%NJD)                  
    CALL BinFile%WriteData(Matrix%iIndexDiag)           
    CALL BinFile%WriteData(SIZE(Matrix%JND))            
    CALL BinFile%WriteData(Matrix%JND)       
    
  END SUBROUTINE WritePreprocessedData


  
  
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
  ! --- ADD COMPONENT TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE AddComponent(Matrix,iCompID,nCompNodes,iStat)
    CLASS(MatrixType)   :: Matrix
    INTEGER,INTENT(IN)  :: iCompID,nCompNodes
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12) :: ThisProcedure = ModName // 'AddComponent'
    INTEGER                      :: nComps
    INTEGER,ALLOCATABLE          :: Temp_iComps(:),Temp_nCompNodes(:),Temp_iCompGlobalNodeStart(:)
    
    !Initailize
    iStat = 0
    
    !Make sure component has not been added before
    IF (LocateInList(iCompID,Matrix%iComps) .GT. 0) THEN
        CALL SetLastMessage('Component ID '//TRIM(IntToText(iCompID))//' has already been added to matrix!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Add component
    nComps        = Matrix%nComps + 1
    Matrix%nComps = nComps
    ALLOCATE(Temp_iComps(nComps) , Temp_nCompNodes(nComps) , Temp_iCompGlobalNodeStart(nComps))
    Temp_iComps(1:nComps-1)               = Matrix%iComps                ;  Temp_iComps(nComps)               = iCompID
    Temp_nCompNodes(1:nComps-1)           = Matrix%nCompNodes            ;  Temp_nCompNodes(nComps)           = nCompNodes
    Temp_iCompGlobalNodeStart(1:nComps-1) = Matrix%iCompGlobalNodeStart  ;  Temp_iCompGlobalNodeStart(nComps) = SUM(Temp_nCompNodes(1:nComps-1)) + 1
    CALL MOVE_ALLOC(Temp_iComps               , Matrix%iComps)
    CALL MOVE_ALLOC(Temp_nCompNodes           , Matrix%nCompNodes)
    CALL MOVE_ALLOC(Temp_iCompGlobalNodeStart , Matrix%iCompGlobalNodeStart)
        
  END SUBROUTINE AddComponent
    

  ! -------------------------------------------------------------
  ! --- ADD CONNECTIVITY LIST FOR MANY NODES (SEQUENTIALLY) TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE AddConnectivity_ToMany_Sequential(Matrix,iCompID_To,iNodeID_To_Begin,iNodeID_To_End,iCompID_Connect,ConnectivityLists,iStat)
    CLASS(MatrixType)          :: Matrix
    INTEGER,INTENT(IN)         :: iCompID_To,iNodeID_To_Begin,iNodeID_To_End,iCompID_Connect
    TYPE(ConnectivityListType) :: ConnectivityLists(:)
    INTEGER,INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33)           :: ThisProcedure = ModName // 'AddConnectivity_ToMany_Sequential'
    INTEGER                                :: iGlobalNodeID_To_Begin,iGlobalNodeID_To_End,iDimConnectivityList,ErrorCode,   &
                                              indx,nConnectedNodes,iCount,iDim,iDimAdd,iDimTotal,iDimKeep
    TYPE(ConnectivityListType),ALLOCATABLE :: Temp_ConnectivityList(:)
    CHARACTER                              :: cErrMessage*500
    INTEGER,ALLOCATABLE                    :: Temp_ConnectedNodesUnique(:),Temp_ConnectedNodes(:)
    
    !Initialize
    iStat = 0
    
    !Make sure both component IDs are added
    IF (LocateInList(iCompID_To,Matrix%iComps) .EQ. 0) THEN
        CALL SetLastMessage('Component ID '//TRIM(IntToText(iCompID_To))//' to add connectivity to has not been added to the matrix!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (LocateInList(iCompID_Connect,Matrix%iComps) .EQ. 0) THEN
        CALL SetLastMessage('Component ID '//TRIM(IntToText(iCompID_Connect))//' to add connectivity from has not been added to the matrix!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
  
    !Convert local node lists to global node lists
    CALL LocalNode_to_GlobalNode(Matrix,iCompID_To,iNodeID_To_Begin,iGlobalNodeID_To_Begin)
    CALL LocalNode_to_GlobalNode(Matrix,iCompID_To,iNodeID_To_End,iGlobalNodeID_To_End)
    iDimKeep = 0
    DO indx=1,SIZE(ConnectivityLists)
        nConnectedNodes = ConnectivityLists(indx)%nConnectedNodes
        IF (nConnectedNodes .GT. iDimKeep) THEN
            DEALLOCATE (Temp_ConnectedNodes , STAT=ErrorCode)
            ALLOCATE(Temp_ConnectedNodes(nConnectedNodes))
            iDimKeep = nConnectedNodes
        END IF
        CALL LocalNode_to_GlobalNode(Matrix,iCompID_Connect,ConnectivityLists(indx)%ConnectedNodes,Temp_ConnectedNodes(1:nConnectedNodes))
        ConnectivityLists(indx)%ConnectedNodes = Temp_ConnectedNodes(1:nConnectedNodes)
    END DO
    
    !If the size of global node list is smaller than iGlobalNodeID_To_End, adjust the global node list
    iDimConnectivityList = SIZE(Matrix%ConnectivityList)
    IF (iDimConnectivityList .LT. iGlobalNodeID_To_End) THEN
        ALLOCATE (Temp_ConnectivityList(iGlobalNodeID_To_End) , STAT=ErrorCode , ERRMSG=cErrMessage)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for matrix connectivity list!'//NEW_LINE('x')//TRIM(cErrMessage),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        IF (iDimConnectivityList .GT. 0) &
            Temp_ConnectivityList(1:iDimConnectivityList) = Matrix%ConnectivityList
        CALL MOVE_ALLOC(Temp_ConnectivityList , Matrix%ConnectivityList)
    END IF
    
    !Add connectivity lists to Matrix
    iCount = 1
    ASSOCIATE (pConnectivityList => Matrix%ConnectivityList)
        DO indx=iGlobalNodeID_To_Begin,iGlobalNodeID_To_End
            iDim                                 = pConnectivityList(indx)%nConnectedNodes
            iDimAdd                              = ConnectivityLists(iCount)%nConnectedNodes
            iDimTotal                            = iDim + iDimAdd
            IF (iDimTotal .GT. iDimKeep) THEN
                DEALLOCATE(Temp_ConnectedNodes ,STAT=ErrorCode)
                ALLOCATE(Temp_ConnectedNodes(iDimTotal))
                iDimKeep = iDimTotal
            END IF
            Temp_ConnectedNodes(1:iDim)           = pConnectivityList(indx)%ConnectedNodes
            Temp_ConnectedNodes(iDim+1:iDimTotal) = ConnectivityLists(iCount)%ConnectedNodes
            CALL GetUniqueArrayComponents(Temp_ConnectedNodes(1:iDimTotal),Temp_ConnectedNodesUnique)
            CALL ShellSort(Temp_ConnectedNodesUnique)
            CALL MOVE_ALLOC(Temp_ConnectedNodesUnique,pConnectivityList(indx)%ConnectedNodes)
            pConnectivityList(indx)%nConnectedNodes = SIZE(pConnectivityList(indx)%ConnectedNodes)
            iCount = iCount + 1
        END DO
    END ASSOCIATE
    
  END SUBROUTINE AddConnectivity_ToMany_Sequential
  
  
  ! -------------------------------------------------------------
  ! --- ADD CONNECTIVITY LIST FOR MANY NODES (RANDOMLY GIVEN IN AN ARRAY) TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE AddConnectivity_ToMany_Random(Matrix,iCompID_To,iNodeID_To_List,iCompID_Connect,ConnectivityLists,iStat)
    CLASS(MatrixType)          :: Matrix
    INTEGER,INTENT(IN)         :: iCompID_To,iNodeID_To_List(:),iCompID_Connect
    TYPE(ConnectivityListType) :: ConnectivityLists(:)
    INTEGER,INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+29)           :: ThisProcedure = ModName // 'AddConnectivity_ToMany_Random'
    INTEGER                                :: iGlobalNodeID_To_List(SIZE(iNodeID_To_List)),iDimConnectivityList,ErrorCode,   &
                                              indx,nConnectedNodes,iDimMax,iDim,iDimAdd,iDimTotal,iDimKeep,iNodeID_To
    TYPE(ConnectivityListType),ALLOCATABLE :: Temp_ConnectivityList(:)
    CHARACTER                              :: cErrMessage*500
    INTEGER,ALLOCATABLE                    :: Temp_ConnectedNodesUnique(:),Temp_ConnectedNodes(:)
    
    !Initialize
    iStat = 0
    
    !Make sure both component IDs are added
    IF (LocateInList(iCompID_To,Matrix%iComps) .EQ. 0) THEN
        CALL SetLastMessage('Component ID '//TRIM(IntToText(iCompID_To))//' to add connectivity to has not been added to the matrix!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (LocateInList(iCompID_Connect,Matrix%iComps) .EQ. 0) THEN
        CALL SetLastMessage('Component ID '//TRIM(IntToText(iCompID_Connect))//' to add connectivity from has not been added to the matrix!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
  
    !Convert local node lists to global node lists
    CALL LocalNode_to_GlobalNode(Matrix,iCompID_To,iNodeID_To_List,iGlobalNodeID_To_List)
    iDimKeep = 0
    DO indx=1,SIZE(ConnectivityLists)
        nConnectedNodes = ConnectivityLists(indx)%nConnectedNodes
        IF (nConnectedNodes .GT. iDimKeep) THEN
            DEALLOCATE (Temp_ConnectedNodes , STAT=ErrorCode)
            ALLOCATE(Temp_ConnectedNodes(nConnectedNodes))
            iDimKeep = nConnectedNodes
        END IF
        CALL LocalNode_to_GlobalNode(Matrix,iCompID_Connect,ConnectivityLists(indx)%ConnectedNodes,Temp_ConnectedNodes(1:nConnectedNodes))
        ConnectivityLists(indx)%ConnectedNodes = Temp_ConnectedNodes(1:nConnectedNodes)
    END DO
    
    !If the size of global node list is smaller than the max value of iGlobalNodeID_To_List, adjust the global node list
    iDimConnectivityList = SIZE(Matrix%ConnectivityList)
    iDimMax              = MAXVAL(iGlobalNodeID_To_List)
    IF (iDimConnectivityList .LT. iDimMax) THEN
        ALLOCATE (Temp_ConnectivityList(iDimMax) , STAT=ErrorCode , ERRMSG=cErrMessage)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for matrix connectivity list!'//NEW_LINE('x')//TRIM(cErrMessage),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Temp_ConnectivityList(1:iDimConnectivityList) = Matrix%ConnectivityList
        CALL MOVE_ALLOC(Temp_ConnectivityList , Matrix%ConnectivityList)
    END IF
    
    !Add connectivity lists to Matrix
    ASSOCIATE (pConnectivityList => Matrix%ConnectivityList)
        DO indx=1,SIZE(iGlobalNodeID_To_List)
            iNodeID_To = iGlobalNodeID_To_List(indx)
            iDim       = pConnectivityList(iNodeID_To)%nConnectedNodes
            iDimAdd    = ConnectivityLists(indx)%nConnectedNodes
            iDimTotal  = iDim + iDimAdd
            IF (iDimTotal .GT. iDimKeep) THEN
                DEALLOCATE(Temp_ConnectedNodes ,STAT=ErrorCode)
                ALLOCATE(Temp_ConnectedNodes(iDimTotal))
                iDimKeep = iDimTotal
            END IF
            Temp_ConnectedNodes(1:iDim)           = pConnectivityList(iNodeID_To)%ConnectedNodes
            Temp_ConnectedNodes(iDim+1:iDimTotal) = ConnectivityLists(indx)%ConnectedNodes
            CALL GetUniqueArrayComponents(Temp_ConnectedNodes(1:iDimTotal),Temp_ConnectedNodesUnique)
            CALL ShellSort(Temp_ConnectedNodesUnique)
            CALL MOVE_ALLOC(Temp_ConnectedNodesUnique,pConnectivityList(iNodeID_To)%ConnectedNodes)
            pConnectivityList(iNodeID_To)%nConnectedNodes = SIZE(pConnectivityList(iNodeID_To)%ConnectedNodes)
        END DO
    END ASSOCIATE
    
  END SUBROUTINE AddConnectivity_ToMany_Random
  
  
  ! -------------------------------------------------------------
  ! --- ADD CONNECTIVITY LIST FOR ONE NODE TO MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE AddConnectivity_ToOne(Matrix,iCompID_To,iNodeID_To,iCompID_Connect,iNodeID_Connect,iStat)
    CLASS(MatrixType)   :: Matrix
    INTEGER,INTENT(IN)  :: iCompID_To,iNodeID_To,iCompID_Connect,iNodeID_Connect(:)
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+21)           :: ThisProcedure = ModName // 'AddConnectivity_ToOne'
    INTEGER                                :: iGlobalNodeID_To,iGlobalNodeID_Connect(SIZE(iNodeID_Connect)),       &
                                              iDimConnectivityList,ErrorCode,iDim,iDimAdd,iDimTotal
    TYPE(ConnectivityListType),ALLOCATABLE :: Temp_ConnectivityList(:)
    INTEGER,ALLOCATABLE                    :: Temp_ConnectedNodesUnique(:),Temp_ConnectedNodes(:)
    CHARACTER                              :: cErrMessage*500
    
    !Initialize
    iStat = 0
    
    !Make sure both component IDs are added
    IF (LocateInList(iCompID_To,Matrix%iComps) .EQ. 0) THEN
        CALL SetLastMessage('Component ID '//TRIM(IntToText(iCompID_To))//' to add connectivity to has not been added to the matrix!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (LocateInList(iCompID_Connect,Matrix%iComps) .EQ. 0) THEN
        CALL SetLastMessage('Component ID '//TRIM(IntToText(iCompID_Connect))//' to add connectivity from has not been added to the matrix!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
  
    !Convert local node lists to global node lists
    CALL LocalNode_to_GlobalNode(Matrix,iCompID_To,iNodeID_To,iGlobalNodeID_To)
    CALL LocalNode_to_GlobalNode(Matrix,iCompID_Connect,iNodeID_Connect,iGlobalNodeID_Connect)
    CALL ShellSort(iGlobalNodeID_Connect)
    
    !If the size of global node list is smaller than iGlobalNodeID_To, adjust the global node list
    iDimConnectivityList = SIZE(Matrix%ConnectivityList)
    IF (iDimConnectivityList .LT. iGlobalNodeID_To) THEN
        ALLOCATE (Temp_ConnectivityList(iGlobalNodeID_To) , STAT=ErrorCode , ERRMSG=cErrMessage)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for matrix connectivity list!'//NEW_LINE('x')//TRIM(cErrMessage),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Temp_ConnectivityList(1:iDimConnectivityList) = Matrix%ConnectivityList
        CALL MOVE_ALLOC(Temp_ConnectivityList , Matrix%ConnectivityList)
    END IF
    
    !Add connectivity list
    ASSOCIATE (pConnectivityList => Matrix%ConnectivityList(iGlobalNodeID_To))
        iDim                                  = pConnectivityList%nConnectedNodes
        iDimAdd                               = SIZE(iGlobalNodeID_Connect)
        iDimTotal                             = iDim + iDimAdd
        ALLOCATE (Temp_ConnectedNodes(iDimTotal))
        Temp_ConnectedNodes(1:iDim)           = pConnectivityList%ConnectedNodes
        Temp_ConnectedNodes(iDim+1:iDimTotal) = iGlobalNodeID_Connect
        CALL GetUniqueArrayComponents(Temp_ConnectedNodes(1:iDimTotal),Temp_ConnectedNodesUnique)
        CALL ShellSort(Temp_ConnectedNodesUnique)
        CALL MOVE_ALLOC(Temp_ConnectedNodesUnique,pConnectivityList%ConnectedNodes)
        pConnectivityList%nConnectedNodes = SIZE(pConnectivityList%ConnectedNodes)
    END ASSOCIATE
    
  END SUBROUTINE AddConnectivity_ToOne
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT A LIST OF LOCAL NODE NUMBERS INTO GLOBAL NODE NUMBERS
  ! -------------------------------------------------------------
  SUBROUTINE LocalNode_to_GlobalNode_Array(Matrix,iCompID,iNodeID,iGlobalNodeID)
    TYPE(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(IN)          :: iCompID,iNodeID(:)
    INTEGER,INTENT(OUT)         :: iGlobalNodeID(:)
    
    !Local variables
    INTEGER :: indx_S
    
    !Global node start for the component minus 1 (for proper conversion)
    indx_S = LocateInList(iCompID,Matrix%iComps)
    indx_S = Matrix%iCompGlobalNodeStart(indx_S) - 1
    
    !Convert
    iGlobalNodeID = iNodeID + indx_S
    
  END SUBROUTINE LocalNode_to_GlobalNode_Array
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT A LIST OF LOCAL NODE NUMBERS FROM MULTIPLE COMPONENTS INTO GLOBAL NODE NUMBERS
  ! -------------------------------------------------------------
  SUBROUTINE LocalNode_to_GlobalNode_Array2(Matrix,iCompID,iNodeID,iGlobalNodeID)
    TYPE(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(IN)          :: iCompID(:),iNodeID(:)
    INTEGER,INTENT(OUT)         :: iGlobalNodeID(:)
    
    !Local variables
    INTEGER :: indx_S,indx
    
    DO indx=1,SIZE(iCompID)
        !Global node start for the component minus 1 (for proper conversion)
        indx_S = LocateInList(iCompID(indx),Matrix%iComps)
        indx_S = Matrix%iCompGlobalNodeStart(indx_S) - 1
        
        !Convert
        iGlobalNodeID(indx) = iNodeID(indx) + indx_S
    END DO
    
  END SUBROUTINE LocalNode_to_GlobalNode_Array2
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT A LOCAL NODE NUMBER INTO GLOBAL NODE NUMBER
  ! -------------------------------------------------------------
  SUBROUTINE LocalNode_to_GlobalNode_Scalar(Matrix,iCompID,iNodeID,iGlobalNodeID)
    TYPE(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(IN)          :: iCompID,iNodeID
    INTEGER,INTENT(OUT)         :: iGlobalNodeID
    
    !Local variables
    INTEGER :: indx_S
    
    !Global node start for the component minus 1 (for proper conversion)
    indx_S = LocateInList(iCompID,Matrix%iComps)
    indx_S = Matrix%iCompGlobalNodeStart(indx_S) - 1
    
    !Convert
    iGlobalNodeID = iNodeID + indx_S
    
  END SUBROUTINE LocalNode_to_GlobalNode_Scalar
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT A GLOBAL NODE NUMBER INTO LOCAL NODE NUMBER
  ! -------------------------------------------------------------
  SUBROUTINE GlobalNode_to_LocalNode(Matrix,iGlobalNodeID,iCompID,iNodeID)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    INTEGER,INTENT(IN)           :: iGlobalNodeID
    INTEGER,INTENT(OUT)          :: iCompID,iNodeID
    
    !Local variables
    INTEGER :: indx,iStart,iEnd
    
    !Initialize
    iCompID = 0
    iNodeID = 0
    
    DO indx=1,Matrix%nComps
        iStart = Matrix%iCompGlobalNodeStart(indx)
        iEnd   = iStart + Matrix%nCompNodes(indx) - 1
        IF (iGlobalNodeID .LE. iEnd   .AND.   iGlobalNodeID .GE. iStart) THEN
            iCompID = Matrix%iComps(indx)
            iNodeID = iGlobalNodeID - iStart + 1
            RETURN
        END IF
    END DO
    
  END SUBROUTINE GlobalNode_to_LocalNode
  
  
  ! -------------------------------------------------------------
  ! --- FLATTEN THE CONNECTIVITY LIST TO SIMPLE ARRAYS OF NJD AND JND
  ! -------------------------------------------------------------
  SUBROUTINE FlattenConnectivity(Matrix,iStat)
    CLASS(MatrixType)   :: Matrix
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'FlattenConnectivity'
    INTEGER                      :: iDimNJD,iDimJND,ErrorCode,indx,iCount_S,iCount_L
    CHARACTER                    :: cErrMessage*500
    
    !Initialize
    iStat = 0
    
    !Return if the connectivity is already flattened
    IF (Matrix%lConnectivityFlattened) RETURN
    
    !Calculate array dimensions
    iDimNJD = SIZE(Matrix%ConnectivityList) + 1
    iDimJND = SUM(Matrix%ConnectivityList%nConnectedNodes)
    
    !Allocate memory
    ALLOCATE (Matrix%NJD(iDimNJD)          , &
              Matrix%JND(iDimJND)          , &
              Matrix%iIndexDiag(iDimNJD-1) , &
              STAT = ErrorCode             , &
              ERRMSG = cErrMessage         )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for the NJD and JND arrays of the matrix!'//NEW_LINE('x')//TRIM(cErrMessage),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Compile NJD and JND arrays
    ASSOCIATE (pConnectivityList => Matrix%ConnectivityList , &
               pNJD              => Matrix%NJD              , &
               pJND              => Matrix%JND              , &
               pIndexDiag        => Matrix%iIndexDiag       )
        pNJD(1)          = 1
        iCount_S         = 1
        iCount_L         = pConnectivityList(1)%nConnectedNodes
        pJND(1:iCount_L) = pConnectivityList(1)%ConnectedNodes
        pIndexDiag(1)    = LocateInList(1,pJND(1:iCount_L))
        DO indx=2,iDimNJD-1
            iCount_S                = iCount_L + 1
            pNJD(indx)              = iCount_S        
            iCount_L                = iCount_S + pConnectivityList(indx)%nConnectedNodes - 1
            pJND(iCount_S:iCount_L) = pConnectivityList(indx)%ConnectedNodes
            pIndexDiag(indx)        = iCount_S - 1 + LocateInList(indx,pJND(iCount_S:iCount_L))
        END DO
        pNJD(iDimNJD) = iCount_L + 1
    END ASSOCIATE
               
    !Allocate memory for the other array attributes
    ALLOCATE (Matrix%COEFF(iDimJND) , Matrix%RHS(iDimNJD-1)  , Matrix%HDelta(iDimNJD-1) , STAT=ErrorCode , ERRMSG=cErrMessage)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for COEFF, RHS or HDelta arrays of the matrix!'//NEW_LINE('x')//TRIM(cErrMessage),f_iFatal,ThisProcedure)    
        iStat = -1
        RETURN
    END IF
        
    !Clear memory from ConnectivityList array
    DEALLOCATE (Matrix%ConnectivityList , STAT=ErrorCode)
    
    !Set flag
    Matrix%lConnectivityFlattened = .TRUE.
    
  END SUBROUTINE FlattenConnectivity
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO SOLVE THE MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE Solve(Matrix,NewtonRaphsonIter,iStat)
    CLASS(MatrixType)   :: Matrix
    INTEGER,INTENT(IN)  :: NewtonRaphsonIter
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    INTEGER              :: ErrorCode,NRow
    INTEGER, ALLOCATABLE :: NJD_CRS(:),JND_CRS(:)                       ! INTERMEDIATE CRS FORMAT STORAGE ARRAYS
    REAL(8), ALLOCATABLE :: COEFF_CRS(:)
    REAL(8),EXTERNAL     :: DNRM2
    
    !Initialize
    IF (Matrix%Solver%ZeroReset) Matrix%HDelta = 0.0
    NRow = SIZE(Matrix%HDelta)
        
    !Compute L2-norm of the RHS vector
    Matrix%RHSL2(NewtonRaphsonIter) = DNRM2(NRow,Matrix%RHS,1)
    
    !Solve the set of equations based on the method chosen by the user
    ASSOCIATE (pSolver => Matrix%Solver)
        SELECT CASE (pSolver%iSolver) 
            CASE (iSolver_SOR)
                CALL SOR(NRow                , &
                         pSolver%IterMax     , &
                         pSolver%Tolerance   , &
                         pSolver%Relax       , &
                         Matrix%NJD          , &
                         Matrix%JND          , &
                         Matrix%iIndexDiag   , &
                         Matrix%RHS          , &
                         Matrix%COEFF        , &
                         Matrix%HDelta       , &
                         iStat               )

            CASE (iSolver_PGMRES)
                !Convert matrix storage to CRS storage
                CALL ConvertToCRSFormat(Matrix%COEFF , Matrix%NJD , Matrix%JND , COEFF_CRS , NJD_CRS , JND_CRS)
                
                CALL PGMRES(NRow                            , &
                            SIZE(COEFF_CRS)                 , &
                            NJD_CRS                         , &
                            JND_CRS                         , &
                            Matrix%RHS                      , &
                            Matrix%RHSL2(NewtonRaphsonIter) , &
                            COEFF_CRS                       , &
                            Matrix%HDelta                   , &
                            pSolver%IterMax                 , &
                            pSolver%Tolerance               , &
                            iStat                           )
    
        END SELECT 

    END ASSOCIATE    
    
    !Clear memory
    DEALLOCATE(COEFF_CRS, NJD_CRS, JND_CRS , STAT=ErrorCode)
    
  END SUBROUTINE Solve
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO SOLVE THE MATRIX
  ! -------------------------------------------------------------
  SUBROUTINE ConvertToCRSFormat(COEFF, NJD, JND, COEFF_CRS, NJD_CRS, JND_CRS)
    INTEGER,INTENT(IN)              :: NJD(:),JND(:)
    REAL(8),INTENT(IN)              :: COEFF(:)
    INTEGER,ALLOCATABLE,INTENT(OUT) :: NJD_CRS(:),JND_CRS(:)                    ! INTERMEDIATE STORAGE ARRAYS
    REAL(8),ALLOCATABLE,INTENT(OUT) :: COEFF_CRS(:)

    INTEGER :: I, K, IM, IshftIDX , N, NJ

    N = SIZE(NJD) - 1
    NJ= SIZE(COEFF)

    IshftIDX = 0
      
    DO I = 1, N   
       DO K = NJD(I),(NJD(I+1)-1)
         IF (COEFF(K).EQ.0.D0 .OR. JND(K).EQ.0) IshftIDX = IshftIDX + 1       
     ENDDO    
    ENDDO
      
    NJ = NJ-IshftIDX
    ALLOCATE(COEFF_CRS(NJ), NJD_CRS(N+1), JND_CRS(NJ))
      
    IM = 1
    NJD_CRS(1) = 1
    IshftIDX = 0

    DI: DO I = 1, N   
     DK: DO K = NJD(I),(NJD(I+1)-1)
         IF (COEFF(K).EQ.0.D0 .OR. JND(K).EQ.0) THEN
           IshftIDX = IshftIDX + 1
         ELSE
           COEFF_CRS(IM) = COEFF(K)
           JND_CRS(IM) = JND(K)
           IM = IM + 1
         ENDIF
        ENDDO DK
       NJD_CRS(I+1) = NJD(I+1) - IshftIDX 
    END DO DI
 
  END SUBROUTINE ConvertToCRSFormat
  
  
  ! -------------------------------------------------------------
  ! --- SOLVE THE MATRIX EQUATION USING SUCCESSIVE OVER-RELAXATION METHOD
  ! -------------------------------------------------------------
  SUBROUTINE SOR(NRow,MaxIter,Toler,Relax,NJD,JND,IndexDiag,RHS,COEFF,U,iStat)
    INTEGER,INTENT(IN)  :: NRow,MaxIter
    REAL(8),INTENT(IN)  :: Toler,Relax  
    INTEGER,INTENT(IN)  :: NJD(:),JND(:),IndexDiag(:)      
    REAL(8),INTENT(IN)  :: COEFF(:),RHS(:)     
    REAL(8)             :: U(:)
    INTEGER,INTENT(OUT) :: iStat

!DIR$ IF (_OPENMP .NE. 0) 
    !$ CALL SOR_OMP(NRow,MaxIter,Toler,Relax,NJD,JND,IndexDiag,RHS,COEFF,U,iStat)
!DIR$ ELSE
    CALL SOR_Sequential(NRow,MaxIter,Toler,Relax,NJD,JND,IndexDiag,RHS,COEFF,U,iStat) 
!DIR$ END IF

  END SUBROUTINE SOR 
  
  
  ! -------------------------------------------------------------
  ! --- SOLVE THE MATRIX EQUATION USING SUCCESSIVE OVER-RELAXATION METHOD (SEQUENTIAL)
  ! -------------------------------------------------------------
  SUBROUTINE SOR_Sequential(NRow,MaxIter,Toler,Relax,NJD,JND,IndexDiag,RHS,COEFF,U,iStat)
    INTEGER,INTENT(IN)  :: NRow,MaxIter
    REAL(8),INTENT(IN)  :: Toler,Relax  
    INTEGER,INTENT(IN)  :: NJD(:),JND(:),IndexDiag(:)      
    REAL(8),INTENT(IN)  :: COEFF(:),RHS(:)     
    REAL(8)             :: U(:)
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables    
    CHARACTER(LEN=ModNameLen+14) :: ThisProcedure = ModName // 'SOR_Sequential'
    INTEGER                      :: IROW,INDX,INDX_S,INDX_L,NODEMAX,Iter,indxDiag
    REAL(8)                      :: DIFF_L2,ADIFFMAX,U_INT,ACCUM,DIFF, DNRM2
    EXTERNAL                     :: DNRM2
    
    !Initialize
    iStat = 0
    Iter  = 0

    !Solve matrix equation using SOR iterative method
    DO
        Iter     = Iter + 1
        DIFF_L2  = 0.0
        ADIFFMAX = 0.0
        
        DO IROW=1,NRow
            indxDiag = IndexDiag(IROW)
            INDX_S   = NJD(IROW)
            INDX_L   = NJD(IROW+1) - 1
            U_INT    = U(IROW)
            ACCUM    = 0.0
            DO INDX=INDX_S,indxDiag-1
                ACCUM = ACCUM + COEFF(INDX) * U(JND(INDX))
            END DO
            DO INDX=indxDiag+1,INDX_L
                ACCUM = ACCUM + COEFF(INDX) * U(JND(INDX))
            END DO
            U(IROW) = (RHS(IROW)-ACCUM)/COEFF(indxDiag)
            DIFF    = U(IROW) - U_INT
            DIFF_L2 = DIFF_L2 + (DIFF*DIFF)
            U(IROW) = U_INT + (DIFF*Relax) 
            IF (ABS(DIFF) .GT. ADIFFMAX) THEN
                ADIFFMAX = ABS(DIFF)
                NODEMAX  = IROW
            END IF
        END DO
        
        !Check convergence of matrix solver     
        IF (Iter.GT.MaxIter  .OR.  SQRT(DIFF_L2).LE.Toler  .OR.  ADIFFMAX.LE.Toler) THEN 
            !WRITE(*,*) 'SOR Iteration: ', ITS, ' : Residual: ', SQRT(DIFF_L2), ' : ||x|| : ', DNRM2(NROW, U, 1)             
                EXIT
        END IF
        
    END DO

    !Stop if solution has not converged
    IF (SQRT(DIFF_L2) .GT. Toler) THEN
        IF (ADIFFMAX .GT. Toler) THEN
            MessageArray(1) = 'Convergence problem in the solution of equation system using' 
            MessageArray(2) = 'Succesive Over-Relaxation method.'     
            WRITE (MessageArray(3), '(A,I8)')   'Iteration =', Iter
            WRITE (MessageArray(4), '(A,I8)')   'Variable  =', NODEMAX
            WRITE (MessageArray(5),'(A,E12.3)') 'Difference=', ADIFFMAX
            CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
   END IF

  END SUBROUTINE SOR_Sequential


  ! -------------------------------------------------------------
  ! --- SOLVE THE MATRIX EQUATION USING SUCCESSIVE OVER-RELAXATION METHOD (PARALLELL)
  ! -------------------------------------------------------------
  SUBROUTINE SOR_OMP(NRow,MaxIter,Toler,Relax,NJD,JND,IndexDiag,RHS,COEFF,U,iStat)
    INTEGER,INTENT(IN)  :: NRow,MaxIter
    REAL(8),INTENT(IN)  :: Toler,Relax  
    INTEGER,INTENT(IN)  :: NJD(:),JND(:),IndexDiag(:)      
    REAL(8),INTENT(IN)  :: COEFF(:),RHS(:)     
    REAL(8)             :: U(:)
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables    
    CHARACTER(LEN=ModNameLen+7) :: ThisProcedure = ModName // 'SOR_OMP'
    INTEGER                     :: IROW,INDX,INDX_S,INDX_L,NODEMAX,Iter,indxDiag,iNThreads,iThread
    REAL(8)                     :: DIFF_L2,ADIFFMAX,U_INT,ACCUM,DIFF, DNRM2
    REAL(8),ALLOCATABLE,SAVE    :: DIFF_L2_Thread(:),ADIFFMAX_Thread(:)
    INTEGER,ALLOCATABLE,SAVE    :: NODEMAX_Thread(:)
    INTEGER,SAVE                :: iChunkSize
    EXTERNAL                    :: DNRM2
    
    !Initialize
    iStat     = 0
    Iter      = 0
    iNThreads = 1
    !$ iNThreads = OMP_GET_NUM_PROCS()-1
    IF (.NOT. ALLOCATED(DIFF_L2_Thread)) THEN
        ALLOCATE(DIFF_L2_Thread(iNThreads) , ADIFFMAX_Thread(iNThreads) , NODEMAX_Thread(iNThreads))
        iChunkSize = NRow / iNThreads
        IF (iChunkSize * iNThreads .LT. NRow) THEN
            IF (iNThreads .GT. 1) THEN
                iChunkSize = NRow / (iNThreads-1)
            ELSE
                iChunkSize = NRow
            END IF
        END IF
    END IF
    

    !Solve matrix equation using SOR iterative method
    DO
        Iter            = Iter + 1
        DIFF_L2_Thread  = 0.0
        ADIFFMAX_Thread = 0.0
        
        !$OMP PARALLEL DO SCHEDULE(STATIC,iChunkSize) DEFAULT(SHARED) PRIVATE(IROW,indxDiag,INDX_S,INDX_L,U_INT,ACCUM,INDX,DIFF,iThread) NUM_THREADS(iNThreads)
        DO IROW=1,NRow
            !$ iThread  = OMP_GET_THREAD_NUM() + 1
            indxDiag = IndexDiag(IROW)
            INDX_S   = NJD(IROW)
            INDX_L   = NJD(IROW+1) - 1
            U_INT    = U(IROW)
            ACCUM    = 0.0
            DO INDX=INDX_S,indxDiag-1
                ACCUM = ACCUM + COEFF(INDX) * U(JND(INDX))
            END DO
            DO INDX=indxDiag+1,INDX_L
                ACCUM = ACCUM + COEFF(INDX) * U(JND(INDX))
            END DO
            U(IROW)                 = (RHS(IROW)-ACCUM)/COEFF(indxDiag)
            DIFF                    = U(IROW) - U_INT
            DIFF_L2_Thread(iThread) = DIFF_L2_Thread(iThread) + (DIFF*DIFF)
            U(IROW)                 = U_INT + (DIFF*Relax) 
            IF (ABS(DIFF) .GT. ADIFFMAX_Thread(iThread)) THEN
                ADIFFMAX_Thread(iThread) = ABS(DIFF)
                NODEMAX_Thread(iThread)  = IROW
            END IF
        END DO
        !$OMP END PARALLEL DO
        DIFF_L2  = SUM(DIFF_L2_Thread)
        ADIFFMAX = 0.0
        DO INDX=1,SIZE(ADIFFMAX_Thread)
            IF (ADIFFMAX_Thread(INDX) .GT. ADIFFMAX) THEN
                ADIFFMAX = ADIFFMAX_Thread(INDX)
                NODEMAX  = NODEMAX_Thread(INDX)
            END IF
        END DO
        
        !Check convergence of matrix solver     
        IF (Iter.GT.MaxIter  .OR.  SQRT(DIFF_L2).LE.Toler  .OR.  ADIFFMAX.LE.Toler) THEN 
            !WRITE(*,*) 'SOR Iteration: ', Iter, ' : Residual: ', SQRT(DIFF_L2), ' : ||x|| : ', DNRM2(NROW, U, 1)             
                EXIT
        END IF
        
    END DO

    !Stop if solution has not converged
    IF (SQRT(DIFF_L2) .GT. Toler) THEN
        IF (ADIFFMAX .GT. Toler) THEN
            MessageArray(1) = 'Convergence problem in the solution of equation system using' 
            MessageArray(2) = 'Succesive Over-Relaxation method.'     
            WRITE (MessageArray(3), '(A,I8)')   'Iteration =', Iter
            WRITE (MessageArray(4), '(A,I8)')   'Variable  =', NODEMAX
            WRITE (MessageArray(5),'(A,E12.3)') 'Difference=', ADIFFMAX
            CALL SetLastMessage(MessageArray(1:5),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
   END IF

  END SUBROUTINE SOR_OMP


  ! -------------------------------------------------------------
  ! --- SOLVE THE MATRIX EQUATION USING (P)RECONDITIONED GMRES
  ! -------------------------------------------------------------
  !                 *** ILUT - Preconditioned GMRES ***                  *
  !                                                                      *
  !----------------------------------------------------------------------*
  ! This is a simple version of the ILUT preconditioned GMRES algorithm. *
  ! The ILUT preconditioner uses a dual strategy for dropping elements   *
  ! instead  of the usual level of-fill-in approach. See details in ILUT *
  ! subroutine documentation. PGMRES uses the L and U matrices generated *
  ! from the subroutine ILUT to precondition the GMRES algorithm.        *
  ! The preconditioning can be applied to the left or right. The             *
  ! stopping criterion utilized is based simply on reducing the              *
  ! residual norm by epsilon.                                                                *
  ! This preconditioning is more reliable than ilu0 but requires more    *
  ! storage. It seems to be much less prone to difficulties related to   *
  ! strong nonsymmetries in the matrix. We recommend using a nonzero tol *
  ! (tol=.005 or .001 usually give good results) in ILUT. Use a large    *
  ! lfil whenever possible (e.g. lfil = 5 to 10). The higher lfil the    *
  ! more reliable the code is. Efficiency may also be much improved.     *
  ! Note that lfil=n and tol=0.0 in ILUT  will yield the same factors as *
  ! Gaussian elimination without pivoting.                               *
  SUBROUTINE PGMRES(N,iDimCOEFF,NJD,JND,RHS,RHS_L2,COEFF,U,MXITER,Toler,iStat)
    INTEGER,INTENT(IN)  :: N,iDimCOEFF,MXITER,NJD(N+1),JND(iDimCOEFF)
    REAL(8),INTENT(IN)  :: COEFF(iDimCOEFF),RHS(N),RHS_L2,Toler     
    REAL(8)             :: U(N)
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+6) :: ThisProcedure=ModName // 'PGMRES'
    INTEGER, ALLOCATABLE        :: JLU(:)
    REAL(8), ALLOCATABLE        :: COEFFLU(:),W(:)
    REAL(8)                     :: DNRM2, DROPTOL, RES, FPAR(16)
    INTEGER                     :: LFIL, IM, IWK, IERR, Iter,IPAR(16),JU(N),IW(3*N)  
    EXTERNAL                    :: DNRM2
    
    !Initialize
    iStat = 0

    ! ILUT PRECONDITIONER PARAMETERS    
    LFIL        =       5                                           ! THE LEVEL OF FILL-IN, TYPICALLY BETWEEN 5 AND 10
    DROPTOL     =       0.01                                    ! THE DROP TOLERANCE, TYPICALLY SET TO < 1.0    
    IWK         =       N*(2*LFIL + 1)                  ! WORKSPACE SIZE REQUIRED BY THE PRECONDITIONER

    IM          =       20                                          ! THE NUMBER OF ITERATIONS PERFORMED BEFORE RESTART OF GMRES(IM) 
    RES         =       0.0D0                                   ! THE RESIDUAL ERROR USED TO ASSESS CONVERGENCE

    
    ! PARMETERS OF THE SOLVER
    IPAR(1)         =   0                                           ! INITIALIZE THE SOLVER
    IPAR(2)     =       1                                           ! CHOOSE LEFT PRECONDITIONING
    IPAR(3)     =       2                                           ! SPECIFY STOPPING CRITERIA BASED ON THE RESIDUAL   
    IPAR(4)     =       (N+3)*(IM+2) + (IM+1)*IM/2      ! WORKSPACE SIZE NEED BY THE SOLVER
    IPAR(5)     =       IM
    IPAR(6)     =       MXITER

    !! ADAPTIVE TOLERANCES FOR GMRES
    !! HIEU NGUYEN 03/29/2011
    !! *** Removed since tests showed a slow down of N-R convergence with the adaptive tolerance
    !STOPCD=1.0d3*Toler
    !rho1=2.0d0
    !rho2=1.7d0
    !if (NR_ITERX > 2) then
    !   alpha=(1.0d0/real(NR_ITERX+1))**rho1*(min(1.0d0,(rhsl2(NR_ITERX)/min(rhsl2(NR_ITERX-1),rhsl2(1)))))**rho2
    !   STOPCD=alpha*STOPCD
    !endif
    !STOPCD=min(1.0d3*Toler,max(STOPCD,Toler))
    !! END ADAPTIVE TOLERANCES
    IF (RHS_L2 .EQ. 0.0) THEN
        FPAR(1) = 0.0
    ELSE
        !FPAR(1) = STOPCD/rDiv                           ! NEW TOLERANCE CONDITION
        FPAR(1) = Toler / RHS_L2
    END IF
    FPAR(2)     = EPSILON(0d0)                           ! MACHINE PRECISION
    
    ALLOCATE(COEFFLU(IWK), JLU(IWK), W(IPAR(4)))   

!   CALL SPARSKIT IMPLEMENTATION OF ILUT IN THE FILE pgmres.f
    CALL ILUT(N, COEFF, JND, NJD, LFIL, DROPTOL, COEFFLU, JLU, JU, IWK, W, IW, IERR)
    SELECT CASE (IERR)
      CASE (-1)
        MessageArray(1) = 'Bad coefficent matrix! Execution cannot proceed.'
        MessageArray(2) = 'Please check input data.'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
       
      CASE (-3:-2)
        MessageArray(1) = 'Insufficent storage for LU factorization!'
        MessageArray(2) = 'Please contact IWFM techical support.'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
        
      CASE (:-5)
        MessageArray(1) = 'All matrix entries for variable '//TRIM(IntToText(-5-IERR))//' are zero!'
        MessageArray(2) = 'Check all data specified for this variable.'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
        
    END SELECT

    Iter         =      0                                           ! INITIALIZE THE NUMBER OF ITERATIONS
   

10  CALL GMRES(N, RHS, U, IPAR, FPAR, W)                ! CALL SPARSKIT IMPLEMENTATION OF GMRES(M) IN THE FILE iters.f

    IF (IPAR(7) .NE. Iter) THEN
         Iter = IPAR(7)
         !!write(80,'(a21,i4,E16.6)')'      PGMRES ITS,RES',Iter,FPAR(5)
    ENDIF
    RES = FPAR(5)
!
 
    IF (IPAR(1).EQ.1) THEN
         CALL AMUX(N, W(IPAR(8)), W(IPAR(9)), COEFF, JND, NJD)  !MATRIX-VECTOR MULTIPLICATION
         GOTO 10
    ELSE IF (IPAR(1).EQ.3 .OR. IPAR(1).EQ.5) THEN
         CALL LUSOL(N,W(IPAR(8)),W(IPAR(9)),COEFFLU,JLU,JU)         !LU SOLVE
         GOTO 10 
    ELSE IF (IPAR(1).LT.0) THEN

         IF (IPAR(1).EQ.-1) THEN
           MessageArray(1) = 'Convergence problem in the solution of equation system using PGMRES(M).'     
           WRITE(MessageArray(2),'(A,I8)')     'Iteration =', Iter
           WRITE (MessageArray(3),'(A,E12.3)') 'Residual  =', RES
           CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
           iStat = -1
           RETURN

         ELSE IF (IPAR(1).EQ.-2) THEN
            MessageArray(1)='ITERATIVE SOLVER WAS NOT GIVEN ENOUGH WORK SPACE.'
            WRITE (MessageArray(2),'(A,I12,A)') 'THE WORK SPACE SHOULD AT LEAST HAVE ', IPAR(4),' ELEMENTS.'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
            
         ELSE IF (IPAR(1).EQ.-3) THEN
           CALL SetLastMessage('ITERATIVE SOLVER IS FACING A BREAK-DOWN.',f_iFatal,ThisProcedure)
           iStat = -1
           RETURN
           
         ELSE
           WRITE(MessageArray(1),'(A,I8)') 'ITERATIVE SOLVER TERMINATED. CODE =', IPAR(1) 
           CALL SetLastMessage(MessageArray(1),f_iFatal,ThisProcedure)
           iStat = -1
           RETURN
           
         ENDIF
    ELSE IF (IPAR(1).EQ.0) THEN
       !WRITE(*,*) 'GMRES Iteration: ', Iter, ' : Residual: ', RES, ' : ||x|| : ', DNRM2(N, U, 1)

    ENDIF
    
    DEALLOCATE(COEFFLU, JLU, W)

  END SUBROUTINE PGMRES
  
  
  ! -------------------------------------------------------------
  ! --- RESET COEFF AND RHS TO ZERO
  ! -------------------------------------------------------------
  SUBROUTINE ResetToZero(Matrix)
    CLASS(MatrixType) :: Matrix
    
    Matrix%RHS   = 0.0
    Matrix%COEFF = 0.0
  
  END SUBROUTINE ResetToZero
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE RHS VECTOR FOR RANDOM ROWS
  ! -------------------------------------------------------------
  SUBROUTINE UpdateRHS_Random(Matrix,iCompIDs,iNodeIDs,rUpdateValues)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompIDs(:),iNodeIDs(:)
    REAL(8),INTENT(IN) :: rUpDateValues(:)
    
    !Local variables
    INTEGER :: iGlobalNodeIDs(SIZE(iNodeIDs))
    
    !Convert local nodes to global nodes
    CALL LocalNode_to_GlobalNode(Matrix,iCompIDs,iNodeIDs,iGlobalNodeIDs)
    
    !Sort the nodes along with the rUpdateValues
    CALL ShellSort(iGlobalNodeIDs,rUpdateValues)
    
    !Update RHS
    Matrix%RHS(iGlobalNodeIDs) = Matrix%RHS(iGlobalNodeIDs) + rUpDateValues
    
  END SUBROUTINE UpdateRHS_Random
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE RHS VECTOR FOR ONE COMPONENT STARTING AT A ROW
  ! -------------------------------------------------------------
  SUBROUTINE UpdateRHS_StartAtRow(Matrix,iCompID,iNodeID_Start,rUpdateValues)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompID,iNodeID_Start
    REAL(8),INTENT(IN) :: rUpDateValues(:)
    
    !Local variables
    INTEGER :: iGlobalNodeID,indx
    
    !Convert local nodes to global nodes
    CALL LocalNode_to_GlobalNode(Matrix,iCompID,iNodeID_Start,iGlobalNodeID)
    
    !Update RHS
    DO indx=0,SIZE(rUpdateValues)-1
        Matrix%RHS(iGlobalNodeID+indx) = Matrix%RHS(iGlobalNodeID+indx) + rUpDateValues(indx+1)
    END DO
    
  END SUBROUTINE UpdateRHS_StartAtRow
  
  
  ! -------------------------------------------------------------
  ! --- UPDATE COEFF
  ! -------------------------------------------------------------
  SUBROUTINE UpdateCOEFF(Matrix,iCompID,iNodeID,iDim,iCompIDs_Connect,iNodeIDs_Connect,rUpdateValues)
    CLASS(MatrixType)  :: Matrix
    INTEGER,INTENT(IN) :: iCompID,iNodeID,iDim,iCompIDs_Connect(iDim),iNodeIDs_Connect(iDim)
    REAL(8),INTENT(IN) :: rUpdateValues(iDim)
    
    !Local variables
    INTEGER :: iGlobalNodeID,iGlobalNodeIDs_Connect(SIZE(iNodeIDs_Connect)),  &
               indx,indx_S,indx_L,iCount,nConnectedNodes
    
    !Initialize
    nConnectedNodes = SIZE(iCompIDs_Connect)
    
    !Convert to global node IDs
    CALL LocalNode_to_GlobalNode(Matrix,iCompID,iNodeID,iGlobalNodeID)
    CALL LocalNode_to_GlobalNode(Matrix,iCompIDs_Connect,iNodeIDs_Connect,iGlobalNodeIDs_Connect)
    
    !Sort the nodes along with update values
    IF (nConnectedNodes .GT. 1) CALL ShellSort(iGlobalNodeIDs_Connect,rUpdateValues)
    
    !Get a pointer to the data for the matrix row that will be updated
    indx_S = Matrix%NJD(iGlobalNodeID)
    indx_L = Matrix%NJD(iGlobalNodeID+1) - 1
    
    !Update values
    iCount = 1
    DO indx=indx_S,indx_L
        IF (Matrix%JND(indx) .EQ. iGlobalNodeIDs_Connect(iCount)) THEN
            Matrix%COEFF(indx) = Matrix%COEFF(indx) + rUpdateValues(iCount)
            iCount             = iCount + 1
            IF (iCount .GT. nConnectedNodes) EXIT
        END IF
    END DO
    
  END SUBROUTINE UpdateCOEFF
  
  
  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(VersionType) :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) 
    
  END FUNCTION GetVersion


  ! -------------------------------------------------------------
  ! --- PRINT OUT COEFF ARRAY IN 2-D MATRIX FORM
  ! -------------------------------------------------------------
  SUBROUTINE PrintCOEFF_In_MatrixForm(Matrix,cOutFileName,iCompID,iStat)
    CLASS(MatrixType),INTENT(IN) :: Matrix
    CHARACTER(LEN=*),INTENT(IN)  :: cOutFileName
    INTEGER,OPTIONAL,INTENT(IN)  :: iCompID
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'PrintCOEFF_In_MatrixForm'
    TYPE(GenericFileType)        :: OutFile
    INTEGER                      :: Local_iCompID,iLoc,iRowStart,iRowEnd,nNodes,iColStart,iColEnd,iCol,iRow,  &
                                    iCount,iDim,ErrorCode,indx_S,indx_L
    CHARACTER(:),ALLOCATABLE     :: ALine
    CHARACTER                    :: cText*14
    
    !Initialize
    iStat = 0
    IF (PRESENT(iCompID)) THEN
        Local_iCompID = iCompID
    ELSE
        Local_iCompID = 0
    END IF
    
    !Open output file
    CALL OutFile%New(cOutFileName,InputFile=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Get the row numebrs to be printed
    IF (Local_iCompID .EQ. 0) THEN
        iRowStart = 1
        iRowEnd   = SIZE(Matrix%RHS)
    ELSE
        !Is the component included in the matrix?
        iLoc = LocateInList(iCompID,Matrix%iComps)
        IF (iLoc .EQ. 0) THEN
            CALL SetLastMessage('Component ID '//TRIM(IntTotext(Local_iCompID))//' is not included in the matrix!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iRowStart = Matrix%iCompGlobalNodeStart(iLoc)
        iRowEnd   = iRowStart + Matrix%nCompNodes(iLoc) - 1
    END IF
    nNodes = iRowEnd - iRowStart + 1
    iDim   = 16 * nNodes
    ALLOCATE (CHARACTER(iDim) :: ALine)
   
    !Print
    DO iRow=iRowStart,iRowEnd
        indx_S    =  Matrix%NJD(iRow)
        indx_L    =  Matrix%NJD(iRow+1)-1       
        iColStart =  MAX(Matrix%JND(indx_S) , iRowStart)
        iColEnd   =  MIN(Matrix%JND(indx_L) , iRowEnd)
        iCount    = indx_S
        ALine     = ''
        DO iCol=iRowStart,iRowEnd
            IF (Matrix%JND(iCount) .EQ. iCol) THEN
                WRITE (cText,'(F14.2)') Matrix%COEFF(iCount)
                iCount = iCount + 1
            ELSE
                WRITE (cText,'(F14.2)') 0.0
            END IF
            ALine = TRIM(ALine) // '  ' // ADJUSTR(cText)
        END DO
        CALL OutFile%WriteData(ALine)  
    END DO
            
    !Close file
    CALL OutFile%Kill()
    
    !Release memory
    DEALLOCATE (ALine , STAT=ErrorCode)
    
  END SUBROUTINE PrintCOEFF_In_MatrixForm 
  
  
END MODULE