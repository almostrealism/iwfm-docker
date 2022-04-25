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
MODULE Class_StrmNodeBudget
  USE MessageLogger       , ONLY: SetLastMessage                 , &
                                  MessageArray                   , &
                                  f_iFatal                         
  USE IOInterface         , ONLY: GenericFileType                                                
  USE TimeSeriesUtilities , ONLY: TimeStepType                                       
  USE GeneralUtilities    , ONLY: GetUniqueArrayComponents       , &
                                  StripTextUntilCharacter        , &
                                  CleanSpecialCharacters         , &
                                  EstablishAbsolutePathFileName  , &
                                  IntToText                      , &
                                  ConvertID_To_Index             , &
                                  LocateInList
  USE Package_Budget      , ONLY: BudgetType                     , &
                                  BudgetHeaderType               , &
                                  f_iMaxLocationNameLen          , &
                                  f_iColumnHeaderLen
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
  PUBLIC :: StrmNodeBudgetType                     


  ! -------------------------------------------------------------
  ! --- STREAM NODE BUDGET DATA TYPE
  ! -------------------------------------------------------------
  TYPE StrmNodeBudgetType
    INTEGER             :: NBudNodes                      = 0        !Number of nodes for budget printing
    INTEGER,ALLOCATABLE :: iBudNodes(:)                              !Stream nodes for budget output
    LOGICAL             :: StrmNodeBudRawFile_Defined     = .FALSE.  !Flag to see if a budget output will be created
    TYPE(BudgetType)    :: StrmNodeBudRawFile                        !Output file
  CONTAINS 
    PROCEDURE,PASS   :: New         => StrmNodeBudget_New
    PROCEDURE,PASS   :: Kill        => StrmNodeBudget_Kill
    PROCEDURE,PASS   :: GetBudNodes
    PROCEDURE,PASS   :: GetBudget_NColumns
    PROCEDURE,PASS   :: GetBudget_ColumnTitles
    PROCEDURE,NOPASS :: GetBudget_MonthlyFlows_GivenFile
    PROCEDURE,PASS   :: GetBudget_MonthlyFlows_GivenStrmNodeBudget
    PROCEDURE,PASS   :: GetBudget_TSData
    GENERIC          :: GetBudget_MonthlyFlows  => GetBudget_MonthlyFlows_GivenFile           , &
                                                   GetBudget_MonthlyFlows_GivenStrmNodeBudget
  END TYPE StrmNodeBudgetType
  

  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE

     FUNCTION Abstract_PrepareStreamNodeBudgetHeader(NLocations,iPrintReachBudgetOrder,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,cReachNames,iBudNodes) RESULT(Header)
        IMPORT                               :: TimeStepType,BudgetHeaderType
        INTEGER,INTENT(IN)                   :: NLocations,iPrintReachBudgetOrder(:),iReachIDs(:),iStrmNodeIDs(:),NTIME
        TYPE(TimeStepType),INTENT(IN)        :: TimeStep
        CHARACTER(LEN=*),INTENT(IN)          :: cVersion
        CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cReachNames(:)
        INTEGER,OPTIONAL,INTENT(IN)          :: iBudNodes(:)
        TYPE(BudgetHeaderType)               :: Header
     END FUNCTION Abstract_PrepareStreamNodeBudgetHeader

  END INTERFACE  
  
  PROCEDURE(Abstract_PrepareStreamNodeBudgetHeader),POINTER :: p 

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_StrmNodeBudget::'
  
  
  
  
CONTAINS
    
    
    
    
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW STREAM NODE BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmNodeBudget_New(StrmNodeBudget,IsRoutedStreams,IsForInquiry,cWorkingDirectory,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,pProcPrepareHeader,InFile,iStat) 
    CLASS(StrmNodeBudgetType),INTENT(OUT)             :: StrmNodeBudget
    LOGICAL,INTENT(IN)                                :: IsRoutedStreams,IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                       :: cWorkingDirectory
    INTEGER,INTENT(IN)                                :: iReachIDs(:),iStrmNodeIDs(:),NTIME
    TYPE(TimeStepType),INTENT(IN)                     :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)                       :: cVersion
    PROCEDURE(Abstract_PrepareStreamNodeBudgetHeader) :: pProcPrepareHeader
    TYPE(GenericFileType)                             :: InFile
    INTEGER,INTENT(OUT)                               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'StrmNodeBudget_New'
    INTEGER                                :: NBudNodes,ErrorCode,indxNode,iStrmNodeID,iDummyArray(0)
    CHARACTER                              :: BudFileName*1000
    TYPE(BudgetHeaderType)                 :: BudHeader
    INTEGER,ALLOCATABLE                    :: iArrayOut(:)
    CHARACTER(:),ALLOCATABLE               :: cAbsPathFileName
    
    !Number of nodes for which budget output is desired
    CALL InFile%ReadData(NBudNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (IsRoutedStreams) THEN
        StrmNodeBudget%NBudNodes = NBudNodes
        ALLOCATE (StrmNodeBudget%iBudNodes(NBudNodes))
    END IF
    
    !Budget binary output filename
    CALL InFile%ReadData(BudFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN
    BudFileName = StripTextUntilCharacter(BudFileName,'/') 
    CALL CleanSpecialCharacters(BudFileName)
    
    !Stream nodes for budget output
    IF (IsRoutedStreams) THEN
        DO indxNode=1,NBudNodes
           CALL InFile%ReadData(iStrmNodeID,iStat)  
           IF (iStat .EQ. -1) RETURN
           CALL ConvertID_To_Index(iStrmNodeID,iStrmNodeIDs,StrmNodeBudget%iBudNodes(indxNode))
           IF (StrmNodeBudget%iBudNodes(indxNode) .EQ. 0) THEN
               CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iStrmNodeID))//' listed for stream node budget printing is not in the model!',f_iFatal,ThisProcedure)
               iStat = -1
               RETURN
           END IF
        END DO
        
        !Make sure that stream nodes for budget are not repeated
        IF (NBudNodes .GT. 0) THEN
            CALL GetUniqueArrayComponents(StrmNodeBudget%iBudNodes,iArrayOut)
            IF (SIZE(iArrayOut) .NE. NBudNodes) THEN
                MessageArray(1) = "Some stream node numbers listed for stream node budget print-out are repeated!"
                MessageArray(2) = "Make sure repeated node numbers are deleted."
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END IF
    
    
    !Return if no node budget output is desired
    IF (NBudNodes.EQ.0  .OR. BudFileName.EQ.'') THEN
      StrmNodeBudget%NBudNodes = 0
      DEALLOCATE (StrmNodeBudget%iBudNodes , STAT=ErrorCode)
      RETURN
    END IF
    
    !Otherwise, open the budget binary file
    IF (IsRoutedStreams) THEN
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(BudFileName)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL StrmNodeBudget%StrmNodeBudRawFile%New(cAbsPathFileName,iStat)  
            IF (iStat .EQ. -1) RETURN
        ELSE
            BudHeader = pProcPrepareHeader(NBudNodes,iDummyArray,iReachIDs,iStrmNodeIDs,NTIME,TimeStep,cVersion,iBudNodes=StrmNodeBudget%iBudNodes)
            CALL StrmNodeBudget%StrmNodeBudRawFile%New(cAbsPathFileName,BudHeader,iStat)
            IF (iStat .EQ. -1) RETURN
            CALL BudHeader%Kill()
        END IF
        StrmNodeBudget%StrmNodeBudRawFile_Defined = .TRUE.
    END IF
    
  END SUBROUTINE StrmNodeBudget_New

  
    
    
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
  ! --- KILL STREAM NODE BUDGET OUTPUT OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE StrmNodeBudget_Kill(StrmNodeBudget)
    CLASS(StrmNodeBudgetType) :: StrmNodeBudget
    
    !Local variables
    INTEGER                  :: ErrorCode
    TYPE(StrmNodeBudgetType) :: Dummy
    
    !Deallocate array attributes
    DEALLOCATE (StrmNodeBudget%iBudNodes , STAT=ErrorCode)
    
    !Close output file
    CALL StrmNodeBudget%StrmNodeBudRawFile%Kill()
    
    !Set attributes to their default values
    SELECT TYPE (p => StrmNodeBudget)
        TYPE IS (StrmNodeBudgetType)
            p = Dummy
    END SELECT
    
  END SUBROUTINE StrmNodeBudget_Kill
    


    
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
  ! --- GET THE NODE NUMBERS WITH BUDGET OUTPUT
  ! -------------------------------------------------------------
  SUBROUTINE GetBudNodes(StrmNodeBudget,iBudNodes)
    CLASS(StrmNodeBudgetType),INTENT(IN) :: StrmNodeBudget
    INTEGER,ALLOCATABLE,INTENT(OUT)      :: iBudNodes(:)
    
    ALLOCATE (iBudNodes(StrmNodeBudget%NBudNodes))
    iBudNodes = StrmNodeBudget%iBudNodes
    
  END SUBROUTINE GetBudNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF COLUMNS IN BUDGET FILE (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_NColumns(StrmNodeBudget,iBudIndex,iNCols,iStat)
    CLASS(StrmNodeBudgetType),INTENT(IN) :: StrmNodeBudget
    INTEGER,INTENT(IN)                   :: iBudIndex
    INTEGER,INTENT(OUT)                  :: iNCols,iStat  
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'GetBudget_NColumns'
    
    !Check if a budget file is defined
    IF (.NOT. StrmNodeBudget%StrmNodeBudRawFile_Defined) THEN
        CALL SetLastMessage('Stream node budget file does not exist to retrieve the number of budget columns!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Get number of columns (includes Time column)
    CALL StrmNodeBudget%StrmNodeBudRawFile%GetNDataColumns(iBudIndex,iNCols,iStat) 
        
    !Exclude Time column
    iNCols = iNCols - 1
        
  END SUBROUTINE GetBudget_NColumns

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN TITLES (EXCLUDING TIME COLUMN) 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_ColumnTitles(StrmNodeBudget,iBudIndex,cUnitLT,cUnitAR,cUnitVL,cColTitles,iStat)
    CLASS(StrmNodeBudgetType),INTENT(IN)     :: StrmNodeBudget
    INTEGER,INTENT(IN)                       :: iBudIndex
    CHARACTER(LEN=*),INTENT(IN)              :: cUnitLT,cUnitAR,cUnitVL
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cColTitles(:)       
    INTEGER,INTENT(OUT)                      :: iStat
        
    !Local variables
    CHARACTER(LEN=ModNameLen+22)                  :: ThisProcedure = ModName // 'GetBudget_ColumnTitles'
    INTEGER                                       :: iNCols,iErrorCode
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColTitles_Local(:)
    
    !Check if a budget file is defined
    IF (.NOT. StrmNodeBudget%StrmNodeBudRawFile_Defined) THEN
        CALL SetLastMessage('Stream node budget file does not exist to retrieve the budget column titles!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Number of columns (includes Time column)
    CALL StrmNodeBudget%StrmNodeBudRawFile%GetNDataColumns(iBudIndex,iNCols,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Get column titles (includes Time column)
    ALLOCATE (cColTitles_Local(iNCols))
    cColTitles_Local = StrmNodeBudget%StrmNodeBudRawFile%GetFullColumnHeaders(iBudIndex,iNCols)
    
    !Insert units
    CALL StrmNodeBudget%StrmNodeBudRawFile%ModifyFullColumnHeaders(cUnitLT,cUnitAR,cUnitVL,cColTitles_Local)
    
    !Remove Time column
    iNCols = iNCols - 1
    ALLOCATE (cColTitles(iNCols))
    cColTitles = ADJUSTL(cColTitles_Local(2:))
    
    !Clear memory
    DEALLOCATE (cColTitles_Local , STAT=iErrorCode)

  END SUBROUTINE GetBudget_ColumnTitles

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM StrmNodeBudget OBJECT 
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenStrmNodeBudget(StrmNodeBudget,iStrmNodeID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    CLASS(StrmNodeBudgetType),INTENT(IN)     :: StrmNodeBudget
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iStrmNodeID
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+42) :: ThisProcedure = ModName // 'GetBudget_MonthlyFlows_GivenStrmNodeBudget'
    
    !Check if a budget file is defined
    IF (.NOT. StrmNodeBudget%StrmNodeBudRawFile_Defined) THEN
        CALL SetLastMessage('Stream node budget file does not exist to retrieve monthly budget flows!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Retrieve flows
    CALL GetBudget_MonthlyFlows_GivenFile(StrmNodeBudget%StrmNodeBudRawFile,iStrmNodeID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
        
  END SUBROUTINE GetBudget_MonthlyFlows_GivenStrmNodeBudget

  
  ! -------------------------------------------------------------
  ! --- GET MONTHLY BUDGET FLOWS FROM A DEFINED BUDGET FILE
  ! --- (Assumes cBeginDate and cEndDate are adjusted properly)
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_MonthlyFlows_GivenFile(Budget,iStrmNodeID,cBeginDate,cEndDate,rFactVL,rFlows,cFlowNames,iStat)
    TYPE(BudgetType),INTENT(IN)              :: Budget      !Assumes Budget file is already open
    CHARACTER(LEN=*),INTENT(IN)              :: cBeginDate,cEndDate
    INTEGER,INTENT(IN)                       :: iStrmNodeID
    REAL(8),INTENT(IN)                       :: rFactVL
    REAL(8),ALLOCATABLE,INTENT(OUT)          :: rFlows(:,:)  !In (column,month) format
    CHARACTER(LEN=*),ALLOCATABLE,INTENT(OUT) :: cFlowNames(:)
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    INTEGER,PARAMETER   :: iReadCols(12) = [1,2,3,4,5,6,7,8,9,10,11,12]
    INTEGER             :: iDimActual,iNTimeSteps
    REAL(8),ALLOCATABLE :: rValues(:,:)
    
    !Get simulation time steps and allocate array to read data
    iNTimeSteps = Budget%GetNTimeSteps()
    ALLOCATE (rValues(13,iNTimeSteps)) !Adding 1 to the first dimension for Time column; it will be removed later
    
    !Read data
    CALL Budget%ReadData(iStrmNodeID,iReadCols,'1MON',cBeginDate,cEndDate,0d0,0d0,0d0,1d0,1d0,rFactVL,iDimActual,rValues,iStat)
    IF (iStat .NE. 0) RETURN
    
    !Store values in return argument
    ALLOCATE (rFlows(11,iDimActual) , cFlowNames(11))
    rFlows(1,:)  = rValues(2,1:iDimActual)                             !Upstream Inflow              
    rFlows(2,:)  = -rValues(3,1:iDimActual)                            !Downstream Outflow          
    rFlows(3,:)  = rValues(4,1:iDimActual)                             !Tributary Inflow            
    rFlows(4,:)  = rValues(5,1:iDimActual)                             !Tile Drain                  
    rFlows(5,:)  = rValues(6,1:iDimActual)                             !Runoff                      
    rFlows(6,:)  = rValues(7,1:iDimActual)                             !Return Flow                  
    rFlows(7,:)  = rValues(8,1:iDimActual) + rValues(9,1:iDimActual)   !Gain from GW    
    rFlows(8,:)  = rValues(10,1:iDimActual)                            !Gain from Lake               
    rFlows(9,:) = -rValues(11,1:iDimActual)                            !Riparian ET                  
    rFlows(10,:) = -rValues(12,1:iDimActual)                           !Diversion                    
    rFlows(11,:) = -rValues(13,1:iDimActual)                           !By-pass Flow                 
    
    !Flow names
    cFlowNames     = ''
    cFlowNames(1)  = 'Upstream Inflow'    
    cFlowNames(2)  = 'Downstream Outflow' 
    cFlowNames(3)  = 'Tributary Inflow'   
    cFlowNames(4)  = 'Tile Drain'         
    cFlowNames(5)  = 'Runoff'             
    cFlowNames(6)  = 'Return Flow'        
    cFlowNames(7)  = 'Gain from GW'    
    cFlowNames(8)  = 'Gain from Lake'     
    cFlowNames(9)  = 'Riparian ET'        
    cFlowNames(10) = 'Diversion'          
    cFlowNames(11) = 'Bypass Flow'       
    
  END SUBROUTINE GetBudget_MonthlyFlows_GivenFile

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TIME SERIES DATA FOR A SET OF COLUMNS 
  ! -------------------------------------------------------------
  SUBROUTINE GetBudget_TSData(StrmNodeBudget,iStrmNodeID,iCols,cBeginDate,cEndDate,cInterval,rFactLT,rFactAR,rFactVL,rOutputDates,rOutputValues,iDataTypes,inActualOutput,iStat)
    CLASS(StrmNodeBudgetType),INTENT(IN) :: StrmNodeBudget
    INTEGER,INTENT(IN)                   :: iStrmNodeID,iCols(:)
    CHARACTER(LEN=*),INTENT(IN)          :: cBeginDate,cEndDate,cInterval
    REAL(8),INTENT(IN)                   :: rFactLT,rFactAR,rFactVL
    REAL(8),INTENT(OUT)                  :: rOutputDates(:),rOutputValues(:,:)    !rOutputValues is in (timestep,column) format
    INTEGER,INTENT(OUT)                  :: iDataTypes(:),inActualOutput,iStat
    
    !Local variables
    INTEGER :: indx
    
    IF (StrmNodeBudget%StrmNodeBudRawFile_Defined) THEN
        DO indx=1,SIZE(iCols)
            CALL StrmNodeBudget%StrmNodeBudRawFile%ReadData(iStrmNodeID,iCols(indx),cInterval,cBeginDate,cEndDate,1d0,0d0,0d0,rFactLT,rFactAR,rFactVL,iDataTypes(indx),inActualOutput,rOutputDates,rOutputValues(:,indx),iStat)
        END DO
    ELSE
        iStat          = 0
        inActualOutput = 0
        iDataTypes     = -1
        rOutputDates   = 0.0
        rOutputValues  = 0.0
    END IF
    
  END SUBROUTINE GetBudget_TSData
    
END MODULE
