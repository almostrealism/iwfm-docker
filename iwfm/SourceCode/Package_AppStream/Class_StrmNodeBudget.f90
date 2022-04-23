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
MODULE Class_StrmNodeBudget
  USE MessageLogger       , ONLY: SetLastMessage                 , &
                                  MessageArray                   , &
                                  iFatal                         
  USE IOInterface         , ONLY: GenericFileType                                                
  USE TimeSeriesUtilities , ONLY: TimeStepType                                       
  USE GeneralUtilities    , ONLY: GetUniqueArrayComponents       , &
                                  StripTextUntilCharacter        , &
                                  CleanSpecialCharacters         , &
                                  EstablishAbsolutePathFileName 
  USE Package_Budget      , ONLY: BudgetType                     , &
                                  BudgetHeaderType
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
    PROCEDURE,PASS :: New         => StrmNodeBudget_New
    PROCEDURE,PASS :: Kill        => StrmNodeBudget_Kill
    PROCEDURE,PASS :: GetBudNodes
  END TYPE StrmNodeBudgetType
  

  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE

     FUNCTION Abstract_PrepareStreamNodeBudgetHeader(NLocations,NTIME,TimeStep,cVersion,iBudNodes) RESULT(Header)
        IMPORT                        :: TimeStepType,BudgetHeaderType
        INTEGER,INTENT(IN)            :: NLocations,NTIME
        TYPE(TimeStepType),INTENT(IN) :: TimeStep
        CHARACTER(LEN=*),INTENT(IN)   :: cVersion
        INTEGER,OPTIONAL,INTENT(IN)   :: iBudNodes(:)
        TYPE(BudgetHeaderType)        :: Header
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
  SUBROUTINE StrmNodeBudget_New(StrmNodeBudget,IsRoutedStreams,IsForInquiry,cWorkingDirectory,NTIME,TimeStep,cVersion,pProcPrepareHeader,InFile,iStat) 
    CLASS(StrmNodeBudgetType),INTENT(OUT)             :: StrmNodeBudget
    LOGICAL,INTENT(IN)                                :: IsRoutedStreams,IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                       :: cWorkingDirectory
    INTEGER,INTENT(IN)                                :: NTIME
    TYPE(TimeStepType),INTENT(IN)                     :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)                       :: cVersion
    PROCEDURE(Abstract_PrepareStreamNodeBudgetHeader) :: pProcPrepareHeader
    TYPE(GenericFileType)                             :: InFile
    INTEGER,INTENT(OUT)                               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'StrmNodeBudget_New'
    INTEGER                                :: NBudNodes,ErrorCode,indxNode
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
           CALL InFile%ReadData(StrmNodeBudget%iBudNodes(indxNode),iStat)  
           IF (iStat .EQ. -1) RETURN
        END DO
        
        !Make sure that stream nodes for budget are not repeated
        IF (NBudNodes .GT. 0) THEN
            CALL GetUniqueArrayComponents(StrmNodeBudget%iBudNodes,iArrayOut)
            IF (SIZE(iArrayOut) .NE. NBudNodes) THEN
                MessageArray(1) = "Some stream node numbers listed for stream node budget print-out are repeated!"
                MessageArray(2) = "Make sure repeated node numbers are deleted."
                CALL SetLastMessage(MessageArray(1:2),iFatal,ThisProcedure)
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
            BudHeader = pProcPrepareHeader(NBudNodes,NTIME,TimeStep,cVersion,StrmNodeBudget%iBudNodes)
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
    INTEGER :: ErrorCode
    
    !Deallocate array attributes
    DEALLOCATE (StrmNodeBudget%iBudNodes , STAT=ErrorCode)
    
    !Close output file
    CALL StrmNodeBudget%StrmNodeBudRawFile%Kill()
    
    !Set attributes to their default values
    StrmNodeBudget%NBudNodes                  = 0
    StrmNodeBudget%StrmNodeBudRawFile_Defined = .FALSE.
    
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
  
END MODULE
