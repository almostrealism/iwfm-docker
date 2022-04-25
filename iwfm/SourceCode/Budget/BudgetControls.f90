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
MODULE BudgetControls
  USE IWFM_Util_VersionF   , ONLY: IWFM_Util
  USE MessageLogger        , ONLY: SetLastMessage            , &
                                   LogMessage                , &
                                   LogLastMessage            , &
                                   PrintRunTime              , &
                                   MessageArray              , &
                                   f_iWarn                   , &
                                   f_iFatal                  , &
                                   f_iMessage                , &
                                   f_iSCREEN                 
  USE ProgramTimer         , ONLY: StopTimer      
  USE GeneralUtilities     , ONLY: StripTextUntilCharacter   , &
                                   CleanSpecialCharacters    , &
                                   LocateInList              , &
                                   ConvertID_To_Index        , &
                                   AllocArray                , &
                                   f_cLineFeed               
  USE TimeSeriesUtilities  , ONLY: IsTimeStampValid          , &
                                   StripTimeStamp
  USE IOInterface          , ONLY: GenericFileType
  USE Opening_Screen       , ONLY: Print_Screen              , &
                                   Get_Main_File
  USE IWFM_Core_Version    , ONLY: IWFM_Core
  USE Package_Budget       , ONLY: BudgetType                , &
                                   PrintIntervalType         , &
                                   Package_Budget_GetVersion
  USE Package_Misc         , ONLY: Package_Misc_GetVersion   
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
  PUBLIC :: PrintBudgetTables  ,  &
            EndExecution
  
  
  ! -------------------------------------------------------------
  ! --- OUTPUT RELATED DATA
  ! -------------------------------------------------------------  
  REAL(8),SAVE::                 &
       FACTLTOU                 ,& !Conversion factor for length output
       FACTAROU                 ,& !Conversion factor for area output
       FACTVLOU                    !Conversion factor for volume output
  CHARACTER(LEN=8),SAVE::        &
       UNITLTOU                 ,& !Unit for length output
       UNITAROU                 ,& !Unit for area output
       UNITVLOU                    !Unit for volume output
  TYPE(PrintIntervalType),SAVE :: &
       PrintInterval
  INTEGER,SAVE                 :: &
       Cache                    , &
       NBudget
       
    
  ! -------------------------------------------------------------
  ! --- BUDGET CLASS TYPE
  ! -------------------------------------------------------------
  TYPE BudgetClassType
    PRIVATE
    CHARACTER(LEN=1000) :: cBudgetInputFileName  = ''
    CHARACTER(LEN=1000) :: cBudgetOutputFileName = ''
    CHARACTER(LEN=6)    :: cPrintTimeStep        = ''
    INTEGER             :: NPrintLoc             = 0
    INTEGER,ALLOCATABLE :: iPrintLocs(:)
  END TYPE BudgetClassType
  TYPE(BudgetClassType),ALLOCATABLE,TARGET :: BudgetClasses(:)
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! ------------------------------------------------------------- 
  INTEGER,PARAMETER                   :: ModNameLen = 16
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName = 'BudgetControls::' 




  
CONTAINS




  ! -------------------------------------------------------------
  ! --- READ IN THE MAIN CONTROL DATA
  ! -------------------------------------------------------------
  SUBROUTINE GetMainControlData(FileName,iStat)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: FileName
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+18)    :: ThisProcedure = ModName // 'GetMainControlData'
    INTEGER                         :: ErrorCode,indxBudget,indx,NPrintLoc
    CHARACTER(LEN=1000)             :: MainFileName
    CHARACTER(LEN=1000)             :: ALine
    TYPE(GenericFileType)           :: MainControlFile
    LOGICAL                         :: TrackTime
    TYPE(BudgetClassType),POINTER   :: pBudgetClass
    
    !INitialize
    iStat = 0

    !Prompt user for the name of the main input file
    IF (PRESENT(FileName)) THEN
      MainFileName = FileName
    ELSE
      CALL Print_screen('Program: Budget',IWFM_Core)
      CALL Get_Main_File(' Enter the Name of the Main Input File >  ',MainFileName)
      IF (TRIM(MainFileName) .EQ. '-about') THEN
        CALL PrintVersionNumbers()
        STOP
      END IF
    END IF

    !Initialize main control file
    CALL MainControlFile%New(FileName=MainFileName,InputFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Read the output unit controls
    CALL MainControlFile%ReadData(FACTLTOU,iStat)  ;  IF(iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN  ;  UNITLTOU = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL MainControlFile%ReadData(FACTAROU,iStat)  ;  IF(iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN  ;  UNITAROU = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL MainControlFile%ReadData(FACTVLOU,iStat)  ;  IF(iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN  ;  UNITVLOU = ADJUSTL(StripTextUntilCharacter(ALine,'/'))

    !Read cache size
    CALL MainControlFile%ReadData(Cache,iStat)  
    IF(iStat .EQ. -1) RETURN
    
    !Read the output control options
    TrackTime = .FALSE.         !Default
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN
    ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/',Back=.TRUE.)) 
    IF (IsTimeStampValid(ALine)) THEN
      PrintInterval%PrintBeginDateAndTime = StripTimeStamp(ALine)
      TrackTime                           = .TRUE.
    END IF

    !Based on the time tracking option, read in the relevant data
    SELECT CASE (TrackTime)
      !Simulation date and time is tracked
      CASE (.TRUE.)
        !Get the ending date and time       
        CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN
        ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/',Back=.TRUE.)) 
        IF (IsTimeStampValid(ALine)) THEN
          PrintInterval%PrintEndDateAndTime = StripTimeStamp(ALine)
        ELSE
          CALL SetLastMessage('Budget output ending time should be in MM/DD/YYYY_hh:mm format!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
        END IF

      !Simulation date and time is NOT tracked
      CASE (.FALSE.)
        !Set the budget output beginning time
        READ (ALine,*,IOSTAT=ErrorCode) PrintInterval%PrintBeginTime
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in reading the budget output beginning time!',f_iFatal,ThisProcedure) 
            iStat = -1
            RETURN
        END IF
        !Get the ending time
        CALL MainControlFile%ReadData(PrintInterval%PrintEndTime,iStat)  ;  IF(iStat .EQ. -1) RETURN      

    END SELECT
    
    !Read the number of budget tables classes and allocate memory
    CALL MainControlFile%ReadData(NBudget,iStat)  ;  IF(iStat .EQ. -1) RETURN
    ALLOCATE (BudgetClasses(NBudget))
    
    !Read the budget class data
    DO indxBudget=1,NBudget
      pBudgetClass => BudgetClasses(indxBudget)
      CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN  ;  ALine  = ADJUSTL(StripTextUntilCharacter(ALine,'/'))  ;  CALL CleanSpecialCharacters(ALine)
      pBudgetClass%cBudgetInputFileName  = ALine
      CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN  ;  ALine  = ADJUSTL(StripTextUntilCharacter(ALine,'/'))  ;  CALL CleanSpecialCharacters(ALine)
      pBudgetClass%cBudgetOutputFileName = ALine
      CALL MainControlFile%ReadData(ALine,iStat)  ;  IF(iStat .EQ. -1) RETURN  ;  ALine  = ADJUSTL(StripTextUntilCharacter(ALine,'/'))  ;  CALL CleanSpecialCharacters(ALine)
      pBudgetClass%cPrintTimeStep = ALine
      CALL MainControlFile%ReadData(NPrintLoc,iStat)  ;  IF(iStat .EQ. -1) RETURN
      pBudgetClass%NPrintLoc = NPrintLoc
      CALL AllocArray(pBudgetClass%iPrintLocs,NPrintLoc,ThisProcedure,iStat)
      IF (iStat .EQ. -1) RETURN
      DO indx=1,NPrintLoc
          CALL MainControlFile%ReadData(pBudgetClass%iPrintLocs(indx),iStat)  
          IF(iStat .EQ. -1) RETURN
      END DO
    END DO
    
    !Close main control file
    CALL MainControlFile%Kill()

  END SUBROUTINE GetMainControlData
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT PRINTS OUT COMPONENT VERSION NUMBERS
  ! -------------------------------------------------------------
  SUBROUTINE PrintVersionNumbers()
  
    MessageArray(1) = NEW_LINE('x')//'VERSION NUMBERS FOR IWFM AND ITS COMPONENTS:'//NEW_LINE('x')
    MessageArray(2) = '  IWFM Core         : '//TRIM(IWFM_Core%GetVersion())
    MessageArray(3) = '  IWFM_Util.lib     : '//TRIM(IWFM_Util%GetVersion())
    MessageArray(4) = '  Package_Misc.lib  : '//TRIM(Package_Misc_GetVersion())
    MessageArray(5) = '  Package_Budget.lib: '//TRIM(Package_Budget_GetVersion())
    
    CALL LogMessage(MessageArray(1:5),f_iMessage,'',Destination=f_iSCREEN)
  
  END SUBROUTINE PrintVersionNumbers
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT BUDGET TABLES WHEN THE CONTROL DATA IS READ FROM CONTROL FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintBudgetTables(cFileName,iStat)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cFileName
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'PrintBudgetTables'
    INTEGER                                :: indxBudget
    
    !Initialzie
    iStat = 0
    
    !Read the main control data
    IF (PRESENT(cFileName)) THEN
      CALL GetMainControlData(cFileName,iStat)
    ELSE
      CALL GetMainControlData(iStat=iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    
    !If NBUdget is zero, message to the user and return
    IF (NBudget .EQ. 0) THEN
        CALL LogMessage('Number of budget tables to be processed is set to zero!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    !Process the budget classes
    DO indxBudget=1,NBudget      
      CALL ProcessBudgetClass(BudgetClasses(indxBudget),iStat)
      IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE PrintBudgetTables


  ! -------------------------------------------------------------
  ! --- PRINT OUT BUDGET TABLES FOR A BUDGET CLASS
  ! -------------------------------------------------------------
  SUBROUTINE ProcessBudgetClass(BudgetClass,iStat)
    TYPE(BudgetClassType) :: BudgetClass
    INTEGER,INTENT(OUT)   :: iStat
  
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'ProcessBudgetClass'
    INTEGER                      :: NPrintLoc,indx,ErrorCode
    TYPE(BudgetType)             :: Budget
    INTEGER,ALLOCATABLE          :: iPrintLocations(:)
    
    !Initialize
    iStat = 0
    
    !If no output is required, return
    IF (BudgetClass%NPrintLoc .EQ. 0) RETURN
    IF (BudgetClass%iPrintLocs(1) .EQ. 0) RETURN
    IF (BudgetClass%cBudgetOutputFileName .EQ. '') RETURN
    
    !Instantiate the budget data
    CALL Budget%New(BudgetClass%cBudgetInputFileName,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Inform user
    CALL LogMessage('Processing '//TRIM(Budget%GetDescriptor())//'.',f_iMessage,'')
    
    !Print locations
    IF (BudgetClass%iPrintLocs(1) .EQ. -1) THEN
      NPrintLoc = Budget%GetNLocations()
      CALL AllocArray(iPrintLocations,NPrintLoc,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
      iPrintLocations = (/(indx,indx=1,NPrintLoc)/)
    ELSE
      NPrintLoc = BudgetClass%NPrintLoc
      CALL AllocArray(iPrintLocations,NPrintLoc,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
      iPrintLocations = BudgetClass%iPrintLocs
      !Make sure that all print location indices are in range
      IF (ANY(iPrintLocations .GT. Budget%GetNLocations()) .OR. ANY(iPrintLocations .LT. -1)) THEN
          CALL SetLastMessage('One or more location indices for '//TRIM(Budget%GetDescriptor())//' printing is out of range!',f_iFatal,ThisProcedure) 
          iStat = -1
          RETURN
      END IF
    END IF
    
    !Print out budget tables
    CALL Budget%PrintResults(BudgetClass%cBudgetOutputFileName,Cache,FACTLTOU,FACTAROU,FACTVLOU,UNITLTOU,UNITAROU,UNITVLOU,PrintInterval,BudgetClass%cPrintTimeStep,iPrintLocations,iStat)

    !Clear memeory
    DEALLOCATE (iPrintLocations, STAT=ErrorCode)
    CALL Budget%Kill()
    
  END SUBROUTINE ProcessBudgetClass
  
  
  ! -------------------------------------------------------------
  ! --- END EXECUTION
  ! -------------------------------------------------------------
  SUBROUTINE EndExecution(iStat)
    INTEGER,INTENT(IN) :: iStat

     IF (iStat .EQ. -1) THEN
         CALL LogLastMessage()
     ELSE
         CALL LogMessage(f_cLineFeed//'Program completed successfully.',f_iMessage,'')
     END IF
     CALL StopTimer()
     CALL PrintRunTime()
     STOP
  
  END SUBROUTINE EndExecution
  
  
END MODULE