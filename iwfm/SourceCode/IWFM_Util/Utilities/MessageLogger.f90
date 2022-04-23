!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2019  
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
MODULE MessageLogger
  USE ProgramTimer
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
  PUBLIC :: SetLogFileName               , &
            SetFlagToEchoProgress        , &
            SetDefaultMessageDestination , &
            SetLastMessage               , &
            GetLastMessage               , &
            GetLogFileUnit               , &
            KillLogFile                  , &
            CloseMessageFile             , &
            LogMessage                   , &
            LogLastMessage               , &
            IsLogFileDefined             , &
            PrintRunTime                 , &
            MessageArray                 , &
            YesEchoProgress              , &
            NoEchoProgress               , &
            EchoProgress                 , &
            SCREEN_FILE                  , &
            SCREEN                       , &
            FILE                         , &
            iMessage                     , &
            iInfo                        , &
            iWarn                        , &
            iFatal    


  ! -------------------------------------------------------------
  ! --- FLAG DEFINITIONS TO ECHO PROGRAM PROGRESS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER      :: YesEchoProgress   = 1  , &
                            NoEchoProgress    = 0
  INTEGER,PROTECTED,SAVE :: iFlagEchoProgress = NoEchoProgress
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS FOR MESSAGE DESTINATION
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: SCREEN_FILE = 1 , &
                       SCREEN      = 2 , &
                       FILE        = 3


  ! -------------------------------------------------------------
  ! --- MESSAGE SEVERITY LEVELS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iMessage = 0 , &
                       iInfo    = 1 , &
                       iWarn    = 2 , &
                       iFatal   = 3

  
  ! -------------------------------------------------------------
  ! --- LAST SAVED MESSAGE
  ! -------------------------------------------------------------
  CHARACTER(LEN=5000),SAVE :: cLastMessage          = ''
  INTEGER,SAVE             :: iLastMessageType      = iInfo
  CHARACTER(LEN=200),SAVE  :: cLastMessageProcedure = ''
  
  
  ! -------------------------------------------------------------
  ! --- DATA DEFINITIONS
  ! -------------------------------------------------------------
  CHARACTER(LEN=13),PARAMETER :: ThisProcedure='MessageLogger'
  CHARACTER(LEN=1),PARAMETER  :: LineFeed = CHAR(10)
  CHARACTER(LEN=11),PARAMETER :: DefaultLogFileName='Message.log'
  CHARACTER(LEN=300)          :: MessageArray(200)
  LOGICAL,PROTECTED,SAVE      :: WarningsGenerated = .FALSE. , &
                                 ConsoleExists     = .TRUE.
  INTEGER,SAVE                :: DefaultMessageDestination = SCREEN_FILE

  TYPE LogFile
    PRIVATE
    INTEGER                  :: UnitN
    CHARACTER(:),ALLOCATABLE :: Name     
  END TYPE LogFile
  TYPE(LogFile),ALLOCATABLE,SAVE::ThisLogFile
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  !Overload the message logging methods
  INTERFACE LogMessage
      MODULE PROCEDURE LogSingleMessage
      MODULE PROCEDURE LogMessageArray
  END INTERFACE LogMessage
  
  !Overload saving the last message
  INTERFACE SetLastMessage
      MODULE PROCEDURE SetLastMessage_Array
      MODULE PROCEDURE SetLastMessage_Single
  END INTERFACE SetLastMessage



CONTAINS


    
    
! *************************************************************
! *************************************************************
! *************************************************************
! ***** CONSTRUCTOR
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- OPEN A NEW LOG FILE
  ! -------------------------------------------------------------
  SUBROUTINE MakeLogFile(LogFileName,iStat)
    CHARACTER(LEN=*),OPTIONAL :: LogFileName
    INTEGER,INTENT(OUT)       :: iStat

    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    iStat = 0

    ALLOCATE(ThisLogFile,STAT=ErrorCode)
    IF (ErrorCode.NE.0) THEN
        CALL PrimitiveErrorHandler('Error in opening log file! Memory for log file attributes cannot be allocated',iStat)  !-1 is returned for iStat
        RETURN
    END IF
      
    !If the log file is already open, do not open new log file
    IF (PRESENT(LogFileName)) THEN
      IF (IsLogFileOpen(LogFileName)) THEN
        ALLOCATE (CHARACTER(LEN(LogFileName)) :: ThisLogFile%Name)
        ThisLogFile%UnitN = GetFileUnitNumber(LogFileName)
        ThisLogFile%Name  = LogFileName
        RETURN
      END IF
    END IF

    ALLOCATE (CHARACTER(LEN(DefaultLogFileName)) :: ThisLogFile%Name)
    ThisLogFile%UnitN = GetAUnitNumber()
    ThisLogFile%Name  = DefaultLogFileName
    IF (PRESENT(LogFileName)) THEN
        DEALLOCATE (ThisLogFile%Name , STAT=ErrorCode)
        ALLOCATE (CHARACTER(LEN(LogFileName)) :: ThisLogFile%Name)
        ThisLogFile%Name = TRIM(ADJUSTL(LogFileName))
    END IF
    
    OPEN (UNIT=ThisLogFile%UnitN,FILE=ThisLogFile%Name,IOSTAT=ErrorCode)
    IF (ErrorCode.NE.0) CALL PrimitiveErrorHandler('Log file cannot be opened',iStat)
    
  END SUBROUTINE MakeLogFile

  
  

! *************************************************************
! *************************************************************
! *************************************************************
! ***** DESTRUCTOR
! *************************************************************
! *************************************************************
! *************************************************************
  
  ! -------------------------------------------------------------
  ! --- CLOSE LOG FILE
  ! -------------------------------------------------------------  
  SUBROUTINE KillLogFile()

    IF (ALLOCATED(ThisLogFile)) THEN
      CLOSE (ThisLogFile%UnitN)
      DEALLOCATE(ThisLogFile)
    END IF

  END SUBROUTINE KillLogFile

  
  

! *************************************************************
! *************************************************************
! *************************************************************
! ***** SETTERS
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- SET FLAG TO ECHO PROGRAM PROGRESS TO SCREEN OR NOT
  ! -------------------------------------------------------------
  SUBROUTINE SetFlagToEchoProgress(iFlag,iStat)
    INTEGER,INTENT(IN)  :: iFlag
    INTEGER,INTENT(OUT) :: iStat
    
    !Initialize
    iStat = 0
    
    !Check if iFlag is recognized
    IF (iFlag.NE.YesEchoProgress .AND. iFlag.NE.NoEchoProgress) THEN
        CALL SetLastMessage('Flag to echo program progress is not recognized!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Set flag
    iFlagEchoProgress = iFlag
    
  END SUBROUTINE SetFlagToEchoProgress
  

  ! -------------------------------------------------------------
  ! --- SET DEFAULT MESSAGE DESTINATION
  ! -------------------------------------------------------------
  SUBROUTINE SetDefaultMessageDestination(Destination,iStat)
    INTEGER,INTENT(IN)  :: Destination
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    LOGICAL::Test
    
    !Initilaize
    iStat = 0

    !Check if Destination is recognized
    Test=Destination .EQ. SCREEN      .OR.  &
         Destination .EQ. FILE        .OR.  &
         Destination .EQ. SCREEN_FILE     
    IF (.NOT. Test) THEN
      IF (ALLOCATED(ThisLogFile)) THEN 
        CALL SetLastMessage('Message destination is not recognized!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
      ELSE
        CALL PrimitiveErrorHandler('Message destination is not recognized!',iStat)
        RETURN
      END IF
    END IF

    !Set DefaultMessageDestination
    DefaultMessageDestination=Destination
    
  END SUBROUTINE SetDefaultMessageDestination
   

  ! -------------------------------------------------------------
  ! --- SET THE LOG FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE SetLogFileName(FileName,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: FileName
    INTEGER,INTENT(OUT)         :: iStat
    
    !Initilaize
    iStat = 0

    !Check if log file is already instantiated
    IF (ALLOCATED(ThisLogFile)) THEN
      CALL SetLastMessage('Error in opening new log file! A log file is already created',iFatal,ThisProcedure) 
      iStat = -1
    ELSE
      CALL MakeLogFile(FileName,iStat)     
    END IF
    
  END SUBROUTINE SetLogFileName

  
  ! -------------------------------------------------------------
  ! --- SET THE LAST MESSAGE AND MESSAGE LEVEL USING AN ARRAY OF MESSAGES
  ! -------------------------------------------------------------
  SUBROUTINE SetLastMessage_Array(cMessageArray,iErrorLevel,cProgName)
    CHARACTER(LEN=*),INTENT(IN) :: cMessageArray(:),cProgName
    INTEGER,INTENT(IN)          :: iErrorLevel
    
    !Local variables
    INTEGER :: indx
    
    !Initialize
    cLastMessage = '*   ' // cMessageArray(1)
    
    !Stich the message arrays into a single string
    DO indx=2,SIZE(cMessageArray)
        cLastMessage = TRIM(cLastMessage) // LineFeed // '*   ' // TRIM(cMessageArray(indx))
    END DO

    !Program name 
    cLastMessageProcedure = TRIM(cProgName) 
    
    !Error level
    iLastMessageType = iErrorLevel
    
  END SUBROUTINE SetLastMessage_Array
  
  
  ! -------------------------------------------------------------
  ! --- SET THE LAST MESSAGE AND MESSAGE LEVEL USING A SINGLE MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE SetLastMessage_Single(cMessage,iErrorLevel,cProgName)
    CHARACTER(LEN=*),INTENT(IN) :: cMessage,cProgName
    INTEGER,INTENT(IN)          :: iErrorLevel
    
    !Local variables
    CHARACTER(LEN=LEN_TRIM(cMessage)) :: cMessageArray(1)
    
    !Initialize
    cMessageArray(1) = TRIM(cMessage)
    
    CALL SetLastMessage_Array(cMessageArray,iErrorLevel,cProgName)

  END SUBROUTINE SetLastMessage_Single
  
  
  
  
! *************************************************************
! *************************************************************
! *************************************************************
! ***** GETTERS
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- GET THE LAST MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE GetLastMessage(cMessage)
    CHARACTER(LEN=*),INTENT(OUT) :: cMessage
    
    !Local variables
    INTEGER                                   :: iLenMessage,iLenLastMessage
    CHARACTER(LEN=LEN_TRIM(cLastMessage)+110) :: cMessageLocal
    
    SELECT CASE (iLastMessageType)
        CASE (iInfo)
            cMessageLocal = '* INFO:'  
        CASE (iWarn)
            cMessageLocal = '* WARN:' 
        CASE (iFatal)
            cMessageLocal = '* FATAL:' 
    END SELECT
    cMessageLocal = TRIM(cMessageLocal) // LineFeed // TRIM(cLastMessage)
    cMessageLocal = TRIM(cMessageLocal) // LineFeed // '*   (' // TRIM(cLastMessageProcedure) // ')'     
    
    iLenMessage     = LEN(cMessage)
    iLenLastMessage = LEN_TRIM(cMessageLocal)
    
    cMessage = ''
    IF (iLenMessage .LT. iLenLastMessage) THEN    
        cMessage = cMessageLocal(1:iLenMessage)
    ELSE
        cMessage(1:iLenLastMessage) = cMessageLocal
    END IF
    
  END SUBROUTINE GetLastMessage
  
  
  ! -------------------------------------------------------------
  ! --- GET THE LOG FILE UNIT
  ! -------------------------------------------------------------
  SUBROUTINE GetLogFileUnit(FileName,LogFileUnit,iStat)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: FileName
    INTEGER,INTENT(OUT)                  :: LogFileUnit,iStat
    
    !Initialize
    iStat = 0

    IF (.NOT.ALLOCATED(ThisLogFile)) THEN
      IF (PRESENT(FileName)) THEN
        CALL MakeLogFile(FileName,iStat)
      ELSE
        CALL MakeLogFile(iStat=iStat)
      ENDIF
    END IF
    IF (iStat .EQ. -1) RETURN
      
    LogFileUnit=ThisLogFile%UnitN
      
  END SUBROUTINE GetLogFileUnit


  
  
! *************************************************************
! *************************************************************
! *************************************************************
! ***** UTILITIES
! *************************************************************
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- RETRIEVE THE UNIT NUMBER OF AN ALREADY OPENED LOG FILE
  ! -------------------------------------------------------------
  FUNCTION GetFileUnitNumber(FileName) RESULT(Number)
    CHARACTER(LEN=*)::FileName
    INTEGER::Number

    !Retrieve
    INQUIRE (FILE=FileName,NUMBER=Number)

  END FUNCTION GetFileUnitNumber


  ! -------------------------------------------------------------
  ! --- CHECK IF A FILE TO BE USED AS LOG FILE IS ALREADY OPEN
  ! -------------------------------------------------------------
  FUNCTION IsLogFileOpen(FileName) RESULT(IsItOpen)
    CHARACTER(LEN=*),INTENT(IN)::FileName
    LOGICAL::IsItOpen

    !Check
    INQUIRE (FILE=FileName,OPENED=IsItOpen)

  END FUNCTION IsLogFileOpen


  ! -------------------------------------------------------------
  ! --- CHECK IF LOG FILE IS ALREADY DEFINED
  ! -------------------------------------------------------------
  FUNCTION IsLogFileDefined() RESULT(IsDefined)
    LOGICAL :: IsDefined

    IF (ALLOCATED(ThisLogFile)) THEN
        IsDefined = .TRUE.
    ELSE
        IsDefined = .FALSE.
    END IF

  END FUNCTION IsLogFileDefined


  ! -------------------------------------------------------------
  ! --- FIND AN UNCONNECTED UNIT NUMBER
  ! -------------------------------------------------------------
  FUNCTION GetAUnitNumber() RESULT(UnitNumber)
    INTEGER::UnitNumber

    !Local variables
    LOGICAL::opn

    DO UnitNumber=8,508
      INQUIRE (UNIT=UnitNumber,OPENED=opn)
      IF (.NOT.opn) EXIT
    END DO

  END FUNCTION GetAUnitNumber


  ! -------------------------------------------------------------
  ! --- PRIMITIVE ERROR HANDLER IF ERRORS OCCUR BEFORE LOG FILE IS OFFICIALLY OPEN
  ! -------------------------------------------------------------
  SUBROUTINE PrimitiveErrorHandler(Message,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: Message
    INTEGER,INTENT(OUT)         :: iStat

    CALL SetDefaultMessageDestination(SCREEN,iStat)
    CALL LogMessage(Message,iFatal,ThisProcedure)
    
    CALL SetLastMessage(Message,iFatal,ThisProcedure)
    iStat = -1

  END SUBROUTINE PrimitiveErrorHandler


  ! -------------------------------------------------------------
  ! --- PRINT OUT ALL TYPES OF MESSAGES (SINGLE VS. ARRAY)
  ! -------------------------------------------------------------
  SUBROUTINE LogAllMessageTypes(MessageArray,ErrorLevel,ProgName,Destination,Fmt,Advance)
    CHARACTER(LEN=*),INTENT(IN) :: MessageArray(:)
    CHARACTER(LEN=*),INTENT(IN) :: ProgName
    INTEGER,INTENT(IN)          :: ErrorLevel,Destination
    CHARACTER(LEN=*),INTENT(IN) :: Fmt,Advance

    !Local variables
    INTEGER                               :: UnitN,indx,NMessage,iStat
    LOGICAL                               :: WillPrintToFile,WillPrintToScreen
    CHARACTER(LEN=5+LEN(MessageArray(1))) :: SeveralMessages(SIZE(MessageArray))

    !Defaults
    WillPrintToFile   = .FALSE.
    WillPrintToScreen = .FALSE.
    SELECT CASE (Destination)
      CASE (SCREEN_FILE)
        WillPrintToFile   = .TRUE.
        WillPrintToScreen = .TRUE.
      CASE (FILE)
        WillPrintToFile   = .TRUE.
        WillPrintToScreen = .FALSE.
      CASE (SCREEN)
        WillPrintToFile   = .FALSE.
        WillPrintToScreen = .TRUE.
    END SELECT
    IF (WillPrintToScreen) CALL CheckConsoleAvailability()
    NMessage                    = SIZE(MessageArray)
    SeveralMessages(1:NMessage) = MessageArray

    !Check if a log file is instantiated
    IF (Destination .NE. SCREEN) THEN
      IF (.NOT. ALLOCATED(ThisLogFile)) CALL MakeLogFile(iStat=iStat)
    END IF

    !Modify messages based on the error level
    IF (ErrorLevel .NE. 0) THEN
      DO indx=1,NMessage
        SeveralMessages(indx) = '*   '//TRIM(SeveralMessages(indx))
      END DO
    END IF

    IF (ALLOCATED(ThisLogFile)) UnitN=ThisLogFile%UnitN
    !Evaluate error severity and print out message
    SELECT CASE (ErrorLevel)
      CASE DEFAULT
        IF (WillPrintToFile) THEN
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) ' '
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*'
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '* FATAL:' 
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*   Incorrect error level returned from procedure '//TRIM(ADJUSTL(ProgName))
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*'
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
        END IF
        !Always print to error unit
        WRITE (*,FMT=Fmt,ADVANCE=Advance) ' '
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*'
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '* FATAL:' 
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*   Incorrect error level returned from procedure '//TRIM(ADJUSTL(ProgName))
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*'
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
        CALL PrintRunTime()
        CALL CloseMessageFile()
        STOP 

      CASE (iMessage)
        IF (WillPrintToFile) CALL PrintMessageArray(FILE)
        IF (WillPrintToScreen) CALL PrintMessageArray(SCREEN)
      CASE (iInfo)
        WarningsGenerated = .TRUE.
        IF (WillPrintToFile) THEN
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '* INFO : '
          CALL PrintMessageArray(FILE)
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'
        END IF
        !Always print to screen
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '* INFO : '
        CALL PrintMessageArray(SCREEN)
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'
      CASE (iWarn)
        WarningsGenerated = .TRUE.
        IF (WillPrintToFile) THEN
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '* WARN : '
          CALL PrintMessageArray(FILE)
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'
        END IF
        !Always write to screen  
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '* WARN : '
        CALL PrintMessageArray(SCREEN)
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'

      CASE (iFatal)
        IF (WillPrintToFile) THEN
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) ' '
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '* FATAL: '
          CALL PrintMessageArray(FILE)
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
        END IF
        !Always print to error unit
        WRITE (*,FMT=Fmt,ADVANCE=Advance) ' '
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '* FATAL: '
        CALL PrintMessageArray(SCREEN)
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*   ('//TRIM(ADJUSTL(ProgName))//')'
        WRITE (*,FMT=Fmt,ADVANCE=Advance) '*******************************************************************************'
        CALL PrintRunTime()
        CALL CloseMessageFile()
        STOP
    END SELECT

  
  CONTAINS

    !PRINT MESSAGE ARRAY
    SUBROUTINE PrintMessageArray(iDest)
      INTEGER,INTENT(IN) :: iDest
      
      !Local variables
      INTEGER :: indxMessage
      
      !Print
      IF (iDest .EQ. FILE) THEN
        DO indxMessage=1,NMessage
          WRITE (UnitN,FMT=Fmt,ADVANCE=Advance) TRIM(SeveralMessages(indxMessage))
        END DO
      ELSE IF (iDest .EQ. SCREEN) THEN
        DO indxMessage=1,NMessage
          WRITE (*,FMT=Fmt,ADVANCE=Advance) TRIM(SeveralMessages(indxMessage))
        END DO
      END IF
            
    END SUBROUTINE PrintMessageArray

  END SUBROUTINE LogAllMessageTypes


  ! -------------------------------------------------------------
  ! --- GATEWAY SUBROUTINE FOR SINGLE MESSAGE LOGGING (integer destination)
  ! -------------------------------------------------------------
  SUBROUTINE LogSingleMessage(Message,ErrorLevel,ProgName,Destination,Fmt,Advance)
    CHARACTER(LEN=*),INTENT(IN) :: Message
    CHARACTER(LEN=*),INTENT(IN) :: ProgName
    INTEGER,INTENT(IN)          :: ErrorLevel
    INTEGER,OPTIONAL,INTENT(IN) :: Destination
    CHARACTER(LEN=*),OPTIONAL   :: Fmt,Advance

    !Local variables
    INTEGER                          :: LocalDestination
    CHARACTER(LEN=50)                :: LocalFmt
    CHARACTER(LEN=3)                 :: LocalAdvance
    CHARACTER(LEN=LEN_TRIM(Message)) :: LocalMessage(1)

    !Set variables
    LocalMessage(1) = TRIM(Message)
    IF (PRESENT(Destination)) THEN
      LocalDestination = Destination
    ELSE
      LocalDestination = DefaultMessageDestination
    END IF 
    
    IF (PRESENT(Fmt)) THEN
      LocalFmt = Fmt
    ELSE
      LocalFmt = '(A)' 
    END IF 
    
    IF (PRESENT(Advance)) THEN
      LocalAdvance = Advance
    ELSE
      LocalAdvance = 'YES'
    END IF 

    !Transfer control to LogAllMessageTypes
    CALL LogAllMessageTypes(LocalMessage,ErrorLevel=ErrorLevel,ProgName=ProgName,Destination=LocalDestination,Fmt=LocalFmt,Advance=LocalAdvance)
      
  END SUBROUTINE LogSingleMessage
 

  ! -------------------------------------------------------------
  ! --- GATEWAY SUBROUTINE FOR ARRAY OF MESSAGE LOGGING
  ! -------------------------------------------------------------
  SUBROUTINE LogMessageArray(Message,ErrorLevel,ProgName,Destination,Fmt,Advance)
    CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: Message
    CHARACTER(LEN=*),INTENT(IN)              :: ProgName
    INTEGER,INTENT(IN)                       :: ErrorLevel
    INTEGER,OPTIONAL,INTENT(IN)              :: Destination
    CHARACTER(LEN=*),OPTIONAL                :: Fmt,Advance

    !Local variables
    INTEGER::LocalDestination
    CHARACTER(LEN=50)::LocalFmt
    CHARACTER(LEN=3)::LocalAdvance

    !Set variables
    IF (PRESENT(Destination)) THEN
      LocalDestination = Destination
    ELSE
      LocalDestination = DefaultMessageDestination
    END IF 
    IF (PRESENT(Fmt)) THEN
      LocalFmt = Fmt
    ELSE
      LocalFmt = '(A)' 
    END IF 
    IF (PRESENT(Advance)) THEN
      LocalAdvance = Advance
    ELSE
      LocalAdvance = 'YES'
    END IF 

    !Transfer control to LogAllMessageTypes
    CALL LogAllMessageTypes(MessageArray=Message,ErrorLevel=ErrorLevel,ProgName=ProgName,Destination=LocalDestination,Fmt=LocalFmt,Advance=LocalAdvance)
      
  END SUBROUTINE LogMessageArray


  ! -------------------------------------------------------------
  ! --- PRINT OUT ALL THE LAST SAVED MESSAGE
  ! -------------------------------------------------------------
  SUBROUTINE LogLastMessage(Destination,Advance)
    INTEGER,OPTIONAL,INTENT(IN)          :: Destination
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Advance
    
    !Local variables
    INTEGER   :: Destination_Local
    CHARACTER :: Advance_Local*3,cMessageLocal*5000
    
    
    !Set destination
    IF (PRESENT(Destination)) THEN
        Destination_Local = Destination
    ELSE
        Destination_Local = DefaultMessageDestination
    END IF
  
    !Set advancement of line
    IF (PRESENT(Advance)) THEN
        Advance_Local = TRIM(Advance)
    ELSE
        Advance_Local = 'YES'
    END IF
    
    !Prepare message for logging
    cMessageLocal = '*******************************************************************************'
    SELECT CASE (iLastMessageType)
        CASE (iInfo)
            cMessageLocal = TRIM(cMessageLocal) //  LineFeed // '* INFO: '  
        CASE (iWarn)
            cMessageLocal = TRIM(cMessageLocal) // LineFeed // '* WARN: ' 
        CASE (iFatal)
            cMessageLocal = TRIM(cMessageLocal) // LineFeed // '* FATAL: ' 
    END SELECT
    cMessageLocal = TRIM(cMessageLocal) // LineFeed // TRIM(cLastMessage)
    cMessageLocal = TRIM(cMessageLocal) // LineFeed // '*   (' // TRIM(cLastMessageProcedure) // ')' // LineFeed // '*******************************************************************************'
    
    !Log the message
    CALL LogMessage(TRIM(cMessageLocal),iMessage,'',Destination=Destination_Local,Advance=Advance_Local)
    
  END SUBROUTINE LogLastMessage
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF A CONSOLE IS AVAILABLE FOR SCREEN OUTPUT
  ! --- Note: This procedure is made redundant for portability reasons (Can Dogrul; 09/23/2010)
  ! -------------------------------------------------------------
  SUBROUTINE CheckConsoleAvailability()
!   USE IFWIN, ONLY:  AllocConsole                 ,&
!                     GetStdHandle                 ,&
!                     STD_OUTPUT_HANDLE            ,&
!                     INVALID_HANDLE_VALUE 

!   !Local variables
!   LOGICAL::Status
!   INTEGER::fhandle

!   !Check if a console is available
!   fhandle=GetStdHandle(STD_OUTPUT_HANDLE)
!   IF (fhandle .NE. INVALID_HANDLE_VALUE) RETURN  !Available, do nothing

!   !If not available create one
!   Status=AllocConsole()
!   fhandle=GetStdHandle(STD_OUTPUT_HANDLE)

  END SUBROUTINE CheckConsoleAvailability


  ! -------------------------------------------------------------
  ! --- PRINT RUN TIME BEFORE STOPPING PROGRAM
  ! -------------------------------------------------------------
  SUBROUTINE PrintRunTime(iDestination)
    INTEGER,OPTIONAL,INTENT(IN) :: iDestination

    !Local variables
    INTEGER          :: Hour, Minute
    REAL(8)          :: Second
    CHARACTER(LEN=5) :: CHour,CMinute

    !Get the program run-time
    IF (.NOT. TimerStopped) CALL StopTimer()
    CALL GetRunTime(Hour,Minute,Second)
    WRITE (CHour,'(I5)') Hour
    WRITE (CMinute,'(I5)') Minute
    CHour   = ADJUSTL(CHour)
    CMinute = ADJUSTL(CMinute)

    !Prepare the final message
    MessageArray(1) = NEW_LINE('a') // REPEAT('*',50)
    MessageArray(2) = 'TOTAL RUN TIME: '
    IF (Hour.GT.0) THEN
        MessageArray(2) = TRIM(MessageArray(2))//' '// &
                          TRIM(CHour)//' HOURS '    // &
                          TRIM(CMinute)//' MINUTES '
    ELSE IF (Minute.GT.0) THEN
        MessageArray(2) = TRIM(MessageArray(2))//' '//TRIM(CMinute)//' MINUTES '
    END IF
    WRITE (MessageArray(2),'(A,1X,F6.3,A)') TRIM(MessageArray(2)),Second,' SECONDS'
    IF (WarningsGenerated) THEN
        MessageArray(3) = 'WARNINGS/INFORMATIONAL MESSAGES ARE GENERATED!'//LineFeed
        IF (ALLOCATED(ThisLogFile))  &
            MessageArray(3) = TRIM(MessageArray(3)) // 'FOR DETAILS CHECK FILE ''' // TRIM(ThisLogFile%Name) // '''.' // LineFeed
        MessageArray(3) = TRIM(MessageArray(3)) // REPEAT('*',50)
    ELSE                               
        MessageArray(3) = REPEAT('*',50)
    END IF

    !Print the final message
    IF (PRESENT(iDestination)) THEN
        CALL LogMessage(MessageArray(1:3),iMessage,'',Destination=iDestination)
    ELSE
        CALL LogMessage(MessageArray(1:3),iMessage,'')
    END IF

  END SUBROUTINE PrintRunTime


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO STOP THE PROGRAM BASED ON THE SCREEN OUTPUT DEVICE
  ! -------------------------------------------------------------
  SUBROUTINE CloseMessageFile()

    CALL KillLogFile()

  END SUBROUTINE CloseMessageFile
  
  
  ! -------------------------------------------------------------
  ! --- ECHO PROGRESS OF THE PROGRAM ONTO SCREEN
  ! -------------------------------------------------------------
  SUBROUTINE EchoProgress(Text,lAdvance)
    CHARACTER(LEN=*),INTENT(IN) :: Text
    LOGICAL,OPTIONAL,INTENT(IN) :: lAdvance
    
    !Local variables
    CHARACTER :: cAdvance*3
    
    !Initailize
    IF (PRESENT(lAdvance)) THEN
        IF (lAdvance) THEN
            cAdvance = 'YES'
        ELSE
            cAdvance = 'NO'
        END IF
    ELSE
        cAdvance = 'YES'
    END IF

    IF (iFlagEchoProgress .EQ. YesEchoProgress) CALL LogMessage(Text,iMessage,'',Advance=cAdvance)
        
  END SUBROUTINE EchoProgress
  
  
END MODULE