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
!
! This module uses DATE_AND_TIME function instead of CPU_TIME to compute the 
! elapsed run time.  This is because CPU_TIME does not produce correct results
! when the executable is run in parallel.  DATE_AND_TIME seems to be more robust 
! in sequential and parallel executions.
!
! Can Dogrul ; 1/22/08
!
!***********************************************************************
MODULE ProgramTimer
  IMPLICIT NONE

  LOGICAL,SAVE ::TimerStopped = .FALSE.
  LOGICAL,SAVE ::TimerStarted = .FALSE.
  INTEGER,SAVE ::StartTimeValues(8),EndTimeValues(8)

  PRIVATE
  PUBLIC :: StartTimer    ,&
            StopTimer     ,&
            GetRunTime    ,&
            TimerStopped  


CONTAINS


!------------------------------------------------------------------------------------

! *************************************************************
! *************************************************************
! **** START TIMER 
! *************************************************************
! *************************************************************
  SUBROUTINE StartTimer()
    
    CALL DATE_AND_TIME(VALUES = StartTimeValues)
    TimerStopped = .FALSE.
    TimerStarted = .TRUE.
    
  END SUBROUTINE StartTimer


!------------------------------------------------------------------------------------

! *************************************************************
! *************************************************************
! **** STOP TIMER 
! *************************************************************
! *************************************************************
  SUBROUTINE StopTimer()

    CALL DATE_AND_TIME(VALUES = EndTimeValues)
    TimerStopped = .TRUE.

  END SUBROUTINE StopTimer

!------------------------------------------------------------------------------------

! *************************************************************
! *************************************************************
! **** GET RUN TIME 
! *************************************************************
! *************************************************************
  SUBROUTINE GetRunTime(Hour,Minute,Second)
    INTEGER,INTENT(OUT) :: Hour,Minute
    REAL(8),INTENT(OUT) :: Second

    !Local variables
    INTEGER :: iJulianStart,iJulianEnd,iYear,iMonth,iDay
    REAL(8) :: RunTime,JulianStart,JulianEnd,rHour,rMin,rSec,rmSec

    !Initialize
    Hour   = 0
    Minute = 0
    Second = 0.0
    
    !If timer hasn't been started at all run time cannot be computed; return
    IF (.NOT. TimerStarted) RETURN
    
    !Start date and time in Julian days
    rHour        = REAL(StartTimeValues(5),8)  
    rMin         = REAL(StartTimeValues(6),8)
    rSec         = REAL(StartTimeValues(7),8)
    rmSec        = REAL(StartTimeValues(8),8)
    iDay         = StartTimeValues(3)
    iMonth       = StartTimeValues(2)
    iYear        = StartTimeValues(1)
    iJulianStart = iDay-32075+1461*(iYear+4800+(iMonth-14)/12)/4+367*(iMonth-2-(iMonth-14)/12*12)/12-3*((iYear+4900+(iMonth-14)/12)/100)/4
    JulianStart  = REAL(iJulianStart,8) + rHour/24.0 + rMin/1440.0 + rSec/86400.0 + rmSec/86400000.0
    
    !End date and time in Julian days
    rHour      = REAL(EndTimeValues(5),8)  
    rMin       = REAL(EndTimeValues(6),8)
    rSec       = REAL(EndTimeValues(7),8)
    rmSec      = REAL(EndTimeValues(8),8)
    iDay       = EndTimeValues(3)
    iMonth     = EndTimeValues(2)
    iYear      = EndTimeValues(1)
    iJulianEnd = iDay-32075+1461*(iYear+4800+(iMonth-14)/12)/4+367*(iMonth-2-(iMonth-14)/12*12)/12-3*((iYear+4900+(iMonth-14)/12)/100)/4
    JulianEnd  = REAL(iJulianEnd,8) + rHour/24.0 + rMin/1440.0 + rSec/86400.0 + rmSec/86400000.0
    
    RunTime = (JulianEnd-JulianStart)*86400.0  !In seconds
    Hour    = INT(RunTime/3600.0)
    Minute  = INT((RunTime-Hour*3600.0)/60.0)
    Second  = RunTime-Hour*3600.0-Minute*60.0

  END SUBROUTINE GetRunTime


  ! -------------------------------------------------------------
  ! --- CONVERT AN INTEGER TO TEXT
  ! -------------------------------------------------------------
  FUNCTION IntToText(Number) RESULT(Text)
    INTEGER,PARAMETER           :: MaxTextLength=20
    INTEGER,INTENT(IN)          :: Number
    CHARACTER(LEN=MaxTextLength):: Text

    !Local variables
    CHARACTER(LEN=6) :: FormatStatement
    WRITE (FormatStatement,'(A,I3,A)') '(I',MaxTextLength,')'
    
    WRITE (Text,FMT=FormatStatement) Number
    Text=ADJUSTL(Text)

  END FUNCTION IntToText

END MODULE