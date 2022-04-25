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
MODULE TimeSeriesUtilities
  USE MessageLogger
  USE GeneralUtilities
  IMPLICIT NONE

  INTEGER,PARAMETER                   :: ModNameLen = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'TimeSeriesUtilities::'

  INTEGER,PARAMETER :: f_iTimeStampLength=16
  INTEGER,SAVE      :: CacheLimit=500 !Default
  INTEGER,SAVE      :: SimulationTimeStep_InMinutes = 0 !Variable that stores the simulation time step length for the conversion of rate-type data read from ASCII file (default is zero)

  PRIVATE
  PUBLIC::TimeStepType                                      , &
          f_iTimeStampLength                                , &
          IsTimeStampValid                                  , &
          IsTimeIntervalValid                               , &
          IsLeapYear                                        , &
          StripTimeStamp                                    , &
          JulianToTimeStamp                                 , &
          JulianDateAndMinutesToTimeStamp                   , &
          TimeStampToJulianDateAndMinutes                   , &
          TimeStampToJulian                                 , &
          DayMonthYearToJulianDate                          , &
          JulianDateToDayMonthYear                          , &
          ExtractDay                                        , &
          ExtractMonth                                      , &
          ExtractYear                                       , &
          AdjustTimeStampWithYear4000                       , &
          TimeStampToYear4000                               , &
          IncrementTimeStamp                                , &
          IncrementJulianDateAndMinutesAfterMidnight        , &
          DSSStyleDate                                      , &
          DSSStyleHoursAfterMidnight                        , &
          NPeriods                                          , &
          OPERATOR(.TSLT.)                                  , &
          OPERATOR(.TSGT.)                                  , &
          OPERATOR(.TSGE.)                                  , &
          OPERATOR(.TULE.)                                  , &
          CTimeStep_To_RTimeStep                            , &
          TimeIntervalConversion                            , &
          SetSimulationTimeStep                             , &
          SetCacheLimit                                     , &
          SetTSDCacheSize                                   , &
          GetCacheLimit                                     , &
          AdjustRateTypeData                                , &
          LeapYearCorrection                                , &
          f_iRecognizedIntervals_InMinutes                  , &
          f_cRecognizedIntervals                            , &
          GetJulianDatesBetweenTimeStampsWithTimeIncrement


  ! -------------------------------------------------------------
  ! --- TIME STEP DATA TYPE
  ! -------------------------------------------------------------
  TYPE TimeStepType
    LOGICAL                           :: TrackTime          = .FALSE.
    REAL(8)                           :: CurrentTime        = 0.0
    REAL(8)                           :: EndTime            = 0.0
    INTEGER                           :: CurrentTimeStep    = 0
    CHARACTER(LEN=f_iTimeStampLength) :: CurrentDateAndTime = ''
    CHARACTER(LEN=f_iTimeStampLength) :: EndDateAndTime     = ''
    REAL(8)                           :: DeltaT             = 0.0
    INTEGER                           :: DeltaT_InMinutes   = 0
    CHARACTER(LEN=6)                  :: Unit               = ''
  END TYPE TimeStepType


  ! -------------------------------------------------------------
  ! --- PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iRecognizedIntervals_InMinutes(20) = [1,2,3,4,5,10,15,20,30,60,120,180,240,360,480,720,1440,10080,43200,525600]
  CHARACTER(LEN=6),PARAMETER :: f_cRecognizedIntervals(20) = ['1MIN  ' , &
                                                              '2MIN  ' , &
                                                              '3MIN  ' , &
                                                              '4MIN  ' , &
                                                              '5MIN  ' , &
                                                              '10MIN ' , &
                                                              '15MIN ' , &
                                                              '20MIN ' , &
                                                              '30MIN ' , &
                                                              '1HOUR ' , &
                                                              '2HOUR ' , &
                                                              '3HOUR ' , &
                                                              '4HOUR ' , &
                                                              '6HOUR ' , &
                                                              '8HOUR ' , &
                                                              '12HOUR' , &
                                                              '1DAY  ' , &
                                                              '1WEEK ' , &
                                                              '1MON  ' , &
                                                              '1YEAR ' ]


  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------

  !Overload methods for computing number of periods between two times or time stamps
  INTERFACE NPeriods
    MODULE PROCEDURE NPeriodsBetweenTimeStamps
    MODULE PROCEDURE NPeriodsBetweenTimes
  END INTERFACE

  !Define .LE. operator for time unit comparison
  INTERFACE OPERATOR(.TULE.)
    MODULE PROCEDURE TimeUnit_CheckForLessThanOrEqualTo
  END INTERFACE

  !Define .LT. operator for time stamp comparison
  INTERFACE OPERATOR(.TSLT.)
    MODULE PROCEDURE CheckForLessThan
  END INTERFACE

  !Define .GT. operator for time stamp comparison
  INTERFACE OPERATOR(.TSGT.)
    MODULE PROCEDURE CheckForGreaterThan
  END INTERFACE

  !Define .GE. operator for time stamp comparison
  INTERFACE OPERATOR(.TSGE.)
    MODULE PROCEDURE CheckForGreaterThanOrEqualTo
  END INTERFACE

  !Conversion to DSS-style date
  INTERFACE DSSStyleDate
    MODULE PROCEDURE DSSStyleDate_FromTimeStamp
    MODULE PROCEDURE DSSStyleDate_FromDayMonthYear
  END INTERFACE


CONTAINS


!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!                     METHODS FOR TIME STAMP MANIPULATIONS
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////

  ! -------------------------------------------------------------
  ! --- FUNCTION TO CHECK IF A TIEM INTERVAL IS RECOGNIZED
  ! -------------------------------------------------------------
  FUNCTION IsTimeIntervalValid(cInterval) RESULT(iIndex)
    CHARACTER(LEN=*),INTENT(IN) :: cInterval
    INTEGER                     :: iIndex

    !Do we have the cInterval in the recgnized interals list?
    DO iIndex=1,SIZE(f_cRecognizedIntervals)
        IF (TRIM(UpperCase(ADJUSTL(cInterval))) .EQ. TRIM(f_cRecognizedIntervals(iIndex))) EXIT
    END DO

    IF (iIndex .GT. SIZE(f_cRecognizedIntervals)) iIndex = 0

  END FUNCTION IsTimeIntervalValid


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CHECK IF A YEAR IS LEAP YEAR
  ! -------------------------------------------------------------
  FUNCTION IsLeapYear(iYear) RESULT(lLeapYear)
    INTEGER,INTENT(IN) :: iYear
    LOGICAL            :: lLeapYear

    IF (MODULO(iYear,400) .EQ. 0) THEN
      lLeapYear = .TRUE.
    ELSE IF (MODULO(iYear,100) .EQ. 0) THEN
      lLeapYear = .FALSE.
    ELSE IF (MODULO(iYear,4) .EQ. 0) THEN
      lLeapYear = .TRUE.
    ELSE
      lLeapYear = .FALSE.
    END IF

  END FUNCTION IsLeapYear


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CONVERT  JULIAN DAY TO DAY, MONTH, YEAR
  ! -------------------------------------------------------------
  SUBROUTINE JulianDateToDayMonthYear(iJulian,iDay,iMonth,iYear,iStat)
    INTEGER,INTENT(IN)           :: iJulian
    INTEGER,INTENT(OUT)          :: iDay,iMonth,iYear
    INTEGER,OPTIONAL,INTENT(OUT) :: iStat

    !Local variables
    INTEGER :: L,N

    L      = iJulian+68569
    N      = 4*L/146097
    L      = L-(146097*N+3)/4
    iYear  = 4000*(L+1)/1461001
    L      = L-1461*iYear/4+31
    iMonth = 80*L/2447
    iDay   = L-2447*iMonth/80
    L      = iMonth/11
    iMonth = iMonth+2-12*L
    iYear  = 100*(N-49)+iYear+L

    IF (PRESENT(iStat)) iStat = 0


  END SUBROUTINE JulianDateToDayMonthYear


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CONVERT DAY, MONTH, YEAR TO JULIAN DAY STARTING FROM 1 Jan 0001
  ! -------------------------------------------------------------
  SUBROUTINE DayMonthYearToJulianDate(iDay,iMonth,iYear,iJulian,iStat)
    INTEGER,INTENT(IN)           :: iDay,iMonth,iYear
    INTEGER,INTENT(OUT)          :: iJulian
    INTEGER,OPTIONAL,INTENT(OUT) :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'DayMonthYearToJulianDate'
    INTEGER                      :: ErrorCode

    !Initialize
    IF (PRESENT(iStat)) iStat = 0

    !Make sure iYear is not less than 1
    IF (iYear .LT. 1) THEN
      IF (PRESENT(iStat)) THEN
        iStat = 1
        RETURN
      ELSE
        CALL LogMessage('Cannot convert dates with calendar year less than 1 to Julian day!',f_iFatal,ThisProcedure)
      END IF
    END IF

    !Make sure iMonth is between 1 and 12
    IF (iMonth.LT.1  .OR.  iMonth.GT.12) THEN
      IF (PRESENT(iStat)) THEN
        iStat = 1
        RETURN
      ELSE
        CALL LogMessage('Incorrect number for month ('//TRIM(IntToText(iMonth))//')!',f_iFatal,ThisProcedure)
      END IF
    END IF

    !Make sure day is correct
    ErrorCode = 0
    SELECT CASE (iMonth)
      CASE (1,3,5,7,8,10,12)
        IF (iDay.LT.1 .OR. iDay.GT.31) ErrorCode = 1

      CASE (4,6,9,11)
        IF (iDay.LT.1 .OR. iDay.GT.30) ErrorCode = 1

      CASE (2)
        IF (IsLeapYear(iYear)) THEN
          IF (iDay.LT.1 .OR. iDay.GT.29) ErrorCode = 1
        ELSE
          IF (iDay.LT.1 .OR. iDay.GT.28) ErrorCode = 1
        END IF

    END SELECT
    IF (ErrorCode .NE. 0) THEN
      IF (PRESENT(iStat)) THEN
        iStat = 1
        RETURN
      ELSE
        CALL LogMessage('Day ('//TRIM(IntToText(iDay))//') of the month is incorrect given the month ('//TRIM(IntToText(iMonth))//')!',f_iFatal,ThisProcedure)
      END IF
    END IF

    !Convert
    iJulian = iDay-32075+1461*(iYear+4800+(iMonth-14)/12)/4+367*(iMonth-2-(iMonth-14)/12*12)/12-3*((iYear+4900+(iMonth-14)/12)/100)/4

  END SUBROUTINE DayMonthYearToJulianDate


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CORRECT A TIME STAMP FOR LEAP YEAR
  ! -------------------------------------------------------------
  FUNCTION LeapYearCorrection(TimeStampIn) RESULT(TimeStampOut)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStampIn
    CHARACTER(LEN=f_iTimeStampLength)            :: TimeStampOut

    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'LeapYearCorrection'
    INTEGER                      :: iYear

    !Initialize
    TimeStampOut = TimeStampIn

    !Check if time stamp refers to February 29
    IF (ExtractMonth(TimeStampIn) .NE. 2) RETURN
    IF (ExtractDay(TimeStampIn) .NE. 29) RETURN

    !Get the year
    iYear = ExtractYear(TimeStampIn)

    !Make sure that year of time stamp is not the Year 4000 flag
    IF (iYear .EQ. 4000) THEN
      MessageArray(1)='Time stamp '//TimeStampIn//' cannot be corrected for '
      MessageArray(2)='leap year because it includes Year 4000 flag!'
      CALL LogMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
    END IF

    !Correct February 29 if it is a leap year
    IF (.NOT. IsLeapYear(iYear)) TimeStampOut(4:5) = '28'

  END FUNCTION LeapYearCorrection


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CHECK IF THE TIME-STAMP OF A DATA IS VALID
  ! -------------------------------------------------------------
  FUNCTION IsTimeStampValid(ALine) RESULT (TimeStampValidity)
    CHARACTER(LEN=*),INTENT(IN) :: ALine
    LOGICAL                     :: TimeStampValidity

    !Local variables
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp

    !Initialize
    TimeStamp         = StripTimeStamp(ALine)
    TimeStampValidity = .FALSE.      !Default value

    !Check if possible time stamp includes two of "/"
    IF (CountOccurance('/',TimeStamp).NE.2) RETURN

    !Check if possible time stamp includes one of "_"
    IF (CountOccurance('_',TimeStamp).NE.1) RETURN

    !Check if possible time stamp includes one of ":"
    IF (CountOccurance(':',TimeStamp).LT.1) RETURN

    !Check time stamp has the right number of characaters
    IF (LEN_TRIM(ADJUSTL(TimeStamp)) .NE. f_iTimeStampLength) RETURN

    !If made it all the way here, the time stamp is valid
    TimeStampValidity = .TRUE.

  END FUNCTION IsTimeStampValid


  ! -------------------------------------------------------------
  ! --- FUNCTION TO STRIP TIME STAMP FROM A STRING
  ! -------------------------------------------------------------
  FUNCTION StripTimeStamp(ALine,Location) RESULT(TimeStamp)
    CHARACTER(LEN=*),INTENT(IN)       :: ALine
    INTEGER,OPTIONAL                  :: Location
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp

    !Local variables
    INTEGER :: LocationOfFirstSlash,TimeStampBegin,TimeStampEnd

    TimeStamp = '' !Default
    IF (PRESENT(Location)) Location = 0 !Default

    !Check if the line includes "/"
    LocationOfFirstSlash=FirstLocation('/',ALine)
    IF (LocationOfFirstSlash.EQ.0) RETURN  !No slashes found; therefore the data is not time stamped

    !Isolate the possible time stamp from the line
    TimeStampBegin = LocationOfFirstSlash-2                   ;  TimeStampBegin = MAX(TimeStampBegin , 1)
    TimeStampEnd   = TimeStampBegin + f_iTimeStampLength - 1  ;  TimeStampEnd   = MAX(TimeStampEnd , TimeStampBegin)
    TimeStamp      = ALine(TimeStampBegin:TimeStampEnd)

    IF (PRESENT(Location)) Location=TimeStampBegin

  END FUNCTION StripTimeStamp


  ! -------------------------------------------------------------
  ! --- FUNCTION TO EXTRACT MONTH FROM TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION ExtractMonth(TimeStamp) RESULT(Month)
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp
    INTEGER                           :: Month

    Month = TextToInt(TimeStamp(1:2))

  END FUNCTION ExtractMonth


  ! -------------------------------------------------------------
  ! --- FUNCTION TO EXTRACT DAY FROM TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION ExtractDay(TimeStamp) RESULT(Day)
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp
    INTEGER                           :: Day

    Day = TextToInt(TimeStamp(4:5))

  END FUNCTION ExtractDay


  ! -------------------------------------------------------------
  ! --- FUNCTION TO EXTRACT YEAR FROM TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION ExtractYear(TimeStamp) RESULT(Year)
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp
    INTEGER                           :: Year

    Year = TextToInt(TimeStamp(7:10))

  END FUNCTION ExtractYear


  ! -------------------------------------------------------------
  ! --- FUNCTION TO EXTRACT HOUR FROM TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION ExtractHour(TimeStamp) RESULT(Hour)
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp
    INTEGER                           :: Hour

    Hour = TextToInt(TimeStamp(12:13))

  END FUNCTION ExtractHour


  ! -------------------------------------------------------------
  ! --- FUNCTION TO EXTRACT MINUTE FROM TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION ExtractMinute(TimeStamp) RESULT(Minute)
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp
    INTEGER                           :: Minute

    Minute = TextToInt(TimeStamp(15:16))

  END FUNCTION ExtractMinute


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO EXTRACT JULIAN DATE AND MINUTES PAST THE MIDNIGHT FROM TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE TimeStampToJulianDateAndMinutes(TimeStamp,JulianDate,MinutesAfterMidnight,STAT)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp
    INTEGER,INTENT(OUT)                          :: JulianDate,MinutesAfterMidnight
    INTEGER,OPTIONAL                             :: STAT

    !Local variables
    CHARACTER(LEN=ModNameLen+31) :: ThisProcedure = ModName // 'TimeStampToJulianDateAndMinutes'
    INTEGER                      :: ErrorCode

    !Convert date to Julian date
    CALL DayMonthYearToJulianDate(ExtractDay(TimeStamp),ExtractMonth(TimeStamp),ExtractYear(TimeStamp),JulianDate,ErrorCode)

    !Handle error code
    IF (PRESENT(STAT)) THEN
      STAT=ErrorCode
    ELSE
      IF (ErrorCode.NE.0) CALL LogMessage('Error in converting simulation date to Julian date',3,ThisProcedure)
    END IF

    !Compute minutes past midnight in the day specified in the time stamp
    MinutesAfterMidnight=ExtractHour(TimeStamp)*60+ExtractMinute(TimeStamp)

  END SUBROUTINE TimeStampToJulianDateAndMinutes


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CONVERT DAY,MONTH, YEAR TO DSS STYLE DATE
  ! -------------------------------------------------------------
  FUNCTION DSSStyleDate_FromDayMonthYear(iDay,iMon,iYear) RESULT(Date)
    INTEGER,INTENT(IN) :: iDay,iMon,iYear
    CHARACTER(LEN=11)  :: Date

    !Local variables
    CHARACTER(LEN=3),DIMENSION(12),PARAMETER::Months=(/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/)

    Date = ''
    IF (iDay .LT. 10) THEN
      Date = '0'//TRIM(IntToText(iDay))//TRIM(Months(iMon))//TRIM(IntToText(iYear))
    ELSE
      Date = TRIM(IntToText(iDay))//TRIM(Months(iMon))//TRIM(IntToText(iYear))
    END IF

  END FUNCTION DSSStyleDate_FromDayMonthYear


  ! -------------------------------------------------------------
  ! --- FUNCTION TO EXTRACT DSS STYLE DATE FROM TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION DSSStyleDate_FromTimeStamp(TimeStamp) RESULT(Date)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp
    CHARACTER(LEN=11)                            :: Date

    !Local variables
    CHARACTER(LEN=3),DIMENSION(12),PARAMETER :: f_cMonths=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    INTEGER                                  :: iDay,iMonth,iYear

    iDay   = ExtractDay(TimeStamp)
    iMonth = ExtractMonth(TimeStamp)
    iYear  = ExtractYear(TimeStamp)
    Date   = TRIM(IntToText(iDay))//' '//TRIM(f_cMonths(iMonth))//' '//TRIM(IntToText(iYear))

  END FUNCTION DSSStyleDate_FromTimeStamp


  ! -------------------------------------------------------------
  ! --- FUNCTION TO EXTRACT DSS STYLE HOURS AFTER MIDNIGHT FROM TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION DSSStyleHoursAfterMidnight(TimeStamp) RESULT(HoursAfterMidnight)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp
    CHARACTER(LEN=4)                             :: HoursAfterMidnight

    !Generate HoursAfterMidnight
    HoursAfterMidnight=TimeStamp(12:13)//TimeStamp(15:16)

  END FUNCTION DSSStyleHoursAfterMidnight


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CONVERT JULIAN DATE AND MINUTES AFTER MIDNIGHT TO TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION JulianDateAndMinutesToTimeStamp(JulianDate,MinutesAfterMidnight) RESULT(TimeStamp)
    INTEGER,INTENT(IN)                :: JulianDate,MinutesAfterMidnight
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp

    !Local variables
    INTEGER :: Day,Month,Year,Minute,Hour,TempJulianDate

    !Initialize
    TimeStamp='00/00/0000_00:00'

    !Find number of hours
    Hour=MinutesAfterMidnight/60
    IF (Hour.GT.0) THEN
      IF (Hour.LT.10) THEN
        TimeStamp(13:13)=TRIM(IntToText(Hour))
      ELSE
        TimeStamp(12:13)=TRIM(IntTotext(Hour))
      END IF
    END IF

    !Find number of minutes
    Minute=MinutesAfterMidnight-Hour*60
    IF (Minute.GT.0) THEN
      IF (Minute.LT.10) THEN
        TimeStamp(16:16)=TRIM(IntToText(Minute))
      ELSE
        TimeStamp(15:16)=TRIM(IntTotext(Minute))
      END IF
    END IF

    !Check for midnight
    TempJulianDate=JulianDate
    IF (Hour.EQ.0 .AND. Minute.EQ.0) THEN
      TimeStamp(12:13)='24'
      TempJulianDate=JulianDate-1
    END IF

    !Convert Julian day to day, month, year
    CALL JulianDateToDayMonthYear(TempJulianDate,Day,Month,Year)
    IF (Month.LT.10) THEN
      TimeStamp(2:2)=TRIM(IntToText(Month))
    ELSE
      TimeStamp(1:2)=TRIM(IntTotext(Month))
    END IF
    IF (Day.LT.10) THEN
      TimeStamp(5:5)=TRIM(IntToText(Day))
    ELSE
      TimeStamp(4:5)=TRIM(IntTotext(Day))
    END IF
    TimeStamp(7:10)=Trim(IntToText(Year))

  END FUNCTION JulianDateAndMinutesToTimeStamp


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CONVERT TIME STAMP TO FRACTIONAL JULIAN TIME
  ! -------------------------------------------------------------
  FUNCTION TimeStampToJulian(TimeStamp,STAT) RESULT(Julian)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp
    INTEGER,OPTIONAL                             :: STAT
    REAL(8)                                      :: Julian

    !Local variables
    INTEGER::JulianDate,Minutes,LocalStat

    !Convert time stamp to Julian date and minutes after midnight
    CALL TimeStampToJulianDateAndMinutes(TimeStamp,JulianDate,Minutes,STAT=LocalStat)

    !Compute fractiona Julian time
    Julian=REAL(JulianDate,8)+REAL(Minutes,8)/1440d0

    !Take care of returned error code
    IF (PRESENT(STAT))  STAT=LocalStat

  END FUNCTION TimeStampToJulian


  ! -------------------------------------------------------------
  ! --- FUNCTION TO CONVERT FRACTIONAL JULIAN TIME TO TIME STAMP
  ! -------------------------------------------------------------
  FUNCTION JulianToTimeStamp(Julian) RESULT(TimeStamp)
    REAL(8),INTENT(IN)                :: Julian
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStamp

    !Local variables
    INTEGER::JulianDate,MinutesAfterMidnight

    !Get the integer date part of the fractional Julian date
    JulianDate = INT(Julian)

    !Get the minutes after midnight
    MinutesAfterMidnight = INT(1440d0 * (Julian - REAL(JulianDate,8)))

    TimeStamp = JulianDateAndMinutesToTimeStamp(JulianDate,MinutesAfterMidnight)

  END FUNCTION JulianToTimeStamp


  ! -------------------------------------------------------------
  ! --- GETE A LIST OF JULIAN DATES BETWEEN TWO TIME STAMPS USING A TIME INCREMENT
  ! -------------------------------------------------------------
  SUBROUTINE GetJulianDatesBetweenTimeStampsWithTimeIncrement(Interval_InMinutes,cBeginDateAndTime,cEndDateAndTime,rJulianDates)
    INTEGER,INTENT(IN)          :: Interval_InMinutes
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime,cEndDateAndTime
    REAL(8),INTENT(OUT)         :: rJulianDates(:)

    !Local variables
    INTEGER                           :: indx,ErrorCode
    CHARACTER(LEN=f_iTimeStampLength) :: cTimeStamp

    cTimeStamp = cBeginDateAndTime
    DO indx=1,SIZE(rJulianDates)
        rJulianDates(indx) = TimeStampToJulian(cTimeStamp,ErrorCode)
        cTimeStamp         = IncrementTimeStamp(cTimeStamp,Interval_InMinutes,1)
        IF (cTimeStamp .TSGT. cEndDateAndTime) THEN
            rJulianDates(indx+1:) = 0.0
            EXIT
        END IF
    END DO

  END SUBROUTINE GetJulianDatesBetweenTimeStampsWithTimeIncrement


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO ADJUST THE YEAR 4000 FLAG WITH THE YEAR OF ANOTHER TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE AdjustTimeStampWithYear4000(AdjustedTimeStamp,TimeStamp,Year4000Flag)
    CHARACTER(LEN=f_iTimeStampLength)            :: AdjustedTimeStamp
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp
    LOGICAL,INTENT(OUT)                          :: Year4000Flag

    !Local variables
    INTEGER::Year
    !Default
    Year4000Flag=.FALSE.

    !If year in AdjustedTimeStamp is 4000, replace it with the year in TimeStamp
    IF (ExtractYear(AdjustedTimeStamp).EQ.4000) THEN
      Year4000Flag=.TRUE.
      !Extract year from TimeStamp
      Year=ExtractYear(TimeStamp)
      !Replace Year4000 flag in AdjustedTimeStamp by year from TimeStamp
      AdjustedTimeStamp(7:10)=TRIM(IntToText(Year))
    END IF

  END SUBROUTINE AdjustTimeStampWithYear4000


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO CONVERT TIME STAMP TO TIME STAMP WITH YEAR 4000 FLAG
  ! -------------------------------------------------------------
  FUNCTION TimeStampToYear4000(TimeStamp) RESULT(ConvertedTimeStamp)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp
    CHARACTER(LEN=f_iTimeStampLength)            :: ConvertedTimeStamp

    ConvertedTimeStamp=TimeStamp
    ConvertedTimeStamp(7:10)=TRIM(IntToText(4000))

  END FUNCTION TimeStampToYear4000


  ! -------------------------------------------------------------
  ! --- FUNCTION TO INCREMENT A TIME STAMP BY A CERTAIN NUMBER OF MINUTES
  ! -------------------------------------------------------------
  FUNCTION IncrementTimeStamp(TimeStamp,Interval_InMinutes,NumberOfIntervals) RESULT(IncrementedTimeStamp)
    CHARACTER(LEN=*),INTENT(IN)       :: TimeStamp
    INTEGER,INTENT(IN)                :: Interval_InMinutes
    INTEGER,INTENT(IN),OPTIONAL       :: NumberOfIntervals
    CHARACTER(LEN=f_iTimeStampLength) :: IncrementedTimeStamp

    !Local variables
    INTEGER :: Sign,LocalMinutes,JulianDate,MinutesAfterMidnight,EndJulianDate,EndMinutesAfterMidnight,LocalNumberOfIntervals

    !Initialize
    LocalNumberOfIntervals=1  ;  IF (PRESENT(NumberOfIntervals)) LocalNumberOfIntervals=NumberOfIntervals

    !Convert TimeStamp to integer JulianDate and MinutesAfterMidnight
    CALL TimeStampToJulianDateAndMinutes(TimeStamp,JulianDate,MinutesAfterMidnight)

    !Increment Julian date and minutes past the midnight
    Sign=1
    LocalMinutes=Interval_InMinutes
    IF (Interval_InMinutes.LT.0) THEN
      Sign=-1
      LocalMinutes=ABS(Interval_InMinutes)
    END IF
    CALL IncrementJulianDateAndMinutesAfterMidnight(LocalMinutes,Sign*LocalNumberOfIntervals,JulianDate,MinutesAfterMidnight,EndJulianDate,EndMinutesAfterMidnight)
    JulianDate           = EndJulianDate
    MinutesAfterMidnight = EndMinutesAfterMidnight

    !Convert integer JulianDate and MinutesAfterMidnight to TimeStamp
    IncrementedTimeStamp=JulianDateAndMinutesToTimeStamp(JulianDate,MinutesAfterMidnight)

  END FUNCTION IncrementTimeStamp


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO INCREMENT A JULIAN DATE AND MINUTES-AFTER-MIDNIGHT BY A CERTAIN NUMBER OF MINUTES
  ! -------------------------------------------------------------
  SUBROUTINE IncrementJulianDateAndMinutesAfterMidnight(Interval_InMinutes,NumberOfIntervals,iBeginJulianDate,iBeginMinutesAfterMidnight,iEndJulianDate,iEndMinutesAfterMidnight)
    INTEGER,INTENT(IN)  :: Interval_InMinutes,NumberOfIntervals,iBeginJulianDate,iBeginMinutesAfterMidnight
    INTEGER,INTENT(OUT) :: iEndJulianDate,iEndMinutesAfterMidnight

    !Local variables
    INTEGER           :: NDays,NMinutes,AllMinutes,iDay,iMonth,iYear,NYears,NMonths
    LOGICAL           :: lLastDay
    INTEGER,PARAMETER :: DaysInMonth(12) = (/31,29,31,30,31,30,31,31,30,31,30,31/)

    !Initialize
    iEndJulianDate           = iBeginJulianDate
    iEndMinutesAfterMidnight = iBeginMinutesAfterMidnight

    !Increment
    SELECT CASE (Interval_InMinutes)
      !Interval is less than or equal to a week
      CASE (:10080)
        !Amount of minutes to increment
        AllMinutes = Interval_InMinutes * NumberOfIntervals

        !Number of days
        NDays = AllMinutes / 1440

        !Number of minutes
        NMinutes = AllMinutes - (NDays * 1440)

        !Incremented values
        iEndJulianDate           = iBeginJulianDate + NDays
        iEndMinutesAfterMidnight = iBeginMinutesAfterMidnight + NMinutes

        !Make sure MinutesAfterMidnight is okay
        IF (iEndMinutesAfterMidnight .GT. 1440) THEN
          iEndJulianDate           = iEndJulianDate + 1
          iEndMinutesAfterMidnight = iEndMinutesAfterMidnight - 1440
        ELSEIF (iEndMinutesAfterMidnight .LT. 1) THEN
          iEndJulianDate           = iEndJulianDate - 1
          iEndMinutesAfterMidnight = 1440 + iEndMinutesAfterMidnight
        END IF

      !Interval is month or year
      CASE (40320:)
        !Find day, month, year of beginning Julian date
        CALL JulianDateToDayMonthYear(iBeginJulianDate,iDay,iMonth,iYear)

        !Are we at the last day of month?
        lLastDay = .FALSE.
        SELECT CASE (iMonth)
          CASE (1,3,5,7,8,10,12)
            IF (iDay .EQ. 31) lLastDay = .TRUE.
          CASE (4,6,9,11)
            IF (iDay .EQ. 30) lLastDay = .TRUE.
          CASE (2)
            IF (IsLeapYear(iYear)) THEN
              IF (iDay .EQ. 29) lLastDay = .TRUE.
            ELSE
              IF (iDay .EQ. 28) lLastDay = .TRUE.
            END IF
        END SELECT

        !Find number of years and months to increment
        IF (Interval_InMinutes.GE.40320  .AND.  Interval_InMinutes.LE.44640) THEN
          NYears  = NumberOfIntervals / 12
          NMonths = NumberOfIntervals - NYears*12
        ELSE
          NYears  = NumberOfIntervals
          NMonths = 0
        END IF

        !Now, increment
        iYear  = iYear + NYears
        iMonth = iMonth + NMonths

        !Make sure month is okay
        IF (iMonth .GT. 12) THEN
          iYear  = iYear + 1
          iMonth = iMonth - 12
        ELSEIF (iMonth .LT. 1) THEN
          iYear  = iYear - 1
          iMonth = 12 + iMonth
        END IF

        !Set the day properly
        IF (lLastDay) THEN
          IF (iBeginMinutesAfterMidnight .EQ. 1440) THEN
            IF (iMonth .EQ. 2) THEN
              IF (IsLeapYear(iYear)) THEN
                iDay = 29
              ELSE
                iDay = 28
              END IF
            ELSE
              iDay = DaysInMonth(iMonth)
            END IF
          END IF
        END IF

        !If incremented month is 2, make sure that day is not greater than 28 or 29
        IF (iMonth .EQ. 2) THEN
          IF (iDay .GT. 28) THEN
            IF (IsLeapYear(iYear)) THEN
              iDay = 29
            ELSE
              iDay = 28
            END IF
          END IF
        END IF

        !Now compute ending Julian Date
        CALL DayMonthYearToJulianDate(iDay,iMonth,iYear,iEndJulianDate)
        iEndMinutesAfterMidnight = iBeginMinutesAfterMidnight

    END SELECT

  END SUBROUTINE IncrementJulianDateAndMinutesAfterMidnight




!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!                     METHODS FOR TIME SERIES DATA
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////

  ! -------------------------------------------------------------
  ! --- SET SIMULATION TIME STEP LENGTH
  ! -------------------------------------------------------------
  SUBROUTINE SetSimulationTimeStep(DELTAT_InMinutes)
    INTEGER,INTENT(IN)::DELTAT_InMinutes

    SimulationTimeStep_InMinutes=DELTAT_InMinutes

  END SUBROUTINE SetSimulationTimeStep


  ! -------------------------------------------------------------
  ! --- SET THE CACHE SIZE FOR THE TIME SERIES DATA OUTPUT
  ! -------------------------------------------------------------
  SUBROUTINE SetCacheLimit(Cache)
    INTEGER,INTENT(IN)::Cache

    CacheLimit=Cache

  END SUBROUTINE SetCacheLimit


  ! -------------------------------------------------------------
  ! --- GET THE CACHE SIZE FOR THE TIME SERIES DATA OUTPUT
  ! -------------------------------------------------------------
  FUNCTION GetCacheLimit() RESULT(iCache)
    INTEGER :: iCache

    iCache = CacheLimit

  END FUNCTION GetCacheLimit


  ! -------------------------------------------------------------
  ! --- SET THE NumberOfDataBatch, NumberOfDataRows AND NumberOfDataColumns FOR STORAGE OF TIME SERIES DATA BEFORE PRINT-OUT
  ! -------------------------------------------------------------
  SUBROUTINE SetTSDCacheSize(FileName            , &
                             ValuesForOutput     , &
                             NumberOfDataBatch   , &
                             NumberOfDataRows    , &
                             NumberOfDataColumns , &
                             NColumnsOfData      , &
                             NRowsOfData         , &
                             TimeArray           )
    CHARACTER(LEN=*),INTENT(IN)           :: FileName
    REAL(8),ALLOCATABLE                   :: ValuesForOutput(:,:,:)
    INTEGER,INTENT(OUT)                   :: NumberOfDataBatch,NumberOfDataRows,NumberOfDataColumns
    INTEGER,INTENT(IN)                    :: NColumnsOfData
    INTEGER,OPTIONAL,INTENT(IN)           :: NRowsOfData
    CHARACTER(LEN=*),OPTIONAL,ALLOCATABLE :: TimeArray(:)

    !Local variables
    CHARACTER(LEN=ModNAmeLen+15) :: ThisProcedure = ModName // 'SetTSDCacheSize'
    INTEGER                      :: nbatch,nrow,ncolumn,ErrorCode

    !Set nrow
    nrow=1  !Default
    IF (PRESENT(NRowsOfData)) nrow=NRowsOfData

    !Identify the size of storage array
    ncolumn=NColumnsOfData
    IF (nrow*ncolumn.GT.CacheLimit) THEN
      nbatch=1
    ELSE
      nbatch=CacheLimit/(nrow*ncolumn)
    END IF

    !Check if the storage arrays are already defined
    IF (ALLOCATED(ValuesForOutput)) THEN
      DEALLOCATE(ValuesForOutput)
      IF (PRESENT(TimeArray)) DEALLOCATE (TimeArray)
    END IF

    !Allocate memory for the data storage array
    ALLOCATE (ValuesForOutput(nrow,ncolumn,nbatch),STAT=ErrorCode)
    IF (ErrorCode.NE.0) CALL LogMessage('Error in allocating data storage array for file '//TRIM(FileName),f_iFatal,ThisProcedure)

    !Allocate memory for the time storage array
    IF (PRESENT(TimeArray)) THEN
      ALLOCATE (TimeArray(nbatch),STAT=ErrorCode)
      IF (ErrorCode.NE.0) CALL LogMessage('Error in allocating time storage array for file '//TRIM(FileName),f_iFatal,ThisProcedure)
    END IF

    !Set the data fields
    NumberOfDataBatch=nbatch
    NumberOfDataRows=nrow
    NumberOfDataColumns=ncolumn

  END SUBROUTINE SetTSDCacheSize


  ! -------------------------------------------------------------
  ! --- MODIFY THE TIME SERIES RATE TYPE INPUT DATA SO THAT ITS TIME UNIT IS CONSISTENT WITH SIMULATION TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE AdjustRateTypeData(r,RateTypeDataArray,ConversionFactor,DataInterval,LastDataDate)
    REAL(8),DIMENSION(:,:)                                :: r
    LOGICAL,DIMENSION(:),INTENT(IN)                       :: RateTypeDataArray
    REAL(8),OPTIONAL,INTENT(IN)                           :: ConversionFactor     !In this case the conversion factor is supplied directly
    INTEGER,OPTIONAL,INTENT(IN)                           :: DataInterval         !In this case the time step length in minutes and
    CHARACTER(LEN=f_iTimeStampLength),OPTIONAL,INTENT(IN) :: LastDataDate         ! the date of last data read are supplied as character

    !Local variables
    INTEGER                           :: nrow,ncol
    REAL(8)                           :: r1d(SIZE(r,DIM=1)*SIZE(r,DIM=2))
    REAL(8)                           :: Factor
    CHARACTER(LEN=f_iTimeStampLength) :: TimeStampBegin

    !Initialize
    nrow=SIZE(r,DIM=1)
    ncol=SIZE(r,DIM=2)

    !Calculate conversion factor if necessary
    IF (PRESENT(ConversionFactor)) THEN        !Conversion factor is supplied directly
      Factor=ConversionFactor
    ELSEIF (PRESENT(DataInterval) .AND. PRESENT(LastDataDate)) THEN !Time step length and the date of last data read are supplied
      IF (DataInterval.EQ.0) THEN
        Factor=1d0
      ELSE
        !Decrement the time stamp of the data last read by the interval minutes
        TimeStampBegin=IncrementTimeStamp(LastDataDate,-DataInterval)
        !Compute Factor
        Factor=REAL(NPeriods(SimulationTimeStep_InMinutes,TimeStampBegin,LastDataDate),8)
      END IF
    END IF

    !Convert
    SELECT CASE (SIZE(RateTypeDataArray))
      CASE (1)
        IF (RateTypeDataArray(1)) r(:,:)=r(:,:)/Factor

      CASE DEFAULT
        r1d=PACK(TRANSPOSE(r),MASK=.TRUE.)
        WHERE (RateTypeDataArray) r1d=r1d/Factor
        r=RESHAPE(r1d,(/nrow,ncol/),ORDER=(/2,1/))

    END SELECT

  END SUBROUTINE AdjustRateTypeData


!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!                  MISCELLENEOUS METHODS FOR TIME RELATED DATA
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////
!////////////////////////////////////////////////////////////////////////////////

  ! -------------------------------------------------------------
  ! --- FUNCTION TO TEST LESS THAN (.LE.) COMPARISON B/W TWO TIME UNITS
  ! -------------------------------------------------------------
  FUNCTION TimeUnit_CheckForLessThanOrEqualTo(TimeUnit1,TimeUnit2) RESULT(lResult)
    CHARACTER(LEN=*),INTENT(IN) :: TimeUnit1,TimeUnit2
    LOGICAL                     :: lResult

    !Local variables
    INTEGER :: TimeUnit1_InMinutes,TimeUnit2_InMinutes
    REAL(8) :: rDummy

    !Convert to minutes
    CALL CTimeStep_To_RTimeStep(TimeUnit1,rDummy,TimeUnit1_InMinutes)
    CALL CTimeStep_To_RTimeStep(TimeUnit2,rDummy,TimeUnit2_InMinutes)

    !Compare
    IF (TimeUnit1_InMinutes .LE. TimeUnit2_InMinutes) THEN
        lResult = .TRUE.
    ELSE
        lResult = .FALSE.
    END IF

  END FUNCTION TimeUnit_CheckForLessThanOrEqualTo


  ! -------------------------------------------------------------
  ! --- FUNCTION TO TEST LESS THAN (.LT.) COMPARISON B/W TWO TIME STAMPS
  ! -------------------------------------------------------------
  FUNCTION CheckForLessThan(TimeStamp1,TimeStamp2) RESULT(TestResult)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp1,TimeStamp2
    LOGICAL                                      :: TestResult

    !Local variables
    INTEGER::JulianDate1,MinutesAfterMidnight1,JulianDate2,MinutesAfterMidnight2

    !Initialize
    TestResult=.FALSE.

    !Convert time stamps to equivalent Julain dates and minutes
    CALL TimeStampToJulianDateAndMinutes(TimeStamp1,JulianDate1,MinutesAfterMidnight1)
    CALL TimeStampToJulianDateAndMinutes(TimeStamp2,JulianDate2,MinutesAfterMidnight2)

    !Compare
    IF (JulianDate1.LT.JulianDate2) THEN
      TestResult=.TRUE.
    ELSEIF (JulianDate1.EQ.JulianDate2) THEN
      IF (MinutesAfterMidnight1.LT.MinutesAfterMidnight2) TestResult=.TRUE.
    END IF

  END FUNCTION CheckForLessThan


  ! -------------------------------------------------------------
  ! --- FUNCTION TO TEST GREATER THAN (.GT.) COMPARISON B/W TWO TIME STAMPS
  ! -------------------------------------------------------------
  FUNCTION CheckForGreaterThan(TimeStamp1,TimeStamp2) RESULT(TestResult)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp1,TimeStamp2
    LOGICAL                                      :: TestResult

    !Local variables
    INTEGER::JulianDate1,MinutesAfterMidnight1,JulianDate2,MinutesAfterMidnight2

    !Initialize
    TestResult=.FALSE.

    !Convert time stamps to equivalent Julain dates and minutes
    CALL TimeStampToJulianDateAndMinutes(TimeStamp1,JulianDate1,MinutesAfterMidnight1)
    CALL TimeStampToJulianDateAndMinutes(TimeStamp2,JulianDate2,MinutesAfterMidnight2)

    !Compare
    IF (JulianDate1.GT.JulianDate2) THEN
      TestResult=.TRUE.
    ELSEIF (JulianDate1.EQ.JulianDate2) THEN
      IF (MinutesAfterMidnight1.GT.MinutesAfterMidnight2) TestResult=.TRUE.
    END IF

  END FUNCTION CheckForGreaterThan


  ! -------------------------------------------------------------
  ! --- FUNCTION TO TEST GREATER THAN OR EQUAL TO (.GE.) COMPARISON B/W TWO TIME STAMPS
  ! -------------------------------------------------------------
  FUNCTION CheckForGreaterThanOrEqualTo(TimeStamp1,TimeStamp2) RESULT(TestResult)
    CHARACTER(LEN=f_iTimeStampLength),INTENT(IN) :: TimeStamp1,TimeStamp2
    LOGICAL                                      :: TestResult

    !Local variables
    INTEGER :: JulianDate1,MinutesAfterMidnight1,JulianDate2,MinutesAfterMidnight2

    !Initialize
    TestResult = .FALSE.

    !Convert time stamps to equivalent Julain dates and minutes
    CALL TimeStampToJulianDateAndMinutes(TimeStamp1,JulianDate1,MinutesAfterMidnight1)
    CALL TimeStampToJulianDateAndMinutes(TimeStamp2,JulianDate2,MinutesAfterMidnight2)

    !Compare
    IF (JulianDate1 .GE. JulianDate2) THEN
        TestResult = .TRUE.
    END IF

  END FUNCTION CheckForGreaterThanOrEqualTo


  ! -------------------------------------------------------------
  ! --- FUNCTION TO COMPUTE THE NUMBER OF PERIODS BETWEEN TWO TIME STAMPS
  ! --- Note: Adopted from HEC-DSS library (Can Dogrul - 4/7/2011)
  ! -------------------------------------------------------------
  FUNCTION NPeriodsBetweenTimeStamps(DELTAT_InMinutes,BeginTimeStamp,EndTimeStamp) RESULT(NPeriod)
    INTEGER,INTENT(IN)          :: DELTAT_InMinutes
    CHARACTER(LEN=*),INTENT(IN) :: BeginTimeStamp,EndTimeStamp
    INTEGER                     :: NPeriod

    !Local variables
    INTEGER                           :: BeginJulianDate,EndJulianDate,BeginMinutesAfterMidnight,EndMinutesAfterMidnight, &
                                         iBeginYear,iBeginMonth,iBeginDay,iEndYear,iEndMonth,iEndDay
    CHARACTER(LEN=f_iTimeStampLength) :: TempTimeStamp

    !Find corresponding Julian dates and minutes after midnight
    CALL TimeStampToJulianDateAndMinutes(BeginTimeStamp,BeginJulianDate,BeginMinutesAfterMidnight)
    CALL TimeStampToJulianDateAndMinutes(EndTimeStamp,EndJulianDate,EndMinutesAfterMidnight)

    !If interval is greater than or equal to a month, find corresponding day, month and year
    IF (DELTAT_InMinutes .GE. 40320) THEN
      !Find corresponding day, month, year
      CALL JulianDateToDayMonthYear(BeginJulianDate,iBeginDay,iBeginMonth,iBeginYear)
      CALL JulianDateToDayMonthYear(EndJulianDate,iEndDay,iEndMonth,iEndYear)
    END IF

    !Compute number of periods between two time stamps
    IF (DELTAT_InMinutes.GE.40320  .AND.  DELTAT_InMinutes.LE.44640) THEN
      !Monthly interval
      NPeriod       = ((iEndYear-iBeginYear) * 12) + (iEndMonth-iBeginMonth) + ((iEndDay-iBeginDay) / 27)
      TempTimeStamp = IncrementTimeStamp(BeginTimeStamp,DELTAT_InMinutes,NPeriod)
      IF (TempTimeStamp .TSGT. EndTimeStamp) NPeriod = NPeriod - 1
    ELSE IF (DELTAT_InMinutes .GE. 525600) THEN
      !Yearly interval
      NPeriod       = (iEndYear-iBeginYear) + (((iEndMonth-iBeginMonth) + (iEndDay-iBeginDay) / 28) / 12)
      TempTimeStamp = IncrementTimeStamp(BeginTimeStamp,DELTAT_InMinutes,NPeriod)
      IF (TempTimeStamp .TSGT. EndTimeStamp) NPeriod = NPeriod - 1
    ELSE
      !Otherwise
      NPeriod = (((EndJulianDate-BeginJulianDate)*1440) + (EndMinutesAfterMidnight-BeginMinutesAfterMidnight)) / DELTAT_InMinutes
    END IF

  END FUNCTION NPeriodsBetweenTimeStamps


  ! -------------------------------------------------------------
  ! --- FUNCTION TO COMPUTE THE NUMBER OF PERIODS BETWEEN TWO TIMES
  ! -------------------------------------------------------------
  FUNCTION NPeriodsBetweenTimes(DELTAT,BeginTime,EndTime) RESULT(NPeriod)
    REAL(8),INTENT(IN)::DELTAT,BeginTime,EndTime
    INTEGER::NPeriod

    !Compute number of periods between two times
    NPeriod=INT((EndTime-BeginTime)/DELTAT)

  END FUNCTION NPeriodsBetweenTimes


!------------------------------------------------------------------------------------

! *************************************************************
! *************************************************************
! **** TIME STEP CONVERSION METHODS
! *************************************************************
! *************************************************************

  ! -------------------------------------------------------------
  ! --- CONVERT CHARACTER TIME STEP TO MINUTES AND REAL NUMBER TIME STEP
  ! -------------------------------------------------------------
  SUBROUTINE CTimeStep_To_RTimeStep(UNITT,DELTAT,DELTAT_InMinutes,iStat)
    CHARACTER(LEN=*),INTENT(IN)  :: UNITT
    REAL(8),INTENT(OUT)          :: DELTAT
    INTEGER,INTENT(OUT)          :: DELTAT_InMinutes
    INTEGER,OPTIONAL,INTENT(OUT) :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: Thisprocedure = ModName // 'CTimeStep_To_RTimeStep'
    INTEGER                      :: TimeStepIndex

    !Initialize
    IF (PRESENT(iStat)) iStat = 0
    DELTAT_InMinutes = 0
    DeltaT           = 1.0 !DELTAT is always 1.0 since the unit includes the time step length, e.g. DELTAT = 1 12HOUR
    
    !Make sure that UNITT entry is a recognized time step
    TimeStepIndex = IsTimeIntervalValid(UNITT)
    IF (TimeStepIndex .EQ. 0) THEN
      IF (PRESENT(iStat)) THEN
          CALL SetLastMessage(TRIM(UNITT)// ' is not a recognized time step',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      ELSE
          CALL LogMessage(TRIM(UNITT)// ' is not a recognized time step',f_iFatal,ThisProcedure)
      END IF
    END IF

    !Convert time step to Julian date increment "in terms of minutes"
    DELTAT_InMinutes = f_iRecognizedIntervals_InMinutes(TimeStepIndex)

  END SUBROUTINE CTimeStep_To_RTimeStep


  ! -------------------------------------------------------------
  ! --- COMPUTE CONVERSION FACTOR BETWEEN TWO TIME INTERVALS
  ! -------------------------------------------------------------
  FUNCTION TimeIntervalConversion(ToInterval,FromInterval) RESULT(ConversionFactor)
    CHARACTER(LEN=*),INTENT(IN) :: ToInterval,FromInterval
    REAL(8)                     :: ConversionFactor

    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'TimeIntervalConversion'
    REAL(8)                      :: DummyReal
    INTEGER                      :: FromInterval_InMinutes,ToInterval_InMinutes,ErrorCode

    !Convert FromInterval to minutes
    CALL CTimeStep_To_RTimeStep(UpperCase(FromInterval),DummyReal,FromInterval_InMinutes,ErrorCode)
    IF (ErrorCode.NE.0) CALL LogMessage(TRIM(UpperCase(FromInterval))//' is not a valid time interval!',f_iFatal,ThisProcedure)

    !Convert ToInterval to minutes
    CALL CTimeStep_To_RTimeStep(UpperCase(ToInterval),DummyReal,ToInterval_InMinutes,ErrorCode)
    IF (ErrorCode.NE.0) CALL LogMessage(TRIM(UpperCase(ToInterval))//' is not a valid time interval!',f_iFatal,ThisProcedure)

    ConversionFactor = REAL(ToInterval_InMinutes,8)/REAL(FromInterval_InMinutes,8)

 END FUNCTION TimeIntervalConversion


END MODULE
