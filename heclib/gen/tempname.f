      SUBROUTINE TEMPNAME (CNAME, IUNIT)
C
C     Generates the name of a temporary file in directory /tmp
C     based on the process id and unit number.
C
      CHARACTER CNAME*(*)
      CHARACTER CTMP*256
      REAL RANDVAL
      LOGICAL LEXISTS
C
C     CALL GTENV('TEMP', CTMP, NTMP, ISTAT)                             M
C     IF (ISTAT .NE. 0) CTMP = '.'                                      M
      CTMP = '/tmp'                                                     u
      NTMP = LEN_TRIM(CTMP)
      CALL GETPROCID(IPID)
      CALL RANDOM_SEED
 20   FORMAT ('/tmp.', I3.3, '.', I8.8, '.', F7.6)
      DO I=1,10000
        CALL RANDOM_NUMBER(RANDVAL)
        WRITE (CTMP(NTMP+1:), 20) IUNIT, IPID, RANDVAL
        INQUIRE(FILE=CTMP, EXIST=LEXISTS)
        IF (.NOT.LEXISTS) EXIT
      END DO
      CNAME = CTMP

      RETURN
      END
