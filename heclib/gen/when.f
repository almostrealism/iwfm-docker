      SUBROUTINE WHEN (CDAT, CTIM)
C
C       This subroutine, when called, returns the current date and
C       time.  The format is shown in the following example:
C            Date: '05MAR85 '
C            Time: '07:35:10'
C
C       Output:  CDAT  (--- Date)
C                CTIM  (--- Time)
C
C
      CHARACTER CDAT*(*), CTIM*(*)
      CHARACTER CTEMP*10
C
      CDAT = ' '
      CTIM = ' '
C     CALL CTIME (CTIM)                                                 M
      CALL CTIME_ (CTIM)                                                u
      CALL CDATE (CTEMP)
      CALL REMBLK (CTEMP(1:10), CDAT, ITMP)
C
      RETURN
      END


