      SUBROUTINE CLINES (NLINES)
C
        CHARACTER CSTR*80
C
C       returns the number of lines on the terminal screen
C         (1) calls SCNLINES
C         (2) checks the LNS register
        CALL SCNLINES(NLINES)
        IF (NLINES .LE. 0) THEN
          CALL GSTRRG ('LNS', CSTR, NSTR, ISTAT)
          IF ((ISTAT .NE. 0) .OR. (NSTR .LE. 0)) THEN
            NLINES = 24                                                 u
C           NLINES = 25                                                 ML
          ELSE
            NLINES = INTGR (CSTR, 1, NSTR, ISTAT)
            IF((NLINES.LT.1).OR.(ISTAT.NE.0)) THEN
              NLINES = 24                                               u
C             NLINES = 25                                               ML
            ENDIF
          ENDIF
        ENDIF
      RETURN
      END
