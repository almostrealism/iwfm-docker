      SUBROUTINE LFLNB ( CLINE, NBEG, NLEN, IBEGNB, ILENNB)
C
C  PURPOSE:  TO LOCATE THE FIRST AND LAST NON-BLANK CHARACTER POSITION
C            WITHIN A CHARACTER STING.
C
C  INPUTS:  CLINE - CHARACTER STRING TO BE SEARCHED
C           NBEG - INITIAL POSTION IN LINE OF STRING
C           NLEN - NUMBER OF CHARACTERS IN STRING
C
C  OUTPUTS:  IBEGNB - POSITION OF FIRST NON-BLANK CHARACTER IN LINE
C            ILENNB - NUMBER OF CHARACTERS IN LINE FROM IBEGNB TO THE
C                 LAST NON-BLANK CHARACTER
C
      CHARACTER CLINE*(*)
C
C     CHECK FOR A NULL LOCATED
      IF (NLEN.EQ.0) THEN
      IBEGNB = 0
      ILENNB = 0
      RETURN
      ENDIF
C
      NEND = NBEG + NLEN - 1
      IBEGNB = NINDX ( CLINE(NBEG:NEND), ' ')
      IF (IBEGNB.GT.0) THEN
      J = NINDXR ( CLINE(NBEG:NEND), ' ')
      ILENNB = J - IBEGNB + 1
      IBEGNB = IBEGNB + NBEG - 1
      ELSE
      ILENNB = 0
      ENDIF
C
      RETURN
      END
