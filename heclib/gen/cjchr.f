      SUBROUTINE CJCHR (CLINE)
C
C     Center Justifies CLINE
C
      CHARACTER CLINE*(*), CTEMP*132
C
C
      ILEN = LEN(CLINE)
      IF (ILEN.EQ.0) GO TO 800
      CALL CHRFLB (CLINE, IBEG, IEND)
      IF (IEND.EQ.0) GO TO 800
C
      NLINE = IEND - IBEG + 1
      IF (NLINE.GT.132) NLINE = 132
C
      CTEMP(1:NLINE) = CLINE(IBEG:IEND)
      CLINE(IBEG:IEND) = ' '
C
      I = ((ILEN - NLINE) / 2) + 1
      CLINE(I:I+NLINE-1) = CTEMP(1:NLINE)
C
C
 800  CONTINUE
      RETURN
      END
