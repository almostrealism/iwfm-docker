      INTEGER FUNCTION INDXI (IBUFF, ISTART, IEND, ICH)
 
      INTEGER IBUFF(*)
 
C       IF ISTART < IEND  SCAN LEFT TO RIGHT
C       IF ISTART > IEND  SCAN RIGHT TO LEFT
 
      IF (IEND.GE.ISTART) THEN
      INC = 1
      ELSE
      INC = -1
      ENDIF
 
      DO 20 I=ISTART, IEND, INC
        CALL GETHOL (IBUFF, I, JCH)
        IF (ICH.EQ.JCH) THEN
          INDXI = I
          RETURN
        ENDIF
 20   CONTINUE
 
      INDXI = 0
      RETURN
      END
