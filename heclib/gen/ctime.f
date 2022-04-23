C     SUBROUTINE CTIME (CARRAY)                                         M
      SUBROUTINE CTIME_ (CARRAY)                                        u
C
      CHARACTER CARRAY*(*)
C
C
      I = LEN(CARRAY)
      IF (I.LT.8) THEN
      CALL CHRFIL (CARRAY,'*')
      RETURN
      ENDIF
C
      CALL SYSTIM (JUL, ISECS)
      IHR = ISECS / 3600
      IMIN = (ISECS - (IHR * 3600)) / 60
      ISEC = ISECS - (IHR * 3600) - (IMIN * 60)
C
      WRITE (CARRAY,20) IHR,IMIN,ISEC
 20   FORMAT (I2.2,':',I2.2,':',I2.2)
C
      RETURN
      END
