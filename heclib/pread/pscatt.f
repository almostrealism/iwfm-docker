      SUBROUTINE PSCATT ( CATT, CSCN, ISATT, ILINE,NLINE,IOFSET,IPSTAT)
C------
C------ Get the meaning of an attribute character and send it
C------ to the terminal.  Note most attributes are cumulative
C------ in affect so set to 00 if necessary!! Ignore undefines
C------
      include 'pchar.h'                                                 u
      CHARACTER CATT*1, CSCN(*)*1, CTMP*2
C------
C     WRITE (*,*)'PREAD Screens object not loaded!'                     M
C     RETURN                                                            M
C------
      IF ( IOFSET .LT.  0 ) THEN                                        u
C ------ RETURN THE NULL DEFINED ATTRIBUTE IF ONE EXISTS
C ------ (I.E.   X=
      K = IABS(IOFSET) - 1                                              u
      IS = ISATT - ILINE                                                u
      DO 15 I = 1, NLINE                                                u
      IS = IS + ILINE                                                   u
      IF ( CSCN(IS+K+1).EQ.' '.AND.CSCN(IS+K+2).EQ.' ') THEN            u
      IPSTAT = 4                                                        u
      CATT = CSCN(IS)                                                   u
      RETURN                                                            u
      ENDIF                                                             u
   15 CONTINUE                                                          u
      IPSTAT = 0                                                        u
      RETURN                                                            u
      ENDIF                                                             u
C ------
      IS = ISATT - ILINE                                                u
      DO 20 I = 1, NLINE                                                u
      IS = IS + ILINE                                                   u
      IF ( CSCN(IS) .EQ. CATT ) THEN                                    u
      K = IOFSET - 1                                                    u
      IF ( CSCN(IS+K) .NE. '=' ) GO TO 150                              u
C ------ Check if null defined attribute character encountoured
      IF(CSCN(IS+K+1).EQ.' '.AND.CSCN(IS+K+2).EQ.' ') GO TO 200         u
    2 CTMP(1:1) = CSCN(IS+K+1)                                          u
      CTMP(2:2) = CSCN(IS+K+2)                                          u
      if(ctmp(1:1).eq.cspl(4:4)) then                                   u
        call pfnkey(cscn(is+k+2),ctmp,nn)                               u
      endif                                                             u
      READ(CTMP,50,ERR=100) IATT                                        u
   50 FORMAT(I2)                                                        u
      CALL VATT ( IATT )                                                u
      K = K + 3                                                         u
      IF ( CSCN(IS+K) .NE. '+' ) GO TO 100                              u
      GO TO 2                                                           u
      ENDIF                                                             u
   20 CONTINUE                                                          u
C ------ UNDEFINED CHAR
      IPSTAT = 3                                                        u
      GO TO 900                                                         u
  100 IPSTAT = 0                                                        u
C     CALL WAITS(1.0)
      GO TO 900                                                         u
C ------ ERROR ILLEGAL FORMAT
  150 IPSTAT = 1                                                        u
      GO TO 900                                                         u
C ------ HERE FOR NULL DEFINED CHAR
  200 IPSTAT = 2                                                        u
  900 RETURN                                                            u
      END
