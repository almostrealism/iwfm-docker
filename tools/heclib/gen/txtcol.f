      SUBROUTINE TXTCOL(CFGIN,CBGIN,CATTIN)
C
C     This routine sets the default foreground, background
C     and attributes for text display using ANSI.SYS
C
      CHARACTER*20 COLORS(8),CMSG,CATTR(5)
      CHARACTER*80 CTMP
      CHARACTER    CFGIN*(*), CBGIN*(*), CATTIN*(*)
      CHARACTER*20 CFG,CBG,CATT
      INTEGER IATTR(5)
C
C
      DATA COLORS /'BLACK','RED','GREEN','YELLOW','BLUE','MAGENTA',
     +   'CYAN','WHITE'/
      DATA CATTR /'BOLD','BLINK','REVERSE','NORMAL','CANCELED'/
      DATA IATTR /1,5,7,0,8/
C
      IFG = LEN(CFGIN)
      IBG = LEN(CBGIN)
      IATT = LEN(CATTIN)
      CFG = CFGIN
      CBG = CBGIN
      CATT = CATTIN
      CALL UPCASE(CFG)
      CALL UPCASE(CBG)
      CALL UPCASE(CATT)
C
      CMSG = CHAR(27)//'['
      IMSG = 2
      CALL CHRWT1 (CMSG(1:2)//'0m',4)
C
      CTMP = CFG(1:IFG)
      CALL UPCASE(CTMP)
      DO 5 I=1,8
         IF (CTMP(1:IFG).EQ.COLORS(I)(1:IFG)) GOTO 10
 5    CONTINUE
      GOTO 14
 10   CONTINUE
      CMSG = CMSG(1:IMSG)//'3'//CHAR(47+I)
      IMSG = IMSG+2
C
 14   CONTINUE
      CTMP = CBG(1:IBG)
      CALL UPCASE(CTMP)
      DO 15 I=1,8
         IF (CTMP(1:IBG).EQ.COLORS(I)(1:IBG)) GOTO 20
 15   CONTINUE
      GOTO 30
 20   CONTINUE
      IF (IMSG.EQ.2) THEN
         CMSG = CMSG(1:IMSG)//'4'//CHAR(47+I)
         IMSG = IMSG+2
      ELSE
         CMSG = CMSG(1:IMSG)//';4'//CHAR(47+I)
         IMSG = IMSG+3
      ENDIF
C
 30   CONTINUE
      CTMP = CATT(1:IATT)
      CALL UPCASE(CTMP)
      DO 35 I=1,5
         ILEN = INDEX(CATTR(I),' ')-1
         IF (INDEX(CTMP(1:IATT),CATTR(I)(1:ILEN)).GT.0) THEN
            IF (IMSG.EQ.2) THEN
               CMSG = CMSG(1:IMSG)//CHAR(48+IATTR(I))
               IMSG = IMSG+1
            ELSE
               CMSG = CMSG(1:IMSG)//';'//CHAR(48+IATTR(I))
               IMSG = IMSG+2
            ENDIF
         ENDIF
 35   CONTINUE
C
      CMSG(IMSG+1:IMSG+1) = 'm'
      CALL CHRWT1 (CMSG(1:IMSG),IMSG+1)
C     CALL CHRWT1 (CMSG(1:IMSG)//'m',IMSG+1)
      RETURN
      END
