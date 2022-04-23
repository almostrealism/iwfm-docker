      SUBROUTINE NAMFIL(IFN,CNAMES,INAMES,MAX,ISTAT)
C
C     SUBROUTINE TO READ A FILE (W/ IFN) AND PUT TRUE
C     AND PSUEDO CNAMES INTO ARRAY CNAMES
C     MAKE FILE LOOK LIKE:
C         CPSUDO,CTRU
C         CPSUDO,CTRU
C         ETC....
C     LEFT JUSTIFIED, ENDED WITH EITHER AN EOF OR BLANK
C     UPDATED SEPT, 82 BY BILL CHARLEY, HEC
C
      CHARACTER CNAMES*(*), COMMA, CLINE*200, CTRU*(80),CPSUDO*(80)
      DIMENSION CNAMES(*)
      DIMENSION INAMES(-4:*)
C
      DATA COMMA /','/
C
C
 10   CONTINUE
      READ(IFN,20,END=100)CLINE
 20   FORMAT(A)
      I = INDEX(CLINE,COMMA)
      IF (I.EQ.0) GO TO 200
      IF (I.EQ.1) GO TO 100
      K = I - 1
      CALL CHRBLK(CPSUDO)
      CPSUDO(1:K) = CLINE(1:K)
      K = I + 1
      CTRU = CLINE(K:)
      CALL SETNAM(CPSUDO,CTRU,MAX,CNAMES,INAMES,ILISTL,JSTAT)
      IF (JSTAT.NE.0) GO TO 220
      GO TO 10
C
 100  CONTINUE
      ISTAT = 0
      RETURN
C
 200  CONTINUE
      WRITE(6,210)CLINE
 210  FORMAT(' ***** NAME LIST --- ILLEGAL LINE, LINE = ',A)
      ISTAT = -1
      RETURN
C
 220  CONTINUE
      IF (JSTAT.EQ.4) GO TO 240
      WRITE(6,230)JSTAT
 230  FORMAT(' ***** NAME LIST --- ERROR IN CNAMES, STATUS = ',I5)
      ISTAT = -2
      CALL NAMLST(CNAMES,INAMES)
      RETURN
C
 240  CONTINUE
      WRITE(6,250)MAX
 250  FORMAT(' ***** NAME LIST --- MAX NUMBER OF CNAMES ENTERED ',I5)
      ISTAT = -3
      RETURN
C
      END
