      SUBROUTINE PLDSCN (CSCRN)
C
C     CHECK SCREEN BUFFER FOR DESIRED SCREEN
C     IF THERE RETURN, ELSE LOAD DESIRED SCREEN
C     IF NO ROOM IN BUFFER KICK OUT LOWEST PRIORITY SCREEN
C
      CHARACTER*8 CSCRN, CLINE*132, clinu*132
C
C     WRITE (*,*)'PREAD Screens object not loaded!'                     M
C     RETURN                                                            M
C
      INCLUDE 'pint.h'                                                  u
      INCLUDE 'pfiles.h'                                                u
      INCLUDE 'pnums.h'                                                 u
      INCLUDE 'plflag.h'                                                u
      INCLUDE 'pscnch.h'                                                u
C
C
C       USE OF ARRAY  IB(I,K) WHERE I=ITEM NUMBER BELOW
C     1       BEGIN BYTE POSITION OF CHAR AREA FOR ENTIRE INFO
C     2       END
C     3       BEGIN RELATIVE BYTE POS OF NAME
C     4       END
C     5       BEGIN RELATIVE BYTE POS OF SCREEN IMAGE
C     6       END
C     7       BEGIN RELATIVE BYTE POS OF TRANSLATE TABLE
C     8       END
C     9       BEGIN RELATIVE BYTE POS OF APPEND TABLE
C     10      END
C     11      # COLS IN SCREEN IMAGE
C     12      # ROWS IN SCREEN IMAGE
C     13      # COLS IN TRANSLATE/APPEND LINES ( ZERO FOR NO REPLY)
C     14      # TRANSLATE LINES
C     15      # APPEND LINES
C     16      TOTAL # BYTES REQ FOR ALL CHAR STORAGE
C     17      PROMPT CURSOR ROW
C     18      PROMPT CURSOR COL
C     19      SCREEN RETENTION PRIORITY
C     20      FULL SCREEN ERASE FLAG
C     21      BASE ROW
C     22      BASE COL
C     23      MESSAGE CURSOR ROW
C     24      MESSAGE CURSOR COL
C     25      ?
C     26      NUMBER OF ALLOWABLE CONSEQUECTIVE ERRORS
C     27      INACTIVITY TIMOUT IN SECONDS FOR THIS SCREEN
C     28      SCREEN DISPLAY LEVEL 0-9
C     29      # LINES IN ATTMAP AREA, EACH SAME WIDTH AS SCREEN
C     30      OFFSET POSITION IN ATTMAP LINE FOR ATTRIBUTES
C
C        USE OF ARRAY  IC(I)  WHERE  I=ITEM NUMBER BELOW
C
C     1       MAX # OF SCREENS SAVED (ONE LESS THAN DIMENSION  KIC)
C     2       NEXT AVAILABLE SLOT
C     3       MAX # CHAR IN CHAR AREA CSCN
C     4       NEXT AVAILABLE CHAR IN CHAR AREA
C
C
      CHARACTER CTMP*80, cscrn0*8                                       u
      LOGICAL LFIRST, LFLUSH                                            u
      DATA LFIRST/.TRUE./                                               u
C
C
C
      IF (ISCN.EQ.-1) RETURN                                            u
      IF(LFIRST) THEN                                                   u
      OPEN (UNIT=ISCN,FILE=CSCNFL,STATUS='OLD',ERR=900,IOSTAT=ISTAT)    u
      IC(1)= KSLOTS - 1                                                 u
      IC(2)=1                                                           u
      IC(3)= KSCN                                                       u
      IC(4)=1                                                           u
      LFIRST=.FALSE.                                                    u
      ENDIF                                                             u
C
      cscrn0 = cscrn                                                    u
      CALL UPCASE ( CSCRN )                                             u
C     CHECK IF DESIRED SCREEN IS IN BUFFER
      DO 1 I=1,IC(2)-1                                                  u
      IS=IB(3,I) + IB(1,I) - 1                                          u
      IE=IS + 7                                                         u
      IT=0                                                              u
      DO 2 II=IS,IE                                                     u
      IT=IT+1                                                           u
      IF(CSCN(II)(1:1) .NE. CSCRN(IT:IT)) GO TO 1                       u
    2 CONTINUE                                                          u
      ISCRN=I                                                           u
      LSCN=.TRUE.                                                       u
      RETURN                                                            u
    1 CONTINUE                                                          u
      REWIND ISCN                                                       u
   10 READ(ISCN,20,END=700) CLINE                                       u
   20 FORMAT(A)                                                         u
      clinu = cline                                                     u
      call upcase ( clinu(1:16) )                                       u
      IF(clinu(1:8) .NE. '#SCREEN ') GO TO 10                           u
      IF(clinu(9:16) .NE. CSCRN) GO TO 10                               u
C     FOUND DESIRED SCREEN
C     MAKE A SLOT FOR IT    - DOES NOT MEAN ROOM AVAILABLE YET !!
      IF(IC(2) .GT. IC(1)) THEN                                         u
      IC(2)=IC(2) - 1                                                   u
      IC(4)=IC(4) - IB(16,IC(2))                                        u
      ENDIF                                                             u
      J=IC(1) + 1                                                       u
      CALL PGFSCN ( CLINE(18:19), '(I2)', IB(11,J) )                    u
      IF ( IB(11,J) .LT. 0 ) GO TO 1010                                 u
      CALL PGFSCN ( CLINE(21:22), '(I2)', IB(12,J) )                    u
      IF ( IB(12,J) .LT. 0 ) GO TO 1010                                 u
      CALL PGFSCN ( CLINE(24:25), '(I2)', IB(21,J) )                    u
      CALL PGFSCN ( CLINE(27:28), '(I2)', IB(22,J) )                    u
      CALL PGFSCN ( CLINE(30:30), '(I1)', IB(19,J) )                    u
      CALL PGFSCN ( CLINE(32:32), '(Z1)', IB(20,J) )                    u
      CALL PGFSCN ( CLINE(33:35), '(I3)', NL       )                    u
      IF ( NL       .LT. 0 ) GO TO 1010                                 u
      CALL PGFSCN ( CLINE(37:38), '(I2)', IB(13,J) )                    u
      IF ( IB(13,J) .LT. 0 ) GO TO 1010                                 u
      CALL PGFSCN ( CLINE(40:40), '(I1)', IB(26,J) )                    u
      CALL PGFSCN ( CLINE(42:44), '(I3)', IB(27,J) )                    u
      CALL PGFSCN ( CLINE(46:46), '(I1)', IB(28,J) )                    u
      CALL PGFSCN ( CLINE(48:49), '(I2)', IB(29,J) )                    u
C ------ Check if screen has ATTRIBUTES
      IF ( IB(29,J) .GT. 0 ) THEN                                       u
      IA = 2                                                            u
      ELSE                                                              u
      IA = 1                                                            u
      ENDIF                                                             u
      CALL PGFSCN ( CLINE(51:52), '(I2)', IB(30,J) )                    u
C
C ------
      NREQ= IB(11,J)*IB(12,J)*IA + NL*IB(13,J) + 12 + IB(29,J)*IB(11,J) u
      IB(16,J) = NREQ                                                   u
C     CHECK IF THERE IS ROOM IN BUFFER FOR THIS SCREEN
   30 IF(IC(3)-IC(4) .LT. NREQ) THEN                                    u
C     NEED TO KICK OUT LOWEST PRIORITY SCREEN
      ILOW=IC(2)-1                                                      u
      IF(ILOW .LT. 1) GO TO 1020                                        u
      IC(2)=IC(2)-1                                                     u
      IC(4)=IC(4) - IB(16,ILOW)                                         u
      GO TO 30                                                          u
      ENDIF                                                             u
C     LOAD IN NEW SCREEN, WE KNOW WE HAVE A SLOT AND ENOUGH SPACE
C     FIND WHICH SLOT AND MOVE THINGS IF NECESSARY
      DO 40 I=1,IC(2)-1                                                 u
      IF(IB(19,I) .GT. IB(19,J)) GO TO 50                               u
      IF(IB(19,I) .EQ. IB(19,J).AND. IB(16,I).GT.IB(16,J)) GO TO  50    u
   40 CONTINUE                                                          u
      I=IC(2)                                                           u
   50 K=I                                                               u
      N=K+1                                                             u
      IF(K.LT.IC(2)) THEN                                               u
C     MUST MOVE STUFF - BACKWARDS
      DO 75 I=IC(2),N,-1                                                u
      DO 60 II=1,KIB                                                    u
   60 IB(II,I) = IB(II,I-1)                                             u
C     NOW MOVE CHAR STUFF
      DO 70 II=IB(2,I),IB(1,I),-1                                       u
   70 CSCN(II+NREQ) = CSCN(II)                                          u
      IB(1,I)=IB(1,I) + NREQ                                            u
      IB(2,I)=IB(2,I) + NREQ                                            u
   75 CONTINUE                                                          u
      ENDIF                                                             u
C
      DO 80 I=1,KIB                                                     u
   80 IB(I,K) = IB(I,J)                                                 u
      IF(K.EQ.IC(2)) THEN                                               u
      IB(1,K) = IC(4)                                                   u
      ELSE                                                              u
      IB(1,K) = IB(1,N) - NREQ                                          u
      ENDIF                                                             u
      IC(4)= IC(4) +NREQ                                                u
      IC(2) = IC(2) + 1                                                 u
      IB(2,K) = IB(1,K) + NREQ - 1                                      u
      IB(14,K)=0                                                        u
      IB(15,K)=0                                                        u
C     READ CHAR IN SCREEN IMAGE
      IB(3,K)= 1                                                        u
      IB(4,K) = 8                                                       u
C------ Keep char position 9,10,11,12 for special ATTRIBUTES
      IB( 5,K) = 13                                                     u
C     STORE NAME OF SCREEN
      IS= IB(1,K) + IB(3,K) - 2                                         u
      DO 85 I=1,8                                                       u
   85 CSCN(IS+I) = CSCRN(I:I)                                           u
      IE= IB(1,K) + IB( 5,K) - 2                                        u
      IBA = IB(12,K) * IA                                               u
      DO 100 II=1,IBA + IB(29,K)                                        u
      IS=IE+1                                                           u
      IE=IS+IB(11,K) - 1                                                u
      READ(ISCN,90,END=800) (CSCN(I), I=IS,IE)                          u
   90 FORMAT(80A1)                                                      u
C------
      IF(IB(29,K).GT.0.AND.MOD(II,IB(12,K)).EQ.0.AND.II.LE.IBA) THEN    u
C------ Read one line out of the way ( ie, #ATTRIBUTES or #ATTMAP )
      READ(ISCN,20,END=800) CLINE                                       u
      clinu = cline                                                     u
      call upcase ( clinu )                                             u
      IF ( clinu(1:4) .NE. '#ATT' ) GO TO 1050                          u
      ENDIF                                                             u
C------
  100 CONTINUE                                                          u
      IB(6,K)=IB(5,K) + IB(11,K)*IB(12,K)*IA +IB(29,K)*IB(11,K) -1      u
C     READ PROMPT LOCATION
      READ(ISCN,20) CLINE                                               u
      clinu = cline                                                     u
      call upcase ( clinu )                                             u
      IF(clinu(1:8) .NE. '#PROMPT ') GO TO 1060                         u
      READ(CLINE,110) IB(17,K), IB(18,K)                                u
  110 FORMAT(8X,I2,1X,I2)                                               u
      KATT = IB(1,K) + IB(4,K)                                          u
      CSCN(KATT)   = CLINE(15:15)                                       u
      CSCN(KATT+1) = CLINE(17:17)                                       u
C     READ MESSAGE LOCATION
      READ(ISCN,20) CLINE                                               u
      clinu = cline                                                     u
      call upcase ( clinu )                                             u
      IF(clinu(1:9) .NE. '#MESSAGE ') GO TO 1060                        u
      READ(CLINE,111) IB(23,K), IB(24,K)                                u
  111 FORMAT(9X,I2,1X,I2)                                               u
      CSCN(KATT+2) = CLINE(16:16)                                       u
C     READ TRANSLATE TABLE
      READ(ISCN,20) CLINE                                               u
      clinu = cline                                                     u
      call upcase ( clinu )                                             u
      IF(clinu(1:10) .NE. '#TRANSLATE') GO TO 1070                      u
      IB(7,K)=IB(6,K) + 1                                               u
C Check if cline includes least 14 characters
      call chrlnb ( cline, nn )                                         u
      if ( nn .le. 13 ) call chrblk ( cline(14:80) )                    u
      READ(CLINE,120) IB(14,K)                                          u
  120 FORMAT(BN,10X,I4)                                                 u
C ------ Check for flush type-ahead flag
      LFLUSH = .FALSE.                                                  u
      call upcase ( CLINE(15:19) )                                      u
      IF ( CLINE(15:19) .EQ. 'FLUSH' ) LFLUSH = .TRUE.                  u
C ------
      IE= IB(1,K) + IB(7,K) - 2                                         u
      DO 130 II=1,IB(14,K)                                              u
      READ ( ISCN, 20, END=800 ) CTMP                                   u
C------ Force upper case if case insensitive --- , or ; ----
      IF ( CTMP(2:2).EQ.',' .OR. CTMP(2:2).EQ.';' ) THEN                u
      CALL UPCASE ( CTMP(3:10) )                                        u
      ENDIF                                                             u
      IS=IE+1                                                           u
      IE=IS+IB(13,K)-1                                                  u
      ITMP = 0                                                          u
      DO 125 I = IS, IE                                                 u
      ITMP = ITMP + 1                                                   u
  125 CSCN(I) = CTMP (ITMP:ITMP)                                        u
  130 CONTINUE                                                          u
      IB(8,K)=IB(7,K) + IB(14,K)*IB(13,K) -1                            u
C     READ APPEND IF PRESENT
      IB(15,K) = 0                                                      u
  135 READ(ISCN,20) CLINE                                               u
      clinu = cline                                                     u
      call upcase ( clinu )                                             u
      IF(clinu(1:8) .EQ. '#APPEND ') THEN                               u
      IB(9,K)=IB(8,K) + 1                                               u
      READ(CLINE,140) IB(15,K)                                          u
  140 FORMAT(8X,I2)                                                     u
      IE= IB(1,K) + IB(9,K) - 2                                         u
      DO 150 II=1,IB(15,K)                                              u
      IS=IE+1                                                           u
      IE=IS+IB(13,K) -1                                                 u
      READ(ISCN,90,END=800) (CSCN(I), I=IS,IE)                          u
  150 CONTINUE                                                          u
      GO TO 135                                                         u
      ELSE IF(clinu(1:11) .EQ. '#ENDSCREEN ') THEN                      u
C     ALL DONE CLEAN UP AND RETURN
C ------ Set Translate count negative to flag a flush of type-ahead
      IF ( LFLUSH ) IB(14,K) = -IB(14,K)                                u
      ISCRN=K                                                           u
      LSCN=.TRUE.                                                       u
      ELSE                                                              u
      GO TO 800                                                         u
      ENDIF                                                             u
C ------
C
      RETURN                                                            u
C
C ------
  700 WRITE(IDSP,710) cscrn0                                            u
  710 FORMAT(/,' Requested screen  "',A,'"  not found')                 u
      CALL WAITS (2.0)                                                  u
      LSCN=.FALSE.                                                      u
      RETURN                                                            u
  800 WRITE(IDSP,810) CLINE                                             u
  810 FORMAT(/,' **** ERROR - UNEXPECTED EOF ON SCREEN FILE'/1X,A/)     u
      LSCN=.FALSE.                                                      u
      RETURN                                                            u
  900 WRITE(IDSP,910) CSCNFL                                            u
  910 FORMAT(/,' **** ERROR - SCREEN FILE NOT FOUND: ',A/)              u
      LSCN=.FALSE.                                                      u
      RETURN                                                            u
 1010 WRITE(IDSP,1011) CLINE                                            u
 1011 FORMAT(/,' **** ERROR - ILLEGAL CHAR FOR INTEGER CONVERSION'      u
     +  /1X,A/)                                                         u
      CALL ABORT                                                        u
 1020 WRITE(IDSP,1021) CLINE                                            u
 1021 FORMAT(/,' **** ERROR - BUFFER TOO SMALL FOR SCREEN'/1X,A/)       u
      CALL ABORT                                                        u
 1050 WRITE(IDSP,1051) CLINE                                            u
 1051 FORMAT(/,' **** ERROR - EXPECTED #ATT    - NOT FOUND'/1X,A/)      u
      CALL ABORT                                                        u
 1060 WRITE(IDSP,1061) CLINE                                            u
 1061 FORMAT(/,' **** ERROR - EXPECTED #PROMPT - NOT FOUND'/1X,A/)      u
      CALL ABORT                                                        u
 1070 WRITE(IDSP,1071) CLINE                                            u
 1071 FORMAT(/,' **** ERROR - EXPECTED #TRANSLATE - NOT FOUND'/1X,A/)   u
      CALL ABORT                                                        u
      END
