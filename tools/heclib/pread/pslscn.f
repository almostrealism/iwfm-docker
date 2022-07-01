      SUBROUTINE PSLSCN (LRESP)
C
C        This routine writes the desired screen to the display
C        and returns the translated response in CLINE.
C        If more than one line is associated with the response
C        the first will be returned in CLINE and the rest will
C        be written to the macro buffer for subsequent use.
C
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
C
C
C
      INCLUDE 'pchar.h'                                                 u
      INCLUDE 'pint.h'                                                  u
      INCLUDE 'pnums.h'                                                 u
      INCLUDE 'plflag.h'                                                u
      INCLUDE 'pscnch.h'                                                u
C
C ------
      CHARACTER CNM*66, CLATT*80, CATT*1, CPRMTA*1, CNATT*1             u
C ------
C
      LOGICAL LANSI,LFRST,LCRT                                          u
      INTEGER IVUNIT                                                    u
      COMMON/LANSI/LANSI,LFRST,LCRT,IVUNIT                              u
C
      CHARACTER*132 CLINX, CTMP*8                                       u
      CHARACTER*1 CDUMMY, CPRMT*80, CSCNRG*7, CCR, CLF, CSAB*5, CREG*6  u
      LOGICAL LFIRST, LEND, LRESP, LBTEST, LSHOW, LABBR, LTMP, LABORT   u
      LOGICAL LFLUSH, LATT, LMESSY, LCASEY                              u
C
C     WRITE (*,*)'PREAD Screens object not loaded!'                     M
C     RETURN                                                            M
C
      DATA LFIRST /.TRUE./                                              u
C ------ The following is all 2-char abreviations for control char
      DATA CNM / 'NUSHSXEXETEQAKBLBSHTLFVTFFCRSOSIDLD1D2D3D4NKSYEBCNEMSBu
     *ECFSGSRSUSDT' /                                                   u
C
      CCR = CHAR(13)                                                    u
      CLF = CHAR(10)                                                    u
C
      CDUMMY = ' '                                                      u
      LSCN=.FALSE.                                                      u
      LRESP = .TRUE.                                                    u
      LMESSY = .FALSE.                                                  u
      K=ISCRN                                                           u
C
C     CHECK IF AN ABORT IS IN PROGRESS
      CALL GSTRRG ( 'SAB', CSAB, ISAB, IS )                             u
      CALL UPCASE(CSAB)                                                 u
      LABORT = .FALSE.                                                  u
      IF ( IS.EQ.0 .AND. CSAB(1:5).EQ.'ABORT' ) THEN                    u
      LABORT = .TRUE.                                                   u
      NERROR = 999                                                      u
      IB(26,K) = 1                                                      u
      CLINE = '#ABORT'                                                  u
      JNCHR = 6                                                         u
      ENDIF                                                             u
C ------
C ------ Check for function character occurance in screen
C ------ definition line, replace with function value
C ------
      IB21 = IB(21,K)                                                   u
      CALL PSFSCN ( IB21 )                                              u
      IB22 = IB(22,K)                                                   u
      CALL PSFSCN ( IB22 )                                              u
      IB19 = IB(19,K)                                                   u
      CALL PSFSCN ( IB19 )                                              u
      IB20 = IB(20,K)                                                   u
      CALL PSFSCN ( IB20 )                                              u
      IB26 = IB(26,K)                                                   u
      CALL PSFSCN ( IB26 )                                              u
      IB27 = IB(27,K)                                                   u
      CALL PSFSCN ( IB27 )                                              u
      IB28 = IB(28,K)                                                   u
      CALL PSFSCN ( IB28 )                                              u
      IB30 = IB(30,K)                                                   u
      CALL PSFSCN ( IB30 )                                              u
      IB31 = IB(31,K)                                                   u
      CALL PSFSCN ( IB31 )                                              u
C ------
C ------ Check if translate count negative - flush typeahead buffer
      LFLUSH = .FALSE.                                                  u
      IF ( IB(14,K) .LT. 0 ) THEN                                       u
      IB(14,K) = IABS( IB(14,K) )                                       u
      LFLUSH = .TRUE.                                                   u
      ENDIF                                                             u
C
C ------ Check if ATTRIBUTES available
      LATT = .FALSE.                                                    u
      IF ( IB(29,K) .GT. 0 ) THEN                                       u
      LATT = .TRUE.                                                     u
      CATT = CHAR(255)                                                  u
      JATT = IB(11,K) * IB(12,K)                                        u
      KATT = IB(1,K) + IB(4,K)                                          u
      IF ( IB(30,K) .EQ. 0 ) LATT = .FALSE.                             u
      ENDIF                                                             u
C
C     CHECK WHAT LEVEL OF SCREENS TO DISPLAY
      CALL GSTRRG ( 'SCN', CSCNRG, NSCNRG, IS )                         u
      CALL UPCASE(CSCNRG)                                               u
      IF (IS.EQ.0 .AND. CSCNRG(1:6).EQ.'NOSCNS' .AND. NSCNRG.EQ.7) THEN u
      LSHOW = .FALSE.                                                   u
      READ ( CSCNRG, '(6X,I1)' ,IOSTAT=IERR) INOSCN                     u
      IF ( IERR.NE.0 ) LSHOW = .TRUE.                                   u
      IF ( IB28 .GE. INOSCN ) LSHOW = .TRUE.                            u
      ELSE                                                              u
      LSHOW = .TRUE.                                                    u
      ENDIF                                                             u
C
C     CHECK IF ONETIME FORCE SCREEN VISABLE OR INVISABLE
C
      IF(LVSCN) THEN                                                    u
         LSHOW=.TRUE.                                                   u
         LVSCN=.FALSE.                                                  u
      ENDIF                                                             u
C
      IF(LISCN) THEN                                                    u
         LSHOW=.FALSE.                                                  u
         LISCN=.FALSE.                                                  u
      ENDIF                                                             u
C
C
C     ERASE GRAPHICS AREA AND DIALOG IF BIT 2 SET (VALUE=4)
      IF(LBTEST(IB20,2)) THEN                                           u
      CALL VERASG                                                       u
      CALL WAITS (0.1)                                                  u
      CALL VSET ( 'ANSI', '----' )                                      u
      CALL WAITS (0.1)                                                  u
      CALL VERASE                                                       u
      ENDIF                                                             u
C
C     ERASE SCREEN IF FLAG SET TO ODD VALUE  1,3,7,F
      IF(LBTEST(IB20,0)) THEN                                           u
      CALL WAITS (0.1)                                                  u
      CALL VSET ( 'ANSI', '----' )                                      u
      CALL WAITS (0.1)                                                  u
      CALL VERASE                                                       u
      ENDIF                                                             u
C
      IF(LBTEST(IB20,3)) THEN                                           u
      CALL WAITS (0.1)                                                  u
      CALL VSET ( 'ANSI', 'ANSI' )                                      u
      ENDIF                                                             u
C
      IF ( LABORT ) GO TO 23                                            u
C
C     WRITE SCREEN IMAGE TO DISPLAY
C
      JC=IB22                                                           u
      IS=IB(1,K) + IB(5,K) - 2 - IB(11,K)                               u
      ISATT = IB(1,K) + IB(5,K) + IB(11,K)*IB(12,K)*2 - 1               u
C ------ Check if null defined attribute exists
      CALL PSCATT ( CNATT, CSCN, ISATT, IB(11,K),IB(29,K),-IB30,INSTAT) u
      ITMP = IB(11,K)                                                   u
      DO 10 I=1,IB(12,K)                                                u
         JR=IB21+I-1                                                    u
         IS=IS + IB(11,K)                                               u
         IF ( INSTAT .EQ. 4 .AND. CSCN(IS+JATT+1) .EQ. CNATT )GO TO 904 u
         CALL CHRBLK ( CLINX )                                          u
         DO 9 J=1,ITMP                                                  u
         IF ( LATT ) CLATT(J:J) = CSCN(IS+J+JATT)                       u
    9    CLINX(J:J) = CSCN(IS+J)                                        u
C ------- Set up for expanding forced function references
C ------- within the screen text itself
         LTMP = LFUN                                                    u
         LFUN = .FALSE.                                                 u
         CLINE = CLINX(1:ITMP)                                          u
         INCHR = ITMP                                                   u
         CALL PEXLIN                                                    u
         LFUN = LTMP                                                    u
         CLINX = CLINE                                                  u
         CALL CHRLNB ( CLINX, NCH)                                      u
      IF ( LSHOW ) THEN                                                 u
         CALL VMOVCR (JR,JC)                                            u
       IF ( LATT ) THEN                                                 u
         I901S = 1                                                      u
         I901E = IB(11,K)                                               u
  903    DO 901 I901 = I901S, I901E                                     u
         IF(CLATT(I901:I901) .NE. CATT ) THEN                           u
         CATT = CLATT(I901:I901)                                        u
         GO TO 902                                                      u
         ENDIF                                                          u
  901    CONTINUE                                                       u
         I901 = I901E + 1                                               u
  902    N901 = I901 - I901S                                            u
         CALL CHRWT ( IDSP, CLINX(I901S:I901), N901 )                   u
         IF ( I901 .LE. I901E ) THEN                                    u
         I901S = I901                                                   u
         CALL PSCATT ( CATT, CSCN, ISATT,IB(11,K),IB(29,K),IB30,IPSTAT) u
         IF ( IPSTAT .EQ. 2 ) GO TO 904                                 u
         GO TO 903                                                      u
         ENDIF                                                          u
       ELSE                                                             u
         CALL CHRWT(IDSP,CLINX,NCH)                                     u
       ENDIF                                                            u
      ENDIF                                                             u
  904 CONTINUE                                                          u
C     REMEMBER THE PROMPT LINE
      IF ( I .EQ. IB(17,K) ) THEN                                       u
      NPRMT = NCH                                                       u
      CPRMT = CLINX(1:NCH)                                              u
      CPRMTA = CLATT(IB(18,K):IB(18,K))                                 u
      ENDIF                                                             u
C
   10 CONTINUE                                                          u
C
      ISIA = ISIA00                                                     u
C
C     MOVE CURSOR TO PROMPT LOCATION
C
      IF ( LCRT ) THEN                                                  u
      IPR = 0                                                           u
      IPC = 1                                                           u
      ELSE                                                              u
      IPR = IB(17,K) + IB21 - 1                                         u
      IPC = IB(18,K) + IB22 - 1                                         u
      ENDIF                                                             u
C
      IF ( LSHOW ) CALL VMOVCR (IPR, IPC)                               u
C
C     NOW GET REPLY
C     CHECK IF NO REPLIES POSSIBLE (IE IB(13,K).LE.0)
      IF(IB(13,K).LE.0) THEN                                            u
      LRESP=.FALSE.                                                     u
      RETURN                                                            u
      ENDIF                                                             u
C
      NERROR = 0                                                        u
   22 CONTINUE                                                          u
C
C     SET UP INACTIVITY TIME OUT IF NECESSARY
      ISIA00 = ISIA                                                     u
      ISIA = IB27                                                       u
C ------ Check if AUTO Mode for screens is on
      IF ( LAUTO ) THEN                                                 u
C ------ Check if there is an #AUTO response defined for this screen
C
      CLINE = '#AUTO'                                                   u
      INCHR = 5                                                         u
C     SEARCH TRANSLATE TABLE FOR MATCH
      IS=IB(1,K) + IB(7,K) - IB(13,K) - 1 + 2                           u
      DO 1030 I=1,IB(14,K)                                              u
      IS=IS + IB(13,K)                                                  u
      DO 1025 II=1,INCHR                                                u
      IF(CSCN(IS+II-1) .NE. CLINE(II:II)) GO TO 1030                    u
 1025 CONTINUE                                                          u
C ------ Found #AUTO go to response processing area
C ------ I know we re-search table there, but don't worry about it!
      GO TO 1111                                                        u
 1030 CONTINUE                                                          u
C ------ No match - continue
      ENDIF                                                             u
C ------ AUTO mode not on, go directly to keyboard for response
      IF ( LATT ) THEN                                                  u
      CALL PSCATT ( CSCN(KATT),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)u
      ENDIF                                                             u
      IF ( .NOT. LSHOW ) THEN                                           u
      CALL CHRWT(IDSP,CCR//CLF, 2)                                      u
      JNCHR = -999                                                      u
      CALL ANREAD ( IKB, CPRMT, NPRMT, CLINE, JNCHR )                   u
      ELSE                                                              u
      CALL VMOVCR (IPR, IPC)                                            u
      JNCHR = -999                                                      u
      CALL ANREAD(IKB,CDUMMY,-99,CLINE,JNCHR)                           u
      ENDIF                                                             u
C ------
C ------
   23 CALL CHRBLK(CKBLIN)                                               u
      IF ( LATT ) THEN                                                  u
      CALL PSCATT(CSCN(KATT+1),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)u
      ENDIF                                                             u
      IF(JNCHR .GT. 0 ) CKBLIN(1:JNCHR) = CLINE(1:JNCHR)                u
      INCHR = JNCHR                                                     u
      IF(INCHR.GT.8) INCHR=8                                            u
      IF(INCHR.EQ.0 .OR. INCHR.EQ.-1) THEN                              u
      INCHR = 3                                                         u
      CLINE = '^CR'                                                     u
      ELSE IF ( INCHR .EQ. -3) THEN                                     u
      CLINE = '#INACT'                                                  u
      INCHR = 6                                                         u
c
c
      ELSE IF ( INCHR .EQ. -4) THEN                                     u
      CLINE = '#TIMEOUT'                                                u
      INCHR = 8                                                         u
C ------ CHECK FOR CONTROL CHARACTERS AND SET TO ^nm
      ELSE IF ( INCHR .EQ. 1 ) THEN                                     u
      ICC = ICHAR ( CKBLIN(1:1) )                                       u
      IF ( ICC .LT. 32 ) THEN                                           u
      ICC = ICC * 2 + 1                                                 u
      CLINE = '^'//CNM(ICC:ICC+1)                                       u
      INCHR = 3                                                         u
      ELSE IF ( ICC .EQ. 127 ) THEN                                     u
      CLINE = '^'//CNM(65:66)                                           u
      INCHR = 3                                                         u
      ENDIF                                                             u
      ENDIF                                                             u
C     SEARCH TRANSLATE TABLE FOR MATCH
 1111 IS=IB(1,K) + IB(7,K) - IB(13,K) - 1 + 2                           u
      DO 30 I=1,IB(14,K)                                                u
      IS=IS + IB(13,K)                                                  u
C     CHECK IF FULL RESPONSE OR ABBREVIATIONS ARE ALLOWED
      LABBR = .TRUE.                                                    u
      IF ( CSCN(IS-1) .EQ. ':' ) LABBR = .FALSE.                        u
      IF ( CSCN(IS-1) .EQ. ';' ) LABBR = .FALSE.                        u
C------ Check if Case sensitive response
      LCASEY = .TRUE.                                                   u
      IF ( CSCN(IS-1) .EQ. ',' ) LCASEY = .FALSE.                       u
      IF ( CSCN(IS-1) .EQ. ';' ) LCASEY = .FALSE.                       u
      CTMP = CLINE                                                      u
      IF ( .NOT. LCASEY ) CALL UPCASE ( CTMP )                          u
C
C     FIRST CHECK FOR TABLE ENTRY OF #ANY
      IF(CSCN(IS) .EQ. '#') THEN                                        u
      IF(CSCN(IS+1).EQ.'A'.AND.CSCN(IS+2).EQ.'N'.AND.CSCN(IS+3).EQ.'Y') u
     1  GO TO 27                                                        u
      ENDIF                                                             u
      DO 25 II=1,INCHR                                                  u
      IF(CSCN(IS+II-1) .NE. CTMP (II:II)) GO TO 30                      u
C------ Is this a potential match???
   25 CONTINUE                                                          u
C     CHECK FOR ABBREVIATION
      IF ( .NOT. LABBR ) THEN                                           u
C     NEXT POSITION MUST BE BLANK TO BE OK
      IF( CSCN(IS+INCHR) .EQ. ' ' ) GO TO 27                            u
      GO TO 30                                                          u
      ENDIF                                                             u
C     FOUND MATCH
C
C     STUFF ONLY ONE LINE IN CLINE, REST IN MACRO BUFFER
   27 CALL CHRBLK (CLINE)                                               u
C ------
C ------ CLEAN UP IF THERE IS AN OLD ERROR MESSAGE THERE
C ------
      IF ( LMESSY ) THEN                                                u
      IF ( LATT ) THEN                                                  u
      CALL PSCATT(CSCN(KATT+2),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)u
      ENDIF                                                             u
      IF ( LSHOW ) THEN                                                 u
      CALL VMOVCR (IMR, IMC)                                            u
      CALL CHRWT(IDSP,'                                                'u
     +  ,NMESSY)                                                        u
      ENDIF                                                             u
      IF ( LATT ) THEN                                                  u
      CALL PSCATT(CSCN(KATT+1),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)u
      LMESSY = .FALSE.                                                  u
      ENDIF                                                             u
      ENDIF                                                             u
C ------ ALL CLEANED UP - CONTINUE
      IS=IS+9                                                           u
      DO 28 II = 1,IB(13,K)-11                                          u
   28 CLINE(II:II) = CSCN(IS+II-1)                                      u
C     CLINE = CNICE(12:)
      IF(CSCN(IS-11).NE.'1'.OR.IB(15,K).GT.0) THEN                      u
C     NEED TO USE MACRO BUFFER FOR MULT-LINES
      NL=ICHAR(CSCN(IS-11)) - 48   - 1                                  u
      IS=IS+IB(13,K)                                                    u
      NLX=NL                                                            u
      ML=IB(15,K)                                                       u
      MLX=ML                                                            u
C--------------------------------------------------------------------
C   ** MOVE TO TOP OF BUFFER
C   ***
      DO 930 J=1,IMACP+1                                                u
      CALL CHRBLK(CLINX)                                                u
      CALL PNXTLN(K,IS,NL,NLX,ML,MLX,CLINX,LEND)                        u
      IF(LEND) GO TO 900                                                u
C   ***
C   ***
C   ***
        IF ( J .EQ. IMACP+1 ) GOTO 1100                                 u
        CLBUFM( J ) = CLINX                                             u
 930  CONTINUE                                                          u
 1100 WRITE (IDSP,1110) IMXMCP                                          u
 1110 FORMAT(/' ** MACRO BUFFER OVERFLOW -- MAX LINES = ',I5/)          u
      IMACP = IMXMCP                                                    u
      LMACRO = .FALSE.                                                  u
      ISIA = ISIA00                                                     u
      RETURN                                                            u
C   ***
C   ***
C   ** HIT END OF MACRO, MOVE IT DOWN
C   ***
 900  CONTINUE                                                          u
      IF ( J .EQ. (IMACP+1) ) THEN                                      u
         IMACP = 0                                                      u
         GOTO 920                                                       u
      ENDIF                                                             u
      DO 910 IM = (J-1), 1, -1                                          u
         CLBUFM( IMACP ) = CLBUFM( IM )                                 u
         IMACP = IMACP - 1                                              u
 910  CONTINUE                                                          u
 920  LMACRO = .TRUE.                                                   u
C   ***
      ENDIF                                                             u
C
      IF(LBTEST(IB20,1)) THEN                                           u
      CALL VERASE                                                       u
      CALL WAITS (0.1)                                                  u
      CALL VSET ( 'ANSI', '----' )                                      u
      CALL WAITS (0.1)                                                  u
      CALL VERASE                                                       u
      ELSE  IF (LSHOW) THEN                                             u
      ITR = IB(12,K) + IB21                                             u
      CALL VMOVCR ( ITR, 1 )                                            u
      ENDIF                                                             u
      IF(LBTEST(IB20,3)) THEN                                           u
      CALL WAITS (0.1)                                                  u
      CALL VSET ( 'TEK', '----' )                                       u
      ENDIF                                                             u
C
      ISIA = ISIA00                                                     u
      RETURN                                                            u
C   ***
C     NO MATCH TRY NEXT LINE
   30 CONTINUE                                                          u
      NERROR = NERROR + 1                                               u
C     CHECK FOR TOO MANY ERRORS IN A SEQUENCE
      IF ( IB26 .GT. 0 ) THEN                                           u
      IF ( NERROR .GT. IB26 +1 ) THEN                                   u
      CLINE = '#ABORT'                                                  u
      LRESP = .TRUE.                                                    u
      ISIA = ISIA00                                                     u
      RETURN                                                            u
      ELSE IF ( NERROR .EQ. IB26 +1 ) THEN                              u
      CLINE = '#ABORT'                                                  u
      JNCHR = 6                                                         u
      GO TO 23                                                          u
      ELSE IF ( NERROR .EQ. IB26    ) THEN                              u
      CLINE = '#SEQERR'                                                 u
      JNCHR = 7                                                         u
      GO TO 23                                                          u
      ENDIF                                                             u
      ENDIF                                                             u
C     NO MATCH IN ENTIRE TABLE, WRITE MESSAGE AND REQUEST AGAIN
   35 IMR = IB(23,K) + IB21 - 1                                         u
      IMC = IB(24,K) + IB22 - 1                                         u
      IF ( LATT ) THEN                                                  u
      CALL PSCATT(CSCN(KATT+2),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)u
      ENDIF                                                             u
      IF (.NOT. LSHOW ) THEN                                            u
      NMESSY = INCHR + 27                                               u
      CALL CHRWT(IDSP,CCR//CLF//'Invalid --"'//CLINE(1:INCHR)           u
     + //'"-- Re-enter'//CCR//CLF,NMESSY)                               u
      ELSE                                                              u
      CALL VMOVCR (IMR, IMC)                                            u
      IF ( NMESSY .LE. 0 ) NMESSY = 1                                   u
      IF ( LMESSY )                                                     u
     +CALL CHRWT(IDSP,'                                                'u
     +  ,NMESSY)                                                        u
      CALL VMOVCR (IMR, IMC)                                            u
      NMESSY = INCHR + 23                                               u
      CALL CHRWT(IDSP,'Invalid --"'//CLINE(1:INCHR)                     u
     + //'"-- Re-enter',NMESSY)                                         u
      LMESSY = .TRUE.                                                   u
      CALL VMOVCR (IPR, IPC)                                            u
      IF ( LATT ) THEN                                                  u
      CALL PSCATT ( CPRMTA, CSCN, ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)  u
      ENDIF                                                             u
      CALL CHRWT(IDSP,'                                     ',JNCHR)    u
      ENDIF                                                             u
      IF ( LATT ) THEN                                                  u
      CALL PSCATT(CSCN(KATT+1),CSCN,ISATT,IB(11,K),IB(29,K),IB30,IPSTAT)u
      ENDIF                                                             u
      GO TO 22                                                          u
      END
