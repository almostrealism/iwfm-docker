      SUBROUTINE ANREAD ( IUNIT, CPROMP, NPROMP, CLINE, NLINE)
C
C     PERFORM A PROMPTED READ
C
      CHARACTER CPROMP*(*), CLINE*(*)
      CHARACTER CHRBUF*132
      CHARACTER*1 CLF,CESC,CCX,CBS*3,CEXCL*2, CDLINE*4, CCS
      CHARACTER*1 CCR
      CHARACTER CNAM1*80, CNAM2*80
      CHARACTER CDLIM*1
      CHARACTER CNOCRRG*10
C
      LOGICAL NOWAIT,LMATCH
      LOGICAL LSCREEN,LNOCR
      LOGICAL LFIRST, LBATCH, LMOV, LPMT
      LOGICAL LRESET
      LOGICAL LMLD, LDLINE
C     INTEGER M1, M2, M3, M4                                            M
C
      SAVE LFIRST, LBATCH, IOUT
      SAVE CBS, CCR, CESC, CLR, CEXL, CCX, CCS, CDLINE, NDLINE, LDLINE
 
      INCLUDE 'pint.h'
      DATA LFIRST/.TRUE./
      DATA IOUT /6/                                                     u
C
      LRESET = .TRUE.
      LNOCR = .FALSE.
      IF(NPROMP.LT.0.OR.NLINE.EQ.-999) THEN
        LSCREEN = .TRUE.
      CALL GSTRRG ( 'pncr', CNOCRRG, NCNOCRRG, IS )
      CALL UPCASE(CNOCRRG)
      IF(CNOCRRG(1:NCNOCRRG).EQ.'YES') LNOCR=.TRUE.
      ELSE
        LSCREEN = .FALSE.
      ENDIF
C
C     SEE IF BATCH OR NOT
C
      IF (LFIRST) THEN
        LFIRST = .FALSE.
        LBATCH = .TRUE.
        CALL GETNAM (IUNIT, CNAM1, IERR)
        IF (CNAM1.EQ.'/dev/tty') LBATCH = .FALSE.
        IF (CNAM1.EQ.'stdin')    LBATCH = .FALSE.
        IF (CNAM1.EQ.'CONIN$')   LBATCH = .FALSE.
        IF(.NOT.LBATCH) THEN                                            u
          CALL IOSAVE(0,'Y',IST)                                        u
          IF(IST.EQ.0) THEN                                             u
            CALL IOSAVE(0,'N',IST)                                      u
          ELSE                                                          u
            LBATCH=.TRUE.                                               u
          ENDIF                                                         u
        ENDIF                                                           u
C
        CBS=CHAR(8)//CHAR(32)//CHAR(8)
        CCR=CHAR(13)
        CESC=CHAR(27)
        CLF = CHAR (10)
        CEXCL = CHAR (33) // CCR
        CCX=CHAR(24)
        CCS = CHAR ( 19 )
        CDLINE = CHAR(27)
        NDLINE = 1
        LDLINE = .TRUE.
      ENDIF
C
C     IF BATCH, JUST READ THE LINE AND RETURN
C
      IF (LBATCH) THEN
        READ (IUNIT, '(A)', END=140) CLINE
        CALL CHRLNB (CLINE, NLINE)
        GO TO 160
 140    NLINE = -1
        CLINE = ' '
 160    ISIA = 0
        RETURN
      ENDIF
C
C     HERE FOR INTERACTIVE
C
      NMAX = LEN(CLINE)
      CDLIM(1:1) = CHAR(0)
      LMLD = .FALSE.
      NSECS = ISIA
      IF ( NSECS .LE. 0 ) THEN
        NOWAIT = .TRUE.
      ELSE
        NOWAIT = .FALSE.
C       MAXWAT = NSECS * 120                                            M
        MAXWAT = NSECS * 10                                             u
      ENDIF
C
      CALL IOSAVE(0,'Y',IST)                                            u
C
      READ_RESPONSE: DO
        IF (LRESET) THEN
          CLINE(1:NMAX) = ' '
C
C         ISSUE PROMPT
C
          NP = NPROMP
          LMOV = .TRUE.
          LPMT = .TRUE.
          IF (NP .LT. 0) LMOV = .FALSE.
          IF (NP .EQ. -99 .OR. NP .EQ. 0 ) LPMT = .FALSE.
          IF (LMOV) CALL IOWRITE(1,CCR,1,IERR)
          NP = IABS(NP)
          IF (LPMT) CALL IOWRITE(1,CPROMP,NP,IERR)
C
C         READ IN RESPONSE (without echoing)
C
          J=0
          LRESET = .FALSE.
        END IF
C
        IF (NOWAIT) THEN
          READ_NO_TIMEOUT: DO
            CALL STDINC ('Y','N','N','N',JCHAR,K)
            IF (JCHAR.GT.0) EXIT READ_NO_TIMEOUT
          END DO READ_NO_TIMEOUT
          NCHS = 1
          CHRBUF(1:1) = CHAR(JCHAR)
        ELSE
          !-----------------------------!
          ! Check for a user timing out !
          !-----------------------------!
C         INACT = 0                                                     M
C         IWAIT = 2                                                     M
          NWAIT = 0
C
          READ_CHARS: DO
            !--------------------------------------!
            ! Read without waiting for a character !
            !--------------------------------------!
            NCHS = 0
            READ_TIMEOUT: DO
              CALL STDINC('N','N','N','N',JCHAR,K)
              IF (JCHAR.LE.0) EXIT READ_TIMEOUT
              NCHS = NCHS + 1
              CHRBUF(NCHS:NCHS) = CHAR(JCHAR)
            END DO READ_TIMEOUT
C
            !---------------------------------------------------!
            ! If a character was received, jump out of the loop !
            !---------------------------------------------------!
            IF (NCHS.GT.0) EXIT READ_CHARS
C
            IF (.NOT. NOWAIT) THEN
              !------------------------------------------------------!
              ! Have we exceeded the maximum amount of time to wait? !
              !------------------------------------------------------!
              IF (NWAIT.GE.MAXWAT) THEN
                NLINE = -3
                EXIT READ_RESPONSE
              ENDIF
C
              !------------------------------------------------------!
              ! Wait for the specified amount of time, then bump the !
              ! counter                                              !
              !------------------------------------------------------!
C             CALL WAIT (IWAIT, 0, K)                                   M
C             INACT = INACT + 1                                         M
C             NWAIT = NWAIT + IWAIT                                     M
C             IF (INACT.EQ.4) THEN                                      M
C               IWAIT = 4                                               M
C             ELSE IF (INACT.EQ.30) THEN                                M
C               IWAIT = 10                                              M
C             ELSE IF (INACT.EQ.80) THEN                                M
C               IWAIT = 20                                              M
C             ELSE IF (INACT.EQ.160) THEN                               M
C               IWAIT = 30                                              M
C             ENDIF                                                     M
              NWAIT = NWAIT + 1                                         u
            END IF
          END DO READ_CHARS
        ENDIF
C
C       Here when we're done reading the input
C
        CHECK_LOOP: DO
          CHECK_CHARS: DO K=1,NCHS
C
C           CHECK FOR CONTROL CHAR, BRANCH ON ALPHA-NUMERIC
C
            IF ((CHRBUF(K:K).EQ.CCR).OR.(CHRBUF(K:K).EQ.CLF)) THEN
              !-----------------!
              ! Carriage return !
              !-----------------!
              CALL IOWRITE(1,CCR,1,IERR)
              IF(LMOV) CALL IOWRITE(1,CLF,1,IERR)
              NLINE=MIN0(NMAX,J)
              IF (NLINE .LT. 0) NLINE = 0
              EXIT READ_RESPONSE
C
            ELSE IF(CHRBUF(K:K).EQ.CBS(1:1)) THEN
              !-----------!
              ! Backspace !
              !-----------!
              IF (J.GT.0) THEN
                CALL IOWRITE(1,CBS,3,IERR)
                CLINE(J:J) = ' '
                J=J-1
              ENDIF 
              IF (J.EQ.0) THEN
                ! Backspaced into prompt
                LRESET = .TRUE.
                CYCLE READ_RESPONSE
              ENDIF
C
            ELSE
              !-----------------------------------!
              ! Normal char, echo for full duplex !
              !-----------------------------------!
              CALL IOWRITE(1,CHRBUF(K:K),1,IERR)
              J = J + 1
              IF (J.GT.NMAX) J=NMAX
              CLINE(J:J) = CHRBUF(K:K)
            ENDIF
          END DO CHECK_CHARS
C
          IF(LSCREEN.AND.LNOCR.AND.CLINE(J:J).NE.' ') THEN
            JJ=J
            CALL PSCHIN(CLINE,J,LMATCH)
            IF(LMATCH) THEN
              NADD=J-JJ
              NCHS = 1 + NADD
              IF(NADD.GT.0) THEN
                CHRBUF(1:NCHS)=CLINE(JJ+1:J)//CCR
                CLINE(JJ+1:)=' '
                CALL IOWRITE(1,CHRBUF,NCHS,IERR)
                J=JJ
              ELSE
                CHRBUF(1:1)=CCR
              ENDIF
              CYCLE CHECK_LOOP
            ENDIF
          ENDIF
          EXIT CHECK_LOOP
        END DO CHECK_LOOP
      END DO READ_RESPONSE
C
      CALL IOSAVE(0,'N',IST)                                            u
C
      ISIA = 0
      RETURN
C
C= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
C
      ENTRY ANROUT (IOUNIT)
      IOUT = IOUNIT
      ISIA = 0
      RETURN
C
      END
 
