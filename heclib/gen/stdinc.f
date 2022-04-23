      SUBROUTINE STDINC (CWAIT, CECHO, CBREAK, CFLUSH, IASCII, ICODE)
C
C
      CHARACTER CWAIT*(*), CECHO*(*), CBREAK*(*), CFLUSH*(*)
      INTEGER IASCII, ICODE, NCHAR /0/
C
C     LOGICAL PEEKCHARQQ                                                M
C     CHARACTER GETCHARQQ*1                                             M
      CHARACTER CH*1
C
      IASCII = -2
      ICODE = 0
C
      CALL IOSAVE(0, 'Y', ISTAT)                                        u
C
      IF (CFLUSH(1:1).EQ.'Y') THEN
C        CALL FLUSH(5)                                                  M
         CALL IOSET(0, 0, 0, ISTAT)                                     u
         DO                                                             u
            CALL IOREAD(0, CH, 1, NCHAR)                                u
            IF (NCHAR.EQ.0) EXIT                                        u
         END DO                                                         u
      END IF
C
      IF (CWAIT(1:1).EQ.'Y') THEN                                       u
         CALL IOSET(0, 0, 1, ISTAT)                                     u
      ELSE                                                              u
         CALL IOSET(0, 0, 0, ISTAT)                                     u
      ENDIF                                                             u
      CALL IOREAD(0, CH, 1, NCHAR)                                      u
      CALL IOSAVE(0, 'N', ISTAT)                                        u
C
C     IF(CWAIT.EQ.'Y'.OR.PEEKCHARQQ()) THEN                             M
C        CH = GETCHARQQ()                                               M
C        NCHAR = 1                                                      M
C     ENDIF                                                             M
C
      IF (NCHAR.GT.0) THEN
         IASCII = ICHAR(CH)
C        IF(IASCII.EQ.0.OR.IASCII.EQ.224) THEN                          M
C           IASCII=0                                                    M
C           ICODE=ICHAR(GETCHARQQ())                                    M
C        ENDIF                                                          M
         IF (CECHO(1:1).EQ.'Y') CALL CHRWT(6, CH, 1)
      ENDIF
C
      RETURN
      END
