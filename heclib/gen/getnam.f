      SUBROUTINE GETNAM (ILFN,CNAME,IFLAG)
C
      CHARACTER CNAME*(*)
C
      CNAME = ' '
      INQUIRE (UNIT=ILFN,NAME=CNAME,IOSTAT=IFLAG)
C
      IF ((ILFN.EQ.5).OR.(ILFN.EQ.6)) THEN                              u
         IF ((CNAME(1:2).EQ.'  ').AND.(IFLAG.EQ.0)) CNAME = '/dev/tty'  u
C        IF (CNAME(1:3).EQ.'std') CNAME = '/dev/tty'                    l
      ENDIF                                                             u
C
      RETURN
      END
