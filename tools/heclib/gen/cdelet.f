      SUBROUTINE CDELET (CNAME, IERR)
C
      CHARACTER CNAME*(*)
      INTEGER UNLINK
C
      IERR = UNLINK(CNAME)
C
      RETURN
      END SUBROUTINE