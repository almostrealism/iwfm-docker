      SUBROUTINE PITAB
C
C     DUMMY SUBROUTINE FOR PITAB
C
      RETURN
      END
C
      SUBROUTINE PRDMEN(C,I)
C
C     DUMMY SUBROUTINE FOR PRDMEN
C
      CHARACTER*1 C
      I = -1
      RETURN
      END
C
      SUBROUTINE PSLMEN(C1,C2,I)
C
C     DUMMY SUBROUTINE FOR PSLMEN
C
      CHARACTER*1 C1,C2
      I = -1
      RETURN
      END
      SUBROUTINE PANRED ( IUNIT, CPROMP, NPROMP, CLINE, NLINE)
C
C     DUMMY SUBROUTINE FOR PANRED
C
      CHARACTER CPROMP*(*), CLINE*(*)
C
      CALL ANREAD ( IUNIT, CPROMP, NPROMP, CLINE, NLINE)
      RETURN
      END
