      SUBROUTINE PEND
C
C       SUBROUTINE 'PEND' IS THE SUBROUTINE CALLED BY THE USER AT THE
C       END OF HIS PROGRAM.  THE SUBROUTINE CLEANS UP AFTER PREAD, THAT
C       IS, IT CLOSES ALL OPENED FILES AND RESETS THE SYSTEM PROMT.
C
C
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLu
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLu
CADD C.PERROR                                                           H
      INCLUDE 'perrc.h'                                                 MLu
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLu
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
C
C
C
      IF (CTRMTY.EQ.'ASY') THEN
      IF (ITBLTY .EQ. 5) THEN
        CALL CHRWT(IKB,CHAR(27)//'%!0'//CHAR(27)//'ID8'//
     +            CHAR(27)//'%!1',12)
      CALL WAITS(0.5)                                                   u
      CALL CHRFL1()                                                     u
      ENDIF
      ENDIF
C
      IF (ISCT.GT.0) CLOSE (UNIT=ISCT, STATUS='DELETE')
      IF (IFUN.GT.0) CLOSE (UNIT=IFUN)
      IF (IMAC.GT.0) CLOSE (UNIT=IMAC)
      IF (IMEN.GT.0) CLOSE (UNIT=IMEN)
      IF (ILOG.GT.0) CLOSE (UNIT=ILOG)
      IF (ISCN.GT.0) CLOSE (UNIT=ISCN)
C
      RETURN
      END
