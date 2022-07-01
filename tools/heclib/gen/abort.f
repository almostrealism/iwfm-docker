      SUBROUTINE ABORT                                                  MLu
C
C     TEMPOARY ABORT ROUTINE FOR NON-HARRIS SITES
C
      WRITE (6,10)                                                      MLu
 10   FORMAT (//,' ***** PROGRAM CALLED ABORT *****',//)                MLu
C
      CALL EXIT (2)                                                     u
C
C     GET THE SQUARE ROOT OF A NEGATIVE NUMBER TO CAUSE A FATAL ERROR
C     X = SIN (0.2)                                                     L
C     X = -X                                                            L
C     Y = SQRT (X)                                                      L
C     WRITE (6,*) Y                                                     L
      STOP                                                              MLu
      END                                                               MLu
