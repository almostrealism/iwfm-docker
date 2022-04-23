      SUBROUTINE PGFSCN ( CFIELD, CFMT, IVAL )
C ------
C ------ Check a field for valid reading with supplied format
C ------ If invalid, return neg ASCII code of right most character
C ------
      CHARACTER CFIELD*(*), CFMT*(*)                                    u
C     WRITE (*,*)'PREAD Screens object not loaded!'                     M
C     RETURN                                                            M
      READ ( CFIELD, FMT=CFMT, ERR=1000 ) IVAL                          u
      RETURN                                                            u
C ------ Error, set field to - ASCII code
 1000 I = LEN ( CFIELD )                                                u
      IVAL = - ICHAR ( CFIELD(I:I) )                                    u
      RETURN                                                            u
      END
