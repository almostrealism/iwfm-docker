      INTEGER*8 FUNCTION I4TOI8 (I4)
C
C     Converts a 4 byte integer to a 8 byte integer without
C     messing up the sign bit
C
      INTEGER*4 I4
C
      INTEGER*4 IN4(2)
      INTEGER(8) I8
      EQUIVALENCE (I8, IN4(1))
C
      IN4(2) = I4
      IN4(1) = 0
      I4TOI8 = I8
C
      RETURN
      END
