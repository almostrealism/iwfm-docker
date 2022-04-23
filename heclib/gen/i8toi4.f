      INTEGER*4 FUNCTION I8TOI4 (I8)
C
C     Converts a 8 byte integer to a 4 byte integer without
C     messing up the sign bit
C
      INTEGER*8 I8
C
      INTEGER*4 I4(2)
      INTEGER*8 IN8
      EQUIVALENCE (IN8, I4(1))
C
      IN8 = I8
      I8TOI4 = I4(2)
C
      RETURN
      END
