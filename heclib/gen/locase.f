      SUBROUTINE LOCASE ( CLINE)
C
C     ROUTINE TO UPPER CASE CHARACTER TO LOWER CASE
C
      CHARACTER CLINE*(*)
C
      ILEN = LEN(CLINE)
C
C     LOOP TO TEST EACH CHARACTER IN THE LINE.  IF CHARACTER IS
C     IN LOWER CASE RANGE THEN CHANGE TO UPPER CASE.
C
      DO 10 I=1, ILEN
      JCH = ICHAR( CLINE(I:I) )
      IF ((JCH .GT. 64) .AND. (JCH .LT. 91 ))
     * CLINE(I:I) = CHAR(JCH+32)
 10   CONTINUE
C
      RETURN
      END
