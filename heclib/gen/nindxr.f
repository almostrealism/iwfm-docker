      INTEGER FUNCTION NINDXR ( CSTR1, CSTR2)
C
C     REVERSE NON-INDEX
C     SEARCHES CHARACTER STRING CSTR1 FOR FIRST NON-OCCURANCE
C     OF STRING CSTR2
C     GOING BACKWARDS (I.E. FROM END OF CSTR1)
C     NINDXR WILL BE THE POSITION OF THE LEFT MOST
C     CHARACTER IN THE FIRST NON-MATCH OF THE LAST OCCURANCE OF CSTR2
C     IF ALL MATCH, NINDXR WILL BE ZERO
C     ALL MOST ALWAYS, CSTR2 WILL BE ONE CHARACTER LONG
C
      CHARACTER CSTR1*(*), CSTR2*(*)
C
      IEND1 = LEN (CSTR1)
      IEND2 = LEN (CSTR2)
C
      NINDXR = 0
C     IS THE STRING TO MATCH TOO LONG?
      IF (IEND2.GT.IEND1) RETURN
C
      N = IEND1 - IEND2 + 1
      IB = N
      IE = IEND1
C
      DO 10 I=1,N
      IF (CSTR2(1:IEND2).NE.CSTR1(IB:IE)) THEN
      NINDXR = IB
      RETURN
      ENDIF
C     NEXT CHARACTER BACKWARDS
      IB = IB - 1
      IE = IE - 1
 10   CONTINUE
C
      RETURN
      END
