      INTEGER FUNCTION NSCAN (CSTR1, NBEG1, NLEN1, CSTR2, NBEG2, NLEN2)
C
C     SCAN CHARACTER STRING CSTR1 FOR FIRST OR LAST NON-OCCURANCE
C     OF ANY CHARACTERS IN CSTR2
C     WHERE CSTR1 BEGINS AT NBEG1 WITH LENGTH NLEN1
C     AND CSTR2 BEGINS AT NBEG2 WITH LENGTH NLEN2
C
C     IF NLEN1 IS POSITIVE, SEARCH IS MADE FROM LEFT TO RIGHT
C     IF NLEN1 IS NEGATIVE, SEARCH IS MADE FROM RIGHT TO LEFT
C
C     NSCAN IS ZERO IF ALL MATCHES ARE FOUND
C
C     YOU SHOULD USE NINDX OR NINDXR IF NLEN IS 1
C
C
      CHARACTER CSTR1*(*), CSTR2*(*)
C
C
      NSCAN = 0
      NBEG1M = NBEG1
      IF (NLEN1.GT.0) THEN
      NEND1 = NBEG1 + NLEN1 - 1
      ELSE
      NBEG1M = NBEG1 + NLEN1 + 1
      NEND1 = NBEG1
      ENDIF
      NEND2 = NBEG2 + NLEN2 - 1
C
      IF (NLEN2.EQ.1) THEN
      IF (NLEN1.GT.0) THEN
      NSCAN = NINDX (CSTR1(NBEG1M:NEND1),CSTR2(NBEG2:NEND2))
      ELSE
      NSCAN = NINDXR (CSTR1(NBEG1M:NEND1),CSTR2(NBEG2:NEND2))
      ENDIF
      IF (NSCAN.GT.0) NSCAN = NSCAN + NBEG1M - 1
      RETURN
      ENDIF
C
      IF (NLEN1.GT.0) THEN
      DO 10 I=NBEG1M,NEND1,1
      IPOS = INDEX (CSTR2(NBEG2:NEND2),CSTR1(I:I))
      IF (IPOS.EQ.0) THEN
      NSCAN = I
      RETURN
      ENDIF
 10   CONTINUE
C
      ELSE
C
      DO 20 I=NEND1,NBEG1M,-1
      IPOS = INDEX (CSTR2(NBEG2:NEND2),CSTR1(I:I))
      IF (IPOS.EQ.0) THEN
      NSCAN = I
      RETURN
      ENDIF
 20   CONTINUE
C
      ENDIF
C
      RETURN
      END
