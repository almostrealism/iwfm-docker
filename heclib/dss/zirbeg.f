      SUBROUTINE ZIRBEG (IFLTAB, JUL, CE, IYR, IMON, IDAY, IBLOCK,
     * MINBLK, INCBLK)
C
C     Get the beginning of an irregular block for irregular
C     interval time series data
C
C     GIVEN THE STARTING TIME AND THE TIME INTL, DETERMINE
C     THE STANDARD START DATE AND BLOCK LENGTH
C
C     JUL  - JULIAN DATE SINCE 31DEC1899
C     CE - E PART OF PATHNAME
C
      INTEGER IFLTAB(*)
      CHARACTER CE*(*), CINTLS(5)*10, CEPART*12
      INTEGER INTL, NDATA, ISTAT
C
      INCLUDE 'zdsskz.h'
C
      DATA CINTLS / 'IR-DAY    ', 'IR-MONTH  ',
     * 'IR-YEAR   ', 'IR-DECADE ', 'IR-CENTURY'/
C
C
      I = JLIYMD (JUL, IYR, IMON, IDAY)
C
      IF (CE(1:1).EQ.'~') THEN
         CEPART = CE(2:)
         ISTAT = 1
         CALL ZGINTL (INTL, CEPART, NDATA, ISTAT)
         IF (ISTAT.EQ.-1) THEN
            IBLOCK = -1
            GO TO 800
         ENDIF
         IF (INTL.LT.15) THEN
           IBLOCK = 1
         ELSE IF (INTL.LT.1440) THEN
           IBLOCK = 2
         ELSE IF (INTL.LT.10080) THEN
           IBLOCK = 3
         ELSE IF (INTL.LT.50000) THEN
           IBLOCK = 4
         ELSE
           IBLOCK = 5
        ENDIF
      ELSE
         CEPART = CE
         DO 20 I=1,5
           IF (CEPART(1:6).EQ.CINTLS(I)(1:6)) THEN
            IBLOCK = I
            GO TO 40
          ENDIF
 20     CONTINUE
        IBLOCK = -1
        GO TO 800
      ENDIF
C
 40   CONTINUE
      INCBLK = IFLTAB(KITSIN)
C
C     USE DAILY BLOCK
      IF (IBLOCK.EQ.1) THEN
      MINBLK = IFLTAB(KITSDA)
C
C     USE MONTHLY BLOCK - BACKUP TO FIRST DAY OF MONTH
      ELSE IF (IBLOCK.EQ.2) THEN
      IDAY = 1
      MINBLK = IFLTAB(KITSMO)
C
C     USE YEARLY BLOCK - BACKUP TO FIRST DAY OF YEAR
      ELSE IF (IBLOCK.EQ.3) THEN
      IDAY = 1
      IMON = 1
      MINBLK = IFLTAB(KITSYE)
C
C     USE DECADE BLOCK - BACKUP TO FIRST DAY OF DECADE
      ELSE IF (IBLOCK.EQ.4) THEN
      IDAY = 1
      IMON = 1
      IYR = (IYR/10)*10
      MINBLK = IFLTAB(KITSDE)
C
C     USE CENTURY BLOCK - BACKUP TO FIRST DAY OF CENTURY
      ELSE IF (IBLOCK.EQ.5) THEN
      IDAY = 1
      IMON = 1
      IYR = (IYR/100)*100
      MINBLK = IFLTAB(KITSCE)
C
      ENDIF
C
C
 800  CONTINUE
      RETURN
C
      END
