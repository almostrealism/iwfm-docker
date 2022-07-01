      SUBROUTINE ETIME
C
      COMMON /BTIMEC/ ISTART
      CHARACTER CSECS*8
C
      CALL CBTIME (IEND)
      JSECS = IEND - ISTART
      IHRS = JSECS / 3600
      IMIN = (JSECS - (IHRS * 60)) / 60
      ISEC = JSECS - (IHRS * 3600) - (IMIN * 60)
C
      WRITE (CSECS,'(I8)') JSECS
      J = NINDX (CSECS, ' ')
C
      WRITE (6,20) IHRS, IMIN, ISEC, CSECS(J:)
 20   FORMAT(' Elapsed Time = ',I2.2,':',I2.2,':',I2.2,
     * 3X,'(',A,' Seconds)')
C
      RETURN
      END
