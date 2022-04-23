      SUBROUTINE DATCLN (JULIN,INTIME,JULOUT,IOUTIM)
C
C     THIS ROUTINE CLEANS UP THE DATE AND MINUTES
C     SO THAT IOUTIM IS NEVER
C     GREATER THAN 1440 (THE NUMBER OF MINUTES PER DAY)
C
C     INTEGER*4 JULIN, JULOUT                                           ML
C
C
      IF (INTIME.GT.1440) THEN
      JULOUT = JULIN + INTIME/1440
      IOUTIM = MOD (INTIME,1440)
      ELSE IF (INTIME.LT.0) THEN
      JULOUT = JULIN + INTIME/1440 -1
      IOUTIM = MOD (INTIME,1440) + 1440
      ELSE
      JULOUT = JULIN
      IOUTIM = INTIME
      ENDIF
C
      IF (IOUTIM.EQ.0) THEN
      JULOUT = JULOUT - 1
      IOUTIM = 1440
      ENDIF
C
      RETURN
      END
