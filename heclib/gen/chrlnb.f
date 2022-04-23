      SUBROUTINE CHRLNB (CLINE,ILAST)                                   MLup
C
C     FINDS THE LAST NON-BLANK CHARACTER IN CHARACTER ARRAY CLINE
C
      CHARACTER CLINE*(*)                                               MLup
C
C     ILEN = LEN(CLINE)
C     ILAST = NINDXC ( CLINE, ILEN, 1, ' ')
C
      ILAST = NINDXR (CLINE, ' ')                                       MLup
C
      RETURN                                                            MLup
      END                                                               MLup
