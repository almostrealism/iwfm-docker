      SUBROUTINE PSCHIN (CLINE,NNCHR,LMATCH)
C
C        This routine checks the input as it is being typed in
C        when a screen is being used.  It will fill out the
C        response when enough character have been typed to make
C        the response unique.
C
C     CALL ABORT('Routine PSCHIN is only available on Unix.')           M
C
C
C       USE OF ARRAY  IB(I,K) WHERE I=ITEM NUMBER BELOW
C     1       BEGIN BYTE POSITION OF CHAR AREA FOR ENTIRE INFO
C     2       END
C     3       BEGIN RELATIVE BYTE POS OF NAME
C     4       END
C     5       BEGIN RELATIVE BYTE POS OF SCREEN IMAGE
C     6       END
C     7       BEGIN RELATIVE BYTE POS OF TRANSLATE TABLE
C     8       END
C     9       BEGIN RELATIVE BYTE POS OF APPEND TABLE
C     10      END
C     11      # COLS IN SCREEN IMAGE
C     12      # ROWS IN SCREEN IMAGE
C     13      # COLS IN TRANSLATE/APPEND LINES ( ZERO FOR NO REPLY)
C     14      # TRANSLATE LINES
C     15      # APPEND LINES
C     16      TOTAL # BYTES REQ FOR ALL CHAR STORAGE
C     17      PROMPT CURSOR ROW
C     18      PROMPT CURSOR COL
C     19      SCREEN RETENTION PRIORITY
C     20      FULL SCREEN ERASE FLAG
C     21      BASE ROW
C     22      BASE COL
C     23      MESSAGE CURSOR ROW
C     24      MESSAGE CURSOR COL
C     25      ?
C     26      NUMBER OF ALLOWABLE CONSEQUECTIVE ERRORS
C     27      INACTIVITY TIMOUT IN SECONDS FOR THIS SCREEN
C     28      SCREEN DISPLAY LEVEL 0-9
C     30      OFFSET POSITION IN ATTMAP LINE FOR ATTRIBUTES
C
C        USE OF ARRAY  IC(I)  WHERE  I=ITEM NUMBER BELOW
C
C     1       MAX # OF SCREENS SAVED (ONE LESS THAN DIMENSION  KIC)
C     2       NEXT AVAILABLE SLOT
C     3       MAX # CHAR IN CHAR AREA CSCN
C     4       NEXT AVAILABLE CHAR IN CHAR AREA
C
C
C
C
C
      INCLUDE 'pint.h'                                                  u
      INCLUDE 'plflag.h'                                                u
      INCLUDE 'pscnch.h'                                                u
C
C ------
C
      CHARACTER CNICE*132,CTMP*8,CTEMP*8                                u
      CHARACTER CLINE*(*)                                               u
      LOGICAL LABBR,LABB                                                u
      LOGICAL LCASEY                                                    u
      LOGICAL LMATCH                                                    u
C
      NUMATCH = 0                                                       u
      K = ISCRN                                                         u
C
C     SEARCH TRANSLATE TABLE FOR MATCH
      IS=IB(1,K) + IB(7,K) - IB(13,K) - 1 + 2                           u
      DO 30 I=1,IB(14,K)                                                u
      IS=IS + IB(13,K)                                                  u
C------
C     CHECK IF FULL RESPONSE OR ABBREVIATIONS ARE ALLOWED
      LABBR = .TRUE.                                                    u
      IF ( CSCN(IS-1) .EQ. ':' ) LABBR = .FALSE.                        u
      IF ( CSCN(IS-1) .EQ. ';' ) LABBR = .FALSE.                        u
C------ Check if Case sensitive response
      LCASEY = .TRUE.                                                   u
      IF ( CSCN(IS-1) .EQ. ',' ) LCASEY = .FALSE.                       u
      IF ( CSCN(IS-1) .EQ. ';' ) LCASEY = .FALSE.                       u
      CTMP = CLINE                                                      u
      IF ( .NOT. LCASEY ) CALL UPCASE ( CTMP )                          u
C
      DO 25 II=1,NNCHR                                                  u
      IF(CSCN(IS+II-1) .NE. CTMP (II:II)) GO TO 30                      u
C------ Is this a potential match???
   25 CONTINUE                                                          u
C     CHECK FOR ABBREVIATION
      LABB = .TRUE.                                                     u
      IF ( .NOT. LABBR ) THEN                                           u
C     NEXT POSITION MUST BE BLANK TO BE OK
      IF( CSCN(IS+NNCHR) .EQ. ' ' ) GO TO 27                            u
      LABB=.FALSE.                                                      u
      ENDIF                                                             u
C
C     FOUND MATCH
C
   27 continue                                                          u
      NUMATCH=NUMATCH+1                                                 u
      IF(NUMATCH.GT.1) THEN                                             u
        LMATCH = .FALSE.                                                u
        RETURN                                                          u
      ENDIF                                                             u
      CTEMP=' '                                                         u
      DO 28 MM=1,IB(13,K)                                               u
        IF(CSCN(IS+MM-1).EQ.' ') GO TO 30                               u
        CTEMP(MM:MM)=CSCN(IS+MM-1)                                      u
        MTEMP=MM                                                        u
   28 CONTINUE                                                          u
   30 CONTINUE                                                          u
      IF(NUMATCH.EQ.1.AND.LABB) THEN                                    u
        LMATCH=.TRUE.                                                   u
         CLINE=CTEMP                                                    u
         NNCHR=MTEMP                                                    u
      ELSE                                                              u
        LMATCH=.FALSE.                                                  u
      ENDIF                                                             u
      RETURN                                                            u
      END
