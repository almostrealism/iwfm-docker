C -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C
C
      INTEGER    IMXMCP
      INTEGER    IFUNLN
      PARAMETER (IMXMCP = 600, IFUNLN=80, MAXCHR=200, MAXFUN=50)        HLMu
C
      CHARACTER*133 C133
      CHARACTER*133  CLBUFM(IMXMCP+2)
      CHARACTER*(MAXCHR)  CLINE, CKBLIN, CLINSV
      CHARACTER CFUNCT(MAXFUN)*(IFUNLN), CKEY(MAXFUN)
      CHARACTER*4   CSPL
      CHARACTER*60  CPROMT
C
      COMMON /PCHAR/ CLBUFM, CKEY,   CFUNCT, CLINE, CSPL, C133,
     +               CPROMT, CLINSV, CKBLIN
C
C
C -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
