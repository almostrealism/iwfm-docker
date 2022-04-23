      SUBROUTINE DKBFOP(IH,CNAME,IB,NW,ISTAT)
C
C     OPEN FOR BUFFERED READ OR WRITE
C
C     IH- FILE HANDLE and input flag
C         if IH = -1, file is opened as read only, else read-write
C     CNAME - FILE NAME TO OPEN
C     IB- BUFFER USED TO READ
C        MUST BE (# OF BYTES/2) + 10   IN LENGTH(16 BIT WORDS)
C        MUST BE PRE SET BY CALL TO PCOPEN
C        DO NOT CHANGE AFTER FIRST USE !!!!!!
C     NW - BUFFER SIZE IN WORDS
C     ISTAT- RETURN STATUS  0= OK
C                          .NE.0 = ERROR
C
C
C      IB(1)- UNIQUE FLAG SET AFTER FIRST USE
C      IB(2)- START WORD OF TRANSFER AREA
C      IB(3)- TOTAL WORDS IN BUFFER
C      IB(4)- OUT BYTE POINTER
C      IB(5)- IN BYTE POINTER (NEXT FREE BYTE)
C      IB(6)- ACTUAL FILE BYTE LOC OF CURRENT BUFFER
C      IB(7)- ACTUAL FILE BYTE LOC OF CURRENT BUFFER
C      IB(8)- IH FILE HANDLE
C      IB(9)- LENGTH OF TRANSFER AREA IN BYTES
C      IB(10)- STARTING BYTE LOCATION OF TRANSFER AREA
C
C
C
      INTEGER IB(NW)
      CHARACTER CNAME*(*), CZERO*1  ,CBUFF*80
C
      COMMON /WORDS/ IWORD(10)
C
C
      DATA JUNIQ/3566/
C
C     OPEN FILE
      CALL CHRLNB (CNAME, IL)
      J=MIN0(IL,80)
      CBUFF(1:J)=CNAME(1:J)
      CZERO=CHAR(0)
      IF (IH.EQ.-1) THEN
         CALL OPENF(CBUFF(1:J)//CZERO,0,IH,ISTAT)
      ELSE
         CALL OPENF(CBUFF(1:J)//CZERO,2,IH,ISTAT)
      ENDIF
      IF(ISTAT.NE.0) RETURN
      IB(1) = JUNIQ
      IB(2)=10 + 1
      IB(3)= NW
      IB(6)= 1
      IB(7)= 0
      IB(8)= IH
      IB(9)= (IB(3)-IB(2) +1) * IWORD(7)
      IB(10)= (IB(2)-1)*IWORD(7)+1
      IB(4)= IB(10)
      IB(5)= IB(10)
C
      ISTAT=0
      RETURN
      END
