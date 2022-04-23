      SUBROUTINE SQUEEZEDSS (CNAME, ISTAT)
C
C     Squeeze a DSS file.
C     (Removes all Dead space, and can recover deleted
C     records or fix a damaged file.)
C
      CHARACTER CNAME*(*)
      CHARACTER CNAME2*256
      CHARACTER CSCRAT*25
      LOGICAL LRETAG
C
      INTEGER IFTAB1(500)
      INTEGER IFTAB2(500)
C
      PARAMETER (KBUFF1=10000, KBUFF2=2000)
      INTEGER IBUFF1(KBUFF1), IBUFF2(KBUFF2)
C
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB, CURRENT_NUMB, INTERRUPT
      INTEGER NERROR, MAXERROR
C
      LRETAG = .FALSE.
      ISTAT = 0
      INTERRUPT = 0
C
C
      TOTAL_NUMB = 0
      CURRENT_NUMB = 0
C
      CALL ZINQIR(IFTAB1, 'MLVL', CSCRAT, MLVL)
      CALL ZINQIR(IFTAB1, 'MUNIT', CSCRAT, MUNIT)
C
C     Rename the file to make sure that we have access to it
C     First, close it
      CALL ZSET ('MLEVEL', ' ', 0)
C
      CNAME2 = CNAME
      CALL CHRLNB (CNAME2, ILAST)
      CNAME2(ILAST:ILAST) = '-'
C     Do the rename.  The new file name is the same
C     file name with the last character replaced with a '$'
      CALL CRENAM (CNAME, CNAME2, IERR)
C     Is the file typed for delete access?
C     An old DSS file there?
C
C     Is someone using the file?  (If so, wait for a time in batch mode)
C
      IF (IERR.NE.0) GO TO 910
C
C     The rename was successful.  Rename it back.
 60   CONTINUE
      CALL CRENAM (CNAME2, CNAME, IERR)
      IF (INTERRUPT.NE.0) GO TO 800
C
C     Reopen the file in exclusive mode
      CALL ZSET ('UNIT', ' ', 71)
      CALL ZOPEN (IFTAB1, CNAME, IERR)
      IF (IERR.NE.0) GO TO 920
C
C     Get the current size of the DSS file
      CALL ZINQIR (IFTAB1, 'SIZE', CSCRAT, ISIZE)
      CALL ZINQIR (IFTAB1, 'DEAD', CSCRAT, IDEAD)
      RDEAD = REAL(IDEAD) / 100.
      SIZE = REAL(ISIZE) * (1.0 - RDEAD)
      JSIZE = INT(SIZE)
C
C
      CALL ZINQIR (IFTAB1, 'NRECS', CSCRAT, NRECS)
C     Set future size optimization here ************************
C     (Check how old the file is etc.)
      J = REAL(NRECS) * 1.1
      IF (J.LT.55) J = 55
      CALL ZSET ('SIZE', ' ', J)
C
C
      CALL ZINQIR (IFTAB1, 'TABLE', CSCRAT, ILARGE)
      I = ILARGE
      CALL ZSET ('TABLE', CSCRAT, I)
C     See if the file size or table type are specified by the user
C     CALL NEWFS (CLINE, NLINE, CSCRAT, .FALSE.)
      CALL ZSET ('WLOCK', 'ON', I)
C
C     Open the new file with write lock on
      CALL ZSET ('UNIT', ' ', 72)
      CALL ZOPEN (IFTAB2, CNAME2, IERR)
      IF (IERR.NE.0) THEN
      CALL ZCLOSE (IFTAB1)
      GO TO 920
      ENDIF
C
C     Copy Permanent File Information
C     IF (LCOPT('T')) LRETAG = .TRUE.
C     CALL ZSQPRM (IFTAB1, IFTAB2, LRETAG)
C
C     We are now ready to squeeze the file.
C     Let the user know this may take some time.
C      CALL PMODE (IMODE)
C     IF (IMODE.LE.2) CALL ZSET ('SQST', 'ON', I)
C     IF (IMODE.NE.3) WRITE (MUNIT, 70)
C70   FORMAT (' Please Wait.')
C
C
C     Should we compress (new) Time-series data
C     IF (LCOPT('C')) CALL ZSET ('COMP', 'ON', I)
C
C     Squeeze the file.
      IF (INTERRUPT.NE.0) GO TO 840
C
      CALL ZCOFIL (IFTAB1, IFTAB2, IBUFF1, KBUFF1, IBUFF2, KBUFF2,
     * .FALSE., LRETAG)
C
      IF (INTERRUPT.NE.0) GO TO 840
C
C     Now close files and rename back
      CALL ZCLOSE (IFTAB1)
      CALL ZCLOSE (IFTAB2)
C
C
C     Get the permissions on the old file
      CALL PERMISSIONS (CNAME, IPERM, IST)
C
      CALL CDELET (CNAME, IERR)
      IF (IERR.NE.0) GO TO 930
C
      CALL CRENAM (CNAME2, CNAME, IERR)
      IF (IERR.NE.0) GO TO 930
C
C     Set the permissions on the new file
      IF (IST.EQ.0) CALL CHMODF (CNAME, IPERM, IST)
C
C
 800  CONTINUE
      CALL ZSET ('MLEVEL', ' ', MLVL)
      RETURN
C
 840  CONTINUE
C     Interrupt squeezed, files opened
      CALL ZCLOSE (IFTAB1)
      CALL ZCLOSE (IFTAB2)
      CALL CDELET (CNAME2, IERR)
      GO TO 800
C
C
 910  CONTINUE
      ISTAT = -1
      CALL CHRLNB(CNAME, N)
      CALL CHRLNB(CNAME2, N2)
      WRITE (MUNIT, 913) CNAME(1:N), CNAME2(1:N2), IERR
 913  FORMAT (' *** ERROR:  Unable to rename DSS file to tempoary name.'
     * /,' Current Name: ',A,/,' Tempoary Name: ',A,/,' Error:',I5)
      GO TO 800
C
 920  CONTINUE
      ISTAT = -2
      CALL CHRLNB(CNAME, N)
      WRITE (MUNIT, 921) CNAME(1:N), IERR
 921  FORMAT (' *** ERROR:  Unable to attach to DSS file ',A,/
     * ' in an exclusive mode.  Error: ',I5)
      GO TO 800
C
 930  CONTINUE
      ISTAT = -3
      CALL CHRLNB(CNAME, N)
      CALL CHRLNB(CNAME2, N2)
      WRITE (MUNIT, 931) CNAME(1:N), CNAME2(1:N2), IERR
 931  FORMAT (' *** ERROR:  Unable to eliminate old DSS file ',A,/
     * ' Current Squeezed DSS file is named ',A,/,' Error: ',I5)
      GO TO 800
C
C
      END
 
 
