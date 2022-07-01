      SUBROUTINE ZCAT (IFLTAB, ICUNIT, ICDUNT, INUNIT, CINSTR,
     *  LABREV, LDOSRT, LCDCAT, NORECS)
C
C     Z - CATALOG,  Generates a listing of record pathnames in a
C     DSS File.  This subroutine will sort the pathnames on Harris
C     Computers (takes significantly longer), or Selectively chose
C     pathnames based on their pathname parts.
C
C     Written by Bill Charley at HEC, 1982.
C
C
      CHARACTER CINSTR*(*)
      INTEGER IMXPRT(6)
      INTEGER IFLTAB(*)
      LOGICAL LABREV, LDOSRT, LSELCA, LCDCAT
C
      INTEGER JORDER(6)
      CHARACTER CINST*392, CSCRAT2*392
      CHARACTER CSORTIN*392, CSORTOUT*392, CSORTMP*392
      CHARACTER CNAME*392, CDNAME*392
      LOGICAL LERR, lsort_tmp, lcdcat_tmp
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssca.h'
C
      INCLUDE 'zdsscc.h'
C
      INCLUDE 'zdsscm.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
C 
C     The following is if another thread interrupts us to stop the 
C     process
      INTERRUPT = 0
C
      CINST = CINSTR
      CALL CHRLNB (CINST, NINST)
      CALL UPCASE (CINST)
C
      IF (MLEVEL.GE.11) THEN
        N = NINST
        IF (N.EQ.0) N = 1
        WRITE (MUNIT,20) ICUNIT, ICDUNT, INUNIT, LABREV, LDOSRT, LCDCAT,
     *    NINST, CINST(1:N)
 20     FORMAT (T6,'-----DSS---Debug:  Enter ZCAT'/,
     *    T11,'ICUNIT:',I4,',  ICDUNT:',I4,',  INUNIT:',I4,/,
     *    T11,'LABREV:',L4,',  LDOSRT:',L4,',  LCDCAT:',L4,/,
     *    T11,'NINST: ',I4,',  Instructions: ',A)
      ENDIF
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZCAT',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C     Find out how many records are in the file
      NUMREC = IFLTAB(KNRECS)
C     If no records in the file, error out.
      IF (NUMREC.EQ.0) THEN
        NORECS = 0
        GO TO 900
      ENDIF
C
      CNAME = ' '
      CDNAME = ' '
      CSINDEX = ' '
      NSINDEX = 0
      LEXTND = .TRUE.
      LSORT = .TRUE.
      IF (.NOT.LDOSRT) LSORT = .FALSE.
      NOPTHS = 0
      JCUNIT = ICUNIT
      JNUNIT = INUNIT
      IF (INUNIT.NE.0) THEN
        REWIND INUNIT
        LEXTND = .FALSE.
        LSORT = .FALSE.
      ENDIF
      LSELCA = .FALSE.
C
C     Determine the length of the maximum pathname and the sort file
      MTOTAL = 0
      DO 40 I=1,6
C     Get the maximum length of each part.  If less than 4, set to 4.
      CALL GETHOL (IFLTAB(KMXPRT), I, IMXPRT(I))
      IF (IMXPRT(I).LT.4) IMXPRT(I) = 4
C     Time series records must have a minimum of 6 for the D and E parts
      IF (((I.EQ.4).OR.(I.EQ.5)).AND.(IMXPRT(I).LT.6)) IMXPRT(I) = 6
      MTOTAL = MTOTAL + IMXPRT(I)
 40   CONTINUE
C
C     Proxy a write lock on the catalog file.
C
C     Since there is no cross-platform way to make an exclusive
C     assignment with Fortran units, use a lockfile instead so we can
C     use Posix or Win32 routines.
C
      if (inunit.eq.0) then
         if (ilkhnd.le.0) then
            inquire(unit=icunit, err=920, name=cname)
            call openf(cname(1:len_trim(cname)-1)//'k',10,ilkhnd,istat)
            if (istat.ne.0) go to 930
            call writf(ilkhnd, ilkhnd, 4, istat, istat)
            call flushf(ilkhnd, istat)
         endif
         call lockfw(ilkhnd, istat)
         if (istat.ne.0)go to 930
      endif
C
C     Determine if a condensed version of the catalog is to be produced
      IF (LSORT.AND.(ICDUNT.GT.0)) THEN
        LCDCAT = .TRUE.
      ELSE
        LCDCAT = .FALSE.
      ENDIF
C
C
C     Initialize Variables
      DO 60 I=1,6
      JORDER(I) = 0
 60   CONTINUE
C     Default Order of pathname parts for sorting
      IORDER(1) = 1
      IORDER(2) = 2
      IORDER(3) = 3
      IORDER(4) = 6
      IORDER(5) = 5
      IORDER(6) = 4
C
C
C     Decipher any Instructions passed in:
C        Is a sort order specified (e.g., O=CB)
C        Is a Selective Catalog Deisired (e.g., C=FLOW)
C
      IF (LSORT) THEN
C
        IF (NINST.GT.0) THEN
C
C         Check for a sort order specified
          IPOS = INDEX (CINST, 'O=')
          IF (IPOS.NE.0) THEN
C           Yes, a sort order was given.  Deactivate the condensed
C           version
            LCDCAT = .FALSE.
C           Blank O= for catalog title
            CINST(IPOS:IPOS+1) = '   '
            IPOS = IPOS + 2
            IF (IPOS+2.LE.NINST) THEN
C             Look for 'OFF' as a sort parameter
              IF (CINST(IPOS:IPOS+2).EQ.'OFF') THEN
                CINST(IPOS:IPOS+2) = '   '
                LSORT = .FALSE.
                GO TO 300
              ENDIF
            ENDIF
C
C           Get the sort order
            DO 100 I=1,6
            M = INDEX ( 'ABCDEF', CINST(IPOS:IPOS))
            IF (M.EQ.0) GO TO 120
            JPOS = I
            JORDER(I) = M
            CINST(IPOS:IPOS) = ' '
            IPOS = IPOS + 1
            IF (IPOS.GT.NINST) GO TO 120
 100        CONTINUE
C
C           Fill in any remainding order not specified
C           First remove from IORDER any parts given by zeroing them out
 120        CONTINUE
            IF (JPOS.GT.0) THEN
              DO 140 I=1,JPOS
              DO 140 J=1,6
              IF (JORDER(I).EQ.IORDER(J)) IORDER(J) = 0
 140          CONTINUE
            ENDIF
C
C           Now fill in any remaining parts
            JPOS = JPOS + 1
            IF (JPOS.LE.6) THEN
              DO 180 I=JPOS,6
              DO 160 J=1,6
C             Has this part been specified yet
              IF (IORDER(J).GT.0) THEN
C               No - Use it then zero it out.
                JORDER(I) = IORDER(J)
                IORDER(J) = 0
                GO TO 180
              ENDIF
 160          CONTINUE
 180          CONTINUE
            ENDIF
C
C           COPY BACK COMPLETED ORDER TO IORDER
            DO 190 I=1,6
            IORDER(I) = JORDER(I)
 190        CONTINUE
C
          ENDIF !(IPOS.NE.0)
        ENDIF !(NINST.GT.0)
      ENDIF !(LSORT)
C
C
C     Look for Selective Catalog Parameters
 300  CONTINUE
      IF (NINST.GT.0) THEN
        CALL ZSETCA (CINST, LSELCA)
        IF (ILWFLG.EQ.-1) GO TO 960
      ENDIF
C
      IF (.NOT.LSORT) LCDCAT = .FALSE.
C
      IF (LCDCAT) THEN
C       Initialize MAXPRT
        DO 320 I=1,6
        MAXPRT(I) = 6
 320    CONTINUE
        MAXPRT(7) = 3
      ENDIF
 
      IF (LABREV .OR. LCDCAT) LEXTND = .FALSE.
C
C     The new sort algorithm ALWAYS produces an unsorted catalog
C     first, because it is much faster.  We have to set these
C     variables before calling zcatfi to get the desired behavior
C
      lsort_tmp = lsort
      lcdcat_tmp = lcdcat
      lsort = .false.
      lcdcat = .false.
C
C     Write the title to the catalog
      CALL ZCATIT (IFLTAB, ICUNIT, LSELCA, .FALSE., CINST, LERR)
      IF (LERR) THEN
         !--------------------------------------------!
         ! kludge for multi-process access to catalog !
         !--------------------------------------------!
         INQUIRE (UNIT=ICUNIT, NAME=CNAME)
         CLOSE (UNIT=ICUNIT)
         OPEN (UNIT=ICUNIT, FILE=CNAME)
         CALL ZCATIT (IFLTAB, ICUNIT, LSELCA, .FALSE., CINST, LERR)
         IF (LERR) GO TO 910
      ENDIF
C
      IF (INUNIT.EQ.0) THEN
C        Now obtain a list of the pathnames from the DSS File
         CALL ZCATFI (IFLTAB, LEXTND, LSELCA, LCDCAT, .FALSE., LERR)
      ELSE
C        Obtain a list of pathnames from the current catalog file
         CALL ZORDPN (LSELCA, LERR)
      ENDIF
      IF (LERR) GO TO 820
C     If another thead set the interrupt flag, return
      IF (INTERRUPT.NE.0) GO TO 845
C
C     now restore the original variables
C
      lsort = lsort_tmp
      lcdcat = lcdcat_tmp
C
      NORECS = NOPTHS
C      
      if (lsort) then
         !--------------------------------------------------!
         ! close catalog files so sort routine cat use them !
         !--------------------------------------------------!
         inquire(unit=icunit, name=cname)
         close(unit=icunit)
         if (lcdcat) then
            inquire(unit=icdunt, name=cdname)
            close(unit=icdunt)
         end if
         !----------------!
         ! do the sorting !
         !----------------!
         call catsort(cname, iorder, lcdcat, interrupt, current_numb,
     *                lcatst, munit)
         !--------------------------!
         ! reopen the catalog files !
         !--------------------------!
         open(unit=icunit, file=cname, err=940)
         if (lcdcat) open(unit=icdunt, file=cdname, err=950)
      end if
      current_numb = -4
      if (interrupt.ne.0) go to 845
C      
 820  CONTINUE
C 
C     revert the write lock to a read lock
      call lockfu(ilkhnd, istat)
      call lockfr(ilkhnd, istat)
      LMAP = .FALSE.
      IF (MLEVEL.GE.11) WRITE (MUNIT,840)
 840  FORMAT (T6,'-----DSS---Debug:  Exit ZCAT')
      RETURN
C
 845  CONTINUE
      IF (MLEVEL.GE.4) WRITE (MUNIT, 850)
 850  FORMAT (' ----- DSS --- ZCAT:  Catalog interrupted.')
      GO TO 820
C
C     Error Conditions
C     No Records in the DSS File
 900  CONTINUE
      WRITE (MUNIT,901)
 901  FORMAT (/' **** ERROR - ZCAT:  No Records in DSS ',
     * 'File (Catalog not Created) ****',/)
      GO TO 820
C
C     Error During Write
 910  CONTINUE
      WRITE (MUNIT,911)
 911  FORMAT(/' **** ERROR - ZCAT:  Error During Write to',
     * ' the Catalog File ***',/,' Unable to Complete Catalog',/)
      GO TO 820
C
C     Catalog File Not Attached
 920  CONTINUE
      WRITE (MUNIT,921)
 921  FORMAT (/' **** ERROR - ZCAT:  Catalog File Not ',
     * 'Attached (New Catalog Not Created)',/)
      GO TO 820
C
C     Error during Exclusive Assign
 930  CONTINUE
      WRITE (MUNIT,931)
 931  FORMAT (/' **** ERROR - ZCAT:  Catalog file Currently in use;',
     * /,' Cannot Create New Catalog at this Time.',/)
      GO TO 820
C
C     Could not reopen the catalog file
 940  CONTINUE
      WRITE (MUNIT,941) CNAME(1:LEN_TRIM(CNAME))
 941  FORMAT (/' **** ERROR - ZCAT:  Cannot Re-open Catalog File',/
     * ' Catalog File: ',A,/)
      GO TO 820
C
C     Could not reopen the catalog file
 950  CONTINUE
      WRITE (MUNIT,951) CDNAME(1:LEN_TRIM(CDNAME))
 951  FORMAT (/' ****CAUTION - ZCAT:  Cannot Re-open Condensed Catalog',
     * /,' Condensed Catalog File: ',A,/)
      GO TO 820
C
C     Error in selective parameter
 960  CONTINUE
      WRITE (MUNIT,961)
 961  FORMAT (' **** ERROR - ZCAT:  Invalid Selective Catalog',
     * ' Parameter ***')
      GO TO 820
C
      END
