C                  KJH  20030327      -- Patched Hyd.K term in LPF option -- cel2wel function
C                  KJH  20030717      -- Patched budget output switch -- subroutine GWF1MNW1bd
c                                        Cleaned output so outrageous pointers are not printed
c                  GZH  20050405      -- Converted calculations to use double precision
c                  KJH  20050419      -- Array WELL2 dimensioned to 18 to store well id
C                  AWH  20080411      -- Retrieve HDRY from GWFBASMODULE rather than from
C                                        LPF, BCF, or HUF
c
      MODULE GWFMNW1MODULE
        DOUBLE PRECISION, PARAMETER :: TWOPI=2.0D0*3.1415926535897932D0
        DOUBLE PRECISION, PARAMETER :: ZERO25=1.0D-25, ZERO20=1.0D-20
        DOUBLE PRECISION, PARAMETER :: ZERO8=1.0D-8, BIG=1.0D30
        CHARACTER(LEN=200),SAVE,POINTER:: MNWNAME
        INTEGER,          SAVE,POINTER :: NWELL2, MXWEL2, IWL2CB, KSPREF
        INTEGER,          SAVE,POINTER :: IWELPT, NOMOITER
        DOUBLE PRECISION, SAVE,POINTER :: PLOSS
        DOUBLE PRECISION, SAVE,POINTER :: SMALL, HMAX
        CHARACTER(LEN=32),SAVE,DIMENSION(:),    POINTER :: MNWSITE
        INTEGER,          SAVE,DIMENSION(:),    POINTER :: IOWELL2
        DOUBLE PRECISION, SAVE,DIMENSION(:,:),  POINTER :: WELL2
        DOUBLE PRECISION, SAVE,DIMENSION(:,:,:),POINTER :: HREF
      TYPE GWFMNWTYPE
        CHARACTER(LEN=200),    POINTER :: MNWNAME
        INTEGER,               POINTER :: NWELL2, MXWEL2, IWL2CB, KSPREF
        INTEGER,               POINTER :: IWELPT, NOMOITER
        DOUBLE PRECISION,      POINTER :: PLOSS
        DOUBLE PRECISION,      POINTER :: SMALL, HMAX
        CHARACTER(LEN=32),     DIMENSION(:),    POINTER :: MNWSITE
        INTEGER,               DIMENSION(:),    POINTER :: IOWELL2
        DOUBLE PRECISION,      DIMENSION(:,:),  POINTER :: WELL2
        DOUBLE PRECISION,      DIMENSION(:,:,:),POINTER :: HREF
      END TYPE
      TYPE(GWFMNWTYPE), SAVE:: GWFMNWDAT(10)
      END MODULE GWFMNW1MODULE
C
c-------------------------------------------------------------------------
c
      SUBROUTINE GWF2MNW17AR(In, Iusip, Iude4, Iusor, Iupcg, Iulmg,
     +                      Iugmg, Fname, Igrid)
C     VERSION 20020819 KJH
c
c----- MNW by K.J. Halford        1/31/98
c     ******************************************************************
c     allocate array storage for well package
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT,NCOL,NROW,NLAY
      USE GWFMNW1MODULE
ctm      USE SIPMODULE,ONLY:HCLOSE
ctm      USE DE4MODULE,ONLY:HCLOSEDE4
ctm      USE PCGMODULE,ONLY:HCLOSEPCG
ctm      USE GMGMODULE,ONLY:HCLOSEGMG
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INTRINSIC ABS
      INTEGER, EXTERNAL :: IFRL
      EXTERNAL NCREAD, UPCASE, QREAD, USTOP
c     ------------------------------------------------------------------
c     Arguments
c     ------------------------------------------------------------------
      INTEGER :: In, Iusip, Iude4, Iusor, Iupcg, Iulmg, Iugmg, Igrid
      CHARACTER(LEN=200) :: Fname                 !!08/19/02KJH-MODIFIED
c     ------------------------------------------------------------------
c     Local Variables
c     ------------------------------------------------------------------
      REAL :: bs
      INTEGER :: ierr, io, iok, jf, ke, kf, ki, kio
      DOUBLE PRECISION :: rn(25)
      CHARACTER(LEN=256) :: txt, tx2
c     ------------------------------------------------------------------
c     Static Variables
c     ------------------------------------------------------------------
      CHARACTER(LEN=6) :: ftag(3)
      INTEGER :: icf(3)
      DATA ftag/'WEL1  ', 'BYNODE', 'QSUM  '/
      DATA icf/4, 6, 4/
c     ------------------------------------------------------------------
      ALLOCATE (MNWNAME, NWELL2, MXWEL2, IWL2CB, NOMOITER, KSPREF,
     +          IWELPT)
      ALLOCATE (PLOSS, SMALL, HMAX, IOWELL2(3),
     +          HREF(NCOL,NROW,NLAY))
c
      IOWELL2(1) = 0
      IOWELL2(2) = 0
      IOWELL2(3) = 0
c
c1------identify package and initialize nwell2
      WRITE (IOUT, 9001) In
 9001 FORMAT (/, ' MNW1 -- MULTI-NODE WELL 1 PACKAGE, VERSION 7,',
     +        ' 11/07/2005.', /, '    INPUT READ FROM UNIT', i4)
      NWELL2 = 0
c
c2------read max number of wells and
c2------unit or flag for cell-by-cell flow terms.
      CALL NCREAD(In, txt, ierr)
      CALL UPCASE(txt)
c
      ki = INDEX(txt, 'REF')
      IF ( ki.GT.0 ) THEN
        tx2 = txt(ki:256)
        CALL QREAD(rn, 1, tx2, ierr)
        IF ( ierr.EQ.0 ) KSPREF = IFRL(rn(1))
        txt(ki:256) = '                                '
      ELSE
        KSPREF = 1
      ENDIF
c
      CALL QREAD(rn, 4, txt, ierr)
      MXWEL2 = IFRL(rn(1))
      IWL2CB = 0
      IF ( ierr.LE.2 ) IWL2CB = IFRL(rn(2))
      IWELPT = 0
      IF ( ierr.EQ.1 ) IWELPT = IFRL(rn(3))
      NOMOITER = 9999
      IF ( ierr.EQ.0 ) NOMOITER = IFRL(rn(4))
c
      WRITE (IOUT, 9002) MXWEL2
      IF ( IWL2CB.GT.0 ) WRITE (IOUT, 9003) IWL2CB
      IF ( IWL2CB.LT.0 ) WRITE (IOUT, 9004)
      WRITE (IOUT, 9005) KSPREF
      WRITE (IOUT, 9006) NOMOITER
 9002 FORMAT (' MAXIMUM OF', i7, ' WELLS')
 9003 FORMAT (' CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT', i3)
 9004 FORMAT (' CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
 9005 FORMAT ('  The heads at the beginning of SP:', i4,
     +        ' will be the default reference elevations.', /)
 9006 FORMAT (' Flow rates will not be estimated after the', i4,
     +        'th iteration')
c
c   Define well model to be used
c
      CALL NCREAD(In, txt, ierr)
      CALL UPCASE(txt)
      PLOSS = 0.0D0   !!  Default use of Skin so linear loss varies with T
      IF ( INDEX(txt, 'LINEAR').GT.0 ) THEN
        PLOSS = 1.0D0 !!  ADD THIS LINE to make sure that the power term is 1 for the linear model
        ki = INDEX(txt, ':') + 1
        tx2 = txt(ki:256)
        CALL QREAD(rn, 1, tx2, ierr)
        IF ( ierr.EQ.0 ) PLOSS = rn(1)
c   Add error checking to shut down MODFLOW
        bs = 3.6           !!   Maximum limit on power term
        IF ( PLOSS.GT.bs ) THEN
          WRITE (*, *) 'Power term of', PLOSS, ' exceeds maximum of', bs
          WRITE (IOUT, *) 'Power term of', PLOSS, ' exceeds maximum of',
     +                    bs
C
C         When compiling MNW with Modflow-96, comment out the call to
C         USTOP and uncomment the STOP statement
          CALL USTOP(' ')
C         STOP
C
        ENDIF
c
      ENDIF
c
c   Test for a specified PREFIX NAME  for time series output from MNW7OT
c
      CALL NCREAD(In, txt, ierr)
      tx2 = txt
      CALL UPCASE(tx2)
      kf = INDEX(tx2, 'PREFIX:')
      IF ( kf.GT.0 ) THEN
        MNWNAME = txt(kf+7:256)
        ke = INDEX(MNWNAME, ' ')
        MNWNAME(ke:200) = '               '
        tx2 = MNWNAME
        CALL UPCASE(tx2)
        IF ( INDEX(tx2, 'FILEPREFIX').GT.0 ) THEN
          MNWNAME = Fname
          ke = INDEX(MNWNAME, '.')
          MNWNAME(ke:200) = '               '
        ENDIF
      ELSE
        MNWNAME = 'OUTput_MNW'
        BACKSPACE (In)
      ENDIF
c
c     Test for creation of a WEL1 package and auxillary output files
c
      iok = 1
      DO WHILE ( iok.EQ.1 )
        CALL NCREAD(In, txt, ierr)
        tx2 = txt
        CALL UPCASE(tx2)
        kf = INDEX(tx2, 'FILE:')
        IF ( kf.GT.0 ) THEN
          kio = 0
          jf = 0
          DO WHILE ( kio.EQ.0 .AND. jf.LT.3 )
            jf = jf + 1
            kio = INDEX(tx2, ftag(jf)(1:icf(jf)))
            IF ( kio.GT.0 ) THEN
              tx2 = txt(kio+1+icf(jf):256)
              CALL QREAD(rn, 1, tx2, ierr)
              IF ( ierr.EQ.0 ) THEN
                IOWELL2(jf) = IFRL(rn(1))
c            OC over ride is ALLTIME
                IF ( INDEX(tx2, 'ALLTIME').GT.0 ) IOWELL2(jf)
     +               = -IOWELL2(jf)
c            Find and use file name
                tx2 = txt(kf+5:256)
                kf = INDEX(tx2, ' ') - 1
                CLOSE (ABS(IOWELL2(jf)))
                OPEN (ABS(IOWELL2(jf)), FILE=tx2(1:kf))
                WRITE (tx2(253:256), '(i4)') ABS(IOWELL2(jf))
                txt = ' A '//ftag(jf)
     +                //' data input file will be written'//' to '//
     +                tx2(1:kf)//' on unit '//tx2(253:256)
                WRITE (IOUT, '(/1x,a79)') txt
                IF ( jf.EQ.1 )
     +          WRITE (ABS(IOWELL2(jf)),'(3i10)') MXWEL2, IWL2CB, IWELPT
              ENDIF
            ENDIF
          ENDDO
        ELSE
          BACKSPACE (In)
          iok = 0
        ENDIF
      ENDDO
c
c  Write header in Auxillary BYNODE file if KPER=1 & IO>0
c
      IF ( IOWELL2(2).NE.0 ) THEN
        io = ABS(IOWELL2(2))
        WRITE (io, 9008)
      ENDIF
c
c  Write header in Auxillary QSUM file if KPER=1 & IO>0
c
      IF ( IOWELL2(3).NE.0 ) THEN
        io = ABS(IOWELL2(3))
        WRITE (io, 9009)
      ENDIF
c
 9008 FORMAT ('SiteID', 27x, 'Entry  NODE', 5x, 'Total_Time', 8x, 'Q',
     +        5x, 'H-Well', 5x, 'H-Cell', 5x, 'QW-Avg')
 9009 FORMAT ('SiteID', 31x, 'Entry', 5x, 'Total_Time', 10x, 'Qin',
     +        10x, 'Qout', 10x, 'Qsum', 5x, 'H-Well', 5x, 'QW-Avg')
c
C  4/18/2005 - KJH:  Explicit well tracking addition changed 1st WELL2
C                    dimension from 17 to 18
      ALLOCATE (WELL2(18, MXWEL2+1), MNWSITE(MXWEL2))
c
C-------SET SMALL DEPENDING ON CLOSURE CRITERIA OF THE SOLVER
ctm      IF ( Iusip.NE.0 ) SMALL = HCLOSE
ctm      IF ( Iude4.NE.0 ) SMALL = HCLOSEDE4
ctm!     IF ( Iusor.NE.0 ) SMALL = HCLOSESOR
ctm      IF ( Iupcg.NE.0 ) SMALL = HCLOSEPCG
ctm      IF ( Iulmg.NE.0 ) SMALL = 0.0D0  !LMG SETS HCLOSE TO ZERO
ctm      IF ( Iugmg.NE.0 ) SMALL = HCLOSEGMG
c
c-----SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2MNW1PSV(Igrid)
c
c7------return
      END SUBROUTINE GWF2MNW17AR
c
C***********************************************************************
      SUBROUTINE GWF2MNW17DA(Igrid)
C     ******************************************************************
C     DEALLOCATE MNW DATA
C     ******************************************************************
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFMNW1MODULE
C     ------------------------------------------------------------------
C Arguments
      INTEGER :: Igrid, IDUM
C     ------------------------------------------------------------------
      IDUM=IGRID
      DEALLOCATE (GWFMNWDAT(Igrid)%NWELL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%MXWEL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%IWL2CB)
      DEALLOCATE (GWFMNWDAT(Igrid)%NOMOITER)
      DEALLOCATE (GWFMNWDAT(Igrid)%KSPREF)
      DEALLOCATE (GWFMNWDAT(Igrid)%IWELPT)
      DEALLOCATE (GWFMNWDAT(Igrid)%PLOSS)
      DEALLOCATE (GWFMNWDAT(Igrid)%SMALL)
      DEALLOCATE (GWFMNWDAT(Igrid)%HMAX)
      DEALLOCATE (GWFMNWDAT(Igrid)%MNWNAME)
      DEALLOCATE (GWFMNWDAT(Igrid)%IOWELL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%HREF)
      DEALLOCATE (GWFMNWDAT(Igrid)%WELL2)
      DEALLOCATE (GWFMNWDAT(Igrid)%MNWSITE)
C
      END SUBROUTINE GWF2MNW17DA
C***********************************************************************
      SUBROUTINE SGWF2MNW1PNT(Igrid)
C     ******************************************************************
C     SET MNW POINTER DATA TO CURRENT GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFMNW1MODULE
C     ------------------------------------------------------------------
C Arguments
      INTEGER :: Igrid
C     ------------------------------------------------------------------
      NWELL2=>GWFMNWDAT(Igrid)%NWELL2
      MXWEL2=>GWFMNWDAT(Igrid)%MXWEL2
      IWL2CB=>GWFMNWDAT(Igrid)%IWL2CB
      NOMOITER=>GWFMNWDAT(Igrid)%NOMOITER
      KSPREF=>GWFMNWDAT(Igrid)%KSPREF
      IWELPT=>GWFMNWDAT(Igrid)%IWELPT
      PLOSS=>GWFMNWDAT(Igrid)%PLOSS
      SMALL=>GWFMNWDAT(Igrid)%SMALL
      HMAX=>GWFMNWDAT(Igrid)%HMAX
      MNWNAME=>GWFMNWDAT(Igrid)%MNWNAME
      IOWELL2=>GWFMNWDAT(Igrid)%IOWELL2
      HREF=>GWFMNWDAT(Igrid)%HREF
      WELL2=>GWFMNWDAT(Igrid)%WELL2
      MNWSITE=>GWFMNWDAT(Igrid)%MNWSITE
C
      END SUBROUTINE SGWF2MNW1PNT
C***********************************************************************
      SUBROUTINE SGWF2MNW1PSV(Igrid)
C     ******************************************************************
C     SAVE MNW POINTER DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFMNW1MODULE
C     ------------------------------------------------------------------
C Arguments
      INTEGER :: Igrid
C     ------------------------------------------------------------------
      GWFMNWDAT(Igrid)%NWELL2=>NWELL2
      GWFMNWDAT(Igrid)%MXWEL2=>MXWEL2
      GWFMNWDAT(Igrid)%IWL2CB=>IWL2CB
      GWFMNWDAT(Igrid)%NOMOITER=>NOMOITER
      GWFMNWDAT(Igrid)%KSPREF=>KSPREF
      GWFMNWDAT(Igrid)%IWELPT=>IWELPT
      GWFMNWDAT(Igrid)%PLOSS=>PLOSS
      GWFMNWDAT(Igrid)%SMALL=>SMALL
      GWFMNWDAT(Igrid)%HMAX=>HMAX
      GWFMNWDAT(Igrid)%MNWNAME=>MNWNAME
      GWFMNWDAT(Igrid)%IOWELL2=>IOWELL2
      GWFMNWDAT(Igrid)%HREF=>HREF
      GWFMNWDAT(Igrid)%WELL2=>WELL2
      GWFMNWDAT(Igrid)%MNWSITE=>MNWSITE
C
      END SUBROUTINE SGWF2MNW1PSV

c
c     ******************************************************************
c     NCREAD: reads lines of input and ignores lines that begin with a "#" sign.
c          All information after a ! is wiped from the input card.
c     ******************************************************************
      SUBROUTINE NCREAD(Io, Txt, Ierr)
      IMPLICIT NONE
      EXTERNAL UPCASE, USTOP
c Arguments
      INTEGER, INTENT(INOUT) :: Io
      INTEGER, INTENT(OUT) :: Ierr
      CHARACTER(LEN=256), INTENT(OUT) :: Txt
c Local Variables
      INTEGER :: ioalt, ioflip, iohold, ki
      CHARACTER(LEN=128) :: afile
      CHARACTER(LEN=256) :: tx2
      DATA ioflip, ioalt/69, 69/
c     ------------------------------------------------------------------
      Ierr = 0
    5 READ (Io, '(a)', END=10) Txt
      IF ( Txt(1:1).EQ.'#' ) GOTO 5
c
      ki = INDEX(Txt, '!')
      IF ( ki.GT.0 )
     +  Txt(ki:256) = '                                                '
c
      tx2 = Txt
      CALL UPCASE(tx2)
c
c    Test for switching control to an auxillary input file
c
      ki = INDEX(Txt, ':')
      IF ( INDEX(tx2, 'REDIRECT').GT.0 .AND. ki.GT.0 ) THEN
        afile = Txt(ki+1:256)
        ki = INDEX(afile, '  ') - 1
        iohold = Io
        Io = ioflip
        ioflip = iohold
        OPEN (Io, FILE=afile(1:ki), STATUS='OLD', ERR=20)
        GOTO 5
      ENDIF
c
c    Test for returning io control from auxillary input to master input file
c
      IF ( INDEX(tx2, 'RETURN').GT.0 .AND.
     +     INDEX(tx2, 'CONTROL').GT.0 ) GOTO 10
c
      ki = INDEX(tx2, '<END>')
      IF ( ki.GT.0 ) THEN
        Ierr = 1
        Txt(ki+5:256) = '                                           '
      ENDIF
c
      IF ( INDEX(tx2, '<STOP>').GT.0 ) Ierr = 2
      RETURN
c
c    Report error in opening auxillary input file and stop
c
   20 WRITE (*, 25) afile
   25 FORMAT (/, '  ERROR opening auxillary input file', //,
     + '   The file:  ', a40, ' does not exist', /)
c
c     When compiling MNW with Modflow-96, comment out the call to
c     USTOP and uncomment the STOP statement
      CALL USTOP(' ')
c      STOP
c
   10 Txt(1:3) = 'EOF'
      IF ( Io.EQ.ioalt ) THEN
        CLOSE (Io)
        iohold = Io
        Io = ioflip
        ioflip = iohold
        GOTO 5
      ELSE
        Ierr = -1
      ENDIF
c
      END SUBROUTINE NCREAD
c
c     ******************************************************************
c     ******************************************************************
      SUBROUTINE QREAD(R, Ni, Ain, Ierr)
      IMPLICIT NONE
      INTRINSIC CHAR, INDEX
      INTEGER, PARAMETER :: MRNV=25
c Arguments
      DOUBLE PRECISION, INTENT(OUT), DIMENSION(MRNV) :: R
      INTEGER, INTENT(IN) :: Ni
      INTEGER, INTENT(OUT) :: Ierr
      CHARACTER(LEN=256), INTENT(IN) :: Ain
c Local Variables
      INTEGER :: i, istat, ki, n, nd
      CHARACTER(LEN=1) :: tab
      CHARACTER(LEN=8) :: rdfmt
      CHARACTER(LEN=256) :: a256
c     ------------------------------------------------------------------
      Ierr = 0
      tab = CHAR(9)           ! sets tab delimiter
c
c   r(ni+1) records the number of non-numeric entries that were attempted to be read as a number
c   r(ni+2) records the last column that was read from the card
c
      R(Ni+1) = -1.0D0
      a256 = Ain
      DO i = 1, 256
        IF ( a256(i:i).EQ.tab ) a256(i:i) = ' '
        IF ( a256(i:i).EQ.',' ) a256(i:i) = ' '
        IF ( a256(i:i).EQ.':' ) a256(i:i) = ' '
        IF ( a256(i:i).EQ.'=' ) a256(i:i) = ' '
      ENDDO
      n = 1
      i = 0
   11 R(Ni+1) = R(Ni+1) + 1.0D0
   10 i = i + 1
      IF ( i.GE.256 ) GOTO 15
      IF ( a256(i:i).EQ.' ' ) THEN
        a256(i:i) = '?'
        GOTO 10
      ENDIF
c
      ki = INDEX(a256, ' ') - 1
      nd = ki - i + 1
      rdfmt = '(F??.0) '
      WRITE (rdfmt(3:4), '(i2.2)') nd
CERB  Fix for bug that caused i to be incremented by only 1 position
CERB  each time the read statement returns an error.  This bug also
CERB  incremented r(ni+1) unnecessarily.  With Lahey-compiled code, the
CERB  buggy version would read a final E in a word (without returning an
CERB  error) as a zero.
CERB      read (a256(i:ki),rdfmt,err=11,end=10) r(n)
      READ (a256(i:ki), rdfmt, ERR=13, IOSTAT=istat) R(n)
   13 CONTINUE
      i = ki
      IF ( istat.GT.0 ) GOTO 11 ! PART OF BUG FIX -- ERB
      n = n + 1
      IF ( n.LE.Ni .AND. i.LT.256 ) GOTO 10
c
   15 n = n - 1
      Ierr = Ni - n
      R(Ni+2) = i
c
      END SUBROUTINE QREAD

c     ******************************************************************
      INTEGER FUNCTION IFRL(R)
      IMPLICIT NONE
      INTRINSIC ABS
c Arguments
      DOUBLE PRECISION, INTENT(IN) :: R
c Local Variables
      INTEGER :: ip
c     ------------------------------------------------------------------
      ip = ABS(R) + 0.5D0
      IF ( R.LT.0.0D0 ) ip = -ip
      IFRL = ip
      END FUNCTION IFRL
