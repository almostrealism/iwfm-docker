      MODULE GWTBCTMODULE
       INTEGER,SAVE,POINTER::IBCTCB,MCOMP,ITVD,IADSORB,ICT,NTITER,IDISP,
     *   IXDISP,IZOD,IFOD
       REAL, SAVE, POINTER    ::CINACT
       DOUBLE PRECISION, SAVE, POINTER ::CICLOSE
       REAL, SAVE, POINTER ::DIFFNC
       INTEGER, SAVE, DIMENSION(:,:), ALLOCATABLE ::  LrcC
       INTEGER,    SAVE, DIMENSION(:), ALLOCATABLE     ::IPCBFLAG
       DOUBLE PRECISION, SAVE, DIMENSION(:), ALLOCATABLE :: Cncg
       INTEGER, SAVE,  ALLOCATABLE,   DIMENSION(:)     ::ICBUND
       REAL, SAVE,     ALLOCATABLE,   DIMENSION(:) ::PRSITY
       DOUBLE PRECISION, SAVE,     ALLOCATABLE,   DIMENSION(:) ::MASSBCT
       REAL, SAVE, ALLOCATABLE,   DIMENSION(:,:) ::ADSORB,FLICH,
     1  ZODRW,FODRW,ZODRS,FODRS
       REAL, SAVE, ALLOCATABLE,   DIMENSION(:,:) ::VELNOD
       REAL, SAVE, ALLOCATABLE,  DIMENSION(:) :: DLX,DLY,DLZ,
     1   ATXY,ATYZ,ATXZ
       REAL, SAVE, ALLOCATABLE,  DIMENSION(:) :: ALXY,ALYZ,ALXZ
       DOUBLE PRECISION, SAVE,  ALLOCATABLE,DIMENSION(:,:) ::CONC,CONCO
       REAL*8, SAVE,     ALLOCATABLE,   DIMENSION(:) ::CBCF
       REAL, SAVE,     ALLOCATABLE,   DIMENSION(:) ::CBCH
       DOUBLE PRECISION,    SAVE, DIMENSION(:,:,:), ALLOCATABLE ::VBVLT
       CHARACTER(LEN=16), SAVE, DIMENSION(:,:),   ALLOCATABLE ::VBNMT
       INTEGER, SAVE, POINTER  ::MSUMT
       INTEGER, SAVE, DIMENSION(:), ALLOCATABLE ::  FLrcC
       DOUBLE PRECISION, SAVE, DIMENSION(:), ALLOCATABLE :: FCncg
       DOUBLE PRECISION, SAVE, DIMENSION(:), ALLOCATABLE :: ADMAT
       REAL, SAVE,     ALLOCATABLE,   DIMENSION(:) ::DXCS,DYCS,DZCS
      END MODULE GWTBCTMODULE
c -------------------------------------------------------------------------------------
      SUBROUTINE GWT2BCT1AR(IN)
C     ******************************************************************
C     ALLOCATE ARRAYS AND READ DATA FOR BLOCK-CENTERED TRANSPORT PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,IFREFM,IUNSTR,
     * NODES,NEQS,NJA,NJAS,ITRNSP,Sn,NIUNIT,INCLN,
     1   IFMBC,MBEGWUNF,MBEGWUNT,MBECLNUNF,MBECLNUNT       
      USE GWFBCFMODULE, ONLY: LAYCON, SC2
      USE GWTBCTMODULE
C
      CHARACTER*200 LINE
      INTEGER IC_IBOUND_FLG
C
C     ------------------------------------------------------------------
C1------ALLOCATE SCALAR VARIABLES IN FORTRAN MODULE.
      ALLOCATE(IBCTCB,MCOMP,ITVD,IADSORB,ICT,NTITER,IDISP,IXDISP,
     1  IZOD,IFOD)
      ALLOCATE(CINACT)
      ALLOCATE(CICLOSE)
      ALLOCATE(DIFFNC)
C
C2------IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'BCT -- BLOCK-CENTERED TRANSPORT PACKAGE, VER. 8',
     1', 9/12/2009',/,9X,'INPUT READ FROM UNIT',I3)
C
C3------READ AND PRINT IBCTCB (FLAG FOR PRINTING
C3------OR UNIT# FOR RECORDING CELL-BY-CELL FLUX TERMS), MCOMP
C3------(NUMBER OF MOBIME COMPONENTS), AND CINACT (C AT INACTIVE CELLS)
      IF(IFREFM.EQ.0) THEN
          READ(IN,2)ITRNSP,IBCTCB,MCOMP,IC_IBOUND_FLG,ITVD,IADSORB,
     *     ICT,CINACT,CICLOSE,IDISP,IXDISP,DIFFNC,IZOD,IFOD,IFMBC
      ELSE
        CALL URDCOM(IN,IOUT,LINE)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITRNSP,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBCTCB,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MCOMP,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IC_IBOUND_FLG,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITVD,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IADSORB,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICT,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CINACT,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,CICLOSES,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDISP,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IXDISP,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,DIFFNC,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IZOD,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFOD,R,IOUT,INDIS)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFMBC,R,IOUT,INDIS)        
        CICLOSE = CICLOSES
      ENDIF
2     FORMAT(7I10,2F10.3,2I10,F10.3,3I10)
      NTITER = 1
      IF(ITVD.NE.0)NTITER = ABS(ITVD)
      IF(CICLOSE.LT.1.0E-20) CICLOSE = 1.0E-6
C
C3B-----PRINT VALUES
      WRITE(IOUT,11) ITRNSP,IBCTCB,MCOMP,IC_IBOUND_FLG,ITVD,IADSORB,ICT,
     *  CINACT,CICLOSE,IDISP,IXDISP,DIFFNC,IZOD,IFOD,IFMBC
   11 FORMAT(1X,'TRANSPORT FLAG (ITRNSP)                             =',
     *  I3/1X,'C-B-C FLUX FLAG OR UNIT NUMBER (IBCTCB)             =',I3
     *  /1X,'NUMBER OF MOBILE COMPONENTS (MCOMP)                 =',I3
     *  /1X,'ACTIVE DOMAIN SAME AS FOR FLOW FLAG (IC_IBOUND_FLG) =',I3
     *  /1X,'TVD SCHEME FLAG AND FCT COUNT (ITVD)                =',I3
     *  /1X,'ADSORPTION FLAG (IADSORB)                           =',I3
     *  /1X,'FLAG TO SOLVE FOR TOTAL CONCENTRATION (ICT)         =',I3
     */1X,'CONCENTRATION AT INACTIVE CELLS (CINACT)            =',G13.5,
     */1X,'SOLVER TOLERANCE FOR TRANSPORT (CICLOSE)            =',G13.5,
     *  /1X,'DISPERSION FLAG (IDISP)                             =',I3,
     *  /1X,'CROSS-DISPERSION FLAG (IXDISP)                      =',I3,
     */1X,'DIFFUSION COEFFICIENT (DIFFNC)                      =',G13.5,
     *  /1X,'ZERO-ORDER DECAY FLAG (IZOD)                        =',I3,
     *  /1X,'FIRST-ORDER DECAY FLAG (IFOD)                       =',I3,
     *  /1X,'FLOW ERROR CORRECTION FLAG OR UNIT NUMBER (IFMBC)   =',I3)
      IF(IBCTCB.GT.0) WRITE(IOUT,9) IBCTCB
    9 FORMAT(1X,'CELL-BY-CELL FLUXES WILL BE SAVED ON UNIT (IBCTCB)  =',
     *  I3)
      IF(ITRNSP.EQ.1)THEN
        WRITE(IOUT,*)' TRANSPORT SIMULATION FOLLOWS FLOW (ITRNSP)',
     *    '         =  1'
      ELSEIF(ITRNSP.EQ.2)THEN
        WRITE(IOUT,*)' RUN TRANSPORT ON PREVIOUSLY RUN FLOW-FIELD'
        WRITE(IOUT,*)'OPTION CURRENTLY NOT AVAILABLE'
        STOP
      ENDIF
C
C4-----ALLOCATE SPACE FOR ARRAYS.
      ALLOCATE(ICBUND(NEQS))
      ALLOCATE(PRSITY(NODES),CONC(NEQS,MCOMP),CONCO(NEQS,MCOMP),
     *  CBCF(NJAS),CBCH(NEQS),MASSBCT(MCOMP),ADMAT(NJA))
      CONC = 0.
      ALLOCATE (IPCBFLAG(NEQS))
      IF(IADSORB.NE.0) ALLOCATE (ADSORB(NODES,MCOMP))
      IF(IADSORB.EQ.2) ALLOCATE (FLICH(NODES,MCOMP))
      ALLOCATE (Cncg(NTITER),LrcC(3,NTITER))
C      IF(INCLN.GT.0)
      ALLOCATE (FCncg(NTITER),FLrcC(NTITER))
      IF(IDISP.GT.0)THEN
        ALLOCATE(VELNOD(NEQS,3))
        ALLOCATE(DLX(NEQS), DLY(NODES), DLZ(NODES))
        ALLOCATE(ATXY(NEQS), ATYZ(NODES), ATXZ(NODES))
        IF(IXDISP.NE.0)THEN
          ALLOCATE(ALXY(NEQS), ALYZ(NODES), ALXZ(NODES))
        ENDIF    
      ENDIF
      IF(IZOD.EQ.1)THEN
        ALLOCATE(ZODRW(NEQS,MCOMP))
        ZODRW = 0.
      ELSEIF(IADSORB.GT.0)THEN
        IF(IZOD.EQ.2)THEN
           ALLOCATE(ZODRS(NEQS,MCOMP))
           ZODRS = 0.
        ELSEIF(IZOD.EQ.3)THEN
          ALLOCATE(ZODRW(NEQS,MCOMP))
          ZODRW = 0.
          ALLOCATE(ZODRS(NEQS,MCOMP))
          ZODRS = 0.
        ENDIF
      ENDIF
      IF(IFOD.EQ.1)THEN
        ALLOCATE(FODRW(NEQS,MCOMP))
        FODRW = 0.
      ELSEIF(IADSORB.GT.0)THEN
        IF(IFOD.EQ.2)THEN
           ALLOCATE(FODRS(NEQS,MCOMP))
           FODRS = 0.
        ELSEIF(IFOD.EQ.3)THEN
          ALLOCATE(FODRW(NEQS,MCOMP))
          FODRW = 0.
          ALLOCATE(FODRS(NEQS,MCOMP))
          FODRS = 0.
        ENDIF
      ENDIF
      IF(IFMBC.NE.0)THEN
        ALLOCATE(MBEGWUNF,MBEGWUNT,MBECLNUNF,MBECLNUNT)  
        MBEGWUNF = 0
        MBEGWUNT = 0
        MBECLNUNF = 0
        MBECLNUNT = 0
C-------...AND READ UNIT NUMBERS FOR FMBE OUTPUT FILES        
        IF(IFREFM.EQ.0) THEN
          READ(IN,2)MBEGWUNF,MBEGWUNT,MBECLNUNF,MBECLNUNT
        ELSE
          CALL URDCOM(IN,IOUT,LINE)
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MBEGWUNF,R,IOUT,INDIS)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MBEGWUNT,R,IOUT,INDIS)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MBECLNUNF,R,IOUT,INDIS)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MBECLNUNT,R,IOUT,INDIS)
        ENDIF   
        WRITE(IOUT,12) MBEGWUNF,MBEGWUNT,MBECLNUNF,MBECLNUNT 
12    FORMAT(1X,'UNIT NUMBER FOR OUTPUT OF FLOW IMBALANCE (MBEGWUNF) =',
     * /1X,'UNIT NUMBER FOR OUTPUT OF TRANSPORT IMBAL(MBEGWUNT) =',I3 
     *  I3/1X,'UNNIT NUMBER FOR OUTPUT OF CLN FLOW IMBAL(MBECLNUNF)=',I3
     *  /1X,'UNIT NUMBER FOR OUTPUT OF TRANSP IMBAL  (MBECLNUNT) =',I3)
      ENDIF   
C4A-----ALLOCATE SPACE FOR VBVLT, AND VBNMT ARRAYS.
      ALLOCATE (VBVLT(4,NIUNIT,MCOMP))
      ALLOCATE (VBNMT(NIUNIT,MCOMP))
      ALLOCATE (MSUMT)
C4B------INITIALIZE ALLOCATED ARRAYS
      ADMAT = 0.0
      CBCF = 0.
      CBCH = 0.
      LrcC = 0
      Cncg = 0.0D0
      FLrcC = 0
      FCncg = 0.0D0
      IPCBFLAG = 0
      DO 600 ICOMP = 1,MCOMP
      DO 600 I=1,NIUNIT
      DO 600 J=1,4
      VBVLT(J,I,ICOMP)=0.0
  600 CONTINUE
C
C5----READ PARAMETERS AND CONVERT FOR UNSTRUCTURED AND STRUCTURED GRIDS
      IF(IUNSTR.EQ.0) THEN
        CALL SGWT2BCT1S(IN,IC_IBOUND_FLG)
      ELSE
        CALL SGWT2BCT1G(IN,IC_IBOUND_FLG)
      ENDIF
C
C6----READ PARAMETERS FOR CONDUIT DOMAIN NODES
      IF(INCLN.GT.0) THEN
        CALL SGWT2BCT1CLN(INCLN,IC_IBOUND_FLG)
      ENDIF
C---------------------------------------------------------------------------
C7-----PREPARE PARAMETERS
C---------------------------------------------------------------------------
C---------------------------------------------------------------------------
C7A----SET CONCO TO CONC
      DO ICOMP = 1,MCOMP
      DO N = 1,NEQS
        CONCO(N,ICOMP) = CONC(N,ICOMP)
      ENDDO
      ENDDO      
C7A----FOR ZERO ORDER DECAY IN WATER
      IF(IZOD.EQ.1.OR.IZOD.EQ.3)THEN
        DO ICOMP = 1,MCOMP
        DO N = 1,NODES
          ZODRW(N,ICOMP) = ZODRW(N,ICOMP) * PRSITY(N)
        ENDDO
        ENDDO
      ENDIF
C7B----FOR ZERO ORDER DECAY IN SOIL
      IF((IZOD.EQ.2.OR.IZOD.EQ.3).AND.IADSORB.NE.0)THEN
        DO ICOMP = 1,MCOMP
        DO N = 1,NODES
          ZODRS(N,ICOMP) = ZODRS(N,ICOMP) * (1.0 - PRSITY(N))
        ENDDO
        ENDDO
      ENDIF
C7C----FOR FIRST ORDER DECAY IN WATER
      IF(IFOD.EQ.1.OR.IFOD.EQ.3)THEN
        DO ICOMP = 1,MCOMP
        DO N = 1,NODES
          FODRW(N,ICOMP) = FODRW(N,ICOMP) * PRSITY(N)
        ENDDO
        ENDDO
      ENDIF
C7D-----FOR CROSS DISPERSION LONGITUDINAL DISPERSIVITIES
      IF(IDISP.GT.0.AND.IXDISP.NE.0)THEN
        DO N = 1,NODES 
          ALXY(N) = 2.0 * DLX(N) * DLY(N)/( DLX(N) + DLY(N))
          ALYZ(N) = 2.0 * DLY(N) * DLZ(N)/( DLY(N) + DLZ(N))
          ALXZ(N) = 2.0 * DLX(N) * DLZ(N)/( DLX(N) + DLZ(N))
        ENDDO
      ENDIF    
C7E-----SET SECONDARY STORAGE TO POROSITY IF IT IS NOT READ      
      ILAYCON13=0
      DO I=1,NLAY
       IF(LAYCON(I).EQ.1.OR.LAYCON(I).EQ.3.OR.LAYCON(I).EQ.4)ILAYCON13=1
      ENDDO      
      IF(ILAYCON13.EQ.0)THEN
        DO N = 1,NODES
          SC2(N) = PRSITY(N)
        ENDDO
      ENDIF  
C
C--------------------------------------------------------------------------------
C8----RETURN
      RETURN
      END
C-------------------------------------------------------------------------
      SUBROUTINE INITMASS
C     ******************************************************************
C     COMPUTE THE INITIAL MASS OF MOBILE COMPONENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IOUT,ISSFLG,NODES,NEQS,So,TOP,BOT,AREA,IDPT,HOLD,
     *            Sn 
      USE GWTBCTMODULE
      USE GWFDPFMODULE, ONLY: PHIF
      USE GWFBCFMODULE, ONLY: SC1,SC2
      DOUBLE PRECISION MASLOCW,MASLOCS,MASLOCC,REFHD
C     ------------------------------------------------------------------
C1----COMPUTE INITIAL MASS FOR MOBILE COMPONENTS
      WRITE(IOUT,12)
12    FORMAT(/5X,'INITIAL MASS OF COMPONENTS IN MOBILE DOMAIN'/
     *       5X,'--------------------------------------------'/
     *       5X,'COMPONENT INDEX',5X,'INITIAL MASS'/
     *       5X,'---------------',5X,'------------')
C1-----INITIALIZE MASS FOR EACH COMPONENT
      DO ICOMP = 1,MCOMP
        MASSBCT (ICOMP) = 0.0
      ENDDO
      MASLOCW = 0.0
      MASLOCS = 0.0
      MASLOCC = 0.0
      ISS=ISSFLG(1)
C2-----COMPUTE MASS FOR EACH ACTIVE NODE
      DO N = 1,NODES
        IF(ICBUND(N).EQ.0) THEN
            DO ICOMP = 1,MCOMP
              CONC(N,ICOMP) = CINACT
            ENDDO
        ELSE
C3---------COMPUTE VOLUME OF NODE
          ALENG = TOP(N) - BOT(N)
          VOLU = AREA(N) * ALENG
C4---------FOR EACH COMPONENT
          DO ICOMP = 1,MCOMP
C5---------COMPUTE MASS IN WATER
            IF(ISS.EQ.1)THEN
              MASLOCW = PRSITY(N) * Sn(N) * CONC(N,ICOMP) * VOLU
            ELSE
C6-----------INITIAL MASS IN PORE STORAGE
              MASLOCW=(PRSITY(N)*VOLU-SC2(N)*ALENG+SC2(N)*ALENG*So(N))
     *         *CONC(N,ICOMP)
C7-------------INITIAL MASS IN COMPRESSIBLE STORAGE ASSUMED FROM TOP OF CELL
              REFHD = HOLD(N) - TOP(N)
              IF(REFHD.LT.0.0) REFHD = 0.0
              MASLOCC = SC1(N) * REFHD * CONC(N,ICOMP)
            ENDIF
C8-----------COMPUTE MASS ON SOIL
            IF(IADSORB.NE.0)THEN
              ETA = 1.0
              IF(IADSORB.EQ.2) ETA = FLICH(N,ICOMP)
              MASLOCS =  ADSORB(N,ICOMP)*CONC(N,ICOMP)**ETA * VOLU
            ENDIF
            MASSBCT(ICOMP) = MASSBCT(ICOMP)+MASLOCW +MASLOCS+MASLOCC
          ENDDO
        ENDIF
      ENDDO
C9-----WRITE MASS TO OUTPUT FILE
      DO ICOMP = 1,MCOMP
        WRITE(IOUT,13) ICOMP, MASSBCT(ICOMP)
      ENDDO
13    FORMAT(I15,5X,E15.6)
C
C--------------------------------------------------------------------------------
C10----RETURN
      RETURN
      END
C
C-------------------------------------------------------------------------
      SUBROUTINE INITMASSCLN
C     ******************************************************************
C     COMPUTE THE INITIAL MASS OF MOBILE COMPONENTS IN THE CLN DOMAIN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IOUT,ISSFLG,NODES,NEQS,Sn,So,AREA,HOLD
      USE GWTBCTMODULE
      USE CLN1MODULE, ONLY: NCLNNDS,ACLNNDS
      DOUBLE PRECISION MASLOCW,SAT
C     ------------------------------------------------------------------
C1----COMPUTE INITIAL MASS FOR MOBILE COMPONENTS
      WRITE(IOUT,12)
12    FORMAT(/5X,'INITIAL MASS OF COMPONENTS IN CLN DOMAIN'/
     *       5X,'-----------------------------------------'/
     *       5X,'COMPONENT INDEX',5X,'INITIAL MASS'/
     *       5X,'---------------',5X,'------------')
C1-----INITIALIZE MASS FOR EACH COMPONENT
      DO ICOMP = 1,MCOMP
        MASSBCT (ICOMP) = 0.0
      ENDDO
      MASLOCW = 0.0
      MASLOCS = 0.0
      MASLOCC = 0.0
      ISS=ISSFLG(1)
C2-----COMPUTE MASS FOR EACH ACTIVE CLN NODE
      DO N = NODES+1, NEQS
        IF(ICBUND(N).EQ.0) THEN
            DO ICOMP = 1,MCOMP
              CONC(N,ICOMP) = CINACT
            ENDDO
        ELSE
C3---------COMPUTE VOLUME OF NODE
          ALENG = ACLNNDS(N-NODES,4)
          VOLU = AREA(N) * ALENG
          SAT = So(N)
          IF(ISS.EQ.1) SAT = Sn(N)
C4---------FOR EACH COMPONENT
          DO ICOMP = 1,MCOMP
C5---------COMPUTE MASS IN WATER (NO COMPRESSIBLE STORAGE IN CLN
            MASLOCW = SAT * CONC(N,ICOMP) * VOLU
            MASSBCT(ICOMP) = MASSBCT(ICOMP)+MASLOCW
          ENDDO
        ENDIF
      ENDDO
C9-----WRITE MASS TO OUTPUT FILE
      DO ICOMP = 1,MCOMP
        WRITE(IOUT,13) ICOMP, MASSBCT(ICOMP)
      ENDDO
13    FORMAT(I15,5X,E15.6)
C
C--------------------------------------------------------------------------------
C10----RETURN
      RETURN
      END
C
C--------------------------------------------------------------------------------
      SUBROUTINE SGWT2BCT1S(IN,IC_IBOUND_FLG)
C     ******************************************************************
C-----READ PARAMETERS AND CONVERT FOR STRUCTURED GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,
     1                      IFREFM,IBOUND,NODES,NJA,NJAS,IA,JA,JAS,ARAD
      USE GWTBCTMODULE,ONLY: PRSITY,CONC,ICBUND,CINACT,MCOMP,IADSORB,
     1  ADSORB,FLICH,IDISP,DLX,ATXY,DLY,ATYZ,DLZ,ATXZ,IZOD,IFOD,
     1  ZODRW,FODRW,ZODRS,FODRS
      USE GWFBCFMODULE, ONLY: IHANISO
C
      INTEGER, DIMENSION(:,:),    ALLOCATABLE ::ITEMP
      REAL,    DIMENSION(:,:),    ALLOCATABLE ::TEMP
      REAL,    DIMENSION(:),    ALLOCATABLE ::BULKD
      CHARACTER*24 ANAME(18)
      DATA ANAME(1) /'                  ICBUND'/
      DATA ANAME(2) /'                POROSITY'/
      DATA ANAME(3) /'            BULK DENSITY'/
      DATA ANAME(4) /'  ADSORPTION COEFFICIENT'/
      DATA ANAME(5) /'           CONCENTRATION'/
      DATA ANAME(6) /'       LONG DISPERSIVITY'/
      DATA ANAME(7) /'       TRNS DISPERSIVITY'/
      DATA ANAME(8) /' X-DIR LONG DISPERSIVITY'/
      DATA ANAME(9) /' Y-DIR LONG DISPERSIVITY'/
      DATA ANAME(10) /' Z-DIR LONG DISPERSIVITY'/
      DATA ANAME(11) /'XY-DIR TRNS DISPERSIVITY'/
      DATA ANAME(12) /'YZ-DIR TRNS DISPERSIVITY'/      
      DATA ANAME(13) /'XZ-DIR TRNS DISPERSIVITY'/
      DATA ANAME(14) /'     FREUNDLICH EXPONENT'/
      DATA ANAME(15) /'  ZERO-ORDER DECAY WATER'/
      DATA ANAME(16) /'   ZERO-ORDER DECAY SOIL'/
      DATA ANAME(17) /' FIRST-ORDER DECAY WATER'/
      DATA ANAME(18) /'  FIRST-ORDER DECAY SOIL'/
      REAL PI
C     ------------------------------------------------------------------
C1-------ALLOCATE TEMP ARRAY FOR STORING 3-D INFORMATION
      ALLOCATE(TEMP(NCOL,NROW))
      IF(IADSORB.NE.0) ALLOCATE(BULKD(NODES))
      PI = 3.1415926536
C-----------------------------------------------------------------------
C2------READ/SET SPECIES INDEPENDENT ARRAYS FOR ALL LAYERS.
C---------------------------------------------------------
C3-----READ/SET ICBUND ARRAY
      IF(IC_IBOUND_FLG.EQ.0)THEN
C3A-------READ ICBUND
        ALLOCATE(ITEMP(NCOL,NROW))
        DO K=1,NLAY
        KK=K
        CALL U2DINT(ITEMP(1,1),ANAME(1),NROW,NCOL,KK,IN,IOUT)
        DO I=1,NROW
        DO J=1,NCOL
          N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
          ICBUND(N) = ITEMP(J,I)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE (ITEMP)
C3B-------CHECK FOR ERRORS
         DO N=1,NODES
           IF(ICBUND(N).NE.0.AND.IBOUND(N).EQ.0)THEN
             WRITE(IOUT,55) N
55    FORMAT(5X,'*** ACTIVE TRANSPORT NODE WHERE FLOW WAS INACTIVE AT ',
     *  'NODE: ',I10,' ***')
             STOP
           ENDIF
         ENDDO
      ELSE
C3C-------SET ICBUND
        DO N=1,NODES
          ICBUND(N) = IBOUND(N)
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------------
C4-----READ POROSITY INTO ARRAY PRSITY.
      DO K=1,NLAY
        KK=K
        CALL U2DREL(TEMP(1,1),ANAME(2),NROW,NCOL,KK,IN,IOUT)
        DO I=1,NROW
        DO J=1,NCOL
          N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
          PRSITY(N) = TEMP(J,I)
        ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------
C5-----READ BULK DENSITY INTO ARRAY BULKD.
      IF(IADSORB.NE.0)THEN
        DO K=1,NLAY
          KK=K
          CALL U2DREL(TEMP(1,1),ANAME(3),NROW,NCOL,KK,IN,IOUT)
          DO I=1,NROW
          DO J=1,NCOL
            N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
            BULKD(N) = TEMP(J,I)
          ENDDO
          ENDDO
        ENDDO
      ENDIF
C----------------------------------------------------------------------------
C6-----READ DISPERSION COEFFICIENTS AND FACE ANGLES IF DISPERSION IS ON.
      IF(IDISP.NE.0)THEN
        IF(IHANISO.EQ.0)THEN
C6A-------ARAD HAS NOT BEEN ALLOCATED YET SO ALLOCATE IT
          ALLOCATE(ARAD(NJAS))
        ENDIF
C6B------SET FACE ANGLES IN ARAD
        DO N=1,NODES
          DO II = IA(N)+1,IA(N+1)-1
            JJ = JA(II)
            IF(JJ.GE.N) CYCLE
            IIS = JAS(II)
            IF((N-JJ).EQ.1) THEN
              ARAD(IIS) = 0.0
            ELSE
              ARAD(IIS) =  pi/2.
            ENDIF
          ENDDO
        ENDDO
C7-----READ DISPERSION COEFFICIENTS
       IF(IDISP.EQ.1)THEN
C8--------READ ISOTROPIC DISPERSION COEFFICIENTS
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(1,1),ANAME(6),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             DLX(N) = TEMP(J,I)
             DLY(N) = TEMP(J,I)
             DLZ(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO
C
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(1,1),ANAME(7),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             ATXY(N) = TEMP(J,I)
             ATYZ(N) = TEMP(J,I)
             ATXZ(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO
       ELSEIF(IDISP.EQ.2)THEN
C9--------READ ANISOTROPIC DISPERSION COEFFICIENTS
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(:,:),ANAME(8),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             DLX(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO
C
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(1,1),ANAME(9),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             DLY(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO
C
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(1,1),ANAME(10),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             DLZ(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO
C
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(1,1),ANAME(11),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             ATXY(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO
C
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(1,1),ANAME(12),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             ATYZ(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO
C
         DO K=1,NLAY
           KK=K
           CALL U2DREL(TEMP(1,1),ANAME(13),NROW,NCOL,KK,IN,IOUT)
           DO I=1,NROW
           DO J=1,NCOL
             N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
             ATXZ(N) = TEMP(J,I)
           ENDDO
           ENDDO
         ENDDO         
       ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C10------READ SPECIES DEPENDENT ARRAYS FOR ALL LAYERS.
C
      DO ICOMP=1,MCOMP
        WRITE(IOUT,10) ICOMP
10      FORMAT(80('-')/1X,'THE FOLLOWING ARRAYS ARE READ FOR SPECIES ',
     *    1X,'NUMBER ',I3/80('-'))
C
C---------------------------------------------------------
C11-----READ ADSORPTION COEFFICIENT OF EACH SPECIES
      IF(IADSORB.NE.0)THEN
        DO K=1,NLAY
          KK=K
          CALL U2DREL(TEMP(1,1),ANAME(4),NROW,NCOL,KK,IN,IOUT)
          DO I=1,NROW
          DO J=1,NCOL
            N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
            ADSORB(N,ICOMP) = TEMP(J,I)
          ENDDO
          ENDDO
        ENDDO
C11A-------PREPARE CONSTANT PART OF ADSORPTION TERM
        DO N = 1, NODES
          ADSORB(N,ICOMP) = ADSORB(N,ICOMP) * BULKD(N)
        ENDDO
C---------------------------------------------------------
C12-----READ FREUNDLICH EXPONENT OF EACH SPECIES
        IF(IADSORB.EQ.2)THEN
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(14),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              FLICH(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C---------------------------------------------------------
C13-----READ ZERO ORDER DECAY COEFFICIENTS
      IF(IZOD.EQ.1)THEN
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(15),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              ZODRW(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
      ELSEIF(IADSORB.GT.0)THEN
        IF(IZOD.EQ.2)THEN
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(16),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              ZODRS(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
        ELSEIF(IZOD.EQ.3)THEN
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(15),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              ZODRW(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(16),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              ZODRS(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C---------------------------------------------------------
C14-----READ FIRST ORDER DECAY COEFFICIENTS
      IF(IFOD.EQ.1)THEN
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(17),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              FODRW(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
      ELSEIF(IADSORB.GT.0)THEN
        IF(IFOD.EQ.2)THEN
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(18),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              FODRS(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
        ELSEIF(IFOD.EQ.3)THEN
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(17),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              FODRW(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
          DO K=1,NLAY
            KK=K
            CALL U2DREL(TEMP(1,1),ANAME(18),NROW,NCOL,KK,IN,IOUT)
            DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              FODRS(N,ICOMP) = TEMP(J,I)
            ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C
C---------------------------------------------------------
C15-----READ CONCENTRATION OF EACH SPECIES
      DO K=1,NLAY
        KK=K
        CALL U2DREL(TEMP(1,1),ANAME(5),NROW,NCOL,KK,IN,IOUT)
        DO I=1,NROW
        DO J=1,NCOL
          N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
          CONC(N,ICOMP) = TEMP(J,I)
        ENDDO
        ENDDO
      ENDDO
C
C---------------------------------------------------------
      ENDDO  ! SPECIES DO LOOP
C---------------------------------------------------------
      IF(IADSORB.NE.0) DEALLOCATE(BULKD)
      DEALLOCATE(TEMP)
C---------------------------------------------------------
C
      RETURN
      END
C----------------------------------------------------------------------------
      SUBROUTINE SGWT2BCT1G(IN,IC_IBOUND_FLG)
C     ******************************************************************
C-----READ PARAMETERS AND CONVERT FOR UNSTRUCTURED (GENERALIZED) GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,IA,JA,JAS,NJA,NJAG,
     1                     ARAD,IFREFM,IBOUND,NODES,NODLAY,IDSYMRD,NJAS,
     2                     IATMP,NJATMP
      USE GWTBCTMODULE,ONLY: PRSITY,CONC,ICBUND,CINACT,MCOMP,IADSORB,
     1  ADSORB,FLICH,IDISP,DLX,ATXY,DLY,ATYZ,DLZ,ATXZ,IZOD,IFOD,
     1  ZODRW,FODRW,ZODRS,FODRS
      USE GWFBCFMODULE, ONLY: IHANISO
C
      REAL,  DIMENSION(:),    ALLOCATABLE ::BULKD
      REAL,  DIMENSION(:),    ALLOCATABLE ::TEMPC
      REAL,  DIMENSION(:),    ALLOCATABLE ::TEMP
      CHARACTER*24 ANAME(19)
      DATA ANAME(1) /'                  ICBUND'/
      DATA ANAME(2) /'                POROSITY'/
      DATA ANAME(3) /'            BULK DENSITY'/
      DATA ANAME(4) /'  ADSORPTION COEFFICIENT'/
      DATA ANAME(5) /'           CONCENTRATION'/
      DATA ANAME(6) /'              FACE ANGLE'/
      DATA ANAME(7) /'       LONG DISPERSIVITY'/
      DATA ANAME(8) /'       TRNS DISPERSIVITY'/
      DATA ANAME(9) /' X-DIR LONG DISPERSIVITY'/
      DATA ANAME(10) /' Y-DIR LONG DISPERSIVITY'/
      DATA ANAME(11) /' Z-DIR LONG DISPERSIVITY'/
      DATA ANAME(12) /'XY-DIR TRNS DISPERSIVITY'/
      DATA ANAME(13) /'YZ-DIR TRNS DISPERSIVITY'/
      DATA ANAME(14) /'XZ-DIR TRNS DISPERSIVITY'/      
      DATA ANAME(15) /'  ZERO-ORDER DECAY WATER'/
      DATA ANAME(16) /'   ZERO-ORDER DECAY SOIL'/
      DATA ANAME(17) /' FIRST-ORDER DECAY WATER'/
      DATA ANAME(18) /'  FIRST-ORDER DECAY SOIL'/
      DATA ANAME(19) /'     FREUNDLICH EXPONENT'/
C     ------------------------------------------------------------------
      ALLOCATE (TEMPC(NODES))
C1-------ALLOCATE TEMP ARRAY FOR BULK DENSITY
      IF(IADSORB.NE.0) ALLOCATE(BULKD(NODES))
C-----------------------------------------------------------------------
C2------READ/SET SPECIES INDEPENDENT ARRAYS FOR ALL LAYERS.
C-----------------------------------------------------------------------      
C3-----READ/SET ICBUND ARRAY
      IF(IC_IBOUND_FLG.EQ.0)THEN
C3A-------READ ICBUND
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DINT(ICBUND(NSTRT),ANAME(1),NDSLAY,K,IN,IOUT)
        ENDDO
C3B-------CHECK FOR ERRORS
         DO N=1,NODES
           IF(ICBUND(N).NE.0.AND.IBOUND(N).EQ.0)THEN
             WRITE(IOUT,55) N
55    FORMAT(5X,'*** ACTIVE TRANSPORT NODE WHERE FLOW WAS INACTIVE AT ',
     *  'NODE: ',I10,' ***')
             STOP
           ENDIF
         ENDDO
      ELSE
C3C-------SET ICBUND
        DO N=1,NODES
          ICBUND(N) = IBOUND(N)
        ENDDO
      ENDIF
C
C---------------------------------------------------------
C4-----READ POROSITY INTO ARRAY PRSITY.
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DREL(PRSITY(NSTRT),ANAME(2),NDSLAY,K,IN,IOUT)
      ENDDO
C---------------------------------------------------------
C5-----READ BULK DENSITY INTO ARRAY TEMP IF ADSORPTION.
      IF(IADSORB.NE.0)THEN
        DO K = 1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(BULKD(NSTRT),ANAME(3),NDSLAY,K,IN,IOUT)
        ENDDO
      ENDIF
C---------------------------------------------------------
C6-----READ DISPERSION COEFFICIENTS AND FACE ANGLES IF DISPERSION IS ON.
      IF(IDISP.NE.0)THEN
C7------READ FACE ANGLES AND STORE IN ARAD
        IF(IHANISO.EQ.0)THEN
C8--------ARAD HAS NOT BEEN ALLOCATED YET SO ALLOCATE IT
          ALLOCATE(ARAD(NJAS))
        ENDIF
        CALL U1DRELNJA(ARAD,IATMP,ANAME(6),NJATMP,IN,IOUT,IDSYMRD)
C9------READ DISPERSION COEFFICIENTS
        IF(IDISP.EQ.1)THEN
C9A---------READ ISOTROPIC COEFFICIENTS
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(DLX(NSTRT),ANAME(7),NDSLAY,K,IN,IOUT)
            DO NL = NSTRT,NNDLAY 
              DLY(NL) = DLX(NL)
              DLZ(NL) = DLX(NL)
            ENDDO
          ENDDO          
C
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(ATXY(NSTRT),ANAME(8),NDSLAY,K,IN,IOUT)
            DO NL = NSTRT,NNDLAY
              ATYZ(NL) = ATXY(NL)
              ATXZ(NL) = ATXY(NL)
            ENDDO
          ENDDO
        ELSEIF(IDISP.EQ.2)THEN
C9B---------READ ANISOTROPIC COEFFICIENTS
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(DLX(NSTRT),ANAME(9),NDSLAY,K,IN,IOUT)
          ENDDO
C
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(DLY(NSTRT),ANAME(10),NDSLAY,K,IN,IOUT)
          ENDDO
C
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(DLZ(NSTRT),ANAME(11),NDSLAY,K,IN,IOUT)
          ENDDO
C
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(ATXY(NSTRT),ANAME(12),NDSLAY,K,IN,IOUT)
          ENDDO
C
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(ATYZ(NSTRT),ANAME(13),NDSLAY,K,IN,IOUT)
          ENDDO
C
          DO K = 1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(ATXZ(NSTRT),ANAME(14),NDSLAY,K,IN,IOUT)
          ENDDO
C
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C10------READ SPECIES DEPENDENT ARRAYS FOR ALL LAYERS.
C
      DO ICOMP=1,MCOMP
        WRITE(IOUT,10) ICOMP
10      FORMAT(80('-')/1X,'THE FOLLOWING ARRAYS ARE READ FOR SPECIES ',
     *    1X,'NUMBER ',I3/80('-'))
C
C---------------------------------------------------------
C11-----READ ADSORPTION COEFFICIENT FOR EACH SPECIES
      IF(IADSORB.NE.0)THEN
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(TEMPC(NSTRT),ANAME(4),NDSLAY,K,IN,IOUT)
        ENDDO
C11A-------PREPARE CONSTANT PART OF ADSORPTION TERM
        DO N = 1, NODES
          ADSORB(N,ICOMP) = TEMPC(N)
          ADSORB(N,ICOMP) = ADSORB(N,ICOMP) * BULKD(N)
        ENDDO
C---------------------------------------------------------
C12-----READ ADSORPTION EXPONENT FOR EACH SPECIES
        IF(IADSORB.EQ.2)THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(19),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FLICH(N,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
      ENDIF
C---------------------------------------------------------
C13-----READ ZERO ORDER DECAY COEFFICIENTS
      IF(IZOD.EQ.1)THEN
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(TEMPC(NSTRT),ANAME(15),NDSLAY,K,IN,IOUT)
        ENDDO
        DO N = 1, NODES
          ZODRW(N,ICOMP) = TEMPC(N)
        ENDDO
      ELSEIF(IADSORB.GT.0)THEN
        IF(IZOD.EQ.2)THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(16),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            ZODRS(N,ICOMP) = TEMPC(N)
          ENDDO
        ELSEIF(IZOD.EQ.3)THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(15),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            ZODRW(N,ICOMP) = TEMPC(N)
          ENDDO
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(16),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            ZODRS(N,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
      ENDIF
C---------------------------------------------------------
C14-----READ FIRST ORDER DECAY COEFFICIENTS
      IF(IFOD.EQ.1)THEN
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(TEMPC(NSTRT),ANAME(17),NDSLAY,K,IN,IOUT)
        ENDDO
        DO N = 1, NODES
          FODRW(N,ICOMP) = TEMPC(N)
        ENDDO
      ELSEIF(IADSORB.GT.0)THEN
        IF(IFOD.EQ.2)THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(18),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FODRS(N,ICOMP) = TEMPC(N)
          ENDDO
        ELSEIF(IFOD.EQ.3)THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(17),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FODRW(N,ICOMP) = TEMPC(N)
          ENDDO
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(18),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FODRS(N,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
      ENDIF
C
C---------------------------------------------------------
C15-----READ CONCENTRATION OF EACH SPECIES
      DO K=1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DREL(TEMPC(NSTRT),ANAME(5),NDSLAY,K,IN,IOUT)
      ENDDO
      DO N = 1,NODES
        CONC(N,ICOMP) = TEMPC(N)
      ENDDO
C
C---------------------------------------------------------
      ENDDO  ! SPECIES DO LOOP
C---------------------------------------------------------
      IF(IADSORB.NE.0) DEALLOCATE(BULKD)
      DEALLOCATE(TEMPC)
C---------------------------------------------------------
C16---RETURN
      RETURN
      END
C----------------------------------------------------------------------------
      SUBROUTINE SGWT2BCT1CLN(IN,IC_IBOUND_FLG)
C     ******************************************************************
C-----READ PARAMETERS FOR CLN NODES AND INTERACTION WITH GRID-BLOCKS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,IA,JA,NJA,
     1                      IFREFM,IBOUND,NODES,NODLAY
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GWTBCTMODULE,ONLY: PRSITY,CONC,ICBUND,CINACT,MCOMP,IADSORB,
     1  ADSORB,FLICH,IDISP,DLX,ATXY,IZOD,IFOD,ZODRW,FODRW
C
      REAL,  DIMENSION(:),    ALLOCATABLE ::TEMPC
      CHARACTER*24 ANAME(6)
      DATA ANAME(1) /'                  ICBUND'/      
      DATA ANAME(2) /'DISP COEF ALONG CLN TUBE'/
      DATA ANAME(3) /'DISP COEF FOR CLN-MATRIX'/
      DATA ANAME(4) /'  ZERO-ORDER DECAY WATER'/
      DATA ANAME(5) /' FIRST-ORDER DECAY WATER'/
      DATA ANAME(6) /'     INITIAL CONC IN CLN'/
C     ------------------------------------------------------------------
C
C0----SET INDICES
      NSTRT = NODES+1
      NDSLAY = NCLNNDS
C1------READ/SET ICBUND ARRAY
      IF(IC_IBOUND_FLG.EQ.0)THEN
C1A-------READ ICBUND
        CALL U1DINT(ICBUND(NSTRT),ANAME(1),NDSLAY,K,IN,IOUT)
C1B-------CHECK FOR ERRORS
         DO N=NODES+1,NODES + NDSLAY
           IF(ICBUND(N).NE.0.AND.IBOUND(N).EQ.0)THEN
             WRITE(IOUT,55) N
55    FORMAT(5X,'*** ACTIVE TRANSPORT NODE WHERE FLOW WAS INACTIVE AT ',
     *  'NODE: ',I10,' ***')
             STOP
           ENDIF
         ENDDO
      ELSE
C1C-------SET ICBUND
        DO N=NODES+1, NODES + NCLNNDS
          ICBUND(N) = IBOUND(N)
        ENDDO
      ENDIF
C
C2-----READ DISPERSION COEFFICIENTS ALONG CONDUIT AND FOR CONDUIT-MATRIX INTERACTION
      IF(IDISP.NE.0)THEN
        NSTRT = NODES+1
        NDSLAY = NCLNNDS
        CALL U1DREL(DLX(NSTRT),ANAME(2),NDSLAY,K,IN,IOUT)
        CALL U1DREL(ATXY(NSTRT),ANAME(3),NDSLAY,K,IN,IOUT)
      ENDIF
C
C------------------------------------------------------------------
C3-------READ COMPONENT DEPENDENT PARAMETERS AND INITIAL CONCENTRATIONS
      ALLOCATE(TEMPC(NCLNNDS))
      DO ICOMP=1,MCOMP
        WRITE(IOUT,10) ICOMP
10      FORMAT(80('-')/1X,'THE FOLLOWING ARRAYS ARE READ FOR SPECIES ',
     *    1X,'NUMBER ',I3/80('-'))
C------------------------------------------------------------------
C4-------ZERO-ORDER DECAY IN WATER
        IF(IZOD.EQ.1.OR.IZOD.EQ.3)THEN
          NSTRT = NODES+1
          NDSLAY = NCLNNDS
          CALL U1DREL(TEMPC,ANAME(4),NDSLAY,K,IN,IOUT)
          DO N = 1,NCLNNDS
            ZODRW(N+NODES,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
C------------------------------------------------------------------
C5-------FIRST-ORDER DECAY IN WATER
        IF(IFOD.EQ.1.OR.IZOD.EQ.3)THEN
          NSTRT = NODES+1
          NDSLAY = NCLNNDS
          CALL U1DREL(TEMPC,ANAME(5),NDSLAY,K,IN,IOUT)
          DO N = 1,NCLNNDS
            FODRW(N+NODES,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
C------------------------------------------------------------------
C6-------INITIAL CONCENTRATIONS
        CALL U1DREL(TEMPC,ANAME(6),NDSLAY,K,IN,IOUT)
        DO N = 1,NCLNNDS
        CONC(N+NODES,ICOMP) = TEMPC(N)
        ENDDO
      ENDDO
      DEALLOCATE(TEMPC)
C
C------------------------------------------------------------------
C7----RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1SOLVE(KITER,KSTP,KPER)
C     ******************************************************************
C     SOLVE THE TRANSPORT EQUATION FOR ALL COMPONENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,NODLAY,ISYM,
     1  AMAT,RHS,IA,JA,PGF,NJA,NODES,IUNIT,IUNSTR,NEQS,INCLN,IDPT,
     2  ISSFLG,HNEW,HOLD,TOP,BOT,Sn,So,LAYNOD,FMBE,IFMBC
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GWFBCFMODULE, ONLY: LAYCON
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CONCO,NTITER,ICBUND,CICLOSE,
     1    CINACT,CNCG,LRCC,MSUMT,IDISP,FCNCG,FLRCC,ADMAT,IXDISP,ITVD,
     1    DXCS,DYCS,DZCS
       DOUBLE PRECISION, SAVE, DIMENSION(:), ALLOCATABLE :: CNEW
       DOUBLE PRECISION DELTO,DTERMS,RTERMS,DTERMD,RTERMD,VODT,SAT
      save itp,DELTO
C     ------------------------------------------------------------------
C1 --------INITIALIZE TRANSPORT FOR ALL COMPONENTS
C     ------------------------------------------------------------------      
C1A--------INDICATE THAT SOLUTION IS FOR TRANSPORT
          CALL UMESPR('SOLVING FOR TRANSPORT',' ',IOUT)
          WRITE(*,25)KPER,KSTP
   25     FORMAT(' Solving:  Stress period: ',i5,4x,
     &       'Time step: ',i5,4x,'Groundwater Transport Eqn.')
C1B------SET FLAGS
      ICNVG = 0
      ISS=ISSFLG(KPER)
      IF(KSTP.EQ.1.AND.KPER.EQ.1) DELTO = 0.0
C1C----ADVANCE SOLUTION 
      CALL GWT2BCT1AD(KITER,KSTP,KPER,ISS)        
C
C-----------------------------------------------------------------------
C2----FILL TERMS CONSTANT TO COMPONENTS AND TRANSPORT ITERATIONS IN ADMAT
C-----------------------------------------------------------------------
      IADMATF = 1
      IF(KSTP.GT.1.AND.ISS.EQ.1)IADMATF = 0
C2A-----NO NEED TO REFILL ADMAT IF IADMATF IS ZERO
      IF(IADMATF.EQ.1)THEN
C-------ZERO OUT ADMAT ARRAY
        DO N=1,NJA
          ADMAT(N) = 0.0
        ENDDO
C-----------------------------------------------------------------------
C2B-------COMPUTE DISPERSION TERM
        IF(IDISP.NE.0) THEN
C2C---------CALCULATE VELOCITY COMPONENTS FOR DISPERSION TERM
          CALL GWT2BCT1VELCALC
          DO N=1,NEQS
            IF(ICBUND(N).NE.0)THEN
C2D-------------FILL DISPERSIVE TERM FOR ACTIVE TRANSPORT NODES
              CALL GWT2BCT1DSP(N)
            ENDIF
          ENDDO
        ENDIF
C-----------------------------------------------------------------------
C2E-------COMPUTE ADVECTION TERM
        DO N=1,NEQS
          IF(ICBUND(N).NE.0)THEN
C2F-----------FILL UPSTREAM ADVECTIVE TERM FOR ACTIVE TRANSPORT NODES
            CALL GWT2BCT1ADV(N)
          ENDIF
          ENDDO
C2G-----------SUBTRACT FLOW BALANCE ERROR FROM DIAGONAL 
        IF(IFMBC.NE.0)THEN
          amaxerr = 0.0 
          nmax = 0
          DO N=1,NEQS
            IF(ICBUND(N).NE.0)THEN
              aerr = abs(fmbe(n)  )
              if(aerr.gt.amaxerr) then
                  amaxerr = aerr
                  nmax = n
              endif    
              ADMAT(IA(N)) = ADMAT(IA(N)) - FMBE(N)
            ENDIF
          ENDDO  
          write(iout,*)'max flow fmbe and n are',amaxerr,nmax
        ENDIF  
      ENDIF
C-----------------------------------------------------------------------
C3----LOOP OVER ALL COMPONENTS
C-----------------------------------------------------------------------
      DO ICOMP = 1,MCOMP ! COMPONENT LOOP
C4------LOOP OVER NONLINEAR TRANSPORT ITERATIONS (TVD)
        DO ITITER = 1,NTITER
C5--------FORMULATE CELL-BY-CELL TRANSPORT TERMS IN LHS MATRIX AND RHS
          RHS = 0.0
C5A---------FILL TERMS CONSTANT TO ITERATIONS AND COMPONENTS ON LHS MATRIX
          AMAT = ADMAT
C-----------------------------------------------------------------------
C5B---------FILL CROSS DISPERSION TERM ON RHS - ONLY FOR MATRIX BLOCKS
          IF(IDISP.GT.0.AND.IXDISP.NE.0)THEN
C5C-----------COMPUTE DCDX, DCDY, DCDZ FOR CROSS DISPERSION TERM ON RHS
            ALLOCATE(DXCS(NODES))
            ALLOCATE(DYCS(NODES))
            ALLOCATE(DZCS(NODES))
C
            CALL GWT2BCT1DC(ICOMP)
C5D-----------FILL CROSS DISPERSION TERM INTO RHS
            DO N=1,NODES
              IF(ICBUND(N).NE.0)THEN
                 CALL GWT2BCT1DSPX(N)
              ENDIF
            ENDDO
            DEALLOCATE(DXCS)
            DEALLOCATE(DYCS)
            DEALLOCATE(DZCS)
          ENDIF
C-----------------------------------------------------------------------
C5E---------FILL TVD TERM ON RHS
          IF(ITVD.GT.0)THEN
            DO N=1,NEQS
              IF(ICBUND(N).NE.0)THEN
                CALL GWT2BCT1TVD(N,ICOMP)
              ENDIF
            ENDDO
          ENDIF
C-----------------------------------------------------------------------
C5F---------FILL STORAGE AND DECAY TERMS ON RHS AND DIAGONAL
          DO N=1,NEQS
            IF(ICBUND(N).NE.0)THEN
              CALL GWT2BCT1STO(N,ICOMP,DTERMS,RTERMS,ISS)
              CALL GWT2BCT1DCY(N,ICOMP,DTERMD,RTERMD)
              IPIV = IA(N)
              AMAT(IPIV) = AMAT(IPIV) + DTERMS + DTERMD
              RHS(N) = RHS(N) + RTERMS + RTERMD
            ENDIF
          ENDDO
C----------------------------------------------------------------------
C6--------FILL TRANSPORT BOUNDARY CONDITIONS
C----------------------------------------------------------------------
C6A-------PRESCRIBED CONCENTRATION BOUNDARY
          IF(IUNIT(14).GT.0) CALL GWT2PCB1FM(ICOMP)
C6B-------PRESCRIBED HEAD BOUNDARY
          CALL GWT2PHB1FM(ICOMP)
C6C-------WELL INFLOW / OUTFLOW BOUNDARY
          IF(IUNIT(2).GT.0) CALL GWT2WEL1FM(ICOMP)
C6D-------GHB INFLOW / OUTFLOW BOUNDARY
          IF(IUNIT(7).GT.0) CALL GWT2GHB1FM(ICOMP)
C6E-------DRAIN OUTFLOW BOUNDARY
          IF(IUNIT(3).GT.0) CALL GWT2DRN1FM(ICOMP)
C6F-------DRT INFLOW / OUTFLOW BOUNDARY
          IF(IUNIT(40).GT.0) CALL GWT2DRT1FM(ICOMP)
C6F-------QRT INFLOW / OUTFLOW BOUNDARY
          IF(IUNIT(41).GT.0) CALL GWT2QRT1FM(ICOMP)          
C6G-------RIVER OUTFLOW BOUNDARY
          IF(IUNIT(4).GT.0) CALL GWT2RIV1FM(ICOMP)          
C6H-------EVT OUTFLOW BOUNDARY
          IF(IUNIT(5).GT.0) CALL GWT2EVT1FM(ICOMP)
C6I-------ETS OUTFLOW BOUNDARY
          IF(IUNIT(39).GT.0) CALL GWT2ETS1FM(ICOMP)          
C6J-------RECH INFLOW / OUTFLOW (WITH SEEPAGE) BOUNDARY
          IF(IUNIT(8).GT.0) CALL GWT2RCH1FM(ICOMP)
C----------------------------------------------------------------------
C7--------FILL DUAL POROSITY TRANSPORT TERMS
C----------------------------------------------------------------------
          IF(IDPT.GT.0) CALL GWT2DPTU1FM(KPER,ICOMP,ISS)          
C7A------REDUCE IMMOBILE DOMAIN EQUATION INTO MATRIX AND RHS
          IF(IDPT.GT.0) CALL SSMS2DPT1RED(KPER)
C----------------------------------------------------------------------
C8 ------SOLVE THE GROUNDWATER TRANSPORT EQUATION FOR THIS SPECIES
         CALL GWT2SOLVE(ITITER,ICNVG,ICOMP,KSTP,KPER,ITP,IN_ITER)
C----------------------------------------------------------------------
C9--------BACK-SUBSTITUTE FOR IMMOBILE DOMAIN CONCENTRATIONS
C----------------------------------------------------------------------
          IF(IDPT.GT.0) CALL SSMS2DPT1BKS(KPER,ICOMP)
C----------------------------------------------------------------------
C10---------EVALUATE AND STORE MAXIMUM CONCENTRATION CHANGE AND CONVERGENCE
          CALL CONCCONV(ICOMP,ITITER,ICNVG)
C
          IF(ICNVG.EQ.1) GO TO 205
C--------------------------------------------------------------------
        ENDDO            !END TRANSPORT ITERATION LOOP (ITITER)
C--------------------------------------------------------------------
205     CONTINUE
C
C11-------WRITE TVD ITERATION SUMMARY
        ITERS = ITITER-1
        IF(ICNVG.EQ.1) ITERS = ITITER
        WRITE(IOUT,1010) ITERS,KSTP,KPER
 1010   FORMAT(/1X,I5,' CALLS TO SPARSE MATRIX SOLVER PACKAGE ',
     &  ' IN TRANSPORT TIME STEP',I8,' STRESS PERIOD',I8)
C
C11A------FOR BCF NODES
       CALL SSMS2BCFU1P(Cncg,LrcC,ITERS,NTITER,IOUT,IUNSTR)
C11B------FOR CLN NODES
       CALL SSMS2CLN7P(ITERS,NTITER,IOUT,IUNSTR,FCNCG,FLRCC,INCLN)
C
        WRITE(IOUT,*)
C----------------------------------------------------------------------
      ENDDO              ! END COMPONENT LOOP (ICOMP)
C----------------------------------------------------------------------
C--------------------------------------------------------------------------
C12-----DETERMINE WHICH PRINTOUT IS REQUIRED.
      CALL GWT2BCT1OC(KSTP,KPER,1)
C----------------------------------------------------------------------
C13------COMPUTE MASS BALANCE TERMS FOR ALL COMPONENTS
      DO ICOMP = 1,MCOMP
C---------------------------------------------------------------------
        MSUMT = 1
C13A-------MASS STORAGE TERM
        CALL GWT2STO1BD(KSTP,KPER,ICOMP,ISS)
C13B-------MASS DECAY TERMS - COMBINE ZERO AND FIRST ORDER
        CALL GWT2DCY1BD(KSTP,KPER,ICOMP)
C
C13C-------MASS STORAGE AND DECAY TERMS IN IMMOBILE DOMAIN
        IF(IDPT.GT.0) THEN
            CALL GWT2STOIM1BD(KSTP,KPER,ICOMP)
            CALL GWT2DCYIM1BD(KSTP,KPER,ICOMP)
        ENDIF
C13D-------MASS ERROR DUE TO FLOW MASS BALANCE ERROR ADJUSTMENT 
        IF(IFMBC.NE.0)THEN
          CALL GWT2FMBE1BD(KSTP,KPER,ICOMP)        
        ENDIF  
C        
C----------------------------------------------------------------------
C14---------MASS BALANCE FOR BOUNDARY COMPONENTS
C        
C14A-------PRESCRIBED CONCENTRATION BOUNDARY
        IF(IUNIT(14).GT.0) CALL GWT2PCB1BD(KSTP,KPER,ICOMP)
C14B-------PRESCRIBED HEAD BOUNDARY
          CALL GWT2PHB1BD(KSTP,KPER,ICOMP)
C14C-------WELL INFLOW / OUTFLOW BOUNDARY
        IF(IUNIT(2).GT.0) CALL GWT2WEL1BD(KSTP,KPER,ICOMP)
C14D-------GHB INFLOW / OUTFLOW BOUNDARY
        IF(IUNIT(7).GT.0) CALL GWT2GHB1BD(KSTP,KPER,ICOMP)
C14E-------DRAIN INFLOW / OUTFLOW BOUNDARY
        IF(IUNIT(3).GT.0) CALL GWT2DRN1BD(KSTP,KPER,ICOMP)
C14F-------DRT INFLOW / OUTFLOW BOUNDARY
        IF(IUNIT(40).GT.0) CALL GWT2DRT1BD(KSTP,KPER,ICOMP)        
C14F-------QRT INFLOW / OUTFLOW BOUNDARY
        IF(IUNIT(41).GT.0) CALL GWT2QRT1BD(KSTP,KPER,ICOMP)          
C14G-------RIVER INFLOW / OUTFLOW BOUNDARY
        IF(IUNIT(4).GT.0) CALL GWT2RIV1BD(KSTP,KPER,ICOMP)        
C14H-------EVT OUTFLOW BOUNDARY
        IF(IUNIT(5).GT.0) CALL GWT2EVT1BD(KSTP,KPER,ICOMP)
C14I-------ETS OUTFLOW BOUNDARY
        IF(IUNIT(39).GT.0) CALL GWT2ETS1BD(KSTP,KPER,ICOMP)        
C14J-------RCH OUTFLOW BOUNDARY
        IF(IUNIT(8).GT.0) CALL GWT2RCH1BD(KSTP,KPER,ICOMP)
      ENDDO
C--------------------------------------------------------------------------
C15-----PRINT AND/OR SAVE TRANSPORT RESULTS.
      CALL GWT2BCT1OT(KSTP,KPER,1)
C15A-----PRINT AND/OR SAVE DUAL POROSITY TRANSPORT RESULTS.
      IF(IDPT.GT.0) CALL GWT2DPT1OT(KSTP,KPER,1)
      DELTO = DELT
C
C16-----RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1AD(KITER,KSTP,KPER,ISS)
C     ******************************************************************
C     ADVANCE TRANSPORT SOLUTION FOR ALL COMPONENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,IBOUND,NODES,IUNIT,IDPT,NEQS,INCLN,
     2  ISSFLG,HNEW,HOLD,TOP,BOT,Sn,So,LAYNOD
      USE GWFBCFMODULE, ONLY: LAYCON
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CONCO,NTITER,ICBUND,CICLOSE,
     1    CINACT,CNCG,LRCC,MSUMT,IDISP,FCNCG,FLRCC,ADMAT,IXDISP,ITVD,
     1    DXCS,DYCS,DZCS
C     ------------------------------------------------------------------
C1----SET/UPDATE OLD ARRAYS USING NEW VALUES (CONCO FROM CONC)
      DO ICOMP=1,MCOMP
        DO N=1,NEQS
          CONCO(N,ICOMP) = CONC(N,ICOMP)
        ENDDO
      ENDDO
C2----COPY NEW VALUES INTO OLD VALUE ARRAYS FOR IMMOBILE DOMAIN
      IF(IDPT.GT.0) CALL GWT2DPT1AD
C3-----COMPUTE Sn AND So FOR GWF DOMAIN WHEN LAYCON IS 1 OR 3 FOR TRANSIENT FLOW SITUATIONS
      IF(ISS.EQ.0)THEN
        DO N=1,NODES
           K = LAYNOD(N)
           LC = LAYCON(K)
           IF(LAYCON(K).EQ.1.OR.LAYCON(K).EQ.3)THEN
             IF (KSTP.EQ.1.AND.KPER.EQ.1) THEN
               SAT = (HOLD(N) - BOT(N)) / (TOP(N) - BOT(N))
               IF(SAT.GT.1.0) SAT = 1.0
               IF(SAT.LT.0.0) SAT = 0.0
               So(N) = SAT
             ELSE
               So(N) = Sn(n)
             ENDIF
             SAT = (HNEW(N) - BOT(N)) / (TOP(N) - BOT(N))
             IF(SAT.GT.1.0) SAT = 1.0
             IF(SAT.LT.0.0) SAT = 0.0
             Sn(N) = SAT
           ENDIF
        ENDDO
      ENDIF
C4------COMPUTE INITIAL MASS AND WRITE TO LISTING FILE AT FIRST TIME OF FIRST STRESS PERIOD
      IF(KSTP.EQ.1.AND.KPER.EQ.1)THEN
        CALL INITMASS
        IF(IDPT.NE.0) CALL INITMASSIM
       IF(INCLN.NE.0) CALL INITMASSCLN
      ENDIF      
C
C5------RETURN
      RETURN
      END
C----------------------------------------------------------------------      
      SUBROUTINE GWT2BCT1VELCALC
C     ******************************************************************
C     COMPUTE DARCY VELOCITY COMPONENTS FOR EACH NODE FOR DISPERSION COMPUTATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,IVC,ISYM,
     1  ARAD,AMAT,RHS,ISSFLG,IA,JA,JAS,NJA,NODES,FAHL,BOT,NEQS,INCLN
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CBCF,ICBUND,VELNOD,PRSITY
      REAL, DIMENSION(:),    ALLOCATABLE  ::XCS, YCS, ZCS
      DOUBLE PRECISION VFACE,VFACE1,VFACE2
      REAL ANGLE,CA,SA,ACA,ASA
C     ------------------------------------------------------------------
C1-----FIRST VELOCITIES FOR POROUS MATRIX GRID-BLOCKS
      ALLOCATE(XCS(NODES))
      ALLOCATE(YCS(NODES))
      ALLOCATE(ZCS(NODES))
      XCS = 0.0
      YCS = 0.0
      ZCS = 0.0
      VELNOD = 0.0
C2-----LOOP OVER ALL ACTIVE NODES AND COMPUTE VELOCITY AS AN AVERAGE FROM EACH FACE FLUX
      DO N=1,NODES
        IF(ICBUND(N).EQ.0) CYCLE
C3-------GO OVER UPPER CONNECTIONS OF NODE N AND FILL Vx, Vy, Vz IN BOTH
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IF(JJ.GT.N.AND.JJ.LE.NODES)THEN
            IIS = JAS(II)
            IF(ICBUND(JJ).NE.0)THEN
              IF(IVC(IIS).EQ.1)THEN !VERTICAL DIRECTION CONNECTION - z flux was in negative direction
                VFACE = CBCF(IIS)/FAHL(IIS)
                VELNOD(N,3) = VELNOD(N,3) + VFACE
                VELNOD(JJ,3) = VELNOD(JJ,3) + VFACE
                ZCS(N) = ZCS(N) + 1
                ZCS(JJ) = ZCS(JJ) + 1
              ELSE                 !HORIZONTAL DIRECTION CONNECTION
                VFACE = CBCF(IIS)/FAHL(IIS)
                VFACE1 = VFACE / (TOP(N) - BOT(N))
                VFACE2 = VFACE / (TOP(JJ) - BOT(JJ))
C4---------------X-DIRECTION COMPONENT IS V*Cos(Angle)
                ANGLE = ARAD(IIS)
                CA = COS(ANGLE)
                SA = SIN(ANGLE)
                ACA = ABS(CA)
                ASA = ABS(SA)
                VELNOD(N,1) = VELNOD(N,1) + VFACE1 * CA * ACA
                VELNOD(JJ,1) = VELNOD(JJ,1) + VFACE2 * CA * ACA
C5---------------Y-DIRECTION COMPONENT IS V*Sin(Angle)
                VELNOD(N,2) = VELNOD(N,2) + VFACE1 * SA * ASA
                VELNOD(JJ,2) = VELNOD(JJ,2) + VFACE2* SA * ASA
C6---------------ADD UP THE DIRECTIONAL COMPONENTS FOR AVERAGING
                XCS(N) = XCS(N) + ACA
                XCS(JJ) = XCS(JJ) + ACA
                YCS(N) = YCS(N) + ASA
                YCS(JJ) = YCS(JJ) + ASA
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C7-------COMPUTE THE AVERAGE VELOCITY FOR THE NODE
      DO N=1,NODES
        IF(XCS(N).GT.0.0)VELNOD(N,1) = VELNOD(N,1)/ XCS(N)
        IF(YCS(N).GT.0.0)VELNOD(N,2) = VELNOD(N,2)/ YCS(N)
        IF(ZCS(N).GT.0.0)VELNOD(N,3) = VELNOD(N,3)/ ZCS(N)
      ENDDO
      DEALLOCATE(XCS)
      DEALLOCATE(YCS)
      DEALLOCATE(ZCS)
C------------------------------------------------------------------------------------
C8-----VELOCITIES FOR CONDUIT DOMAIN NODES - 
C8----- VELNOD(1) CONTAINS CONDUIT-CONDUIT AND VELNOD(2) CONTAINS CONDUIT-MATRIX
      IF(INCLN.EQ.0) GO TO 100
      ALLOCATE(XCS(NCLNNDS))
      XCS = 0.0
ccsp      VELNOD = 0.0
C9-----LOOP OVER ALL ACTIVE CLN NODES AND COMPUTE VELOCITY AS AN AVERAGE FROM EACH FACE FLUX
      DO ICLN=1,NCLNNDS
        N = NODES + ICLN
        IF(ICBUND(N).EQ.0) CYCLE
C10-------GO OVER UPPER CONNECTIONS OF NODE N AND FILL Vx, Vy, Vz IN BOTH
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IF(JJ.GT.N.AND.ICBUND(JJ).NE.0)THEN
            IIS = JAS(II)
            IF(IVC(IIS).EQ.3)THEN !CONDUIT-CONDUIT CONNECTION (CAN BE MULTIPLE)
              VFACE = CBCF(IIS)/FAHL(IIS)
              VELNOD(N,1) = VELNOD(N,1) + VFACE
              VELNOD(JJ,1) = VELNOD(JJ,1) + VFACE
              XCS(N-NODES) = XCS(N-NODES) + 1
              XCS(JJ-NODES) = XCS(JJ-NODES) + 1
            ELSEIF(IVC(IIS).EQ.4)THEN   !CONDUIT-MATRIX CONNECTION (ONLY ONE PER CONDUIT NODE)
              VFACE = CBCF(IIS)/FAHL(IIS)
              VELNOD(N,2) = VELNOD(N,2) + VFACE
              VELNOD(JJ,2) = VELNOD(JJ,2) + VFACE
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C11-------COMPUTE THE AVERAGE VELOCITY FOR THE NODES
      DO ICLN=1,NCLNNDS
        N = NODES + ICLN
        IF(XCS(N-NODES).GT.0.0)VELNOD(N,1) = VELNOD(N,1)/ XCS(N-NODES)
      ENDDO
      DEALLOCATE(XCS)
C------------------------------------------------------------------------------------
100   CONTINUE
C
C12-----RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1DSP(N)
C     ******************************************************************
C     FORMULATE PRINCIPAL DISPERSIVE TERM FOR EACH NODE IMPLICITLY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,IVC,ISYM,
     1     IA,JA,JAS,ARAD,NJA,NODES,FAHL,TOP,BOT,CL1,CL2,Sn,NEQS
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,ICBUND,ITVD,ADMAT,
     *  VELNOD,DLX,ATXY,DLY,ATYZ,DLZ,ATXZ,PRSITY,DIFFNC
      DOUBLE PRECISION VX,VY,VZ,DXX,DYY,DZZ,DISP,AREA,THIK1,THIK2,SATN,
     *  SATJJ,SATC,DIFF,DXX2,DYY2,DZZ2,DXX1,DYY1,DZZ1,RL1,RL2
      REAL ANGLE,ACA,ASA
C     ------------------------------------------------------------------
C1-----COMPUTE Dxx, Dyy AND Dzz FOR THE CELL N
      VX = VELNOD(N,1)
      VY = VELNOD(N,2)
      VZ = VELNOD(N,3)
      VTOT = SQRT (VX*VX + VY*VY + VZ*VZ)
      SATN = Sn(N)
C
      DXX1 = 0.0
      DYY1 = 0.0
      DZZ1 = 0.0
      IF(VTOT.GT.0.0)THEN
        IF(N.LT.NODES) THEN ! MATRIX BLOCKS
          DXX1 = (DLX(N)*VX*VX + ATXY(N)*VY*VY + ATXZ(N)*VZ*VZ) / VTOT
          DYY1 = (ATXY(N)*VX*VX + DLY(N)*VY*VY + ATYZ(N)*VZ*VZ) / VTOT
          DZZ1 = (ATXZ(N)*VX*VX + ATYZ(N)*VY*VY + DLZ(N)*VZ*VZ) / VTOT
        ELSE      ! CONDUIT CONNECTIONS 
          DXX1 = DLX(N)*VX*VX / VTOT
          DYY1 = ATXY(N)*VY*VY / VTOT
        ENDIF
      ENDIF
C2-----LOOP OVER CONNECTIONS OF NODE N AND FILL
      DO II = IA(N)+1,IA(N+1)-1
        JJ = JA(II)
        IF(JJ.LE.N) CYCLE !ONLY FOR UPPER TRIANGLE
        IF(ICBUND(JJ).NE.0)THEN
          IIS = JAS(II)
C3-----COMPUTE Dxx, Dyy AND Dzz FOR THE CELL JJ
          VX = VELNOD(JJ,1)
          VY = VELNOD(JJ,2)
          VZ = VELNOD(JJ,3)
          VTOT = SQRT (VX*VX + VY*VY + VZ*VZ)
          SATN = Sn(N)
C
          DXX2 = 0.0
          DYY2 = 0.0
          DZZ2 = 0.0
          IF(VTOT.GT.0.0)THEN
            IF(JJ.LT.NODES) THEN ! MATRIX BLOCKS
              DXX2 =(DLX(JJ)*VX*VX +ATXY(JJ)*VY*VY +ATXZ(JJ)*VZ*VZ)/VTOT
              DYY2 =(ATXY(JJ)*VX*VX +DLY(JJ)*VY*VY +ATYZ(JJ)*VZ*VZ)/VTOT
              DZZ2 =(ATXZ(JJ)*VX*VX +ATYZ(JJ)*VY*VY +DLZ(JJ)*VZ*VZ)/VTOT
            ELSE      ! CONDUIT CONNECTIONS 
              DXX2 = DLX(JJ)*VX*VX / VTOT
              DYY2 = ATXY(JJ)*VY*VY / VTOT
            ENDIF
          ENDIF
C4-----COMPUTE Dxx, Dyy AND Dzz FOR FACE BETWEEN CELLS N AND JJ
          DXX = (DXX1 + DXX2)*0.5
          DYY = (DYY1 + DYY2)*0.5
          DZZ = (DZZ1 + DZZ2)*0.5
C5-----COMPUTE THE DISPERSION CONDUCTANCE TERM BETWEEN CELLS N AND JJ
          IF(IVC(IIS).EQ.1)THEN !VERTICAL DIRECTION CONNECTION
            AREA = FAHL(IIS)
            DISP = DZZ
          ELSEIF(JJ.GT.NODES.OR.N.GT.NODES)THEN !CONDUIT-MATRIX OR CONDUIT-CONDUIT CONNECTION
            AREA = FAHL(IIS)
            IF(N.GT.NODES.AND.JJ.GT.NODES)THEN
              DISP = DXX  !CONDUIT-CONDUIT
            ELSE
              DISP = DYY   !CONDUIT-MATRIX
            ENDIF
          ELSE                                 !HORIZONTAL DIRECTION CONNECTION
            THIK1 = (TOP(N) - BOT(N))
            THIK2 = (TOP(JJ) - BOT(JJ))
            ANGLE = ARAD(IIS)
            AREA =  (THIK1 + THIK2)*0.5 * FAHL(IIS)
C6-------------FOR FACE OF LAYER WITH STACKED LAYERS AREA IS MINIMUM THICKNESS
            ACA = ABS(COS(ANGLE))
            ASA = ABS(SIN(ANGLE))
            DISP = DXX * ACA + DYY*ASA
          ENDIF
C
C7---------ADD DIFFUSION TERM
          SATC = 0.0
          IF(SATN.GT.1.0E-15)THEN
            SATJJ = Sn(JJ)
            SATC = 2.0 * SATJJ * SATN / (SATJJ + SATN)
          ENDIF
          DIFF = SATC * DIFFNC
          IF(N.LE.NODES) DIFF = DIFF * PRSITY(N)
C
          DISP = (DISP + DIFF)
C
C8---------CONVERT DISPERSION  TO DISPERSION CONDUCTANCE
          DISP = DISP * AREA / (CL1(IIS) + CL2(IIS))
C
C9---------ADD TERM TO MATRIX FOR THIS NODE AND ITS EFFECT ON CONNECTING NODE JJ
          ADMAT(II) = ADMAT(II) + DISP
          ADMAT(IA(N)) = ADMAT(IA(N)) - DISP
          ADMAT(ISYM(II)) = ADMAT(ISYM(II)) + DISP
          ADMAT(IA(JJ)) = ADMAT(IA(JJ)) - DISP
        ENDIF
      ENDDO
C
C10-----RETURN
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1DC(ICOMP)
C     ******************************************************************
C     COMPUTE CONCENTRATION GRADIENTS IN X-, Y-, AND Z-DIRECTIONS
C     FOR RHS CROSS-DISPERSION COMPUTATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,IVC,ISYM,
     1    ARAD,AMAT,RHS,ISSFLG,IA,JA,JAS,NJA,NODES,FAHL,BOT,NEQS,INCLN,
     1    CL1,CL2
      USE CLN1MODULE, ONLY: NCLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,ICBUND,PRSITY,DXCS,DYCS,DZCS
      REAL, DIMENSION(:),    ALLOCATABLE  ::XCS, YCS, ZCS
      REAL ANGLE,CA,SA,ACA,ASA,DCDL
C     ------------------------------------------------------------------
C1-----VELOCITIES FOR POROUS MATRIX GRID-BLOCKS
      ALLOCATE(XCS(NODES))
      ALLOCATE(YCS(NODES))
      ALLOCATE(ZCS(NODES))
      DO N=1,NODES
        XCS(N) = 0.0
        YCS(N) = 0.0
        ZCS(N) = 0.0
        DXCS(N) = 0.0
        DYCS(N) = 0.0
        DZCS(N) = 0.0
      ENDDO
C2-----LOOP OVER ALL ACTIVE NODES AND COMPUTE VELOCITY AS AN AVERAGE FROM EACH FACE FLUX
      DO N=1,NODES
        IF(ICBUND(N).EQ.0) CYCLE
C3-------GO OVER UPPER CONNECTIONS OF NODE N AND FILL Vx, Vy, Vz IN BOTH
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IF(JJ.GT.N.AND.JJ.LE.NODES)THEN
            IIS = JAS(II)
            EL1 = CL1(IIS)
            EL2 = CL2(IIS)
            DCDL = (CONC(JJ,ICOMP) - CONC(N,ICOMP))/(EL1 + EL2)
            IIS = JAS(II)
            IF(ICBUND(JJ).NE.0)THEN
              IF(IVC(IIS).EQ.1)THEN !VERTICAL DIRECTION CONNECTION
                DZCS(N) = DZCS(N) + DCDL
                DZCS(JJ) = DZCS(JJ) + DCDL
                ZCS(N) = ZCS(N) + 1
                ZCS(JJ) = ZCS(JJ) + 1
              ELSE                 !HORIZONTAL DIRECTION CONNECTION
C4---------------X-DIRECTION COMPONENT IS V*Cos(Angle)
                ANGLE = ARAD(IIS)
                CA = COS(ANGLE)
                SA = SIN(ANGLE)
                ACA = ABS(CA)
                ASA = ABS(SA)
                DXCS(N) = DXCS(N) + DCDL * CA * ACA
                DXCS(JJ) = DXCS(JJ) + DCDL * CA * ACA
C5---------------Y-DIRECTION COMPONENT IS V*Sin(Angle)
                DYCS(N) = DYCS(N) + DCDL * SA * ASA
                DYCS(JJ) = DYCS(JJ) + DCDL* SA * ASA
C6---------------ADD UP THE DIRECTIONAL COMPONENTS FOR AVERAGING
                XCS(N) = XCS(N) + ACA
                XCS(JJ) = XCS(JJ) + ACA
                YCS(N) = YCS(N) + ASA
                YCS(JJ) = YCS(JJ) + ASA
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C7-------COMPUTE THE AVERAGE GRADIENTS DCDX, DCDY, DCDZ FOR THE NODE
      DO N=1,NODES
        IF(XCS(N).GT.0.0)DXCS(N) = DXCS(N)/ XCS(N)
        IF(YCS(N).GT.0.0)DYCS(N) = DYCS(N)/ YCS(N)
        IF(ZCS(N).GT.0.0)DZCS(N) = DZCS(N)/ ZCS(N)
      ENDDO
      DEALLOCATE(XCS)
      DEALLOCATE(YCS)
      DEALLOCATE(ZCS)
C
C8------RETURN
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE GWT2BCT1DSPX(N)
C     ******************************************************************
C     FORMULATE CROSS-DISPERSIVE TERM FOR EACH NODE ON RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,IVC,ISYM,
     1     RHS,IA,JA,JAS,NJA,ARAD,NODES,FAHL,TOP,BOT,CL1,CL2,Sn
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CBCF,ICBUND,ITVD,
     *  VELNOD,DLX,ATXY,DLY,ATYZ,DLZ,ATXZ,PRSITY,DIFFNC,DXCS,DYCS,DZCS,
     1  ALXY,ALYZ,ALXZ
      DOUBLE PRECISION VX,VY,VZ,DXY1,DYZ1,DXZ1,DYX1,DZY1,DZX1,AREA,
     *  THIK1,THIK2,SATN,SATJJ,SATC,DCDX1,DCDY1,DCDZ1,DCDX2,DCDY2,DCDZ2,
     *  DZ,DL,TERM1,TERM2,SA,CA,EL,DXY2,DYZ2,DXZ2,DYX2,DZY2,DZX2
      REAL ANGLE,ACA,ASA
      REAL, DIMENSION(:), ALLOCATABLE  ::XCS, YCS, ZCS

C     ------------------------------------------------------------------
C1-----COMPUTE Dxy, Dyz AND Dxz FOR THE CELL
      VX = (VELNOD(N,1))
      VY = (VELNOD(N,2))
      VZ = (VELNOD(N,3))
      VTOT = SQRT (VX*VX + VY*VY + VZ*VZ)
C
      DXY1 = 0.0
      DYZ1 = 0.0
      DXZ1 = 0.0
      IF(VTOT.GT.0.0)THEN
        DXY1 = (ALXY(N) - ATXY(N))*VX*VY / VTOT
        DYZ1 = (ALYZ(N) - ATYZ(N))*VY*VZ / VTOT
        DXZ1 = (ALXZ(N) - ATXZ(N))*VX*VZ / VTOT
        
      ENDIF
C1A-------Dyx, Dzy, AND Dzx ARE THE SYMMETRIC TERMS
      DYX1 = DXY1
      DZY1 = DYZ1
      DZX1 = DXZ1
C------------------------------------------------------------------
C2-------FILL DCDX, DCDY AND DCDZ, AND THICKNESS FOR EACH CELL
      DCDX1 = DXCS(N)
      DCDY1 = DYCS(N)
      DCDZ1 = DZCS(N)
      THIK1 = TOP(N) - BOT(N)
C
C3-----GO OVER ALL CONNECTIONS OF CELL N AND FILL
      DO II = IA(N)+1,IA(N+1)-1
        JJ = JA(II)
C3A-----ONLY GO OVER UPPER TRIANGLE OF MATRIX (FORWARD FACES)
C3A-----ALSO NO CROSS DISPERSION COMPONENT ON CLN CELLS        
        IF(JJ.LT.N.OR.JJ.GT.NODES) CYCLE
        IF(ICBUND(JJ).NE.0)THEN
          IIS = JAS(II)
C------------------------------------------------------------------
C4-------COMPUTE Dxy, Dyz AND Dxz FOR THE NEIGHBORING CELL
          VX = (VELNOD(JJ,1))
          VY = (VELNOD(JJ,2))
          VZ = (VELNOD(JJ,3))
          VTOT = SQRT (VX*VX + VY*VY + VZ*VZ)
C
          DXY2 = 0.0
          DYZ2 = 0.0
          DXZ2 = 0.0
          IF(VTOT.GT.0.0)THEN
            DXY2 = (ALXY(JJ) - ATXY(JJ))*VX*VY / VTOT
            DYZ2 = (ALYZ(JJ) - ATYZ(JJ))*VY*VZ / VTOT
            DXZ2 = (ALXZ(JJ) - ATXZ(JJ))*VX*VZ / VTOT      
          ENDIF
C4A-------Dyx, Dzy, AND Dzx ARE THE SYMMETRIC TERMS
          DYX2 = DXY2
          DZY2 = DYZ2
          DZX2 = DXZ2
C------------------------------------------------------------------
C5-------FILL DCDX, DCDY AND DCDZ, AND THICKNESS FOR NEIGHBORING CELL
          DCDX2 = DXCS(JJ)
          DCDY2 = DYCS(JJ)
          DCDZ2 = DZCS(JJ)
          THIK2 = TOP(JJ) - BOT(JJ)
C------------------------------------------------------------------
C6------COMPUTE DISPERSION TERM IN VERTICAL DIRECTION
          IF(IVC(IIS).EQ.1)THEN !VERTICAL DIRECTION CONNECTION
C------------------------------------------------------------------
C6A------d/dz (Dzx dC/dx)
             DZX = (DZX1 + DZX2)*0.5
             DCDX = (DCDX1 + DCDX2) * 0.5
             DISP = DZX * DCDX
C------------------------------------------------------------------
C6B------d/dz (Dzy dC/dy)
             DZY = (DZY1 + DZY2)*0.5
             DCDY = (DCDY1 + DCDY2) * 0.5
             DISP = DISP + DZY * DCDY
C
C6C------CONVERT DISPERSION  TO DISPERSION CONDUCTANCE
            AREA = FAHL(IIS)
            DISP = DISP * AREA
C------------------------------------------------------------------
C7------COMPUTE DISPERSION TERM IN HORIZONTAL DIRECTION
          ELSE                  !HORIZONTAL DIRECTION CONNECTION
C------------------------------------------------------------------
C7A------d/dx (Dxy dC/dy)
            ANGLE = ARAD(IIS)
            CA = ABS(COS(ANGLE))
            SA = ABS(SIN (ANGLE))
C
             DXY = (DXY1 + DXY2)*0.5
             DCDY = (DCDY1 + DCDY2) * 0.5
             DISP = DXY * DCDY * CA
C------------------------------------------------------------------
C7B------d/dy (Dyx dC/dx)
             DYX = (DYX1 + DYX2)*0.5
             DCDX = (DCDX1 + DCDX2) * 0.5
             DISP = DISP + DYX * DCDX * SA
C7C------d/dx (Dxz dC/dz)
             DXZ = (DXZ1 + DXZ2)*0.5
             DCDZ = (DCDZ1 + DCDZ2) * 0.5
             DISP = DISP + DXZ * DCDZ * CA
C------------------------------------------------------------------
C7D------d/dy (Dyz dC/dz)
             DYZ = (DYZ1 + DYZ2)*0.5
             DCDZ = (DCDZ1 + DCDZ2) * 0.5
             DISP = DISP + DYZ * DCDZ * SA
C
C7E------CONVERT DISPERSION  TO DISPERSION CONDUCTANCE
            THIK1 = (TOP(N) - BOT(N))
            THIK2 = (TOP(JJ) - BOT(JJ))
            AREA =  (THIK1 + THIK2)*0.5 * FAHL(IIS)
            DISP = DISP * AREA  
          ENDIF
C8---------ADD TERM TO RHS VECTOR FOR THIS NODE AND ITS EFFECT ON CONNECTING NODE JJ
          RHS(N) = RHS(N) - DISP
          RHS(JJ) = RHS(JJ) + DISP
        ENDIF
      ENDDO
C
C9------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1ADV(N)
C     ******************************************************************
C     FORMULATE ADVECTIVE TERM FOR EACH NODE AND COMPONENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1     AMAT,RHS,ISSFLG,IA,JA,JAS,NJA,NODES
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CBCF,ICBUND,ITVD,ADMAT
      DOUBLE PRECISION QIJ
C     ------------------------------------------------------------------
C1-----GO OVER CONNECTIONS OF NODE N AND FILL
      DO II = IA(N)+1,IA(N+1)-1
        JJ = JA(II)
        IF(JJ.GT.N.AND.ICBUND(JJ).NE.0)THEN
        IIS = JAS(II)
C----------------------------------------------------------------------
C2---------FILL UPSTREAM TERM
          QIJ = CBCF(IIS)
          IF(QIJ.GT.0)THEN !N IS UPSTREAM OF CELLS N AND JJ
            ADMAT(IA(N)) = ADMAT(IA(N)) - QIJ
            ADMAT(ISYM(II)) = ADMAT(ISYM(II)) + QIJ
          ELSE ! JJ IS UPSTREAM OF CELLS N AND JJ
            ADMAT(II) = ADMAT(II) - QIJ
            ADMAT(IA(JJ)) = ADMAT(IA(JJ)) + QIJ
          ENDIF
C-----------------------------------------------------------------------
        ENDIF
      ENDDO
C
C3------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1TVD(N,ICOMP)
C     ******************************************************************
C     FILL TVD TERM ON RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1     RHS,ISSFLG,IA,JA,JAS,NJA,NODES
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CBCF,ICBUND,ITVD
      DOUBLE PRECISION QIJ
C     ------------------------------------------------------------------
C1-----GO OVER CONNECTIONS OF NODE N AND FILL
      DO II = IA(N)+1,IA(N+1)-1
        JJ = JA(II)
        IF(JJ.GT.N.AND.ICBUND(JJ).NE.0)THEN
          IIS = JAS(II)
          QIJ = CBCF(IIS)
          CALL SGWT2BCT1TVDS(N,ICOMP,II,JJ,QIJ)
        ENDIF
      ENDDO
C
C2------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SGWT2BCT1TVDS(N,ICOMP,II,JJ,QIJ)
C     ******************************************************************
C     FORMULATE TVD TERM FOR EACH NODE AND COMPONENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1     AMAT,RHS,ISSFLG,IA,JA,JAS,NJA,NODES,CL1,CL2
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CBCF,ICBUND,ITVD
      DOUBLE PRECISION QIJ,Q2,ELUPDN,ELUP2UP,ALIMITER,SMOOTH,CDIFF
C1---------------FIND UPSTREAM NODE
      IIS = JAS(II)
      IF(QIJ.GT.0)THEN !N IS UPSTREAM OF CELLS N AND JJ
        IUP = N
        IDN = JJ
      ELSE ! JJ IS UPSTREAM OF CELLS N AND JJ
        IUP = JJ
        IDN = N
      ENDIF
      ELUPDN = (CL1(IIS) + CL2(IIS))
C2-----------FIND SECOND POINT UPSTREAM TO POINT IUP
      I2UP = 0
      Q2 = 0.0
      DO I2 = IA(IUP)+1,IA(IUP+1)-1
        J2 = JA(I2)
        IF(ICBUND(J2).NE.0)THEN
          I2S = JAS(I2)
          IF(J2.GT.IUP)THEN   ! J2 IS HIGHER NODE NUMBER - Q INTO IUP IS NEGATIVE
            IF(-CBCF(I2S).GT.Q2) THEN
              Q2 = -CBCF(I2S)
              I2UP = J2
              ELUP2UP = (CL1(I2S) + CL2(I2S))
            ENDIF
          ELSEIF(IUP.GT.J2)THEN !J2 IS LOWER NODE NUMBER
            IF(CBCF(I2S).GT.Q2) THEN
              Q2 = CBCF(I2S)
              I2UP = J2
              ELUP2UP = (CL1(I2S) + CL2(I2S))
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C3---------------COMPUTE SMOOTHNESS SENSOR AND LIMITER TERM
      IF(I2UP.NE.0)THEN
        SMOOTH = 0.0
        CDIFF = ABS(CONC(IDN,ICOMP) - CONC(IUP,ICOMP))
        IF(CDIFF.GT.1.0E-10)
     *  SMOOTH = (CONC(IUP,ICOMP) - CONC(I2UP,ICOMP)) /ELUP2UP
     *  * ELUPDN / (CONC(IDN,ICOMP) - CONC(IUP,ICOMP))
        ALIMITER = 0.0
        IF(SMOOTH.GT.0) ALIMITER = 2. * SMOOTH / (1. + SMOOTH)
        RHS(N) = RHS(N) + 0.5*ALIMITER*QIJ*
     *  (CONC(IDN,ICOMP)-CONC(IUP,ICOMP))
         RHS(JJ) = RHS(JJ) - 0.5*ALIMITER*QIJ*
     *  (CONC(IDN,ICOMP)-CONC(IUP,ICOMP))
      ENDIF
C
C4------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1STO(N,ICOMP,DTERMS,RTERMS,ISS)

C     ******************************************************************
C     FORMULATE STORAGE TERM FOR EACH NODE AND COMPONENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1 HNEW,AMAT,RHS,HOLD,ISSFLG,IA,JA,NJA,NODES,AREA,BOT,TOP,Sn,So
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CONCO,ICT,IADSORB,PRSITY,
     * ADSORB,FLICH,IZOD,IFOD,ZODRW,FODRW,
     1  ZODRS,FODRS
      DOUBLE PRECISION VODT,ADSTERM,CW,FL,CWO,DT,RT,ALENG,VOLU,
     1  X,Y,EPSS,EPS,CT,CEPS,QA,QEPS,DQ,OMP,DTERMS,RTERMS
C     ------------------------------------------------------------------
C1------INITIALIZE TERMS      
      DTERMS = 0.0
      RTERMS = 0.0
      IF(N.LE.NODES)THEN
        ALENG = TOP(N) - BOT(N)
      ELSE
        ALENG = ACLNNDS(N-NODES,4)
      ENDIF
      VOLU = AREA(N) * ALENG
      VODT = VOLU / DELT
C----------------------------------------------------------------------
      IF(ICT.EQ.0)THEN  !----------WATER PHASE CONCENTRATION FORMULATION
C2-------STORAGE TERMS ON SOIL
        IF(N.LE.NODES.AND.IADSORB.EQ.1)THEN
C---------LINEAR ISOTHERM
          ADSTERM = ADSORB(N,ICOMP) * VODT
          DTERMS = DTERMS - ADSTERM
          RTERMS = RTERMS - ADSTERM * CONCO(N,ICOMP)
C-----------------------------------------------------------------------
        ELSEIF(N.LE.NODES.AND.IADSORB.EQ.2)THEN
C3---------NONLINEAR FREUNDLICH ISOTHERM FILLED AS NEWTON
          ADSTERM = ADSORB(N,ICOMP) * VODT
          FL = FLICH(N,ICOMP)
          CW = CONC(N,ICOMP)
          CWO = CONCO(N,ICOMP)
          DT = 0.0
          RT = 0.0
          IF(CWO.GT.0.0)THEN
            RT = -CWO**FL
          ENDIF
          IF(CW.GT.0.0)THEN
            DT = ADSTERM*FL * CW**(FL - 1.0)
            RT = RT + CW**FL
          ENDIF
          RT = RT * ADSTERM
          DTERMS = DTERMS - DT
          RTERMS = RTERMS - DT * CW + RT
        ENDIF
C-----------------------------------------------------------------------
C4-------STORAGE TERM IN WATER
        CALL GWT2BCT1STOW (N,ICOMP,DTERMS,RTERMS,VODT,VOLU,ALENG,ISS)
C-----------------------------------------------------------------------
      ELSE       !-----------------------TOTAL CONCENTRATION FORMULATION
C5-------NET STORAGE TERM FOR TOTAL CONCENTRATION FORMULATION
        DTERMS = DTERMS - VODT
        RTERMS = RTERMS - VODT * CONCO(N,ICOMP)
      ENDIF
C
C9------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE GWT2BCT1STOW(N,ICOMP,DTERMS,RTERMS,VODT,VOLU,ALENG,ISS)
C     ******************************************************************
C     FORMULATE STORAGE TERM FOR EACH NODE AND COMPONENT IN WATER
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1 HNEW,AMAT,RHS,HOLD,ISSFLG,IA,JA,NJA,NODES,AREA,BOT,TOP,Sn,So,
     2 LAYNOD
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWFBCFMODULE,ONLY:LAYCON,SC1,SC2
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CONCO,ICT,IADSORB,PRSITY,
     * ADSORB,FLICH,IZOD,IFOD,ZODRW,FODRW,
     1  ZODRS,FODRS
      DOUBLE PRECISION VODT,ADSTERM,CW,FL,CWO,DT,RT,ALENG,VOLU,
     1  X,Y,EPSS,EPS,CT,CEPS,QA,QEPS,DQ,OMP,DTERMS,RTERMS,
     1  H1,H2,TP,SATN,SATO
C-------------------------------------------------------------------------------------
C   
      SATN = Sn(N) 
      SATO = So(N)
      IF(ISS.EQ.1.OR.N.GT.NODES) THEN
        IF(ISS.EQ.1) SATO = SATN        
C1-------USE TRADITIONAL STORAGE TERM FOR CLN DOMAIN OR SS FLOW
        IF(N.LE.NODES) VODT = VODT * PRSITY(N)
        DTERMS = DTERMS - VODT * SATN
        RTERMS = RTERMS - VODT * SATO * CONCO(N,ICOMP)
      ELSE
        K = LAYNOD(N)
        LC=LAYCON(K)
        IF(LC.EQ.4.OR.LC.EQ.5)THEN !
C2---------TWO STORAGES FOR UPSTREAM WEIGHTING FORMULATION
          DTERMS = DTERMS -
     *      ((PRSITY(N)*VOLU-SC2(N)*ALENG) + ALENG*SC2(N)*SATN +
     *      SATN*SC1(N)*HNEW(N)) / DELT
          RTERMS = RTERMS -
     *      ((PRSITY(N)*VOLU-SC2(N)*ALENG) + ALENG*SC2(N)*SATO +
     *      SATN*SC1(N)*HOLD(N)) * CONCO(N,ICOMP) / DELT
        ELSEIF(LC.EQ.0.OR.LC.EQ.2) THEN
C3---------ONE CONFINED STORAGE CAPACITY FOR FLOW
            DTERMS = DTERMS - (PRSITY(N)*VOLU + SC1(N)*HNEW(N)) / DELT
            RTERMS = RTERMS - (PRSITY(N)*VOLU + SC1(N)*HOLD(N)) / DELT
     *       * CONCO(N,ICOMP)
        ELSEIF(LC.EQ.1) THEN ! IN BCF, IF LAYCON = 1, THE SPECIRFIC YIELD IS READ IN SC2 ?
C4---------ONE UNCONFINED STORAGE CAPACITY FOR FLOW
          DTERMS = DTERMS -
     *     ((PRSITY(N)*VOLU-SC2(N)*ALENG) + SC2(N)*ALENG*SATN) / DELT
          RTERMS = RTERMS -
     *     ((PRSITY(N)*VOLU-SC2(N)*ALENG) + SC2(N)*ALENG*SATO) / DELT
     *     * CONCO(N,ICOMP)
        ELSEIF(LC.EQ.3) THEN
C5---------TWO STORAGE CAPACITIES FOR FLOW
          TP=TOP(N)
          IF(HOLD(N).GT.TP.AND.HNEW(N).GT.TP) THEN
C6-----------CELL IS CONFINED
            DTERMS = DTERMS - (PRSITY(N)*VOLU + SC1(N)*HNEW(N)) / DELT
            RTERMS = RTERMS - (PRSITY(N)*VOLU + SC1(N)*HOLD(N)) / DELT
     *       * CONCO(N,ICOMP)
          ELSEIF (HOLD(N).LT.TP.AND.HNEW(N).LT.TP) THEN
C7-----------CELL IS UNCONFINED
            DTERMS = DTERMS -
     *       ((PRSITY(N)*VOLU-SC2(N)*ALENG) + SC2(N)*ALENG*SATN) / DELT
            RTERMS = RTERMS -
     *       ((PRSITY(N)*VOLU-SC2(N)*ALENG) + SC2(N)*ALENG*SATO) / DELT
     *     * CONCO(N,ICOMP)
          ELSE
C8-----------CELL HAS CONVERTED THIS TIME STEP
            IF(HNEW(N).GT.HOLD(N)) THEN
C8A-------------WLEs RAISED - BECOMES CONFINED
              H1 = HNEW(N)
              H2 = TP
            ELSE
C8B-------------WLEs LOWERED - BECOMES UNCONFINED
              H1 = TP
              H2 = HOLD(N)
            ENDIF
            DTERMS = DTERMS - ((PRSITY(N)*VOLU-SC2(N)*ALENG) +
     *       SC2(N)*ALENG*SATN + SC1(N)*H1) / DELT
            RTERMS = RTERMS - ((PRSITY(N)*VOLU-SC2(N)*ALENG) +
     *       SC2(N)*ALENG*SATO + SC1(N)*H2) / DELT * CONCO(N,ICOMP)
          ENDIF
        ENDIF
      ENDIF
C
C9------RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1DCY(N,ICOMP,DTERMD,RTERMD)
C     ******************************************************************
C     FORMULATE DECAY TERM FOR EACH NODE AND COMPONENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1 HNEW,AMAT,RHS,HOLD,ISSFLG,IA,JA,NJA,NODES,AREA,BOT,TOP,Sn,So
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CONCO,ICT,IADSORB,PRSITY,
     * ADSORB,FLICH,IZOD,IFOD,ZODRW,FODRW,
     1  ZODRS,FODRS
      DOUBLE PRECISION ADSTERM,CW,FL,CWO,DT,RT,ALENG,VOLU,
     1  X,Y,EPSS,EPS,CT,CEPS,QA,QEPS,DQ,OMP,DTERMD,RTERMD
C     ------------------------------------------------------------------
C1------INITIALIZE TERMS      
      DTERMD = 0.0
      RTERMD = 0.0
      IF(N.LE.NODES)THEN
        ALENG = TOP(N) - BOT(N)
      ELSE
        ALENG = ACLNNDS(N-NODES,4)
      ENDIF
      VOLU = AREA(N) * ALENG
C----------------------------------------------------------------------
      IF(ICT.EQ.0)THEN  !----------WATER PHASE CONCENTRATION FORMULATION
C2-------DECAY TERMS ON SOIL (NO ADSORPTION ON CLN)
        IF(N.LE.NODES)THEN
C3---------ZERO ORDER DECAY ON SOIL - APPLY NEWTON EXPANSION OF SUPPLY-DEMAND FUNCTION
          IF(IZOD.GE.2.AND.IADSORB.GT.0)THEN
            CT = - VOLU * ZODRS(N,ICOMP)
            EPS = 0.01
            CEPS = MAX(0.0,CONC(N,ICOMP))
            X = CEPS /EPS
            CALL SMOOTH(X,Y)
            QA =  CT * Y
C3A-----------CALCULATE DQ/DH
            EPSS = 0.001 * EPS
            CEPS = MAX(0.0,CONC(N,ICOMP)+EPSS)
            X = (CEPS)/EPS
            CALL SMOOTH(X,Y)
            QEPS = CT * Y
            DQ = (QEPS - QA) / EPSS
            DTERMD = DTERMD + DQ
            RTERMD = RTERMD - QA + DQ*CONC(N,ICOMP)
          ENDIF
C
C4---------FIRST ORDER DECAY ON SOIL
          IF(IFOD.GE.2.AND.IADSORB.GT.0)THEN
            CT = -ADSORB(N,ICOMP) * VOLU * FODRS(N,ICOMP)
            IF(IADSORB.EQ.1)THEN
C5--------------FOR LINEAR ADSORPTION
              DTERMD = DTERMD + CT
            ELSE
C6--------------FOR NON-LINEAR ADSORPTION FILL AS NEWTON
              ETA = FLICH(N,ICOMP)
              QA =  CT * CONC(N,ICOMP) ** ETA
C6A--------------CALCULATE DQ/DH
C              EPSS = 0.00001
C              CEPS = CONC(N,ICOMP)+EPSS
C              QEPS = CT * CEPS ** ETA
C              DQ = (QEPS - QA) / EPSS
CC              DQ = CT * ETA * CONC(N,ICOMP) ** (ETA-1.0)
CC              DTERMD = DTERMD + DQ
CC              RTERMD = RTERMD - QA + DQ*CONC(N,ICOMP)
              DTERMD = DTERMD + CT * CONC(N,ICOMP) **(ETA - 1.0)
            ENDIF
          ENDIF
        ENDIF
C-----------------------------------------------------------------------
C7---------ZERO ORDER DECAY IN WATER - APPLY NEWTON EXPANSION OF SUPPLY-DEMAND FUNCTION
        IF(IZOD.EQ.1.OR.IZOD.EQ.3)THEN
          CT = -Sn(N)* VOLU * ZODRW(N,ICOMP)
          EPS = 0.01
          CEPS = MAX(0.0,CONC(N,ICOMP))
          X = CEPS /EPS
          CALL SMOOTH(X,Y)
          QA =  CT * Y
C7A-----------CALCULATE DQ/DH
          EPSS = 0.001 * EPS
          CEPS = MAX(0.0,CONC(N,ICOMP)+EPSS)
          X = (CEPS)/EPS
          CALL SMOOTH(X,Y)
          QEPS = CT * Y
          DQ = (QEPS - QA) / EPSS
          DTERMD = DTERMD + DQ
          RTERMD = RTERMD - QA + DQ*CONC(N,ICOMP)
        ENDIF
C
C8---------FIRST ORDER DECAY IN WATER
        IF(IFOD.EQ.1.OR.IFOD.EQ.3)THEN
          CT = -Sn(N)* VOLU * FODRW(N,ICOMP)
          DTERMD = DTERMD + CT
        ENDIF
      ELSE       !-----------------------TOTAL CONCENTRATION FORMULATION
C
C-------NET DECAY TERM FOR TOTAL CONCENTRATION FORMULATION
CSP TO DO
      ENDIF
C
C9------RETURN
      RETURN
      END
C--------------------------------------------------------------------------
      SUBROUTINE GWT2SOLVE(ITITER,ICNVG,ICOMP,KSTP,KPER,ITP,IN_ITER)
C     ******************************************************************
C     SOLVE THE GROUNDWATER TRANSPORT EQUATION FOR THIS COMPONENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY: IOUT,NEQS,IBOUND,AMAT,RHS,IA,NJA
      USE SMSMODULE,ONLY:HTEMP,LINMETH
      USE XMDMODULE,ONLY:IACL     
      USE PCGUMODULE, ONLY: ILINMETH
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,ICBUND,CICLOSE,
     1 CINACT,CNCG,LRCC,MSUMT,FCNCG,FLRCC
      DOUBLE PRECISION BIG,ADIAG,ROD    
C     ------------------------------------------------------------------      
C1-----TAKE CARE OF LOOSE ENDS FOR ALL NODES BEFORE CALL TO SOLVER
          BIG = 1.0E20
          DO N=1,NEQS
C2---------TAKE CARE OF ZERO ROW DIAGONAL
            ADIAG = ABS(AMAT(IA(N)))
            IF(ADIAG.LT.1.0E-06)THEN
              AMAT(IA(N)) = 1.0E08
              RHS(N) = CONC(N,ICOMP)*1.0E08       !   + RHS(N) 
            ENDIF
C2B--------TAKE CARE OF ZERO OFFDIAGONALS (NO INFLOW) BUT LARGE RHS/DIAGONAL
c            IOFFDIAG = 0
c            DO JJ = IA(N)+1,IA(N+1)-1
c              IF (ABS(AMAT(JJ)).GT.1.0E-6) THEN
c                IOFFDIAG = 1
c                GO TO 11
c              ENDIF
c            ENDDO
c11          CONTINUE
c            ROD = ABS(RHS(N) / AMAT(IA(N)))
c            IF(IOFFDIAG.EQ.0. AND. ROD. GT.1.0E6) THEN
c              AMAT(IA(N)) = 1.0E00
c              RHS(N) = CONC(N,ICOMP) 
c            ENDIF    
          ENDDO
C
C3 --------STORE CONC IN TEMP LOCATION
          DO N=1,NEQS
            HTEMP(N) = CONC(N,ICOMP)
          ENDDO
C4 ---------ENSURE SOLVER IS UNSYMMETRIC
          IF(LINMETH.EQ.1)THEN    ! XMD SOLVER
            IACLTMP = -1  
            IF(IACL.EQ.0)THEN
              IACLTMP = IACL 
              IACL = 1      
            ENDIF
          ELSE                    ! PCGU SOLVER 
            ILINMETHTMP = -1  
            IF(ILINMETH.EQ.1)THEN  
              ILINMETHTMP = ILINMETH
              ILINMETH = 2 
            ENDIF  
          ENDIF
C5 ---------EVALUATE CONDITIONS WHEN ILU NEED NOT BE PERFORMED
          ILUFLAG = 1
cccc          IF(ITITER.GT.1) ILUFLAG = 0
cccc          IF(IADMATF.EQ.1.AND.DABS(DELT - DELTO).LT.1.0E-15) ILUFLAG = 0
C6---------CALL LINEAR SOLVER ROUTINES
          CALL SOLVERS(IOUT,ITITER,ICNVG,KSTP,KPER,AMAT,CONC(1,ICOMP),
     *      RHS,ICBUND,CICLOSE,CINACT,ITP,NEQS,NJA,ILUFLAG,IN_ITER)
C
C7--------RESET SOLVER TO SYMMETRIC IF CHANGED          
         IF(LINMETH.EQ.1)THEN    ! XMD SOLVER
            IF(IACLTMP.EQ.0)THEN
              IACL = 0      
            ENDIF
          ELSE                    ! PCGU SOLVER 
            IF(ILINMETHTMP.EQ.1)THEN  
              ILINMETH = 1 
            ENDIF  
          ENDIF         
C
C9------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE CONCCONV(ICOMP,ITITER,ICNVG)
C     ******************************************************************
C     COMPUTE MAX CONCENTRATION CHANGES AND CHECK CONVERGENCE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IUNSTR,NODES,NEQS,NLAY,NROW,NCOL,INCLN,
     1  IBOUND
      USE CLN1MODULE, ONLY: NCLNNDS
      USE SMSMODULE,ONLY:HTEMP
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,ICBUND,CICLOSE,
     1 CINACT,CNCG,LRCC,MSUMT,FCNCG,FLRCC
      DOUBLE PRECISION BIGCC,ABIGCC,CDIF,ACDIF,BIGCL,ABIGCL
C     ------------------------------------------------------------------
C
C1---------STORE LARGEST CONC CHANGE FOR ITERATION
          NB=1
          ICNVG=0
          BIGCC=0.0
          ABIGCC=0.0
          DO N=1,NODES
            IF(ICBUND(N).EQ.0) CYCLE
            CDIF=CONC(N,ICOMP)-HTEMP(N)
            ACDIF=ABS(CDIF)
            IF(ACDIF.GE.ABIGCC)THEN
              BIGCC= CDIF
              ABIGCC= ACDIF
              NB = N
            ENDIF
          ENDDO
          IF(ABIGCC.LE.CICLOSE) ICNVG=1
C
C2---------STORE MAXIMUM CHANGE VALUE AND LOCATION
          CNCG(ITITER) = BIGCC
C
          IF(IUNSTR.EQ.0)THEN !GET LAYER, ROW AND COLUMN FOR STRUCTURED GRID
            KLAYER = (NB-1) / (NCOL*NROW) + 1
            IJ = NB - (KLAYER-1)*NCOL*NROW
            IROW = (IJ-1)/NCOL + 1
            JCOLMN = IJ - (IROW-1)*NCOL
            LRCC(1,ITITER) = KLAYER
            LRCC(2,ITITER) = IROW
            LRCC(3,ITITER) = JCOLMN
          ELSE
            LRCC(1,ITITER) = NB
          ENDIF
C
C3---------CHECK OUTER ITERATION CONVERGENCE FOR CONDUIT-NODES
          IF(INCLN.EQ.0) GO TO 204
          NB=1
          ICNVGL=0
          BIGCCL=0.0
          ABIGCCL=0.0
          DO N=NODES+1,NODES+NCLNNDS
            IF(IBOUND(N).EQ.0) CYCLE
            CDIF=CONC(N,ICOMP)-HTEMP(N)
            ACDIF=ABS(CDIF)
            IF(ACDIF.GE.ABIGCCL)THEN
              BIGCCL= CDIF
              ABIGCCL= ACDIF
              NB = N
            ENDIF
          ENDDO
C
          IF(ABIGCCL.LE.CICLOSE) ICNVGL=1
C
C4---------STORE MAXIMUM CHANGE VALUE AND LOCATION
          FCNCG(ITITER) = BIGCCL
          FLRCC(ITITER) = NB - NODES
C5---------NOT CONVERGED, IF EITHER IS NOT CONVERGED
          IF(ICNVG.EQ.0.OR.ICNVGL.EQ.0) ICNVG = 0
204       CONTINUE
C
C6------RETURN
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GWT2BCT1OC(KSTP,KPER,ISA)
C     ******************************************************************
C     SET PRINT FLAGS FOR TRANSPORT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:ITMUNI,IOUT,IUNSTR,ISSFLG,PERLEN
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,ISPCFL,IBUDFL,IATS,TMINAT,
     1  NPTIMES,NPSTPS,ITIMOT,TIMOT,ISPCFLAT
      USE GWTBCTMODULE, ONLY: MCOMP,VBVLT,VBNMT,MSUMT
C     ------------------------------------------------------------------
C
C1------DETERMINE IF TRANSPORT PRINTING IS NEEDED AT THIS TIME
      IF(ISSFLG(KPER).EQ.1 .AND. IATS.NE.0)THEN
C      IF(IATS.NE.0)THEN
        IATSPR = 0
C1B-----PRINT EVERY NPSTPS TIMES
        IF (MOD(KSTP,NPSTPS).EQ.0) IATSPR = 1
C1C-----PRINT AT END OF STRESS PERIOD
        IF(ABS(PERTIM-PERLEN(KPER)).LT.TMINAT) IATSPR = 1
C1D-----PRINT WHEN OUTPUT TIME IS REACHED
        IF(NPTIMES.GT.0)THEN
          IF(TOTIM.GE.(TIMOT(ITIMOT) - TMINAT))THEN
            IATSPR = 1
            ITIMOT = ITIMOT + 1
          ENDIF
        ENDIF
C2------SET FLAGS IF PRINTING
        ISPCFL = 0
        IF(IATSPR.EQ.1) THEN
          ISPCFL = ISPCFLAT
        ENDIF
      ENDIF
C
C3------RETURN
      RETURN
      END
C---------------------------------------------------------------------------------
      SUBROUTINE GWT2BCT1OT(KSTP,KPER,ISA)
C     ******************************************************************
C     OUTPUT CONCENTRATION FOR EACH SPECIES AND MASS BALANCE SUMMARY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:ITMUNI,IOUT,IUNSTR,ISSFLG,PERLEN
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,ISPCFL,IBUDFL,IATS,TMINAT,
     1  NPTIMES,NPSTPS,ITIMOT,TIMOT,ISPCFLAT
      USE GWTBCTMODULE, ONLY: MCOMP,VBVLT,VBNMT,MSUMT
C     ------------------------------------------------------------------
C
C1------CLEAR PRINTOUT FLAG (IPFLG)
      IPFLG=0
      DO ICOMP = 1,MCOMP
        WRITE(IOUT,1)ICOMP
1       FORMAT(5X,'TRANSPORT SOLUTION COMPLETE FOR COMPONENT SPECIES',
     *    1X,'NUMBER',I5/5X,60('-'))
C
C2------IF CONCENTRATION PRINT FLAG (ISPCFL) IS SET WRITE
C2------CONCENTRATION, AND ICBUND IN ACCORDANCE WITH FLAGS IN IOFLG.
        IF(ISPCFL.EQ.0) GO TO 100
C
        IF(IUNSTR.EQ.0)THEN ! WRITE M2K5 STYLE FOR STRUCTURED GRID
          CALL SGWT2BCT1C(KSTP,KPER,IPFLG,ISA,ICOMP)
          CALL SGWT2BCT1IB(KSTP,KPER)
        ELSE
          CALL SGWT2BCT1CU(KSTP,KPER,IPFLG,ISA,ICOMP)
          CALL SGWT2BCT1IBU(KSTP,KPER)
        ENDIF
        IPFLG = 1
C
  100   CONTINUE

C3------PRINT TOTAL BUDGET IF REQUESTED
      IF(IBUDFL.EQ.0) GO TO 120
        CALL SGWF2BAS7V(MSUMT,VBNMT(1,ICOMP),VBVLT(1,1,ICOMP),
     *  KSTP,KPER,IOUT)
        IPFLG=1
C
C4------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT
C4------WILL BE PRODUCED.
  120   IF(IPFLG.EQ.0) GO TO 99
        CALL SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
        WRITE(IOUT,101)
  101   FORMAT('1')
C
      ENDDO
C
C5------RETURN
99    CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SGWT2BCT1C(KSTP,KPER,IPFLG,ISA,ICOMP)
C     ******************************************************************
C     PRINT AND RECORD CONCS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,NODLAY,
     1                      IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,ISPCFM,ISPCUN,LBHDSV,
     2                      CSPCFM,IOFLG
      USE GWTBCTMODULE, ONLY: ICBUND,CONC
C
      REAL,          SAVE,    DIMENSION(:,:,:),    ALLOCATABLE ::BUFF
      CHARACTER*16 TEXT
      DATA TEXT /'            CONC'/
C     ------------------------------------------------------------------
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
C
C1------FOR EACH LAYER MOVE HNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS CONC NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,5).EQ.0.AND.IOFLG(KL,6).EQ.0) GO TO 59
C
C3------MOVE CONC TO BUFF FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      N = (K-1)*NROW*NCOL + (I-1)*NCOL + J
      BUFF(J,I,K)=CONC(N,ICOMP)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT CONC.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,5).EQ.0) GO TO 69
           IF(ISPCFM.LT.0) CALL ULAPRS(BUFF(1,1,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,-ISPCFM,IOUT)
           IF(ISPCFM.GE.0) CALL ULAPRW(BUFF(1,1,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,ISPCFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT CONC FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,5).NE.0) THEN
             IF(ISPCFM.LT.0) CALL ULAPRS(BUFF(1,1,1),TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-ISPCFM,IOUT)
             IF(ISPCFM.GE.0) CALL ULAPRW(BUFF(1,1,1),TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,ISPCFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE CONC.
      IFIRST=1
      IF(ISPCUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        NSTRT = NODLAY(K-1)+1
        KK=K
        IF(IOFLG(K,6).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) ISPCUN,KSTP,KPER
   74   FORMAT(1X,/1X,'CONC WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I8,', STRESS PERIOD ',I8)
        IFIRST=0
        IF(CSPCFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,ISPCUN)
        ELSE
           CALL ULASV2(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,ISPCUN,CSPCFM,LBHDSV,ICBUND(NSTRT))
        END IF
   79   CONTINUE
C
C5A-----SAVE CONC FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,6).NE.0) THEN
          WRITE(IOUT,74) ISPCUN,KSTP,KPER
          IF(CSPCFM.EQ.' ') THEN
             CALL ULASAV(BUFF(1,1,1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,ISPCUN)
          ELSE
             CALL ULASV2(BUFF(1,1,1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,ISPCUN,CSPCFM,LBHDSV,ICBUND)
          END IF
        END IF
      END IF
80    CONTINUE
      DEALLOCATE(BUFF)
C
C6------RETURN.
      RETURN
      END
      SUBROUTINE SGWT2BCT1IB(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
      USE GWTBCTMODULE, ONLY:ICBUND
C
      INTEGER,  SAVE,    DIMENSION(:,:,:),    ALLOCATABLE ::ITEMP
      CHARACTER*16 TEXT
      DATA TEXT /'          ICBUND'/
C     ------------------------------------------------------------------
C1------TRANSFER IBOUND TO TEMP LOCATION      
      IF(IBOUUN.LE.0) RETURN
      ALLOCATE (ITEMP(NCOL,NROW,NLAY))
C
      N=0
      DO 58 K=1,NLAY
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      N = N+1
      ITEMP(J,I,K)=ICBUND(N)
   58 CONTINUE
C
C2------FOR EACH LAYER: SAVE ICBUND WHEN REQUESTED.
      IFIRST=1
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,7).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IBOUUN,KSTP,KPER
   74   FORMAT(1X,/1X,'ICBUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I8,', STRESS PERIOD ',I8)
        IFIRST=0
        CALL ULASV3(ITEMP(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IBOUUN,CBOUFM,LBBOSV)
   79   CONTINUE
C
C3-----SAVE IBOUND FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,5).NE.0) THEN
          WRITE(IOUT,74) IBOUUN,KSTP,KPER
          CALL ULASV3(ITEMP(1,1,1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IBOUUN,CBOUFM,LBBOSV)
        END IF
      END IF
C
C4------RETURN.
      DEALLOCATE(ITEMP)
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SGWT2BCT1CU(KSTP,KPER,IPFLG,ISA,ICOMP)
C     ******************************************************************
C     PRINT AND RECORD CONCENTRATIONS FOR UNSTRUCTURED GRIDS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,NODLAY,
     1                      IBOUND,IOUT,NODES,BUFF
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,ISPCFM,ISPCUN,LBHDSV,
     2                      CSPCFM,IOFLG
      USE GWTBCTMODULE,ONLY:ICBUND,CONC
C
      CHARACTER*16 TEXT
      DATA TEXT /'            CONC'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER MOVE HCONC TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS CONC NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,5).EQ.0 .AND. IOFLG(KL,6).EQ.0) GO TO 59
C
C3------MOVE HNEW TO BUFF FOR THE LAYER.
      NNDLAY = NODLAY(K)
      NSTRT = NODLAY(K-1)+1
      DO 58 N=NSTRT,NNDLAY
      BUFF(N)=CONC(N,ICOMP)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRU TO PRINT CONC.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,5).EQ.0) GO TO 69
           NNDLAY = NODLAY(K)
           NSTRT = NODLAY(K-1)+1
           CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,KK,IABS(ISPCFM),IOUT,PERTIM,TOTIM,NODES)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT CONC FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,5).NE.0) THEN
             CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,-1,IABS(ISPCFM),IOUT,PERTIM,TOTIM,NODES)
             IPFLG=1
C
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
      IFIRST=1
      IF(ISPCUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,6).EQ.0) GO TO 79
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        IF(IFIRST.EQ.1) WRITE(IOUT,74) ISPCUN,KSTP,KPER
   74   FORMAT(1X,/1X,'CONC WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I8,', STRESS PERIOD ',I8)
        IFIRST=0
        IF(CSPCFM.EQ.' ') THEN
           CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,KK,ISPCUN,NODES)
        ELSE
           CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1             NNDLAY,KK,ISPCUN,CSPCFM,LBHDSV,ICBUND(NSTRT),NODES)
        END IF
   79   CONTINUE
C
C5A-----SAVE CONC FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,6).NE.0) THEN
          WRITE(IOUT,74) ISPCUN,KSTP,KPER
          IF(CSPCFM.EQ.' ') THEN
             CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,-1,ISPCUN,NODES)
          ELSE
             CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                  NNDLAY,-1,ISPCUN,CSPCFM,LBHDSV,ICBUND,NODES)
          END IF
        END IF
      END IF
C
C6------RETURN.
   80 CONTINUE
      RETURN
C
      END
      SUBROUTINE SGWT2BCT1IBU(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND FOR UNSTRUCTURED GRIDS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:NCOL,NROW,NLAY,IXSEC,IOUT,NODLAY,NODES
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
      USE GWTBCTMODULE,ONLY:ICBUND
C
      CHARACTER*16 TEXT
      DATA TEXT /'          IBOUND'/
C     ------------------------------------------------------------------
      IF(IBOUUN.LE.0) RETURN
C
C1------FOR EACH LAYER: SAVE ICBUND WHEN REQUESTED.
      IFIRST=1
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,7).EQ.0) GO TO 79
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
C
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IBOUUN,KSTP,KPER
   74   FORMAT(1X,/1X,'IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I8,', STRESS PERIOD ',I8)
        IFIRST=0
        CALL ULASV3U(ICBUND,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,KK,IBOUUN,CBOUFM,LBBOSV,NODES)
   79   CONTINUE
C
C1A-----SAVE ICBUND FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,7).NE.0) THEN
          WRITE(IOUT,74) IBOUUN,KSTP,KPER
          CALL ULASV3U(ICBUND,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                  NNDLAY,-1,IBOUUN,CBOUFM,LBBOSV,NODES)
        END IF
      END IF
C
C6------RETURN.
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE GWT2BCT1DA
      USE GWTBCTMODULE
      USE GLOBAL, ONLY: ARAD
      INTEGER ALLOC_ERR
C
      DEALLOCATE(IBCTCB, STAT = ALLOC_ERR)
      DEALLOCATE(MCOMP, STAT = ALLOC_ERR)
      DEALLOCATE(ITVD, STAT = ALLOC_ERR)
      DEALLOCATE(CINACT, STAT = ALLOC_ERR)
      DEALLOCATE(CICLOSE, STAT = ALLOC_ERR)
      DEALLOCATE(ICBUND, STAT = ALLOC_ERR)
      DEALLOCATE(PRSITY, STAT = ALLOC_ERR)
      DEALLOCATE(CONC, STAT = ALLOC_ERR)
      DEALLOCATE(CONCO, STAT = ALLOC_ERR)
      DEALLOCATE(MASSBCT, STAT = ALLOC_ERR)
      DEALLOCATE(ADMAT, STAT = ALLOC_ERR)
      DEALLOCATE(IPCBFLAG, STAT = ALLOC_ERR)
      DEALLOCATE(CBCF, STAT = ALLOC_ERR)
      DEALLOCATE(CBCH, STAT = ALLOC_ERR)
      DEALLOCATE (Cncg,LrcC, STAT = ALLOC_ERR)
c      IF(IADSORB.NE.0) DEALLOCATE(ADSORB, STAT = ALLOC_ERR)
c      IF(IADSORB.EQ.2) DEALLOCATE(FLICH, STAT = ALLOC_ERR)
      DEALLOCATE(ADSORB, STAT = ALLOC_ERR)
      DEALLOCATE(FLICH, STAT = ALLOC_ERR)
c      IF(IDISP.NE.0)THEN
       DEALLOCATE(VELNOD, STAT = ALLOC_ERR)
       DEALLOCATE(DLX, DLY, DLZ, ATXY,ATYZ, ATXZ, STAT = ALLOC_ERR)
c       IF(IXDISP.NE.0) DEALLOCATE(ALXY, ALYZ, ALXZ)
       DEALLOCATE(ALXY, ALYZ, ALXZ, STAT = ALLOC_ERR)
c      ENDIF
c      IF(IZOD.EQ.1)THEN
c        DEALLOCATE(ZODRW, STAT = ALLOC_ERR)
c      ELSEIF(IADSORB.GT.0)THEN
c        IF(IZOD.EQ.2)THEN
c           DEALLOCATE(ZODRS, STAT = ALLOC_ERR)
c        ELSEIF(IZOD.EQ.3)THEN
          DEALLOCATE(ZODRW, STAT = ALLOC_ERR)
          DEALLOCATE(ZODRS, STAT = ALLOC_ERR)
c        ENDIF
c      ENDIF
c      IF(IFOD.EQ.1)THEN
c        DEALLOCATE(FODRW, STAT = ALLOC_ERR)
c      ELSEIF(IADSORB.GT.0)THEN
c        IF(IFOD.EQ.2)THEN
c           DEALLOCATE(FODRS, STAT = ALLOC_ERR)
c        ELSEIF(IFOD.EQ.3)THEN
          DEALLOCATE(FODRW, STAT = ALLOC_ERR)
          DEALLOCATE(FODRS, STAT = ALLOC_ERR)
c        ENDIF
c      ENDIF
      DEALLOCATE(IADSORB, STAT = ALLOC_ERR)
      DEALLOCATE(IDISP, STAT = ALLOC_ERR)
      DEALLOCATE(IXDISP, STAT = ALLOC_ERR)
      DEALLOCATE(DIFFNC, STAT = ALLOC_ERR)
      DEALLOCATE(VBVLT, STAT = ALLOC_ERR)
      DEALLOCATE(VBNMT, STAT = ALLOC_ERR)
      DEALLOCATE(MSUMT, STAT = ALLOC_ERR)
      DEALLOCATE(IFOD, IZOD, STAT = ALLOC_ERR)
      DEALLOCATE(FCncg, STAT = ALLOC_ERR)
      DEALLOCATE(FLrcC, STAT = ALLOC_ERR)
      DEALLOCATE(ARAD, STAT = ALLOC_ERR)

C
      RETURN
      END
