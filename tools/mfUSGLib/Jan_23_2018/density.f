C -------------------------------------------------------------------------------------
      MODULE DDFMODULE
C
        INTEGER, SAVE, POINTER :: IDDF
        DOUBLE PRECISION, SAVE, POINTER :: RHOFRESH,RHOSTD,CSTD
        DOUBLE PRECISION, SAVE, POINTER :: DRHODC
        DOUBLE PRECISION, SAVE,DIMENSION(:),ALLOCATABLE::RHONORM,ZEECELL
C
      END MODULE DDFMODULE
C
C -------------------------------------------------------------------------------------
C
      SUBROUTINE DDF1AR(IUDDF)
C     ******************************************************************
C     ALLOCATE SPACE AND READ INFORMATION FOR DENSITY DEPENDENT FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE DDFMODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION ZEE,RNORM
      REAL TEMPVAR
      CHARACTER*200 LINE
C
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
        INDDF = IUDDF
        WRITE(IOUT,1)INDDF
    1   FORMAT(1X,/1X,'DDF -- DENSITY DEPENDENT FLOW MODULE ',
     1    'VERSION 1, 4/4/2016 INPUT READ FROM UNIT ',I4)
C
C2------ALLOCATE SCALAR VARIABLES VECTORS, AND INITIALIZE.
      ALLOCATE(IDDF)
        IDDF = 1  ! INDEX THAT DENSITY DEPENDENT FLOW IS ACTIVE ON ONE SPECIES (HARDWIRE)
      ALLOCATE(RHOFRESH,RHOSTD,CSTD)
      ALLOCATE (DRHODC)
      ALLOCATE(RHONORM(NEQS),ZEECELL(NEQS))
C
C3------READ FRESHWATER DENSITY, STANDARD SOLUTION DENSITY AND STANDARD CONCENTRATION
      CALL URDCOM(INDDF,IOUT,LINE)
      IF(IFREFM.EQ.0) THEN
        READ(LINE,'(3F10.4)') RHOFRESH,RHOSTD,CSTD
        LLOC=41
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TEMPVAR,IOUT,INDDF)
        RHOFRESH = TEMPVAR
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TEMPVAR,IOUT,INDDF)
        RHOSTD = TEMPVAR
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TEMPVAR,IOUT,INDDF)
        CSTD = TEMPVAR
      END IF
C---------------------------------------------------------------------------
C3A-----REFLECT INPUT IN OUTPUT LISTING FILE
      WRITE(IOUT,3) RHOFRESH,RHOSTD,CSTD
    3 FORMAT(1X,'DENSITY OF FRESHWATER (RHOFRESH) =',F10.3,
     1  /1X,'DENSITY OF STANDARD SOLUTION (RHOSTD) =', F10.3,
     1  /1X,'CONCENTRATION OF STANDARD SOLUTION (CSTD) =',F10.3)
C---------------------------------------------------------------------------
C4------FILL ARRAY  ZEECELL, INITIALIZE RHO/RHOFRESH, AND 
C4------INITIALIZE HNEW TO BE THE NORMALIZED POTENTIAL (RHO/RHOFRESH*HEAD)
      DRHODC = (RHOSTD - RHOFRESH)/CSTD 
      DO N=1,NEQS
        CALL RHONORMCALC(N,RNORM)  
        RHONORM(N) = RNORM  
C        
        CALL ZCELLCALC(N,ZEE)
        ZEECELL(N) = ZEE
C        
CSP (USE SEPARATE SUBROUTINE FOR THIS)        HNEW(N) = RHONORM(N) * HNEW(N)
      ENDDO
C---------------------------------------------------------------------------
C5----RETURN
      RETURN
      END
C--------------------------------------------------------------------------- 
      SUBROUTINE DDF1AD(IUDDF)
C     ******************************************************************
C     ADVANCE DENSITY OF ALL CELLS FROM CONCENTRATION AFTER TRANSPORT 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE DDFMODULE, ONLY: DRHODC,RHONORM
      USE GLOBAL,ONLY: IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW,SN
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION ZEE,RNORM
      CHARACTER*200 LINE
C
C     ------------------------------------------------------------------
C1----LOOP OVER ALL CELLS
      DO N=1,NEQS
C2------COMPUTE RHO/RHOFRESH
c        IF(CONC(N,1).GT.CSTD) CONC(N,1) = CSTD
c        IF(CONC(N,1).LT.0.0) CONC(N,1) = 0.0
        CALL RHONORMCALC(N,RNORM)  
        RHONORM(N) = RNORM  
      ENDDO
C---------------------------------------------------------------------------
C5----RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE RHONORMCALC(N,RNORM)
C     ******************************************************************
C     CALCULATE THE NORMALIZED RHO FOR A GIVEN CONC AT CELL N
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE DDFMODULE
      USE GLOBAL,ONLY: IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW,Sn
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION RNORM,CNC
C     ------------------------------------------------------------------
C
C1----COMPUTE RNORM
      CNC = CONC(N,1)
      IF(CNC.LT.0.0) CNC = 0.0
      IF(CNC.GT.CSTD) CNC = CSTD
      RNORM = (RHOFRESH + Sn(N) * DRHODC * CNC)/RHOFRESH
C---------------------------------------------------------------------------
C2----RETURN
      RETURN
      END      
C---------------------------------------------------------------------------
      SUBROUTINE ZCELLCALC(N,ZEE)
C     ******************************************************************
C     CALCULATE CELL ELEVATION FOR DENSITY DEPENDENT FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE DDFMODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW,
     1            TOP,BOT
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION ZEE,FRAD,FLENG,FANGLE,FDPTH
C     ------------------------------------------------------------------
C
C1----CHECK IF CELL IS BCF OR CLN
      IF(N.LE.NODES)THEN
C2------GROUNDWATER CELL ELEVATION IS AVERAGE OF TOP AND BOT
        ZEE = 0.5 * (TOP(N) + BOT(N))
      ELSE 
C3------CLN CELL ELEVATION SET AT BOTTOM OF THE CLN CELL (LOCAL CELL INDEX)
        I = N - NODES
        ZEE = ACLNNDS(I,5) 
C4------ADJUST CLN CELL ELEVATION TO CENTER DEPENDING ON ORITENTATION
        IFDIR = ACLNNDS(I,3) 
        IF(IFDIR.EQ.2)THEN 
C5--------ANGLED PIPE            
          FLENG = ACLNNDS(I,4) 
          FANGLE = ACLNNDS(I,6)   
          FDPTH = FLENG * SIN(FANGLE)
        ELSEIF(IFDIR.EQ.1)THEN
C6--------HORIZONTAL PIPE
          IFTYP =  ACLNNDS(I,2)
          CALL CLNR(IFTYP,FRAD)
C          FDPTH = 2.0 * FRAD
          FDPTH = FRAD
        ELSEIF(IFDIR.EQ.0)THEN  
C7--------VERTICAL PIPE
         FLENG = ACLNNDS(I,4) 
         FDPTH = FLENG
        ENDIF  
        ZEE = ZEE + 0.5 * FDPTH  
      ENDIF
C---------------------------------------------------------------------------
C5----RETURN
      RETURN
      END      
C---------------------------------------------------------------------------
      SUBROUTINE DDF1FM(KSTP,KPER)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS DUE TO DENSITY TERM
C     FOR TRANSIENT SIMULATION ALSO ADD DENSITY STORAGE TERM      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IBOUND,NEQS,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,
     1                AMAT,RHS,Sn,ISSFLG,AREA,TOP,BOT,HNEW,HOLD
      USE GWFBASMODULE,ONLY:DELT
      USE SMSMODULE, ONLY: AMATFL
      USE GWTBCTMODULE, ONLY: PRSITY,CONC,CONCO
      USE CLN1MODULE, ONLY: ACLNNDS
      USE DDFMODULE, ONLY: ZEECELL,RHONORM,DRHODC,RHOFRESH,CSTD
C
      DOUBLE PRECISION QCON,ZEENJJ,RHO,VOL,HPHI,RHOTERM,AMAT_TERM,
     1  CNC,CNCO
C     ------------------------------------------------------------------
C     
C1------FILL DENSITY GRADIENT TERM FOR EVERY CELL
      DO N=1,NEQS
C
C2------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).LE.0) CYCLE
C
C3------CALCULATE FOR ALL CONNECTING FACES.
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(IBOUND(JJ).LE.0) CYCLE
C4--------CALCULATE FLOW DUE TO DENSITY TERM
          ZEENJJ = (ZEECELL(N) + ZEECELL(JJ)) * 0.5
          HPHI = (HNEW(N) + HNEW(JJ)) * 0.5  
C          HPHI = (HOLD(N) + HOLD(JJ)) * 0.5  !TIME LAG THIS TERM FOR STABILITY? USE HOLD? 
          QCON = AMAT(II) *(HPHI - ZEENJJ) * (RHONORM(N)-RHONORM(JJ))
C5--------ADD THIS FLOW TERM ON RHS OF N        
          RHS(N) = RHS(N) + QCON
        ENDDO
C
      ENDDO
C1------UPDATE FLOW MATRIX WITH DENSITY TERM 
      DO N=1,NEQS
C
C2------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).EQ.0) CYCLE
C
C3------CALCULATE FOR ALL CONNECTING FACES.
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(IBOUND(JJ).EQ.0) CYCLE
C4--------ADJUST MATRIX FOR DENSITY TERM
          RHOTERM = (RHONORM(N) + RHONORM(JJ)) * 0.5
          AMAT_TERM = AMAT(II) 
          AMAT(II) = AMAT_TERM * RHOTERM
          AMAT(IA(N)) = AMAT(IA(N)) + AMAT_TERM * (1.0 - RHOTERM)
        ENDDO
C
      ENDDO
C----------------------------------------------------------------------
C6------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO RHS
      ISS=ISSFLG(KPER)
c      IF(ISS.NE.0) RETURN  ! need this term even if flow is steady-state
c      RETURN    !skipping small transient density term
C7------FILL TRANSIENT DENSITY TERM FOR EACH CELL      
      DO N=1,NEQS      
C
C8------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).EQ.0) CYCLE
C
C9------COMPUTE TERM AND PUT ON RHS
        IF(N.LE.NODES)THEN
          VOL = AREA(N) * (TOP(N) - BOT(N))*PRSITY(N)
        ELSE
          I=N-NODES  
          VOL = AREA(N) * ACLNNDS(I,4)   
        ENDIF
csp        RHO = VOL*PRSITY(N)*Sn(N)/RHONORM(N)*DRHODC
        RHO = VOL*Sn(N)/RHONORM(N)/RHOFRESH*DRHODC
        CNC = CONC(N,1)
        IF(CNC.LT.0.0) CNC = 0.0
        IF(CNC.GT.CSTD) CNC = CSTD
        CNCO = CONCO(N,1)
        IF(CNCO.LT.0.0) CNCO = 0.0
        IF(CNCO.GT.CSTD) CNCO = CSTD        
        RHO = RHO * (CNC - CNCO) / DELT
        RHS(N) = RHS(N) + RHO
      ENDDO
c6----return
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE DDF1BD(KSTP,KPER)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS DUE TO DENSITY TERM
C     FOR TRANSIENT SIMULATION ALSO ADD DENSITY STORAGE TERM      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IBOUND,NEQS,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,
     1                AMAT,FLOWJA,Sn,ISSFLG,AREA,TOP,BOT,BUFF,HNEW
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,VBVL,VBNM,DELT,PERTIM,TOTIM
      USE SMSMODULE, ONLY: AMATFL
      USE GWTBCTMODULE, ONLY: PRSITY,CONC,CONCO
      USE DDFMODULE, ONLY: ZEECELL,RHONORM,DRHODC,RHOFRESH,CSTD
      USE GWFBCFMODULE,ONLY:IBCFCB
      USE CLN1MODULE, ONLY: ACLNNDS
C
      DOUBLE PRECISION QCON,ZEENJJ,RHO,VOL,STOIN,STOUT,SSTRG,STIN,SOUT,
     1  ZERO,HPHI,RHOTERM,CNC,CNCO,STRG
      CHARACTER*16 TEXT
      DATA TEXT /' DENSITY STORAGE'/
C     ------------------------------------------------------------------
C 0-----initialize
      ZERO = 0.0
      STOIN=ZERO
      STOUT=ZERO
C1------FILL DENSITY GRADIENT TERM FOR EVERY CELL
      DO N=1,NEQS
C
C2------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).LE.0) CYCLE
C
C3------CALCULATE FOR ALL CONNECTING FACES.
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(IBOUND(JJ).LE.0) CYCLE 
C4--------CALCULATE FLOW DUE TO DENSITY TERM
          ZEENJJ = (ZEECELL(N) + ZEECELL(JJ))*0.5
          HPHI = (HNEW(N) + HNEW(JJ)) * 0.5
C          HPHI = (HOLD(N) + HOLD(JJ)) * 0.5  !TIME LAG THIS TERM FOR STABILITY? USE HOLD? 
C4--------MATRIX WAS ADJUSTED FOR DENSITY TERM
          RHOTERM = (RHONORM(N) + RHONORM(JJ)) * 0.5
C          RHOTERM = 1.0
          QCON = AMATFL(II)/RHOTERM * (HPHI - ZEENJJ) 
     *         * (RHONORM(N)-RHONORM(JJ))
C5--------SUBTRACT THIS FLOW TERM ON FLOWJA (MINUS ON LHS, PLUS ON RHS)         
          FLOWJA(II) = FLOWJA(II) + QCON
        ENDDO
C
      ENDDO
C----------------------------------------------------------------------
C6------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO RHS
      ISS=ISSFLG(KPER)
c      IF(ISS.NE.0) GO TO 400   ! need this term even if flow is steady-state
c      GO TO 400  !skipping small transient density term
C
C7------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(IBCFCB.GT.0) IBD=ICBCFL 
C
C8------CLEAR BUFFER.
      DO 210 N=1,NEQS
        BUFF(N)=ZERO
210   CONTINUE
C7------FILL TRANSIENT DENSITY TERM FOR EACH CELL      
      DO N=1,NEQS      
C
C8------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).EQ.0) CYCLE
C
C9------COMPUTE TERM AND PUT ON RHS
        IF(N.LE.NODES)THEN
          VOL = AREA(N) * (TOP(N) - BOT(N))*PRSITY(N)
        ELSE
          I=N-NODES  
          VOL = AREA(N) * ACLNNDS(I,4)   
        ENDIF
csp        RHO = VOL*PRSITY(N)*Sn(N)/RHONORM(N)*DRHODC
        RHO = VOL*Sn(N)/RHONORM(N)/RHOFRESH*DRHODC
        CNC = CONC(N,1)
        IF(CNC.LT.0.0) CNC = 0.0
        IF(CNC.GT.CSTD) CNC = CSTD
        CNCO = CONCO(N,1)
        IF(CNCO.LT.0.0) CNCO = 0.0
        IF(CNCO.GT.CSTD) CNCO = CSTD        
        RHO = RHO * (CNC - CNCO) / DELT
        STRG = -RHO
C
C8-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS.
        BUFF(N)=STRG
        FLOWJA(IA(N)) = FLOWJA(IA(N)) - STRG
        SSTRG=STRG
        IF(STRG.LT.ZERO) THEN
          STOUT=STOUT-SSTRG
        ELSE
          STOIN=STOIN+SSTRG
        END IF
      ENDDO  
C9------record contents of buffer for structured and unstructured grids
      IF(IUNSTR.EQ.0)THEN
C
C9A-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
        IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                       IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)
        IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,IBCFCB,
     1            BUFF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ELSE
C
C9B-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
        IF(IBD.EQ.1) CALL UBUDSVU(KSTP,KPER,TEXT,IBCFCB,BUFF(1),NODES,
     1         IOUT,PERTIM,TOTIM)
        IF(IBD.EQ.2) CALL UBDSV1U(KSTP,KPER,TEXT,IBCFCB,BUFF(1),NODES,
     1     IOUT,DELT,PERTIM,TOTIM,IBOUND,NODES)
      ENDIF
C
C10-----ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      STIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+STIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=STIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
c6----return
      RETURN
      END
C-----------------------------------------------------------------
      SUBROUTINE DDF1DA
C  Deallocate DDF data 
      USE DDFMODULE
C
        DEALLOCATE(IDDF)
        DEALLOCATE(RHOFRESH,RHOSTD,CSTD,DRHODC)
        DEALLOCATE(RHONORM,ZEECELL)
C
      RETURN
      END      
