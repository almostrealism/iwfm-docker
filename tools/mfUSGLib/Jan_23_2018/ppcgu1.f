C
C PARALLEL UNSTRUCTURED CONJUGATE-GRADIENT SOLUTION PACKAGE
C
C   Calls the ParallelMODFLOW DLL to run the GPU PCG linear solver.
C
C (C) Copyright 2015, HydroAlgorithmics Pty Ltd.
C
      MODULE PPCGUMODULE
        INTEGER,SAVE :: IPRINTOPENCL,NBPOL
        INTEGER,SAVE :: ILINMETH,IPOLYDEGREE,IPRECMODE
        INTEGER,SAVE :: ITER1C,IPC,NNZC,NIAC
        INTEGER,SAVE :: IASMINIMPROVE,IGPUPLATFORM,IGPUDEVICE
        INTEGER,SAVE :: NCELLSUNCONVERGEDM1
        REAL   ,SAVE :: HCLOSEPCGU,RCLOSEPCGU,DAMP,HCHG,RCHG,RELAXPCGU
      END MODULE

      SUBROUTINE PPCGU1AR(IN, NJA, NEQS, MXITER, HICLOSE, ITER1, 
     1   IPRSMS, IFDPARAM, IPCGUM)
C     ******************************************************************
C     ALLOCATE STORAGE FOR PPCG ARRAYS AND READ PPCGU DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT, IAC=>IA, JAC=>JA
      USE PPCGUMODULE
C       DUMMY VARIABLES
      CHARACTER*200 LINE
      CHARACTER (LEN=  4) :: clin(1:2)
      CHARACTER (LEN= 10) :: cipc(0:2)
      CHARACTER (LEN= 28) :: cprec(0:2)
      CHARACTER (LEN=  4) :: cval
      INTEGER, INTENT(IN) :: IN
      INTEGER, INTENT(IN) :: NJA
      INTEGER, INTENT(IN) :: NEQS
      INTEGER, INTENT(IN) :: MXITER
      DOUBLE PRECISION, INTENT(IN) :: HICLOSE
      INTEGER, INTENT(IN) :: ITER1
      INTEGER, INTENT(IN) :: IFDPARAM
      INTEGER, INTENT(INOUT) :: IPCGUM
C       LOCAL VARIABLES
      INTEGER, PARAMETER :: IZERO = 0
      REAL, PARAMETER :: RZERO = 0.0
      DOUBLE PRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLE PRECISION, PARAMETER :: DONE  = 1.0D0
C       DATA
      DATA clin  /'CG  ',
     2            'BCGS'/
      DATA cipc  /'NONE      ',
     2            'NONE      ',
     3            'POLYNOMIAL'/
      DATA cprec /'DOUBLE                      ',
     2            'SINGLE                      ',
     3            'TRUNCATED (SINGLE AS DOUBLE)'/
C       OUTPUT FORMATS
02010 FORMAT (1X,/,14X,'SOLUTION BY THE CONJUGATE-GRADIENT METHOD',
     &        /,1X,66('-'),/,
     &        ' MAXIMUM OF ',I6,' CALLS OF SOLUTION ROUTINE',/,
     &        ' MAXIMUM OF ',I6,
     &        ' INTERNAL ITERATIONS PER CALL TO SOLUTION ROUTINE',/,
     &        ' LINEAR ACCELERATION METHOD            =',1X,A,/,
     &        ' MATRIX PRECONDITIONING TYPE           =',1X,A,/,
     &        ' PRECISION                             =',1X,A,/,
     &        ' POLYNOMIAL DEGREE                     =',I3,/,
     &        ' GPU PLATFORM                          =',I3,/,
     &        ' GPU DEVICE                            =',I3,/,
     &        ' HEAD CHANGE CRITERION FOR CLOSURE     =',E15.5,/,
     &        ' RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5,//)
2030  FORMAT(1X,A20,1X,6(I6,1X))
2040  FORMAT(1X,20('-'),1X,6(6('-'),1X))
2050  FORMAT(1X,62('-'),/)
C------------------------------------------------------------------
C-------TRANSFER COMMON VARIABLES FROM SMS TO PPCGU
      NNZC=NJA
      NIAC=NEQS
      HCLOSEPCGU = HICLOSE
      IPRECMODE=0
C
C-------PRINT A MESSAGE IDENTIFYING PACKAGE
      WRITE (IOUT,2000)
02000 FORMAT (1X,/1X,'PPCGU -- PARALLEL UNSTRUCTURED CONJUGATE-GRADIENT'
     &       ,' SOLUTION PACKAGE, VERSION 1.0, 06/29/2015')
C
C-------READ AND PRINT COMMENTS
      CALL URDCOM(IN,IOUT,LINE)
      IF ( IFDPARAM.EQ.0 ) THEN
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IPC,R,IOUT,IN)
        cval = LINE(ISTART:ISTOP)
        SELECT CASE (cval)
          CASE ( 'CG' )
            ILINMETH = 1
          CASE ( 'BCGS' )
            ILINMETH = 2
          CASE DEFAULT
            ILINMETH = 1
            READ (CVAL,*) IPC
        END SELECT
        IPCGUM = ILINMETH
        IF ( cval.EQ.'CG  ' .OR. cval.EQ.'BCGS' ) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPC,R,IOUT,IN)
        END IF
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IGPUPLATFORM,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IGPUDEVICE,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPOLYDEGREE,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,RCLOSEPCGU,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPRECMODE,R,IOUT,IN)
      ELSE
        CALL SET_PPCGUINPUT(IFDPARAM)
      END IF
C
C-------ERROR CHECKING FOR OPTIONS
      IF ( IPC.NE.2 .AND. IPC.NE.0 ) THEN
        WRITE( IOUT,'(A)' ) 'PPCGU1AR: IPC MUST BE .EQ. 2 OR 0'
        CALL USTOP('PPCGU1AR: IPC MUST BE .EQ. 2 OR 0')
      END IF
      IF ( IPOLYDEGREE.LT.0 .AND. IPC.EQ.2 ) THEN
        WRITE( IOUT,'(A)' ) 'PPCGU1AR: IPOLYDEGREE MUST BE .GE. 0'
        CALL USTOP('PPCGU1AR: IPOLYDEGREE MUST BE .GE. 0')
      END IF
      IF (IPRECMODE.LT.0 .AND. IPRECMODE.GT.2) THEN
        WRITE( IOUT,'(A)' ) 'PPCGU1AR: IPRECMODE MUST BE .EQ. 0, 1 OR 2'
        CALL USTOP('PPCGU1AR: IPRECMODE MUST BE .EQ. 0, 1 OR 2')
      END IF
C
C-------INITIALIZE PCGU VARIABLES
      IF(IPRSMS.EQ.2) THEN
          IPRINTOPENCL = 1
      ELSE
          IPRINTOPENCL = 0
      END IF
      ITER1C = ITER1
      NBPOL = 2
      DAMP = 1.0
      IASMINIMPROVE = 0
C
C-------PRINT MXITER,ITER1C,IPC,HCLOSEPCGU,RCLOSEPCGU
      WRITE (IOUT,2010) MXITER, ITER1C, 
     2                  clin(ILINMETH), cipc(IPC), cprec(IPRECMODE),
     3                  IPOLYDEGREE, IGPUPLATFORM, IGPUDEVICE,
     4                  HCLOSEPCGU, RCLOSEPCGU
C
C-------RETURN
      RETURN
      END SUBROUTINE PPCGU1AR
C
      SUBROUTINE PPCGU1AP(AC,RHS,HNEW,IAC,JAC,
     &                   ICNVG,KSTP,KPER,MXITER,KITER,IN_ITER,IOUT)
C
C     ******************************************************************
C     SOLUTION BY THE CONJUGATE GRADIENT METHOD -
C                                          UP TO ITER1 ITERATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PPCGUMODULE
      USE PMMODULE
      IMPLICIT NONE
C
      DOUBLEPRECISION, TARGET,  DIMENSION(NNZC), INTENT(INOUT) :: AC
      DOUBLE PRECISION, DIMENSION(NIAC), INTENT(INOUT)         :: RHS
      DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)          :: HNEW
      INTEGER, TARGET,  DIMENSION(NIAC+1),INTENT(IN)           :: IAC
      INTEGER, TARGET,  DIMENSION(NNZC),INTENT(IN)             :: JAC
      INTEGER, INTENT(INOUT)                                   :: ICNVG
      INTEGER, INTENT(IN)                                      :: KSTP
      INTEGER, INTENT(IN)                                      :: KPER
      INTEGER, INTENT(IN)                                      :: MXITER
      INTEGER, INTENT(IN)                                      :: KITER
      INTEGER, INTENT(INOUT)                                  :: IN_ITER
      INTEGER, INTENT(IN)                                      :: IOUT
C-------Run OpenCL solver
      CALL PCGOpenCLSolve_USG(NIAC,NNZC,ITER1C,HCLOSEPCGU,RCLOSEPCGU,
     1                   AC,IAC,JAC,HNEW,RHS,ICNVG,DAMP,
     2                   IPC,NBPOL,IN_ITER,HCHG,RCHG,
     3                   IPRINTOPENCL,IGPUPLATFORM,IGPUDEVICE,RELAXPCGU,
     4                   IPOLYDEGREE,IASMINIMPROVE,NCELLSUNCONVERGEDM1,
     5                   ILINMETH,IPRECMODE)
C
      RETURN
C
      END SUBROUTINE PPCGU1AP
C
C
      SUBROUTINE PPCGU1DA()
C-------Deallocate PPCGU DATA
      USE PPCGUMODULE
      USE PMMODULE
C
      CALL PCGOpenCLDeinit_USG()
C
      RETURN
      END SUBROUTINE PPCGU1DA
C
C
      SUBROUTINE SET_PPCGUINPUT(IFDPARAM)
      INTEGER IFDPARAM
C Simple option
      SELECT CASE ( IFDPARAM )
      CASE(1)
        ILINMETH=1
        IPC=2
        IPOLYDEGREE=2
        RCLOSEPCGU=0.1
        DAMP=1.0
C Moderate
      CASE(2)
        ILINMETH=2
        IPC=2
        IPOLYDEGREE=5
        RCLOSEPCGU=0.1
        DAMP=1.0
C Complex
      CASE(3)
        ILINMETH=2
        IPC=2
        IPOLYDEGREE=8
        RCLOSEPCGU=0.1
        DAMP=1.0
      END SELECT
      RETURN
      END SUBROUTINE
