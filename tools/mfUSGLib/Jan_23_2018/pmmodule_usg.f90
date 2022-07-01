! ------------------------------------------------
! ParallelMODFLOW USG C++ DLL subroutine interface
! ------------------------------------------------
MODULE PMMODULE
      INTERFACE

      SUBROUTINE GetStartTime(startTime) BIND(C, NAME='GetStartTime')
      IMPLICIT NONE

          INTEGER, INTENT(OUT) :: startTime

      END SUBROUTINE
      
      SUBROUTINE PrintTimeTaken(startTime) BIND(C, NAME='PrintTimeTaken')
      IMPLICIT NONE

          INTEGER, INTENT(IN) :: startTime

      END SUBROUTINE
      
      SUBROUTINE PCGOpenCLDeinit_USG() BIND(C, NAME='PCGOpenCLDeinit_USG')
      IMPLICIT NONE

      END SUBROUTINE
      
      SUBROUTINE PCGOpenCLSolve_USG(nodeCount, &
                                    NJA, &
                                    innerIterationCount, &
                                    HCLOSE, &
                                    RCLOSE, &
                                    AMAT, &
                                    IA, &
                                    JA, &
                                    HNEW, &
                                    RHS, &
                                    ICNVG, &
                                    DAMP, &
                                    NPCOND, &
                                    NBPOL, &
                                    NITER, &
                                    HCHG, &
                                    RCHG, &
                                    IPRINT, &
                                    IGPUPLATFORM, &
                                    IGPUDEVICE, &
                                    RELAX, &
                                    IPOLYDEGREE, &
                                    IASMINIMPROVE, &
                                    NCELLSUNCONVERGEDM1, &
                                    ILINMETH, &
                                    IPRECMODE) BIND(C, NAME='PCGOpenCLSolve_USG')
      IMPLICIT NONE

          INTEGER, INTENT(IN)                           :: nodeCount
          INTEGER, INTENT(IN)                           :: NJA
          INTEGER, INTENT(IN)                           :: innerIterationCount
          REAL, INTENT(IN)                              :: HCLOSE
          REAL, INTENT(IN)                              :: RCLOSE
          DOUBLEPRECISION, DIMENSION(*), INTENT(IN)     :: AMAT
          INTEGER, DIMENSION(*), INTENT(IN)             :: IA
          INTEGER, DIMENSION(*), INTENT(IN)             :: JA
          DOUBLEPRECISION, DIMENSION(*), INTENT(INOUT)  :: HNEW
          DOUBLEPRECISION, DIMENSION(*), INTENT(INOUT)  :: RHS
          INTEGER, INTENT(INOUT)                        :: ICNVG
          REAL, INTENT(IN)                              :: DAMP
          INTEGER, INTENT(IN)                           :: NPCOND
          INTEGER, INTENT(IN)                           :: NBPOL
          INTEGER, INTENT(INOUT)                        :: NITER
          REAL, INTENT(INOUT)                           :: HCHG
          REAL, INTENT(INOUT)                           :: RCHG
          INTEGER, INTENT(IN)                           :: IPRINT
          INTEGER, INTENT(IN)                           :: IGPUPLATFORM
          INTEGER, INTENT(IN)                           :: IGPUDEVICE
          REAL, INTENT(IN)                              :: RELAX
          INTEGER, INTENT(IN)                           :: IPOLYDEGREE
          INTEGER, INTENT(IN)                           :: IASMINIMPROVE
          INTEGER, INTENT(INOUT)                        :: NCELLSUNCONVERGEDM1
          INTEGER, INTENT(IN)                           :: ILINMETH
          INTEGER, INTENT(IN)                           :: IPRECMODE

      END SUBROUTINE
      
      END INTERFACE
END MODULE
