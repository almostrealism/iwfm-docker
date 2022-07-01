        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 06 10:28:31 2020
        MODULE CALC_UNSAT_INFIL__genmod
          INTERFACE 
            SUBROUTINE CALC_UNSAT_INFIL(FLOBOT,UZSEEP,UZTHST,THR,HA,    &
     &THETAS,EPSILON,FKS,AVHC,DEPTH,SBDTHK,WETPER,UZWDTH,FLOW,NWAVST,   &
     &STRLEN,IWIDTHCHECK,ICALC)
              USE GWFSFRMODULE, ONLY :                                  &
     &          ISUZN,                                                  &
     &          NSTOTRL
              REAL(KIND=8) :: FLOBOT
              REAL(KIND=8) :: UZSEEP(ISUZN)
              REAL(KIND=8) :: UZTHST(NSTOTRL)
              REAL(KIND=8) :: THR
              REAL(KIND=4) :: HA
              REAL(KIND=8) :: THETAS
              REAL(KIND=8) :: EPSILON
              REAL(KIND=4) :: FKS
              REAL(KIND=4) :: AVHC
              REAL(KIND=8) :: DEPTH
              REAL(KIND=8) :: SBDTHK
              REAL(KIND=8) :: WETPER(ISUZN)
              REAL(KIND=8) :: UZWDTH(ISUZN)
              REAL(KIND=8) :: FLOW
              INTEGER(KIND=4) :: NWAVST(ISUZN)
              REAL(KIND=4) :: STRLEN
              INTEGER(KIND=4) :: IWIDTHCHECK
              INTEGER(KIND=4) :: ICALC
            END SUBROUTINE CALC_UNSAT_INFIL
          END INTERFACE 
        END MODULE CALC_UNSAT_INFIL__genmod
