        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 06 10:28:31 2020
        MODULE CALCUNSATFLOBOT__genmod
          INTERFACE 
            FUNCTION CALCUNSATFLOBOT(DEPTH,AVHC,FKS,WETPERM,SBDTHK,     &
     &AREAMAX,STRLEN,FBCHECK,NWAVST,MAXWAV,FOLDFLBT)
              USE GWFSFRMODULE, ONLY :                                  &
     &          NSTRAIL,                                                &
     &          ISUZN,                                                  &
     &          NEARZEROSFR
              REAL(KIND=8) :: DEPTH
              REAL(KIND=4) :: AVHC
              REAL(KIND=4) :: FKS
              REAL(KIND=8) :: WETPERM
              REAL(KIND=8) :: SBDTHK
              REAL(KIND=4) :: AREAMAX
              REAL(KIND=4) :: STRLEN
              REAL(KIND=8) :: FBCHECK
              INTEGER(KIND=4) :: NWAVST(ISUZN)
              INTEGER(KIND=4) :: MAXWAV
              REAL(KIND=8) :: FOLDFLBT
              REAL(KIND=4) :: CALCUNSATFLOBOT
            END FUNCTION CALCUNSATFLOBOT
          END INTERFACE 
        END MODULE CALCUNSATFLOBOT__genmod
