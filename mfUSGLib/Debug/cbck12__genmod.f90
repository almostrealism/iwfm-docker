        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 06 10:28:33 2020
        MODULE CBCK12__genmod
          INTERFACE 
            SUBROUTINE CBCK12(II,N1,N2,K,EL1,EL2,ANUM,HK,TH1,TH2)
              USE GLOBAL, ONLY :                                        &
     &          NODES,                                                  &
     &          ISYM,                                                   &
     &          ARAD,                                                   &
     &          JAS
              INTEGER(KIND=4) :: II
              INTEGER(KIND=4) :: N1
              INTEGER(KIND=4) :: N2
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: EL1
              REAL(KIND=4) :: EL2
              REAL(KIND=8) :: ANUM
              REAL(KIND=4) :: HK(NODES)
              REAL(KIND=8) :: TH1
              REAL(KIND=8) :: TH2
            END SUBROUTINE CBCK12
          END INTERFACE 
        END MODULE CBCK12__genmod
