        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 06 10:28:16 2020
        MODULE GENRCM__genmod
          INTERFACE 
            SUBROUTINE GENRCM(NEQNS,NJA,XADJ,ADJNCY,PERM,MASK,XLS)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQNS
              INTEGER(KIND=4) :: XADJ(NEQNS+1)
              INTEGER(KIND=4) :: ADJNCY(NJA)
              INTEGER(KIND=4) :: PERM(NEQNS)
              INTEGER(KIND=4) :: MASK(NEQNS)
              INTEGER(KIND=4) :: XLS(NEQNS+1)
            END SUBROUTINE GENRCM
          END INTERFACE 
        END MODULE GENRCM__genmod
