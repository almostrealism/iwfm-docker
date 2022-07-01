        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 06 10:28:16 2020
        MODULE FNROOT__genmod
          INTERFACE 
            SUBROUTINE FNROOT(LLS,NEQNS,NJA,ROOT,XADJ,ADJNCY,MASK,NLVL, &
     &XLS,LS)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQNS
              INTEGER(KIND=4) :: LLS
              INTEGER(KIND=4) :: ROOT
              INTEGER(KIND=4) :: XADJ(NEQNS+1)
              INTEGER(KIND=4) :: ADJNCY(NJA)
              INTEGER(KIND=4) :: MASK(NEQNS)
              INTEGER(KIND=4) :: NLVL
              INTEGER(KIND=4) :: XLS(NEQNS+1)
              INTEGER(KIND=4) :: LS(LLS)
            END SUBROUTINE FNROOT
          END INTERFACE 
        END MODULE FNROOT__genmod
