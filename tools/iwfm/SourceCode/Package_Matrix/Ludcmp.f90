SUBROUTINE LUDCMP(A,N,NP,INDX,D)
  IMPLICIT NONE
  
  !Transferred variables
  INTEGER::N,NP,INDX(N)
  REAL(8)::D,A(NP,NP)
  
  !Local variables
  INTEGER,PARAMETER::NMAX=500
  REAL(8),PARAMETER::TINY=1D-20
  INTEGER::I,IMAX,J,K
  REAL(8)::AAMAX,DUM,SUM,VV(NMAX)
  
  D=1.0
  DO I=1,N
    AAMAX=0.0
    DO J=1,N
      IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
    END DO
    IF (AAMAX.EQ.0.0) PAUSE 'Singular matrix.'
    VV(I)=1.0/AAMAX
  END DO
  DO J=1,N
    IF (J.GT.1) THEN
      DO I=1,J-1
        SUM=A(I,J)
        IF (I.GT.1)THEN
          DO K=1,I-1
            SUM=SUM-A(I,K)*A(K,J)
          END DO
          A(I,J)=SUM
        END IF
      END DO
    END IF
    AAMAX=0.0
    DO I=J,N
      SUM=A(I,J)
      IF (J.GT.1) THEN
        DO K=1,J-1
          SUM=SUM-A(I,K)*A(K,J)
        END DO
        A(I,J)=SUM
      END IF
      DUM=VV(I)*ABS(SUM)
      IF (DUM.GE.AAMAX) THEN
        IMAX=I
        AAMAX=DUM
      END IF
    END DO
    IF (J.NE.IMAX) THEN
      DO K=1,N
        DUM=A(IMAX,K)
        A(IMAX,K)=A(J,K)
        A(J,K)=DUM
      END DO
      D=-D
      VV(IMAX)=VV(J)
    END IF
    INDX(J)=IMAX
    IF (J.NE.N) THEN
      IF (A(J,J).EQ.0.0) A(J,J)=TINY
      DUM=1.0/A(J,J)
      DO I=J+1,N
        A(I,J)=A(I,J)*DUM
      END DO
    END IF
  END DO
  IF (A(N,N).EQ.0.0) A(N,N)=TINY
      
END SUBROUTINE LUDCMP
