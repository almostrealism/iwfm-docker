SUBROUTINE LUBKSB(A,N,NP,INDX,B)
  IMPLICIT NONE
  
  !Transferred variables
  INTEGER::N,NP,INDX(N)
  REAL(8)::A(NP,NP),B(N)
  
  !Local variables
  INTEGER::I,II,J,LL
  REAL(8)::SUM
  
  II=0
  DO I=1,N
    LL=INDX(I)
    SUM=B(LL)
    B(LL)=B(I)
    IF (II.NE.0) THEN
      DO J=II,I-1
        SUM=SUM-A(I,J)*B(J)
      END DO
    ELSE IF (SUM.NE.0.0) THEN
      II=I
    END IF
    B(I)=SUM
  END DO
  DO I=N,1,-1
    SUM=B(I)
    IF (I.LT.N) THEN
      DO J=I+1,N
        SUM=SUM-A(I,J)*B(J)
      END DO
    END IF
    B(I)=SUM/A(I,I)
  END DO
      
END SUBROUTINE LUBKSB
