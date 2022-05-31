Program SquareRootStream

  
  integer      i,j,k
  character (len=1000) line
  real                 value
  
  
  
  Open(11,file='STR_OUT.out')
  Open(12,file='SQRTSTR_OUT.out')
  do
    read(11,'(a40,f20.2)',end=100)line,value
    if (value.lt.0) then
      value=0
    else
      value=value**0.5
    end if
!    write(12,'(a39,f10.2)')line,value
    write(12,'(a36,f20.2)')line,value
  end do
  
100 stop  
  
  end program
  