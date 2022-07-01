      integer function fortranwrite (iunit, string, ladv)
c
c     interface routine for c/c++ functions to write to a file
c     under fortran
c
      integer   (kind=4), intent(in)           :: iunit
      character (len=*),  intent(in)           :: string
      character (len=3)                        :: adv
      logical   (kind=4), intent(in), optional :: ladv 
c
      integer   (kind=4)                       :: istat
c
      if (present(ladv)) then
         if (ladv) then
            adv = 'yes'
         else
            adv = 'no'
         end if
      else
         adv = 'yes'
      end if
      write (iunit, '(a)',  advance=adv, iostat=istat) string
      fortranwrite = istat
c
      return
      end
