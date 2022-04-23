      subroutine chrwt(klfn,cstr,nstr)
 
      character*(*) cstr
 
      if (nstr .ne. 0) then
         if (nstr .lt. 0 .or. nstr .gt. len(cstr)) then
            num_chars = len(cstr)
         else
            num_chars = nstr
         end if
         if (klfn .eq. 6) then
            call iowrite(1, cstr, num_chars, j)
         else
            write(klfn,'(a,$)')cstr(1:num_chars)
         end if
      end if
 
      return
      end subroutine
