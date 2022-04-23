      subroutine xqtlne (cline, nchs)
 
      character *(*)  cline
      character *4096 cmdline /' '/
      character *256  arg     /' '/
 
      i = 0 ! start with parameter 0 (include program name)
      go to 10
 
      entry cparms(cline, nchs)
 
      i = 1 ! start with parameter 1 (skip program name)
 
   10 continue
      cmdline = ' '
      do
         call getarg(i, arg)
         if (arg .eq. ' ') exit
         j = len_trim(cmdline) + 1
         if (j .gt. 1) then
            cmdline(j:j) = ' '
            j = j + 1
         end if
         if (index(arg(1:len_trim(arg)), ' ') .gt. 0) then
            arg(2:) = arg
            arg(1:1) = '"'
            arg(len_trim(arg)+1:) = '"'
         end if
         cmdline(j:) = arg
         i = i + 1
      end do
 
      nchs = len_trim(cmdline)
      cline = cmdline(1:nchs)
 
      return
      end
 
