module fpath
  implicit none
!-----------------------------------------------------------------------------!
! Simple File Path Handling
! Leland Scantlebury, SSP&A, 2018
!-----------------------------------------------------------------------------!
  integer,   parameter, private     :: maxlen=150
!-----------------------------------------------------------------------------!

  contains
  
subroutine fpath_strip(path, cfolder, cfile)
  implicit none
  
  character(maxlen), intent(in)     :: path
  character(maxlen), intent(out)    :: cfolder, cfile
  
  integer                  :: lastslash
  
  lastslash = scan(path, '/\', .true.)
  cfolder = path(1:lastslash)
  cfile = path(lastslash+1:)
  
end subroutine fpath_strip
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine fpath_join(path1, path2, pathcombo)
  implicit none
  
  character(maxlen), intent(in)  :: path1, path2
  character(maxlen), intent(out) :: pathcombo
  
  character(maxlen)              :: c1, c2
  integer                        :: len1, len2
  
  ! Copy
  c1 = path1
  c2 = path2
  
  ! Set
  len1 = len_trim(c1)
  len2 = len_trim(c2)
  
  ! A work in progress - many potential situations to handle
  
  ! Checks for path1
  if (len1 > 0) then
    if (c1(len1:len1) /= '/'.and. &
        c1(len1:len1) /= '\') then
      c1 = trim(c1) // '\'
    end if
  end if
        
  ! Checks for path2
  if (c2(1:2)=='./'.or.c2(1:2)=='.\') then
    c2 = c2(3:)
  end if

  pathcombo = adjustl(trim(path1)) // adjustl(trim(path2))
  

end subroutine fpath_join
!-----------------------------------------------------------------------------!
end module fpath