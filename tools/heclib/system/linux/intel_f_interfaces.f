!=======================================================================
! 01/07/2019 - Nicky Sandhu. Borrowed from windows/*. Removing the !ms$ directives as those only work with Windows. 
! Only supporting Intel Fortran compilers No need to support defunct Digital Fortran compilers
! This file is required for the Intel and Digital Fortran compilers
! because these library functions cannot be called with the
! /names:lowercase and /assume:underscore complier flags used for
! the rest of the Fortran source files.
!
! Compile this source file without /assume:underscore and with
! /names:as_is
!
!=======================================================================
      subroutine flush_(iunit)
      use ifcore
      logical result

      result = COMMITQQ(iunit)

      return
      end subroutine
!=======================================================================
      integer*4 function rename_(oldname, newname)
      use ifport
      character*(*) oldname, newname
 
      rename_ = RENAME(oldname, newname)
 
      return
      end function
!=======================================================================
      integer*4 function unlink_(name)
      use ifport
      character*(*) name

      unlink_ = UNLINK(name)
 
      return
      end function
!=======================================================================
!      integer*4 function ierrno_
!      use ifport
 
!      ierrno_ = IERRNO()
 
!      return
!      end function
!=======================================================================
      subroutine gerror_(cerrmsg)
      use ifcore
      character*(*) cerrmsg
 
      call GERROR(cerrmsg)
 
      end subroutine
!=======================================================================
!      logical function peekcharqq_
!      use ifcore
 
!      peekcharqq_ = PEEKCHARQQ()
 
!      return
!      end function
!=======================================================================
!      character*1 function getcharqq_
!      use ifcore
 
!      getcharqq_ = GETCHARQQ()
 
!      return
!      end function
!=======================================================================
      character*24 function fdate_
      use ifport
 
      fdate_ = FDATE()
 
      return
      end function
!=======================================================================
 
