subroutine IDW(nobs,x,y,ObsElem, InterpValues, nnodes,nlayers,nodex,nodey,NodeVal,nelements,elements)
  implicit none
  
  integer         :: i,j,k,n,m,nelements,nodeID
  integer         :: nnodes,nlayers,nobs
  integer         :: elements(nelements,4),ObsElem(nobs)
  real            :: interpV, totalDistance, Distance
  real            :: x(nobs),y(nobs),InterpValues(nobs,nlayers)
  real            :: nodex(nnodes),nodey(nnodes),NodeVal(nnodes,nlayers)
  real            :: CalcDistance,wgt_tmp,wgt
  InterpValues=0
  do i=1,nobs
    wgt=0
    do j=1,4
      if (ObsElem(i).gt.0) then
        nodeID = elements(ObsElem(i),j)
        if (nodeID.gt.0) then
         Distance = CalcDistance(x(i),y(i),nodex(NodeID),nodey(nodeID))
         wgt_tmp = 1.0 / Distance;
	       wgt =wgt+ wgt_tmp;
         do k=1,nlayers
          InterpValues(i,k) =InterpValues(i,k) + wgt_tmp * NodeVal(nodeID,k)
         end do
        end if
      end if
    end do
    do k=1,nlayers
        InterpValues(i,k) =InterpValues(i,k) / wgt
    end do
  end do
  end subroutine IDW

!-----------------------------------------------------------------------------!
  
  real function CalcDistance(x1,y1, x2,y2)
    implicit none
    real          :: x1,x2,y1,y2
    
    CalcDistance = sqrt ( (x1-x2)**2 + (y1-y2)**2)
    return
  end function
  
!-----------------------------------------------------------------------------!
  
!-----------------------------------------------------------------------------!
  subroutine loc2glo(locx, locy, xoff, yoff, rot, glox, gloy)
    implicit none
    
    real,intent(in)        :: locx, locy, xoff, yoff, rot
    real,intent(out)       :: glox, gloy
    real, parameter        :: DEG2RAD = 3.141592654/180.0
    
    glox = xoff + locx * cos(rot*DEG2RAD) - locy * sin(rot*DEG2RAD)
    gloy = yoff + locy * cos(rot*DEG2RAD) + locx * sin(rot*DEG2RAD)
  
  end subroutine loc2glo
!-----------------------------------------------------------------------------!
  
!-----------------------------------------------------------------------------!
  subroutine glo2loc(glox, gloy, xoff, yoff, rot, locx, locy)
    implicit none
    
    real,intent(in)        :: glox, gloy, xoff, yoff, rot
    real,intent(out)       :: locx, locy
    real, parameter        :: DEG2RAD = 3.141592654/180.0
    
    locx = (glox-xoff) * cos(rot*DEG2RAD) + (gloy-yoff) * sin(rot*DEG2RAD)
    locy = (gloy-yoff) * cos(rot*DEG2RAD) - (glox-xoff) * sin(rot*DEG2RAD)
  
  end subroutine glo2loc
!-----------------------------------------------------------------------------!
  
MODULE errorhandle
  CONTAINS
  !-----------------------------------------------------------------------------!
    subroutine iostathandler(ierr, filename, linenumber)
      implicit none
  !-----------------------------------------------------------------------------!
  ! Handles some simple errors returned by iostat during I/O operations
  ! Error codes may be specific to the Intel Fortran Compiler.
  ! STOPS IF IT FINDS AN ERROR
  !
  ! Error Code Source: https://software.intel.com/en-us/node/678472
  !
  ! Author: Leland Scantlebury, SSP&A
  !-----------------------------------------------------------------------------!

      integer, intent(in)        :: ierr
      integer, optional          :: linenumber
      character(1000)            :: message
      character(*), optional     :: filename

  ! Assemble character of optinally included error information
      message = ''
      if (present(filename)) write(message, '(2a)') ' -Filename: ', trim(filename)
      if (present(linenumber)) write(message, '(2a,i5)') trim(message), ' -Line: ', & linenumber

  ! Only does something if iostat is not zero
      if (ierr /= 0) then
        if (ierr < 0) then
          write(*, '(a)') 'Error - Unexpected end of file.', message
        else if (ierr == 29) then
          write(*, '(a)') 'Error - File not found.', message
        else if (ierr == 39) then
          write(*, '(a)') 'Error - error during read.', message
        else if (ierr == 43) then
          write(*, '(a)') 'Error - Invalid path or filename.', message
        else
          write (*, '(a,i3,a)') 'Unexpected Error #', ierr, message
        end if
        stop
      end if

    end subroutine iostathandler

!-----------------------------------------------------------------------------!

    subroutine ifailhandler(ifail, filename, cline)
      implicit none
      
      ! To handle errors output by intread, multilinesplit
      integer, intent(in)        :: ifail
      character(*), intent(in)   :: filename, cline
      
      if (ifail /= 0) then
        write(*,'(a)') 'Error processing line in ' // trim(filename)
        write(*,'(a)') 'Line: ' // trim(cline)
        stop
      end if
    
    end subroutine ifailhandler
  !---------------------------------------------------------------------------!
    
    subroutine parexceed(par, parstr, message, finalstr, finalreal, finalint)
      implicit none
      
      ! To handle if a hardcoded parameter is exceeded
      integer                        :: par
      character(*)                   :: parstr
      character(*), optional         :: message, finalstr
      integer, optional              :: finalint
      real, optional                 :: finalreal
      
      write(*,'(3a)') 'Error - ', trim(parstr), ' exceeded!'
      if (present(message)) then
        write(*,'(8x,a)') trim(message)
      end if
      if (present(finalstr)) then
        write(*,'(8x,2a)') 'Last value: ', trim(finalstr)
      else if (present(finalint)) then
        write(*,'(8x,a,i8)') 'Last value: ', finalint
      else if (present(finalreal)) then
        write(*,'(8x,a,f18.5)') 'Last value: ', finalreal
      end if
      write(*,'(8x,2a)') 'Recompile with greater value for parameter ', trim(parstr)
      
      ! Stops
      stop
      
    end subroutine parexceed
  
!---------------------------------------------------------------------------!    
    subroutine USGunimplemented()
      write(*,*) 'ERROR - MODFLOW-USG is not currently supported in Texture2Par'
      write(*,*) 'Please contact lelands@sspa.com'
      stop
    end subroutine USGunimplemented
  !---------------------------------------------------------------------------!

!---------------------------------------------------------------------------!    
    subroutine NotInNam(filetype)
      implicit none
      
      character(*)         :: filetype
      
      write(*,'(2a)') 'ERROR - MODFLOW package is required in the Name file:', trim(filetype)
      stop
    end subroutine NotInNam
  !---------------------------------------------------------------------------!
    
  end MODULE errorhandle 
  
!-----------------------------------------------------------------------------!
subroutine FindSectionLength(unitno, length, symbol)
  implicit none
!-----------------------------------------------------------------------------!
! Scans open file from current location (so file must already be open) to
! location of next specified symbol (assumes position 1). Assigns the number of
! lines between start and symbol to 'length' integer argument and rewinds file
! back to the position it was at when the subroutine was called. If end of file
! is reached, returns that length instead.
!
! Author: Leland Scantlebury, SSP&A
!-----------------------------------------------------------------------------!
    
    integer, intent(in)        :: unitno
    integer, intent(inout)     :: length
    integer                    :: i, ierr
    character(1), intent(in)   :: symbol
    character(1000)            :: line
    
    length = 0
    do
      read(unitno, *, iostat=ierr) line
      if (ierr /= 0) exit
      if (line(1:1) == symbol) exit
      length = length + 1
    end do
    
    do i=1, length + 1
      backspace(unitno)
    end do
    
  end subroutine FindSectionLength

!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
  subroutine ReadToSymbol(unitno, symchar)
    use ErrorHandle
    implicit none
    
    ! Reads file until a specified symbol is found at the start of a line,
    ! Checks that next line doesn't have symbol, then backspaces
    ! Effictively giving you the next "non-commented" line for reading
    ! Useful for jumping past commented lines
    ! LS 7/10/2018
    
    integer, intent(in)      :: unitno
    integer                  :: ierr
    character(1), intent(in) :: symchar
    character(5)             :: line
    
    ! Find Symbol
    do
      read(unitno, *, iostat=ierr) line
      if (ierr /= 0) then
        call iostathandler(ierr)
        stop
      end if
      if (index(line,symchar) > 0) exit
    end do
    
    ! Make sure following lines don't have symbol
    do
      read(unitno, *, iostat=ierr) line
      if (ierr /= 0) exit
      if (index(line,symchar) == 0) exit
    end do
    ! End of File is NOT handled in this subroutine
    
    ! Back one line
    backspace(unitno)
    
  end subroutine ReadToSymbol
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
real function pinpol(px, py, polyx, polyy, npoly) result(inpoly)
  implicit none
  !---------------------------------------------------------------------------!
  !                                 PINPOL
  !                               *********
  !                 Checks if a point is inside a polygon.
  !      "An improved version of the algorithm of Nordbeck and Rydstedt"
  !
  ! Arguments:
  !   -- px is the x-coordinate of the point to be tested
  !   -- py is the y-coordinate of the point to be tested
  !   -- polyx is a vector of nodal x-coordinates in counter-clockwise order
  !   -- polyy is a vector of nodal y-coordinates in counter-clockwise order
  !   -- npoly is the number of polygon verticies
  !
  ! Requires:
  !   -- Function tridet to calculate triangle determinant
  !
  !
  ! Adapted from Sloan, S. W., 1985 A point-in-polygon program, Adv. Eng.
  !                  Software, Vol 7., No. 1
  !
  !               Implemented in f90 by Leland Scantlebury
  !---------------------------------------------------------------------------!
  
  integer, intent(in)      :: npoly
  integer                  :: i, next, prev, j
  real, intent(in)         :: px, py, polyx(npoly), polyy(npoly)
  real                     :: x1, y1, x21, y21, x1p, y1p, t, d, dx, dy, area
  real, parameter          :: smalld = 1e-6
  logical                  :: snear
  ! Functions
  real                     :: tridet
  
  inpoly = HUGE(1.0)
  
  ! Loop over each side defining the polygon
  do i=1, npoly
    ! Side coordinates, length, distance from x,y
    next = i + 1
    if (i == npoly) next = 1
    x1 = polyx(i)
    y1 = polyy(i)
    x21 = polyx(next)-x1
    y21 = polyy(next)-y1
    x1p = x1-px
    y1p = y1-py
    
    ! Find where normal of px,py intersects infinite line
    t = -(x1p*x21 + y1p*y21)/(x21**2 + y21**2)
    if (t < 0.0d0) then
      ! Normal does not intersect side, point is closer to (x1, y1)
      ! Compute square distance to vertex
      d = x1p**2 + y1p**2
      if (d < inpoly) then
        ! Smallest distance yet
        snear = .false.
        inpoly = d
        j = i
      end if
    else if (t < 1.0d0) then
      ! Normal intersects the side
      dx = x1p + t * x21
      dy = y1p + t * y21
      d = dx**2 + dy**2
      if (d < inpoly) then
        ! Smallest distance yet
        snear = .true.
        inpoly = d
        j = i
      end if
    else
      ! Point is closer to the next vertex, continue on to next side
      cycle
    end if
  end do
  
  if (inpoly < smalld) then
    ! Point lies on the side of the polygon
    inpoly = 0.0d0
  else
    next = j + 1
    prev = j - 1
    if (j == 1) then
      prev = npoly
    else if (j == npoly) then
      next = 1
    else
      continue
    end if
    if (snear) then
      ! Point is closer to side, check if right or left of polygon
      ! (using determinant) if left, point is in polygon
      area = tridet(polyx(j),polyy(j),polyx(next),polyy(next),px,py)
      inpoly = sign(inpoly, area)
    else
      ! Point is closer to node. Check if nearest vertex is concave
      ! If concave, point is inside the polygon
      !if (j == 1) j = npoly + 1
      area = tridet(polyx(next),polyy(next),polyx(j),polyy(j),polyx(prev),polyy(prev))
      inpoly = sign(inpoly, area)
    end if
  end if
  
  return
    
  end function pinpol
  
!-----------------------------------------------------------------------------!

real function tridet(x1, y1, x2, y2, x3, y3) result(det)
  implicit none
!-----------------------------------------------------------------------------!
! Computes twice the area of the triangle defined by coordinates
! (x1,y1) (x2,y2) (x3,y3) using determinate formula
!
! If the area is positive, the points are counter-clockwise
! If the area is negative, the points are clockwise
! If the area is zero, two or more of the points are co-located or all three
!    points are collinear
!
! Useful for determining what side of a line a point (x3, y3) is on.
!-----------------------------------------------------------------------------!
  real, intent(in)         :: x1, y1, x2, y2, x3, y3
  
  det = (x1-x3)*(y2-y3)-(x2-x3)*(y1-y3)
  return

  end function tridet
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine strtracker(n, str, strarray, wasnew)
  implicit none
!-----------------------------------------------------------------------------!
! Looks through strarray from 1 to n, looking for str.
! If it doesn't find str, it adds it to the end of strarray
! And increases n by 1
! Reports whether the str was new or not via wasnew (boolean)
!
! Author: Leland Scantlebury
!-----------------------------------------------------------------------------!

    integer, intent(inout)   :: n
    character(30), intent(inout) :: str, strarray(*)
    logical, intent(inout)   :: wasnew
    integer                  :: i

    wasnew = .true.
    ! Find if str in strarray
    do i=1, n
      if (str == strarray(i)) then
        wasnew = .false.
      end if
    end do
  
    ! If not, add to it and update n
    if (wasnew) then 
      n = n + 1
      strarray(n) = str
    end if

end subroutine strtracker
!-----------------------------------------------------------------------------!