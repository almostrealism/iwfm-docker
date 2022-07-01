MODULE Krige
  implicit none
  
    SAVE
    integer, parameter         :: maxrot = 1, ind = 1, &                       ! one matrix
                                  ang2=0.0, ang3=0.0                           ! no z-dimension (2D)
    integer                    :: itype
    real                       :: sill, range, ang1, anis1, &
                                  anis2, nugget
    real                       :: rotmat(maxrot,3,3)

    ! Note: Z-values are all set to zero. There is no interpolation done in the
    !       z-direction since well pc values and pilot points effectively
    !       have no z-value
    
    ! Note: number of rotation matricies is currently hardcoded to 1 (maxrot)
    !    (more than that number would need to be changed to implement multiple)

  CONTAINS
  
  !-----------------------------------------------------------------------------!
subroutine spkrige(nobs, ngrid, gx, gy, obsx, obsy, obsv, kriged_val)
    implicit none
!-----------------------------------------------------------------------------!
!                       Simple Point Kriging Subroutine
!                     ***********************************
!    
! Requires: PSET, decomp, variogram, doolittle    
!    
! Arguments:
!   nobs (int) number of observations
!   ngrid (int) number of grid points
!   gx (real array) grid X values
!   gy (real array) grid Y values
!   itype (int) variogram shape (0-linear, 1-spherical, 2-exponential)
!   ind (int) matrix indicator to initialize
!   sill
!   ang1, ang2, ang3 (real) azimuth, dip plunge
!   anis1, anis2 (real) First & second anisotropy ratios
!   maxrot (int) maximum number of rotation matrices dimensioned
!   rotmat (int) rotation matrices
!   a
!   nugget
!   kriged_val (real array) kriging results
!-----------------------------------------------------------------------------!

    integer                  :: maxpt, ntot, i, g, n, grouploop, ier
    integer, allocatable     :: id(:)
    integer, intent(in)      :: nobs, ngrid
    real                     :: value, zkrig
    real, allocatable        :: p(:), r(:), w(:)
    real, intent(in)         :: gx(ngrid), gy(ngrid), obsx(nobs), obsy(nobs), &
                                obsv(nobs)
    real, intent(inout)      :: kriged_val(ngrid)
    real, parameter          :: nodataval = -999.0

    ntot = nobs + 1              ! +1 for ordinary
    maxpt = ntot * (ntot + 1) / 2
    allocate(p(maxpt),r(ntot),w(ntot),id(ntot))
    p = 0.d0
    id = 0
    call PSET(p,obsx,obsy,id,nobs,ntot)
    call decomp(p,id,ntot, ier)

    if (ier == 1) then
      write(*,'(2x,a)') 'Error - Kriging Matrix Decomposition Failure.'
      write(*,'(2x,a)') 'This error is usually caused by co-located values.'
      stop
    end if
    
    ! Loop over each grid point
    do i = 1, ngrid
      r = 0.d0
      w = 0.d0
      r(ntot) = 1.0d0              ! commented = simple, uncommented = ordinary
      ! Loop over observations
      do n = 1, nobs
        call variogram(gx(i),gy(i),0.0d0,obsx(n),obsy(n),0.0d0,value)
        r(n) = value
      end do
      call doolittle(r,p,id,w,ntot)
      value = 0.0d0
      zkrig = 0.0d0
      do n=1, nobs
        zkrig = zkrig + w(n) * obsv(n)
      end do
      zkrig = zkrig + value
      kriged_val(i) = zkrig
    end do

    deallocate(p, r, w, id)

end subroutine spkrige
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine spkrige_tree(ngrid, gx, gy, obstree, nobs, obsval, kriged_val, nnear)
    use kd_tree
    implicit none
!-----------------------------------------------------------------------------!
!                       Simple Point Kriging Subroutine
!                     ***********************************
!    
! Requires: PSET, decomp, variogram, doolittle    
!    
!
!-----------------------------------------------------------------------------!

    integer                  :: maxpt, ntot, i, j, g, n, closeobs(nnear), &
                                usenear, ier
    integer, allocatable     :: id(:)
    integer, intent(in)      :: ngrid, nnear, nobs
    real                     :: value, zkrig, queryxy(2), obsdists(nnear), &
                                obsx(nnear), obsy(nnear)
    real, allocatable        :: p(:), r(:), w(:)
    real, intent(in)         :: gx(ngrid), gy(ngrid), obsval(nobs)
    real, intent(inout)      :: kriged_val(ngrid)
    
    type(tree_master_record), pointer, intent(in) :: obstree

    ! If there's not many datapoints
    if (obstree%N < nnear) then
      write(*,'(4x,a)') 'Well Data < Kriging Tree Search. Adjusting...'
      usenear = obstree%N
    else
      usenear = nnear
    end if
    
    ntot = usenear + 1              ! +1 for ordinary
    maxpt = ntot * (ntot + 1) / 2
    allocate(p(maxpt),r(ntot),w(ntot),id(ntot))
    
    ! Loop over each grid point
    do i = 1, ngrid
      ! Get nnear closest observation points
      queryxy = (/ gx(i), gy(i) /)
      call n_nearest_to(obstree, queryxy, usenear, closeobs, obsdists)
      do j=1, usenear
        obsx(j) = obstree%THE_DATA(closeobs(j),1)
        obsy(j) = obstree%THE_DATA(closeobs(j),2)
      end do
      
      ! Reset values, matricies for new grid point
      p = 0.d0
      id = 0
      call PSET(p,obsx,obsy,id,usenear,ntot)
      call decomp(p,id,ntot, ier)

      if (ier == 1) then
        write(*,'(2x,a)') 'Error - Kriging Matrix Decomposition Failure.'
        write(*,'(2x,a)') 'This error is usually caused by co-located wells.'
        stop
      end if
      r = 0.d0
      w = 0.d0
      r(ntot) = 1.0d0              ! commented = simple, uncommented = ordinary
      
      ! Loop over observations
      do n = 1, usenear
        call variogram(gx(i),gy(i),0.0d0,obsx(n),obsy(n),0.0d0,value)
        r(n) = value
      end do
      call doolittle(r,p,id,w,ntot)
      
      ! Correct for negative weights, if necessary
      if (minval(w(1:usenear)) < 0.0d0) then
        call ok_negative_correction(w, usenear)
      end if
      
      value = 0.0d0
      zkrig = 0.0d0
      do n=1, usenear
        zkrig = zkrig + w(n) * obsval(closeobs(n))
      end do
      zkrig = zkrig + value
      kriged_val(i) = zkrig
    end do
    
    deallocate(p, r, w, id)

end subroutine spkrige_tree
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine spkrige_tree_group(ngrid, gx, gy, obstree, nobs, obsval, &
                              kriged_val, nnear, ggroup, targetgroup)
    use kd_tree
    implicit none
!-----------------------------------------------------------------------------!
!                       Simple Point Kriging Subroutine
!                     ***********************************
!    
! Groups are set as CHARACTERS(30)
!-----------------------------------------------------------------------------!

    integer                  :: maxpt, ntot, i, j, g, n, closeobs(nnear), &
                                usenear, ier
    integer, allocatable     :: id(:)
    integer, intent(in)      :: ngrid, nnear, nobs
    real                     :: value, zkrig, queryxy(2), obsdists(nnear), &
                                obsx(nnear), obsy(nnear)
    real, allocatable        :: p(:), r(:), w(:)
    real, intent(in)         :: gx(ngrid), gy(ngrid), obsval(nobs)
    real, intent(inout)      :: kriged_val(ngrid)
    character(30),intent(in) :: ggroup(ngrid), targetgroup
    
    type(tree_master_record), pointer, intent(in) :: obstree

    ! If there's not many datapoints
    if (obstree%N < nnear) then
      ! Warning moved outside routine
      !write(*,'(4x,2a)') 'Well Data < Kriging Tree Search in group: ', targetgroup
      usenear = obstree%N
    else
      usenear = nnear
    end if
    
    ntot = usenear + 1              ! +1 for ordinary
    maxpt = ntot * (ntot + 1) / 2
    allocate(p(maxpt),r(ntot),w(ntot),id(ntot))
    
    ! Loop over each grid point
    do i = 1, ngrid
      
      ! Skip if grid point not in group
      if (ggroup(i) /= targetgroup) then
        !kriged_val(i) = nodataval
        cycle
      end if
      
      ! Get nnear closest observation points
      queryxy = (/ gx(i), gy(i) /)
      call n_nearest_to(obstree, queryxy, usenear, closeobs, obsdists)
      do j=1, usenear
        obsx(j) = obstree%THE_DATA(closeobs(j),1)
        obsy(j) = obstree%THE_DATA(closeobs(j),2)
      end do
      
      ! Reset values, matricies for new grid point
      p = 0.d0
      id = 0
      call PSET(p,obsx,obsy,id,usenear,ntot)
      call decomp(p,id,ntot, ier)

      if (ier == 1) then
        write(*,'(2x,a)') 'Error - Kriging Matrix Decomposition Failure.'
        write(*,'(2x,a)') 'This error is usually caused by co-located wells.'
        stop
      end if
      r = 0.d0
      w = 0.d0
      r(ntot) = 1.0d0              ! commented = simple, uncommented = ordinary
      
      ! Loop over observations
      do n = 1, usenear
        call variogram(gx(i),gy(i),0.0d0,obsx(n),obsy(n),0.0d0,value)
        r(n) = value
      end do
      call doolittle(r,p,id,w,ntot)
      
      ! Correct for negative weights, if necessary
      if (minval(w(1:usenear)) < 0.0d0) then
        call ok_negative_correction(w, usenear)
      end if
      
      value = 0.0d0
      zkrig = 0.0d0
      do n=1, usenear
        zkrig = zkrig + w(n) * obsval(closeobs(n))
      end do
      zkrig = zkrig + value
      kriged_val(i) = zkrig
    end do
    
    deallocate(p, r, w, id)

end subroutine spkrige_tree_group
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine spkrige_group(nobs, ngrid, gx, gy, obsx, obsy, obsv, kriged_val, &
                   ggroup, targetgroup)
    implicit none
!-----------------------------------------------------------------------------!
!                       Simple Point Kriging Subroutine
!                     ***********************************
!
! Groups are set as INTEGERS
!-----------------------------------------------------------------------------!

    integer                  :: maxpt, ntot, i, g, n, grouploop, ier
    integer, allocatable     :: id(:)
    integer, intent(in)      :: nobs, ngrid
    real                     :: value, zkrig
    real, allocatable        :: p(:), r(:), w(:)
    real, intent(in)         :: gx(ngrid), gy(ngrid), obsx(nobs), obsy(nobs), &
                                obsv(nobs)
    real, intent(inout)      :: kriged_val(ngrid)
    real, parameter          :: nodataval = -999.0
    ! Arguments for grouped kriging
    integer,intent(in)       :: ggroup(ngrid), targetgroup

    ntot = nobs + 1              ! +1 for ordinary
    maxpt = ntot * (ntot + 1) / 2
    allocate(p(maxpt),r(ntot),w(ntot),id(ntot))
    p = 0.d0
    id = 0
    call PSET(p,obsx,obsy,id,nobs,ntot)
    call decomp(p,id,ntot, ier)

    if (ier == 1) then
      write(*,'(2x,a)') 'Error - Kriging Matrix Decomposition Failure.'
      write(*,'(2x,a)') 'This error is usually caused by co-located pilot points.'
      stop
    end if
    
    ! Loop over each grid point
    do i = 1, ngrid
      ! Skip if grid point not in group
      if (ggroup(i) /= targetgroup) then
        !kriged_val(i) = nodataval
        cycle
      end if
      
      r = 0.d0
      w = 0.d0
      r(ntot) = 1.0d0              ! commented = simple, uncommented = ordinary
      ! Loop over observations
      do n = 1, nobs
        call variogram(gx(i),gy(i),0.0d0,obsx(n),obsy(n),0.0d0,value)
        r(n) = value
      end do
      call doolittle(r,p,id,w,ntot)
      
      ! Correct for negative weights, if necessary
      if (minval(w(1:nobs)) < 0.0d0) then
        call ok_negative_correction(w, nobs)
      end if
      
      value = 0.0d0
      zkrig = 0.0d0
      do n=1, nobs
        zkrig = zkrig + w(n) * obsv(n)
      end do
      zkrig = zkrig + value
      kriged_val(i) = zkrig
    end do

    deallocate(p, r, w, id)

end subroutine
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
  subroutine setanis(a_hmin, a_vert)
    implicit none
    
    real,intent(in)        :: a_hmin, a_vert
    
    ! Sets anis1, anis2 ratios based on ranges aa1, aa2
    anis1 = a_hmin/range
    anis2 = a_vert/range
    
  end subroutine setanis
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
  subroutine ok_negative_correction(wts, nwts)
    implicit none
!-----------------------------------------------------------------------------!
! Use for correcting issues with negative weights, presumably due to the
! screen effect. Follows the advice of Deutsch (1995) "Correcting for Negative
! Weights in Ordinary Kriging" and assigns negative weights to 0 and normalizes
! the weight sum back to one
!-----------------------------------------------------------------------------!
    
    real, intent(inout)    :: wts(nwts)
    integer, intent(in)    :: nwts
    integer                :: i
    real                   :: wtsum
    
    ! Loop over weights, correct negative to positive, add up sum
    wtsum = 0.0
    do i=1, nwts
      if (wts(i) < 0.0d0) wts(i) = 0.0d0
      wtsum = wtsum + wts(i)
    end do
    
    ! Normalize weight sum back to unity
    do i=1, nwts
      wts(i) = wts(i) / wtsum
    end do
  
  end subroutine ok_negative_correction
!-----------------------------------------------------------------------------!
  
!-----------------------------------------------------------------------------!
      subroutine setrot()
      implicit none
!
!              Sets up an Anisotropic Rotation Matrix
!              **************************************
!
! Sets up the matrix to transform cartesian coordinates to coordinates
! accounting for angles and anisotropy (see manual for a detailed
! definition):
!
!
! INPUT PARAMETERS: (moved to module definition)
!
!   ang1             Azimuth angle for principal direction
!   ang2             Dip angle for principal direction
!   ang3             Third rotation angle
!   anis1            First anisotropy ratio
!   anis2            Second anisotropy ratio
!   ind              The matrix indicator to initialize
!   MAXROT           The maximum number of rotation matrices dimensioned
!   rotmat           The rotation matrices
!
!
!
! Author: C. Deutsch                                Date: September 1989
!-----------------------------------------------------------------------
      real*8, parameter         :: DEG2RAD = 3.141592654/180.0, &
                                   EPSLON = 1.e-10
      real*8                    :: alpha, beta, theta, &
                                   sina, sinb, sint, cosa, cosb, cost, &
                                   afac1, afac2
!
! Converts the input angles to three angles which make more
!  mathematical sense:
!
!         alpha   angle between the major axis of anisotropy and the
!                 E-W axis. Note: Counter clockwise is positive.
!         beta    angle between major axis and the horizontal plane.
!                 (The dip of the ellipsoid measured positive down)
!         theta   Angle of rotation of minor axis about the major axis
!                 of the ellipsoid.
!
      if(ang1.ge.0.0d0.and.ang1.lt.270.0d0) then
          alpha = (90.0d0   - ang1) * DEG2RAD
      else
          alpha = (450.0d0  - ang1) * DEG2RAD
      endif
      beta  = -1.0d0 * ang2 * DEG2RAD
      theta =          ang3 * DEG2RAD
!
! Get the required sines and cosines:
!
      sina  = sin(alpha)
      sinb  = sin(beta)
      sint  = sin(theta)
      cosa  = cos(alpha)
      cosb  = cos(beta)
      cost  = cos(theta)
!
! Construct the rotation matrix in the required memory:
!
      afac1 = 1.0d0 / max(anis1,EPSLON)
      afac2 = 1.0d0 / max(anis2,EPSLON)
      rotmat(ind,1,1) =       (cosb * cosa)
      rotmat(ind,1,2) =       (cosb * sina)
      rotmat(ind,1,3) =       (-sinb)
      rotmat(ind,2,1) = afac1*(-cost*sina + sint*sinb*cosa)
      rotmat(ind,2,2) = afac1*(cost*cosa + sint*sinb*sina)
      rotmat(ind,2,3) = afac1*( sint * cosb)
      rotmat(ind,3,1) = afac2*(sint*sina + cost*sinb*cosa)
      rotmat(ind,3,2) = afac2*(-sint*cosa + cost*sinb*sina)
      rotmat(ind,3,3) = afac2*(cost * cosb)
!
! Return to calling program:
!
      return
    end subroutine setrot
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
    SUBROUTINE PSET (P,X,Y,ID,N,ntot)
      implicit none
!***  FROM SUBROUTINE WRITTEN BY SKRIVAN AND KARLINGER AND SLIGHTLY
!***  MODIFIED BY W.D. GRUNDY
!***  CALCULATE COEFFICIENTS IN THE SMETRIC P MATRIX WHICH WILL
!***  CONSIST OF THE LOWER TRIANGULAR PORTION STORED BY COLUMNS.
!***  ID IS THE POINTER VECTOR GIVING THE LOCATION OF THE DIAGONAL
!***  ELEMENTS IN P.
      integer    :: id,ntot,np1,N,MAXPT,i,j,js
      real       :: p,x,y,x1,x2,y1,y2,value
      DIMENSION  :: P(1),X(1),Y(1),ID(1)
!      SAVE
!      NTOT=N+1
      NP1=N+1
!***  ZERO OUT ELEMENTS OF P-MATRIX NEEDED FOR KRIGING
      MAXPT=NTOT*(NTOT+1)/2
      DO 10 I=1,MAXPT
      P(I)=0.
   10 CONTINUE
!***  CALCULATE THE POINTERS FOR ID
      ID(1)=1
      DO 20 I=2,NTOT
   20 ID(I)=ID(I-1)+NTOT+2-I
!***  CALCULATE THE COEFFICIENTS OF THE COVARIANCE OF POINTS I AND J
      DO 30 J=1,N
        JS=ID(J)-J
      DO 30 I=J,N
        x1=x(i)
        y1=y(i)
        x2=x(j)
        y2=y(j)
        call variogram(x1,y1,0.0d0,x2,y2,0.0d0,value)
        P(JS+I)=value
   30 CONTINUE
      DO 40 I=1,N
   40 P(ID(I)+NP1-I)=1
      RETURN
      END SUBROUTINE PSET
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine decomp(p,id,ntot,ier)
    implicit none
!*** DECOMPOSITION TO GET LOWER TRIANGULAR S MATRIX
    integer                :: id,ier,ks,jm1,k,js,is,is1,j,i,i1,ntot
    real                   :: p,sum,p1,p2
    dimension              :: p(1),id(1)

          IER=0
          P(1)=1./P(1)
          DO 65 J=2,NTOT
          DO 65 I=J,NTOT
            KS=ID(I)
            JM1=J-1
            SUM=0.
            DO 55 K=1,JM1
              JS=ID(K)
              IS=JS+I-K
              IS1=JS+J-K
              P1=P(IS)
              P2=P(IS1)
              IF (P1) 39,55,39
   39         IF (P2) 50,55,50
   50         SUM=SUM+P1*P2*P(JS)
   55         CONTINUE
            I1=ID(J)+I-J
            P(I1)=P(I1)-SUM
            IF (I-J) 65,70,65
   70       IF(P(KS)) 90,91,90
   90       P(KS)=1./P(KS)
   65       CONTINUE
          GOTO 92
   91     IER=1
   92     CONTINUE
      return
end subroutine decomp
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine decomp_debug(p,id,ntot,plen)
    implicit none
!*** DECOMPOSITION TO GET LOWER TRIANGULAR S MATRIX
    integer                :: id,ier,ks,jm1,k,js,is,is1,j,i,i1,ntot,plen
    real                   :: p,sum,p1,p2
    dimension              :: p(plen),id(ntot)
    logical                :: debug

      IER=0
      P(1)=1./P(1)
      DO 65 J=2,NTOT
      DO 65 I=J,NTOT
        KS=ID(I)
        JM1=J-1
        SUM=0.
        DO 55 K=1,JM1
          JS=ID(K)
          IS=JS+I-K
          IS1=JS+J-K
          P1=P(IS)
          P2=P(IS1)
          IF (P1) 39,55,39
39         IF (P2) 50,55,50
50         SUM=SUM+P1*P2*P(JS)
55         CONTINUE
        I1=ID(J)+I-J
        P(I1)=P(I1)-SUM
        IF (I-J) 65,70,65
70       IF(P(KS)) 90,91,90
90       P(KS)=1./P(KS)
65       CONTINUE
      GOTO 92
91     IER=1
92     CONTINUE

      if (ier == 1) then
        pause
        stop
      end if
       
      return
      end subroutine decomp_debug
!-----------------------------------------------------------------------------!


!-----------------------------------------------------------------------
subroutine variogram(x1,y1,z1,x2,y2,z2,value)
      implicit none
!      common block/vario/slope,sill,a
      real           :: x1,y1,z1,x2,y2,z2,value,h,sqdist,tmp
      real,parameter :: epsilon=1.e-10
      
      h = sqdist(x1,y1,z1,x2,y2,z2,ind,MAXROT,rotmat)
      
  ! Check for "zero" distance, return with maximum value
      if (h < epsilon) then
        value = nugget + sill
        return
      end if
  ! linear variogram
      if(itype.eq.0) then
        h=sqrt(h)*sill
        !write(11,*) 'Linear', h
      end if
  ! spherical variogram
      if(itype.eq.1) then
        H=SQRT(h)
        tmp=h/range
        if(tmp.ge.1.) then
          h=sill
        else
          h=sill*(1.5*tmp-0.5*tmp*tmp*tmp)
        endif
      endif
  ! exponential variogram
      if (itype.eq.2) then
        H=SQRT(h)
        h=sill*(1.d0-exp(-3.d0*h/range))
      endif
      value=sill-h

end subroutine variogram
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
subroutine doolittle(r,p,id,w,ntot)

    implicit none
          integer   :: id,ntot,i,is,js,im1,j,ii,iip1,iis
          real      :: r,p,w,sum
      dimension     :: r(1),p(1),w(1),id(1)
!***  START OF DOOLITTLE ***
!***  FORWARD SUBSTITUTION FOR SYSTEM S*W = R
          W(1)=R(1)*P(1)
          DO 130 I=2,NTOT
            IS=ID(I)
            SUM=0.
            IM1=I-1
            DO 120 J=1,IM1
              JS=ID(J)+I-J
  120       SUM=SUM+W(J)*P(JS)
  130     W(I)=(R(I)-SUM)*P(IS)
!***  BACKWARD SUBSTITUTION FOR SYSTEM T*V = W
!***  WHERE V IS STORED IN W
          DO 150 I=2,NTOT
            SUM=0.
            II=NTOT-I+1
            JS=ID(II)
            IIP1=II+1
            DO 140 J=IIP1,NTOT
              IIS=JS+J-II
  140       SUM=SUM+W(J)*P(IIS)
  150     W(II)=W(II)-SUM*P(JS)
!*** END OF DOOLITTLE ****
      return

end subroutine doolittle
!-----------------------------------------------------------------------------!

  END MODULE Krige
  
!-----------------------------------------------------------------------------!
real*8 function sqdist(x1,y1,z1,x2,y2,z2,ind,MAXROT,rotmat)
      implicit none
!
!    Squared Anisotropic Distance Calculation Given Matrix Indicator
!    ***************************************************************
!
! This routine calculates the anisotropic distance between two points
!  given the coordinates of each point and a definition of the
!  anisotropy.
!
! INPUT VARIABLES:
!   x1,y1,z1         Coordinates of first point
!   x2,y2,z2         Coordinates of second point
!   ind              The matrix indicator to initialize
!   MAXROT           The maximum number of rotation matrices dimensioned
!   rotmat           The rotation matrices
!                  
!
! OUTPUT VARIABLES:
!   sqdist           The squared distance accounting for the anisotropy
!                      and the rotation of coordinates (if any).
!
!
! Author: C. Deutsch                                Date: September 1989
      integer   i,ind,MAXROT
      real*8    rotmat(MAXROT,3,3),dx,dy,dz,x1,y1,x2,y2,z1,z2,cont
!
! Compute component distance vectors and the squared distance:
!
      dx = x1 - x2
      dy = y1 - y2
      dz = z1 - z2
          sqdist = 0.0d0
      do i=1,3
        cont   = rotmat(ind,i,1) * dx &
               + rotmat(ind,i,2) * dy &
               + rotmat(ind,i,3) * dz
        sqdist = sqdist + cont * cont
      end do
      return

end function sqdist
!-----------------------------------------------------------------------------!