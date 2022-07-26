MODULE Module_PGMRES
    !$ USE OMP_LIB
    IMPLICIT NONE
!----------------------------------------------------------------------c
!   Name:    pgmres.f						                           c
!   Date:    6/30/09						                           c
!   Version: 1.1						                               c
!   Author:  mfdixon@ucdavis.edu				                       c
!   File dependences: none					                           c
!----------------------------------------------------------------------c
!
!----------------------------------------------------------------------c
!  This file has the following subroutines from SPARSKIT	           c
!								                                       c
!  GMRES: GMRES(m) -Generalized Minimum RESidual method with restart   c
!  BISINT:GMRES initialization routine				                   c
!  GIVENS:Givens rotation	 				                           c
!  MGSRO: Modified Gram-Schmidt Algorithm			                   c
!  ILUT : Incomplete LU factorization with dual truncation strategy    c
!  QSPLIT:Quick split routine used by ilutp to sort out the k largest  c
!         elements in absolute value				                   c
!  AMUX:  CRS format Matrix Vector Multiplication	                   c
!  LUSOL: LU solve						                               c
!  BLAS 1 Extras						                               c
!     distdot:  ddot interface					                       c
!     ddot:  vector dot product					                       c
!     dnrm2: vector 2-norm					                           c
!----------------------------------------------------------------------c
!
    PRIVATE
    PUBLIC :: GMRES , &
              AMUX  , &
              LUSOL , &
              ILUT  , &
              DNRM2 

    
CONTAINS
    
    
      subroutine gmres(n, rhs, sol, ipar, fpar, w)
!----------------------------------------------------------------------c
!     This a version of GMRES implemented with reverse communication.
!     It is a simple restart version of the GMRES algorithm.
!
!     ipar(5) == the dimension of the Krylov subspace
!     after every ipar(5) iterations, the GMRES will restart with
!     the updated solution and recomputed residual vector.
!
!     the space of the `w' is used as follows:
!     (1) the basis for the Krylov subspace, size n*(m+1);
!     (2) the Hessenberg matrix, only the upper triangular
!     portion of the matrix is stored, size (m+1)*m/2 + 1
!     (3) three vectors, all are of size m, they are
!     the cosine and sine of the Givens rotations, the third one holds
!     the residuals, it is of size m+1.
!
!     TOTAL SIZE REQUIRED == (n+3)*(m+2) + (m+1)*m/2
!     Note: m == ipar(5). The default value for this is 15 if
!     ipar(5) <= 1.
!-----------------------------------------------------------------------
      integer n, ipar(16)
      real(8) rhs(n), sol(n), fpar(16), w(*)

!
      real(8) one, zero
      parameter(one=1.0D0, zero=0.0D0)
!
!     local variables, ptr and p2 are temporary pointers,
!     hess points to the Hessenberg matrix,
!     vc, vs point to the cosines and sines of the Givens rotations
!     vrn points to the vectors of residual norms, more precisely
!     the right hand side of the least square problem solved.
!
      integer i,ii,idx,k,m,ptr,p2,hess,vc,vs,vrn
      real(8) alpha, c, s
      logical lp, rp
      save
!
!     check the status of the call
!
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 30, 40, 50, 60, 70) ipar(10)
!
!     initialization
!
      if (ipar(5).le.1) then
         m = 15
      else
         m = ipar(5)
      endif
      idx = n * (m+1)
      hess = idx + n
      vc = hess + (m+1) * m / 2 + 1
      vs = vc + m
      vrn = vs + m
      i = vrn + m + 1
      call bisinit(ipar,fpar,i,1,lp,rp,w)
      if (ipar(1).lt.0) return
!
!     request for matrix vector multiplication A*x in the initialization
!
 100  ipar(1) = 1
      ipar(8) = n+1
      ipar(9) = 1
      ipar(10) = 1
      k = 0
      do i = 1, n
         w(n+i) = sol(i)
      enddo
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      if (lp) then
         do i = 1, n
            w(n+i) = rhs(i) - w(i)
         enddo
         ipar(1) = 3
         ipar(10) = 2
         return
      else
         do i = 1, n
            w(i) = rhs(i) - w(i)
         enddo
      endif
      fpar(11) = fpar(11) + n
!
 20   alpha = sqrt(distdot(n,w,1,w,1))
      fpar(11) = fpar(11) + 2*n
      if (ipar(7).eq.1 .and. ipar(3).ne.999) then
         if (abs(ipar(3)).eq.2) then
            fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
            fpar(11) = fpar(11) + 2*n
         else
            fpar(4) = fpar(1) * alpha + fpar(2)
         endif
         fpar(3) = alpha
      endif
      fpar(5) = alpha
      w(vrn+1) = alpha
      if (alpha.le.fpar(4) .and. ipar(3).ge.0 .and. ipar(3).ne.999) then
         ipar(1) = 0
         fpar(6) = alpha
         goto 300
      endif
      alpha = one / alpha
      do ii = 1, n
         w(ii) = alpha * w(ii)
      enddo
      fpar(11) = fpar(11) + n
!
!     request for (1) right preconditioning
!     (2) matrix vector multiplication
!     (3) left preconditioning
!
 110  k = k + 1
      if (rp) then
         ipar(1) = 5
         ipar(8) = k*n - n + 1
         if (lp) then
            ipar(9) = k*n + 1
         else
            ipar(9) = idx + 1
         endif
         ipar(10) = 3
         return
      endif
!
 30   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = (k-1)*n + 1
      endif
      if (lp) then
         ipar(9) = idx + 1
      else
         ipar(9) = 1 + k*n
      endif
      ipar(10) = 4
      return
!
 40   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = k*n + 1
         ipar(10) = 5
         return
      endif
!
!     Modified Gram-Schmidt orthogonalization procedure
!     temporary pointer 'ptr' is pointing to the current column of the
!     Hessenberg matrix. 'p2' points to the new basis vector
!
 50   ipar(7) = ipar(7) + 1
      ptr = k * (k - 1) / 2 + hess
      p2 = ipar(9)
      call mgsro(.false.,n,n,k+1,k+1,fpar(11),w,w(ptr+1),ipar(12))
      if (ipar(12).lt.0) goto 200
!
!     apply previous Givens rotations and generate a new one to eliminate
!     the subdiagonal element.
!
      p2 = ptr + 1
      do i = 1, k-1
         ptr = p2
         p2 = p2 + 1
         alpha = w(ptr)
         c = w(vc+i)
         s = w(vs+i)
         w(ptr) = c * alpha + s * w(p2)
         w(p2) = c * w(p2) - s * alpha
      enddo
      call givens(w(p2), w(p2+1), c, s)
      w(vc+k) = c
      w(vs+k) = s
      p2 = vrn + k
      alpha = - s * w(p2)
      w(p2) = c * w(p2)
      w(p2+1) = alpha
!
!     end of one Arnoldi iteration, alpha will store the estimated
!     residual norm at current stage
!
      fpar(11) = fpar(11) + 6*k + 2
      alpha = abs(alpha)
      fpar(5) = alpha
      if (k.lt.m .and. .not.(ipar(3).ge.0 .and. alpha.le.fpar(4))  &
           .and. (ipar(6).le.0 .or. ipar(7).lt.ipar(6))) goto 110
!
!     update the approximate solution, first solve the upper triangular
!     system, temporary pointer ptr points to the Hessenberg matrix,
!     p2 points to the right-hand-side (also the solution) of the system.
!
 200  ptr = hess + k * (k + 1) / 2
      p2 = vrn + k
      if (w(ptr).eq.zero) then
!
!     if the diagonal elements of the last column is zero, reduce k by 1
!     so that a smaller trianguler system is solved [It should only
!     happen when the matrix is singular, and at most once!]
!
         k = k - 1
         if (k.gt.0) then
            goto 200
         else
            ipar(1) = -3
            ipar(12) = -4
            goto 300
         endif
      endif
      w(p2) = w(p2) / w(ptr)
      do i = k-1, 1, -1
         ptr = ptr - i - 1
         do ii = 1, i
            w(vrn+ii) = w(vrn+ii) - w(p2) * w(ptr+ii)
         enddo
         p2 = p2 - 1
         w(p2) = w(p2) / w(ptr)
      enddo
!
      do ii = 1, n
         w(ii) = w(ii) * w(p2)
      enddo
      do i = 1, k-1
         ptr = i*n
         p2 = p2 + 1
         do ii = 1, n
            w(ii) = w(ii) + w(p2) * w(ptr+ii)
         enddo
      enddo
      fpar(11) = fpar(11) + 2*k*n - n + k*(k+1)
!
      if (rp) then
         ipar(1) = 5
         ipar(8) = 1
         ipar(9) = idx + 1
         ipar(10) = 6
         return
      endif
!
 60   if (rp) then
         do i = 1, n
            sol(i) = sol(i) + w(idx+i)
         enddo
      else
         do i = 1, n
            sol(i) = sol(i) + w(i)
         enddo
      endif
      fpar(11) = fpar(11) + n
!
!     process the complete stopping criteria
!
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = -1
         ipar(9) = idx + 1
         ipar(10) = 7
         return
      else if (ipar(3).lt.0) then
         if (ipar(7).le.m+1) then
            fpar(3) = abs(w(vrn+1))
            if (ipar(3).eq.-1) fpar(4) = fpar(1)*fpar(3)+fpar(2)
         endif
         fpar(6) = abs(w(vrn+k))
      else
         fpar(6) = fpar(5)
      endif
!
!     do we need to restart ?
!
 70   if (ipar(12).ne.0) then
         ipar(1) = -3
         goto 300
      endif
      if ((ipar(7).lt.ipar(6) .or. ipar(6).le.0) .and.  &
           ((ipar(3).eq.999.and.ipar(11).eq.0) .or.     &
           (ipar(3).ne.999.and.fpar(6).gt.fpar(4)))) goto 100
!
!     termination, set error code, compute convergence rate
!
      if (ipar(1).gt.0) then
         if (ipar(3).eq.999 .and. ipar(11).eq.1) then
            ipar(1) = 0
         else if (ipar(3).ne.999 .and. fpar(6).le.fpar(4)) then
            ipar(1) = 0
         else if (ipar(7).ge.ipar(6) .and. ipar(6).gt.0) then
            ipar(1) = -1
         else
            ipar(1) = -10
         endif
      endif
 300  if (fpar(3).ne.zero .and. fpar(6).ne.zero .and. ipar(7).gt.ipar(13)) then
         fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
      else
         fpar(7) = zero
      endif
      return
      end
!-----end-of-gmres

      
!-----------------------------------------------------------------------
      subroutine givens(x,y,c,s)
      implicit none
      real(8) x,y,c,s
!-----------------------------------------------------------------------
!     Given x and y, this subroutine generates a Givens' rotation c, s.
!     And apply the rotation on (x,y) ==> (sqrt(x**2 + y**2), 0).
!     (See P 202 of "matrix computation" by Golub and van Loan.)
!-----------------------------------------------------------------------
      real(8) t,one,zero
      parameter (zero=0.0D0,one=1.0D0)
!
      if (x.eq.zero .and. y.eq.zero) then
         c = one
         s = zero
      else if (abs(y).gt.abs(x)) then
         t = x / y
         x = sqrt(one+t*t)
         s = sign(one / x, y)
         c = t*s
      else if (abs(y).le.abs(x)) then
         t = y / x
         y = sqrt(one+t*t)
         c = sign(one / y, x)
         s = t*c
      else
!
!     X or Y must be an invalid floating-point number, set both to zero
!
         x = zero
         y = zero
         c = one
         s = zero
      endif
      x = abs(x*y)
!
!     end of givens
!
      return
      end
!-----end-of-givens

      
!-----------------------------------------------------------------------
      subroutine bisinit(ipar,fpar,wksize,dsc,lp,rp,wk)
      implicit none
      integer i,ipar(16),wksize,dsc
      logical lp,rp
      real(8)  fpar(16),wk(*)
!-----------------------------------------------------------------------
!     some common initializations for the iterative solvers
!-----------------------------------------------------------------------
      real(8) zero, one
      parameter(zero=0.0D0, one=1.0D0)
!
!     ipar(1) = -2 inidcate that there are not enough space in the work
!     array
!
      if (ipar(4).lt.wksize) then
         ipar(1) = -2
         ipar(4) = wksize
         return
      endif
!
      if (ipar(2).gt.2) then
         lp = .true.
         rp = .true.
      else if (ipar(2).eq.2) then
         lp = .false.
         rp = .true.
      else if (ipar(2).eq.1) then
         lp = .true.
         rp = .false.
      else
         lp = .false.
         rp = .false.
      endif
      if (ipar(3).eq.0) ipar(3) = dsc
!     .. clear the ipar elements used
      ipar(7) = 0
      ipar(8) = 0
      ipar(9) = 0
      ipar(10) = 0
      ipar(11) = 0
      ipar(12) = 0
      ipar(13) = 0
!
!     fpar(1) must be between (0, 1), fpar(2) must be positive,
!     fpar(1) and fpar(2) can NOT both be zero
!     Normally return ipar(1) = -4 to indicate any of above error
!
      if (fpar(1).lt.zero .or. fpar(1).ge.one .or. fpar(2).lt.zero .or.  (fpar(1).eq.zero .and. fpar(2).eq.zero)) then
         if (ipar(1).eq.0) then
            ipar(1) = -4
            return
         else
            fpar(1) = 1.0D-6
            fpar(2) = 1.0D-16
         endif
      endif
!     .. clear the fpar elements
      do i = 3, 10
         fpar(i) = zero
      enddo
      if (fpar(11).lt.zero) fpar(11) = zero
!     .. clear the used portion of the work array to zero
      do i = 1, wksize
         wk(i) = zero
      enddo
!
      return
      end
!-----end-of-bisinit--------------------------------------

      
!-----------------------------------------------------------------------
      subroutine mgsro(full,lda,n,m,ind,ops,vec,hh,ierr)
      implicit none
      logical full
      integer lda,m,n,ind,ierr
      real(8)  ops,hh(m),vec(lda,m)
!-----------------------------------------------------------------------
!     MGSRO  -- Modified Gram-Schmidt procedure with Selective Re-
!               Orthogonalization
!     The ind'th vector of VEC is orthogonalized against the rest of
!     the vectors.
!
!     The test for performing re-orthogonalization is performed for
!     each indivadual vectors. If the cosine between the two vectors
!     is greater than 0.99 (REORTH = 0.99**2), re-orthogonalization is
!     performed. The norm of the 'new' vector is kept in variable NRM0,
!     and updated after operating with each vector.
!
!     full   -- .ture. if it is necessary to orthogonalize the ind'th
!               against all the vectors vec(:,1:ind-1), vec(:,ind+2:m)
!               .false. only orthogonalize againt vec(:,1:ind-1)
!     lda    -- the leading dimension of VEC
!     n      -- length of the vector in VEC
!     m      -- number of vectors can be stored in VEC
!     ind    -- index to the vector to be changed
!     ops    -- operation counts
!     vec    -- vector of LDA X M storing the vectors
!     hh     -- coefficient of the orthogonalization
!     ierr   -- error code
!               0 : successful return
!               -1: zero input vector
!               -2: input vector contains abnormal numbers
!               -3: input vector is a linear combination of others
!
!     External routines used: real*8 distdot
!-----------------------------------------------------------------------
      integer i,k
      real(8)  nrm0, nrm1, fct, thr, zero, one, reorth
      parameter (zero=0.0D0, one=1.0D0, reorth=0.98D0)
!
!     compute the norm of the input vector
!
      nrm0 = distdot(n,vec(1,ind),1,vec(1,ind),1)
      ops = ops + n + n
      thr = nrm0 * reorth
      if (nrm0.le.zero) then
         ierr = - 1
         return
      else if (nrm0.gt.zero .and. one/nrm0.gt.zero) then
         ierr = 0
      else
         ierr = -2
         return
      endif
!
!     Modified Gram-Schmidt loop
!
      if (full) then
         do 40 i = ind+1, m
            fct = distdot(n,vec(1,ind),1,vec(1,i),1)
            hh(i) = fct
            do 20 k = 1, n
               vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 20         continue
            ops = ops + 4 * n + 2
            if (fct*fct.gt.thr) then
               fct = distdot(n,vec(1,ind),1,vec(1,i),1)
               hh(i) = hh(i) + fct
               do 30 k = 1, n
                  vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 30            continue
               ops = ops + 4*n + 1
            endif
            nrm0 = nrm0 - hh(i) * hh(i)
            if (nrm0.lt.zero) nrm0 = zero
            thr = nrm0 * reorth
 40      continue
      endif
!
      do 70 i = 1, ind-1
         fct = distdot(n,vec(1,ind),1,vec(1,i),1)
         hh(i) = fct
         do 50 k = 1, n
            vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 50      continue
         ops = ops + 4 * n + 2
         if (fct*fct.gt.thr) then
            fct = distdot(n,vec(1,ind),1,vec(1,i),1)
            hh(i) = hh(i) + fct
            do 60 k = 1, n
               vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 60         continue
            ops = ops + 4*n + 1
         endif
         nrm0 = nrm0 - hh(i) * hh(i)
         if (nrm0.lt.zero) nrm0 = zero
         thr = nrm0 * reorth
 70   continue
!
!     test the resulting vector
!
      nrm1 = sqrt(distdot(n,vec(1,ind),1,vec(1,ind),1))
      ops = ops + n + n
 75   hh(ind) = nrm1
      if (nrm1.le.zero) then
         ierr = -3
         return
      endif
!
!     scale the resulting vector
!
      fct = one / nrm1
      do 80 k = 1, n
         vec(k,ind) = vec(k,ind) * fct
 80   continue
      ops = ops + n + 1
!
!     normal return
!
      ierr = 0
      return
!
      end
!---------end mgsro-----------------------------------------------------

      
!-----------------------------------------------------------------------
    SUBROUTINE ilut(n,a,ja,ia,lfil,droptol,alu,jlu,ju,iwk,w,jw,ierr)
      INTEGER,INTENT(IN)             :: n,lfil,iwk,ia(n+1)
      REAL(8),INTENT(IN)             :: droptol
      REAL(8),CONTIGUOUS,INTENT(IN)  :: a(:)
      INTEGER,CONTIGUOUS,INTENT(IN)  :: ja(:)
      REAL(8),INTENT(OUT)            :: alu(iwk),w(n+1)
      INTEGER,INTENT(OUT)            :: jlu(iwk),jw(2*n),ju(n),ierr

      !Locals
      integer :: ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,iLen
      real(8) :: tnorm, t, s, fact

      !Check for illegal lfil
      if (lfil .lt. 0) then
          ierr = -4
          return
      end if  
      !-----------------------------------------------------------------------
      ! Initialize ju0 (points to next element to be added to alu,jlu)
      ! and pointer array.
      !-----------------------------------------------------------------------
      ju0    = n+2
      jlu(1) = ju0
      !
      !Initialize nonzero indicator array.
      !
      jw(n+1:) = 0
      
      !-----------------------------------------------------------------------
      !Beginning of main loop.
      !-----------------------------------------------------------------------
      MAIN_LOOP:  &
      do ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
         tnorm = 0.0d0
         do k=j1,j2
            tnorm = tnorm+dabs(a(k))
         end do
         
         !Zero row encountered; return with error
         if (tnorm .eq. 0.0) then
             ierr = -5-ii
             return
         end if
         tnorm = tnorm/real(j2-j1+1)
         !
         !Unpack L-part and U-part of row of A in arrays w
         !
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii

         do j = j1, j2
            k = ja(j)
            t = a(j)
            if (k .lt. ii) then
               lenl = lenl+1
               jw(lenl) = k
               w(lenl) = t
               jw(n+k) = lenl
            else if (k .eq. ii) then
               w(ii) = t
            else
               lenu = lenu+1
               jpos = ii+lenu-1
               jw(jpos) = k
               w(jpos) = t
               jw(n+k) = jpos
            endif
         end do
         jj = 0
         iLen = 0
         !
         !Eliminate previous rows
         !
         DO
             jj = jj+1
             if (jj .gt. lenl) EXIT
             !-----------------------------------------------------------------------
             ! in order to do the elimination in the correct order we must select
             ! the smallest column index among jw(k), k=jj+1, ..., lenl.
             !-----------------------------------------------------------------------
             jrow = jw(jj)
             k = jj
             !
             !determine smallest column index
             !
             do j=jj+1,lenl
                if (jw(j) .lt. jrow) then
                   jrow = jw(j)
                   k = j
                endif
             end do
             
             if (k .ne. jj) then
                !Exchange in jw
                j = jw(jj)
                jw(jj) = jw(k)
                jw(k) = j
                
                !Exchange in jr
                jw(n+jrow) = jj
                jw(n+j) = k
                !Exchange in w
                s = w(jj)
                w(jj) = w(k)
                w(k) = s
             endif
             !
             !Zero out element in row by setting jw(n+jrow) to zero.
             !
             jw(n+jrow) = 0
             !
             !Get the multiplier for row to be eliminated (jrow).
             !
             fact = w(jj)*alu(jrow)
             if (dabs(fact) .le. droptol) CYCLE
             !
             !Combine current row and row jrow
             !
             do k = ju(jrow), jlu(jrow+1)-1
                s = fact*alu(k)
                j = jlu(k)
                jpos = jw(n+j)
                if (j .ge. ii) then
                   !
                   !Dealing with upper part.
                   !
                   if (jpos .eq. 0) then
                      !
                      !This is a fill-in element
                      !
                      lenu = lenu+1
                      !Incomprehensible error
                      if (lenu .gt. n) then
                          ierr = -1
                          return
                      end if
                      i = ii+lenu-1
                      jw(i) = j
                      jw(n+j) = i
                      w(i) = - s
                   else
                      !
                      !This is not a fill-in element
                      !
                      w(jpos) = w(jpos) - s
                   endif
                else
                   !
                   !Dealing  with lower part.
                   !
                   if (jpos .eq. 0) then
                      !
                      !This is a fill-in element
                      !
                      lenl = lenl+1
                      !Incomprehensible error
                      if (lenl .gt. n) then
                          ierr = -1
                          return
                      end if
                      jw(lenl) = j
                      jw(n+j) = lenl
                      w(lenl) = - s
                   else
                      !
                      !This is not a fill-in element
                      !
                      w(jpos) = w(jpos) - s
                   endif
                endif
             end do
             !
             ! Store this pivot element -- (from left to right -- no danger of
             ! overlap with the working elements in L (pivots).
             ! 
             iLen = iLen+1
             w(iLen) = fact
             jw(iLen)  = jrow
         END DO

         !
         !Reset double-pointer to zero (U-part)
         !
         do k=1, lenu
            jw(n+jw(ii+k-1)) = 0
         end do
         !
         !Update L-matrix
         !
         lenl = iLen
         iLen = min0(lenl,lfil)
         !
         !Sort by quick-split
         !
         call qsplit (w,jw,lenl,iLen)
         !
         !Store L-part
         !
         do k=1, iLen
             !Insufficent storage in L
             if (ju0 .gt. iwk) then
                 ierr = -2
                 return
             end if
             alu(ju0) =  w(k)
             jlu(ju0) =  jw(k)
             ju0 = ju0+1
         end do
         !
         !Save pointer to beginning of row ii of U
         !
         ju(ii) = ju0
         !
         !Update U-matrix -- first apply dropping strategy
         !
         iLen = 0
         do k=1, lenu-1
            if (dabs(w(ii+k)) .gt. droptol*tnorm) then
               iLen = iLen+1
               w(ii+iLen) = w(ii+k)
               jw(ii+iLen) = jw(ii+k)
            endif
         enddo
         lenu = iLen+1
         iLen = min0(lenu,lfil)

         call qsplit (w(ii+1), jw(ii+1), lenu-1,iLen)
         !
         !Copy
         !
         t = dabs(w(ii))
         !Insufficent storage in U
         if (iLen + ju0 .gt. iwk) then
             ierr = -3
             return
         end if 
         do k=ii+1,ii+iLen-1
            jlu(ju0) = jw(k)
            alu(ju0) = w(k)
            t = t + dabs(w(k) )
            ju0 = ju0+1
         end do
         !
         !Store inverse of diagonal element of u
         !
         if (w(ii) .eq. 0.0) w(ii) = (0.0001 + droptol)*tnorm

         alu(ii) = 1.0d0/ w(ii)
         !
         !Update pointer to beginning of next row of U.
         !
         jlu(ii+1) = ju0

      END DO MAIN_LOOP
      ierr = 0

    END SUBROUTINE ilut
!----------------end-of-ilut--------------------------------------------


!-----------------------------------------------------------------------
    SUBROUTINE qsplit(a,ind,n,ncut)
      INTEGER :: n, ind(n), ncut
      REAL(8) :: a(n)
      !-----------------------------------------------------------------------
      !     does a quick-sort split of a real array.
      !     on input a(1:n). is a real array
      !     on output a(1:n) is permuted such that its elements satisfy:
      !
      !     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
      !     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
      !
      !     ind(1:n) is an integer array which permuted in the same way as a(*).
      !-----------------------------------------------------------------------
      REAL(8) :: tmp, abskey
      INTEGER :: itmp, first, last, j, mid

      first = 1
      last = n
      IF (ncut .lt. first .or. ncut .gt. last) RETURN
      !
      !Outer loop -- while mid .ne. ncut do
      !
      DO
          mid = first
          abskey = DABS(a(mid))
          DO j=first+1, last
             IF (DABS(a(j)) .gt. abskey) THEN
                mid = mid+1
                !Interchange
                tmp = a(mid)
                itmp = ind(mid)
                a(mid) = a(j)
                ind(mid) = ind(j)
                a(j)  = tmp
                ind(j) = itmp
             ENDIF
          END DO
          !
          !Interchange
          !
          tmp = a(mid)
          a(mid) = a(first)
          a(first)  = tmp
          
          itmp = ind(mid)
          ind(mid) = ind(first)
          ind(first) = itmp
          !
          !Test for while loop
          !
          IF (mid .eq. ncut) EXIT
          IF (mid .gt. ncut) THEN
             last = mid-1
          ELSE
             first = mid+1
          ENDIF
      END DO
!----------------end-of-qsplit------------------------------------------
    END

        
!-----------------------------------------------------------------------
    SUBROUTINE amux (n, x, y, a, ja, ia)
      INTEGER,INTENT(IN)            :: n
      INTEGER,CONTIGUOUS,INTENT(IN) :: ja(:),ia(:)
      REAL(8),CONTIGUOUS,INTENT(IN) :: a(:),x(:)
      REAL(8),CONTIGUOUS,INTENT(OUT):: y(:)
      
      !Locals
      REAL(8) :: t
      INTEGER :: i, k

      !$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i,t,k) SCHEDULE(STATIC,128)
      DO i = 1,n
         !Compute the inner product of row i with vector x
         t = 0.0
         DO k=ia(i), ( ia(i+1)-1 )
            t = t + a(k)*x(ja(k))
         END DO

         !Store result in y(i)
         y(i) = t
      END DO
      !$OMP END PARALLEL DO
      
    END SUBROUTINE amux
!---------end-of-amux---------------------------------------------------

    
!-----------------------------------------------------------------------
      subroutine lusol(n, y, x, alu, jlu, ju)
        implicit none
        integer :: n, jlu(*), ju(*)
        real(8) :: x(n), y(n), alu(*)
!-----------------------------------------------------------------------
!
! This routine solves the system (LU) x = y,
! given an LU decomposition of a matrix stored in (alu, jlu, ju)
! modified sparse row format
!
!-----------------------------------------------------------------------
! on entry:
! n   = dimension of system
! y   = the right-hand-side vector
! alu, jlu, ju
!     = the LU matrix as provided from the ILU routines.
!
! on return
! x   = solution of LU x = y.
!-----------------------------------------------------------------------
!
! Note: routine is in place: call lusol (n, x, x, alu, jlu, ju)
!       will solve the system with rhs x and overwrite the result on x .
!
!-----------------------------------------------------------------------
! local variables
!
        integer i,k
!
! forward solve
!
        do 40 i = 1, n
           x(i) = y(i)
           do 41 k=jlu(i),ju(i)-1
              x(i) = x(i) - alu(k)* x(jlu(k))
 41        continue
 40     continue
!
!     backward solve.
!
      do i = n, 1, -1
         do k=ju(i),jlu(i+1)-1
              x(i) = x(i) - alu(k)*x(jlu(k))
         end do
          x(i) = alu(i)*x(i)
      end do
!
      return
      end
!----------------end of lusol ------------------------------------------

      
!-----------------------------------------------------------------------
    REAL(8) FUNCTION distdot(n,x,ix,y,iy)
      INTEGER :: n, ix, iy
      REAL(8) :: x(*), y(*)

      distdot = ddot(n,x,ix,y,iy)

    END FUNCTION distdot
!-----end-of-distdot-----------------------------------------------------


!-----------------------------------------------------------------------
    REAL(8) FUNCTION ddot(n,dx,incx,dy,incy)
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      REAL(8),INTENT(IN) :: dx(*),dy(*)
      INTEGER,INTENT(IN) :: incx,incy,n
      
      !Locals
      INTEGER :: i,ix,iy,m,mp1
      REAL(8) :: dtemp
!
      ddot = 0.0d0
      dtemp = 0.0d0
      IF (n.le.0) RETURN
      IF (incx.eq.1.and.incy.eq.1) GO TO 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix = 1
      iy = 1
      IF (incx.lt.0) ix = (-n+1)*incx + 1
      IF (incy.lt.0) iy = (-n+1)*incy + 1
      DO i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
      END DO
      ddot = dtemp
      RETURN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 m = MOD(n,5)
      IF ( m .eq. 0 ) GO TO 40
      DO i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
      END DO
      IF ( n .lt. 5 ) GO TO 60
40    mp1 = m + 1
      DO i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) + dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
      END DO
   60 ddot = dtemp
    END FUNCTION ddot
!---------end-of-ddot---------------------------------------------------

      
!------------------------------------------------------------------
      real(8) function dnrm2 ( n, dx, incx)
      implicit none
      integer          incx, next, n, nn, i, j
      real(8)   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one
      data   zero, one /0.0d0, 1.0d0/

      data cutlo, cuthi / 8.232d-11,  1.304d19 /
!
      if(n .gt. 0) go to 10
         dnrm2  = zero
         go to 300
!
   10 assign 30 to next
      sum = zero
      nn = n * incx
!                                                 begin main loop
      i = 1
   20    go to next,(30, 50, 70, 110)
   30 if( dabs(dx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
!
!                        phase 1.  sum is zero
!
   50 if( dx(i) .eq. zero) go to 200
      if( dabs(dx(i)) .gt. cutlo) go to 85
!
!                                prepare for phase 2.
      assign 70 to next
      go to 105
!
!                                prepare for phase 4.
!
  100 i = j
      assign 110 to next
      sum = (sum / dx(i)) / dx(i)
  105 xmax = dabs(dx(i))
      go to 115
!
!                   phase 2.  sum is small.
!                             scale to avoid destructive underflow.
!
   70 if( dabs(dx(i)) .gt. cutlo ) go to 75
!
!                     common code for phases 2 and 4.
!                     in phase 4 sum is large.  scale to avoid overflow.
!
  110 if( dabs(dx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / dx(i))**2
         xmax = dabs(dx(i))
         go to 200
!
  115 sum = sum + (dx(i)/xmax)**2
      go to 200
!
!
!                  prepare for phase 3.
!
   75 sum = (sum * xmax) * xmax
!
!
!     for real or d.p. set hitest = cuthi/n
!     for complex      set hitest = cuthi/(2*n)
!
   85 hitest = cuthi/float( n )
!
!                   phase 3.  sum is mid-range.  no scaling.
!
      do j =i,nn,incx
          if(dabs(dx(j)) .ge. hitest) go to 100
          sum = sum + dx(j)**2
      end do
      dnrm2 = dsqrt( sum )
      go to 300
!
  200 continue
      i = i + incx
      if ( i .le. nn ) go to 20
!
!              end of main loop.
!
!              compute square root and adjust for scaling.
!
      dnrm2 = xmax * dsqrt(sum)
  300 continue
      return
      end
!--------end-of-dnrm2-------------------------------------------

END MODULE
