c----------------------------------------------------------------------c
c   Name:    pgmres.f						                           c
c   Date:    6/30/09						                           c
c   Version: 1.1						                               c
c   Author:  mfdixon@ucdavis.edu				                       c
c   File dependences: none					                           c
c----------------------------------------------------------------------c
c
c----------------------------------------------------------------------c
c  This file has the following subroutines from SPARSKIT	           c
c								                                       c
c  GMRES: GMRES(m) -Generalized Minimum RESidual method with restart   c
c  BISINT:GMRES initialization routine				                   c
c  GIVENS:Givens rotation	 				                           c
c  MGSRO: Modified Gram-Schmidt Algorithm			                   c
c  ILUT : Incomplete LU factorization with dual truncation strategy    c
c  QSPLIT:Quick split routine used by ilutp to sort out the k largest  c
c         elements in absolute value				                   c
c  AMUX:  CRS format Matrix Vector Multiplication	                   c
c  LUSOL: LU solve						                               c
c  BLAS 1 Extras						                               c
c     distdot:  ddot interface					                       c
c     ddot:  vector dot product					                       c
c     dnrm2: vector 2-norm					                           c
c----------------------------------------------------------------------c
c
      subroutine gmres(n, rhs, sol, ipar, fpar, w)
c----------------------------------------------------------------------c
c     This a version of GMRES implemented with reverse communication.
c     It is a simple restart version of the GMRES algorithm.
c
c     ipar(5) == the dimension of the Krylov subspace
c     after every ipar(5) iterations, the GMRES will restart with
c     the updated solution and recomputed residual vector.
c
c     the space of the `w' is used as follows:
c     (1) the basis for the Krylov subspace, size n*(m+1);
c     (2) the Hessenberg matrix, only the upper triangular
c     portion of the matrix is stored, size (m+1)*m/2 + 1
c     (3) three vectors, all are of size m, they are
c     the cosine and sine of the Givens rotations, the third one holds
c     the residuals, it is of size m+1.
c
c     TOTAL SIZE REQUIRED == (n+3)*(m+2) + (m+1)*m/2
c     Note: m == ipar(5). The default value for this is 15 if
c     ipar(5) <= 1.
c-----------------------------------------------------------------------
      implicit none
      integer n, ipar(16)
      real(8) rhs(n), sol(n), fpar(16), w(*)

c     external functions used
c
      real(8) distdot
      external distdot
c
      real(8) one, zero
      parameter(one=1.0D0, zero=0.0D0)
c
c     local variables, ptr and p2 are temporary pointers,
c     hess points to the Hessenberg matrix,
c     vc, vs point to the cosines and sines of the Givens rotations
c     vrn points to the vectors of residual norms, more precisely
c     the right hand side of the least square problem solved.
c
      integer i,ii,idx,k,m,ptr,p2,hess,vc,vs,vrn
      real(8) alpha, c, s
      logical lp, rp
      save
c
c     check the status of the call
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 30, 40, 50, 60, 70) ipar(10)
c
c     initialization
c
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
c
c     request for matrix vector multiplication A*x in the initialization
c
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
c
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
c
c     request for (1) right preconditioning
c     (2) matrix vector multiplication
c     (3) left preconditioning
c
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
c
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
c
 40   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = k*n + 1
         ipar(10) = 5
         return
      endif
c
c     Modified Gram-Schmidt orthogonalization procedure
c     temporary pointer 'ptr' is pointing to the current column of the
c     Hessenberg matrix. 'p2' points to the new basis vector
c
 50   ipar(7) = ipar(7) + 1
      ptr = k * (k - 1) / 2 + hess
      p2 = ipar(9)
      call mgsro(.false.,n,n,k+1,k+1,fpar(11),w,w(ptr+1),
     $     ipar(12))
      if (ipar(12).lt.0) goto 200
c
c     apply previous Givens rotations and generate a new one to eliminate
c     the subdiagonal element.
c
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
c
c     end of one Arnoldi iteration, alpha will store the estimated
c     residual norm at current stage
c
      fpar(11) = fpar(11) + 6*k + 2
      alpha = abs(alpha)
      fpar(5) = alpha
      if (k.lt.m .and. .not.(ipar(3).ge.0 .and. alpha.le.fpar(4))
     +     .and. (ipar(6).le.0 .or. ipar(7).lt.ipar(6))) goto 110
c
c     update the approximate solution, first solve the upper triangular
c     system, temporary pointer ptr points to the Hessenberg matrix,
c     p2 points to the right-hand-side (also the solution) of the system.
c
 200  ptr = hess + k * (k + 1) / 2
      p2 = vrn + k
      if (w(ptr).eq.zero) then
c
c     if the diagonal elements of the last column is zero, reduce k by 1
c     so that a smaller trianguler system is solved [It should only
c     happen when the matrix is singular, and at most once!]
c
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
c
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
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = 1
         ipar(9) = idx + 1
         ipar(10) = 6
         return
      endif
c
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
c
c     process the complete stopping criteria
c
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
c
c     do we need to restart ?
c
 70   if (ipar(12).ne.0) then
         ipar(1) = -3
         goto 300
      endif
      if ((ipar(7).lt.ipar(6) .or. ipar(6).le.0) .and.
     +     ((ipar(3).eq.999.and.ipar(11).eq.0) .or.
     +     (ipar(3).ne.999.and.fpar(6).gt.fpar(4)))) goto 100
c
c     termination, set error code, compute convergence rate
c
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
 300  if (fpar(3).ne.zero .and. fpar(6).ne.zero .and.
     +     ipar(7).gt.ipar(13)) then
         fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
      else
         fpar(7) = zero
      endif
      return
      end
c-----end-of-gmres

c-----------------------------------------------------------------------
      subroutine givens(x,y,c,s)
      implicit none
      real(8) x,y,c,s
c-----------------------------------------------------------------------
c     Given x and y, this subroutine generates a Givens' rotation c, s.
c     And apply the rotation on (x,y) ==> (sqrt(x**2 + y**2), 0).
c     (See P 202 of "matrix computation" by Golub and van Loan.)
c-----------------------------------------------------------------------
      real(8) t,one,zero
      parameter (zero=0.0D0,one=1.0D0)
c
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
c
c     X or Y must be an invalid floating-point number, set both to zero
c
         x = zero
         y = zero
         c = one
         s = zero
      endif
      x = abs(x*y)
c
c     end of givens
c
      return
      end
c-----end-of-givens

c-----------------------------------------------------------------------
      subroutine bisinit(ipar,fpar,wksize,dsc,lp,rp,wk)
      implicit none
      integer i,ipar(16),wksize,dsc
      logical lp,rp
      real(8)  fpar(16),wk(*)
c-----------------------------------------------------------------------
c     some common initializations for the iterative solvers
c-----------------------------------------------------------------------
      real(8) zero, one
      parameter(zero=0.0D0, one=1.0D0)
c
c     ipar(1) = -2 inidcate that there are not enough space in the work
c     array
c
      if (ipar(4).lt.wksize) then
         ipar(1) = -2
         ipar(4) = wksize
         return
      endif
c
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
c     .. clear the ipar elements used
      ipar(7) = 0
      ipar(8) = 0
      ipar(9) = 0
      ipar(10) = 0
      ipar(11) = 0
      ipar(12) = 0
      ipar(13) = 0
c
c     fpar(1) must be between (0, 1), fpar(2) must be positive,
c     fpar(1) and fpar(2) can NOT both be zero
c     Normally return ipar(1) = -4 to indicate any of above error
c
      if (fpar(1).lt.zero .or. fpar(1).ge.one .or. fpar(2).lt.zero .or.
     &     (fpar(1).eq.zero .and. fpar(2).eq.zero)) then
         if (ipar(1).eq.0) then
            ipar(1) = -4
            return
         else
            fpar(1) = 1.0D-6
            fpar(2) = 1.0D-16
         endif
      endif
c     .. clear the fpar elements
      do i = 3, 10
         fpar(i) = zero
      enddo
      if (fpar(11).lt.zero) fpar(11) = zero
c     .. clear the used portion of the work array to zero
      do i = 1, wksize
         wk(i) = zero
      enddo
c
      return
      end
c-----end-of-bisinit--------------------------------------

c-----------------------------------------------------------------------
      subroutine mgsro(full,lda,n,m,ind,ops,vec,hh,ierr)
      implicit none
      logical full
      integer lda,m,n,ind,ierr
      real(8)  ops,hh(m),vec(lda,m)
c-----------------------------------------------------------------------
c     MGSRO  -- Modified Gram-Schmidt procedure with Selective Re-
c               Orthogonalization
c     The ind'th vector of VEC is orthogonalized against the rest of
c     the vectors.
c
c     The test for performing re-orthogonalization is performed for
c     each indivadual vectors. If the cosine between the two vectors
c     is greater than 0.99 (REORTH = 0.99**2), re-orthogonalization is
c     performed. The norm of the 'new' vector is kept in variable NRM0,
c     and updated after operating with each vector.
c
c     full   -- .ture. if it is necessary to orthogonalize the ind'th
c               against all the vectors vec(:,1:ind-1), vec(:,ind+2:m)
c               .false. only orthogonalize againt vec(:,1:ind-1)
c     lda    -- the leading dimension of VEC
c     n      -- length of the vector in VEC
c     m      -- number of vectors can be stored in VEC
c     ind    -- index to the vector to be changed
c     ops    -- operation counts
c     vec    -- vector of LDA X M storing the vectors
c     hh     -- coefficient of the orthogonalization
c     ierr   -- error code
c               0 : successful return
c               -1: zero input vector
c               -2: input vector contains abnormal numbers
c               -3: input vector is a linear combination of others
c
c     External routines used: real*8 distdot
c-----------------------------------------------------------------------
      integer i,k
      real(8)  nrm0, nrm1, fct, thr, distdot, zero, one, reorth
      parameter (zero=0.0D0, one=1.0D0, reorth=0.98D0)
      external distdot
c
c     compute the norm of the input vector
c
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
c
c     Modified Gram-Schmidt loop
c
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
c
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
c
c     test the resulting vector
c
      nrm1 = sqrt(distdot(n,vec(1,ind),1,vec(1,ind),1))
      ops = ops + n + n
 75   hh(ind) = nrm1
      if (nrm1.le.zero) then
         ierr = -3
         return
      endif
c
c     scale the resulting vector
c
      fct = one / nrm1
      do 80 k = 1, n
         vec(k,ind) = vec(k,ind) * fct
 80   continue
      ops = ops + n + 1
c
c     normal return
c
      ierr = 0
      return
c
      end
c---------end mgsro-----------------------------------------------------

c-----------------------------------------------------------------------
      subroutine ilut(n,a,ja,ia,lfil,droptol,alu,jlu,ju,iwk,w,jw,ierr)
      implicit none
      integer,intent(in)  :: n
      real(8),intent(in)  :: a(*),droptol
      integer,intent(in)  :: ja(*),ia(n+1),lfil,iwk
      integer,intent(out) :: jlu(*),jw(2*n),ju(n),ierr 
      real(8),intent(out) :: alu(*),w(n+1)
c
c     locals
      integer ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,len
      real(8) tnorm, t, s, fact
      intrinsic          dabs
      if (lfil .lt. 0) goto 998
c-----------------------------------------------------------------------
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c     initialize nonzero indicator array.
c
      do 1 j=1,n
         jw(n+j)  = 0
 1    continue
c-----------------------------------------------------------------------
c     beginning of main loop.
c-----------------------------------------------------------------------
      do 500 ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
         tnorm = 0.0d0
         do 501 k=j1,j2
            tnorm = tnorm+dabs(a(k))
 501     continue
         if (tnorm .eq. 0.0) goto 999
         tnorm = tnorm/real(j2-j1+1)
c
c     unpack L-part and U-part of row of A in arrays w
c
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
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
 170     continue
         jj = 0
         len = 0
c
c     eliminate previous rows
c
 150     jj = jj+1
         if (jj .gt. lenl) goto 160
c-----------------------------------------------------------------------
c     in order to do the elimination in the correct order we must select
c     the smallest column index among jw(k), k=jj+1, ..., lenl.
c-----------------------------------------------------------------------
         jrow = jw(jj)
         k = jj
c
c     determine smallest column index
c
         do 151 j=jj+1,lenl
            if (jw(j) .lt. jrow) then
               jrow = jw(j)
               k = j
            endif
 151     continue
c
         if (k .ne. jj) then
c     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
c     exchange in jr
            jw(n+jrow) = jj
            jw(n+j) = k
c     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
         endif
c
c     zero out element in row by setting jw(n+jrow) to zero.
c
         jw(n+jrow) = 0
c
c     get the multiplier for row to be eliminated (jrow).
c
         fact = w(jj)*alu(jrow)
         if (dabs(fact) .le. droptol) goto 150
c
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
            j = jlu(k)
            jpos = jw(n+j)
            if (j .ge. ii) then
c
c     dealing with upper part.
c
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c
                  lenu = lenu+1
                  if (lenu .gt. n) goto 995
                  i = ii+lenu-1
                  jw(i) = j
                  jw(n+j) = i
                  w(i) = - s
               else
c
c     this is not a fill-in element
c
                  w(jpos) = w(jpos) - s

               endif
            else
c
c     dealing  with lower part.
c
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c
                  lenl = lenl+1
                  if (lenl .gt. n) goto 995
                  jw(lenl) = j
                  jw(n+j) = lenl
                  w(lenl) = - s
               else
c
c     this is not a fill-in element
c
                  w(jpos) = w(jpos) - s
               endif
            endif
 203     continue
c
c     store this pivot element -- (from left to right -- no danger of
c     overlap with the working elements in L (pivots).
c
         len = len+1
         w(len) = fact
         jw(len)  = jrow
         goto 150
 160     continue
c
c     reset double-pointer to zero (U-part)
c
         do 308 k=1, lenu
            jw(n+jw(ii+k-1)) = 0
 308     continue
c
c     update L-matrix
c
         lenl = len
         len = min0(lenl,lfil)
c
c     sort by quick-split
c
         call qsplit (w,jw,lenl,len)
c
c     store L-part
c
         do 204 k=1, len
            if (ju0 .gt. iwk) goto 996
            alu(ju0) =  w(k)
            jlu(ju0) =  jw(k)
            ju0 = ju0+1
 204     continue
c
c     save pointer to beginning of row ii of U
c
         ju(ii) = ju0
c
c     update U-matrix -- first apply dropping strategy
c
         len = 0
         do k=1, lenu-1
            if (dabs(w(ii+k)) .gt. droptol*tnorm) then
               len = len+1
               w(ii+len) = w(ii+k)
               jw(ii+len) = jw(ii+k)
            endif
         enddo
         lenu = len+1
         len = min0(lenu,lfil)
c
         call qsplit (w(ii+1), jw(ii+1), lenu-1,len)
c
c     copy
c
         t = dabs(w(ii))
         if (len + ju0 .gt. iwk) goto 997
         do 302 k=ii+1,ii+len-1
            jlu(ju0) = jw(k)
            alu(ju0) = w(k)
            t = t + dabs(w(k) )
            ju0 = ju0+1
 302     continue
c
c     store inverse of diagonal element of u
c
         if (w(ii) .eq. 0.0) w(ii) = (0.0001 + droptol)*tnorm
c
         alu(ii) = 1.0d0/ w(ii)
c
c     update pointer to beginning of next row of U.
c
         jlu(ii+1) = ju0
c-----------------------------------------------------------------------
c     end main loop
c-----------------------------------------------------------------------
 500  continue
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c
 995  ierr = -1
      return
c
c     insufficient storage in L.
c
 996  ierr = -2
      return
c
c     insufficient storage in U.
c
 997  ierr = -3
      return
c
c     illegal lfil entered.
c
 998  ierr = -4
      return
c
c     zero row encountered
c
 999  ierr = -5-ii
      return
      end
c----------------end-of-ilut--------------------------------------------




c-----------------------------------------------------------------------
        subroutine qsplit(a,ind,n,ncut)
        implicit none
        integer :: n, ind(n), ncut
        real(8) :: a(n)
c-----------------------------------------------------------------------
c     does a quick-sort split of a real array.
c     on input a(1:n). is a real array
c     on output a(1:n) is permuted such that its elements satisfy:
c
c     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
c     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
c
c     ind(1:n) is an integer array which permuted in the same way as a(*).
c-----------------------------------------------------------------------
        real(8) :: tmp, abskey
        integer :: itmp, first, last, j, mid
        intrinsic dabs
c-----
        first = 1
        last = n
        if (ncut .lt. first .or. ncut .gt. last) return
c
c     outer loop -- while mid .ne. ncut do
c
 1      mid = first
        abskey = dabs(a(mid))
        do 2 j=first+1, last
           if (dabs(a(j)) .gt. abskey) then
              mid = mid+1
c     interchange
              tmp = a(mid)
              itmp = ind(mid)
              a(mid) = a(j)
              ind(mid) = ind(j)
              a(j)  = tmp
              ind(j) = itmp
           endif
 2      continue
c
c     interchange
c
        tmp = a(mid)
        a(mid) = a(first)
        a(first)  = tmp
c
        itmp = ind(mid)
        ind(mid) = ind(first)
        ind(first) = itmp
c
c     test for while loop
c
        if (mid .eq. ncut) return
        if (mid .gt. ncut) then
           last = mid-1
        else
           first = mid+1
        endif
        goto 1
c----------------end-of-qsplit------------------------------------------
        end

c-----------------------------------------------------------------------
      subroutine amux (n, x, y, a,ja,ia)
      implicit none
      real(8), intent(in)::x(*), a(*)
      real(8), intent(out)::y(*)
      integer, intent(in)::n, ja(*), ia(*)
      real(8) t
      integer i, k

      do 100 i = 1,n
c
c     compute the inner product of row i with vector x
c
         t = 0.0d0
         do 99 k=ia(i), ( ia(i+1)-1 )
            t = t + a(k)*x(ja(k))
 99      continue
c
c     store result in y(i)
c
         y(i) = t
 100  continue
c
      return
      end
c---------end-of-amux---------------------------------------------------

c-----------------------------------------------------------------------
      subroutine lusol(n, y, x, alu, jlu, ju)
        implicit none
        integer :: n, jlu(*), ju(*)
        real(8) :: x(n), y(n), alu(*)
c-----------------------------------------------------------------------
c
c This routine solves the system (LU) x = y,
c given an LU decomposition of a matrix stored in (alu, jlu, ju)
c modified sparse row format
c
c-----------------------------------------------------------------------
c on entry:
c n   = dimension of system
c y   = the right-hand-side vector
c alu, jlu, ju
c     = the LU matrix as provided from the ILU routines.
c
c on return
c x   = solution of LU x = y.
c-----------------------------------------------------------------------
c
c Note: routine is in place: call lusol (n, x, x, alu, jlu, ju)
c       will solve the system with rhs x and overwrite the result on x .
c
c-----------------------------------------------------------------------
c local variables
c
        integer i,k
c
c forward solve
c
        do 40 i = 1, n
           x(i) = y(i)
           do 41 k=jlu(i),ju(i)-1
              x(i) = x(i) - alu(k)* x(jlu(k))
 41        continue
 40     continue
c
c     backward solve.
c
      do i = n, 1, -1
         do k=ju(i),jlu(i+1)-1
              x(i) = x(i) - alu(k)*x(jlu(k))
         end do
          x(i) = alu(i)*x(i)
      end do
c
      return
      end
c----------------end of lusol ------------------------------------------

c-----------------------------------------------------------------------
      function distdot(n,x,ix,y,iy)
      implicit none
      integer n, ix, iy
      real(8) distdot, x(*), y(*), ddot
      external ddot
      distdot = ddot(n,x,ix,y,iy)
      return
      end
c-----end-of-distdot-----------------------------------------------------


c-----------------------------------------------------------------------
      real(8) function ddot(n,dx,incx,dy,incy)
      implicit none
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      real(8) :: dx(1),dy(1),dtemp
      integer :: i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
c---------end-of-ddot---------------------------------------------------

c------------------------------------------------------------------
      real(8) function dnrm2 ( n, dx, incx)
      implicit none
      integer          incx, next, n, nn, i, j
      real(8)   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one
      data   zero, one /0.0d0, 1.0d0/

      data cutlo, cuthi / 8.232d-11,  1.304d19 /
c
      if(n .gt. 0) go to 10
         dnrm2  = zero
         go to 300
c
   10 assign 30 to next
      sum = zero
      nn = n * incx
c                                                 begin main loop
      i = 1
   20    go to next,(30, 50, 70, 110)
   30 if( dabs(dx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
c
c                        phase 1.  sum is zero
c
   50 if( dx(i) .eq. zero) go to 200
      if( dabs(dx(i)) .gt. cutlo) go to 85
c
c                                prepare for phase 2.
      assign 70 to next
      go to 105
c
c                                prepare for phase 4.
c
  100 i = j
      assign 110 to next
      sum = (sum / dx(i)) / dx(i)
  105 xmax = dabs(dx(i))
      go to 115
c
c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.
c
   70 if( dabs(dx(i)) .gt. cutlo ) go to 75
c
c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.
c
  110 if( dabs(dx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / dx(i))**2
         xmax = dabs(dx(i))
         go to 200
c
  115 sum = sum + (dx(i)/xmax)**2
      go to 200
c
c
c                  prepare for phase 3.
c
   75 sum = (sum * xmax) * xmax
c
c
c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)
c
   85 hitest = cuthi/float( n )
c
c                   phase 3.  sum is mid-range.  no scaling.
c
      do j =i,nn,incx
          if(dabs(dx(j)) .ge. hitest) go to 100
          sum = sum + dx(j)**2
      end do
      dnrm2 = dsqrt( sum )
      go to 300
c
  200 continue
      i = i + incx
      if ( i .le. nn ) go to 20
c
c              end of main loop.
c
c              compute square root and adjust for scaling.
c
      dnrm2 = xmax * dsqrt(sum)
  300 continue
      return
      end
c--------end-of-dnrm2-------------------------------------------


