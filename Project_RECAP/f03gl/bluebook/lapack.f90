module lapack_s
   
!   interface
!     subroutine lapack_sgetrf( m, n, a, lda, ipiv, info )
!       integer                :: info, lda, m, n
!       integer, dimension(*)  :: ipiv
!       real, dimension(lda,*) :: a
!     end subroutine lapack_sgetrf
!   end interface

public  :: lapack_sgetrf
private :: sgetrf, xerbla, sgemm, slaswp, sscal, sger, strsm 
private :: lsame, isamax, ilaenv

contains 

subroutine lapack_sgetrf( m, n, a, lda, info )

      integer, intent(in)    :: lda, m, n
      integer, intent(out)   :: info 

      integer, dimension(:), allocatable :: ipiv
      real, dimension(lda,*), intent(inout) :: a

      min_mn = min(M,N)
      allocate(ipiv(min_mn))
       
      call sgetrf( m, n, a, lda, ipiv, info )

      deallocate(ipiv)
      
end subroutine lapack_sgetrf

subroutine sgetrf( m, n, a, lda, ipiv, info )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
      integer, intent(in)    :: lda, m, n
      integer, intent(out)   :: info 
      integer, dimension(*),intent(out)     :: ipiv
      real, dimension(lda,*), intent(inout) :: a
!
!  Purpose
!  =======
!
!  SGETRF computes an LU factorization of a general M-by-N matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 3 BLAS version of the algorithm.
!
!  Arguments
!  =========
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) REAL array, dimension (LDA,N)
!          On entry, the M-by-N matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!                has been completed, but the factor U is exactly
!                singular, and division by zero will occur if it is used
!                to solve a system of equations.
!
!  =====================================================================
!
      real, parameter :: one = 1.0
      integer         :: i, iinfo, j, jb, nb
!     ..
!     .. External Subroutines ..
     !EXTERNAL           SGEMM, SGETF2, SLASWP, STRSM, XERBLA
!     ..
!     .. External Functions ..
     !integer, external  :: ilaenv
      integer, intrinsic :: max, min
      !
      !     Test the input parameters.
      !
      info = 0
      if( m.lt.0 ) then
         info = -1
      else if( n.lt.0 ) then
         info = -2
      else if( lda.lt.max( 1, m ) ) then
         info = -4
      end if
      if( info.ne.0 ) then
         call xerbla( 'SGETRF', -info )
         return
      end if
      !
      ! Quick return if possible
      !
      if( m.eq.0 .or. n.eq.0 ) return
      !
      ! Determine the block size for this environment.
      !
      nb = ilaenv( 1, 'sgetrf', ' ', m, n, -1, -1 )
      if( nb.le.1 .or. nb.ge.min( m, n ) ) then

         call sgetf2( m, n, a, lda, ipiv, info )    ! Use unblocked code.

      else
         ! Use blocked code.
         do j = 1, min( m, n ), nb
            jb = min( min( m, n )-j+1, nb )
            !
            ! Factor diagonal and subdiagonal blocks and test for exact
            ! singularity.
            !
            call sgetf2( m-j+1, jb, a( j, j ), lda, ipiv( j ), iinfo )
            !
            ! Adjust INFO and the pivot indices.
            !
            if( info.eq.0 .and. iinfo.gt.0 ) info = iinfo + j - 1

            do i=j,min( m, j+jb-1 )
              ipiv(i) = j - 1 + ipiv(i)
            end do
            !
            ! Apply interchanges to columns 1:J-1.
            !
            call slaswp( j-1, a, lda, j, j+jb-1, ipiv, 1 )

            if( j+jb.le.n )then
              !
              ! Apply interchanges to columns J+JB:N.
              !
              call slaswp( n-j-jb+1, a(1,j+jb), lda, j, j+jb-1,ipiv, 1 )
              !
              ! Compute block row of U.
              !
              call strsm( 'Left', 'Lower', 'No transpose', 'Unit', JB,  &
                           n-j-jb+1, one, a( j, j ), lda, a( j, j+jb ), lda )
              if( j+jb.le.m ) then
              !
              ! Update trailing submatrix.
              !
              call sgemm( 'No transpose', 'No transpose', M-J-JB+1,  &
                           N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,    &
                          A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ), LDA )
              endif
            endif
          end do
      endif

end subroutine sgetrf
!==================================================================
subroutine sgetf2( m, n, a, lda, ipiv, info )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
      integer :: info, lda, m, n

      integer, dimension(*)  :: ipiv
      real, dimension(lda,*) :: a

      real, parameter :: one  = 1.0
      real, parameter :: zero = 0.0

      real            :: sfmin
      integer         :: i, j, jp

      ! .. external functions ..
     !real   :: slamch
     !integer :: isamax
     !external        :: slamch, isamax

      ! .. external subroutines ..
     !external           sger, sscal, sswap, xerbla

      !  .. intrinsic functions ..
      integer, intrinsic :: max, min
      !
      ! Test the input parameters.
      !
      info = 0
      if( m.lt.0 )then
         info = -1
      elseif( n.lt.0 )then
         info = -2
      elseif( lda.lt.max( 1, m ) )then
         info = -4
      endif
      if( info.ne.0 )then
        call xerbla( 'sgetf2', -info )
        return
      endif
      !
      ! quick return if possible
      !
      if( m.eq.0 .or. n.eq.0 ) return
      !
      ! Compute machine safe minimum 
      ! 
     !sfmin = slamch('S')
      sfmin = 1.1754944E-38
      
      do j=1,min( m, n )
        !
        ! find pivot and test for singularity.
        !
        jp = j - 1 + isamax( m-j+1, a( j, j ), 1 )
        ipiv( j ) = jp
        if( a( jp, j ).ne.zero )then
          !
          ! apply the interchange to columns 1:n.
          !
          if( jp.ne.j ) call sswap( n, a( j, 1 ), lda, a( jp, 1 ), lda )
          !
          ! compute elements j+1:m of j-th column.
          !
          if( j.lt.m )then 
            if( abs(a( j, j )) .ge. sfmin )then 
              call sscal( m-j, one / a( j, j ), a( j+1, j ), 1 ) 
            else 
              do i = 1, m-j 
                a( j+i, j ) = a( j+i, j ) / a( j, j ) 
              end do 
            endif 
          endif 

        elseif( info.eq.0 )then
            
          info = j
	    
        endif

        if( j.lt.min( m, n ) )then
          !
          ! update trailing submatrix.
          !
          call sger( m-j, n-j, -one, a( j+1, j ), 1, a( j, j+1 ), lda, &
                     a( j+1, j+1 ), lda )
        endif
      end do
 
end subroutine sgetf2
subroutine xerbla( srname, info )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
      character(len=6) :: srname
      integer          :: info

      write(*,'('' ** On entry to '',a6,'' parameter number '',i2,'' had an illegal value'')' ) srname, info
      stop

end subroutine xerbla
function lsame(ca,cb)
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
      logical          :: lsame
      character(len=1) :: ca,cb

      intrinsic :: ichar
      integer   :: inta,intb,zcode
      !
      ! Test if the characters are equal
      !
      lsame = ca .eq. cb
      if( lsame ) return

      ZCODE = ICHAR('Z')
      inta = ichar(ca)
      intb = ichar(cb)

      if( zcode.eq.90 .or. zcode.eq.122 )then
        !
        ! ASCII is assumed - ZCODE is the ASCII code of either lower or
        ! upper case 'Z'.
        !
        if( inta.ge.97 .and. inta.le.122 ) inta = inta - 32
        if( intb.ge.97 .and. intb.le.122 ) intb = intb - 32

      elseif( zcode.eq.233 .or. zcode.eq.169 )then
        !
        ! EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
        ! upper case 'Z'.
        !
        if( inta.ge.129 .and. inta.le.137 .or. &
            inta.ge.145 .and. inta.le.153 .or. &
            inta.ge.162 .and. inta.le.169 ) inta = inta + 64
        if( intb.ge.129 .and. intb.le.137 .or. &
            intb.ge.145 .and. intb.le.153 .or. &
            intb.ge.162 .and. intb.le.169 ) intb = intb + 64

      elseif( zcode.eq.218 .or. zcode.eq.250 )then
        !
        ! ASCII is assumed, on Prime machines - ZCODE is the ASCII code
        ! plus 128 of either lower or upper case 'Z'.
        !
        if (inta.ge.225 .and. inta.le.250) inta = inta - 32
        if (intb.ge.225 .and. intb.le.250) intb = intb - 32
      endif
      lsame = inta .eq. intb

end function lsame
!==================================================================
subroutine sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
      real             :: alpha,beta
      integer          :: k,lda,ldb,ldc,m,n
      character(len=1) :: transa,transb

      real, dimension(lda,*) :: A
      real, dimension(ldb,*) :: B
      real, dimension(ldc,*) :: C
      
      ! .. external functions ..
     !logical :: lsame
     !external lsame
      ! .. external subroutines ..
     !external xerbla
      ! .. intrinsic functions ..
      intrinsic max

      real    :: temp
      integer :: i,info,j,l,ncola,nrowa,nrowb
      logical :: nota,notb

      real, parameter :: one  = 1.0
      real, parameter :: zero = 0.0
      !
      ! Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
      ! transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
      ! and  columns of  A  and the  number of  rows  of  B  respectively.
      !
      nota = lsame(transa,'n')
      notb = lsame(transb,'n')
      if( nota )then
        nrowa = m
        ncola = k
      else
        nrowa = k
        ncola = m
      endif
      if( notb )then
        nrowb = k
      else
        nrowb = n
      endif
      !
      ! Test the input parameters.
      !
      info = 0
      if( (.not.nota) .and. (.not.lsame(transa,'c')) .and. &
          (.not.lsame(transa,'t')) )then
        info = 1
      elseif( (.not.notb) .and. (.not.lsame(transb,'c')) .and. &
              (.not.lsame(transb,'t')) )then
        info = 2
      elseif( m.lt.0 )then
        info = 3
      elseif( n.lt.0 )then
        info = 4
      elseif( k.lt.0 )then
        info = 5
      elseif( lda.lt.max(1,nrowa) )then
        info = 8
      elseif( ldb.lt.max(1,nrowb) )then
        info = 10
      elseif( ldc.lt.max(1,m) )then
        info = 13
      endif
      if( info.ne.0 )then
        call xerbla('sgemm ',info)
        return
      endif
      !
      ! Quick return if possible.
      !
      if( (m.eq.0) .or. (n.eq.0) .or. &
        (((alpha.eq.zero).or. (k.eq.0)).and. (beta.eq.one))) return
      !
      ! And if  alpha.eq.zero.
      !
      if( alpha.eq.zero )then
        if( beta.eq.zero )then
          do j = 1,n
            do i = 1,m
              c(i,j) = zero
            end do
          end do
        else
          do j = 1,n
            do i = 1,m
              c(i,j) = beta*c(i,j)
            end do
          end do
        endif
        return
      endif
      !
      ! Start the operations.
      !
      if( notb )then
          if( nota )then
            !
            ! form  c := alpha*a*b + beta*c.
            !
            do j = 1,n
                  if( beta.eq.zero )then
                      do i = 1,m
                          c(i,j) = zero
                      end do
                  elseif( beta.ne.one )then
                      do i = 1,m
                          c(i,j) = beta*c(i,j)
                      end do
                  endif
                  do l = 1,k
                      if( b(l,j).ne.zero )then
                          temp = alpha*b(l,j)
                          do i = 1,m
                              c(i,j) = c(i,j) + temp*a(i,l)
                          end do
                      end if
                  end do
              end do
          else
            !
            ! Form  C := alpha*A'*B + beta*C
            !
            do j = 1,n
              do i = 1,m
                temp = zero
                do l = 1,k
                  temp = temp + a(l,i)*b(l,j)
                end do
                if( beta.eq.zero )then
                  c(i,j) = alpha*temp
                else
                  c(i,j) = alpha*temp + beta*c(i,j)
                endif
              end do
            end do
          endif
      else
          if( nota )then
            !
            ! Form  C := alpha*A*B' + beta*C
            !
            do j = 1,n
              if( beta.eq.zero )then
                do i = 1,m
              	  c(i,j) = zero
                end do
              elseif( beta.ne.one )then
                do i = 1,m
                  c(i,j) = beta*c(i,j)
                end do
              endif
              do l = 1,k
                if( b(j,l).ne.zero )then
                  temp = alpha*b(j,l)
                  do i = 1,m
                    c(i,j) = c(i,j) + temp*a(i,l)
                  end do
                endif
              end do
            end do
          else
            !
            ! Form  C := alpha*A'*B' + beta*C
            !
            do j = 1,n
              do i = 1,m
                temp = zero
                do l = 1,k
                  temp = temp + a(l,i)*b(j,l)
                end do
                if( beta.eq.zero )then
                  c(i,j) = alpha*temp
                else
                  c(i,j) = alpha*temp + beta*c(i,j)
                endif
              end do
            end do
          endif
      endif

end subroutine sgemm
!==================================================================
subroutine slaswp( n, a, lda, k1, k2, ipiv, incx )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
      integer                :: incx, k1, k2, lda, n

      integer, dimension(*)  :: ipiv
      real, dimension(lda,*) :: a

      integer :: i, i1, i2, inc, ip, ix, ix0, j, k, n32
      real    :: temp
      !
      !  Interchange row I with row IPIV(I) for each of rows K1 through K2.
      !
      if( incx.gt.0 )then
         ix0 = k1
         i1 = k1
         i2 = k2
         inc = 1
      elseif( incx.lt.0 )then
         ix0 = 1 + ( 1-k2 )*incx
         i1 = k2
         i2 = k1
         inc = -1
      else
         return
      endif

      n32 = ( n / 32 )*32
      if( n32.ne.0 )then
        do j = 1, n32, 32
          ix = ix0
          do i = i1, i2, inc
            ip = ipiv( ix )
            if( ip.ne.i )then
              do k = j, j + 31
                temp = a( i, k )
                a( i, k ) = a( ip, k )
                a( ip, k ) = temp
              end do
            endif
            ix = ix + incx
          end do
        end do
      endif
      if( n32.ne.n )then
        n32 = n32 + 1
        ix = ix0
        do i = i1, i2, inc
          ip = ipiv( ix )
          if( ip.ne.i )then
            do k = n32, n
              temp = a( i, k )
              a( i, k ) = a( ip, k )
              a( ip, k ) = temp
            end do
          endif
          ix = ix + incx
        end do
      endif

end subroutine slaswp
!==================================================================
subroutine sswap(n,sx,incx,sy,incy)
!
!     interchanges two vectors.
!     uses unrolled loops for increments equal to 1.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
      integer incx,incy,n
      real, dimension(*) :: sx ,sy

      real    :: stemp
      integer :: i,ix,iy,m,mp1

      intrinsic :: mod
   
      if( n.le.0 ) return
      if( incx.eq.1 .and. incy.eq.1 ) goto 20
      !
      ! code for unequal increments or equal increments not equal to 1
      !
      ix = 1
      iy = 1
      if( incx.lt.0 ) ix = (-n+1)*incx + 1
      if( incy.lt.0 ) iy = (-n+1)*incy + 1
      do i = 1,n
        stemp = sx(ix)
        sx(ix) = sy(iy)
        sy(iy) = stemp
        ix = ix + incx
        iy = iy + incy
      end do
      return
      !
      ! code for both increments equal to 1
      !
      ! clean-up loop
      !
   20 continue
   
      m = mod(n,3)
      if( m.eq.0 ) goto 40
      do i = 1,m
        stemp = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp
      end do
      if( n.lt.3 ) return
   
   40 continue
      mp1 = m + 1
      do i = mp1,n,3
        stemp = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp
        stemp = sx(i+1)
        sx(i+1) = sy(i+1)
        sy(i+1) = stemp
        stemp = sx(i+2)
        sx(i+2) = sy(i+2)
        sy(i+2) = stemp
      end do
      
end subroutine sswap
!==================================================================
subroutine sscal(n,sa,sx,incx)
!
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to 1.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
      real    :: sa
      integer :: incx,n
      real, dimension(*) :: sx

      integer :: i,m,mp1,nincx
      
      intrinsic :: mod

      if( n.le.0 .or. incx.le.0 ) return
      if( incx.eq.1 ) goto 20
      !
      ! code for increment not equal to 1
      !
      nincx = n*incx
      do i=1,nincx,incx
        sx(i) = sa*sx(i)
      end do
      return
      !
      ! code for increment equal to 1
      !
      ! clean-up loop
      !
   20 continue
      m = mod(n,5)
      if( m.eq.0 ) goto 40
      do i = 1,m
        sx(i) = sa*sx(i)
      end do
      if( n.lt.5 ) return
   40 continue 
      mp1 = m + 1
      do i = mp1,n,5
        sx(i) = sa*sx(i)
        sx(i+1) = sa*sx(i+1)
        sx(i+2) = sa*sx(i+2)
        sx(i+3) = sa*sx(i+3)
        sx(i+4) = sa*sx(i+4)
      end do
      
end subroutine sscal
!==================================================================
function isamax(n,sx,incx)
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
      integer :: isamax
      integer :: incx,n
      real, dimension(*) :: sx

      real    :: smax
      integer :: i,ix

      intrinsic :: abs

      isamax = 0
      if( n.lt.1 .or. incx.le.0) return
      isamax = 1
      if( n.eq.1) return
      if( incx.eq.1) goto 20
      !
      ! code for increment not equal to 1
      !
      ix = 1
      smax = abs(sx(1))
      ix = ix + incx
      do i = 2,n
        if( abs(sx(ix)).le.smax) goto 5
        isamax = i
        smax = abs(sx(ix))
    5   continue
        ix = ix + incx
      end do
      return
      !
      ! code for increment equal to 1
      !
   20 continue
      smax = abs(sx(1))
      do i = 2,n
        if( abs(sx(i)).le.smax) go to 30
        isamax = i
        smax = abs(sx(i))	
   30   continue
      end do
      
end function isamax
!==================================================================
subroutine sger(m,n,alpha,x,incx,y,incy,a,lda)
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
      real    :: alpha
      integer :: incx,incy,lda,m,n

      real, dimension(lda,*) :: a
      real, dimension(*)     :: x, y

      real, parameter :: zero = 0.0

      real    :: temp
      integer :: i,info,ix,j,jy,kx

      ! .. external subroutines ..
     !external xerbla

      ! .. intrinsic functions ..
      intrinsic max
      !
      ! test the input parameters.
      !
      info = 0
      if( m.lt.0 )then
        info = 1
      elseif( n.lt.0 )then
        info = 2
      elseif( incx.eq.0 )then
        info = 5
      elseif( incy.eq.0 )then
        info = 7
      elseif( lda.lt.max(1,m) )then
        info = 9
      endif
      if( info.ne.0 )then
        call xerbla('sger  ',info)
        return
      endif
      !
      ! Quick return if possible.
      !
      if((m.eq.0) .or. (n.eq.0) .or. (alpha.eq.zero) ) return
      !
      ! Start the operations. In this version the elements of A are
      ! accessed sequentially with one pass through A.
      !
      if( incy.gt.0 )then
        jy = 1
      else
        jy = 1 - (n-1)*incy
      endif
      if( incx.eq.1 )then
        do j = 1,n
          if( y(jy).ne.zero )then
            temp = alpha*y(jy)
            do i = 1,m
              a(i,j) = a(i,j) + x(i)*temp
            end do
          endif
          jy = jy + incy
        end do
      else
        if( incx.gt.0 )then
          kx = 1
        else
          kx = 1 - (m-1)*incx
        endif
        do j = 1,n
          if( y(jy).ne.zero )then
            temp = alpha*y(jy)
            ix = kx
            do i = 1,m
              a(i,j) = a(i,j) + x(ix)*temp
              ix = ix + incx
            end do
          endif
          jy = jy + incy
        end do
      endif

end subroutine sger

subroutine strsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
!
!  Level 3 Blas routine.
!
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!

      real      :: alpha
      integer   :: lda,ldb,m,n
      character :: diag,side,transa,uplo

      real, dimension(lda,*) :: a
      real, dimension(ldb,*) :: b

     !logical  :: lsame
     !external :: lsame

     !external :: xerbla
      integer, intrinsic :: max

      real     :: temp
      integer  :: i,info,j,k,nrowa
      logical  :: lside,nounit,upper

      real, parameter :: one  = 1.0
      real, parameter :: zero = 0.0
      !
      ! Test the input parameters.
      !
      lside = lsame(side,'L')
      if( LSIDE )then
        NROWA = M
      ELSE
        NROWA = N
      endif
      nounit = lsame(diag,'N')
      upper = lsame(uplo,'U')

      info = 0
      if( (.not.lside) .and. (.not.lsame(side,'R')) )then
    	info = 1
      elseif( (.not.upper) .and. (.not.lsame(uplo,'L')) )then
     	info = 2
      elseif( (.not.lsame(transa,'N')) .and. &
     	      (.not.lsame(transa,'T')) .and. &
     	      (.not.lsame(transa,'C')) )then
        info = 3
      elseif( (.not.lsame(diag,'U')) .and. (.not.lsame(diag,'N')) )then
        info = 4
      elseif( m.lt.0 )then
     	info = 5
      elseif( n.lt.0 )then
     	info = 6
      elseif( lda.lt.max(1,nrowa) )then
     	info = 9
      elseif( ldb.lt.max(1,m) )then
     	info = 11
      endif
      if( info.ne.0 )then
        call xerbla('strsm ',info)
     	return
      endif
      !
      ! Quick return if possible.
      !
      if( n.eq.0) return
      !
      ! And when  alpha.eq.zero.
      !
      if( alpha.eq.zero )then
        do j = 1,n
          do  i = 1,m
            b(i,j) = zero
          end do
        end do
        return
      endif
      !
      !     Start the operations.
      !
      if( lside )then
        if( lsame(transa,'N') )then
          !
          ! Form  B := alpha*inv( A )*B.
          !
          if( upper )then
            do j = 1,n
              if( alpha.ne.one )then
                do i = 1,m
                  b(i,j) = alpha*b(i,j)
                end do
              endif
              do k = m,1,-1
                if( b(k,j).ne.zero )then
                  if( nounit) b(k,j) = b(k,j)/a(k,k)
                  do i = 1,k - 1
                    b(i,j) = b(i,j) - b(k,j)*a(i,k)
                  end do
                endif
              end do
            end do
          else
            do j = 1,n
              if( alpha.ne.one )then
                do i = 1,m
                  b(i,j) = alpha*b(i,j)
                end do
              endif
              do k = 1,m
                if( b(k,j).ne.zero )then
                  if( nounit) b(k,j) = b(k,j)/a(k,k)
                    do i = k + 1,m
                      b(i,j) = b(i,j) - b(k,j)*a(i,k)
                    end do
                  endif
              end do
            end do
          endif
        else
          !
          ! Form  B := alpha*inv( A' )*B.
          !
          if( upper )then
            do j = 1,n
              do i = 1,m
                temp = alpha*b(i,j)
                do k = 1,i - 1
                  temp = temp - a(k,i)*b(k,j)
                end do
                if( nounit) temp = temp/a(i,i)
                b(i,j) = temp
              end do
            end do
          else
            do j = 1,n
              do i = m,1,-1
                temp = alpha*b(i,j)
                do k = i + 1,m
                  temp = temp - a(k,i)*b(k,j)
                end do
                if( nounit) temp = temp/a(i,i)
                b(i,j) = temp
              end do
            end do
          endif
        endif
      else
        if( lsame(transa,'N') )then
          !
          ! Form  B := alpha*B*inv( A ).
          !
          if( upper )then
            do j = 1,n
              if( alpha.ne.one )then
                do i = 1,m
                  b(i,j) = alpha*b(i,j)
                end do
              endif
              do k = 1,j - 1
                if( a(k,j).ne.zero )then
                  do i = 1,m
                    b(i,j) = b(i,j) - a(k,j)*b(i,k)
                  end do
                endif
              end do
              if( nounit )then
                temp = one/a(j,j)
                do i = 1,m
                  b(i,j) = temp*b(i,j)
                end do
              endif
            end do
          else
            do j = n,1,-1
              if( alpha.ne.one )then
                do i = 1,m
                  b(i,j) = alpha*b(i,j)
                end do
              endif
              do k = j + 1,n
                if( a(k,j).ne.zero )then
                  do i = 1,m
                    b(i,j) = b(i,j) - a(k,j)*b(i,k)
                  end do
                endif
              end do
              if( nounit )then
                temp = one/a(j,j)
                do i = 1,m
                  b(i,j) = temp*b(i,j)
                end do
              endif
            end do
          endif
        else
          !
          ! Form  B := alpha*B*inv( A' ).
          !
          if( upper )then
            do k = n,1,-1
              if( nounit )then
                temp = one/a(k,k)
                do i = 1,m
                  b(i,k) = temp*b(i,k)
                end do
              endif
              do j = 1,k - 1
                if( a(j,k).ne.zero )then
                  temp = a(j,k)
                  do i = 1,m
                    b(i,j) = b(i,j) - temp*b(i,k)
                  end do
                endif
              end do
              if( alpha.ne.one )then
                do i = 1,m
                  b(i,k) = alpha*b(i,k)
                end do
              endif
            end do
          else
            do k = 1,n
              if( nounit )then
                temp = one/a(k,k)
                do i = 1,m
                  b(i,k) = temp*b(i,k)
                end do
              endif
              do j = k + 1,n
                if( a(j,k).ne.zero )then
                  temp = a(j,k)
                  do i = 1,m
                    b(i,j) = b(i,j) - temp*b(i,k)
                  end do
                endif
              end do
              if( alpha.ne.one )then
                do i = 1,m
                  b(i,k) = alpha*b(i,k)
                end do
              endif
            end do
          endif
        endif
      endif

end subroutine strsm

integer function ilaenv( ispec, name, opts, n1, n2, n3, n4 )
!
!  -- LAPACK auxiliary routine (version 3.1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     January 2007
!
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4

      INTEGER            I, IC, IZ, NB, NBMIN, NX
      LOGICAL            CNAME, SNAME
      CHARACTER          C1*1, C2*2, C4*2, C3*3, SUBNAM*6

      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL

     !INTEGER            IEEECK, IPARMQ
     !EXTERNAL           IEEECK, IPARMQ

      goto ( 10, 10, 10, 80, 90, 100, 110, 120,       &
              130, 140, 150, 160, 160, 160, 160, 160 )ispec
      !
      ! Invalid value for ISPEC
      !
      ilaenv = -1
      return

   10 continue
      !
      ! Convert NAME to upper case if the first character is lower case.
      !
      ilaenv = 1
      subnam = name
      ic = ichar( subnam( 1: 1 ) )
      iz = ichar( 'Z' )
      if( iz.eq.90 .or. iz.eq.122 )then
        !
        ! ASCII character set
        !
        if( ic.ge.97 .and. ic.le.122 ) then
          subnam( 1: 1 ) = char( ic-32 )
          do i = 2, 6
            ic = ichar( subnam( i: i ) )
            if( ic.ge.97 .and. ic.le.122 ) subnam( i: i ) = char( ic-32 )
          end do
         endif
      elseif( iz.eq.233 .or. iz.eq.169 ) then
         !
         ! EBCDIC character set
         !
         if( ( ic.ge.129 .and. ic.le.137 ) .or. &
             ( ic.ge.145 .and. ic.le.153 ) .or. &
             ( ic.ge.162 .and. ic.le.169 ) ) then
            subnam( 1: 1 ) = char( ic+64 )
            do i = 2, 6
               ic = ichar( subnam( i: i ) )
               if( ( ic.ge.129 .and. ic.le.137 ) .or. &
                   ( ic.ge.145 .and. ic.le.153 ) .or. &
                   ( ic.ge.162 .and. ic.le.169 ) )subnam(i:i) = char(ic+64)
           end do
         endif
      elseif( iz.eq.218 .or. iz.eq.250 ) then
         !
         ! Prime machines:  ASCII+128
         !
         if( ic.ge.225 .and. ic.le.250 ) then
            subnam( 1: 1 ) = char( ic-32 )
            do i = 2, 6
               ic = ichar( subnam( i: i ) )
               if( ic.ge.225 .and. ic.le.250 ) subnam(i:i) = char( ic-32 )
           end do
         endif
      endif

      c1 = subnam( 1: 1 )
      sname = c1.eq.'S' .or. c1.eq.'D'
      cname = c1.eq.'C' .or. c1.eq.'Z'
      if( .not.( cname .or. sname ) )  RETURN
      c2 = subnam(2:3)
      c3 = subnam(4:6)
      c4 = c3(2:3)

      GOTO ( 50, 60, 70 ) ISPEC

   50 CONTINUE
      !
      ! ISPEC = 1:  block size
      !
      ! In these examples, separate code is provided for setting NB for
      ! real and complex.  We assume that NB will take the same value in
      ! single or double precision.
      !
      NB = 1

      if( c2.eq.'GE' )then
     	if( c3.eq.'TRF' )then
     	  if( sname )then
     	    nb = 64
     	  else
            nb = 64
          endif
        elseif( C3.eq.'QRF' .or. C3.eq.'RQF' .or. C3.eq.'LQF' .or. C3.eq.'QLF' )then
          if( sname )then
            nb = 32
          else
            nb = 32
          endif
        elseif( c3.eq.'HRD' )then
          if( sname )then
            nb = 32
          else
            nb = 32
          endif
        elseif( c3.eq.'BRD' )then
          if( sname )then
            nb = 32
          else
            nb = 32
          endif
        elseif( c3.eq.'TRI' )then
          if( sname )then
            nb = 64
          else
            nb = 64
          endif
        endif
      elseif( c2.eq.'PO' )then
        if( c3.eq.'TRF' )then
          if( sname )then
            nb = 64
          else
            nb = 64
          endif
        endif
      elseif( c2.eq.'SY' )then
        if( C3.eq.'TRF' )then
          if( sname )then
            nb = 64
          else
            nb = 64
          endif
        elseif( sname .and. c3.eq.'TRD' )then
          nb = 32
        elseif( sname .and. c3.eq.'GST' )then
          nb = 64
        endif
      elseif( cname .and. c2.eq.'HE' )then
        if( C3.eq.'TRF' )then
          nb = 64
        elseif( c3.eq.'TRD' )then
          nb = 32
        elseif( c3.eq.'GST' )then
          nb = 64
         endif
      elseif( sname .and. c2.eq.'OR' )then
        if( c3( 1: 1 ).eq.'G' )then
          if( c4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
              'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
            nb = 32
          endif
        elseif( C3( 1: 1 ).eq.'M' )then
          if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
               'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
            nb = 32
          endif
        endif
      elseif( cname .and. c2.eq.'UN' )then
        if( C3( 1: 1 ).eq.'G' )then
          if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
              'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
            nb = 32
          endif
        elseif( C3( 1: 1 ).eq.'M' )then
          if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
              'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
            nb = 32
          endif
       endif
     elseif( C2.eq.'GB' )then
       if( C3.eq.'TRF' )then
         if( sname )then
           if( n4.le.64 )then
             nb = 1
           else
             nb = 32
           endif
         else
           if( n4.le.64 )then
             nb = 1
           else
             nb = 32
           endif
         endif
       endif
     elseif( c2.eq.'PB' )then
       if( C3.eq.'TRF' )then
         if( sname )then
           if( n2.le.64 )then
             nb = 1
           else
             nb = 32
           endif
         else
           if( n2.le.64 )then
             nb = 1
           else
             nb = 32
           endif
         endif
       endif
     elseif( c2.eq.'TR' )then
       if( C3.eq.'TRI' )then
         if( sname )then
           nb = 64
         else
           nb = 64
         endif
       endif
     elseif( c2.eq.'LA' )then
       if( C3.eq.'UUM' )then
         if( sname )then
           nb = 64
         else
           nb = 64
         endif
       endif
     elseif( sname .and. c2.eq.'ST' )then
       if( C3.eq.'EBZ' )then
         nb = 1
       endif
     endif
     ilaenv = nb
     return

 60  continue
     !
     ! ISPEC = 2:  minimum block size
     !
     NBMIN = 2
     if( C2.eq.'GE' )then
        if( C3.eq.'QRF' .or. C3.eq. 'RQF' .or. &
            C3.eq.'LQF' .or. C3.eq. 'QLF' )then
           if( SNAME )then
              NBMIN = 2
           ELSE
              NBMIN = 2
           endif
        elseif( C3.eq.'HRD' )then
           if( SNAME )then
              NBMIN = 2
           ELSE
              NBMIN = 2
           endif
        elseif( C3.eq.'BRD' )then
           if( SNAME )then
              NBMIN = 2
           ELSE
              NBMIN = 2
           endif
        elseif( C3.eq.'TRI' )then
           if( SNAME )then
              NBMIN = 2
           ELSE
              NBMIN = 2
           endif
        endif
     elseif( C2.eq.'SY' )then
        if( C3.eq.'TRF' )then
           if( SNAME )then
              NBMIN = 8
           ELSE
              NBMIN = 8
           endif
        elseif( SNAME .AND. C3.eq.'TRD' )then
           NBMIN = 2
        endif
     elseif( CNAME .AND. C2.eq.'HE' )then
        if( C3.eq.'TRD' )then
           NBMIN = 2
        endif
     elseif( SNAME .AND. C2.eq.'OR' )then
        if( C3( 1: 1 ).eq.'G' )then
           if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
               'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
              nbmin = 2
           endif
        elseif( C3( 1: 1 ).eq.'M' )then
           if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq.&
               'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
              nbmin = 2
           endif
        endif
     elseif( CNAME .AND. C2.eq.'UN' )then
        if( C3( 1: 1 ).eq.'G' )then
           if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
               'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
              nbmin = 2
           endif
        elseif( C3( 1: 1 ).eq.'M' )then
           if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
               'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
              nbmin = 2
           endif
        endif
     endif
     ilaenv = nbmin
     return

  70 continue
     !
     ! ISPEC = 3:  crossover point
     !
     NX = 0
     if( C2.eq.'GE' )then
        if( C3.eq.'QRF' .or. C3.eq.'RQF' .or. C3.eq.'LQF' .or. C3.eq. 'QLF' )then
           if( sname )then
              nx = 128
           else
              nx = 128
           endif
        elseif( c3.eq.'HRD' )then
           if( sname )then
              nx = 128
           else
              nx = 128
           endif
        elseif( c3.eq.'BRD' )then
           if( sname )then
              nx = 128
           else
              nx = 128
           endif
        endif
     elseif( c2.eq.'SY' )then
        if( sname .and. c3.eq.'TRD' )then
           nx = 32
        endif
     elseif( cname .and. c2.eq.'HE' )then
        if( C3.eq.'TRD' )then
           nx = 32
        endif
     elseif( sname .and. c2.eq.'OR' )then
        if( C3( 1: 1 ).eq.'G' )then
           if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
               'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
              nx = 128
           endif
        endif
     elseif( cname .and. c2.eq.'UN' )then
        if( C3( 1: 1 ).eq.'G' )then
           if( C4.eq.'QR' .or. C4.eq.'RQ' .or. C4.eq.'LQ' .or. C4.eq. &
               'QL' .or. C4.eq.'HR' .or. C4.eq.'TR' .or. C4.eq.'BR' )then
              nx = 128
           endif
        endif
     endif
     ilaenv = nx
     return

  80 continue
     !
     !ISPEC = 4:  number of shifts (used by xHSEQR)
     !
     ilaenv = 6
     return

  90 continue
     !
     ! ISPEC = 5:  minimum column dimension (not used)
     !
     ilaenv = 2
     return

 100 continue
     !
     ! ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
     !
     ilaenv = int( real( min( n1, n2 ) )*1.6e0 )
     return

 110 continue
     !
     ! ISPEC = 7:  number of processors (not used)
     !
     ilaenv = 1
     return

 120 continue
     !
     !     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
     !
     ilaenv = 50
     return

 130 continue
     !
     !     ISPEC = 9:  maximum size of the subproblems at the bottom of the
     !                 computation tree in the divide-and-conquer algorithm
     !                 (used by xGELSD and xGESDD)
     !
     ilaenv = 25
     return

 140 continue
     !
     !     ISPEC = 10: ieee NaN arithmetic can be trusted not to trap
     !
     !     ILAENV = 0
     ilaenv = 1
     if( ilaenv.eq.1 )then
    !   ilaenv = ieeeck( 0, 0.0, 1.0 )
     endif
     return

 150 continue
     !
     !     ISPEC = 11: infinity arithmetic can be trusted not to trap
     !
     !     ILAENV = 0
     ilaenv = 1
     if( ilaenv.eq.1 )then
    !   ilaenv = ieeeck( 1, 0.0, 1.0 )
     endif
     return

 160 continue
     !
     !    12 <= ISPEC <= 16: xHSEQR or one of its subroutines. 
     !
     !ILAENV = IPARMQ( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
     ilaenv = 0
     return

end function ilaenv
end module lapack_s
