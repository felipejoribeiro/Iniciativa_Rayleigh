!
! original file:
!
! Math3d.c
! Implementation of non-inlined functions in the Math3D Library
! Richard S. Wright Jr.
!
!/////////////////////////////////////////////////////////////////////////////
!/////////////////////////////////////////////////////////////////////////////
! Misc. Utilities
!/////////////////////////////////////////////////////////////////////////////

!/////////////////////////////////////////////////////////////////////////////
!
! Calculate the plane equation of the plane that the three specified points lay
! in. The points are given in clockwise winding order, with normal pointing 
! out of clockwise face planeEq contains the A,B,C, and D of the plane 
! equation coefficients
!
subroutine m3dGetPlaneEquation( planeEq, p1, p2, p3)

   real,dimension(3), intent(in)  :: p1, p2, p3
   real,dimension(4), intent(out) :: planeEq
   real,dimension(3)              :: v1, v2

   ! Get two vectors... do the cross product

   v1 = p3 - p1
   v2 = p2 - p1

   ! Unit normal to plane - Not sure which is the best way here
   call m3dCrossProduct(planeEq, v1, v2)
   call m3dNormalizeVector(planeEq)
   
   ! Back substitute to get D
   planeEq(4) = -(planeEq(1)*p3(1) + planeEq(2)*p3(2) + planeEq(3)*p3(3))

end subroutine m3dGetPlaneEquation
!/////////////////////////////////////////////////////////////////////////
!
! Create a projection to "squish" an object into the plane.
! Use m3dGetPlaneEquationf(planeEq, point1, point2, point3);
! to get a plane equation.
!
subroutine m3dMakePlanarShadowMatrix( proj, planeEq, vLightPos )

   real, dimension(4,4), intent(out) :: proj
   real, dimension(4), intent(in)    :: planeEq
   real, dimension(3), intent(in)    :: vLightPos

   real :: a, b, c, d, dx, dy, dz
   
   ! These just make the code below easier to read. They will be 
   ! removed by the optimizer.    
   a  =  planeEq(1)
   b  =  planeEq(2)
   c  =  planeEq(3)
   d  =  planeEq(4)

   dx = -vLightPos(1)
   dy = -vLightPos(2)
   dz = -vLightPos(3)

   ! Now build the projection matrix
   proj(1,1) =  b * dy + c * dz
   proj(2,1) = -a * dy
   proj(3,1) = -a * dz
   proj(4,1) = 0.0

   proj(1,2) = -b * dx
   proj(2,2) =  a * dx + c * dz
   proj(3,2) = -b * dz
   proj(4,2) = 0.0

   proj(1,3) = -c * dx
   proj(2,3) = -c * dy
   proj(3,3) = a * dx + b * dy
   proj(4,3) = 0.0

   proj(1,4) = -d * dx
   proj(2,4) = -d * dy
   proj(3,4) = -d * dz
   proj(4,4) = a * dx + b * dy + c * dz
   ! Shadow matrix ready

end subroutine m3dMakePlanarShadowMatrix
!/////////////////////////////////////////////////////////////////////////
!
! normal stuff
!
subroutine m3dCrossProduct( result, u, v)
	
   real,dimension(*), intent(in)  :: u, v
   real,dimension(*), intent(out) :: result
   
   result(1) =  u(2)*v(3) - v(2)*u(3)
   result(2) = -u(1)*v(3) + v(1)*u(3)
   result(3) =  u(1)*v(2) - v(1)*u(2)

end subroutine m3dCrossProduct
real function m3dGetVectorLength( u )

   real,dimension(3), intent(in) :: u 

   m3dGetVectorLength = sqrt(dot_product(u,u))

end function m3dGetVectorLength
real function m3dGetVectorLengthSquared( u )

   real,dimension(3), intent(in) :: u 

   m3dGetVectorLengthSquared = dot_product(u,u)
   
end function m3dGetVectorLengthSquared
subroutine m3dNormalizeVector( u )

   real,dimension(3), intent(inout) :: u 

   f = 1.0/sqrt(dot_product(u,u))
   u = f * u

end subroutine m3dNormalizeVector
subroutine m3dMatrixMultiply44(c,a,b)

   real, dimension(4,4), intent(in)  :: a, b
   real, dimension(4,4), intent(out) :: c

   do i=1,4
     do j=1,4
       c(i,j) = 0.0
       do k=1,4
         c(i,j) = c(i,j) + a(i,k)*b(k,j)
       end do
     end do
   end do
   
end subroutine m3dMatrixMultiply44 
!/////////////////////////////////////////////////////////////////////////
! Useful shortcuts  
! Radians are king... but we need a way to swap back and forth
real function m3dDegToRad(x)	

   m3dDegToRad = x * 0.017453292519943296

end function m3dDegToRad
real function m3dRadToDeg(x)	

   m3dRadToDeg = x * 57.2957795130823229

end function m3dRadToDeg
! Hour angles
real function m3dHrToDeg(x)	
   
   m3dHrToDeg = x / 15.0

end function m3dHrToDeg
real function m3dHrToRad(x)	

   real m3dDegToRad, m3dHrToDeg
   external m3dDegToRad, m3dHrToDeg

   m3dHrToRad = m3dDegToRad(m3dHrToDeg(x))

end function m3dHrToRad
real function m3dDegToHr(x)  
 
   m3dDegToHr = x * 15.0
   
end function m3dDegToHr
real function m3dRadToHr(x)	

   real m3dDegToHr, m3dRadToDeg
   external m3dDegToHr, m3dRadToDeg

   m3dRadToHr = m3dDegToHr(m3dRadToDeg(x))

end function m3dRadToHr
!/////////////////////////////////////////////////////////////////////////
!
! Creates a 4x4 rotation matrix, takes radians NOT degrees
!
! ifort (correctly) segv on xin, yin, and zin not being intent(in)
!
subroutine m3dRotationMatrix44(mat, angle, xin, yin, zin)

   real, dimension(16), intent(out) :: mat
   real, intent(in) :: xin, yin, zin     
   real, intent(in) :: angle

   real, dimension(4,4) :: m
   real    :: s, c, mag, rmag
   real    :: x, y, z, xx, yy, zz, xy, yz, zx, xs, ys, zs, onemc
   integer :: i, j
   
   x = xin
   y = yin
   z = zin
   
   s = sin(angle)
   c = cos(angle)
   ! Length of vector
   mag = sqrt( x*x + y*y + z*z )

   ! Identity matrix
   if( mag == 0.0 )then
     call m3dLoadIdentity44(mat)
     return
   endif

   ! Rotation matrix is normalized
   rmag = 1.0/mag
   x = x*rmag
   y = y*rmag
   z = z*rmag
   
   xx = x * x
   yy = y * y
   zz = z * z
   xy = x * y
   yz = y * z
   zx = z * x
   xs = x * s
   ys = y * s
   zs = z * s
   
   onemc = 1.0 - c

   m(1,1) = (onemc * xx) + c 
   m(2,1) = (onemc * xy) - zs
   m(3,1) = (onemc * zx) + ys
   m(4,1) = 0.0

   m(1,2) = (onemc * xy) + zs
   m(2,2) = (onemc * yy) + c
   m(3,2) = (onemc * yz) - xs
   m(4,2) = 0.0

   m(1,3) = (onemc * zx) - ys
   m(2,3) = (onemc * yz) + xs
   m(3,3) = (onemc * zz) + c
   m(4,3) = 0.0

   m(1,4) = 0.0
   m(2,4) = 0.0
   m(3,4) = 0.0
   m(4,4) = 1.0

   do j=1,4
     do i=1,4
       mat(j+(i-1)*4) = m(i,j)
     end do
   end do

end subroutine m3dRotationMatrix44
subroutine m3dLoadIdentity44(matrix)

   real, dimension(16) :: matrix
   
   matrix( 1) = 1.0
   matrix( 2) = 0.0
   matrix( 3) = 0.0
   matrix( 4) = 0.0
   
   matrix( 5) = 0.0
   matrix( 6) = 1.0
   matrix( 7) = 0.0
   matrix( 8) = 0.0
   
   matrix( 9) = 0.0
   matrix(10) = 0.0
   matrix(11) = 1.0
   matrix(12) = 0.0
   
   matrix(13) = 0.0
   matrix(14) = 0.0
   matrix(15) = 0.0
   matrix(16) = 1.0

end subroutine m3dLoadIdentity44
subroutine m3dTransformVector3(vOut,v, m)
!
! Transform - Does rotation and translation via a 4x4 matrix. Transforms
! a point or vector.
!
   real, dimension(16), intent(in)  :: m
   real, dimension( 3), intent(in)  :: v
   real, dimension( 3), intent(out) :: vout

   ! vo1   | m1 m5  m9 m13 |   v1
   ! vo2 = | m2 m6 m10 m14 | . v2
   ! vo3   | m3 m7 m11 m15 |   v3
   !  .    ! m4 m8 m12 m16 |    1
   
   vOut(1) = m(1) * v(1) + m(5) * v(2) + m( 9) * v(3) + m(13)      
   vOut(2) = m(2) * v(1) + m(6) * v(2) + m(10) * v(3) + m(14)     
   vOut(3) = m(3) * v(1) + m(7) * v(2) + m(11) * v(3) + m(15)     

end subroutine m3dTransformVector3
subroutine m3dSetMatrixColumn44(mat,vec,column)

   real, dimension(16), intent(inout) :: mat
   real, dimension( 3), intent(in)    :: vec
   integer, intent(in) :: column

   do i=1,3
     mat((column-1)*4+i) = vec(i)
   end do

end subroutine m3dSetMatrixColumn44 
!/////////////////////////////////////////////////////////////////////////////
!/////////////////////////////////////////////////////////////////////////////
! Misc. Utilities
!/////////////////////////////////////////////////////////////////////////////
!
! Calculates the normal of a triangle specified by the three points
! p1, p2, and p3. Each pointer points to an array of three floats. The
! triangle is assumed to be wound counter clockwise. 
!
subroutine m3dFindNormal(vec,pnt1,pnt2,pnt3)

   real, dimension(3), intent(in)  :: pnt1, pnt2, pnt3
   real, dimension(3), intent(out) :: vec
   
   real, dimension(3) :: v1, v2
   
   ! Calculate two vectors from the three points. 
   ! Assumes counter clockwise winding!
   v1(1) = pnt1(1) - pnt2(1)
   v1(2) = pnt1(2) - pnt2(2)
   v1(3) = pnt1(3) - pnt2(3)

   v2(1) = pnt2(1) - pnt3(1)
   v2(2) = pnt2(2) - pnt3(2)
   v2(3) = pnt2(3) - pnt3(3)

   ! Take the cross product of the two vectors to get
   ! the normal vector.
   call m3dCrossProduct(vec, v1, v2)
   
end subroutine m3dFindNormal 
subroutine m3dInvertMatrix44(dst,src)

   real, dimension(4,4) :: dst, src , c, d
   logical :: sngl                         

   sngl = .false.
   
   ! source redden
   !
   d = src

   ! check of de diagonaal niet een nul heeft
   !
   do i=1,4
     if( abs(src(i,i)) < 1.e-6 ) sngl = .true.
   end do
    
   if( sngl )then
     write(*,*) 'Matrix singulier'
     do i=1,4
       write(*,*)'>',(src(i,j),j=1,4)
     end do
   endif
   
   dst = 0.0
   do i=1,4
     dst(i,i) = 1.0
   end do

   ! neem bovenste rij
   f = src(1,1)
   do j=1,4
     src(1,j) = src(1,j) / f
     dst(1,j) = dst(1,j) / f
   end do
     
   do i=2,4
     if( abs(src(i,1)) > 1.e-6 )then
       f = src(i,1)/src(1,1)
       do j=1,4
	 src(i,j) = src(i,j) - f*src(1,j)
	 dst(i,j) = dst(i,j) - f*dst(1,j)
       end do
     endif
   end do

   ! neem 2e rij
   f = src(2,2)
   do j=1,4
     src(2,j) = src(2,j) / f
     dst(2,j) = dst(2,j) / f
   end do
     
   if( abs(src(2,2)) > 1.e-6 )then
     f = src(1,2)/src(2,2)
     do j=1,4
       src(1,j) = src(1,j) - f*src(2,j)
       dst(1,j) = dst(1,j) - f*dst(2,j)
     end do
   endif

   do i=3,4
     if( abs(src(i,2)) > 1.e-6 )then
       f = src(i,2)/src(2,2)
       do j=1,4
	 src(i,j) = src(i,j) - f*src(2,j)
	 dst(i,j) = dst(i,j) - f*dst(2,j)
       end do
     endif
   end do

   ! neem 3e rij
   f = src(3,3)
   do j=1,4
     src(3,j) = src(3,j) / f
     dst(3,j) = dst(3,j) / f
   end do
     
   do i=1,2
     if( abs(src(i,3)) > 1.e-6 )then
       f = src(i,3)/src(3,3)
       do j=1,4
	 src(i,j) = src(i,j) - f*src(3,j)
	 dst(i,j) = dst(i,j) - f*dst(3,j)
       end do
     endif
   end do

   do i=4,4
     if( abs(src(i,3)) > 1.e-6 )then
       f = src(i,3)/src(3,3)
       do j=1,4
	 src(i,j) = src(i,j) - f*src(3,j)
	 dst(i,j) = dst(i,j) - f*dst(3,j)
       end do
     endif
   end do

   ! neem 4e rij
   f = src(4,4)
   do j=1,4
     src(4,j) = src(4,j) / f
     dst(4,j) = dst(4,j) / f
   end do

   do i=1,3
     if( abs(src(i,4)) > 1.e-6 )then
       f = src(i,4)/src(4,4)
       do j=1,4
	 src(i,j) = src(i,j) - f*src(4,j)
	 dst(i,j) = dst(i,j) - f*dst(4,j)
       end do
     endif
   end do

   do i=1,4
     do j=1,4
       if( abs(dst(i,j)) < 1.e-6 ) dst(i,j) = 0.0
     end do
   end do
   
  !do i=1,4
  !  do j=1,4
  !    c(i,j) = 0.0
  !    do k=1,4
  !      c(i,j) = c(i,j) + d(i,k)*dst(k,j)
  !    end do
  !  end do
  !end do

   src = d
   
end subroutine m3dInvertMatrix44
logical function m3dCloseEnough(r1,r2,e)

  real, intent(in) :: r1, r2, e

  if( abs(r1-r2) < e )then
    m3dCloseEnough = .true.
  else
    m3dCloseEnough = .false.
  endif

end function m3dCloseEnough
subroutine m3dTranslateMatrix44(m,x,y,z)

   real, dimension(16), intent(inout) :: m
   real, intent(in)    :: x,y,z

   m(13) = m(13) + x
   m(14) = m(14) + y
   m(15) = m(15) + z
   
end subroutine m3dtranslatematrix44
subroutine m3dScaleMatrix44(m,x,y,z)

   real, dimension(16), intent(inout) :: m
   real, intent(in)    :: x,y,z

   m( 1) = m( 1) * x
   m( 6) = m( 6) * y
   m(11) = m(11) * z
   
end subroutine m3dScaleMatrix44
subroutine m3dTransposeMatrix44(dst,src)

   real, dimension(4,4), intent(in)  :: src
   real, dimension(4,4), intent(out) :: dst

   do j=1,4
     do i=1,4
       dst(i,j) = src(j,i)
     end do
   end do
     
end subroutine m3dTransposeMatrix44


