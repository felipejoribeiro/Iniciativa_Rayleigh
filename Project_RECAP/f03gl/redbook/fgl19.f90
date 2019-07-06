program fgl19

  use opengl_gl
  use opengl_glu
  use opengl_glut

  interface
    subroutine display() bind(C)
    end subroutine display

    subroutine reshape(w,h) bind(C)
      use opengl_gl
      integer(glcint), intent(in), value :: w,h
    end subroutine reshape

    subroutine keyboard(key, x,y) bind(C)
      use opengl_gl
      integer(kind=GLbyte), intent(IN), value :: key
      integer(kind=GLint), intent(in), value  :: x, y
    end subroutine keyboard
  end interface

  integer(kind=GLint) :: winWidth = 100, winHeight = 100
  integer(kind=GLint) :: iwin
   
  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl19 Example 13-7"//char(0))

  call setup

  call glutDisplayFunc( display )
  call glutKeyboardFunc( keyboard )

  call glutMainLoop()

end program
subroutine setup

  use opengl_gl
    
  call glEnable(GL_LIGHTING)
  call glEnable(GL_LIGHT0)
  
end subroutine setup
subroutine display() bind(C)

  use OpenGL_GL
  use OpenGL_GLu
  use OpenGL_GLut
      
  real(kind=GLfloat), dimension(1024), target :: feedbuffer
  integer(kind=GLint) :: size
  
  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity
  call glOrtho( 0.0_gldouble, 100.0_gldouble, &
                0.0_gldouble, 100.0_gldouble, &
                0.0_gldouble,   1.0_gldouble  )

  call glClearColor(0.0,0.0,0.0,0.0)
  call glClear(GL_COLOR_BUFFER_BIT)

  call drawgeometry(GL_RENDER)

  call glFeedbackBuffer(1024, GL_3D_COLOR, c_loc(feedbuffer))
  idum = glRenderMode(GL_FEEDBACK)
  
  call drawgeometry(GL_FEEDBACK)
  
  size = glRenderMode(GL_RENDER)
  
  call printbuffer(size,feedbuffer)
  
end subroutine display
subroutine reshape(newWidth, newHeight) bind(C)

  use OpenGL_GL
  use OpenGL_GLU

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight
  real(kind=GLdouble) :: Zero, One, Width, Height, Aspect

  Zero   = 0.0
  One    = 1.0
  Width  = newWidth
  Height = newHeight
  Aspect = Width/Height

  call glviewport(0,0,newWidth,newHeight)

end subroutine reshape
subroutine keyboard(key, x,y) bind(C)

  use OpenGL_GL

  integer(kind=GLbyte), intent(in), value :: key
  integer(kind=GLint), intent(in), value  :: x, y

  if( key == 27) stop 'esc hit'
    
end subroutine keyboard
subroutine drawgeometry(mode)
  
  use OpenGL_GL
  use OpenGL_GLu

  integer(GLenum) :: mode

  write(*,*)'drawgeometry',mode

  call glBegin(GL_LINE_STRIP)
  call glNormal3f(  0.0,  0.0, 1.0)
  call glVertex3f( 30.0, 30.0, 0.0)
  call glVertex3f( 50.0, 60.0, 0.0)
  call glVertex3f( 70.0, 40.0, 0.0)
  call glEnd
 
  if( mode == GL_FEEDBACK ) call glPassThrough( 1.0 )
  
  call glBegin(GL_POINTS)  
  ! will be clipped:
  call glVertex3f( -100.0, -100.0, -100.0)
  call glEnd
  
  if( mode == GL_FEEDBACK ) call glPassThrough( 2.0 )
  
  call glBegin(GL_POINTS)
  call glNormal3f(  0.0,  0.0, 1.0)
  call glVertex3f( 50.0, 50.0, 0.0)
  call glEnd
  
  call glFlush
  
end subroutine drawgeometry
double precision function convert_uint_to_double( uint )

  integer, intent(in)    :: uint
  
  integer, dimension(32) :: bits(0:32)
  double precision       :: big
  
  bits = 0
  do i=0,31
    if( btest(uint,i) ) bits(i) = 1
  end do
  
  write(*,'('' bits>'',$)') 
  do i=31,0,-1
    write(*,'(i1,$)') bits(i)
  end do
  write(*,'(''<'')')

  ! check highest bit
  if( btest(uint,31) )then
    big = 4294967295.0
  else
    big = 0.0
  endif
  ! clear highest bit 
  iconv = ibclr(uint,31) 
  ! simple type conversion:

  convert_uint_to_double = dble(iconv) + big
  
end function convert_uint_to_double
subroutine printbuffer(size, buffer)

  use OpenGL_GL

  integer(kind=GLint), intent(in) :: size
  real(kind=GLfloat), dimension(*), intent(in) :: buffer
  
  real(kind=GLfloat) :: token

  write(*,*)'printbuffer', size

  ic = size
  do while( ic > 0 )
    
    token = buffer(1+size - ic)
    ic    = ic - 1
 
    if( token == GL_PASS_THROUGH_TOKEN )then
       write(*,*) 'GL_PASS_THROUGH_TOKEN', token
       write(*,*) '=>',buffer(1+size-ic)
       ic = ic - 1
    else if( token == GL_POINT_TOKEN )then
       write(*,*) 'GL_POINT_TOKEN', token
       call printcolor(size,ic,buffer)
    else if( token == GL_LINE_TOKEN )then
       write(*,*) 'GL_LINE_TOKEN', token
       call printcolor(size,ic,buffer)
       call printcolor(size,ic,buffer)
    else if( token == GL_LINE_RESET_TOKEN )then
       write(*,*) 'GL_LINE_RESET_TOKEN', token
       call printcolor(size,ic,buffer)
       call printcolor(size,ic,buffer)
    endif

  end do
  
end subroutine printbuffer
subroutine printcolor(size,icount,buffer)

  use OpenGL_GL

  integer, intent(inout)          :: icount
  integer(kind=GLint), intent(in) :: size
  real(kind=GLfloat), dimension(*), intent(in) :: buffer

  write(*,'(7(1x,f6.2))') (buffer(1+size-j),j=icount,icount-6,-1)
  icount = icount - 7

!  do i=1,7
!    write(*,*) '>',i,buffer(1+size-icount)
!    icount = icount - 1
!  end do

end subroutine printcolor


 
  
