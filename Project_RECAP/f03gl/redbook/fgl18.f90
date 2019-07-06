program fgl18

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

    subroutine picksquares(b,a,x,y) bind(C)
      use OpenGL_GL
      integer(GLint), intent(in), value :: b,a,x,y
    end subroutine picksquares

    subroutine keyboard(key, x,y) bind(C)
      use opengl_gl
      integer(kind=GLbyte), intent(in), value :: key
      integer(kind=GLint), intent(in), value  :: x, y
    end subroutine keyboard
  end interface

  integer(kind=GLint) :: winWidth = 200, winHeight = 200
  integer(kind=GLint) :: iwin
  
  common /board/ iboard(3,3)
  
  call glutInit()
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl18 Example 13-3"//char(0))

  call setup

  call glutMouseFunc( picksquares )
  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )
  call glutKeyboardFunc( keyboard )

  call glutMainLoop()

end program
subroutine setup

  use opengl_gl
    
  common /board/ iboard(3,3)

  iboard(:,:) = 0
  
  call glClearColor(0.,0.,0.,0.)

end subroutine setup
subroutine display() bind(C)

  use OpenGL_GL
  use OpenGL_GLu
  use OpenGL_GLut
      
  call glClear(GL_COLOR_BUFFER_BIT)

  call drawsquares(GL_RENDERER)

  call glflush()
  
end subroutine display
subroutine reshape(newWidth, newHeight) bind(C)

  use OpenGL_GL
  use OpenGL_GLU

  integer(kind=GLcint), intent(in), value :: newWidth, newHeight
  real(kind=GLdouble) :: Zero, One, Width, Height, Aspect

  Zero   = 0.0
  One    = 1.0
  Width  = newWidth
  Height = newHeight
  Aspect = Width/Height

  call glviewport(0,0,newWidth,newHeight)

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()

  call gluOrtho2D(zero,3.0_gldouble,zero,3.0_gldouble)

  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity()

end subroutine reshape
subroutine keyboard(key, x,y) bind(C)

  use OpenGL_GL

  integer(kind=GLbyte), intent(in), value :: key
  integer(kind=GLint), intent(in), value  :: x, y

  if( key == 27) stop 'esc hit'
    
end subroutine keyboard
subroutine drawsquares(mode)
  
  use OpenGL_GL
  use OpenGL_GLu

  integer(GLenum) :: mode

  common/board/ iboard(3,3)

  write(*,*)'drawsquares'

  do i=1,3
    if( mode == GL_SELECT ) call glLoadName(i)
    do j=1,3 
      if( mode == GL_SELECT ) call glPushName(j)
      call glColor3f(float(i-1)/3., float(j-1)/3., float(iboard(i,j))/3.)
      call glRecti(i-1,j-1,i,j)
      if( mode == GL_SELECT ) call glPopName
    end do
  end do
    
end subroutine drawsquares
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

  write(*,*) '>>>',convert_uint_to_double,'<<<'
  
end function convert_uint_to_double
subroutine processhits( BUFSIZE, hits, buffer )

  use opengl_gl

  integer(kind=GLsizei), intent(in) :: BUFSIZE
  integer(kind=GLint), intent(in) :: hits
  integer(kind=GLuint), dimension(BUFSIZE), intent(in) :: buffer 

  integer(kind=GLuint) :: names 
  double precision :: z1, z2, conv, convert_uint_to_double

  common/board/ iboard(3,3)

  write(*,*)'hits:',hits
  
  ! Depth values (which are in the range [0,1]) are multiplied by 
  ! 2^32 - 1, before being placed in the hit record. 
  conv = 1.0D0/convert_uint_to_double(z'7fffffff') 

  j=0
  do i=1,hits
    j=j+1
    names = buffer(j)
    write(*,*)'number of names:',names
    j=j+1
    z1 = convert_uint_to_double(buffer(j))*conv
    write(*,*)'z1:',z1
    
    j=j+1
    z2 = convert_uint_to_double(buffer(j))*conv
    write(*,*)'z2:',z2
    
    write(*,*)'name:'
    do k=1,names
      j=j+1
      write(*,*)'>',buffer(j)
      if( k == 1 )then
        ii = buffer(j)
      else if( k == 2 )then
        jj = buffer(j)
      endif
    end do
    iboard(ii,jj) = mod(iboard(ii,jj)+1,3)
  end do
  
end subroutine processhits
subroutine picksquares( ibutton, iaction, ix, iy ) bind(C)

  use opengl_gl     
  use opengl_glu
  use opengl_glut
  			
  integer(GLint), intent(in), value :: ibutton, iaction, ix, iy  
  real(GLdouble) :: x, y
  
  integer(kind=GLsizei), parameter :: BUFSIZE = 512
  
  integer(GLuint), dimension(BUFSIZE), target :: selectBuf
  integer(kind=GLint)  :: hits
  integer(GLint), dimension(4) :: viewport
  
  write(*,*)'picksquares'

  if( ibutton /= GLUT_LEFT_BUTTON .and. &
      iaction /= GLUT_DOWN ) return

  call glGetIntegerv(GL_VIEWPORT, viewport)

  call glSelectBuffer(BUFSIZE, c_loc(selectBuf))
  i = glRenderMode(GL_SELECT)
  write(*,*)'1, i=',i
  
  call glInitNames
  call glPushName(0)
  
  call glMatrixMode(GL_PROJECTION)
  call glPushMatrix
  call glLoadIdentity
  
  ! 5x5 pixel region near cursor
  x = ix
  y = viewport(4) - iy
  call gluPickMatrix(x,y,5.0_gldouble,5.0_gldouble, viewport)
  
  call gluOrtho2D(0.0_gldouble, 3.0_gldouble, &
                  0.0_gldouble, 3.0_gldouble  )
  
  call drawsquares(GL_SELECT)  
  write(*,*)'continue picksquares'
	       
  call glMatrixMode(GL_PROJECTION)

  call glPopMatrix
  call glFlush
  
  hits = glRendermode(GL_RENDER)
  write(*,*)'hits:',hits,' processing'

  call processhits(BUFSIZE, hits, selectBuf)
  call glutPostRedisplay
  
end subroutine picksquares

