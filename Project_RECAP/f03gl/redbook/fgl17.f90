program fgl17

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

  integer(kind=GLint) :: winWidth = 400, winHeight = 400
  integer(kind=GLint) :: iwin
  
  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB + GLUT_DEPTH )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl17 Example 13-2"//char(0))

  call setup

  call glutDisplayFunc( display )
 !call glutReshapeFunc( reshape )
  call glutKeyboardFunc( keyboard )

  call glutMainLoop()

end program
subroutine setup

  use opengl_gl
    
  call glEnable(GL_DEPTH_TEST)
  call glShadeModel(GL_FLAT)

end subroutine setup
subroutine display() bind(C)

  use OpenGL_GL
  use OpenGL_GLu
  use OpenGL_GLut
      
  call glClearColor(0.0,0.0,0.0,0.0)
  call glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT )

  call drawscene
  call axis(2.5)
    
  call selectobjects

  call glflush()
  
end subroutine display
subroutine keyboard(key, x,y) bind(C)

  use OpenGL_GL

  integer(kind=GLbyte), intent(IN), value :: key
  integer(kind=GLint), intent(in), value  :: x, y

  if( key == 27) stop 'esc hit'
    
end subroutine keyboard
subroutine drawtriangle(x1,y1,x2,y2,x3,y3,z)

  use OpenGL_GL
  
  call glBegin(GL_TRIANGLES)
  call glVertex3f(x1,y1,z)
  call glVertex3f(x2,y2,z)
  call glVertex3f(x3,y3,z)
  call glEnd()
  
end subroutine drawtriangle
subroutine drawvolume(x1,x2,y1,y2,z1,z2)

  use OpenGL_GL
  
  call glColor3f(1.0,1.0,1.0)
  call glBegin(GL_LINE_LOOP)
  call glVertex3f(x1,y1,-z1)
  call glVertex3f(x2,y1,-z1)
  call glVertex3f(x2,y2,-z1)
  call glVertex3f(x1,y2,-z1)
  call glEnd()
  
  call glBegin(GL_LINE_LOOP)
  call glVertex3f(x1,y1,-z2)
  call glVertex3f(x2,y1,-z2)
  call glVertex3f(x2,y2,-z2)
  call glVertex3f(x1,y2,-z2)
  call glEnd()
  
  ! 4 lines
  call glBegin(GL_LINES)
  call glVertex3f(x1,y1,-z1)
  call glVertex3f(x1,y1,-z2)

  call glVertex3f(x1,y2,-z1)
  call glVertex3f(x1,y2,-z2)

  call glVertex3f(x2,y1,-z1)
  call glVertex3f(x2,y1,-z2)

  call glVertex3f(x2,y2,-z1)
  call glVertex3f(x2,y2,-z2)
  call glEnd()
  
end subroutine drawvolume
subroutine drawscene
  
  use OpenGL_GL
  use OpenGL_GLu

  write(*,*)'drawscene'

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()
  call gluPerspective(40.0_gldouble, 0.75_gldouble, &
                      1.0_gldouble, 100.0_gldouble)
  
  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity()

  call gluLookAt(20.0_gldouble, 7.5_gldouble, 12.5_gldouble, &
                  2.5_gldouble, 2.5_gldouble, -5.0_gldouble, &
                  0.0_gldouble, 1.0_gldouble,  0.0_gldouble  )

  call glColor3f(0.0,1.0,0.0)
  call drawtriangle(2.,2.,3.,2.,2.5,3.,-2.)
  
  call glColor3f(1.0,0.0,0.0)
  call drawtriangle(2.,7.,3.,7.,2.5,8.,-5.)
  
  call glColor3f(1.0,1.0,0.0)
  !call drawtriangle(2.,2.,3.,2.,2.5,3.,  0.0)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -2.5)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -5.0)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -7.5)
  !call drawtriangle(2.,2.,3.,2.,2.5,3.,-10.0)

  call glColor3f(0.0,0.0,1.0)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -1.0)

  call glColor3f(1.0,0.0,1.0)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -9.5)

  call glColor3f(1.0,1.0,0.0)
  call drawtriangle(1.5,1.5,3.5,1.5,2.5,3.5,-9.99)

  call glColor3f(1.0,1.0,1.0)
  call drawtriangle(1.0,1.0,4.0,1.0,2.5,4.0,-10.0)

  call drawvolume(0.0,5.0, 0.0,5.0 ,0.0,10.)
  
end subroutine drawscene
function convert_uint_to_real( uint )

  integer, intent(in)    :: uint
  
  integer, dimension(32) :: bits(0:32)
  real                   :: big
  
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
    big = float(4294967295)
  else
    big = 0.0
  endif
  ! clear highest bit 
  iconv = ibclr(uint,31) 
  ! simple type conversion:

  convert_uint_to_real = float(iconv) + big
  
end function convert_uint_to_real
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
subroutine processhits( hits, buffer)

  use opengl_gl

  integer(kind=GLint)  :: hits
  integer(kind=GLuint) :: names 

  integer(GLuint), dimension(512) :: buffer 
  
  real(kind=8)    :: z, conv, convert_uint_to_double
  integer(kind=4) :: iz
  
  write(*,*)'hits:',hits
  
  ! Depth values (which are in the range [0,1]) are multiplied by 
  ! 2^32 - 1, before being placed in the hit record. 
  conv = 1.0/convert_uint_to_double(4294967295)
  j=0
  do i=1,hits
    j=j+1
    names = buffer(j)
    write(*,*)'number of names:',names
    j=j+1
    z = convert_uint_to_double(buffer(j))
    write(*,*)'z1:',buffer(j),z,z*conv
    j=j+1
    z = convert_uint_to_double(buffer(j))
    write(*,*)'z2:',buffer(j),z,z*conv
    write(*,*)'name:'
    do k=1,names
      j=j+1
      write(*,*)'>',buffer(j)
    end do
  end do
  
end subroutine processhits
subroutine selectobjects

  use opengl_gl, only : GLsizei, C_PTR, GLint, GLuint, GLdouble, &
                        GL_SELECT, GL_PROJECTION, GL_MODELVIEW,  &
			GL_RENDER, &
                        glInitNames, glPushName, glLoadName,     &
			glPushMatrix,   &
                        glMatrixMode, glLoadIdentity, glOrtho,   &
		        glPopMatrix, glFlush, glRendermode
  interface 
    subroutine glSelectBuffer(size, buffer) BIND(C,NAME="glSelectBuffer")
      use opengl_gl, only : GLsizei, C_PTR, GLint, GLuint
      integer(GLsizei), VALUE :: size
      integer(GLuint), dimension(size) :: buffer
    end subroutine glSelectBuffer
  end interface


  integer(kind=GLsizei), parameter :: BUFSIZE = 512
  
  integer(GLuint), dimension(BUFSIZE)          :: selectBuf
  integer(kind=GLint)  :: hits
  
  write(*,*)'selectobjects'

  call glSelectBuffer(BUFSIZE, selectBuf)
  i = glRenderMode(GL_SELECT)
  write(*,*)'1, i=',i
  
  call glInitNames
  call glPushName(0)
  
  call glPushMatrix()
  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity
  
  call glOrtho(0.0_gldouble,  5.0_gldouble, &
               0.0_gldouble,  5.0_gldouble, &
               0.0_gldouble, 10.0_gldouble  )
               
  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity
  
  call glLoadName(1)
  call drawtriangle(2.,2.,3.,2.,2.5,3.,-2.)
  
  call glLoadName(2)
  call drawtriangle(2.,7.,3.,7.,2.5,8.,-5.)
  
  call glLoadName(3)
  !call drawtriangle(2.,2.,3.,2.,2.5,3.,  0.0)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -2.5)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -5.0)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -7.5)
  !call drawtriangle(2.,2.,3.,2.,2.5,3.,-10.0)

  call glLoadName(4)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -1.0)

  call glLoadName(5)
  call drawtriangle(2.,2.,3.,2.,2.5,3., -9.5)

  call glLoadName(6)
  call drawtriangle(1.5,1.5,3.5,1.5,2.5,3.5,-9.99)

  call glLoadName(7)
  call drawtriangle(1.0,1.0,4.0,1.0,2.5,4.0,-10.0)

  call glPopMatrix
  call glFlush
  
  hits = glRendermode(GL_RENDER)
  call processhits(hits, selectBuf)
  
end subroutine selectobjects
subroutine axis(size)

  use opengl_gl
  
  real, intent(in)   :: size
  real(kind=GLfloat) :: glsize
  
  glsize = size
  
  call glLineWidth(3.)
  call glColor3f(1.0,0.0,0.0) ! rood x-as
  call glBegin(GL_LINES)
    call glVertex3f(0.0,0.0,0.0)
    call glVertex3f(glsize,0.0,0.0)
  call glEnd()

  call glColor3f(1.0,1.0,0.0) ! geel y-as
  call glBegin(GL_LINES)
    call glVertex3f(0.0,0.0,0.0)
    call glVertex3f(0.0,glsize,0.0)
  call glEnd()

  call glColor3f(0.0,1.0,0.0) ! groen z-as
  call glBegin(GL_LINES)
    call glVertex3f(0.0,0.0,0.0)
    call glVertex3f(0.0,0.0,glsize)
  call glEnd()
  call glLineWidth(1.)

end subroutine axis

  
