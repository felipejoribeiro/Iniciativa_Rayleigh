program fgl14

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

  real(kind=glfloat)  :: year, day
  common/waar/ year, day

  year = 0.0
  day  = 0.0
  
  call glutInit
  call glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl14 Example 3-6"//char(0))

  call glClearColor(0.0,0.0,0.0,0.0)
  call glShadeModel(GL_FLAT)

  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )
  call glutKeyboardFunc( keyboard )

  call glutMainLoop()

end program
subroutine display() bind(C)

  use OpenGL_GL
  use OpenGL_GLu
  use OpenGL_GLut
  
  real(kind=GLdouble), dimension(4) :: eqn1 = (/ &
                              0.0, 1.0, 0.0, 0.0 /)
  real(kind=GLdouble), dimension(4) :: eqn2 = (/ &
                              1.0, 0.0, 0.0, 0.0 /)

  real(kind=glfloat) :: year, day
  common/waar/ year, day
   
  call glClear(GL_COLOR_BUFFER_BIT)

  call glPushMatrix()

  call axis(2.5)

  call glColor3f(1.0,1.0,1.0) ! wit
  ! zon
  call glutWireSphere(1.0_gldouble, 20,16)

  ! planeet
  call glRotatef(Year, 0.0, 1.0, 0.0)
  call glTranslatef( 2.8, 0.0, 0.0 )
  
  call axis(0.4)
  call glRotatef(Day, 0.0, 1.0, 0.0)
 
  call glColor3f(1.0,1.0,1.0) ! wit
  call glPushMatrix()
  call glRotatef(20.0, 0.0, 0.0, 1.0)
  call glutWireSphere(0.3_gldouble, 10,8)
  call glPopMatrix()

  ! maan1
  call glPushMatrix()

  call glRotatef(Year, 0.0, 1.0, 0.0)
  call glTranslatef( 0.4, 0.0, 0.0 )
  
  call axis(0.2)
  call glRotatef(Day, 0.0, 1.0, 0.0)
 
  call glColor3f(1.0,1.0,1.0) ! wit
  call glutWireSphere(0.1_gldouble, 10,8)
  call glPopMatrix()

  ! maan2
  call glPushMatrix()

  call glRotatef(4*Year, 0.0, 1.0, 0.0)
  call glTranslatef( 0.8, 0.0, 0.0 )
  
  call axis(0.2)
  call glRotatef(Day*2, 0.0, 1.0, 0.0)
 
  call glColor3f(1.0,1.0,1.0) ! wit
  call glutWireSphere(0.05_gldouble, 10,8)
  call glPopMatrix()


  call glPopMatrix()
  call glutSwapBuffers()

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

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()

  call gluPerspective(60.0_gldouble,Aspect,One,20.0_gldouble)
  call glMatrixMode(GL_MODELVIEW)

  call glLoadIdentity()
  call gluLookAt( 1.0_gldouble, 3.0_gldouble, 5.0_gldouble, & ! eye
                  0.0_gldouble, 0.0_gldouble, 0.0_gldouble, & ! at
                  0.0_gldouble, 1.0_gldouble, 0.0_gldouble  ) ! up

end subroutine reshape
subroutine keyboard(key, x,y) bind(C)

  use OpenGL_GL
  use OpenGL_GLut

  integer(kind=GLbyte), intent(IN), value :: key
  integer(kind=GLint), intent(in), value  :: x, y

  real(kind=glfloat) :: year, day
  common/waar/ year, day

  select case(key)
  case(iachar('d'))
    day = day+30.
    call glutPostRedisplay()
    
  case(iachar('D'))
    day = day-30.
    call glutPostRedisplay()
    
  case(iachar('y'))
    year = year+30
    call glutPostRedisplay()

  case(iachar('Y'))
    year = year-30
    call glutPostRedisplay()

  case(27)
    stop 'esc hit'
  
  end select
  
end subroutine keyboard
subroutine axis(size)

  use opengl_gl
  
  real, intent(in)   :: size
  real(kind=GLfloat) :: glsize
  
  glsize = size
  
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

end subroutine axis
