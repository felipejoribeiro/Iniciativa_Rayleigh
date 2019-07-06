program fgl13

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
  end interface

  integer(kind=GLint) :: winWidth = 400, winHeight = 400
  integer(kind=GLint) :: iwin

  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl13 Example 3-1"//char(0))

  call glClearColor(0.0,0.0,0.0,0.0)
  call glShadeModel(GL_FLAT)

  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )

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

  call glClear(GL_COLOR_BUFFER_BIT)

  call glPushMatrix()

  call gluLookAt( 1.0_gldouble, 2.0_gldouble, 2.0_gldouble, & ! eye
                  0.0_gldouble, 0.0_gldouble, 0.0_gldouble, & ! at
                  0.0_gldouble, 1.0_gldouble, 0.0_gldouble  ) ! up

  call glColor3f(1.0,0.0,0.0) ! rood x-as
  call glBegin(GL_LINES)
    call glVertex3f(0.0,0.001,0.001)
    call glVertex3f(1.5,0.001,0.001)
  call glEnd()

  call glColor3f(1.0,1.0,0.0) ! geel y-as
  call glBegin(GL_LINES)
    call glVertex3f(0.001,0.0,0.001)
    call glVertex3f(0.001,1.5,0.001)
  call glEnd()

  call glColor3f(0.0,1.0,0.0) ! groen z-as
  call glBegin(GL_LINES)
    call glVertex3f(0.001,0.001,0.0)
    call glVertex3f(0.001,0.001,1.5)
  call glEnd()

  call glColor3f(1.0,1.0,1.0)

  call glTranslatef( 0.001, 0.001, 0.0 )
  
  ! clip lower half
  call glClipPlane(GL_CLIP_PLANE0, eqn1)
  call glEnable(GL_CLIP_PLANE0)
  
  ! clip left half
  call glClipPlane(GL_CLIP_PLANE1, eqn2)
  call glEnable(GL_CLIP_PLANE1)

  call glRotatef(90.0, 1.0, 0.0, 0.0)


  call glutWireSphere(1.0_gldouble, 20,16)

  call glPopMatrix()
  call glFlush()

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

end subroutine reshape
