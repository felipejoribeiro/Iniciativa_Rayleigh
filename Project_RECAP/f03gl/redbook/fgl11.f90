program fgl11

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

  integer(kind=GLint) :: winWidth = 350, winHeight = 150
  integer(kind=GLint) :: iwin

  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl11 Example 2-6"//char(0))

  call glClearColor(0.0,0.0,0.0,0.0)

  call glShadeModel(GL_FLAT)

  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )

  call glutMainLoop()

end program
subroutine display() bind(C)

  use OpenGL_GL

  integer(GLushort) :: iz
  real(GLfloat)     :: x, y
  

  integer(Glubyte), dimension(128) :: fly = (/ &
    z'00', z'00', z'00', z'00', z'00', z'00', z'00', z'00', &
    z'03', z'80', z'01', z'c0', z'06', z'c0', z'03', z'60', &
    z'04', z'60', z'06', z'20', z'04', z'30', z'0c', z'20', &
    z'04', z'18', z'18', z'20', z'04', z'0c', z'30', z'20', &
    z'04', z'06', z'60', z'20', z'44', z'03', z'c0', z'22', &
    z'44', z'01', z'80', z'22', z'44', z'01', z'80', z'22', &
    z'44', z'01', z'80', z'22', z'44', z'01', z'80', z'22', &
    z'44', z'01', z'80', z'22', z'44', z'01', z'80', z'22', &
    z'66', z'01', z'80', z'66', z'33', z'01', z'80', z'cc', &
    z'19', z'81', z'81', z'98', z'0c', z'c1', z'83', z'30', &
    z'07', z'e1', z'87', z'e0', z'03', z'3f', z'fc', z'c0', &
    z'03', z'31', z'8c', z'c0', z'03', z'33', z'cc', z'c0', &
    z'06', z'64', z'26', z'60', z'0c', z'cc', z'33', z'30', &
    z'18', z'cc', z'33', z'18', z'10', z'c4', z'23', z'08', &
    z'10', z'63', z'c6', z'08', z'10', z'30', z'0c', z'08', &
    z'10', z'18', z'18', z'08', z'10', z'00', z'00', z'08'  /)

  integer(Glubyte), dimension(128) :: halftone = (/ &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55', &
    z'aa', z'aa', z'aa', z'aa', z'55', z'55', z'55', z'55'  /)

  call glClear(GL_COLOR_BUFFER_BIT)

  call glColor3f(1.0,1.0,1.0)
  call glrectf(25.0,25.0,125.,125.)

  call glenable(GL_POLYGON_STIPPLE)

  call glpolygonstipple(fly)
  call glrectf(125.0,25.0,225.,125.)

  call glpolygonstipple(halftone)
  call glrectf(225.0,25.0,325.,125.)

  call gldisable(GL_POLYGON_STIPPLE)

  call glFlush()

end subroutine display
subroutine reshape(newWidth, newHeight) bind(C)

  use OpenGL_GL
  use OpenGL_GLU

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight
  real(kind=GLdouble) :: Zero, Width, Height

  Zero   = 0.0
  Width  = newWidth
  Height = newHeight

  call glviewport(0,0,newWidth,newHeight)

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()

  call gluOrtho2D( Zero, Width, Zero, Height )

end subroutine reshape
