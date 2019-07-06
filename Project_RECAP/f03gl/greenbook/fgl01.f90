module callbacks
  
  contains
  
  subroutine display() bind(C)

  use opengl_gl

  call glClear( GL_COLOR_BUFFER_BIT )
  
  call glColor3f( 1.0, 0.0, 0.0)
  call glBegin( GL_LINES )
  
  call glVertex2i( 180, 20)
  call glVertex2i(  20, 80)
  
  call glEnd()
  
  call glFlush()

  end subroutine display
end module callbacks
program fgl01

  use opengl_gl
  use opengl_glu
  use opengl_glut

  use callbacks
  
  integer(kind=GLint) :: iwin
  real(kind=GLdouble) :: window(4) = (/ 0.0, 200.0, 0.0, 100.0 /)
  
  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 50, 100)
  call glutInitWindowSize( 200, 100)
  iwin = glutCreateWindow("fgl01"//char(0))
  
  call glClearColor( 1.0, 1.0, 1.0, 0.0)
  call glMatrixMode( GL_PROJECTION )
  call gluOrtho2D( window(1), window(2), window(3), window(4) )
  
  call glutdisplayfunc( display )
  
  call glutMainLoop()
  
end program fgl01

