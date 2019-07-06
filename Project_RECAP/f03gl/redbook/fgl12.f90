program fgl12

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

  integer(kind=GLint) :: winWidth = 200, winHeight = 200
  integer(kind=GLint) :: iwin

  call glutInit
  call glutInitDisplayMode(GLUT_SINGLE + GLUT_RGB )
  call glutInitWindowPosition( 100, 100)
  call glutInitWindowSize( winWidth, winHeight )
  iwin = glutCreateWindow("fgl12 Example 3-1"//char(0))

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
  
  call glClear(GL_COLOR_BUFFER_BIT)
  call glColor3f(1.0,1.0,1.0)

  call glLoadIdentity()
  call gluLookAt( 0.0_gldouble, 0.0_gldouble, 5.0_gldouble, &
                  0.0_gldouble, 0.0_gldouble, 0.0_gldouble, &
                  0.0_gldouble, 1.0_gldouble, 0.0_gldouble  )

  call glscalef(1.0,2.0,1.0)
  call glutWireCube(1.0_gldouble)

  call glFlush()

end subroutine display
subroutine reshape(newWidth, newHeight) bind(C)

  use OpenGL_GL
  use OpenGL_GLU

  integer(kind=GLcint), intent(IN), value :: newWidth, newHeight
  real(kind=GLdouble) :: Zero, One, Width, Height

  Zero   = 0.0
  One    = 1.0
  Width  = newWidth
  Height = newHeight

  call glviewport(0,0,newWidth,newHeight)

  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity()

  call glFrustum(-One,One,-One,One,3.5_gldouble,20._gldouble)
  call glMatrixMode(GL_MODELVIEW)

end subroutine reshape
