program fgl16

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
  iwin = glutCreateWindow("fgl16 Example 5-1"//char(0))

  call setup

  call glutDisplayFunc( display )
  call glutReshapeFunc( reshape )
  call glutKeyboardFunc( keyboard )

  call glutMainLoop()

end program
subroutine setup

  use opengl_gl
  
  real(GLfloat), dimension(4) :: spec  = (/ 1.0, 1.0, 1.0, 1.0 /)
  real(GLfloat), dimension(1) :: shine = 50.0
  real(GLfloat), dimension(4) :: pos   = (/ 1.0, 1.0, 1.0, 1.0 /)
  real(GLfloat), dimension(4) :: white = (/ 1.0, 1.0, 1.0, 1.0 /)
  real(GLfloat), dimension(4) :: amb   = (/ 0.5, 0.5, 0.5, 1.0 /)
  
  call glClearColor(0.0,0.0,0.0,0.0)
  call glShadeModel(GL_SMOOTH)

  call glMaterialfv(GL_FRONT, GL_SPECULAR, spec)
  call glMaterialfv(GL_FRONT, GL_SHININESS, shine)

  call glLightfv(GL_LIGHT0, GL_POSITION, pos)
  call glLightfv(GL_LIGHT0, GL_DIFFUSE, white)
  call glLightfv(GL_LIGHT0, GL_SPECULAR, white)

  call glLightModelfv(GL_LIGHT_MODEL_AMBIENT, amb)

  call glEnable(GL_LIGHTING)
  call glEnable(GL_LIGHT0)
  call glEnable(GL_DEPTH_TEST)
  
end subroutine setup
subroutine display() bind(C)

  use OpenGL_GL
  use OpenGL_GLu
  use OpenGL_GLut
      
  call glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT )

  call glutSolidSphere(1.0_gldouble, 32,32)

  call glflush()
  
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

  if( newWidth < newHeight )then
    call glOrtho(-1.5_gldouble,  1.5_gldouble, &
                 -1.5/Aspect,    1.5/Aspect,   &
	         -10.0_gldouble, 10.0_gldouble)
  else
    call glOrtho(-1.5*Aspect,    1.5*Aspect,   &    
                 -1.5_gldouble,  1.5_gldouble, &
	         -10.0_gldouble, 10.0_gldouble)
  endif
  call glMatrixMode(GL_MODELVIEW)
  call glLoadIdentity()

end subroutine reshape
subroutine keyboard(key, x,y) bind(C)

  use OpenGL_GL
  use OpenGL_GLut

  integer(kind=GLbyte), intent(IN), value :: key
  integer(kind=GLint), intent(in), value  :: x, y

  if( key == 27) stop 'esc hit'
    
end subroutine keyboard
